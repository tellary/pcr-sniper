{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module OneMedical
  ( findAppointmentsScenario
  -- Predicates
  , allP
  , dayEndP
  , dayStartP
  , locationsInP
  , locationsP
  , timeStartP
  , timeEndP
  -- Selenium
  , remoteConfig
  , returnSession
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import           Data.Time              (Day, LocalTime (LocalTime), ParseTime,
                                         TimeOfDay (TimeOfDay), addDays,
                                         defaultTimeLocale, fromGregorian,
                                         parseTimeM)
import           Test.WebDriver
import           Test.WebDriver.Session
import           Text.Pretty.Simple

-- ./chromedriver --port=9515 --log-level=ALL --url-base=/wd/hub
-- ./chromedriver --port=9515  --url-base=/wd/hub
remoteConfig = useBrowser chrome defaultConfig { wdHost = "localhost"
                                               , wdPort = 9515
                                               }

data HomePage = HomePage
data ChooseAppointmentPage = ChooseAppointmentPage

defaultWait = 3000

login :: Text -> Text -> WD HomePage
login user pwd = do
  openPage "https://onemedical.com"
  click =<< findElem (ByPartialLinkText "Log in")
  sendKeys user =<< findElem (ById "email")
  sendKeys pwd =<< findElem (ById "password")
  click =<< findElem (ById "btn-login")
  return HomePage

gotoAppointmentSelection :: Text -> HomePage -> WD ChooseAppointmentPage
gotoAppointmentSelection location _  = do
  click =<< findElem (ByPartialLinkText "Get Care")
  sendKeys "COVID PCR Test"
    =<< findElem (ByXPath "//*[@formcontrolname='reasonForVisit']")
  select <- findElem (ByXPath "//*[@formcontrolname='serviceArea']")
  click =<< head <$> (selectItem select location)
  click =<< findElem (ByXPath "//*[@aria-label='Submit']")
  return ChooseAppointmentPage

selectAppointment :: Text -> ChooseAppointmentPage -> WD ()
selectAppointment appointmentType _ = do
  click
    =<< fmap head . filterM (fmap (T.isInfixOf appointmentType) . getText)
    =<< findElems (ByTag "h5")

logWD = liftIO . putStrLn

findFirstAppointments :: Day -> WD [Appointment]
findFirstAppointments lastDay = do
  exist <- checkProvidersExistOnPage
  if exist
    then do
      pageFirstDay <- toLastDay =<< findFirstDayElem
      pageLastDay  <- (toLastDay =<< findLastDayElem :: WD Day)
      if pageFirstDay > lastDay
        then do
          logWD
            $ "pageFirstDay > lastDay: "
            ++ show pageFirstDay
            ++ " > "
            ++ show lastDay
          return []
        else do
          appts <- readAppointments
          if null appts
            then do
              logWD
                $  "No appointments found between "
                ++ show pageFirstDay
                ++ " and "
                ++ show pageLastDay
              if pageLastDay >= lastDay
                then do
                  logWD
                    $ "pageLastDay >= lastDay: "
                    ++ show pageLastDay
                    ++ " >= "
                    ++ show lastDay
                  return []
                else do
                  nextAppointmentsPage
                  findFirstAppointments lastDay
            else
              return appts
    else do
      logWD "No providers found"
      return []

findAppointments :: Day -> (Appointment -> Bool) -> WD [Appointment]
findAppointments lastDay p = do
  appts <- findFirstAppointments lastDay
  if null appts
    then return []
    else do
      let appts'
            = filter
              (allP [ (<= nextDayStartTime lastDay) . time, p ])
              appts
      if not . null $ appts'
        then return appts'
        else do
          logWD
            $ "No appointments match predicate:\n"
            ++ (LT.unpack . pShowNoColor $ appts)
          nextAppointmentsPage
          findAppointments lastDay p

nextAppointmentsPage = click =<< findElem (ByXPath "//div/om-arrow-right/div")

findLastDayElem :: WD Element
findLastDayElem = findElem (ByXPath "//div[om-arrow-right]/div/div[3]")

findFirstDayElem :: WD Element
findFirstDayElem = findElem (ByXPath "//div[om-arrow-right]/div/div[1]")

toLastDay :: ParseTime t => Element -> WD t
toLastDay = fmap parseLastDay . getText

-- parseLastDay "Sat Jan 08" -> 2022-01-08
parseLastDay :: ParseTime t => Text -> t
parseLastDay
  = fromJust
  . parseTimeM True defaultTimeLocale "%Y %a %b %d"
  . ("2022 " ++ ) . T.unpack

data Appointment
  = Appointment
  { providerInfo :: ProviderInfo
  , time         :: LocalTime
  } deriving Show

data ProviderInfo
  = ProviderInfo
  { location :: Text
  } deriving Show

data Provider
  = Provider
  { providerInfo :: ProviderInfo
  , appointments :: [Appointment]
  } deriving Show

readAppointments :: WD [Appointment]
readAppointments = do
  exist <- checkAppointmentsExistOnPage
  if exist
    then do
      setImplicitWait 0
      providers <- readProviders
      setImplicitWait defaultWait
      return . concat . map appointments $ providers
    else
      return []

waitAppointmentsPageLoaded = do
  elems <- findElems (ByPartialLinkText "Back to Appointment Selection")
  if null elems then error "Appointments page didn't load" else return ()

checkAppointmentsExistOnPage = do
  waitAppointmentsPageLoaded
  -- Seems that waitUntil gets affected by setImplicitWait
  -- so waitUntil 0 $ findElems ... doesn't work below
  setImplicitWait 0
  elems <- findElems (ByXPath "//button[@data-cy='inventory-button']")
  setImplicitWait defaultWait
  return . not . null $ elems

checkProvidersExistOnPage = do
  waitAppointmentsPageLoaded
  setImplicitWait 0
  elems <- findProviders
  setImplicitWait defaultWait
  return . not . null $ elems

findProviders = findElems (ByXPath "//om-provider-inventory")

readProviders =
  mapM toProvider =<< findProviders

toProvider elem = do
  location <- getText =<< findElemFrom elem (ByClass "office-name")
  times <- toAppointmentTimes =<< findAppointmentsOfProvider elem
  let info = ProviderInfo location
  let appts = map (Appointment info) times
  return $ Provider info appts

findAppointmentsOfProvider elem =
  findElemsFrom elem (ByXPath ".//button[@data-cy='inventory-button']")

toAppointmentTimes apptElems =
  fromJust . sequence . filter isJust <$> mapM apptTime apptElems

apptTime elem = do
  textMaybe <- attr elem "aria-label"
  return (parseApptTimeLabel =<< textMaybe)

-- parseApptTimeLabel "Tue 01 25 3 15 pm"
parseApptTimeLabel :: Text -> Maybe LocalTime
parseApptTimeLabel
  = parseTimeM True defaultTimeLocale "%Y %a %m %d %l %M %P"
  -- Hack to add year to the current date
  . ("2022 " ++)
  . T.unpack

backToAppointmentSelection :: WD ()
backToAppointmentSelection
  = click =<< findElem (ByPartialLinkText "Back to Appointment Selection")

waitForAppointments
  :: Text -> Day -> (Appointment -> Bool) -> ChooseAppointmentPage
  -> WD [Appointment]
waitForAppointments appointmentType lastDay p page = do
  selectAppointment appointmentType page
  appts <- findAppointments lastDay p
  if null appts
    then do
      backToAppointmentSelection
      waitForAppointments appointmentType lastDay p page
    else
      return appts

selectItem :: Element -> Text -> WD [Element]
selectItem selectElem value = do
  opts <- findElemsFrom selectElem (ByTag "option")
  filterM (fmap (== value) . getText) $ opts

returnSession config wd = runSession config $ do
  sess <- getSession
  catch (wd >>= \a -> return (sess, Right a))
    $ \(e::SomeException) -> return (sess, Left e)

findAppointmentsScenario user pwd appointmentType lastDay p = do
  setImplicitWait defaultWait
  appts <- login user pwd
           >>= gotoAppointmentSelection "SF Bay Area"
           >>= waitForAppointments appointmentType lastDay p
  logWD
    $ "Found matching appointments:\n"
    ++ (LT.unpack . pShowNoColor $ appts)

nextDayStartTime day
  = LocalTime (addDays 1 day ) (TimeOfDay 0 0 0)

dayEndP m d = (<= dayEnd) . time
  where dayEnd = nextDayStartTime $ fromGregorian 2022 m d

dayStartP m d
  = (>= LocalTime (fromGregorian 2022 m d) (TimeOfDay 0 0 0))
  . time

locationsInP locations = locationsP (`elem` locations)
locationsP p
  = p . location
  . (providerInfo :: Appointment -> ProviderInfo)

timeStartP h m = (>= TimeOfDay h m 0)

timeEndP h m = (<= TimeOfDay h m 0)

allP :: [a -> Bool] -> a -> Bool
allP ps a =
  and . flip map ps $ \p -> p a