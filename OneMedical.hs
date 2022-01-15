{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module OneMedical
  ( findAppointmentsScenario
  , bookAppointmentScenario
  , FindAppointmentParams(..)
  , Appointment(..)
  , ProviderInfo(..)
  -- :Predicates:
  , allP
  , dayEndP
  , dayStartP
  , locationsInP
  , locationsP
  , timeStartP
  , timeEndP
  -- :Selenium:
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

defaultWait = 10000

login :: Text -> Text -> WD HomePage
login user pwd = do
  openPage "https://onemedical.com"
  click =<< findElem (ByPartialLinkText "Log in")
  loop
  where loop = do
          sendKeys user =<< findElem (ById "email")
          sendKeys pwd =<< findElem (ById "password")
          login <- findElem (ById "btn-login")
          loginDisplayed <- isDisplayed login
          logWD ("'Log In' button is displayed: " ++ show loginDisplayed)
          click login
          catch (
            findElem (ByPartialLinkText "Get Care")
            >> return HomePage
            ) $ \(e::SomeException) -> do
              logWD $ "Can't login, trying again"
              clearInput =<< findElem (ById "email")
              clearInput =<< findElem (ById "password")
              loop

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
          pageLastDay  <- (toLastDay =<< findLastDayElem :: WD Day)
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
  , element      :: Element
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
  let info = ProviderInfo location
  appts <- fmap keepJusts . mapM (toAppointment info)
           =<< findAppointmentsOfProvider elem
  return $ Provider info appts

findAppointmentsOfProvider elem =
  findElemsFrom elem (ByXPath ".//button[@data-cy='inventory-button']")

keepJusts = fromJust . sequence . filter isJust

toAppointment :: ProviderInfo -> Element -> WD (Maybe Appointment)
toAppointment info elem = do
  timeMaybe <- apptTime elem
  case timeMaybe of
    Nothing -> do
      logWD $ "Failed to parse appointment time"
      return Nothing
    Just time ->
      return . Just $ Appointment info time elem

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

data FindAppointmentParams
  = FindAppointmentParams
  { user            :: Text
  , password        :: Text
  -- Choice in the "I'm looking for appointments in..." field, i.e:
  -- "SF Bay Area", "Chicago", "Ney York", etc
  , area            :: Text
  , appointmentType :: Text
  -- We search appointments from today until the `lastDay`
  , lastDay         :: Day
  -- Predicate that tells is the given appointment matches our search
  , predicate       :: Appointment -> Bool
  } -- Do not derive Show to prevent printing `user` and `password`
findAppointmentsScenario params = do
  setImplicitWait defaultWait
  appts <- login (user params) (password params)
           >>= gotoAppointmentSelection (area params)
           >>= waitForAppointments
               (appointmentType params)
               (lastDay params)
               (predicate params)
  logWD
    $ "Found matching appointments:\n"
    ++ (LT.unpack . pShowNoColor $ appts)
  return appts

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

findConfirmAppointmentButton = findElem (ByXPath "//button[text() = 'Confirm Appointment']")
findOkButton = findElem (ByXPath "//button[text() = ' OK ']")

checkYourAppointmentIsBookedText = do
  elems <- findElems (ByXPath "//h1[text() = 'Your appointment is booked!']")
  if null elems
    then return False
    else return True

findPleaseTryYourSearchAgainText :: WD Element
findPleaseTryYourSearchAgainText
  = findElem (ByXPath "//h2[text() = 'Please try your search again.']")

bookAppointment :: [Appointment] -> WD (Maybe Appointment)
bookAppointment [] = return Nothing
bookAppointment (appt:appts) = do
  click . element $ appt
  click =<< findConfirmAppointmentButton
  booked <- checkYourAppointmentIsBookedText
  if booked
    then return . Just $ appt
    else do
      logWD $ "Failed to book appointment: " ++ show appt
      click =<< findOkButton
      logWD $ "Returned to appointments page"
      bookAppointment appts

bookAppointmentScenario params = do
  appts <- findAppointmentsScenario params
  apptMb <- bookAppointment appts
  case apptMb of
    Just appt -> do
      logWD $ "Booked appointment: " ++ show appt
      return appt
    Nothing -> bookAppointmentScenario params
