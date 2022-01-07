{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Catch
import Test.WebDriver
import Test.WebDriver.Session
import Test.WebDriver.Commands.Wait
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Maybe

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
  click =<< fmap head . filterM (fmap (T.isInfixOf appointmentType) . getText) =<< findElems (ByTag "h5")

findAppointments = do
  appts <- readAppointments
  if null appts
    then do
      nextAppointmentsPage
      findAppointments
  else
    return appts

nextAppointmentsPage = click =<< findElem (ByXPath "//div/om-arrow-right/div")

data Appointment
  = Appointment
  { providerInfo :: ProviderInfo
  , time :: LocalTime
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

checkAppointmentsExistOnPage = do
  elems <- waitUntil 1000
           $ findElems (ByXPath "//button[@data-cy='inventory-button']")
  return . not . null $ elems

readProviders =
  mapM toProvider =<< findElems (ByXPath "//om-provider-inventory")

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
  :: Text -> (Appointment -> Bool) -> ChooseAppointmentPage
  -> WD [Appointment]
waitForAppointments appointmentType p page = do
  selectAppointment appointmentType page
  appts <- filter p <$> findAppointments
  if null appts
    then do
      backToAppointmentSelection
      waitForAppointments appointmentType p page
    else
      return appts

waitForAppointmentsNoLaterThanDay :: Text -> Day -> ChooseAppointmentPage -> WD [Appointment]
waitForAppointmentsNoLaterThanDay appointmentType day page
  = waitForAppointments appointmentType ((<= dayEnd) . time) page
  where dayEnd = LocalTime (addDays 1 day) (TimeOfDay 0 0 0)

selectItem :: Element -> Text -> WD [Element]
selectItem selectElem value = do
  opts <- findElemsFrom selectElem (ByTag "option")
  filterM (fmap (== value) . getText) $ opts

returnSession config wd = runSession remoteConfig $ do
  sess <- getSession
  catch (wd >>= \a -> return (sess, Right a))
    $ \(e::SomeException) -> return (sess, Left e)

returnSessionScenario user pwd dayStr = returnSession remoteConfig $ do
  let Just day = parseTimeM True defaultTimeLocale "%Y-%m-%d" dayStr
  setImplicitWait 3000
  login user pwd
    >>= gotoAppointmentSelection "SF Bay Area"
    >>= waitForAppointmentsNoLaterThanDay "COVID-19 PCR Test" day

-- sess <- returnSessionScenario user pwd "2022-01-10"
-- sess <- returnSession remoteConfig (login user pwd >> getCare)
-- main = returnSessionScenario user pwd "2022-01-10"
