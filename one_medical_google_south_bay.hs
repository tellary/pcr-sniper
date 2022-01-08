{-# LANGUAGE OverloadedStrings #-}

import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Time      (fromGregorian)
import           OneMedical
import           Test.WebDriver (runSession)

params
  = FindAppointmentParams
  { user = "<your email registered with OneMedical>"
  , password = "<your OneMedical password>"
  , area = "SF Bay Area"
  , appointmentType = "Google Onsite Covid19 Testing"
  , lastDay = fromGregorian 2022 1 11
  , predicate = locationsP $ \l ->
         not ("SBO" `T.isInfixOf` l)
      && not ("SFO" `T.isInfixOf` l)
  }

findAppointmentsGoogleSouthBay = findAppointmentsScenario params

main = runSession remoteConfig findAppointmentsGoogleSouthBay

