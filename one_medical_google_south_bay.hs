{-# LANGUAGE OverloadedStrings #-}

import           Data.Text  (Text)
import qualified Data.Text  as T
import           Data.Time  (fromGregorian)
import           OneMedical
import           Test.WebDriver(runSession)

findAppointmentsGoogleSouthBay
  = findAppointmentsScenario user pwd "Google Onsite Covid19 Testing"
    (fromGregorian 2022 1 31)
  . locationsP $ \l ->
         not ("SBO" `T.isInfixOf` l)
      && not ("SFO" `T.isInfixOf` l)

user :: Text
user = "<your email registered with OneMedical>"
pwd :: Text
pwd = "<your OneMedical password>"

main = runSession remoteConfig findAppointmentsGoogleSouthBay

