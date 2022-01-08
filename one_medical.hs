{-# LANGUAGE OverloadedStrings #-}

import           Data.Text  (Text)
import qualified Data.Text  as T
import           Data.Time  (fromGregorian)
import           OneMedical
import           Test.WebDriver(runSession)

southBayLocations =
  [ "Sunnyvale"
  , "Sunnyvale Outdoor Testing Site"
  , "San Mateo - Bay Meadows"
  , "San Jose - The Alameda"
  , "San Jose - North First"
  , "Redwood City"
  , "Phillips Brooks School COVID Testing Site" -- Menlo Park
  , "Palo Alto"
  ]

findSouthBayLocations
  = findAppointmentsScenario user pwd "COVID-19 PCR Test"
    (fromGregorian 2022 1 10)
  $ locationsInP southBayLocations
-- (not . locationsP ["UCSF One Medical Testing Site"])

findAppointmentsScenarioGoogle
  = findAppointmentsScenario user pwd "Google Onsite Covid19 Testing"
    (fromGregorian 2022 1 11)
  . locationsP $ \l ->
         not ("SBO" `T.isInfixOf` l)
      && not ("SFO" `T.isInfixOf` l)

user :: Text
user = "<your email registered with OneMedical>"
pwd :: Text
pwd = "<your OneMedical password>"

-- sess <- returnSession remoteConfig findSouthBayLocations

main = runSession remoteConfig findSouthBayLocations

