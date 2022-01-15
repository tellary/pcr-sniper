{-# LANGUAGE OverloadedStrings     #-}

import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Time
import           OneMedical
import           Test.WebDriver (runSession)

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

params
  = FindAppointmentParams
  { user = "<your email registered with OneMedical>"
  , password = "<your OneMedical password>"
  , area = "SF Bay Area"
  , appointmentType = "COVID-19 PCR Test"
  , lastDay = fromGregorian 2022 01 20
  , predicate = locationsInP southBayLocations
  -- , predicate = not . locationsInP [ "UCSF One Medical Testing Site"
  --                                  , "Sunnyvale Outdoor Testing Site"
  --                                  ]
  -- , predicate = \appt ->
  --     not ("San Jose" `T.isInfixOf` location (providerInfo appt))
  --     && (localTimeOfDay (time appt) >= TimeOfDay 10 0 0)
  }

findAppointments = findAppointmentsScenario params
bookAppointment = bookAppointmentScenario params
-- sess <- returnSession remoteConfig findSouthBayLocations

main = runSession remoteConfig findAppointments
