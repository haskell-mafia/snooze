module BuildInfo_ambiata_snooze where
import Prelude
data RuntimeBuildInfo = RuntimeBuildInfo { buildVersion :: String, timestamp :: String, gitVersion :: String }
buildInfo :: RuntimeBuildInfo
buildInfo = RuntimeBuildInfo "0.0.1" "20160404173329" "3015176-M"
buildInfoVersion :: String
buildInfoVersion = "0.0.1-20160404173329-3015176-M"
