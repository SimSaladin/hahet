module Hahet.Modules.DateTimeManager where

import Hahet
import qualified Data.Text as T

type TimeZone = Text

data DTM = DTM
    { dtmTimezone :: TimeZone
    , dtmUseNtp   :: Bool
    } deriving (Typeable)

instance Hahet c => HahetModule DTM c where
    hmInit dtm = do
        unless (not $ dtmUseNtp dtm) $ do
            manage $ Pkg     "ntpd"
            manage $ Service "ntpd" (Just True)
            manage $ File "/etc/ntpd.conf" def "servers pool.ntp.org"
        manage $ AfterSh $ do 
            current <- curTimeZone
            let timezone = dtmTimezone dtm

            mlog $ "Current timezone is " <> T.unpack current <> if current == timezone 
                then " which is the wanted timezone."
                else ", which differs from the wanted timezone" <> T.unpack timezone

            unless (current == timezone) $
                cmd "timedatectl" "set-timezone" timezone

            return $ ResFailed "Not yet implemented"


-- | Get the current timezone from the system.
curTimeZone :: Sh TimeZone
curTimeZone = 
    cmd "timedatectl" "status" -|- cmd "awk" "/Timezone/{print $2}"
