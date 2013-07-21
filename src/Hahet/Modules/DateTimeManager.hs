{-# LANGUAGE UndecidableInstances #-}
module Hahet.Modules.DateTimeManager where

import Hahet
import qualified Data.Text as T
import GHC.IO.Handle
default(Text)

type TimeZone = Text

data DTM = DTM
    { dtmTimezone :: TimeZone
    , dtmUseNtp   :: Bool
    } deriving (Typeable)

instance PackageManagement c => HahetModule DTM c where
    fromModule DTM{dtmTimezone = timezone, dtmUseNtp = useNTP} = do
        when useNTP $ do
            manage $ Pkg     "ntpd"
            manage $ Service "ntpd.service" (Just True)
            manage $ File "/etc/ntpd.conf" def "servers pool.ntp.org"

        manage $ AfterSh $ do
            current <- curTimeZone
            mlog $ "Current timezone is "
                <> show current <> if current == timezone 
                    then ", which is the wanted timezone."
                    else ", which differs from the wanted timezone " <> show timezone

            unless (current == timezone) $
                cmd "timedatectl" "set-timezone" timezone

            return $ ResFailed "Not yet implemented"


-- | Get the current timezone from the system.
curTimeZone :: Sh TimeZone
curTimeZone = liftM T.pack $ print_stdout False $ escaping False $
    runHandle "timedatectl" ["status | awk '/Timezone/{printf $2}'"]
              (liftIO . hGetLine)
              -- XXX: why this does not add a newline?
