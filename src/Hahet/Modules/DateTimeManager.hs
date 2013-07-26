{-# LANGUAGE ScopedTypeVariables #-}
module Hahet.Modules.DateTimeManager where

import Hahet.Modules
import Hahet.Imports
import qualified Data.Text as T
import           GHC.IO.Handle

default(Text)

type TimeZone = Text

data DTM = DTM
    { dtmTimezone :: TimeZone
    , dtmUseNtp   :: Bool
    } deriving (Typeable)

instance (Typeable c, PackageManagement c) => HahetModule DTM c where
    fromModule DTM{dtmTimezone = timezone, dtmUseNtp = useNTP} = do
        when useNTP $ do
            manage $ Pkg     "ntpd"
            manage $ Service "ntpd.service" (Just True)
            manage $ File "/etc/ntpd.conf" /- fileSource "servers pool.ntp.org"

        let script :: AfterH c ApplyResult
            script = AfterH $ do
                current <- sh curTimeZone
                if current == timezone
                    then do
                        $(logDebug) $ [qc|Current timezone is { current }, which is the wanted timezone|]
                        return ResNoop
                    else do
                        $(logInfo) $ [qc|Change timezone from {current} to {timezone}|]
                        _ <- sh $ cmd "timedatectl" "set-timezone" timezone
                        return $ ResFailed "Not yet implemented"
        manage script


-- | Get the current timezone from the system.
curTimeZone :: Sh TimeZone
curTimeZone = liftM T.pack $ print_stdout False $ escaping False $
    runHandle "timedatectl" ["status | awk '/Timezone/{printf $2}'"]
              (liftIO . hGetLine)
              -- XXX: why this does not add a newline?
