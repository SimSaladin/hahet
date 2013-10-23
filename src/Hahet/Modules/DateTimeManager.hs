{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Hahet.Modules.DateTimeManager where

import           Hahet
import           Hahet.Imports
import qualified Data.Text as T
import           GHC.IO.Handle
import           Hahet.Internal
default(Text)

type TimeZone = Text

data DateTime = DateTime
    { dtmTimezone :: TimeZone
    } deriving (Typeable)

instance Typeable c => HahetModule DateTime c where
    fromModule DateTime{dtmTimezone = timezone} = do
        manage . AfterH . setTimeZone timezone =<< getConf

setTimeZone :: Typeable c
            => TimeZone -> c -> H c ApplyResult
setTimeZone timezone c = do
    current <- sh curTimeZone
    if current == timezone
        then do
            $(logDebug) $ [qc|Current timezone is { current }, which is the wanted timezone|]
            return ResNoop
        else do
            $(logInfo) $ [qc|Change timezone from {current} to {timezone}|]
            _res <- sh $ cmd "timedatectl" "set-timezone" timezone
            return ResSuccess

-- | Get the current timezone from the system.
curTimeZone :: Sh TimeZone
curTimeZone = liftM T.pack . print_stdout False . escaping False $
    runHandle "timedatectl" ["status | awk '/Timezone/{printf $2}'"]
              (liftIO . hGetLine)
              -- XXX: why this does not add a newline?

-- * NTP daemons

-- ** Chrony

data Chrony = Chrony deriving (Typeable)
instance PackageManagement conf => HahetModule Chrony conf where
    fromModule Chrony = do
        manage $ Pkg  "chrony"
        manage $ Service "chronyd.service" (Just True)
        manage $ file "/etc/chrony.conf" /- owner "root" /- group "root" /- perms "644"
            /- fileSource [qc|
server 0.pool.ntp.org iburst
server 1.pool.ntp.org iburst
server 2.pool.ntp.org iburst

rtcfile /etc/chrony.rtc
rtconutc

keyfile /etc/chrony.keys
commandkey 1

driftfile /etc/chrony.drift
|]

-- ** ntpd

data NTP = NTP
         { ntpServers :: [Text]
         } deriving (Typeable)

instance PackageManagement conf => HahetModule NTP conf where
    fromModule NTP{} = do
        manage $ Pkg     "ntpd"
        manage $ Service "ntpd.service" (Just True)
        manage $ file "/etc/ntpd.conf" /- fileSource "servers pool.ntp.org"
