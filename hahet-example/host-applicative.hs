{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Prelude
import Hahet
import Hahet.Imports
import Hahet.Modules.DateTimeManager
import Hahet.Modules.SSH

data Applicative = Applicative deriving Typeable
instance PackageManagement Applicative where
        pkgManager _ = pacman undefined

app :: ((), Configuration Applicative, [String])
app = configure Applicative $ do
    use =<< ssh $* (\s -> s{ sshClient = SSHClientConf })
    use $ DateTime "Europe/Helsinki"
    use $ Chrony

main :: IO ()
main = let (_, conf, logs) = app
           in apply conf >>= putStrLn . prettyPrintResults
