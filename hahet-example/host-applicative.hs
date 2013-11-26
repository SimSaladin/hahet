{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Prelude
import Hahet
import Hahet.Imports
import Hahet.Modules.DateTimeManager
import Hahet.Modules.SSH

data Applicative = Applicative deriving Typeable
instance PackageManagement Applicative where
        pkgManager _ = pacman undefined

(app, msgs) :: (Configuration Applicative, [String])
(app, msgs) = configure Applicative $ do
    use =<< ssh $* sshClient .~ SSHClientConf
    use $ DateTime "Europe/Helsinki"
    use $ Chrony

main :: IO ()
main = let (conf, _) = app
           in apply conf >>= putStrLn . prettyPrintResults
