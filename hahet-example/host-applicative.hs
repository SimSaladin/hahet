{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Prelude
import Hahet
import Hahet.Imports

import Hahet.Modules.DateTimeManager
import Hahet.Modules.SSH

data Applicative = Applicative deriving Typeable
                 
instance PackageManagement Applicative where
        pkgManager _ = pacman undefined

main :: IO ()
main = do
    app <- configure Applicative $ do
        use =<< ssh
            $* (\s -> s{ sshClient = SSHClientConf })
        use $ DateTime "Europe/Helsinki"
        use $ Chrony

    apply app >>= putStrLn . prettyPrintResults
