{-# LANGUAGE TypeFamilies, DeriveDataTypeable, ConstraintKinds, OverloadedStrings #-}

module TestSystemConf where

import Hahet
import Hahet.Modules.ModNginx

-- | Our configuration datatype.
data TestConf = TestConf deriving Typeable

type Conf = ConfMonad TestConf

-- | Make testconf an instance of the Hahet configuration manager. Here we
-- specify static properties for the configuration.
instance Hahet TestConf where
    type PackageManager TestConf = Pacman

-- * My modules

myNginx :: Conf NginxServer
myNginx = do
    return $ NginxServer ["mui."]

-- * Execute tests

myConf :: TestConf
myConf = TestConf

myInit :: Conf ()
myInit = do
    use =<< myNginx

main :: IO ()
main = do
    app     <- confToApp myConf myInit
    results <- runHahet app []
    return ()
