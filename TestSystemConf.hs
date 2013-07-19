{-# LANGUAGE TypeFamilies, DeriveDataTypeable, ConstraintKinds, OverloadedStrings #-}

module TestSystemConf where

import Hahet
import Hahet.Modules.ModNginx

-- todos
-- - inotify limits
-- - networking
-- - snd alsa conf
-- - /etc/systemd/system/auto-tunnel-functor.service
--
-- - pacman
--  * mirrorlist

-- | A datatype for a configuration "class". Values of this type should
-- correspond to a system.
data TestConf = TestConf
        deriving Typeable

-- | Convenience synonym.
type Conf = ConfMonad TestConf

-- | Make our conf instance of the Hahet typeclass.
instance Hahet TestConf where
    type PackageManager TestConf = Pacman

-- * 
configure :: Conf ()
configure = 
    use (def :: Nginx)

main :: IO ()
main = do
    app     <- confToApp TestConf configure
    results <- runHahet app []
    return ()
