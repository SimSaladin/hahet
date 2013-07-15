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

-- | Our configuration datatype.
data TestConf = TestConf
        deriving Typeable

-- | Convenience synonym.
type Conf = ConfMonad TestConf

-- | Make our conf instance of the Hahet typeclass.
instance Hahet TestConf where
    type PackageManager TestConf = Pacman

-- * Modules configuration

mkNginx :: Conf Nginx
mkNginx = return def

-- * Execute tests

main :: IO ()
main = do
    app     <- confToApp TestConf (use (def :: Nginx))
    results <- runHahet app []
    return ()
