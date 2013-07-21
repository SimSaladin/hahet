{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Manifests where

import Hahet
import Hahet.Modules.DateTimeManager
import Hahet.Modules.SSH

-- | A datatype for a configuration "class". Values of this type should
-- correspond to a system.
data TestConf = TestConf deriving Typeable

-- | Convenience synonym.
type Conf = C TestConf

-- | Make our conf instance of the Hahet typeclass.
instance PackageManagement TestConf where
    pkgManager _ = pacman

-- | 
conf :: Conf ()
conf = do
    -- use (def :: Nginx)
    use $ DTM "Europe/Helsinki" True

    use =<< ssh
        $* (\s -> s{ sshClient = SSHClientConf })
