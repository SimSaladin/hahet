{-# LANGUAGE UndecidableInstances #-}

-- | Nginx configuration module for Hahet.
module Hahet.Modules.ModNginx where

import Hahet.Modules
import Hahet.Imports

data Nginx = Nginx
    { nginxFilesRoot :: FilePath
    , nginxServers   :: [Text]
    } deriving (Typeable)

instance Default Nginx where
    def = Nginx "/etc/nginx"
                []

instance PackageManagement c => HahetModule Nginx c where
    fromModule nserver = do
        let dir      = nginxFilesRoot nserver
            basefile = dir </> "nginx.conf"

        manage $ Pkg "nginx"
        manage $ Directory (toFileNode dir)
        manage $ File (toFileNode basefile) >>> "nginx conf"
