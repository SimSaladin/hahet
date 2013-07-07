
-- | Nginx configuration module for Hahet.
module Hahet.Modules.ModNginx where

import Data.Text
import Hahet

data Nginx = Nginx
    { nginxFilesRoot :: FilePath
    , nginxServers   :: [Text]
    } deriving (Typeable)

instance Default Nginx where
    def = Nginx "/etc/nginx"
                []

instance HahetModule Nginx where
    hmInit = nginx

nginx :: ModuleHandler Nginx c
nginx nserver = do
    let dir      = nginxFilesRoot nserver
        basefile = dir

    requirePkg "nginx"
    manageDir  dir
    
    manageFile "/etc/nginx/" $ \fp ->
        return ""
