{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Nginx configuration module for Hahet.
module Hahet.Modules.ModNginx where

import Data.Text
import Hahet

data NginxServer = NginxServer
        { nginxServers :: [Text]
        }

instance HahetModule NginxServer where
    dependsOnPkgs conf = []
