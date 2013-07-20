------------------------------------------------------------------------------
-- File:          
-- Creation Date:
-- Last Modified: Jul 15 2013 [23:39:03]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Main where

import Prelude
import System.Exit (exitFailure)
import Hahet.Core


main = do
    putStrLn $ "Read permissions \"777\" and show it: " ++ show (read "777" :: Permissions)
    putStrLn "Testing file /tmp/test.file permissions change with handlePerms"
    shelly $ handlePerms "/tmp/test.file" "777"
