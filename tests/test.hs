------------------------------------------------------------------------------
-- File:          
-- Creation Date:
-- Last Modified: Jul 21 2013 [07:38:55]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Main where

import Prelude
import System.Exit (exitFailure)
import Hahet.Core


-- exampleFileNode :: FileNode SourcedFile
-- exampleFileNode = File "/tmp/testi"
--                     >>> ("testi-sisältö" :: Text)
--                     >$> "777"
-- exDirectory :: FileNode PlainDirectory
-- exDirectory = Directory "/tmp/dtes" -- >&& [("testi", "sisältö")] >$> "777"

main = do
    putStrLn $ "Read permissions \"777\" and show it: " ++ show (read "777" :: Permissions)
    putStrLn "Testing file /tmp/test.file permissions change with handlePerms"
    shelly $ handlePerms "/tmp/test.file" "777"
