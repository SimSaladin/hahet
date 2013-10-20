------------------------------------------------------------------------------
-- File:          
-- Creation Date:
-- Last Modified: Oct 20 2013 [04:56:43]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Text as T
import System.Exit (exitFailure)

import Hahet
import Hahet.Imports
import Hahet.Internal


-- exampleFileNode :: FileNode SourcedFile
-- exampleFileNode = File "/tmp/testi"
--                     >>> ("testi-sisältö" :: Text)
--                     >$> "777"
-- exDirectory :: FileNode PlainDirectory
-- exDirectory = Directory "/tmp/dtes" -- >&& [("testi", "sisältö")] >$> "777"


-- * Filenodes

instance Arbitrary (FileNode PlainFile) where
        arbitrary = do
            fileName <- vectorOf 10 $ elements $ ['a'..'z'] ++ ['A'..'Z']
            return . file . fromText $ "/tmp/" <> T.pack fileName

prop_fnApplyExists :: Property
prop_fnApplyExists = monadicIO $ do
    fn <- pick arbitrary
    undefined

main = do
    -- putStrLn $ "Read permissions \"777\" and show it: " ++ show (read "777" :: Permissions)
    -- putStrLn "Testing file /tmp/test.file permissions change with handlePerms"
    -- shelly $ handlePerms "/tmp/test.file" "777"

    quickCheck prop_fnApplyExists
