-- | This module provides realization methods for configurations.
module Hahet.Core.Execution where

import Prelude hiding (FilePath)
import Data.Text (Text)
import Data.Typeable
import qualified Data.Text as T
import Shelly
import qualified Data.Map as M

import Hahet.Core.Internals

default (Text)

-- | Get the application's top configuration type.
getAppIdent :: Application c -> ModuleIdent
getAppIdent = last . appHierarchy

getModule :: Application c -> ModuleIdent
getModule = head . appHierarchy

getAppHierarchy :: Application c -> [ModuleIdent]
getAppHierarchy = appHierarchy

pushAppModule :: ModuleIdent -> Application c -> Application c
pushAppModule i app = app
    { appHierarchy = i : appHierarchy app }

popAppModule :: Application c -> Application c
popAppModule app = app{ appHierarchy = tail $ appHierarchy app }

pushTarget :: Target c target => target -> Application c -> Application c
pushTarget t app = app
    { appTargets = M.insertWith' (++) m [MkTarget t] $ appTargets app }
     where m = getModule app

runTarget :: Application c -> AppTarget c -> IO ApplyResult
runTarget app (MkTarget t) = do
    putStrLn $ show (typeOf t) ++ ": "
            ++ T.unpack (targetDesc (appConf app) t)
    shellyNoDir $ apply app $ targetApply t

-- | Applying a configuration on system.
runHahet :: Application c -> IO [ApplyResult]
runHahet app = do
    mlog $ "-- Applying configuration: " ++ getAppIdent app

    -- XXX: filtering and dependencies
    results <- mapM (mapM (runTarget app))
                    (M.elems $ appTargets app)
    return $ concat results
