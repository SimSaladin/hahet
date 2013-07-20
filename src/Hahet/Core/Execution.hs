-- | This module provides realization methods for configurations.
module Hahet.Core.Execution where

import Prelude hiding (FilePath)
import Data.Text (Text)
import qualified Data.Map as M

import Hahet.Core.Internals

default (Text)

-- | Get the application's top configuration type.
getAppIdent :: Application -> ModuleIdent
getAppIdent = last . appHierarchy

getModule :: Application -> ModuleIdent
getModule = head . appHierarchy

getAppHierarchy :: Application -> [ModuleIdent]
getAppHierarchy = appHierarchy

pushAppModule :: ModuleIdent -> Application -> Application
pushAppModule i app = app
    { appHierarchy = i : appHierarchy app }

popAppModule :: Application -> Application
popAppModule app = app{ appHierarchy = tail $ appHierarchy app }

pushTarget :: Target target => target -> Application -> Application
pushTarget t app = app
    { appTargets = M.insertWith' (++) m [MkTarget t] $ appTargets app }
     where m = getModule app

runTarget :: AppTarget -> IO ApplyResult
runTarget (MkTarget t) = targetApply t

-- | Applying a configuration on system.
runHahet :: Application -> IO [ApplyResult]
runHahet app = do
    mlog $ "-- Applying configuration: " ++ getAppIdent app

    -- XXX: filtering and dependencies
    results <- mapM (mapM runTarget) (M.elems $ appTargets app)
    return $ concat results
