{-# LANGUAGE ScopedTypeVariables #-}
-- | API exposed from internal.
module Hahet.Core ( module Hahet.Core ) where

import Data.Typeable
import qualified Data.Text as T

import Prelude                      as Hahet.Core hiding (FilePath)
import Shelly                       as Hahet.Core hiding (path) -- XXX deprecated, removed in future? convenient variable name for us :)
import Control.Monad                as Hahet.Core
import Hahet.Core.Internals         as Hahet.Core
import Hahet.Core.Execution         as Hahet.Core
import Hahet.Targets.FileNodes      as Hahet.Core
import Hahet.Targets.Packages       as Hahet.Core
import Hahet.Targets.Services       as Hahet.Core

import Control.Monad.Reader
import Control.Monad.State

-- ** Configuration functions

-- | Use a module. Loads up the module, checks dependency conflicts.
-- Conflicts are logged to stdout.
use :: HahetModule mc c => mc -> C c ()
use mconf = do
    mlog $ "Entered module " ++ modId
    modify (pushAppModule modId)
    fromModule mconf -- run the module configuration initialization function
                 -- XXX: check for system-wide daemon conficts
    where
        modId = show (typeOf mconf)

-- | Require a target to be applied.
manage :: (Target c t) => t -> C c ()
manage t = do
    c <- ask
    mlog   $ "Added target "
        ++ show (typeOf t)
        ++ ": "
        ++ T.unpack (targetDesc c t)
    modify $ pushTarget t

revoke :: Target c t => t -> C c ()
revoke _ = error "Target revoking not implemented"
