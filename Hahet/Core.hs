-- | API exposed from internal.
module Hahet.Core ( module Hahet.Core ) where

import Data.Text
import Data.Typeable
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Prelude                      as Hahet.Core hiding (FilePath)
import Shelly.Pipe                  as Hahet.Core
import Hahet.Core.Internals         as Hahet.Core
import Hahet.Core.PackageManager    as Hahet.Core

-- ** Configuration functions

-- | Use a module. Loads up the module, checks dependency conflicts.
-- Conflicts are logged to stdout.
use :: HahetModule mc c => mc -> ConfMonad c ()
use mconf = do
    mlog $ "Entered module " ++ modId
    modify (pushAppModule modId)
    hmInit mconf -- run the module configuration initialization function
                 -- XXX: check for system-wide daemon conficts
    where
        modId = show (typeOf mconf)

-- | Require a target to be applied.
manage :: Target t => t -> ConfMonad conf ()
manage t = do
    mlog   $ "Added target " ++ show (typeOf t)
    modify $ pushTarget t

revoke :: Target t => t -> ConfMonad conf ()
revoke t = error "Target revoking not implemented"
