module Hahet.Core where

import Data.Text
import Data.Typeable
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Hahet.Core.Module

-- * Base

-- | Base class for a Hahet config.
class Typeable conf => Hahet conf where

    -- | Package manager this configuration uses.
    type PackageManager conf :: *

data Application = Application
    { appTarget  :: String
    --, appModules :: [Module]
    }

-- **  The Hahet monad

-- | Configuration monad.
newtype ConfMonad conf a = ConfMonad {
    unConfMonad :: ReaderT conf (StateT Application IO) a
    } deriving (Monad, MonadIO, MonadReader conf, MonadState Application)

-- | Run a configuration monad
confToApp :: Hahet conf => conf -> ConfMonad conf a -> IO Application
confToApp c m =
    let app = Application (show $ typeOf c) -- empty application
        in liftM snd $ runStateT (runReaderT (unConfMonad m) c) app


-- ** Configuration functions

-- | Use a module. Loads up the module, checks dependency conflicts.
-- Conflicts are logged to stdout.
use :: HahetModule mod => mod -> ConfMonad conf ()
use mod = do
    app <- get
    -- run the module configuration initialization
    -- check for package conflicts
    -- check for system-wide daemon conficts
    -- ..?
    undefined


-- ** Applying

-- | Apply-time flags
data Flag = ModuleFlag Text
          | DevFlag    Text

data ApplyResult = ApplyResult

-- | Applying a configuration on system.
runHahet :: Application -> [Flag] -> IO [ApplyResult]
runHahet app flags = do
    print (appTarget app)
    return []
