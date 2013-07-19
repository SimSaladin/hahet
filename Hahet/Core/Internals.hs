-- | The very basic datatypes for configuration creation.
module Hahet.Core.Internals where

import Prelude hiding (FilePath)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Shelly

default (Text)

-- | Logging system - for now :)
mlog :: MonadIO m => String -> m ()
mlog = liftIO . putStrLn

convertFilePath :: FilePath -> String
convertFilePath = T.unpack . toTextIgnore

-- * Targets

type Conflict = Text

-- | The primitive of configurations.
class Typeable target => Target target where
    targetApply    :: target -> IO ()

    targetApplyAll :: [target] -> IO ()
    targetApplyAll = mapM_ targetApply

    targetConflicts :: target -> target -> Maybe Conflict
    targetConflicts _ _ = Nothing

data AppTarget where
    MkTarget :: Target a => a -> AppTarget

-- * Application

type ModuleIdent = String

-- | Application is a configuration instance. Contains everything needed to
--   apply a configuration.
data Application = Application
    { appHierarchy      :: [ModuleIdent]
    -- Targets are sorted by the module identifier
    , appTargets        :: M.Map ModuleIdent [AppTarget]
    , appFlags          :: [Flag]
--    , appCurTargets     :: [Target]
--    , appCurDepModules  :: [ModuleIdent]
--    , appPkgTargets     :: [Pkg]
--    , appPkgUntargets   :: [Pkg]
    --, appModules        :: Map String
    }

mkApplication :: String -> Application
mkApplication target = Application [target]
                                   M.empty
                                   []

-- | Base class for a Hahet config.
class Typeable conf => Hahet conf where

    -- | Package manager this configuration uses.
    type PackageManager conf :: *

class (Hahet c, Typeable mc) => HahetModule mc c where
    hmInit :: mc -> ConfMonad c ()

-- **  The Hahet monad

-- | Configuration monad.
newtype ConfMonad conf a = ConfMonad {
    unConfMonad :: ReaderT conf (StateT Application IO) a
    } deriving (Monad, MonadIO, MonadReader conf, MonadState Application)

-- | Run a configuration monad
confToApp :: Hahet conf => conf -> ConfMonad conf a -> IO Application
confToApp c m = liftM snd -- discard the monad's result
    $ runStateT (runReaderT (unConfMonad m') c)
    $ mkApplication $ show $ typeOf c
  where
      -- Logging for configuration check
      m' = do mlog "-- Starting configuration check --"
              m
              mlog "--   Configuration check done   --"


-- | Apply-time flags
data Flag = ModuleFlag Text
          | DevFlag    Text

data ApplyResult = ApplyResult
