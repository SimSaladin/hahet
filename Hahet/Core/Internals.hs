{-# LANGUAGE RankNTypes #-}
-- | The very basic datatypes for configuration creation.
module Hahet.Core.Internals where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

-- | Logging system - for now :)
mlog :: MonadIO m => String -> m ()
mlog = liftIO . putStrLn

-- | Pkg reperesents a package. IsString provided for extending purposes (like
-- version numbers).
data Pkg = Pkg Text

instance Show Pkg     where
    show (Pkg txt) = T.unpack txt

instance IsString Pkg where
    fromString = Pkg . T.pack

type ModuleIdent = String

-- * Application

-- | Application is a configuration instance. Contains everything needed to
--   apply a configuration.
data Application = Application
    { appHierarchy      :: [ModuleIdent]
    , appPkgTargets     :: [Pkg]
    , appPkgUntargets   :: [Pkg]
    --, appModules        :: Map String
    }


mkApplication :: String -> Application
mkApplication target = Application [target]
                                   [] []

-- | Base class for a Hahet config.
class Typeable conf => Hahet conf where

    -- | Package manager this configuration uses.
    type PackageManager conf :: *


class Typeable mconf => HahetModule mconf where
    hmInit :: mconf -> ConfMonad c ()

type ModuleHandler mc c = mc -> ConfMonad c ()

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


-- * Execution

-- | Apply-time flags
data Flag = ModuleFlag Text
          | DevFlag    Text

data ApplyResult = ApplyResult

-- | Get the application's top configuration type.
getAppIdent :: Application -> ModuleIdent
getAppIdent = last . appHierarchy

getAppHierarchy :: Application -> [ModuleIdent]
getAppHierarchy = appHierarchy

pushAppModule :: ModuleIdent -> Application -> Application
pushAppModule i app = app{ appHierarchy = i : appHierarchy app }

popAppModule :: Application -> Application
popAppModule app = app{ appHierarchy = tail $ appHierarchy app }

-- | Applying a configuration on system.
runHahet :: Application -> [Flag] -> IO [ApplyResult]
runHahet app flags = do
    mlog $ "-- Applying configuration: " ++ getAppIdent app
    return []
