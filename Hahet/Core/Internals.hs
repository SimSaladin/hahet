{-# LANGUAGE RankNTypes #-}
-- | The very basic datatypes for configuration creation.
module Hahet.Core.Internals where

import Data.String
import Data.Text (Text)
import Data.Default
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

-- | Logging system - for now :)
mlog :: MonadIO m => String -> m ()
mlog = liftIO . putStrLn

-- * Targets

-- | The primitive of configurations.
class Typeable target => Target target where
    targetApply    :: target -> IO ()

    targetApplyAll :: [target] -> IO ()
    targetApplyAll = mapM_ targetApply

    targetConflicts :: target -> target -> IO ()
data AppTarget where
    MkTarget :: Target a => a -> AppTarget

-- ** File Targets

-- | A file target.
data FileNode = File      FilePath FileSettings AppFileSource
              | Directory FilePath FileSettings
            deriving Typeable

-- | Filesystem settings for a file.
data FileSettings = FileSettings
    { fOwner :: Text
    , fGroup :: Text
    }

-- | Some source which provides the content of a file
class FileSource source where
    fileSource :: source -> IO Text

-- | For wrapping source in a File.
data AppFileSource where
    MkFileSource :: FileSource a => a -> AppFileSource

instance Default FileSettings where
    def = FileSettings "" ""

instance Target FileNode where
    targetApply t = mlog "File target uimplemented!"

-- ** Pkg Targets

-- | Pkg reperesents a package. Provides a IsString instance, so you can create
-- a file from a String literal.
data Pkg = Pkg Text
    deriving Typeable

instance Show Pkg where
    show (Pkg txt) = T.unpack txt

instance IsString Pkg where
    fromString = Pkg . T.pack

instance Target Pkg where


-- * Application

type ModuleIdent = String

-- | Application is a configuration instance. Contains everything needed to
--   apply a configuration.
data Application = Application
    { appHierarchy      :: [ModuleIdent]
    , appTargets        :: M.Map ModuleIdent [AppTarget]
    , appFlags          :: [Flag]
--    , appCurTargets     :: [Target]
--    , appCurDepModules  :: [ModuleIdent]
    , appPkgTargets     :: [Pkg]
    , appPkgUntargets   :: [Pkg]
    --, appModules        :: Map String
    }

mkApplication :: String -> Application
mkApplication target = Application [target]
                                   M.empty
                                   [] [] []

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

-- | Applying a configuration on system.
runHahet :: Application -> [Flag] -> IO [ApplyResult]
runHahet app flags = do
    mlog $ "-- Applying configuration: " ++ getAppIdent app
    return []

