-- | The very basic datatypes for configuration creation.
module Hahet.Core.Internals where

import Prelude hiding (FilePath)
import Data.String
import Data.Text (Text)
import Data.Default
import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Shelly
import qualified System.Directory as SD
default (Text)

-- | Logging system - for now :)
mlog :: MonadIO m => String -> m ()
mlog = liftIO . putStrLn

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

-- ** File targets

-- | A file target.
data FileNode = File            FilePath FileSettings AppFileSource
              | Directory       FilePath FileSettings
              | DirectorySource FilePath FileSettings AppDirectorySource
            deriving Typeable

data Permissions = Octal Int Int Int Int -- ^ Set permissions in octal notation.
                 | PermNoop              -- ^ Do not touch permissions.

instance Show Permissions where
    show (Octal i1 i2 i3 i4) = show i1 ++ show i2 ++ show i3 ++ show i4
    show PermNoop = "(No change)"

instance Read Permissions where
    readPrec

type Owner = Text
type Group = Text

-- | Filesystem settings for a file.
data FileSettings = FileSettings
    { fOwner :: Owner
    , fGroup :: Group
    , fPerms :: Permissions
    }

instance Default FileSettings where
    def = FileSettings "" "" PermNoop

-- *** File sources

-- | Some source which provides the content of a file
class FileSource source where
    fileSource :: source -> IO Text

-- | For wrapping source in a File.
data AppFileSource where
    MkFileSource :: FileSource a => a -> AppFileSource

-- | For implementing different means of populating a directory. This could be
-- an archive, git repo or what ever.
class DirectorySource source where
    directorySource :: source -> FilePath -> IO Text

-- | Wrapping @DirectorySource@ to allow storing in a File.
data AppDirectorySource where
    MkAppDirectorySource :: DirectorySource a => a -> AppDirectorySource

instance Target FileNode where
    targetApply (File path settings source) = shellyNoDir $ do
        handlePerms path (fPerms settings)
        test_e path
        return ()

    targetApply (Directory path settings) = shellyNoDir $ do
        handlePerms path (fPerms settings)

    targetConflicts (File p1 _ _)     (File p2 _ _)   = if p1 == p2 then Just "!" else Nothing
    targetConflicts (File p1 _ _)    (Directory p2 _) = if p1 == p2 then Just "!" else Nothing
    targetConflicts (Directory p1 _) (File p2 _ _)    = if p1 == p2 then Just "!" else Nothing
    targetConflicts (Directory p1 _) (Directory p2 _) = if p1 == p2 then Just "!" else Nothing

-- | Usees stat executable
handlePerms :: FilePath -> Permissions -> Sh ()
handlePerms  _ PermNoop = return ()
handlePerms fp new      = do
    current <- liftM (read . T.unpack) $ silently $ cmd "stat" "-c%a" fp
    mlog $ "old: " <> show (current :: Permissions) <> " new: " <> show new
    return ()

-- ** Pkg targets

-- | Pkg reperesents a package. Provides a IsString instance, so you can create
-- a file from a String literal.
data Pkg = Pkg Text
    deriving Typeable

instance Show Pkg where
    show (Pkg txt) = T.unpack txt

instance IsString Pkg where
    fromString = Pkg . T.pack

instance Target Pkg where
    targetApply pkg = mlog $ "Should install package " ++ show pkg


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

    -- XXX: filtering and dependencies
    mapM_ (mapM_ (\(MkTarget t) -> targetApply t)) $ M.elems $ appTargets app

    return []

