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

-- | Helper to convert from shelly FilePath to a String.
convertFilePath :: FilePath -> String
convertFilePath = T.unpack . toTextIgnore

-- * Targets

-- | Result from applying a target.
data ApplyResult = ResSuccess       -- ^ Target was successfully applied.
                 | ResFailed Text   -- ^ Target failed to apply.
                 | ResNoop          -- ^ Target had nothing to do.
                 | ResOther Text    -- ^ Something else happened.

-- | Explanation for a conflict.
type Conflict = Text

-- | Targets are the primitives of configurations. They may be applied
-- (@manage@), or revoked (@revoke@).
--
-- Minimal complete implemantation: @targetApply@.
class Typeable target => Target target where

    -- | How to describe a target in (verbose) logging.
    targetDesc :: target -> Text

    -- | A function which applies the target and returns a @ApplyResult@.
    targetApply    :: target -> IO ApplyResult

    -- | Apply a list of targets of some type.
    targetApplyAll :: [target] -> IO ()
    targetApplyAll = mapM_ targetApply

    -- | To check whether the target conflicts with another target of the same
    -- type of target. The default implementation assumes targets won't
    -- confilct (always returns Nothing).
    targetConflicts :: target -> target -> Maybe Conflict
    targetConflicts _ _ = Nothing

-- | For wrapping targets for an Application.
data AppTarget where
    MkTarget :: Target a => a -> AppTarget

newtype AfterSh = AfterSh { unAfterSh :: Sh ApplyResult }
    deriving (Typeable)

instance Target AfterSh where
    targetDesc _ = "Shell script"
    targetApply  = shellyNoDir . print_stdout False . unAfterSh

-- ** Flow control

-- | For grouping targets together
data TargetGroup a b = TargetGroup a b
    deriving (Typeable)

instance (Target a, Target b) => Target (TargetGroup a b) where
    targetDesc  _ = "(group)"
    targetApply (TargetGroup ta tb) = do
        res <- targetApply ta
        case res of
            ResSuccess  -> targetApply tb
            _           -> return res

-- * Target combinators

-- | On change
(==>) :: (Target a, Target b) => a -> b -> TargetGroup a b
x ==> y = TargetGroup x y
infixl 5 ==>

-- * Application

type ModuleIdent = String

-- | Apply-time flags for an Application.
data Flag = ModuleFlag Text
          | DevFlag    Text

-- | Application is a configuration instance. Contains everything needed to
--   apply the configuration.
data Application = Application
    { appHierarchy      :: [ModuleIdent]                 -- ^ Defines the modules in order they should be applied.
    , appTargets        :: M.Map ModuleIdent [AppTarget] -- ^ Targets are sorted by the module identifier.
    , appFlags          :: [Flag]                        -- ^ Flags that should be applied.
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
