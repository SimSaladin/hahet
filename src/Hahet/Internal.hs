{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | Implementation details and the internal API.
module Hahet.Internal where

import           Prelude hiding (FilePath)
import           Control.Applicative
import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text           as T
import           Data.Tree
import           Data.Typeable
import           Shelly
import           System.Log.FastLogger
import           Debug.Trace as D

import           Hahet.Logging

default (Text)

----------------------------------------------------
-- * Types

-- ** Configuration

-- | Hahet configurations
class (Typeable variant, Typeable conf) => Hahet variant conf where

type ConfigId = Text

-- | Configurations are defined in the C Monad.
--
-- In C conf a, we have access to
--  
--      1. Read-only configuration data conf which holds parameters for the
--         configuration,
--      2. Actual configuration as Configuration conf, and
--      3. Support to log messages from configuring stage.
--
-- Note that C is pure, so no I/O operations may be performed at
-- configuration stage. This way we ensure that the resulting application
-- is independant of the system it was built on, and therefore can be
-- applied to any system.
newtype C variant conf a = C {
    unC :: RWS conf [LogMsg] (Application variant) a
    } deriving ( Functor, Applicative, Monad, MonadReader conf
               , MonadWriter [LogMsg], MonadState (Application variant))

-- ** Apply

-- | An @Application config@ holds everything necessary to apply the
-- configuration against a system given necessary parameters. The system
-- may be the local system or the configuration may be applied over SSH.
data Application variant = Application
         { _appTargets :: [AppTarget variant] }

data AppTarget variant where
    MkTarget :: Target variant conf target => conf -> target -> AppTarget variant
    MkSubApp :: Application variant -> AppTarget variant

-- | System-changing operations are described in @Apply@.
newtype Apply variant conf a = Apply {
    unApply :: ReaderT (conf, RTConf variant) Sh a
    } deriving ( Functor, Applicative, Monad, MonadIO
               , MonadReader (conf, RTConf variant))

-- | Run-time configuration
data RTConf variant = RTConf
    { _rtApplication  :: Application variant
    , _rtLogLevel     :: Bool -- ^ Debug or not
    , _rtLogger       :: Logger
    }

-- ** Targets

-- | Targets are the primitives of configurations. They may be applied
-- (@manage@), or revoked (@revoke@).
--
-- Minimal complete definition: @targetApply@.
class (Hahet variant conf, Typeable target) => Target variant conf target where

    -- | How to describe a target in (verbose) logging. Default
    -- implementation: `typeOf target`.
    targetDesc :: conf -> target -> Text
    targetDesc _ = T.pack . show . typeOf

    -- | Executes the target. Returns the results from doing so. See
    -- @ApplyResult@ for possible results.
    targetApply :: target -> Apply variant conf ApplyResult

    -- | Apply a list of targets of some type. Default implementation:
    -- `mapM targetApply`. May be overridden for efficiency and finer
    -- control.
    targetApplyAll :: [target] -> Apply variant conf [ApplyResult]
    targetApplyAll = mapM targetApply

    -- | To check whether the target conflicts with another target of the same
    -- type of target. The default implementation assumes targets won't
    -- confilct (always returns Nothing).
    targetConflicts :: target -> target -> Maybe Conflict
    targetConflicts _ _ = Nothing

--instance Target c t => Target c [t] where
--    targetDesc _    []  = "(no targets)"
--    targetDesc _ (_:[]) = "one target"
--    targetDesc _ xs     = T.pack (show $ length xs) <> " targets"
--    targetApply = liftM ResMany . targetApplyAll

-- | Explanation for a possible conflict between two targets (of same type).
type Conflict = Text

-- ** Results

-- | Result from applying a target.
data ApplyResult = ResSuccess       -- ^ Target was successfully applied.
                 | ResFailed Text   -- ^ Target failed to apply.
                 | ResNoop          -- ^ Target had nothing to do.
                 | ResOther Text    -- ^ Something else happened.
                 | ResMany [ApplyResult] -- ^ There may also be many results
    deriving Typeable

-- | Apply-time flags.
data Flag = ModuleFlag Text
          | DevFlag    Text

-- | The results from applying a configuration.
type Results conf = Forest (String, ApplyResult)

----------------------------------------------------
-- * Base machinery

-- Note that functions below use these lenses! ----------
makeLenses ''Application
makeLenses ''RTConf

-- ** Evaluate configuration

-- | Evaluate a configuration to an application.
runConfig :: Hahet variant conf => conf -> C variant conf () -> (Application variant, [LogMsg])
runConfig conf comp = (app, logs)
    where
        (_, app, logs) = applyConfig conf initApplication comp

applyConfig :: Hahet variant conf => conf -> Application variant -> C variant conf a
            -> (a, Application variant, [LogMsg])
applyConfig conf app c = runRWS (unC c) conf app

-- | Include a configuration inside a configuration isolated from the
-- parent configuration.
section :: (Hahet variant conf0, Hahet variant conf1)
        => C variant conf1 a -> conf1 -> C variant conf0 a
section comp conf = do
        addTarget (MkSubApp subapp)
        mapM_ (logc . LogMsgFrom (T.pack . show $ typeOf conf)) sublogs
        return res
    where
        (res, subapp, sublogs) = applyConfig conf initApplication comp

addTarget :: Hahet variant conf => AppTarget variant -> C variant conf ()
addTarget target = do
    modify $ Application . (:) target . _appTargets

-- | Create a new empty application.
--
-- Note that applying an empty application is the same as not doing
-- anything at all.
initApplication :: Application variant
initApplication = Application [] -- c mempty mempty

getConf :: C variant conf conf
getConf = ask

-- * Logging

instance Hahet variant conf => MonadLogger (Apply variant conf) where
    monadLoggerLog loc logsource loglevel msg =
        applyLogger loc logsource loglevel msg =<< view (_2.rtLogger)

-- | A debug message
debug :: Text -> C variant conf ()
debug = logc . LogMsgDebug

logc :: LogMsg -> C variant conf ()
logc = tell . return

-- * Utilities

resNoop :: Monad m => m ApplyResult
resNoop = return ResNoop

resSuccess :: Monad m => m ApplyResult
resSuccess = return ResSuccess

resFailed :: Monad m => Text -> m ApplyResult
resFailed = return . ResFailed

ppApplication :: Application variant -> String
ppApplication = undefined -- drawForest . map (either id fst <$>)

ppResults :: Results c -> String
ppResults = drawForest . map (fmap fst)

-- | traceShow a a
tr :: Show a => a -> a
tr a = D.traceShow a a
