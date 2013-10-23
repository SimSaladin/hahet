{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | Implementation details and the whole internal API.
module Hahet.Internal where

import           Prelude hiding (FilePath)
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List
import           Data.Text (Text)
import qualified Data.Text           as T
import qualified Data.Traversable    as Tr
import           Data.Tree
import           Data.Typeable
import           Language.Haskell.TH
import           Shelly
import qualified System.Console.ANSI as A
import qualified System.IO           as IO
import           System.Log.FastLogger
import           Text.InterpolatedString.Perl6
import           Hahet.Logging
import           Debug.Trace as D
default (Text)

-- * Configuring

-- | @Configuration config@ describes a configuration on a user-provided
-- config data @config@.
--
-- A "Configuration" contains everything necessary to "apply" it on any
-- given system.
data Configuration c = Configuration
        { _aconf     :: c               -- ^ User configuration.
        , _atargets  :: Targets c     -- ^ Targets' configuration.
        , _amodules  :: [ModuleIdent]   -- ^ Configuration time module
                                        -- hierarchy. Used for logging.
                                    {- TODO doesn't really belong here. -}
        }

-- | Configurations are defined in the C monad.
--
-- C does not provide IO. This way we know that the resulting configuration
-- is independant of the system the configuration was built on, and so may
-- be applied to any other system.
newtype C c a = C {
    unC :: RWS c [String] (Configuration c) a
    } deriving ( Functor, Applicative, Monad, MonadReader c
               , MonadWriter [String], MonadState (Configuration c))

-- | Construct the C config on top of the empty configuration.
configure :: (Typeable c)
          => c -> C c r -> (r, Configuration c, [String])
configure uc mc = configureOn (emptyConfiguration uc) uc mc

-- | Construct the C config on top the provided existing configuration.
configureOn :: (Typeable c)
            => Configuration c -> c -> C c r -> (r, Configuration c, [String])
configureOn app conf comp = runRWS (unC comp) conf app

-- | Create a new empty application.
-- Applying an empty application is the same as not doing anything at all.
emptyConfiguration :: c -> Configuration c
emptyConfiguration c = Configuration c mempty mempty

-- | A notice in C.
debug :: String -> C c ()
debug = tell . return

-- * Targets

-- | Targets are the primitives of configurations. They may be applied
-- (@manage@), or revoked (@revoke@).
--
-- Minimal complete definition: @targetApply@.
class (Typeable c, Typeable t) => Target c t where

    -- | How to describe a target in (verbose) logging. Default
    -- implementation: `typeOf target`.
    targetDesc :: c -> t -> Text
    targetDesc _ = T.pack . show . typeOf

    -- | Executes the target. Returns the results from doing so. See
    -- @ApplyResult@ for possible results.
    targetApply :: t -> Apply c ApplyResult

    -- | Apply a list of targets of some type. Default implementation:
    -- `mapM targetApply`. May be overridden for efficiency and finer
    -- control.
    targetApplyAll :: [t] -> Apply c [ApplyResult]
    targetApplyAll = mapM targetApply

    -- | To check whether the target conflicts with another target of the same
    -- type of target. The default implementation assumes targets won't
    -- confilct (always returns Nothing).
    targetConflicts :: t -> t -> Maybe Conflict
    targetConflicts _ _ = Nothing

instance Target c t => Target c [t] where
    targetDesc _    []  = "(no targets)"
    targetDesc _ (_:[]) = "one target"
    targetDesc _ xs     = T.pack (show $ length xs) <> " targets"
    targetApply = liftM ResMany . targetApplyAll

-- | Configuration primitives, targets, are represented as a tree.
type Targets c = Forest (Either ModuleIdent (String, AppTarget c))

-- | To quantify different targets in configurations we wrap them in
-- AppTargets.
data AppTarget c where
    MkTarget :: Target c a => a -> AppTarget c

-- ** Target conflict

-- | Explanation for a possible conflict.
type Conflict = Text

-- * Modules

-- | Modules are logical blocks of configuration. They are identified by
-- their Typeable instances, aka ModuleIdent's.
type ModuleIdent = String

-- | All modules are datatypes that have a "HahetModule" instance.
--
--  1. Create a base datatype for the module (its configuration etc.)
--
--  2. Make it an instance of the HahetModule class.
--
--  3. Export the datatype and relevant constructors or your configuration
--     interface.
class (Typeable m, Typeable c) => HahetModule m c where

    -- | How to actualize configuration targets from the module data.
    fromHM :: m -> C c ()

-- * Results

-- | Result from applying a target.
data ApplyResult = ResSuccess       -- ^ Target was successfully applied.
                 | ResFailed Text   -- ^ Target failed to apply.
                 | ResNoop          -- ^ Target had nothing to do.
                 | ResOther Text    -- ^ Something else happened.
                 | ResMany [ApplyResult] -- ^ There may also be many results
    deriving Typeable

-- | The results from applying a configuration.
type Results conf = Forest (String, ApplyResult)

prettyPrintResults :: Results c -> String
prettyPrintResults = drawForest . map (fmap fst)

-- * Apply

-- | System-changing operations are applied in @Apply@-monad, in which the
-- configuration (@Configuration@) is available.
newtype Apply c a = Apply {
    unApply :: ReaderT (RTConf c) Sh a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (RTConf c))

-- | Run-time configuration
data RTConf c = RTConf
    { _rtConfiguration  :: Configuration c
    , _rtLogLevel       :: Bool -- ^ Debug or not
    , _rtLogger         :: Logger
    }

-- Note that functions below use these lenses! ----------
makeLenses ''Configuration
makeLenses ''RTConf
---------------------------------------------------------

-- | Apply-time flags.
data Flag = ModuleFlag Text
          | DevFlag    Text

-- | Execute an @Apply@ computation given some RTConf.
runApply :: RTConf c -> Apply c a -> IO a
runApply app f = shelly $ runReaderT (unApply f) app

-- | Apply a configuration on the running system.
apply :: (Typeable c) => Configuration c -> IO (Results c)
apply conf = do
    let logHandle = IO.stdout     -- XXX: duplicate to a file?
    logger <- mkLogger True logHandle
    let rtconf = RTConf conf True logger
        in runApply rtconf apply' -- XXX: get application and discrete rtconf instead?

apply' :: (Typeable c) => Apply c (Results c)
apply' = do
    app <- view rtConfiguration
    $status [qc|Applying configuration { show $ typeOf (app ^. aconf) }|]
    $logDebug [qc|Configuration:
{ prettyPrintTargets (app ^. atargets) }|]
    results <- mapM (Tr.mapM handleTarget) (app ^. atargets)
    $status [qc|Configuration applied.|]
    return results
    where
        handleTarget (Left  mident) =
            $status [qc|Apply module { mident }|] >> return (mident, ResNoop)

        handleTarget (Right (tident, MkTarget t)) =
            $logDebug [qc|Applying target {typeOf t}|] >> targetApply t >>= return . (,) tident

-- ** Logging in Apply

instance Typeable c => MonadLogger (Apply c) where
    monadLoggerLog = applyLogger

applyLogger :: (ToLogStr msg, Typeable c)
            => Loc -> LogSource -> LogLevel -> msg -> Apply c ()
applyLogger loc logsource loglevel msg = do
    logger <- view rtLogger
    app    <- view rtConfiguration

    let currentModule = show (typeOf $ app^.aconf)
            ++ "[" ++ (intercalate "." $ app^.amodules) ++ "] "
    liftIO . loggerPutStr logger $
        heading
        ++ ( LS currentModule : LS color : toLogStr msg : footing )
        ++ [ LS reset, LS "\n" ]
    where
        reset         = A.setSGRCode [A.Reset]
        color         = A.setSGRCode $ case loglevel of
            LevelError -> [A.SetColor A.Foreground A.Vivid A.Red     ]
            LevelWarn  -> [A.SetColor A.Foreground A.Vivid A.Yellow  ]
            LevelDebug -> [A.SetColor A.Foreground A.Dull  A.Cyan    ]
            LevelInfo  -> [A.Reset]
            LevelOther "status" -> [ A.SetColor A.Foreground A.Dull  A.Magenta  ]
            LevelOther "action" -> [ A.SetColor A.Foreground A.Vivid A.Green ]
            LevelOther{} -> [A.SetColor A.Foreground A.Dull A.Yellow ]
        heading = case loglevel of
            LevelDebug          -> LS "(" : LS color : LS " Debug " : LS reset : [ LS ") " ]
            LevelError          -> LS "(" : LS color : LS " Error " : LS reset : [ LS ") " ]
            LevelWarn           -> LS "(" : LS color : LS "Warning" : LS reset : [ LS ") " ]
            LevelOther "status" -> LS "(" : LS color : LS "Status " : LS reset : [ LS ") " ]
            LevelOther "action" -> LS "(" : LS color : LS "==> "    : LS reset : [ LS ") " ]
            _                   -> []
        footing = case loglevel of
            LevelDebug -> [ LS $ A.setSGRCode [A.SetColor A.Foreground A.Dull A.Cyan ]
                          , LS [qc|{logsource} {loc_filename loc} {loc_start loc} |]
                          , LS reset ]
            _          -> []

-- * Primitive configuration interface

-- | Include a module name in the module hiera.
pushModule :: ModuleIdent -> C c ()
pushModule mident = do
    modify $ over amodules (++ [mident])

-- | Leaving a module.
popModule :: C c ()
popModule = modify $ over amodules init

pushTarget :: Typeable c => String -> AppTarget c -> C c ()
pushTarget tident target = do
    debug [qc|Target: {tident}|]
    modify $ flip (atargets .~)
         <*> liftA2 (pushTarget' (tident, target)) _amodules _atargets
    debug . show . prettyPrintTargets =<< use atargets
        where
    -- Traverse the targets by module and push the target to right place.
    pushTarget' :: (String, AppTarget c) -> [ModuleIdent] -> Targets c -> Targets c
    pushTarget' e     [] forest = (Node (Right e)) [] : forest
    pushTarget' e (x:xs) forest = ys ++ [y]
      where
          (y, ys) = case partition (isModule x) forest of
                ([ ], ns) -> (Node (Left x) (pushTarget' e xs [           ]), ns)
                ([n], ns) -> (n{ subForest = pushTarget' e xs (subForest n)}, ns)
                _         -> error "Tried to use the same module twice; this is not implemented, for now."

    isModule a (Node (Left b) _) = a == b
    isModule _ _                 = False

-- | Get user configuration.
getConf :: C c c
getConf = ask

-- | Apply a function to a module
($*) :: HahetModule d c => C c d -> (d -> d) -> C c d
m $* f = liftM f m -- TODO useless?

-- * Useful targets

-- ** ApplySh

newtype ApplySh = ApplySh { unApplySh :: Sh ApplyResult }
                  deriving (Typeable)

instance (Typeable c) => Target c ApplySh where
    targetDesc _ = return "Shell script"
    targetApply  = shellyNoDir . unApplySh

-- ** ApplyAfter

newtype ApplyAfter c a = ApplyAfter { unApplyAfter :: Apply c a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader (RTConf c), MonadLogger, Typeable)

instance (Typeable c) => Target c (ApplyAfter c ApplyResult) where
    targetDesc _ = return "After Apply procudere"
    targetApply  = unApplyAfter

-- * Utilities

getConfiguration :: Apply c c
getConfiguration = view (rtConfiguration.aconf)

prettyPrintTargets :: Targets c -> String
prettyPrintTargets = drawForest . map (fmap f)
    where f (Left m)    = m
          f (Right (x,_)) = x

-- | Run a shell action inside a Apply monad.
sh :: Sh a -> Apply c a
sh comp = do
    doDebug <- view rtLogLevel
    Apply . ReaderT $ const $ if doDebug
                              then verbosely comp
                              else comp

tr :: Show a => a -> a
tr a = D.traceShow a a

-- | Helper to convert from shelly FilePath to a String.
convertFilePath :: FilePath -> String
convertFilePath = T.unpack . toTextIgnore
