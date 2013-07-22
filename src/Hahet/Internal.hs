{-# LANGUAGE ScopedTypeVariables #-}
-- | All internal datatypes of Hahet.
module Hahet.Internal
--   (
--   -- * Utils
--   mlog, convertFilePath
--
--   -- * C Monad
--   , C(..)
--   , configure
--   , getConf
--
--   -- * H Monad
--   , H
--   , apply
--   , sh
--    
--   -- * Application
--   , Application(..)
--   , mkApplication
--
--   -- ** Accessors/Mutators
--   , appConf
--   , appModule
--   , getAppHierarchy
--   , pushAppModule
--   , popAppModule
--   , pushTarget
--
--    -- ** Execution
--   , Flag(..)
--   , runTarget
--   , runHahet
--
--    -- * Modules
--   , ModuleIdent
--   , HahetModule(..)
--   , ($*)
--
--   -- * Targets
--   , Target(..)
--   , ApplyResult(..)
--   , Conflict
--   , AppTarget(..)
--   -- ** Sh
--   , AfterSh(..), AfterH(..)
--   -- ** Deps
--   , TargetGroup(..)
--   , (==>)
--
--   )
       where

import Debug.Trace as D
import Prelude hiding (FilePath)
import qualified Data.Traversable as Tr
import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State as State
import System.Log.FastLogger
import Data.Monoid
import Data.Text (Text)
import Data.Typeable
import Data.Tree
import Data.List
import qualified Data.Text as T
import Shelly
import qualified System.IO as IO
import qualified System.Console.ANSI as A
import Text.InterpolatedString.Perl6
import Language.Haskell.TH

import Hahet.Logging

default (Text)

tr :: Show a => a -> a
tr a = D.traceShow a a

-- | The C monad is used to build an Application.
newtype C conf a = C { unC :: ReaderT conf (StateT (Application conf) IO) a }
    deriving (Monad, MonadIO, MonadReader conf, MonadState (Application conf))
instance MonadLogger (C conf) where monadLoggerLog = defaultLogger State.get

-- | Targets are applied in the H monad.
newtype H conf a = H { unH :: ReaderT (Application conf) Sh a }
    deriving (Monad, MonadIO, MonadReader (Application conf))
instance MonadLogger (H conf) where monadLoggerLog = defaultLogger ask

-- | Helper to convert from shelly FilePath to a String.
convertFilePath :: FilePath -> String
convertFilePath = T.unpack . toTextIgnore

-- * Logging

defaultLogger :: (MonadIO m, ToLogStr msg) =>
    m (Application conf) -> Loc -> LogSource -> LogLevel -> msg -> m ()
defaultLogger how_app loc logsource loglevel msg = do
    app <- how_app

    let reset         = A.setSGRCode [A.Reset]
        color         = A.setSGRCode $ case loglevel of
            LevelInfo  -> [A.Reset]
            LevelDebug -> [A.SetColor A.Foreground A.Vivid A.Black ]
            LevelError -> [A.SetColor A.Foreground A.Vivid A.Red     ]
            LevelWarn  -> [A.SetColor A.Foreground A.Vivid A.Yellow  ]
            LevelOther "status" -> [ A.SetColor A.Foreground A.Dull  A.Cyan  ]
            LevelOther "action" -> [ A.SetColor A.Foreground A.Vivid A.Green ]
            LevelOther{} -> [A.SetColor A.Foreground A.Dull A.Yellow   ]
        heading = case loglevel of
            LevelDebug          -> [ LS [qc|{logsource} {loc_filename loc} {loc_start loc} |] ]
            LevelError          -> [LS "error:  "]
            LevelWarn           -> [LS "warn:   "]
            LevelOther "status" -> [LS "status: "]
            LevelOther "action" -> [LS "==> "]
            _                   -> []
        footing = case loglevel of
            LevelDebug ->  []
            _ -> []
    liftIO $ loggerPutStr (appLogger app) $
        [ LS color ] ++ heading ++
        [ LS reset, toLogStr msg ] ++ footing ++
        [ LS "\n" ]

-- | Logging system - for now :)
mlog :: MonadIO m => String -> m ()
mlog = liftIO . putStrLn

-- * Configuration

-- | Running a configuration monad
configure :: conf -> C conf () -> IO (Application conf)
configure c cm = do
    app <- mkApplication c
    ((), res_app) <- runStateT (runReaderT (unC cm') c) app
    return res_app
  where
      -- Logging configuration build start/end.
      cm' = do $status "Starting configuration check"
               cm
               $status "Configuration check done"

getConf :: C conf conf
getConf = ask

pushModule :: ModuleIdent -> C conf ()
pushModule mident = do
    $debug [qc| Add module "{ mident }"|]
    modify $ \app -> app{ appModuleHiera = appModuleHiera app ++ [mident] }

popModule :: C conf ()
popModule = modify $ \app -> app{ appModuleHiera = init $ appModuleHiera app }

pushTarget :: String -> AppTarget conf -> C conf ()
pushTarget tident target = do
    $debug [qc| Add target "{ tident }"|]

    modify $ \app -> app{
        appTargets = liftA2 pushTarget' appModuleHiera appTargets app
    }
    where
        --pushTarget' :: [ModuleIdent] -> Forest (String, Maybe (AppTarget conf)) -> Forest (String, Maybe (AppTarget conf))
        pushTarget'     [] forest = (Node $ Right (tident, target)) [] : forest
        pushTarget' (x:xs) forest = case partition (isModule x) forest of
            ([n], ns) -> n{ subForest = pushTarget' xs (subForest n) } : ns
            ([ ], ns) -> Node (Left x) []                              : ns
            _         -> error "Tried to use the same module twice.  This is not implemented, and may never be."

        isModule a (Node (Left b) _) = a == b --fst (rootLabel n)
        isModule _ _                 = False

-- * Application

-- | Application is a configuration instance.
--   Contains everything needed to apply the configuration.
data Application conf = Application
    { appConf          :: conf             -- ^ Origin of the application.
    , appLogger        :: Logger
    , appModuleHiera   :: [ModuleIdent]    -- ^ Configuration time module hierarchy. Used for logging.
    , appTargets       :: Targets conf
    }

type Targets conf = Forest (Either ModuleIdent (String, AppTarget conf))

prettyPrintTargets :: Targets conf -> String
prettyPrintTargets = drawForest . map (fmap f)
    where f (Left m)    = m
          f (Right (x,_)) = x

-- | Create an empty application of configuration @conf@.
mkApplication :: conf -> IO (Application conf)
mkApplication conf = do
    let logHandle = IO.stdout -- XXX: duplicate to a file?
    logger <- mkLogger True logHandle
    return $ Application conf logger [] []

-- | Run a shell action inside a H monad.
sh :: Sh a -> H conf a
sh = H . ReaderT . const

getConfiguration :: H conf conf
getConfiguration = asks appConf

-- ** Results

type Results conf = Forest (String, ApplyResult)

prettyPrintResults :: Results conf -> String
prettyPrintResults = drawForest . map (fmap fst)

-- ** Running

-- | Apply-time flags for an Application.
data Flag = ModuleFlag Text
          | DevFlag    Text

-- | Apply some action in the 'H' monad on an 'Application'.
runH :: Application conf -> H conf a -> IO a
runH app h = shelly $ runReaderT (unH h) app

apply' :: Typeable conf => H conf (Results conf)
apply' = do
    app <- ask
    $logInfo [qc|Applying configuration { show . typeOf $ appConf app }|]
    $logDebug [qc|Configuration: { prettyPrintTargets (appTargets app) }|]
    mapM (Tr.mapM handleTarget) (appTargets app)
    where
        handleTarget (Left  mident)               = do
            $logInfo [qc|At module { mident }|]
            return (mident, ResNoop)
        handleTarget (Right (tident, MkTarget t)) = do
            $logDebug [qc|Applying target {typeOf t}|]
            r <- targetApply t
            return (tident, r)

-- | Apply the configuration on the system.
apply :: Typeable conf => Application conf -> IO (Results conf)
apply app = runH app apply'

-- * Modules

type ModuleIdent = String

-- | Steps for writing a module: --  1. Create a base datatype for the module (its configuration etc.)
--  2. Make it an instance of the HahetModule class.
--  3. Export the datatype and relevant constructors or your configuration
--     interface.
class Typeable mc => HahetModule mc c where
    -- | How to actualize configuration targets from the module data.
    fromModule   :: mc -> C c ()

--    modifyModule :: mc -> (mc -> mc) -> C c mc

($*) :: HahetModule mc c => C c mc -> (mc -> mc) -> C c mc
m $* f = return . f =<< m

-- * Targets

-- | Result from applying a target.
data ApplyResult = ResSuccess       -- ^ Target was successfully applied.
                 | ResFailed Text   -- ^ Target failed to apply.
                 | ResNoop          -- ^ Target had nothing to do.
                 | ResOther Text    -- ^ Something else happened.
                 | ResMany [ApplyResult] -- ^ There may also be many results
    deriving Typeable

-- | Explanation for a conflict.
type Conflict = Text

-- | Targets are the primitives of configurations. They may be applied
-- (@manage@), or revoked (@revoke@).
--
-- Minimal complete definition: @targetApply@.
class (Typeable conf, Typeable target) => Target conf target where

    -- | How to describe a target in (verbose) logging.
    targetDesc :: conf -> target -> Text

    -- | A function which applies the target and returns a @ApplyResult@.
    targetApply    :: target -> H conf ApplyResult

    -- | Apply a list of targets of some type.
    targetApplyAll :: [target] -> H conf [ApplyResult]
    targetApplyAll = mapM targetApply

    -- | To check whether the target conflicts with another target of the same
    -- type of target. The default implementation assumes targets won't
    -- confilct (always returns Nothing).
    targetConflicts :: target -> target -> Maybe Conflict
    targetConflicts _ _ = Nothing

instance Target c t => Target c [t] where
    targetDesc  _ []     = "(no targets)"
    targetDesc  _ (_:[]) = "one target"
    targetDesc  _ xs     = T.pack (show $ length xs) <> " targets"
    targetApply = liftM ResMany . targetApplyAll

-- | For wrapping targets for an Application.
data AppTarget c where
    MkTarget :: Target c a => a -> AppTarget c

newtype AfterSh = AfterSh { unAfterSh :: Sh ApplyResult }
    deriving (Typeable)

newtype AfterH conf a = AfterH { unAfterH :: H conf a }
    deriving ( Monad, MonadIO, MonadReader (Application conf)
             , MonadLogger, Typeable)

instance Typeable c => Target c AfterSh where
    targetDesc _ = return "Shell script"
    targetApply  = shellyNoDir . unAfterSh

instance Typeable c => Target c (AfterH c ApplyResult) where
    targetDesc _ = return "Shell script"
    targetApply = unAfterH

-- ** Flow control

-- | For grouping targets together
data TargetGroup a b = TargetGroup a b
    deriving (Typeable)

instance (Target c a, Target c b) => Target c (TargetGroup a b) where
    targetDesc  _ = return "(group)"
    targetApply (TargetGroup ta tb) = do
        res <- targetApply ta
        case res of
            ResSuccess  -> targetApply tb
            _           -> return res

-- ** Target combinators

-- | On change
(==>) :: (Target c a, Target c b) => a -> b -> TargetGroup a b
x ==> y = TargetGroup x y
infixl 5 ==>
