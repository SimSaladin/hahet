-- | All internal datatypes of Hahet.
module Hahet.Internal
   (
   -- * Utils
   mlog, convertFilePath

   -- * C Monad
   , C(..), configure

   -- * H Monad
   , H, apply
    
   -- * Application
   , Application(..)
   , mkApplication
   , Flag(..)
   , getConfiguration

    -- * Modules
   , ModuleIdent
   , HahetModule(..)
   , ($*)

   -- * Targets
   , Target(..)
   , ApplyResult(..)
   , Conflict
   , AppTarget(..)
   -- ** Sh
   , AfterSh(..)
   -- ** Deps
   , TargetGroup(..)
   , (==>)

   )  where

import Prelude hiding (FilePath)
import Data.Text (Text)
import Data.Monoid ((<>))
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

-- * Configuration

-- | Configuration monad is used to build an Application.
newtype C conf a = C {
    unC :: ReaderT conf (StateT (Application conf) IO) a
    } deriving (Monad, MonadIO, MonadReader conf, MonadState (Application conf))

-- | Running a configuration monad
configure :: conf -> C conf () -> IO (Application conf)
configure c cm = liftM snd -- discard the monad's result
    $ runStateT (runReaderT (unC cm') c) (mkApplication c)
  where
      -- Logging configuration build start/end.
      cm' = do mlog "-- Starting configuration check --"
               cm
               mlog "--   Configuration check done   --"

-- * Application

-- | Application is a configuration instance.
--   Contains everything needed to apply the configuration.
data Application conf = Application
    { appConf           :: conf -- ^ Origin of the application.
    , appHierarchy      :: [ModuleIdent] -- ^ Defines the modules in order they should be applied.
    , appTargets        :: M.Map ModuleIdent [AppTarget conf] -- ^ Targets are sorted by the module identifier.
    , appFlags          :: [Flag] -- ^ Flags that should be applied.
    }

mkApplication :: conf -> Application conf
mkApplication conf = Application conf
                                 []
                                 M.empty
                                 []

-- | Apply-time flags for an Application.
data Flag = ModuleFlag Text
          | DevFlag    Text

type H conf a = ReaderT (Application conf) Sh a
    --deriving (Monad, MonadIO, MonadReader (Application conf))

apply :: Application conf -> H conf a -> Sh a
apply app h = runReaderT h app

getConfiguration :: H conf conf
getConfiguration = asks appConf

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

-- | Explanation for a conflict.
type Conflict = Text

-- | Targets are the primitives of configurations. They may be applied
-- (@manage@), or revoked (@revoke@).
--
-- Minimal complete implemantation: @targetApply@.
class (Typeable target) => Target c target where

    -- | How to describe a target in (verbose) logging.
    targetDesc :: c -> target -> Text

    -- | A function which applies the target and returns a @ApplyResult@.
    targetApply    :: target -> H c ApplyResult

    -- | Apply a list of targets of some type.
    targetApplyAll :: [target] -> H c [ApplyResult]
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

instance Target c AfterSh where
    targetDesc _ = return "Shell script"
    targetApply  = shellyNoDir . unAfterSh

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
