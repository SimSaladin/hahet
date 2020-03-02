module Hahet.Targets.Script where

-- * Shell

-- | Run a shell action inside a Apply monad.
sh :: Sh a -> Apply c a
sh comp = do
    doDebug <- view rtLogLevel
    Apply . ReaderT $ const $ if doDebug
                              then verbosely comp
                              else comp

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
