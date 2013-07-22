module Hahet.Targets.Services where

import Hahet.Targets
import Hahet.Imports

type Started = Maybe Bool

data Service = Service Text Started
    deriving (Typeable, Show)

instance Typeable c => Target c Service where
    targetDesc  _ (Service service _) = service

    targetApply (Service _ Nothing)      = return ResNoop
    targetApply (Service _ (Just False)) = return $ ResFailed "Not yet implemented"
    targetApply (Service _ (Just True))  = return $ ResFailed "Not yet implemented"
