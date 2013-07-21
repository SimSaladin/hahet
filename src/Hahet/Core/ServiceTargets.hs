module Hahet.Core.ServiceTargets where

import Prelude
import Data.Text (Text)
import Data.Typeable

import Hahet.Core.Internals

type Started = Maybe Bool

data Service = Service Text Started
    deriving (Typeable, Show)

instance Target c Service where
    targetDesc  (Service service _) = return service

    targetApply (Service _ Nothing)      = return ResNoop
    targetApply (Service _ (Just False)) = return $ ResFailed "Not yet implemented"
    targetApply (Service _ (Just True))  = return $ ResFailed "Not yet implemented"
