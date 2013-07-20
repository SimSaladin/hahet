module Hahet.Core.ServiceTargets where

import Prelude
import Data.Text (Text)
import Data.Typeable

import Hahet.Core.Internals

type Started = Maybe Bool

data Service = Service Text Started
    deriving (Typeable, Show)

instance Target Service where
    targetDesc (Service service _) = service
    targetApply (Service service Nothing) = return ResNoop
    targetApply (Service service mstate ) 
        | mstate == Just False = return $ ResFailed "Not yet implemented"
        | mstate == Just True  = return $ ResFailed "Not yet implemented"
