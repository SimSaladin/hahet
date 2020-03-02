-- | Targets interface.
module Hahet.Targets 
    ( Hahet

    , Target(..)
    , Conflict
    , Apply

    , getConf
    , resNoop, resSuccess, resFailed

    -- * Results
    , ApplyResult(..)

    , module Hahet.Logging
    ) where

import Hahet.Internal
import Hahet.Logging
