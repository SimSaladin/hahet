-- | Targets interface.
module Hahet.Targets 
    ( Target(..)
    , Conflict
    , Apply, sh
    , getConfiguration

    , module Hahet.Logging

    -- * Results
    , ApplyResult(..)

    -- * Misc
    , convertFilePath
    ) where

import Hahet.Internal
import Hahet.Logging
