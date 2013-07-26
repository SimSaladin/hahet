-- | The Targets interface. You should only need to import this when writing
-- custom Targets.
module Hahet.Targets 
    ( Target(..)
    , Conflict
    , H, sh
    , getConfiguration

    , module Hahet.Logging

    -- * Results
    , ApplyResult(..)

    -- * Misc
    , convertFilePath
    ) where

import Hahet.Internal
import Hahet.Logging
