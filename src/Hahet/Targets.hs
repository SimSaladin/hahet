-- | The Targets interface. You should only need to import this when writing
-- custom Targets.
module Hahet.Targets 
    ( Target(..)
    , Conflict
    , H, sh
    , getConfiguration

    -- * Results
    , ApplyResult(..)

    -- * Misc
    , mlog, convertFilePath
    ) where

import Hahet.Internal
