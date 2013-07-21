{-|
Hahet public API.

Intented for writing configurations. For targets see "Hahet.Targets", and for modules see "Hahet.Modules".

For best usage I suggest using something along the lines of this in your
configurations:

@
import Hahet
import "Hahet.Imports" -- Exports many conveniences, see documentation for details.
default("Text")
@

-- * Example

A complete functioning example:

(not done yet)

@
module Main where

import Hahet
import "Hahet.Imports"
default("Text")

main :: IO ()
main = do
    app     <- 'configure' ...
    results <- 'runHahet' app
    return ()
@

-}
module Hahet 
    (
    -- * Configuration
    C, ($*), use

    -- ** Convenience
    , mlog, convertFilePath

    -- ** Applying
    , configure, runHahet

    -- * Targets
    , module Hahet.Targets.Packages
    , module Hahet.Targets.FileNodes
    , module Hahet.Targets.Services
    , manage, revoke
    , ApplyResult(..)
    , AfterSh(..)
    ) where

import           Hahet.Imports
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text  as T
import           Hahet.Internal
import           Hahet.Targets.Packages
import           Hahet.Targets.FileNodes
import           Hahet.Targets.Services
import           Hahet.Apply

-- * Configuring

-- | Use a module. Loads up the module, checks dependency conflicts.
-- Conflicts are logged to stdout.
use :: HahetModule mconf c => mconf -> C c ()
use mconf = do
    mlog $ "Entered module " ++ modId
    modify (pushAppModule modId)
    fromModule mconf -- run the module configuration initialization function
                 -- XXX: check for system-wide daemon conficts
    where
        modId = show (typeOf mconf)

-- | Require a target to be applied.
manage :: (Target c target) => target -> C c ()
manage t = do
    c <- ask
    mlog   $ "Added target "
        ++ show (typeOf t)
        ++ ": "
        ++ T.unpack (targetDesc c t)
    modify $ pushTarget t

revoke :: Target c target => target -> C c ()
revoke _ = error "Target revoking not implemented"
