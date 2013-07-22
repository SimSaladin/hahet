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
    results <- 'apply' app
    return ()
@

-}
module Hahet 
    (
    -- * Configuration
    C, ($*), use, convertFilePath

    -- ** Logging
    , logDebug, logInfo, logWarn, logError

    -- ** Applying
    , configure, apply

    -- *** Results
    , prettyPrintResults


    -- *** Lower-level
    , runH, apply'

    -- * Targets
    , module Hahet.Targets.Packages
    , module Hahet.Targets.FileNodes
    , module Hahet.Targets.Services
    , manage, revoke
    , ApplyResult(..)
    , AfterSh(..), AfterH(..), sh
    ) where

import           Control.Monad.Logger
import qualified Data.Text  as T

import           Hahet.Internal
import           Hahet.Targets.Packages
import           Hahet.Targets.FileNodes
import           Hahet.Targets.Services
import           Hahet.Imports

-- * Configuring

-- | Use a module. Loads up the module, checks dependency conflicts.
-- Conflicts are logged to stdout.
use :: HahetModule mconf c => mconf -> C c ()
use mconf = let mident = show $ typeOf mconf
                in do
    $(logDebug) ("Entered module " <> T.pack mident)
    pushModule mident
    fromModule mconf
    popModule

-- | Require a target to be applied.
manage :: (Typeable conf, Target conf target) => target -> C conf ()
manage target = do
    conf <- getConf
    let tident = show $ typeOf target
        desc   = targetDesc conf target

    $(logDebug) ("Reached target " <> T.pack tident <> ": " <> desc)
    pushTarget (tident ++ ": " ++ T.unpack desc) (MkTarget target)

revoke :: Target c target => target -> C c ()
revoke _ = error "Target revoking not implemented"
