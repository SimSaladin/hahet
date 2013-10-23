{-# LANGUAGE ScopedTypeVariables #-}
{-|
Hahet is a system configuration management framework for Haskell. To get
started, just import two modules:

@
import Hahet
import "Hahet.Imports"
default("Text") -- not mandatory
@

An Example.

Here is a complete example of setting system timezone:

@
import Hahet
import "Hahet.Imports"
import "Hahet.Modules.DateTimeManager"
default('Text')

main = let
    (_, conf, _) = "configure" () $ do
            "use" $ "DateTime" \"Europe/Helsinki\"
    in "apply" conf >>= putStrLn . "prettyPrintSesults"
@

For targets see "Hahet.Targets", and for modules see "Hahet.Modules".

-}
module Hahet 
    (
    -- * The basics
    Configuration
    , C
    , configure
    , apply
    , prettyPrintResults
    , use

    -- ** Targets
    , manage, revoke
    , module Hahet.Targets.Packages
    , module Hahet.Targets.FileNodes
    , module Hahet.Targets.Services

    -- * Utilities
    , ($*)
    , convertFilePath

    -- * Logging
    , logDebug, logInfo, logWarn, logError 

    -- * Extending

    -- ** Targets
    , ApplyResult(..)
    , ApplySh(..)
    , ApplyAfter(..)
    , sh

    -- ** Modules
    , HahetModule(..)

    -- * Advanced
    , configureOn
    , emptyConfiguration
    ) where

import           Control.Monad.Logger
import qualified Data.Text  as T
import           Hahet.Internal
import           Hahet.Targets.Packages
import           Hahet.Targets.FileNodes
import           Hahet.Targets.Services
import           Hahet.Imports

-- * Configuring

-- | Use "use" to import a module to a configuration.  Modules are
-- collections of "Target"s, which are the smallest pieces of configuration
-- in Hahet.
use :: HahetModule d c => d -> C c ()
use mconf = pushModule mident >> fromHM mconf >> popModule
    where
        mident = show $ typeOf mconf

-- | Use "manage" to add a target to a configuration.
manage :: (Typeable c, Typeable t, Target c t)
       => t -> C c ()
manage target = do
    conf <- getConf
    let tident = show $ typeOf target
        desc   = targetDesc conf target
    pushTarget ("[ " ++ tident ++ "; " ++ T.unpack desc ++ " ]")
               (MkTarget target)

-- | "revoke" makes sure that a target is not applied.  Different target
-- types provide their own means to identify overlapping targets.
revoke :: Target c t => t -> C c ()
revoke _ = error "Target revoking not implemented"
