-- | Modules in internal Hahet.
module Hahet.Core.Module where

import Hahet.Core.PackageManager

-- | Modules, "Recipes"
class HahetModule mconf where
    dependsOnPkgs     :: mconf -> [Pkg]
    conflictsWithPkgs :: mconf -> [Pkg]
    dependsOnPkgs     = const []
    conflictsWithPkgs = const []
