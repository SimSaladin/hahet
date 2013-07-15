-- | Defines the package manager interface
module Hahet.Core.PackageTargets where

import Prelude
import Data.Text
import qualified Data.Text as T
import Data.String
import Data.Typeable

import Hahet.Core.Internals

data Pacman = Pacman

type PkgVersion = Text

-- | Pkg reperesents a package. Provides a IsString instance, so you can create
-- a file from a String literal.
data Pkg = Pkg Text
    deriving Typeable

instance Show Pkg where
    show (Pkg txt) = T.unpack txt

instance IsString Pkg where
    fromString = Pkg . T.pack

instance Target Pkg where
    targetApply pkg = mlog $ "Should install package " ++ show pkg
