{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Package management.
--
-- To use:
--
-- >   data MyConf = ... deriving (Typeable)
-- >
-- >   instance PackageManagement MyConf where
-- >      pkgManager _ = pacman -- example
-- > 
-- >   myconf = manage ("openssh" :: Pkg)
--
module Hahet.Targets.Packages
    ( Pkg(..)
    , PackageManagement(..)
    -- * Package managers
    -- ** Pacman
    , pacman, PacmanConf(..)
    ) where

import Hahet.Targets
import Hahet.Imports
import Data.String
import qualified Data.Text as T
default(Text)

-- | @Pkg@ reperesents a package. Provides a IsString instance, so packages
-- may be defined as String literals.
data Pkg = Pkg Text deriving (Typeable)
instance IsString Pkg where fromString          = Pkg . T.pack
instance ShellArg Pkg where toTextArg (Pkg txt) = txt
instance Show Pkg     where show      (Pkg txt) = T.unpack txt


-- | 
instance (PkgBackend variant, Hahet variant conf)
        => Target variant conf Pkg where
    targetDesc _ (Pkg txt) = txt
    targetApply pkg        = installPkg undefined pkg

class PkgBackend variant where

    data BackendConf variant :: *

    pkgBackendDef :: BackendConf variant
    installPkg    :: BackendConf variant -> Pkg -> Sh ApplyResult
    revokePkg     :: BackendConf variant -> Pkg -> Sh ApplyResult

-- ** Pacman

-- | Configuration of Pacman

instance PkgBackend arch where
    data BackendConf arch = Pacman { pacmanMirrors :: [Text] }

    installPkg _c pkg    = cec (pacmanQ pkg) resNoop installPkg
        where installPkg = cec (pacmanCmd "-S" pkg) resSuccess
                               (resFailed "Couldn't install the package!")

    revokePkg _c pkg    = cec (pacmanQ pkg) removePkg resNoop -- Package was not installed to begin with
        where removePkg = cec (pacmanCmd "-R" pkg) resSuccess
                              (resFailed "Couldn't revoke the package!")

pacmanQ :: Pkg -> Sh Text
pacmanQ pkg = silently . cmd "pacman" "-Qs" $ "^" ++ show pkg ++ "$"

pacmanCmd :: Text -> Pkg -> Sh Text
pacmanCmd = cmd "pacman" "--noconfirm"

-- | f, zero, one
cec :: Sh a -> Sh b -> Sh b -> Sh b
cec f zero one = errExit False f >> lastExitCode >>= \x -> case x of
      0 -> zero
      1 -> one
      _ -> error "Unhandled exit code."

