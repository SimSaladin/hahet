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

-- | Configurations implement package management by implementing
-- @pkgManager@. Some common package managers are provided.
class (Typeable conf) => PackageManagement conf where
    pkgManager :: conf -> PkgManager

-- | @Pkg@ reperesents a package. Provides a IsString instance, so packages
-- may be defined as String literals.
data Pkg = Pkg Text deriving (Typeable)
instance IsString Pkg where fromString          = Pkg . T.pack
instance ShellArg Pkg where toTextArg (Pkg txt) = txt
instance Show Pkg     where show      (Pkg txt) = T.unpack txt
instance PackageManagement c => Target c Pkg where
    targetDesc _ (Pkg txt) = txt
    targetApply pkg        = onPkgMgr $ installPackage pkg

-- * Managers

-- | The package manager interface.
class PackageManager pm where
    applyConfiguration  ::        pm -> Sh ApplyResult
    installPackage      :: Pkg -> pm -> Sh ApplyResult
    revokePackage       :: Pkg -> pm -> Sh ApplyResult

-- | Package manager existential.
data PkgManager where
    MkPkgManager :: PackageManager pm => pm -> PkgManager

onPkgMgr :: PackageManagement c
         => (forall m. PackageManager m => m -> Sh a) -> Apply c a
onPkgMgr f = do
    MkPkgManager mgr <- liftM pkgManager getConfiguration
    sh $ f mgr

-- ** Pacman

pacman :: PacmanConf -> PkgManager
pacman pc = MkPkgManager (Pacman pc)

-- | Configuration of Pacman
data PacmanConf = PacmanConf
data Pacman     = Pacman PacmanConf

instance PackageManager Pacman where
    applyConfiguration _ = undefined
    installPackage pkg _ = caseExitCode (pacmanPkgQuery pkg) (return ResNoop) installPkg
        where installPkg = caseExitCode
                (pacmanCmd "-S" pkg) (return ResSuccess)
                (return $ ResFailed "Couldn't install the package!")
    revokePackage pkg _ = caseExitCode (pacmanPkgQuery pkg) removePkg (return ResNoop) -- Package was not installed to begin with
        where removePkg = caseExitCode
                (pacmanCmd "-R" pkg) (return ResSuccess)
                (return $ ResFailed "Couldn't revoke the package!")

pacmanPkgQuery :: Pkg -> Sh Text
pacmanPkgQuery pkg = silently $ cmd "pacman" "-Qs" $ "^" ++ show pkg ++ "$"

pacmanCmd :: Text -> Pkg -> Sh Text
pacmanCmd = cmd "pacman" "--noconfirm"

-- | f, zero, one
caseExitCode :: Sh a -> Sh b -> Sh b -> Sh b
caseExitCode f zero one = errExit False f >> lastExitCode >>= \x -> case x of
                  0 -> zero
                  1 -> one
                  _ -> error "Unhandled exit code."

