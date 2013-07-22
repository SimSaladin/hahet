{-# LANGUAGE LambdaCase #-}
-- | Defines the package manager interface
module Hahet.Targets.Packages where

import Hahet.Targets
import Hahet.Imports
import qualified Data.Text as T
import Data.String


-- * Interface

-- | Class which allows defining a package manager for a configuration.
class (Typeable c) => PackageManagement c where
    pkgManager :: c -> PkgManager

-- | Package manager interface.
class PackageManager pm where
    applyConfiguration  ::        pm -> Sh ApplyResult
    installPackage      :: Pkg -> pm -> Sh ApplyResult
    revokePackage       :: Pkg -> pm -> Sh ApplyResult

-- | Pkg reperesents a package. Provides a IsString instance, so you can create
-- a file from a String literal.
data Pkg = Pkg Text
    deriving (Typeable)

instance ShellArg Pkg where
    toTextArg (Pkg txt) = txt

instance Show Pkg where
    show (Pkg txt) = T.unpack txt

instance IsString Pkg where
    fromString = Pkg . T.pack

-- * Managers

-- ** Pacman

data Pacman = Pacman

pacman :: PkgManager
pacman = MkPkgManager Pacman

pacmanCmd :: Text -> Pkg -> Sh Text
pacmanCmd = cmd "pacman" "--noconfirm"

pacmanPkgQuery :: Pkg -> Sh Text
pacmanPkgQuery pkg = silently $ cmd "pacman" "-Qs" $ "^" ++ show pkg ++ "$"

instance PackageManager Pacman where
    applyConfiguration _ = undefined

    installPackage pkg _ = errExit False $ do
        exitCode (pacmanPkgQuery pkg) >>= \case
            0 -> return ResNoop
            1 -> exitCode (pacmanCmd "-S" pkg) >>= return . \case
                0 -> ResSuccess
                1 -> ResFailed "Couldn't install the package!"
                _ -> error "Unhandled exit code."
            _ -> error "Unhandled exit code."

    revokePackage pkg _ = errExit False $ do
        exitCode (pacmanPkgQuery pkg) >>= \case
            0 -> exitCode (pacmanCmd "-R" pkg) >>= return . \case
                0 -> ResSuccess
                1 -> ResFailed "Couldn't revoke the package!"
                _ -> error "Unhandled exit code."
            1 -> return ResNoop -- Package was not installed
            _ -> error "Unhandled exit code."

exitCode :: Sh a -> Sh Int
exitCode f = f >> lastExitCode

-- * Internal implementations

instance PackageManagement c => Target c Pkg where
    targetDesc _ (Pkg txt) = txt
    targetApply pkg = do
        onPkgMgr $ installPackage pkg

data PkgManager where
    MkPkgManager :: PackageManager pm => pm -> PkgManager

onPkgMgr :: PackageManagement c
         => (forall m. PackageManager m => m -> Sh a)
         -> H c a
onPkgMgr f = do
    MkPkgManager mgr <- liftM pkgManager getConfiguration
    sh $ f mgr

