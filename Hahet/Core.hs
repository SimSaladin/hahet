-- | API exposed from internal.
module Hahet.Core ( module Hahet.Core ) where

import Data.Text
import Data.Typeable
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Hahet.Core.Internals         as Hahet.Core
import Hahet.Core.PackageManager    as Hahet.Core

-- ** Configuration functions

-- | Use a module. Loads up the module, checks dependency conflicts.
-- Conflicts are logged to stdout.
use :: HahetModule mconf
    => mconf -> ConfMonad c ()
use mconf = do
    mlog $ "module:        " ++ modId
    modify (pushAppModule modId)
    hmInit mconf -- run the module configuration initialization function
    -- check for system-wide daemon conficts
    where
        modId = show (typeOf mconf)

-- | Make the module manage a file.
manageFile :: FilePath -> (FilePath -> IO Text) -> ConfMonad conf ()
manageFile fp content = do
    mlog $ "manage file:    " ++ fp

manageDir :: FilePath -> ConfMonad conf ()
manageDir fp = do
    mlog $ "manage dir:     " ++ fp

revokeFile :: FilePath -> ConfMonad conf ()
revokeFile fp = do
    mlog $ "revoke file:    " ++ fp

requirePkg :: Pkg -> ConfMonad conf ()
requirePkg pkg = do
    -- check for package conflicts
    mlog $ "require pkg:    " ++ show pkg
