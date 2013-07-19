-- | Handling files
module Hahet.Core.FileTargets where

import Prelude hiding (FilePath)
import Data.String
import Data.Text (Text)
import Data.Default
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Typeable
import Control.Monad
import Shelly
import Text.Read as R
import Hahet.Core.Internals
default (Text)

--

-- * Permissions

data Permissions = PermOctal Int Int Int -- ^ Permissions in octal notation (ugo).
                 | PermNoop              -- ^ Do not change permissions.
    deriving (Eq)

instance Show Permissions where
    show (PermOctal i1 i2 i3) = show i1 ++ show i2 ++ show i3
    show PermNoop = "(No change)"

instance Read Permissions where
    readPrec = parens $ do
        owner <- liftM permCheck R.get
        group <- liftM permCheck R.get
        other <- liftM permCheck R.get
        return $ PermOctal owner group other 
        where
            permCheck x = let n = read [x]
                          in if n <= 7
                          then n
                          else error $ "Permission out of range: " ++ [x]

instance IsString Permissions where
    fromString = read -- [owner, group, other] = read


-- * Files

type Owner = Text
type Group = Text

-- | Filesystem settings for a file.
data FileSettings = FileSettings
    { fOwner :: Owner
    , fGroup :: Group
    , fPerms :: Permissions
    }
instance Default FileSettings where
    def = FileSettings "" "" PermNoop

-- | A file target.
data FileNode = File            FilePath FileSettings AppFileSource
              | Directory       FilePath FileSettings
              | DirectorySource FilePath FileSettings AppDirectorySource
            deriving Typeable

-- | Some source which provides the content of a file
class FileSource source where
    fileSource :: source -> IO Text

-- | For wrapping source in a File.
data AppFileSource where
    MkFileSource :: FileSource a => a -> AppFileSource

-- | For implementing different means of populating a directory. This could be
-- an archive, git repo or what ever.
class DirectorySource source where
    directorySource :: source -> FilePath -> IO Text

-- | Wrapping @DirectorySource@ to allow storing in a File.
data AppDirectorySource where
    MkAppDirectorySource :: DirectorySource a => a -> AppDirectorySource

instance Target FileNode where
    targetApply (File path settings source) = shellyNoDir $ do
        handlePerms path (fPerms settings)
        test_e path
        return ()

    targetApply (Directory path settings) = shellyNoDir $ do
        handlePerms path (fPerms settings)

    targetConflicts (File p1 _ _)     (File p2 _ _)   = if p1 == p2 then Just "!" else Nothing
    targetConflicts (File p1 _ _)    (Directory p2 _) = if p1 == p2 then Just "!" else Nothing
    targetConflicts (Directory p1 _) (File p2 _ _)    = if p1 == p2 then Just "!" else Nothing
    targetConflicts (Directory p1 _) (Directory p2 _) = if p1 == p2 then Just "!" else Nothing

getPerms :: FilePath -> Sh Permissions
getPerms fp = liftM (read . T.unpack) . silently $ cmd "stat" "-c%a" fp

-- | Usees stat executable
handlePerms :: FilePath -> Permissions -> Sh ()
handlePerms  _ PermNoop = return ()
handlePerms fp new      = do
    cur <- getPerms fp
    if cur == new
        then return ()
        else do
            mlog ("Perm change: " <> show cur <> " => " <> show new)
            cmd "chmod" (show new) fp
