-- | Handling files
module Hahet.Targets.FileNodes where

import Prelude hiding (FilePath)
import Control.Monad
import Control.Monad.Trans (lift)
import Data.Default
import Data.Monoid         ((<>))
import Data.String
import Data.Text           (Text)
import qualified Data.Text as T
import Data.Typeable
import Shelly hiding (path)
import Text.Read           as R hiding (lift)

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

-- | Some source which provides the content of a file.
class FileSource source where
    -- TODO: This should be extended to conduits or something to allow easy
    -- efficient big files.
    fileSource :: source -> IO Text
instance FileSource Text where fileSource = return
instance FileSource String where fileSource = return . T.pack

-- | For implementing different means of populating a directory. This could be
-- an archive, git repo or what ever.
class DirectorySource source where
    directorySource :: source -> FilePath -> IO Text

-- | A file target.
data FileNode where
    File            :: FileSource s => FilePath -> FileSettings -> s -> FileNode
    Directory       :: FilePath -> FileSettings -> FileNode
    DirectorySource :: DirectorySource s => FilePath -> FileSettings -> s -> FileNode
        deriving (Typeable)

filenodePath :: FileNode -> FilePath
filenodePath (File            p _ _) = p
filenodePath (Directory       p _  ) = p
filenodePath (DirectorySource p _ _) = p

filenodeSettings :: FileNode -> FileSettings
filenodeSettings (File            _ s _) = s
filenodeSettings (Directory       _ s  ) = s
filenodeSettings (DirectorySource _ s _) = s

instance Target c FileNode where
    targetDesc _ = toTextIgnore . filenodePath

    targetApply fn = lift $ do
        let path     = filenodePath fn
            settings = filenodeSettings fn
        exists <- test_e path
        mlog $ if exists
            then "Path exists"
            else "Path does not exist"
        handlePerms path (fPerms settings)

    targetConflicts a b
        | filenodePath a == filenodePath b = Just "Conflicts"
        | otherwise = Nothing

getPerms :: FilePath -> Sh Permissions
getPerms fp = liftM (read . T.unpack) . silently $ cmd "stat" "-c%a" fp

-- | Usees stat executable
handlePerms :: FilePath -> Permissions -> Sh ApplyResult
handlePerms  _ PermNoop = return ResNoop
handlePerms fp new      = do
    cur <- getPerms fp
    if cur == new
        then return ResNoop
        else do
            mlog ("Perm change: " <> show cur <> " => " <> show new)
            _ <- cmd "chmod" (show new) fp
            return ResSuccess
