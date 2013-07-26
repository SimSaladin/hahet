{-# LANGUAGE LambdaCase, EmptyDataDecls #-}
-- |
-- FileNodes target files and directories: their owner, group, permissions and
-- content.
--
-- Ensure that directory /etc exists and is owned by root with perms 755:
--
-- > manage $ Directory "/etc"
-- >     /- setOwner "root"
-- >     /- setPerms "755"
-- 
-- Ensure that a file /etc/hostname exists, is owned by root, has 755 permissions and
-- the content "myhost".
--
-- > manage $ File "/etc/hostname"
-- >     /- setOwner "root"
-- >     /- setPerms "755"
-- >     /- fileSource "myhost"
module Hahet.Targets.FileNodes 
    ( FileNode(..), file, directory
    -- , FileSettings
    --, PlainFile, SourcedFile, PlainDirectory, SourcedDirectory

    -- * Properties
    , (/-)
    , Owner, Group
    , Permissions(..)
    , setOwner, setGroup, setPerms

    -- * Sources
    , FileSource(..)
    , DirectorySource(..)
    , fileSource
    , directorySource
    ) where

import           Hahet.Targets
import           Hahet.Imports hiding (writeFile, path)

import           Data.List            (intercalate)
import           Data.String
import qualified Data.Text     as T
import           Data.Text.IO
import           Text.Read     as R
default (Text)

data PlainFile deriving Typeable
data SourcedFile deriving Typeable
data PlainDirectory deriving Typeable
data SourcedDirectory deriving Typeable

data FileNode a where
    File             ::                      FileSettings -> FileNode PlainFile
    Directory        ::                      FileSettings -> FileNode PlainDirectory
    FileSourced      :: FileSource      s => FileSettings -> s -> FileNode SourcedFile
    DirectorySourced :: DirectorySource s => FileSettings -> s -> FileNode SourcedDirectory
    deriving (Typeable)

instance (Typeable c, Typeable a) => Target c (FileNode a) where
    targetDesc  _ = toTextIgnore . filenodePath
    targetApply   = applyFileNode
    targetConflicts a b | filenodePath a == filenodePath b = Just "Conflicts"
                        | otherwise = Nothing

-- | (Internal)
mkFileSettings :: FilePath -> FileSettings
mkFileSettings txt = FileSettings txt Nothing Nothing PermNoop

file :: FilePath -> FileNode PlainFile
file = File . mkFileSettings

directory :: FilePath -> FileNode PlainDirectory
directory = Directory . mkFileSettings

-- | File properties.
data FileSettings = FileSettings
    { fPath  :: FilePath
    , fOwner :: Maybe Owner
    , fGroup :: Maybe Group
    , fPerms :: Permissions
    }

instance IsString FileSettings where
    fromString = mkFileSettings . fromText . T.pack

-- | Specialized @flip ($)@ to set properties in infix.
(/-) :: FileNode a -> (FileNode a -> FileNode b) -> FileNode b
(/-) = flip ($)

-- * Properties

filenodeSettings :: FileNode a -> FileSettings
filenodeSettings (File             s  ) = s
filenodeSettings (Directory        s  ) = s
filenodeSettings (FileSourced      s _) = s
filenodeSettings (DirectorySourced s _) = s

filenodeSettingsAlter :: FileNode a -> (FileSettings -> FileSettings) -> FileNode a
filenodeSettingsAlter (File             s  ) f = File (f s)
filenodeSettingsAlter (Directory        s  ) f = Directory (f s)
filenodeSettingsAlter (FileSourced      s b) f = FileSourced (f s) b
filenodeSettingsAlter (DirectorySourced s b) f = DirectorySourced (f s) b

setOwner :: Owner -> FileNode a -> FileNode a
setGroup :: Group -> FileNode a -> FileNode a
setPerms :: Permissions -> FileNode a -> FileNode a

setOwner x fn = filenodeSettingsAlter fn (\s -> s{ fOwner = Just x })
setGroup x fn = filenodeSettingsAlter fn (\s -> s{ fGroup = Just x }) 
setPerms x fn = filenodeSettingsAlter fn (\s -> s{ fPerms = x })

filenodePath :: FileNode a -> FilePath
filenodePath (File           s)   = fPath s
filenodePath (FileSourced    s _) = fPath s
filenodePath (Directory      s)     = fPath s
filenodePath (DirectorySourced s _) = fPath s

type Owner = Text
type Group = Text

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

getPerms :: FilePath -> Sh Permissions
getPerms fp = liftM (read . T.unpack) . silently $ cmd "stat" "-c%a" fp

-- | Use "stat" to check and/or change the permissions.
handlePerms :: FilePath -> Permissions -> H c ApplyResult
handlePerms  _ PermNoop = return ResNoop
handlePerms fp new      = do
    cur <- sh $ getPerms fp
    if cur == new
        then return ResNoop
        else do
            $action [qc|Perm change  { cur } => { new }|]
            _ <- sh $ cmd "chmod" (show new) fp
            return ResSuccess

-- * FileSources

-- | Set source for the directory.
directorySource :: DirectorySource source => source -> FileNode PlainDirectory -> FileNode SourcedDirectory
directorySource s (Directory ps) = DirectorySourced ps s

-- | Set source for the file.
fileSource :: FileSource source => source -> FileNode PlainFile -> FileNode SourcedFile
fileSource s (File ps) = FileSourced ps s

-- | Some source which provides the content of a file.
class FileSource source where
    -- TODO: This should be extended to conduits or something to allow easy
    -- efficient big files.
    getFileSource :: source -> IO Text

instance FileSource Text     where getFileSource = return
instance FileSource String   where getFileSource = return . T.pack
instance FileSource [Text]   where getFileSource = return . T.intercalate "\n"
instance FileSource [String] where getFileSource = return . T.pack . intercalate "\n"

-- | For implementing different means of populating a directory. This could be
-- an archive, git repo or what ever. Note that the source function must return
-- (), so it should do the population of the directory.
class DirectorySource source where
    getDirectorySource :: source -> FilePath -> IO ApplyResult

instance FileSource a => DirectorySource [(FilePath, a)] where
    getDirectorySource xs root = liftM ResMany $ mapM addFile xs
        where
      addFile (filename, source) = do
          let new = root </> filename
          content <- getFileSource source
          _ <- shellyNoDir $ cmd "echo" content " > " new
          return $ ResFailed "Not yet implemented!"

applyFileNode :: FileNode a -> H c ApplyResult
applyFileNode fn = do
    let settings = filenodeSettings fn
        path     = fPath settings
    fileRes <- case fn of
        File _ -> sh (liftM2 (,) (test_e path) (test_f path)) >>= \case
            (_,    True)  -> return ResNoop
            (True, False) -> return $ ResFailed "Is a directory"
            (False,    _) -> sh $ cmd "touch" path >> return ResSuccess

        FileSourced _ source -> sh (liftM2 (,) (test_e path) (test_f path)) >>= \case
            (_   , True ) -> return ResNoop
            (True, False) -> return (ResFailed "Is a directory")
            (False,    _) -> do
                liftIO $ writeFile (convertFilePath path) =<< getFileSource source
                return ResSuccess

        Directory _ -> sh (liftM2 (,) (test_e path) (test_d path)) >>= \case -- (Exists, Is directory)
            (_   , True ) -> return ResNoop
            (True, False) -> return (ResFailed "Is a file and not a directory!")
            (False,    _) -> sh $ cmd "mkdir" "-p" path >> return ResSuccess

        DirectorySourced _ source -> liftIO $ getDirectorySource source path

    permRes <- handlePerms path (fPerms settings)
    return $ ResMany [fileRes, permRes]
