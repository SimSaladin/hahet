{-# LANGUAGE LambdaCase, EmptyDataDecls #-}
-- |
-- Methods for managing files and directories.
--
-- Ensure that directory /etc exists and is owned by root with perms 755:
--
-- > manage $ Directory "/etc"
-- >     /- setOwner "root"
-- >     /- setPerms "755"
-- 
-- Ensure that /etc/hostname exists, is owned by root, has 755 permissions and
-- the content "myhost".
--
-- > manage $ File "/etc/hostname"
-- >     /- setOwner "root"
-- >     /- setPerms "755"
-- >     >>> "myhost"
module Hahet.Targets.FileNodes 
--   Examples:
    ( FileNode(..)
    , toFileNode

    -- * Properties
    , (/-)
    , Owner, Group
    , Permissions(..)
    , setOwner, setGroup, setPerms

    -- * Sourcing
    , (>>>), (&>>)
    , FileSource(..)
    , DirectorySource(..)
    ) where

import Prelude hiding (FilePath, writeFile)
import Data.Text.IO
import Control.Monad
import Control.Monad.Trans (lift)
import Data.Monoid         ((<>))
import Data.String
import Data.Text           (Text)
import Data.List           (intercalate)
import qualified Data.Text as T
import Data.Typeable
import Shelly hiding (path)
import Text.Read           as R hiding (lift)

import Hahet.Core.Internals
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

instance Typeable a => Target c (FileNode a) where
    targetDesc  _ = toTextIgnore . filenodePath
    targetApply   = lift . applyFileNode
    targetConflicts a b | filenodePath a == filenodePath b = Just "Conflicts"
                        | otherwise = Nothing

toFileNode :: FilePath -> FileSettings
toFileNode txt = FileSettings txt Nothing Nothing PermNoop

-- | File properties.
data FileSettings = FileSettings
    { fPath  :: FilePath
    , fOwner :: Maybe Owner
    , fGroup :: Maybe Group
    , fPerms :: Permissions
    }

instance IsString FileSettings where
    fromString = toFileNode . fromText . T.pack

-- | Set properties in combinatorical style
--
(/-) :: a -> (a -> b) -> b
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
handlePerms :: FilePath -> Permissions -> Sh ApplyResult
handlePerms  _ PermNoop = return ResNoop
handlePerms fp new = do
    cur <- getPerms fp
    if cur == new
        then return ResNoop
        else do
            mlog ("Perm change: " <> show cur <> " => " <> show new)
            _ <- cmd "chmod" (show new) fp
            return ResSuccess

-- * FileSources

-- | Set source of a file.
(>>>) :: FileSource s => FileNode PlainFile -> s -> FileNode SourcedFile
(File ps) >>> s = FileSourced ps s

-- | Set source of a directory.
(&>>) :: DirectorySource source => FileNode PlainDirectory -> source -> FileNode SourcedDirectory
(Directory ps) &>> s = DirectorySourced ps s

-- | Some source which provides the content of a file.
class FileSource source where
    -- TODO: This should be extended to conduits or something to allow easy
    -- efficient big files.
    fileSource :: source -> IO Text

instance FileSource Text     where fileSource = return
instance FileSource String   where fileSource = return . T.pack
instance FileSource [Text]   where fileSource = return . T.intercalate "\n"
instance FileSource [String] where fileSource = return . T.pack . intercalate "\n"

-- | For implementing different means of populating a directory. This could be
-- an archive, git repo or what ever. Note that the source function must return
-- (), so it should do the population of the directory.
class DirectorySource source where
    directorySource :: source -> FilePath -> IO ApplyResult

instance FileSource a => DirectorySource [(FilePath, a)] where
    directorySource xs root = liftM ResMany $ mapM addFile xs
        where
      addFile (filename, source) = do
          let new = root </> filename
          content <- fileSource source
          _ <- shellyNoDir $ cmd "echo" content " > " new
          return $ ResFailed "Not yet implemented!"

applyFileNode :: FileNode a -> Sh ApplyResult
applyFileNode fn = do
    let settings = filenodeSettings fn
        path     = fPath settings
    fileRes <- case fn of
        File _ -> liftM2 (,) (test_e path) (test_f path) >>= \case
            (_,    True)  -> return ResNoop
            (True, False) -> return $ ResFailed "Is a directory"
            (False,    _) -> cmd "touch" path >> return ResSuccess

        FileSourced _ source -> liftM2 (,) (test_e path) (test_f path) >>= \case
            (_   , True ) -> return ResNoop
            (True, False) -> return (ResFailed "Is a directory")
            (False,    _) -> do
                liftIO $ writeFile (convertFilePath path) =<< fileSource source
                return ResSuccess

        Directory _ -> liftM2 (,) (test_e path) (test_d path) >>= \case -- (Exists, Is directory)
            (_   , True ) -> return ResNoop
            (True, False) -> return (ResFailed "Is a file and not a directory!")
            (False,    _) -> cmd "mkdir" "-p" path >> return ResSuccess

        DirectorySourced _ source -> liftIO $ directorySource source path

    permRes <- handlePerms path (fPerms settings)
    return $ ResMany [fileRes, permRes]
