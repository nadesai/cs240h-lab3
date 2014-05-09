module FileActions where

import Control.Applicative
import Control.Exception (catch)
import Codec.Digest.SHA
import Data.Word (Word64)
import Data.List ((\\))
import System.FilePath ((</>))
import System.Random (randomIO) 
import System.Posix.Types (EpochTime, FileOffset) 
import System.PosixCompat.Files (getSymbolicLinkStatus, isRegularFile, fileSize, modificationTime, FileStatus)
import System.Directory (getDirectoryContents)
import System.IO (openFile, hGetContents, IOMode(..))
import qualified Data.Map.Strict as S
import qualified Data.ByteString.Lazy as L

dbFileName :: FilePath
dbFileName = ".trahs.db"

dbTempFileName :: FilePath
dbTempFileName = ".trahs.db~"

type ReplicaID = Word64 
type VersionID = Integer
type VersionVector = S.Map ReplicaID VersionID

type FileHash = String
type FileSize = FileOffset
type FileModTime = EpochTime

data WriteStamp = WriteStamp { rid :: ReplicaID, vid :: VersionID } deriving (Read, Show)
data FileInfo = FileInfo { fsize :: FileSize, fmodtime :: FileModTime } deriving (Read, Show)
type FileMap = S.Map FilePath (WriteStamp,FileInfo)

data TraDatabase = TraDatabase { dbid :: ReplicaID, dbmap :: FileMap, dbvv :: VersionVector} deriving (Read, Show)

-- Given a directory path, generates a database.
genDB :: FilePath -> IO TraDatabase
genDB = undefined

-- Given a directory path, reads in the existing database if one exists.
-- Returns nothing otherwise. 
getDBContents :: FilePath -> IO (Maybe String)
getDBContents fp = getExistingDB fp `catch` returnNothing where
           getExistingDB :: FilePath -> IO (Maybe String) 
           getExistingDB fp' = do
                              handle <- openFile (fp' </> dbFileName) ReadMode
                              contents <- hGetContents handle
                              return (Just contents)
           returnNothing :: IOError -> IO (Maybe String)
           returnNothing _ = return Nothing

-- Gets full paths for all the components of a directory other than the files.
getDirectoryFilePaths :: FilePath -> IO [FilePath]
getDirectoryFilePaths fp = do 
  files' <- getDirectoryContents fp
  let files = filter (/= dbFileName) $ files'
  return (map (fp </>) files)

-- Gets the paths for and statuses of the various files within a directory. These are REGULAR files
-- only, ignoring the database.
getDirectoryStatuses :: FilePath -> IO [(FilePath,FileInfo)]
getDirectoryStatuses fp = do
  paths <- getDirectoryFilePaths fp
  stats <- sequence (map getSymbolicLinkStatus paths)
  let regularFiles = filter (isRegularFile . snd) (zip paths stats)
  return (map (\(a,b) -> (a,getFileInfo b)) regularFiles)

-- Given a FileStatus struct, returns a corresponding FileInfo struct
getFileInfo :: FileStatus -> FileInfo
getFileInfo fs = FileInfo { fsize = fileSize fs, fmodtime = modificationTime fs }

-- Serialize-deserialize code for TraDatabase.
deserialize :: String -> TraDatabase
deserialize = read

serialize :: TraDatabase -> String
serialize = show

-- Generates a random UUID 
genRandomUUID :: IO ReplicaID
genRandomUUID = randomIO

-- Computes the files present in an old FileMap that were not present in the current directory.
deletedFiles :: FileMap -> [FilePath] -> [FilePath]
deletedFiles fm fps = (S.keys fm) \\ fps

createdFiles :: FileMap -> [FilePath] -> [FilePath]
createdFiles fm fps = fps \\ (S.keys fm)

-- Hashes the given file (may not be necessary)
hashFile :: FilePath -> IO FileHash 
hashFile path = showBSasHex <$> (hash SHA256 <$> L.readFile path)

