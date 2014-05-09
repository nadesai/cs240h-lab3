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
data FileStruct = FileStruct { wstamp :: WriteStamp, finfo :: FileInfo} deriving (Read, Show)

type FileInfoMap = S.Map FilePath FileInfo
type FileMap = S.Map FilePath FileStruct

data TraDatabase = TraDatabase { dbid :: ReplicaID, dbmap :: FileMap, dbvv :: VersionVector} deriving (Read, Show)

-- Code for obtaining the existing database representing the current directory.

-- Given a directory path, looks for a trahs database and returns 
getDB :: FilePath -> IO TraDatabase
getDB dir = do
  contents <- getFileContents (dir </> dbFileName) 
  let readDB (Nothing) = getNewEmptyDB
      readDB (Just s)  = return (deserialize s) 
  readDB contents

-- Given a directory path, generates a database.
getNewEmptyDB :: IO TraDatabase
getNewEmptyDB = do uuid <- genRandomUUID
                   let singleVV = [(uuid,0)]
                   return TraDatabase { dbid = uuid, dbmap = S.empty, dbvv = S.fromList singleVV } 

-- Generates a random UUID 
genRandomUUID :: IO ReplicaID
genRandomUUID = randomIO

-- Serialize-deserialize code for TraDatabase.
deserialize :: String -> TraDatabase
deserialize = read

serialize :: TraDatabase -> String
serialize = show

-- Given a directory path, reads the existing file if one exists.
-- Returns nothing otherwise. 
getFileContents :: FilePath -> IO (Maybe String)
getFileContents fp = getExistingDB fp `catch` returnNothing where
           getExistingDB :: FilePath -> IO (Maybe String) 
           getExistingDB fp' = do
                              handle <- openFile fp' ReadMode
                              contents <- hGetContents handle
                              return (Just contents)
           returnNothing :: IOError -> IO (Maybe String)
           returnNothing _ = return Nothing

-- Code for reading information from the directory.

-- Gets a map of file path to modification info for a given directory.
getDirectoryInfo :: FilePath -> IO FileInfoMap
getDirectoryInfo fp = do paths <- getDirectoryFilePaths fp 
                         stats <- getDirectoryStatuses paths
                         let fileinfo = getRegularFileInfo stats
                         return $ S.fromList fileinfo

-- Gets full paths for all the components of a directory other than the database file.
getDirectoryFilePaths :: FilePath -> IO [FilePath]
getDirectoryFilePaths fp = do 
  files <- getDirectoryContents fp
  -- let files = filter (/= dbFileName) $ files'
  return $ map (fp </>) files

-- Gets the paths for and statuses of the various files. These are REGULAR files
-- only, ignoring the database.
getDirectoryStatuses :: [FilePath] -> IO [(FilePath,FileStatus)]
getDirectoryStatuses paths = do
  stats <- sequence (map getSymbolicLinkStatus paths)
  return $ zip paths stats

-- Given a map of file paths to file status structs, filters out everything but ordinary files,
-- filters out the database file, and turns FileStatus into FileInfo.
getRegularFileInfo :: [(FilePath,FileStatus)] -> [(FilePath,FileInfo)]
getRegularFileInfo = map toInfoTuple . filter ((/= dbFileName) . fst) . filter (isRegularFile . snd) 
                     where toInfoTuple (a,b) = (a,getFileInfo b)

-- Given a FileStatus struct, returns a corresponding FileInfo struct
getFileInfo :: FileStatus -> FileInfo
getFileInfo fs = FileInfo { fsize = fileSize fs, fmodtime = modificationTime fs }



--- Unnecessary (for now) utility functions
-- Computes the files present in an old FileMap that were not present in the current directory.
deletedFiles :: FileMap -> [FilePath] -> [FilePath]
deletedFiles fm fps = (S.keys fm) \\ fps

createdFiles :: FileMap -> [FilePath] -> [FilePath]
createdFiles fm fps = fps \\ (S.keys fm)

-- Hashes the given file (may not be necessary)
hashFile :: FilePath -> IO FileHash 
hashFile path = showBSasHex <$> (hash SHA256 <$> L.readFile path)

