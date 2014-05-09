module FileActions where

import Control.Applicative
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

hashFile :: FilePath -> IO FileHash 
hashFile path = showBSasHex <$> (hash SHA256 <$> L.readFile path)

dbFileName :: FilePath
dbFileName = ".trahs.db"

dbTempFileName :: FilePath
dbTempFileName = ".trahs.db~"

getDB :: FilePath -> IO String
getDB fp = openFile (fp </> dbFileName) ReadMode >>= hGetContents

genDB :: FilePath -> IO TraDatabase
genDB = undefined

getDirectoryStatuses :: FilePath -> IO [FileInfo]
getDirectoryStatuses fp = do
  files' <- getDirectoryContents fp
  let files = filter (/= dbFileName) files'
  stats <- sequence (map getSymbolicLinkStatus (map (fp </>) files))
  let regularFiles = filter isRegularFile stats
  return (map getFileInfo regularFiles)

getFileInfo :: FileStatus -> FileInfo
getFileInfo fs = FileInfo { fsize = fileSize fs, fmodtime = modificationTime fs }

-- Serialize-deserialize code for TraDatabase.
deserialize :: String -> TraDatabase
deserialize = read

serialize :: TraDatabase -> String
serialize = show

-- Gets a random UUID 
getRandomUUID :: IO ReplicaID
getRandomUUID = randomIO

-- Computes the files present in an old FileMap that were not present in the current directory.
deletedFiles :: FileMap -> [FilePath] -> [FilePath]
deletedFiles fm fps = (S.keys fm) \\ fps

createdFiles :: FileMap -> [FilePath] -> [FilePath]
createdFiles fm fps = fps \\ (S.keys fm)

