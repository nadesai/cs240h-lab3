module FileActions where

import Types

import Control.Applicative
import Control.Exception (catch)
import Codec.Digest.SHA
import Data.List ((\\))
import System.FilePath ((</>))
import System.Random (randomIO) 
import System.PosixCompat.Files (getSymbolicLinkStatus, isRegularFile, fileSize, modificationTime, FileStatus)
import System.Directory (getDirectoryContents)
import System.IO (openFile, hPutStrLn, hClose, IOMode(..))
import qualified Data.Map.Strict as S
import qualified Data.ByteString.Lazy as L

dbFileName :: FileName
dbFileName = ".trahs.db"

dbTempFileName :: FileName
dbTempFileName = ".trahs.db~"

-- Code for obtaining the existing database representing the current directory.
updateDB :: DirectoryName -> IO ()
updateDB dir = do
  oldDB <- getOldDB dir
  newInfo <- getDirectoryInfo dir 
  let newDB = updateDatabase newInfo oldDB
  writeNewDB dir newDB

-- Given a directory path, looks for a trahs database and returns 
getOldDB :: DirectoryName -> IO TraDatabase
getOldDB dir = do
  contents <- getFileContents (dir </> dbFileName) 
  let readDB (Nothing) = getNewEmptyDB
      readDB (Just s)  = return (deserialize s) 
  readDB contents

writeNewDB :: DirectoryName -> TraDatabase -> IO ()
writeNewDB dir db = do
  handle <- openFile (dir </> dbFileName) WriteMode
  hPutStrLn handle $ serialize db
  -- hPutStrLn stderr $ serialize db
  hClose handle

-- Given a directory path, generates a database.
getNewEmptyDB :: IO TraDatabase
getNewEmptyDB = do uuid <- genRandomUUID
                   return TraDatabase { dbid = uuid, dbmap = S.empty, dbvv = S.singleton uuid 0 } 

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
           getExistingDB fp' = do contents <- readFile fp' 
                                  return $ (length contents `seq` Just contents)
           returnNothing :: IOError -> IO (Maybe String)
           returnNothing _ = return Nothing

-- Code for reading information from the directory.

-- Gets a map of file path to modification info for a given directory.
getDirectoryInfo :: DirectoryName -> IO FileInfoMap
getDirectoryInfo dir = do names <- getDirectoryContents dir 
                          stats <- getDirectoryStatuses dir names
                          let fileinfo = getRegularFileInfo stats
                          return $ S.fromList fileinfo

-- Gets the paths for and statuses of the various files. These are REGULAR files
-- only, ignoring the database.
getDirectoryStatuses :: DirectoryName -> [FileName] -> IO [(FileName,FileStatus)]
getDirectoryStatuses dir names = do
  let paths = map (dir </>) names
  stats <- sequence (map getSymbolicLinkStatus paths)
  return $ zip names stats

-- Given a map of file paths to file status structs, filters out everything but ordinary files,
-- filters out the database file, and turns FileStatus into FileInfo.
getRegularFileInfo :: [(FileName,FileStatus)] -> [(FileName,FileInfo)]
getRegularFileInfo = map toInfoTuple . filter ((/= dbFileName) . fst) . filter (isRegularFile . snd) 
                     where toInfoTuple (a,b) = (a,getFileInfo b)

-- Given a FileStatus struct, returns a corresponding FileInfo struct
getFileInfo :: FileStatus -> FileInfo
getFileInfo fs = FileInfo { fsize = fileSize fs, fmodtime = modificationTime fs }


-- Logic for updating the current version of the database given the old version and the current
-- FileInfoMap.
updateDatabase :: FileInfoMap -> TraDatabase -> TraDatabase
updateDatabase newmap old = TraDatabase { dbid = oldID, dbvv = newVV, dbmap = newMap } where
                            newMap = updateMap oldID newVersion newmap oldMap
                            newVV = S.adjust (const newVersion) oldID oldVV
                            newVersion = (oldVV S.! oldID) + 1
                            oldID = dbid old
                            oldVV = dbvv old
                            oldMap = dbmap old

-- Updates the writestamps to the latest version. O(n log n) for now.
updateMap :: ReplicaID -> VersionID -> FileInfoMap -> FileStructMap -> FileStructMap
updateMap r v newmap oldmap = S.mapWithKey (\k i -> getWriteStamp (S.lookup k oldmap) r v i) newmap

-- Logic for manipulating the file structure and 
-- Logic for computing writestamp of file.
getWriteStamp :: Maybe FileStruct -> ReplicaID -> VersionID -> FileInfo -> FileStruct
getWriteStamp (Nothing) r v info = FileStruct { wstamp = WriteStamp { rid = r, vid = v }, finfo = info }
getWriteStamp (Just fs) r v info 
  | (finfo fs) == info = fs
  | otherwise          = FileStruct { wstamp = WriteStamp { rid = r, vid = v }, finfo = info }

--- Unnecessary (for now) utility functions
-- Computes the files present in an old FileStructMap that were not present in the current directory.
deletedFiles :: FileStructMap -> [FileName] -> [FileName]
deletedFiles fm fps = (S.keys fm) \\ fps

createdFiles :: FileStructMap -> [FileName] -> [FileName]
createdFiles fm fps = fps \\ (S.keys fm)

-- Hashes the given file (may not be necessary)
hashFile :: FilePath -> IO FileHash 
hashFile path = showBSasHex <$> (hash SHA256 <$> L.readFile path)

