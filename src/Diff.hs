module Diff where

import Types

import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as S

data Action = NoChange | Download | Conflict | Delete deriving (Show)
type Diff = S.Map FileName Action

-- Computes the diff between two TraDatabases 
diff :: TraDatabase -> TraDatabase -> Diff
diff local remote = S.unions [getDeletionActions vvs localFiles, getAdditionActions vvs remoteFiles, getMergeActions vvs mergedFiles] where
  localFiles = localMap S.\\ remoteMap
  remoteFiles = remoteMap S.\\ localMap
  mergedFiles = S.intersectionWith (,) localMap remoteMap
  vvs = (localVV,remoteVV)
  localVV = dbvv local
  remoteVV = dbvv remote
  localMap = dbmap local
  remoteMap = dbmap remote
                     
getDeletionActions :: (VersionVector,VersionVector) -> FileStructMap -> Map FileName Action
getDeletionActions vvs localFiles = S.map (resolveDeletionConflict vvs) localFiles

resolveDeletionConflict :: (VersionVector,VersionVector) -> FileStruct -> Action
resolveDeletionConflict (_,remoteVector) localFile = 
  if fileVersion localFile <= vectorVersion remoteVector localFile then Delete else NoChange

getAdditionActions :: (VersionVector,VersionVector) -> FileStructMap -> Map FileName Action
getAdditionActions vvs remoteFiles = S.map (resolveAdditionConflict vvs) remoteFiles

resolveAdditionConflict :: (VersionVector,VersionVector) -> FileStruct -> Action
resolveAdditionConflict (localVector,_) remoteFile = 
  if fileVersion remoteFile > vectorVersion localVector remoteFile then Download else NoChange

getMergeActions :: (VersionVector,VersionVector) -> Map FileName (FileStruct,FileStruct) -> Map FileName Action
getMergeActions vvs files = S.map (resolveMergeConflict vvs) files 

resolveMergeConflict :: (VersionVector,VersionVector) -> (FileStruct,FileStruct) -> Action
resolveMergeConflict (localVector,remoteVector) (localFile,remoteFile)
  | localFile == remoteFile = NoChange
  | fileVersion remoteFile <= vectorVersion localVector remoteFile = NoChange
  | fileVersion localFile <= vectorVersion remoteVector localFile = Download
  | otherwise = Conflict

-- Gets the version of the replica that originated this file. 
fileVersion :: FileStruct -> VersionID
fileVersion = vid . wstamp

-- Given a version vector of a machine and a file with a given writestamp, gives the version
-- of the machine with replica ID equal to that of the file's writestamp, as known by the
-- verison vector.
-- Useful for figuring out if a machine has "seen" this file before.
vectorVersion :: VersionVector -> FileStruct -> VersionID
vectorVersion vector file = fromMaybe 0 $ S.lookup (rid $ wstamp file) vector

-- Merging vectors. 
mergeVectors :: VersionVector -> VersionVector -> VersionVector
mergeVectors = S.unionWith max 
