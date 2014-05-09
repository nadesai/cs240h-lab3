module Types where

import Data.Word (Word64)
import System.Posix.Types (EpochTime, FileOffset) 

import qualified Data.Map.Strict as S

type FileName = FilePath
type DirectoryName = FilePath
type ReplicaID = Word64 
type VersionID = Integer
type VersionVector = S.Map ReplicaID VersionID

type FileHash = String
type FileSize = FileOffset
type FileModTime = EpochTime
type DBModTime = EpochTime

data WriteStamp = WriteStamp { rid :: ReplicaID, vid :: VersionID } deriving (Eq, Read, Show)
data FileInfo = FileInfo { fsize :: FileSize, fmodtime :: FileModTime } deriving (Eq, Read, Show)
data FileStruct = FileStruct { wstamp :: WriteStamp, finfo :: FileInfo} deriving (Eq, Read, Show)

type FileInfoMap = S.Map FileName FileInfo
type FileStructMap = S.Map FileName FileStruct

data TraDatabase = TraDatabase { dbid :: ReplicaID, dbmap :: FileStructMap, dbvv :: VersionVector} deriving (Eq, Read, Show)
