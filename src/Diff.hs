module Diff where

import Types

import qualified Data.Map.Strict as S

data Action = NoChange | Download | Conflict | Delete deriving (Show)

diff :: TraDatabase -> TraDatabase -> [(FileName,Action)]
diff = undefined 

mergeVectors :: VersionVector -> VersionVector -> VersionVector
mergeVectors = S.unionWith max 
