module FileUtils (
    hashFile
    ) where

import Codec.Digest.SHA
import Control.Applicative
import qualified Data.ByteString.Lazy as L

hashFile :: FilePath -> IO String
hashFile path = showBSasHex <$> (hash SHA256 <$> L.readFile path)
