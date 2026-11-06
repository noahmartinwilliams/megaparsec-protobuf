module Text.Megaparsec.ProtoBuf.Types where

import Data.Map
import Data.Void
import Text.Megaparsec

type ProtoBufParser = Parsec Void String 

data PBufType = PBTInt32 | PBTString deriving(Show)
data PBufVariable = PBufVariable PBufType String Int deriving(Show)
data PBufMessage = PBufMessage String (Map String PBufVariable) deriving(Show)

newtype PBufReserved = PBufReserved [Either String Int] deriving(Show)
