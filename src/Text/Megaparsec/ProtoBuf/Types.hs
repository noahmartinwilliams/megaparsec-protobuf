module Text.Megaparsec.ProtoBuf.Types where

import Data.Void
import Text.Megaparsec

type ProtoBufParser = Parsec Void String 

data PBufType = PBTInt32 | PBTString deriving(Show)
data PBufVariable = PBufVariable PBufType String Int deriving(Show)
data PBufReserved = PBufReservedIdent [String] | PBufReservedNum [Int] deriving(Show)

data PBufMessageEntry = PBMEntryVar PBufVariable | PBMEntryReserved PBufReserved deriving(Show)
data PBufMessage = PBufMessage String [PBufMessageEntry] deriving(Show)

