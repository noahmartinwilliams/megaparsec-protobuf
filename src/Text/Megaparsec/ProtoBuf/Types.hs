module Text.Megaparsec.ProtoBuf.Types where

import Data.Void
import Text.Megaparsec

type ProtoBufParser = Parsec Void String 

data PBufType = PBTInt32 | PBTString | PBTMap PBufType PBufType deriving(Show)
data PBufVariable = PBufVariable PBufType String Int (Maybe String) deriving(Show)
data PBufReserved = PBufReservedIdent [String] | PBufReservedNum [Int] deriving(Show)

data PBufEnumEntry = PBufEnumEntry String Int Bool deriving(Show)
-- Name, Number, isDeprecated
data PBufEnum = PBufEnum Bool String [PBufEnumEntry] deriving(Show)
-- allow_aliasing, Name, Entries

data PBufMessageEntry = PBMEntryVar PBufVariable | PBMEntryReserved PBufReserved deriving(Show)
data PBufMessage = PBufMessage String [PBufMessageEntry] deriving(Show)

