module Text.Megaparsec.ProtoBuf.Space where

import Control.Applicative
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.ProtoBuf.Types

sc :: ProtoBufParser ()
sc = L.space space1 empty empty 

lexeme :: ProtoBufParser a -> ProtoBufParser a
lexeme = L.lexeme sc
