module Text.Megaparsec.ProtoBuf.Ident where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.Types 

pbIdent :: ProtoBufParser String
pbIdent = do
    f <- letterChar
    r <- S.lexeme (some (alphaNumChar <|> single '_'))
    return (f : r)
