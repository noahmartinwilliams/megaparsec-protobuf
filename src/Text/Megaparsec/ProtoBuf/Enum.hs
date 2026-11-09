module Text.Megaparsec.ProtoBuf.Enum where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.ProtoBuf.Ident
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.Types
import Text.Megaparsec.ProtoBuf.Values

pbSimpleEnumEntry :: ProtoBufParser PBufEnumEntry
pbSimpleEnumEntry = do
    i <- S.lexeme pbIdent 
    void $ S.lexeme (single '=')
    d <- S.lexeme decimal
    void $ S.lexeme (single ';')
    return (PBufEnumEntry i d False Nothing)

pbComplicatedEnumEntry :: ProtoBufParser PBufEnumEntry 
pbComplicatedEnumEntry = do
    i <- S.lexeme pbIdent
    void $ S.lexeme (single '=')
    d <- S.lexeme decimal
    void $ S.lexeme (single '[')
    void $ S.lexeme (string "deprecated") -- Just for now we'll only be able to understand deprecated or not here.
    void $ S.lexeme (single '=')
    b <- S.lexeme pbBoolValue
    void $ S.lexeme (single ']')
    void $ S.lexeme (single ';')
    return (PBufEnumEntry i d b Nothing)

pbEnumEntry :: ProtoBufParser PBufEnumEntry 
pbEnumEntry = (try pbComplicatedEnumEntry) <|> pbSimpleEnumEntry

pbEnum :: ProtoBufParser PBufEnum 
pbEnum = do
    void $ S.lexeme (string "enum")
    i <- S.lexeme pbIdent
    void $ S.lexeme (single '{')
    entries <- S.lexeme (some pbEnumEntry )
    void $ S.lexeme (single '}')
    return (PBufEnum i entries)
