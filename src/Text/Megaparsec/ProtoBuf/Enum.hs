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
    return (PBufEnumEntry i d False )

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
    return (PBufEnumEntry i d b )

pbEnumEntry :: ProtoBufParser PBufEnumEntry 
pbEnumEntry = (try pbComplicatedEnumEntry) <|> pbSimpleEnumEntry

pbAllowAliases :: ProtoBufParser Bool
pbAllowAliases = do
    void $ S.lexeme (string "option")
    void $ S.lexeme ((string "allow_aliasing") <|> (string "allow_alias") )
    void $ S.lexeme (single '=')
    ret <- S.lexeme pbBoolValue
    void $ S.lexeme (single ';')
    return ret

pbEnum :: ProtoBufParser PBufEnum 
pbEnum = do
    void $ S.lexeme (string "enum")
    i <- S.lexeme pbIdent
    void $ S.lexeme (single '{')
    allowAliases <- optional pbAllowAliases
    entries <- S.lexeme (some pbEnumEntry )
    void $ S.lexeme (single '}')
    case allowAliases of
        (Just True) -> return (PBufEnum True i entries)
        (Just False) -> return (PBufEnum False i entries)
        Nothing -> return (PBufEnum False i entries)
