module Text.Megaparsec.ProtoBuf.Message where

import Control.Monad
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.ProtoBuf.Ident
import Text.Megaparsec.ProtoBuf.Reserved
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.TypeNames
import Text.Megaparsec.ProtoBuf.Types

pbVarDeclares :: ProtoBufParser PBufVariable
pbVarDeclares = do
    t <- S.lexeme pbType
    i <- S.lexeme pbIdent
    void $ S.lexeme (single '=')
    n <- S.lexeme decimal
    void $ S.lexeme (single ';')
    return (PBufVariable t i n)

pbVarDeclares' :: ProtoBufParser PBufMessageEntry
pbVarDeclares' = do
    v <- pbVarDeclares
    return (PBMEntryVar v)

pbReserved' :: ProtoBufParser PBufMessageEntry
pbReserved' = do
    r <- pbReserved
    return (PBMEntryReserved r)

pbMessageEntry :: ProtoBufParser PBufMessageEntry 
pbMessageEntry = pbVarDeclares' <|> pbReserved'

pbMessage :: ProtoBufParser PBufMessage
pbMessage = do
    void $ S.lexeme (string "message")
    i <- S.lexeme pbIdent
    void $ S.lexeme (single '{')
    entries <- S.lexeme (many pbMessageEntry)
    void $ S.lexeme (single '}')
    return (PBufMessage i entries)
