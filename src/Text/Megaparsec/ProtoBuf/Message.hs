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
    return (PBufVariable t i n Nothing)

pbVarDeclaresWDefault :: ProtoBufParser PBufVariable 
pbVarDeclaresWDefault = do
    t <- S.lexeme pbType
    i <- S.lexeme pbIdent
    void $ S.lexeme (single '=')
    n <- S.lexeme decimal
    void $ S.lexeme (single '[')
    void $ S.lexeme (string "default")
    void $ S.lexeme (single '=')
    def <- S.lexeme decimal -- For now we'll just do decimals, but this will need to be changed later to include parsing of all possible default values.
    void $ S.lexeme (single ']')
    void $ S.lexeme (single ';')
    return (PBufVariable t i n (Just (show (def :: Int))))

pbVarDeclares' :: ProtoBufParser PBufVariable
pbVarDeclares' = ((try pbVarDeclaresWDefault) <|> (try pbVarDeclares))

pbVarDeclares'' :: ProtoBufParser PBufMessageEntry
pbVarDeclares'' = do
    v <- pbVarDeclares'
    return (PBMEntryVar v)

pbReserved' :: ProtoBufParser PBufMessageEntry
pbReserved' = do
    r <- pbReserved
    return (PBMEntryReserved r)

pbMessageEntry :: ProtoBufParser PBufMessageEntry 
pbMessageEntry = pbVarDeclares'' <|> pbReserved'

pbMessage :: ProtoBufParser PBufMessage
pbMessage = do
    void $ S.lexeme (string "message")
    i <- S.lexeme pbIdent
    void $ S.lexeme (single '{')
    entries <- S.lexeme (many pbMessageEntry)
    void $ S.lexeme (single '}')
    return (PBufMessage i entries)
