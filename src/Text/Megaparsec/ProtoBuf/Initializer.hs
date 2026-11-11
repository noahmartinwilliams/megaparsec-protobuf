module Text.Megaparsec.ProtoBuf.Initializer where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.ProtoBuf.Ident
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.Types
import Text.Megaparsec.ProtoBuf.Values

pbInitStr :: ProtoBufParser PBInitValue
pbInitStr = do
    str <- pbStringValue
    return (PBInitString str)

pbInitDecimal :: ProtoBufParser PBInitValue 
pbInitDecimal = do
    dec <- decimal
    return (PBInitInt dec)

pbInitValue :: ProtoBufParser PBInitValue
pbInitValue = (try pbInitStr) <|> (try pbInitDecimal)

pbInitKeyValue :: ProtoBufParser (String, PBInitValue)
pbInitKeyValue = do
    i <- pbIdent
    void $ S.lexeme (single ':')
    v <- pbInitValue
    void $ S.lexeme (single '\n')
    return (i, v)

pbInitializer :: ProtoBufParser PBufInitializer
pbInitializer = do
    void $ S.lexeme (string "initializer")
    void $ S.lexeme (single '{')
    kvs <- some pbInitKeyValue
    void $ S.lexeme (single '}')
    return (PBInit kvs)
