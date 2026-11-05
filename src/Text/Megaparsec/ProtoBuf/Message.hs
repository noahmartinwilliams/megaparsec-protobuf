module Text.Megaparsec.ProtoBuf.Message where

import Control.Monad
import Data.Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.ProtoBuf.Ident
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

pbMessage :: ProtoBufParser PBufMessage
pbMessage = do
    void $ S.lexeme (string "message")
    i <- S.lexeme pbIdent
    void $ S.lexeme (single '{')
    vars <- S.lexeme (many pbVarDeclares)
    void $ S.lexeme (single '}')
    let names = Prelude.map (\(PBufVariable _ s _) -> s) vars
    return (PBufMessage i (Data.Map.fromList (zip names vars)))
