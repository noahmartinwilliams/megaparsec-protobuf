module Text.Megaparsec.ProtoBuf.TypeNames where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.Types

pbMapType :: ProtoBufParser PBufType
pbMapType = do
    void $ S.lexeme (string "map")
    void $ S.lexeme (single '<')
    keys <- S.lexeme pbType
    void $ S.lexeme (single ',')
    values <- S.lexeme pbType 
    void $ S.lexeme (single '>')
    return (PBTMap keys values)

pbSimpleType :: ProtoBufParser PBufType
pbSimpleType = do
    t <- (string "string" <|> string "int32")
    case t of
        "string" -> return PBTString
        "int32" -> return PBTInt32
        e -> error ( "Internal error in pbType. Got string: \"" ++ (show e) ++ "\".")

pbType :: ProtoBufParser PBufType 
pbType = (try pbSimpleType ) <|> (try pbMapType)
