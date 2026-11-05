module Text.Megaparsec.ProtoBuf.TypeNames where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.Types

pbType :: ProtoBufParser PBufType
pbType = do
    t <- (string "string" <|> string "int32")
    case t of
        "string" -> return PBTString
        "int32" -> return PBTInt32
        e -> error ( "Internal error in pbType. Got string: \"" ++ (show e) ++ "\".")
