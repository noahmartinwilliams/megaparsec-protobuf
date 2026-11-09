module Text.Megaparsec.ProtoBuf.Values where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.Types

pbBoolValue :: ProtoBufParser Bool
pbBoolValue = do
    str <- S.lexeme ((string "true") <|> (string "false"))
    case str of 
        "true" -> return True
        "false" -> return False
        x -> error ("Internal error in Text.Megaparsec.ProtoBuf.Values. Got " ++ (show x) ++ " in function pbBoolValue.")
