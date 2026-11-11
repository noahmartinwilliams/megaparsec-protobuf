module Text.Megaparsec.ProtoBuf.Values where

import Control.Monad
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

pbStringValue :: ProtoBufParser String
pbStringValue = do
    void $ single '"'
    strVal <- some ((try (noneOf ['\\', '"'])) <|> (try bslash) <|> (try newl) <|> (try escapedQuote ))
    void $ single '"'
    return strVal where
        
        bslash :: ProtoBufParser Char
        bslash = do
            void $ single '\\'
            void $ single '\\'
            return '\\'

        newl :: ProtoBufParser Char
        newl = do
            void $ single '\\'
            void $ single 'n'
            return '\n'

        escapedQuote :: ProtoBufParser Char
        escapedQuote = do
            void $ single '\\'
            void $ single '"'
            return '"'
