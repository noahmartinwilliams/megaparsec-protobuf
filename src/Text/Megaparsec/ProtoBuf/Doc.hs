module Text.Megaparsec.ProtoBuf.Doc where

import Control.Monad
import Data.List.NonEmpty
import Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.ProtoBuf.Message
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.Types
import Text.Megaparsec.ProtoBuf.Values

pbDocEntry :: ProtoBufParser PBDocEntry 
pbDocEntry = do
    m <- pbMessage
    return (PBDocMessage m)

pbDoc :: ProtoBufParser PBDoc
pbDoc = do
    void $ S.lexeme (string "syntax")
    void $ S.lexeme (single '=')
    ver <- S.lexeme (pbStringValue)
    void $ S.lexeme (single ';')
    entries <- S.lexeme (some pbDocEntry)
    let validVers = Set.fromList [Tokens ('p':| "roto3")]
    case ver of
        "proto3" -> return (PBDoc ProtoBuf3 entries)
        x -> failure (Just (Tokens ('v' :| ("ersion: " ++ (show x))))) validVers
