module Text.Megaparsec.ProtoBuf.Reserved where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.ProtoBuf.Ident
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.TypeNames
import Text.Megaparsec.ProtoBuf.Types

pbReservedEltRange :: ProtoBufParser [Either String Int]
pbReservedEltRange = do
    n <- S.lexeme decimal
    void $ S.lexeme (string "to")
    n' <- S.lexeme decimal
    if n >= n' 
    then
        intern n' n 
    else
        intern n n' where

            intern :: Int -> Int -> ProtoBufParser [Either String Int]
            intern n m | n == m = return [Right n]
            intern n m = do
                let n' = n + 1
                m' <- intern n' m
                return ((Right n) : m')

pbReservedElt' :: ProtoBufParser [Either String Int]
pbReservedElt' = do
    d <- S.lexeme (decimal)
    return [(Right d)]

pbReservedElt :: ProtoBufParser [Either String Int]
pbReservedElt = (try pbReservedEltRange) <|> (try pbReservedElt')
    
pbReserved :: ProtoBufParser PBufReserved
pbReserved = do
    void $ S.lexeme (string "reserved")
    res <- sepBy1 (S.lexeme pbReservedElt) (S.lexeme (single ',')) 
    void $ S.lexeme (single ';')
    return (PBufReserved (foldr (++) [] res))

