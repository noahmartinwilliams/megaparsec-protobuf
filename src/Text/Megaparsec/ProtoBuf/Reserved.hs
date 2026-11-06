module Text.Megaparsec.ProtoBuf.Reserved where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.ProtoBuf.Ident
import Text.Megaparsec.ProtoBuf.Space as S
import Text.Megaparsec.ProtoBuf.TypeNames
import Text.Megaparsec.ProtoBuf.Types

pbReservedEltRange :: ProtoBufParser [Int]
pbReservedEltRange = do
    n <- S.lexeme decimal
    void $ S.lexeme (string "to")
    n' <- S.lexeme decimal
    if n >= n' 
    then
        intern n' n 
    else
        intern n n' where

            intern :: Int -> Int -> ProtoBufParser [Int]
            intern n m | n == m = return [n]
            intern n m = do
                let n' = n + 1
                m' <- intern n' m
                return (n : m')

pbReservedElt' :: ProtoBufParser [Int]
pbReservedElt' = do
    d <- S.lexeme (decimal)
    return [d]

pbReservedWord :: ProtoBufParser [String]
pbReservedWord = do
    i <- pbIdent
    return [i]

pbReservedElt :: ProtoBufParser [Int]
pbReservedElt = (try pbReservedEltRange) <|> (try pbReservedElt')

pbReservedWords :: ProtoBufParser PBufReserved
pbReservedWords = do
    void $ S.lexeme (string "reserved")
    res <- sepBy1 (S.lexeme pbReservedWord) (S.lexeme (single ','))
    void $ S.lexeme (single ';')
    return (PBufReservedIdent (foldr (++) [] res))

    
pbReservedNums :: ProtoBufParser PBufReserved
pbReservedNums = do
    void $ S.lexeme (string "reserved")
    res <- sepBy1 (S.lexeme pbReservedElt) (S.lexeme (single ',')) 
    void $ S.lexeme (single ';')
    return (PBufReservedNum (foldr (++) [] res))

pbReserved :: ProtoBufParser PBufReserved
pbReserved = ((try pbReservedNums) <|> (try pbReservedWords))
