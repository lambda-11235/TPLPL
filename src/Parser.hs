{-|
Module: Parse

The module to parse lisp expressions.
-}

module Parser (rule, rules, query) where

import Data
import Lexer

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim


type Parser = Parsec [LexOut] ()


match :: Token -> Parser ()
match tok = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' x = if x == tok then Just () else Nothing

idP :: Parser String
idP = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (LId name) = Just name
    match' _ = Nothing

pos :: (SourcePos -> LexOut -> [LexOut] -> SourcePos)
pos oldPos (LexOut _ line col _) _ = newPos (sourceName oldPos) line col


rules :: Parser [(String, [Data], [Data])]
rules = many rule <* eof

rule =
  do (name, params) <- term
     subGoals <- optionMaybe (match LImplied *> andP)
     match LDot
     return (name, params, maybe [] id subGoals)

query :: Parser [Data]
query = andP <* match LDot <* eof

andP = sepBy1 value (match LComma)


varP =
  do match LQMark
     idP

term =
  do name <- idP
     xs <- optionMaybe (match LLParen *> (sepBy value (match LComma)) <* match LRParen)
     return (name, maybe [] id xs)

value = (var <$> varP) <|> (uncurry Struct <$> term)

