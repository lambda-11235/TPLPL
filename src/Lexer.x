{
module Lexer (Token (..), LexOut (..), scan) where
}

%wrapper "posn"

tokens :-

  $white+                               ;
  "#".*                                 ;
  "("                                   { \p s -> lexOut p LLParen }
  ")"                                   { \p s -> lexOut p LRParen }
  ","                                   { \p s -> lexOut p LComma }
  "."                                   { \p s -> lexOut p LDot }
  "?"                                   { \p s -> lexOut p LQMark }
  ":-"                                  { \p s -> lexOut p LImplied }
  [a-zA-Z][_a-zA-Z0-9]*                 { \p s -> lexOut p (LId s) }

{
data Token = LLParen
           | LRParen
           | LComma
           | LDot
           | LQMark
           | LImplied
           | LId String
           deriving (Eq, Show)

data LexOut = LexOut { offset :: Int
                     , line :: Int
                     , column :: Int
                     , getToken :: Token }
              deriving (Eq, Show)

lexOut (AlexPn offset line col) tok = LexOut offset line col tok

scan = alexScanTokens
}
