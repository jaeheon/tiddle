-- Copyright (c) 2012, Caitlin Sadowski (University of California, Santa Cruz)
--                     and Jaeheon Yi (University of California, Santa Cruz).
-- All Rights Reserved.
{ 
  module Lexer where
}

%wrapper "posn"

$newLine = \n
@jletter = [a-zA-Z]
@jletterdigit = @jletter | [0-9] | ['_]

@identifier         = @jletter @jletterdigit*
@decIntegerLiteral  = 0 | [1-9][0-9]*


tokens :-
  $white+              ;
  $newLine+            ;
  "("                  ;
  ")"                  ;
  ","                  ;
  "rd"                 { \p _ -> TokenRd }
  "wr"                 { \p _ -> TokenWr }
  "acq"                { \p _ -> TokenAcq }
  "rel"                { \p _ -> TokenRel }
  "fork"               { \p _ -> TokenFork }
  "join"               { \p _ -> TokenJoin }
  "beg"                { \p _ -> TokenBeg }
  "end"                { \p _ -> TokenEnd }
  @identifier          { \p s -> TokenId s }
  @decIntegerLiteral   { \p s -> TokenInt (read s) }
  .                    { \p s -> error $ "Weird character"++(show s) }

{
data Token = TokenRd
           | TokenWr
           | TokenAcq
           | TokenRel
           | TokenFork
           | TokenJoin
           | TokenBeg
           | TokenEnd
           | TokenId String
           | TokenInt Int
           deriving Show
}
