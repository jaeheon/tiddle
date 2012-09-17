-- Copyright (c) 2012, Caitlin Sadowski (University of California, Santa Cruz)
--                     and Jaeheon Yi (University of California, Santa Cruz).
-- All Rights Reserved.
{
module Parser where
import Syntax
import Lexer

parseError :: [Token] -> a
parseError _ = error "Parse error!"
}

%name parseTrace
%tokentype { Token }
%error { parseError }

%token
  RD   { TokenRd }
  WR   { TokenWr }
  ACQ  { TokenAcq }
  REL  { TokenRel }
  FORK { TokenFork }
  JOIN { TokenJoin }
  BEG  { TokenBeg }
  END  { TokenEnd }
  ID   { TokenId $$ }
  INT  { TokenInt $$ }

%%

Trace : { [] }
      | Trace Op { $1 ++ [$2] }

Op : RD INT ID INT { Event $2 $ Rd $3 $ Just $4 }
   | WR INT ID INT { Event $2 $ Wr $3 $ Just $4 }
   | RD INT ID     { Event $2 $ Rd $3 Nothing }
   | WR INT ID     { Event $2 $ Wr $3 Nothing }
   | ACQ INT ID    { Event $2 $ Lock $3 }
   | REL INT ID    { Event $2 $ Rel $3 }
   | FORK INT INT  { Event $2 $ Fork $3 }
   | JOIN INT INT  { Event $2 $ Join $3 }
   | BEG INT ID    { Event $2 $ Beg $3 }
   | END INT ID    { Event $2 $ End $3 }

