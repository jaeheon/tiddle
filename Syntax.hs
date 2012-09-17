{-
Copyright (c) 2012, Caitlin Sadowski (University of California, Santa Cruz)
                    and Jaeheon Yi (University of California, Santa Cruz).
All Rights Reserved.
-}

-- syntax for traces

module Syntax where

type Tid = Int
type Var = String
type Val = Int
type Label = String
type Obj = String

data Event = Event Tid Op
  deriving (Show, Eq)

data Op = Rd   Var (Maybe Val)
        | Wr   Var (Maybe Val)
        | Fork Tid
        | Join Tid
        | Lock Obj
        | Rel  Obj
        | Beg  Label  
        | End  Label
  deriving (Show, Eq)

type Trace = [Event]


