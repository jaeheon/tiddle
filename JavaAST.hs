{-
Copyright (c) 2012, Caitlin Sadowski (University of California, Santa Cruz)
                    and Jaeheon Yi (University of California, Santa Cruz).
All Rights Reserved.
-}

module JavaAST where
 
{---------------- Java AST Type Definition------------}
type NumThreads = Int
type Name = String
type Value = Int
type Tid = Int
type Var = String
type Obj = Name
type Label = Name

data Type = TInt 
          | TLInt
          | TObj Name
-- Class packagename classname 
data Class = Class Name Name [Decl] [Tid] [ThreadBlock]
-- Declarations
data Decl = Decl Type Name Value
          | ObjDecl Name
-- List of operations by a thread
data ThreadBlock = ThreadBlock Tid [Stmt]
data Stmt = Read Var (Maybe Value)
          | Write Var (Maybe Value)
          | Fork Tid
          | Join Tid
          | Nop
          | Lock Obj [Stmt]
          | Trans Label [Stmt]
          | Empty
          deriving Show

