{-
Copyright (c) 2012, Caitlin Sadowski (University of California, Santa Cruz)
                    and Jaeheon Yi (University of California, Santa Cruz).
All Rights Reserved.
-}

module ToJavaBarriers where

-- Pretty Printing Module
import Text.PrettyPrint.HughesPJ
import JavaAST


{--------------- Outputting Java code ----------------}

{------ formatting methods --------}
class MPretty a where
  mpretty:: a -> Doc

instance MPretty ThreadBlock where
  mpretty (ThreadBlock tid stmts) =
    vcat $ map mpretty stmts

instance MPretty Stmt where
  mpretty s =
    case s of 
      (Trans l stmts) -> 
        indentBlock ("static void " ++ l ++ "() throws BrokenBarrierException, InterruptedException ") 
                ((text "await(cb);") 
                    $+$ (vcat (map pretty stmts))
                    $+$ (text "await(cc);"))
                $+$
                (vcat (map mpretty stmts))
      (Lock o stmts)  -> ( vcat ( map mpretty stmts))
      otherwise -> empty

-- Polymorphic function definition for various JavaAST types 
-- to formatted Java code strings
class Pretty a where
    pretty:: a -> Doc

instance Pretty ThreadBlock where
  pretty (ThreadBlock tid stmts) =
    indentBlock (header tid) (run stmts) <> text";"
    where
      header t      = "final Thread t" ++(show t)++ " = new Thread()" 
      run blks      = indentBlock "public void run()" (trycatch blks)
      trycatch blks = tryCatchBlock 
                      ["InterruptedException", "BrokenBarrierException"] 
                      ((pretty (Decl TLInt "_z" 0))$+$(vcat (map pretty blks)))

instance Pretty Stmt where
   pretty s = 
      case s of 
        Fork  tforked    ->  text "await(cc);"
                                    $+$ (reInitBarrier "cc" "++numThreads") 
                                    $+$ text ( "t"++(show tforked)++".start();")
                                    $+$ text "await(cb);"
                                    $+$ (reInitBarrier "cb" "numThreads")
        Join  tjoined    -> text "await(cc);"
                                    $+$ (reInitBarrier "cc" "--numThreads")
                                    $+$ text "await(cb);"
                                    $+$ reInitBarrier "cb" "numThreads"
                                    $+$ ( text ( "t"++(show tjoined)++".join();"))
        Empty            -> empty
        (Lock obj stmts) -> indentBlockWait ("synchronized("++obj++")")
                                    $ vcat $ map pretty stmts
        (Trans l stmts)  -> (text "await(cc);") 
                              $+$ (text $ l ++ "();")
                              $+$ (text  "await(cb);")
                              --indentBlockWait "/*# atomic */"  
                              --      $ vcat $ map pretty stmts
        otherwise        -> (text "await(cc);") 
                                    $+$ prettyOp s 
                                    $+$ (text "await(cb);")
      where 
        reInitBarrier bar val = text $ bar ++ " = new " 
                                      ++ "CyclicBarrier" ++ "(" ++ val ++ ");"
        prettyOp (Read x (Just v))  = (text ("_z = "++x++";") ) 
                                      <+> text ("assert (_z == "++(show v)++");")
        prettyOp (Read x Nothing)   = text $ "_z = "++x++";"
        prettyOp (Write x (Just v)) = text $ x++" = "++(show v)++";" 
        prettyOp (Write x Nothing)  = text $ x++" = 1;" 
        prettyOp Nop                = empty


instance Pretty Decl where
  pretty (Decl typ nam val) =
        case typ of
           TLInt  -> text $ "int "++ nam ++ " = " ++ (show val) ++";"
           TInt   -> text $ "static int " ++ nam ++ " = " ++ (show val)++";"
           TObj n -> text $ "static " ++ n ++ " " ++ nam 
                            ++ " = new " ++ n ++ "(" ++ (show val) ++ ");"
  pretty (ObjDecl nam) =
        text $ "static Object " ++ nam ++ " = new Object();"

        
{----------------------Calls to pretty start here------------}
instance Pretty Class where
  pretty  (Class package name decls initialThreads tb) = 
        let numThreads = length initialThreads in
        text ("package "++package++";") $+$ 
        text "import java.util.concurrent.BrokenBarrierException;" $+$ 
        text "import java.util.concurrent.CyclicBarrier;" $#$
        indentBlock ("public class "++name) (classBody decls initialThreads tb)

        where
          classBody d t tb = vcat (map pretty (allDecls d t)) $#$
                             (methods tb) $#$
                             (main tb t)
          allDecls decls initThreads = 
            let numThreads = length initialThreads in
              decls++
              [Decl (TObj "CyclicBarrier") "cb" numThreads]++ 
              [Decl (TObj "CyclicBarrier") "cc" numThreads]++ 
              [Decl TInt "counter" 0]++
              [Decl TInt "numThreads" numThreads]
          methods tb = (indentBlock  "static public void await(CyclicBarrier c) throws BrokenBarrierException, InterruptedException"
                                  (text "c.await();"))
                    $#$
                    ( vcat ( map mpretty (reverse tb)))
          main tb t = indentBlock 
                        "public static void main(String[] args)"
                        $ vcat $ map pretty (reverse tb) ++ map start t
          start t = text $ "t"++(show t)++".start();"


{-------------Helper functions---------------}

-- Block wrappers to give proper indentation to java code
indentBlock :: String -> Doc -> Doc
indentBlock header body = text header <+> lbrace  
                          $+$ (nest 2 body) $+$ rbrace

-- adds barrier calls around the start and end of indented block
indentBlockWait :: String -> Doc -> Doc                          
indentBlockWait header body = 	text "await(cc);"$+$
				text header <+> lbrace
        			$+$ (nest 2 (text "await(cb);" $+$ body $+$ text "await(cc);"))
        			$+$ rbrace $+$
				text "await(cb);"

tryCatchBlock::[String] -> Doc -> Doc
tryCatchBlock exceptions body = 
  (indentBlock "try" body) $+$  vcat ( map makeCatch exceptions)
  where 
    makeCatch ex = indentBlock ("catch ("++ex++" e)") (text "e.printStackTrace();") 

-- Newline infix operator
infixl 5 $#$
($#$) :: Doc -> Doc -> Doc
a $#$ b = a $+$ text "" $+$ b

