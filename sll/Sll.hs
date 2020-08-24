main :: IO ()
main = interpret prog

----- Abstract syntax, based on Grammar 1.3 -----

data Stm = CompoundStm Stm Stm
         | AssignStm Ident Exp
         | PrintStm [Exp]

data Exp = IdExp Ident
         | NumExp Int
         | OpExp Binop Exp Exp
         | EseqExp Stm Exp

data Binop = Plus | Minus | Times | Divide

type Ident = String

----- Example programs -----

-- | Example program from page 8.
--
-- Equivalent to
--
-- @a := 5 + 3; b := (printExps(a, a-1), 10*a); printExps(b)
--
-- Output should be:
-- 8 7
-- 80
prog :: Stm
prog = CompoundStm
        (AssignStm "a" (OpExp Plus (NumExp 5) (NumExp 3)))
        (CompoundStm
            (AssignStm "b"
                       (EseqExp
                            (PrintStm [ IdExp "a"
                                      , OpExp Minus (IdExp "a") (NumExp 1)
                                      ])
                            (OpExp Times (NumExp 10) (IdExp "a"))))
            (PrintStm [IdExp "b"]))

----- Interpreter -----

-- | Table of mappings of variable values.
type Table = [(Ident, Int)]

-- | Interprets a program.
interpret :: Stm -> IO ()
interpret s = interpret' s [] >> return ()

-- | Interprets a program with the table of mappings in the 2nd argument.
interpret' :: Stm -> Table -> IO Table
interpret' (CompoundStm x y) ts = interpret' x ts >>= interpret' y
interpret' (AssignStm id ex) ts = do (y, ts') <- eval ex ts
                                     return $ update ts' id y
interpret' (PrintStm (x:xs)) ts = do (y, ts') <- eval x ts
                                     putStr (show y <> " ")
                                     interpret' (PrintStm xs) ts'
interpret' (PrintStm [])     ts = putStrLn "" >> return ts

-- | Evaluates an expression given a tabble of mappings.
eval :: Exp -> Table -> IO (Int, Table)
eval (IdExp id) ((i, v):ts) | id == i   = return (v, (i, v):ts)
                            | otherwise = do (y, ts') <- eval (IdExp id) ts
                                             return (y, (i, v):ts')
eval (IdExp id) []     = error $ "Invalid identifier " <> id
eval (NumExp x) ts     = return (x, ts)
eval (OpExp op l r) ts = do (yl, ts')  <- eval l ts
                            (yr, ts'') <- eval r ts'
                            return $ ((binop op) yl yr, ts'')
eval (EseqExp s e) ts  = interpret' s ts >>= eval e

-- | Returns the corresponding Haskell function that evaluates the represented
-- binary operation.
binop :: Binop -> (Int -> Int -> Int)
binop Plus   = (+)
binop Minus  = (-)
binop Times  = (*)
binop Divide = div

-- | @update ts i v@ returns table @ts@ with @i@ newly mapped to @v@.
update :: Table -> Ident -> Int -> Table
update (t:ts) i v | fst t == i = (i, v) : ts
                  | otherwise  = t : update ts i v
update [] i v = [(i, v)]
