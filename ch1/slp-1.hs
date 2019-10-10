type Id = String

data Binop = Plus | Minus | Times | Div

data Stm = CompoundStm Stm Stm
    | AssignStm Id Exp
    | PrintStm [Exp]

data Exp = IdExp Id
    | NumExp Int
    | OpExp Exp Binop Exp
    | EseqExp Stm Exp

prog = CompoundStm (AssignStm "a"
                    (OpExp (NumExp 5) Plus (NumExp 3)))
       (CompoundStm
         (AssignStm "b" (EseqExp
                            (PrintStm
                              [(IdExp "a"),
                               (OpExp (IdExp "a")
                                 Minus (NumExp 1))])
                            (OpExp (NumExp 10) Times (IdExp "a"))))
         (PrintStm [IdExp "b"]))

-- Task 1
maxargs :: Stm -> Int
maxargs (CompoundStm stm1 stm2) =
  max (maxargs stm1) (maxargs stm2)
maxargs (AssignStm _ exp) = maxhelper exp
maxargs (PrintStm list)
  = max (length list) (maximum (map maxhelper list))

maxhelper :: Exp -> Int
maxhelper (IdExp _) = 0
maxhelper (NumExp _) = 0
maxhelper (OpExp exp1 _ exp2)
  = max (maxhelper exp1) (maxhelper exp2)
maxhelper (EseqExp stm exp) = max (maxargs stm) (maxhelper exp)

-- From the book:
--
--    “For example, `maxargs(prog)` is `2`.”
testMaxargs = (maxargs prog) == 2
