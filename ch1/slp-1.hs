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
