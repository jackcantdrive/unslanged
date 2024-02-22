(*   translate_expr : Past.expr -> Ast.expr 
     
	 Lifted and amended from the original Slang interpreter

*) 

let translate_uop = function 
  | Past.NEG -> Ast.NEG 
  | Past.FIB -> Ast.FIB

let translate_bop = function 
  | Past.ADD -> Ast.ADD 
  | Past.MUL -> Ast.MUL
  | Past.DIV -> Ast.DIV
  | Past.SUB -> Ast.SUB
  | Past.MOD -> Ast.MOD
  | Past.GTEQ -> Ast.GTEQ


let rec translate_expr = function 
    | Past.Integer(_, n)     -> Ast.Integer n
    | Past.UnaryOp(_, op, e) -> Ast.UnaryOp(translate_uop op, translate_expr e)
    | Past.Op(_, e1, op, e2) -> Ast.Op(translate_expr e1, translate_bop op, translate_expr e2)
    | Past.Seq(_, e1) -> Ast.Seq(List.map translate_expr e1)
    | Past.Var(_, var) -> Ast.Var var
    | Past.Assign(_, var, e) -> Ast.Assign(var, translate_expr e)
    | Past.Para(_, e1, e2) -> Ast.Para(translate_expr e1, translate_expr e2)
    | Past.If(_, e1, e2, e3) -> Ast.If(translate_expr e1, translate_expr e2, translate_expr e3)
    | Past.While(_, e1, e2) -> Ast.While(translate_expr e1, translate_expr e2)
    | Past.Bool(_, b) -> Ast.Bool b
    | Past.Dec(_, var) -> Ast.Dec var
