
type var = string 

type oper = ADD | MUL | DIV | SUB | MOD | GTEQ

type unary_oper = NEG | FIB

type expr = 
       | Integer of int
       | UnaryOp of unary_oper * expr
       | Op of expr * oper * expr
       | Seq of (expr list)
       | Var of var
       | Assign of var * expr
       | Para of expr * expr
       | If of expr * expr * expr
       | Bool of bool
       | While of expr * expr
       | Dec of var
       | Unit

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_uop : unary_oper -> string 
val string_of_bop : oper -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit
val string_of_expr : expr -> string 
