(* 
   The Parsed AST 
*) 
type var = string 

type loc = Lexing.position 

type type_expr = 
   | TEint 
   | TEbool 
   | TEunit 
   | TEref of type_expr 
   | TEarrow of type_expr * type_expr
   | TEproduct of type_expr * type_expr
   | TEunion of type_expr * type_expr

type oper = ADD | MUL | DIV | SUB | MOD | GTEQ

type unary_oper = NEG | FIB

type expr = 
    | Integer of loc * int
    | UnaryOp of loc * unary_oper * expr
    | Op of loc * expr * oper * expr
    | Seq of loc * (expr list)
    | Var of loc * var
    | Assign of loc * var * expr
    | Para of loc * expr * expr
    | Bool of loc * bool
    | While of loc * expr * expr
    | If of loc * expr * expr * expr
    | Dec of loc * var
    | Unit of loc

val loc_of_expr : expr -> loc 
val string_of_loc : loc -> string 

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_type : type_expr -> string 
val string_of_expr : expr -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit


