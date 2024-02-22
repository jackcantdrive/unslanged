
type var = string 

type oper = ADD | MUL | DIV | SUB | MOD

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
       | Gteq of expr * expr

and lambda = var * expr 


open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 

let pp_uop = function 
  | NEG -> "-" 
  | FIB -> "fib" 


let pp_bop = function 
  | ADD -> "+" 
  | MUL  -> "*" 
  | DIV  -> "/" 
  | SUB -> "-" 
  | MOD -> "%" 


let string_of_oper = pp_bop 
let string_of_unary_oper = pp_uop 

let fstring ppf s = fprintf ppf "%s" s

let pp_unary ppf t = fstring ppf (pp_uop t) 

let pp_binary ppf t = fstring ppf (pp_bop t) 

let rec pp_expr ppf = function 
    | Integer n        -> fstring ppf (string_of_int n)
    | UnaryOp(op, e)   -> fprintf ppf "%a(%a)" pp_unary op pp_expr e 
    | Op(e1, op, e2)   -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2 

    | Seq el           -> fprintf ppf "begin %a end" pp_expr_list el 
    | Var var          -> fprintf ppf "%s" var
    | Assign (var, e) -> fprintf ppf "begin %s = %a end" var pp_expr e
    | Para(e1, e2)           -> fprintf ppf "%a|%a" pp_expr e1 pp_expr e2
    | Bool b -> fstring ppf (string_of_bool b)
    | While (e1, e2) -> fprintf ppf "while %a do %a end" pp_expr e1 pp_expr e2
    | If (e1, e2, e3) -> fprintf ppf "if %a then %a else %a" pp_expr e1 pp_expr e2 pp_expr e3
    | Gteq (e1, e2) -> fprintf ppf "%a >= %a" pp_expr e1 pp_expr e2
	
and pp_expr_list ppf = function 
  | [] -> () 
  | [e] -> pp_expr ppf e 
  |  e:: rest -> fprintf ppf "%a; %a" pp_expr e pp_expr_list rest


let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in pp_print_flush err_formatter () 

(* useful for debugging *) 

let string_of_uop = function 
  | NEG -> "NEG" 
  | FIB -> "FIB" 

let string_of_bop = function 
  | ADD -> "ADD" 
  | MUL  -> "MUL" 
  | DIV  -> "DIV" 
  | SUB -> "SUB" 
  | MOD -> "MOD"

let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_expr = function 
    | Integer n        -> mk_con "Integer" [string_of_int n] 
    | UnaryOp(op, e)   -> mk_con "UnaryOp" [string_of_uop op; string_of_expr e]
    | Op(e1, op, e2)   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | Seq el           -> mk_con "Seq" [string_of_expr_list el]
    | Var(var) -> mk_con "Var" [var]
    | Assign(var, e) -> mk_con "Assign" [var; string_of_expr e]
    | Para (e1, e2)           -> mk_con "Para" [string_of_expr e1; string_of_expr e2]
    | If (e1, e2, e3)           -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | While (e1, e2)           -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Gteq (e1, e2)           -> mk_con "Gteq" [string_of_expr e1; string_of_expr e2]
    | Bool b           -> mk_con "Bool" [string_of_bool b]

and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)

