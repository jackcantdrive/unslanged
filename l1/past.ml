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

type formals = (var * type_expr) list

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


and lambda = var * type_expr * expr 

let loc_of_expr = function 
    | Integer (loc, _)              -> loc 
    | UnaryOp(loc, _, _)            -> loc 
    | Op(loc, _, _, _)              -> loc 
    | Seq(loc, _)                   -> loc
    | Var(loc, _) -> loc
    | Assign(loc, _, _) -> loc
    | Para(loc, _, _) -> loc
    | Bool(loc, _) -> loc
    | While(loc, _, _) -> loc
    | If(loc, _, _, _) -> loc
    | Dec(loc, _) -> loc


let string_of_loc loc = 
    "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
    "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))

open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 

let rec pp_type = function 
  | TEint -> "int" 
  | TEbool -> "bool" 
  | TEunit -> "unit" 
  | TEref t           -> "(" ^ (pp_type t) ^ " ref)"
  | TEarrow(t1, t2)   -> "(" ^ (pp_type t1) ^ " -> " ^ (pp_type t2) ^ ")" 
  | TEproduct(t1, t2) -> "(" ^ (pp_type t1) ^ " * " ^ (pp_type t2) ^ ")"  
  | TEunion(t1, t2)   -> "(" ^ (pp_type t1) ^ " + " ^ (pp_type t2) ^ ")"  

let pp_uop = function 
  | NEG -> "-" 
  | FIB -> "fib"

let pp_bop = function 
  | ADD -> "+" 
  | MUL  -> "*" 
  | DIV  -> "/" 
  | SUB -> "-" 
  | MOD -> "%" 
  | GTEQ -> ">="

let string_of_oper = pp_bop 
let string_of_unary_oper = pp_uop 

let fstring ppf s = fprintf ppf "%s" s
let pp_type ppf t = fstring ppf (pp_type t) 
let pp_unary ppf op = fstring ppf (pp_uop op) 
let pp_binary ppf op = fstring ppf (pp_bop op) 

(* ignore locations *) 
let rec pp_expr ppf = function 
    | Integer (_, n)      -> fstring ppf (string_of_int n)
    | UnaryOp(_, op, e)   -> fprintf ppf "%a(%a)" pp_unary op pp_expr e 
    | Op(_, e1, op, e2)   -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2 
    | Seq (_, [])         -> () 
    | Seq (_, [e])        -> pp_expr ppf e 
    | Seq (l, e :: rest)  -> fprintf ppf "%a; %a" pp_expr e pp_expr (Seq(l, rest))
    | Var (_, var) -> fstring ppf (var)
    | Assign (_, var, e) -> fprintf ppf "%s = %a" var pp_expr e
    | Para (l, e1, e2) -> fprintf ppf "%a | %a" pp_expr e1 pp_expr e2
    | Bool (_, b) -> fstring ppf (string_of_bool b)
    | While (_, e1, e2) -> fprintf ppf "while %a do %a end" pp_expr e1 pp_expr e2
    | If (_, e1, e2, e3) -> fprintf ppf "if %a then %a else %a" pp_expr e1 pp_expr e2 pp_expr e3
    | Dec (_, var) -> fprintf ppf "--%s" var

let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in print_flush () 

(* useful for degugging *) 


let string_of_uop = function 
  | NEG -> "NEG" 
  | FIB -> "FIB"

let string_of_bop = function 
  | ADD -> "ADD" 
  | MUL  -> "MUL" 
  | DIV  -> "DIV" 
  | SUB -> "SUB"
  | MOD -> "MOD"
  | GTEQ -> "GTEQ"

let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_type = function 
  | TEint             -> "TEint" 
  | TEbool            -> "TEbool" 
  | TEunit            -> "TEunit" 
  | TEref t           -> mk_con "TEref" [string_of_type t] 
  | TEarrow(t1, t2)   -> mk_con "TEarrow" [string_of_type t1; string_of_type t2] 
  | TEproduct(t1, t2) -> mk_con "TEproduct" [string_of_type t1; string_of_type t2] 
  | TEunion(t1, t2)   -> mk_con "TEunion" [string_of_type t1; string_of_type t2] 

let rec string_of_expr = function 
    | Integer (_, n)      -> mk_con "Integer" [string_of_int n] 
    | UnaryOp(_, op, e)   -> mk_con "UnaryOp" [string_of_uop op; string_of_expr e]
    | Op(_, e1, op, e2)   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | Seq (_, el)         -> mk_con "Seq" [string_of_expr_list el]
    | Var(_, var) -> mk_con "Var" [var]
    | Assign(_, var, e) -> mk_con "Assign" [var; string_of_expr e]
    | Para(_, e1, e2) -> mk_con "Para" [string_of_expr e1; string_of_expr e2]
    | Bool(_, b) -> mk_con "Bool" [string_of_bool b] 
    | If(_, e1, e2, e3) -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | While(_, e1, e2) -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Dec(_, var) -> mk_con "Dec" [var]


and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)
