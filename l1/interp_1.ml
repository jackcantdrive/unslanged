(**************************************
Compiler Construction 2020
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 
(* Interpreter 1. 

   Derived from Interpreter 1 via 
   CPS and DFC transformations applied
   to the code of Interp_0.interpret. 

*) 


open Ast 

let complain = Errors.complain

type address = int 

type value = 
     | INT of int 
     | BOOL of bool
     | UNIT

and closure = var * expr * env 

and continuation_action = 
  | UNARY of unary_oper 
  | OPER of oper * value 
  | OPER_FST of Ast.expr * env * Ast.oper 
  | TAIL of Ast.expr list * env
  | IF of Ast.expr * Ast.expr * env
  | WHILE of Ast.expr * Ast.expr * env
  | ASSIGN of Ast.var * env

and continuation = continuation_action  list

and binding = var * value

and env = binding list

type state = 
   | EXAMINE of expr * env * continuation 
   | COMPUTE of continuation * value 



(* update : (env * binding) -> env *) 
let update(env, (x, v)) = (x, v) :: env 

(* When making a closure, only include bindings that 
   are needed. 
*) 

let rec inlist x = function 
  | [] -> false 
  | y :: rest -> (x = y) || (inlist x rest) 

let rec filter_env fvars = function 
  | [] -> [] 
  | (x, v) :: rest -> if inlist x fvars then (x, v) :: (filter_env fvars rest) else (filter_env fvars rest)


let readint () = let _ = print_string "input> " in read_int() 

let do_unary = function 
  | (NEG,  INT m)  -> INT (-m)
  | (op, _) -> complain ("malformed unary operator: " ^ (string_of_unary_oper op))

let do_oper = function 
  | (ADD,  INT m,   INT n)  -> INT (m + n)
  | (SUB,  INT m,   INT n)  -> INT (m - n)
  | (MUL,  INT m,   INT n)  -> INT (m * n)
  | (DIV,  INT m,   INT n)  -> INT (m / n)
  | (MOD,  INT m,   INT n)  -> INT (m mod n)
  | (GTEQ,  INT m,   INT n)  -> BOOL (m >= n)
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))


let string_of_list sep f l = 
   let rec aux f = function 
     | [] -> ""
     | [t] -> (f t)
     | t :: rest -> (f t) ^  sep  ^ (aux f rest)
   in "[" ^ (aux f l) ^ "]"


let rec string_of_value = function 
     | INT n          -> string_of_int n 
     | BOOL b -> string_of_bool b
     | UNIT -> "()"

and string_of_closure (x, e, env) = x ^ ", " ^ (Ast.string_of_expr e) ^  ", " ^ (string_of_env env)

and string_of_env env = string_of_list ",\n " string_of_binding env 

and string_of_binding (x, v) =    "(" ^ x ^ ", " ^ (string_of_value v) ^ ")"

let string_of_expr_list = string_of_list "; " Ast.string_of_expr

let string_of_continuation_action = function 
  | UNARY op     -> "UNARY " ^ (string_of_unary_oper op)
  | OPER (op, v) -> "OPER(" ^ (string_of_oper op) ^ ", " ^ (string_of_value v) ^ ")"
  | OPER_FST(e, env, op) ->  
      "OPER_FST(" ^ (Ast.string_of_expr e) ^ ", " ^ (string_of_env env) ^ ", " ^ (string_of_oper op) ^ ")"
  | TAIL (el , env) -> "TAIL("  ^ (string_of_expr_list el)   ^ ", " ^ (string_of_env env) ^ ")"
  | IF (e1, e2, env) -> "IF( then"  ^ (string_of_expr e1) ^ " else " ^ (string_of_expr e2)  ^ ", " ^ (string_of_env env) ^ ")"
  | WHILE (e1, e2, env) -> "WHILE("  ^ (string_of_expr e1) ^ " do " ^ (string_of_expr e2)  ^ ", " ^ (string_of_env env) ^ ")"
  | ASSIGN (var, env) -> "ASSIGN("  ^ var ^ " = , " ^ (string_of_env env) ^ ")"


let string_of_continuation = string_of_list ";\n " string_of_continuation_action

let string_of_state = function 
   | EXAMINE(e, env, cnt) -> 
      "EXAMINE(" ^ (Ast.string_of_expr e) ^ ", " 
              ^ (string_of_env env) ^ ", " 
              ^ (string_of_continuation cnt) ^ ")" 
   | COMPUTE(cnt, v)     -> 
      "COMPUTE(" ^ (string_of_continuation cnt) ^ ", " 
               ^ (string_of_value v) ^ ")"


let heap  = Array.make Option.heap_max (INT 0)

let next_address = ref 0 

let new_address () = let a = !next_address in (next_address := a + 1; a) 

let do_assign a v = (heap.(a) <- v)

(* let rec find_var var = function
  | (k,v)::rest when k=var -> v
  | _::rest -> find_var var rest
  | [] -> complain (var ^ " is not defined!\n") *)

module StringMap = Map.Make(String)

(* Define a mutable map *)
let mutable_map = ref StringMap.empty

(* Function to add/update a key-value pair in the map *)
let add_to_map key value =
  mutable_map := StringMap.add key value !mutable_map

(* Function to retrieve a value from the map *)
let get_from_map key =
  StringMap.find_opt key !mutable_map

let find_var var = match (get_from_map var) with
  | Some value -> value
  | None -> complain (var ^ " is not defined!\n")

let assign_var var value = add_to_map var value

let step = function 
 (* EXAMINE --> EXAMINE *) 
 | EXAMINE(UnaryOp(op, e),              env, k) -> EXAMINE(e,  env, (UNARY op) :: k)
 | EXAMINE(Op(e1, op, e2),              env, k) -> EXAMINE(e1, env, OPER_FST(e2, env, op) :: k)
 | EXAMINE(Seq [e],                     env, k) -> EXAMINE(e, env, k) 
 | EXAMINE(Seq (e :: rest),             env, k) -> EXAMINE(e, env, TAIL (rest, env) :: k) 
 (* EXAMINE --> COMPUTE *) 
 | EXAMINE(Integer n,         _, k) -> COMPUTE(k, INT n) 
 (* COMPUTE --> COMPUTE *) 
 | COMPUTE((UNARY op) :: k,    v) -> COMPUTE(k ,(do_unary(op, v)))
 | COMPUTE(OPER(op, v1) :: k, v2) -> COMPUTE(k, do_oper(op, v1, v2))
 (* COMPUTE --> EXAMINE *) 
 | COMPUTE(OPER_FST (e2, env, op) :: k,         v1)  -> EXAMINE(e2, env, OPER (op, v1) :: k)
 | COMPUTE((TAIL (el, env)) :: k,     _)  ->  EXAMINE(Seq el, env, k) 

 | EXAMINE(If (e1, e2, e3), env, k) -> EXAMINE(e1, env, IF(e2, e3, env) :: k)
 | COMPUTE(IF (e2, e3, env) :: k, BOOL true) -> EXAMINE(e2, env, k)
 | COMPUTE(IF (e2, e3, env) :: k, BOOL false) -> EXAMINE(e3, env, k)

 | EXAMINE(While (e1, e2), env, k) -> EXAMINE(e1, env, WHILE(e1, e2, env) :: k)

 (* | COMPUTE(WHILE(e1, e2, env) :: k, BOOL true) -> EXAMINE(e2, env, WHILE(e1, e2, env) :: k) *)
 | COMPUTE(WHILE(e1, e2, env) :: k, BOOL true) -> EXAMINE(Seq(e2::e1::[]), env, WHILE(e1, e2, env) :: k)
 (* | COMPUTE(WHILE(e1, e2, env) :: k, BOOL true) -> EXAMINE(Seq(e2::While(e1, e2)::[]), env, k) *)
 | COMPUTE(WHILE(e1, e2, env) :: k, BOOL false) -> COMPUTE(k, UNIT)
 | EXAMINE(Unit, _, k) -> COMPUTE(k, UNIT) 
 (* | EXAMINE(Var var, env, k) -> COMPUTE (k, find_var var env) *)
 | EXAMINE(Var var, env, k) -> COMPUTE (k, find_var var)
 | EXAMINE(Assign (var, e), env, k) -> EXAMINE (e, env, ASSIGN (var, env) :: k)
 (* | COMPUTE(ASSIGN (var, env) :: k, v) -> print_endline (string_of_state (COMPUTE(ASSIGN (var, env) :: k, v))) ; COMPUTE (k, UNIT) *)

 (* | COMPUTE(ASSIGN (var, env) :: k, v) -> EXAMINE(UNIT, (var, v)::env, k) *)
 (* | COMPUTE(ASSIGN (var, env) :: k, v) -> env; COMPUTE(k, UNIT) *)
 | COMPUTE(ASSIGN (var, env) :: k, v) -> assign_var var v; COMPUTE(k, UNIT)



 | state -> complain ("step : malformed state = " ^ (string_of_state state) ^ "\n")


let rec driver n state = 
  let _ = if Option.verbose 
          then print_string ("\nstate " ^ (string_of_int n) ^ " = \n" ^ (string_of_state state) ^ "\n")
          else () 
  in match state with 
     | COMPUTE([], v) -> v 
     | _              -> driver (n + 1) (step state) 

let eval(e, env) = driver 1 (EXAMINE(e, env, []))

(* env_empty : env *) 
let env_empty = [] 

(* interpret : expr -> value *) 
let interpret e = eval(e, env_empty)

    

      
    
    
