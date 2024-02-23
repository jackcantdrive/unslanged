(**************************************
Compiler Construction 2021 - Supo
mjp217@cam.ac.uk

Very much using the structure of the Slang interpreter provided by Tim Griffin
*****************************************) 

(*  Interpreter 0 for L1 

    This is a "definitional" interpreter for  for language L1 
    using high-level constructs of Ocaml (the defining language). 

	The interpreter is deliberately incomplete - we will use the supervisions to populate...
*) 
open Ast 

let complain = Errors.complain

let verbose = ref false 
type var = string 
type address = int 
and value = 
     | INT of int 
     | BOOL of bool
     | UNIT

type env = var -> value 

type binding = var * value

type bindings = binding list


(* type store = address -> value  *)
type store = binding list

(* auxiliary functions *) 

let rec string_of_value = function 
     | INT n -> string_of_int n 
     | BOOL b -> string_of_bool b
     | UNIT -> "()"
    
(* update : (env * binding) -> env 
   update : (store * (address * value)) -> store
*) 
let update(env, (x, v)) = fun y -> if x = y then v else env y

let readint () = let _ = print_string "input> " in read_int() 

let do_unary = function 
  | (NEG,  INT m)  -> INT (-m)
  | (FIB,  INT m)  -> INT (7337)
  | (op, _) -> complain ("malformed unary operator: " ^ (string_of_unary_oper op))

let do_oper = function 
  | (ADD,  INT m,   INT n)  -> INT (m + n)
  | (SUB,  INT m,   INT n)  -> INT (m - n)
  | (MUL,  INT m,   INT n)  -> INT (m * n)
  | (DIV,  INT m,   INT n)  -> INT (m / n)
  | (MOD,  INT m,   INT n)  -> INT (m mod n)
  | (GTEQ,  INT m,   INT n)  -> BOOL (m >= n)
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))

(*
    interpret : (expr * env * store) -> (value * store) 
              : (expr * (var -> value) * address -> value) -> value
*) 

let rec find_var var = function
  | (k,v)::rest when k=var -> v
  | _::rest -> find_var var rest
  | [] -> complain (var ^ " is not defined!\n")

let rec take n list = match (n, list) with
  | 0, _ -> [] | _, [] -> []
  | n, hd::rest -> hd::take (n-1) rest

let rec interpret (e, env, store) = 
  match e with
    | Integer n        -> (INT n, store)
    | Bool b -> (BOOL b, store)
    | Op(e1, op, e2)   -> let (v1, store1) = interpret(e1, env, store) in 
                          let (v2, store2) = interpret(e2, env, store1) in (do_oper(op, v1, v2), store2)
    | UnaryOp(FIB, Integer n) when n < 2 -> (INT n, store)
    | UnaryOp(FIB, Integer n) -> let (v1, store1) = interpret(UnaryOp(FIB, Integer (n-1)), env, store) in 
          let (v2, store2) = interpret(UnaryOp(FIB, Integer (n-2)), env, store1) in (do_oper(ADD, v1, v2), store2)
    | UnaryOp(FIB, e)   -> let (v, store1) = interpret(e, env, store) in (match v with
                  | INT n -> interpret(UnaryOp(FIB, Integer n), env, store1)
                  | UNIT -> complain ("expected INT")
                  | BOOL _ -> complain ("expected INT"))
    | UnaryOp(uop, e)   -> let (v, store1) = interpret(e, env, store) in (do_unary(uop, v), store1) 
    | Seq [e]          -> interpret (e, env, store)
    | Seq []           -> complain ("empty seq")
    | Seq (e :: rest)  -> let (_,  store1) = interpret(e, env, store) 
                          in interpret(Seq rest, env, store1) 
    | Var var -> (find_var var store, store)
    | Assign(var, e) -> let (v1, store1) = interpret(e, env, store) in
                              (UNIT, (var, v1)::store)
    | Para(e1, e2) -> let (v1, store1) = interpret(e1, env, store) in
                        let (v2, store2) = interpret(e2, env, store) in
                        let num_new = List.length store2 - List.length store in 
                        (UNIT, take num_new store2 @ store1)
    | If(e1, e2, e3) -> let (v1, store1) = interpret (e1, env, store) in (match v1 with
                  | BOOL true -> interpret (e2, env, store1)
                  | BOOL false -> interpret (e3, env, store1)
                  | _ -> complain ("expected BOOL"))
    | While(e1, e2) -> let (v1, store1) = interpret (e1, env, store) in (match v1 with
                  | BOOL true -> (interpret (Seq ([e2; While (e1, e2)]), env, store1))
                  | BOOL false -> UNIT, store1
                  | _ -> complain ("expected BOOL"))
    | Dec var -> match (find_var var store) with
                  | INT n -> INT (n-1), (var, INT (n-1))::store
                  | _ -> complain ("expected INT")

(* env_empty : env *) 
let empty_env = fun x -> complain (x ^ " is not defined!\n")

(* store_empty : env *) 
let empty_store = []

(* interpret_top_level : expr -> value *) 
let interpret_top_level e = let (v, _) = interpret(e, empty_env, empty_store) in v 
    


      
    
    
