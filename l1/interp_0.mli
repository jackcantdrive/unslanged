type address 
type var = string


and value = 
     | INT of int 
     | BOOL of bool
     | UNIT

type env = Ast.var -> value 


type binding = var * value
type store = binding list

val string_of_value : value -> string 

val interpret :  Ast.expr * env * store -> (value * store) 

val interpret_top_level : Ast.expr -> value 




