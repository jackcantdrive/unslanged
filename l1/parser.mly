/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token<string> IDENT
%token EQUAL
%token ADD SUB MUL DIV SEMICOLON FIB MOD GTEQ
%token LPAREN RPAREN
%token BEGIN END IF DO THEN ELSE WHILE
%token EOF
%token BAR
%left BAR
%left EQUAL
%left ADD SUB        /* lowest precedence */
%left MUL DIV MOD         /* medium precedence */
%nonassoc UMINUS FIB        /* highest precedence */
%nonassoc IDENT


%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
simple_expr:
| INT                                { Past.Integer (get_loc(), $1) }
| IDENT                              { Past.Var (get_loc(), $1) }
| LPAREN expr RPAREN                 { $2 }

expr:
| simple_expr                        {  $1 }
| UMINUS expr                           { Past.UnaryOp(get_loc(), Past.NEG, $2) }
| FIB expr                           { Past.UnaryOp(get_loc(), Past.FIB, $2) }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr SUB expr                      { Past.Op(get_loc(), $1, Past.SUB, $3) }
| expr MUL expr                      { Past.Op(get_loc(), $1, Past.MUL, $3) }
| expr DIV expr                      { Past.Op(get_loc(), $1, Past.DIV, $3) }
| expr MOD expr                      { Past.Op(get_loc(), $1, Past.MOD, $3) }
| BEGIN exprlist END                 { Past.Seq(get_loc(), $2) }
| IDENT EQUAL expr { Past.Assign(get_loc(), $1, $3) }
| expr BAR expr { Past.Para(get_loc(), $1, $3) }
| WHILE expr DO expr END { Past.While(get_loc(), $2, $4) }
| IF expr THEN expr ELSE expr { Past.If(get_loc(), $2, $4, $6) }
| expr GTEQ expr { Past.Gteq(get_loc(), $1, $3) }

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }


