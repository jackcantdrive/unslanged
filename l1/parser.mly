/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token<string> IDENT
%token UNIT
%token EQUAL GTEQ DEC
%token ADD SUB MUL DIV SEMICOLON FIB MOD
%token LPAREN RPAREN
%token BEGIN END IF DO THEN ELSE WHILE
%token EOF
%token BAR
%right SEMICOLON
%left BAR
%left EQUAL
%left GTEQ
%left ADD SUB        /* lowest precedence */
%left MUL DIV MOD         /* medium precedence */
%nonassoc UMINUS FIB DEC        /* highest precedence */
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
| UNIT                                { Past.Unit (get_loc()) }
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
| expr GTEQ expr                     { Past.Op(get_loc(), $1, Past.GTEQ, $3) }
| exprlist                           { Past.Seq(get_loc(), $1) }
| IDENT EQUAL expr { Past.Assign(get_loc(), $1, $3) }
| expr BAR expr { Past.Para(get_loc(), $1, $3) }
| WHILE expr DO expr END { Past.While(get_loc(), $2, $4) }
| IF expr THEN expr ELSE expr END { Past.If(get_loc(), $2, $4, $6) }
| IF expr THEN expr END { Past.If(get_loc(), $2, $4, Past.Seq (get_loc(), [])) }
| DEC IDENT { Past.Dec(get_loc(), $2) }

exprlist:
| expr SEMICOLON exprlist   { $1 :: $3 }
| expr SEMICOLON expr       { $1 :: $3 :: [] }


