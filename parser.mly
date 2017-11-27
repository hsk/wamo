%{
open Ast
let rec makeLst ls cdr = match ls with
  | [] -> cdr
  | x::xs -> T ("cons", [x; makeLst xs cdr])
%}
%token <string> ATOM VARIABLE ATOM_LPAREN VARIABLE_LPAREN
%token GOAL BODY
%token BAR DOT COMMA EOF
%token LPAREN RPAREN LBRACK RBRACK
%type <Ast.program> parseprolog
%start parseprolog
%%
struct_    : ATOM                      { T ($1, []) }
           | ATOM_LPAREN arguments     { T ($1, $2) }
           | VARIABLE                  { V $1 }
           | VARIABLE_LPAREN RPAREN    { V $1 }
           | VARIABLE_LPAREN arguments { V2 ($1, $2) }
list_      : LBRACK terms listTail     { makeLst $2 $3 }
listTail   : BAR term RBRACK           { $2 }
           | RBRACK                    { T ("nil",[]) }
arguments  : terms RPAREN              { $1 }
           | /* empty */               { [] }
term       : struct_                   { $1 }
           | list_                     { $1 }
terms      : /* empty */               { [] }
           | term                      { [$1] }
           | term COMMA terms          { $1::$3 }
goal       : GOAL terms DOT            { $2 }
clause     : term body DOT             { $1,$2 }
body       : BODY terms                { $2 }
           | /* empty */               { [] }
clauses    : clause clauses            { $1::$2 }
           | /* empty */               { [] }
prolog     : goal clauses              { $1,$2 }
parseprolog: prolog EOF                { $1 }
