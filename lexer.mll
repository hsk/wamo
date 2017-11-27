{
  open Parser
}
let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']
let alphaNum = lower | upper | digit
let atom     = lower alphaNum*
let variable = upper alphaNum*

rule token = parse
  | [' ' '\t' '\n']   { token lexbuf }
  | "%" [^'\n']*      { token lexbuf }
  | atom as a '('     { ATOM_LPAREN a }
  | variable as a '(' { VARIABLE_LPAREN a }
  | atom as a         { ATOM a }
  | variable as a     { VARIABLE a }
  | "?-"              { GOAL }
  | ":-"              { BODY }
  | "|"               { BAR }
  | "."               { DOT }
  | ","               { COMMA }
  | "("               { LPAREN }
  | ")"               { RPAREN }
  | "["               { LBRACK }
  | "]"               { RBRACK }
  | eof               { EOF }
