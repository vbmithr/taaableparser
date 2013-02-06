{
  open Lexing
  open Parser
}

let identifier = ['a'-'z''_''0'-'9']+
let blank = [' ''\t']+
let newline = '\n' | "\n\r" | "\r\n"

rule scanner = parse
  | '('      { LPAR }
  | ')'      { RPAR }
  | ':'      { COLON }
  | ','      { COMMA }
  | "=>"     { TO }
  | identifier as id { ID(id) }
  | newline { new_line lexbuf; scanner lexbuf }
  | blank { scanner lexbuf }
  | eof      { EOF }
