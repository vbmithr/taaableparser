open Lexing
open Recettes

let () =
  let ic = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel ic in

  try
    let lines = Parser.receipe Lexer.scanner lexbuf in
    List.iter (fun t -> print_endline (string_of_triple t)) lines
  with Parser.Error ->
    let start_p, curr_p = lexbuf.lex_start_p, lexbuf.lex_curr_p in
    Printf.eprintf "Parse error at line %d, offset %d\n" curr_p.pos_lnum
      (curr_p.pos_cnum - curr_p.pos_bol)
