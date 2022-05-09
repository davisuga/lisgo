open Sedlexing.Utf8
open Parser

exception Invalid_token

let whitespace = [%sedlex.regexp? Plus (' ' | '\n' | '\t')]
let lower_alpha = [%sedlex.regexp? 'a' .. 'z']
let upper_alpha = [%sedlex.regexp? 'A' .. 'Z']
let number = [%sedlex.regexp? '0' .. '9']
let ident = [%sedlex.regexp? lower_alpha, Star (lower_alpha | number | '_')]
let int_literal = [%sedlex.regexp? Plus number]
let float_literal = [%sedlex.regexp? Plus number, '.', Plus number]

let symbol =
  [%sedlex.regexp?
    ( '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|' | '~' | '!' | '=' | '<'
    | '>' | '.' | ',' | ':' | ';' | '(' | ')' | '[' | ']' | '{' | '}' | '#'
    | '@' | '$' | '?' | '\\' | '"' | '\'' | '`' )]

let string_literal =
  [%sedlex.regexp?
    '"', Star (upper_alpha | lower_alpha | number | symbol | whitespace), '"']

let get_string_content s = String.sub s 1 (String.length s - 1)

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | ident -> IDENT (lexeme buf)
  | int_literal -> INT (lexeme buf |> int_of_string)
  | string_literal -> STRING (get_string_content (lexeme buf))
  | float_literal -> FLOAT (lexeme buf |> float_of_string)
  | "true" -> TRUE
  | "false" -> FALSE
  | "fn" -> FN
  | ':' -> COLON
  | "->" -> ARROW
  | '(' -> LEFT_PAREN
  | ')' -> RIGHT_PAREN
  (* | any -> if lexeme buf = "λ" then LAMBDA else raise Invalid_token *)
  | eof -> EOF
  | _ -> assert false

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

let from_string f string =
  provider (from_string string)
  |> MenhirLib.Convert.Simplified.traditional2revised f
