open Sedlexing.Utf8
open Parser

exception Invalid_token of string

let bin_op =
  [%sedlex.regexp?
    '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|' | '~' | '!' | '=' | '<' | '>']

let whitespace = [%sedlex.regexp? Plus (' ' | '\n' | '\t')]
let lower_alpha = [%sedlex.regexp? 'a' .. 'z']
let upper_alpha = [%sedlex.regexp? 'A' .. 'Z']
let number = [%sedlex.regexp? '0' .. '9']
let ident = [%sedlex.regexp? lower_alpha, Star (lower_alpha | number | '_') | bin_op]
let int_literal = [%sedlex.regexp? Plus number]
let float_literal = [%sedlex.regexp? Plus number, '.', Plus number]

let symbol =
  [%sedlex.regexp?
    ( '+'
    | '-'
    | '*'
    | '/'
    | '%'
    | '^'
    | '&'
    | '|'
    | '~'
    | '!'
    | '='
    | '<'
    | '>'
    | '.'
    | ','
    | ':'
    | ';'
    | '('
    | ')'
    | '['
    | ']'
    | '{'
    | '}'
    | '#'
    | '@'
    | '$'
    | '?'
    | '\\'
    | '"'
    | '\''
    | '`' )]

let string_literal =
  [%sedlex.regexp?
    '"', Star (upper_alpha | lower_alpha | number | symbol | whitespace), '"']

let get_string_content s = String.sub s 1 (String.length s - 2)

let rec tokenizer buf =
  match%sedlex buf with
  | "fn" -> FN
  | "true" -> TRUE
  | "false" -> FALSE
  | whitespace -> tokenizer buf
  | ident -> IDENT (lexeme buf)
  | int_literal -> INT (lexeme buf |> int_of_string)
  | string_literal -> STRING (get_string_content (lexeme buf))
  | float_literal -> FLOAT (lexeme buf |> float_of_string)
  | ',' -> COMMA
  | ':' -> COLON
  | "->" -> ARROW
  | '(' -> LEFT_PAREN
  | '[' -> LEFT_BRACK
  | ']' -> RIGHT_BRACK
  | ')' -> RIGHT_PAREN
  | any -> raise @@ Invalid_token (lexeme buf)
  | eof -> EOF
  | _ -> assert false

let string_of_token = function
  | FN -> "FN"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | TRUE -> "TRUE"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | RIGHT_BRACK -> "RIGHT_BRACK"
  | LEFT_PAREN -> "LEFT_PAREN"
  | LEFT_BRACK -> "LEFT_BRACK"
  | FALSE -> "FALSE"
  | EOF -> "EOF"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | ARROW -> "ARROW"
  | STRING a -> "STRING(" ^ a ^ ")"
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  token, start, stop

let from_string f string =
  provider (from_string string) |> MenhirLib.Convert.Simplified.traditional2revised f
