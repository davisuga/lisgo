let () = print_endline "Hey"

open Expr
open Typ
module Ctx = Map.Make (String)

type value =
  | VClosure of { context : value Ctx.t; param : string; body : expr }
  | VNative of (value -> value)
  | VInt of int
  | VBool of bool
  | VString of string

exception Type_error of string

(* let parse_sexpr (expr_list: expr list) =
   match expr_list with
   | [] -> List [Unit]
   | (x:: xs) ->
    match x with
    | (Variable s) -> List [Symbol s]
     | (List l) -> List [List l]
     | (Vector v) -> List [Vector v] *)

let _ = Golang.CodeGen.of_typ Golang.Types.TInt8

let rec infer context expr =
  let rec check_literal_list_type expr_list current_typ =
    match expr_list with
    | [] -> TList current_typ
    | [ fst ] ->
      if current_typ == infer context fst
      then TList current_typ
      else raise (Type_error "List type mismatch")
    | fst :: tail ->
      if current_typ == infer context fst
      then check_literal_list_type tail (infer context fst)
      else raise (Type_error "List type mismatch")
  in
  let check_sexpr expr_list =
    match expr_list with
    | [] -> List [ Unit ]
    | x :: _ ->
      (match x with
      | Variable s -> List [ Variable s ]
      | List l -> List [ List l ]
      | Int v -> List [ Int v ]
      | _ -> List [ Unit ])
  in
  match expr with
  | Int _ -> TInt
  | SExpr expr_list -> infer context (check_sexpr expr_list)
  | True | False -> TBool
  | Unit -> TUnit
  | String _ -> TStr
  | Float _ -> TFloat
  (* TODO: Introduce parametric types for lists *)
  | List [] -> TList TUnit
  | List (first_expr :: expressions) ->
    check_literal_list_type expressions (infer context first_expr)
  | Variable name ->
    (match Ctx.find_opt name context with
    | Some typ -> typ
    | None -> raise @@ Type_error ("Could not find variable: " ^ name))
  | Abstraction { param; param_type; body } ->
    let context = Ctx.add param param_type context in
    let body_typ = infer context body in
    TArrow { param_type; body_typ }
  | Application { func; arg } ->
    let func_typ = infer context func in
    let arg_typ = infer context arg in
    (match func_typ with
    | TArrow { param_type; body_typ } when Typ.equal param_type arg_typ -> body_typ
    | TArrow { param_type; _ } ->
      raise
        (Type_error
           ("Your function wants "
           ^ Typ.show_typ param_type
           ^ " but you gave it "
           ^ Typ.show_typ arg_typ))
    | called ->
      raise
      @@ Type_error ("You are fucking calling " ^ Typ.show_typ called ^ " as a function"))

let sum = Application { func = Variable "+"; arg = List [ Int 1; Int 2 ] }
let sum_and_print = Application { func = Variable "println"; arg = sum }

let initial_type_context =
  Ctx.empty
  |> Ctx.add "println" (TArrow { param_type = TInt; body_typ = TUnit })
  |> Ctx.add
       "+"
       (TArrow
          { param_type = TInt; body_typ = TArrow { param_type = TInt; body_typ = TInt } })

let getarg_opt n =
  try Some Sys.argv.(n) with
  | Invalid_argument _ -> None

module Option = struct
  include Option

  let get_or default optional =
    match optional with
    | Some value -> value
    | None -> default
end

let snd _ a = a
let fst a _ = a

let print_prog_info p =
  getarg_opt 1
  |> Option.get_or p
  |> Lexer.from_string Parser.expr_opt
  |> Option.get
  |> fun ast ->
  Format.printf "AST: %a\n%!" Expr.pp_expr ast;
  (* ast *)
  (* |> infer initial_type_context
  |> fun typ ->
  Format.printf "TYPE: %a\n%!" Typ.pp_typ typ; *)
  ast

type 'a localized = { start_pos : int; end_pos : int; value : 'a }
