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

let rec infer context expr =
  match expr with
  | Int _ -> TInt
  | True | False -> TBool
  | Unit -> TUnit
  | String _ -> TStr
  | Float _ -> TFloat
  (* TODO: Introduce parametric types for lists *)
  | List [] -> TList TUnit
  | List (first_expr :: expressions) ->
      let rec check_list_type expr_list current_typ =
        match expr_list with
        | [] -> TList current_typ
        | [ fst ] ->
            if current_typ == infer context fst then TList current_typ
            else raise (Type_error "List type mismatch")
        | fst :: tail ->
            if current_typ == infer context fst then
              check_list_type tail (infer context fst)
            else raise (Type_error "List type mismatch")
      in
      check_list_type expressions (infer context first_expr)
  | Variable name -> (
      match Ctx.find_opt name context with
      | Some typ -> typ
      | None -> raise @@ Type_error ("Could not find variable: " ^ name))
  | Abstraction { param; param_type; body } ->
      let context = Ctx.add param param_type context in
      let body_typ = infer context body in
      TArrow { param_type; body_typ }
  | Application { func; arg } -> (
      let func_typ = infer context func in
      let arg_typ = infer context arg in
      match func_typ with
      | TArrow { param_type; body_typ } when Typ.equal param_type arg_typ ->
          body_typ
      | TArrow { param_type; _ } ->
          raise
            (Type_error
               ("Your function wants" ^ Typ.show_typ param_type
              ^ "but you gave it" ^ Typ.show_typ arg_typ))
      | called ->
          raise
          @@ Type_error
               ("You are fucking calling " ^ Typ.show_typ called
              ^ " as a function"))

let sum = Application { func = Variable "+"; arg = List [ Int 1; Int 2 ] }
let sum_and_print = Application { func = Variable "println"; arg = sum }

let initial_type_context =
  Ctx.empty
  |> Ctx.add "println" (TArrow { param_type = TInt; body_typ = TUnit })
  |> Ctx.add "+"
       (TArrow
          {
            param_type = TInt;
            body_typ = TArrow { param_type = TInt; body_typ = TInt };
          })

let () =
  infer initial_type_context sum_and_print |> Typ.show_typ |> print_endline

let getarg_opt n = try Some Sys.argv.(n) with Invalid_argument _ -> None

let get_or default optional =
  match optional with Some value -> value | None -> default

let _ =
  getarg_opt 1 |> get_or "(println (+ 1 2))"
  |> Lexer.from_string Parser.expr_opt
  |> Option.get
  |> Format.printf "%a\n%!" Expr.pp_expr

(* (println ((+ 1) 2))

    ->
   Application
       {
         func = Variable "println";
         arg =
           Application
             {
               func = Application { func = Variable "+"; arg = Int 1 };
               arg = Int 2;
             };
       }

    ->
    File
      {
        name = "main.go";
        declarations =
          [
            GenDeclaration
              (ImportList { imports = [ { path = "fmt"; name = None } ] });
            FunDeclaration
              {
                name = "main";
                body =
                  [
                    ExprStmt
                      (Application
                         {
                           func = Ident "fmt.PrintLn";
                           arg =
                             BinExpr { op = Add; left = Int 1; right = Int 2 };
                         });
                  ];
              };
          ];
      }

    ->

    package main

    import (
      "fmt"
    )

    func main () {
       fmt.Println(1 + 2)
    }
*)

type 'a localized = { start_pos : int; end_pos : int; value : 'a }

module Go = struct
  type bin_operation =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | Neq
    | Lt
    | Leq
    | Gt
    | Geq

  type import = { name : string option; path : string }

  type expr =
    | Int of int
    | String of string
    | Ident of string
    | Application of { func : expr; arg : expr }
    | BinExpr of { op : bin_operation; left : expr; right : expr }

  type stmt =
    | DeclarationStmt
    | AssignStmt of { left : string; right : expr }
    | ExprStmt of expr
    | ReturnStmt of { results : expr list }

  type gen_declaration =
    | ImportList of { imports : import list }
    | TypeDeclaration of { name : string; typ : string }

  type fun_declaration = { body : stmt list; name : string }

  type declaration =
    | GenDeclaration of gen_declaration
    | FunDeclaration of fun_declaration
  (*
     type typ =
       | TGoInt of int
       | TGoBool of bool
       | TGoFloat64 of float
       | TGoFunc of {
           type_params : typ list;
           params : string list;
           statements : stmt list;
         } *)

  type file = File of { name : string; declarations : declaration list }

  (* type field_list = { name : string; typ : typ } *)
end

(* let rec interpret current_context expr =
   match expr with
   | Int i -> VInt i
   | Variable name -> Ctx.find name current_context
   | Abstraction { param; body } ->
       VClosure { context = current_context; param; body }
   | Application { func; arg } -> (
       let arg = interpret current_context arg in
       match interpret current_context func with
       | VClosure { context; param; body } ->
           interpret (Ctx.add param arg context) body
       | VNative func -> func arg
       | VInt _ ->
           raise @@ Type_error "Int is not a funcion, you cant apply it bro") *)
