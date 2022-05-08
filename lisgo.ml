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

exception Type_error

let rec infer context expr =
  match expr with
  | Int _ -> TInt
  | True | False -> TBool
  | Unit -> TUnit
  | String _ -> TStr
  | Float _ -> TFloat
  | List expression ->
      let rec check_list_type (expr_list : expr list) =
        match expr_list with
        (* TODO: Introduce parametric types for lists *)
        | [] -> TList TUnit
        | [ fst ] -> TList (infer context fst)
        | fst :: tail ->
            let fst_typ = infer context fst in
            if fst_typ == infer context (List tail) then TList fst_typ
            else raise Type_error
      in

      check_list_type expression
  | Variable name -> (
      match Ctx.find_opt name context with
      | Some typ -> typ
      | None -> raise Type_error)
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
      | _ -> raise Type_error)

let sum =
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

let initial_context =
  Ctx.empty
  |> Ctx.add "println" (TArrow { param_type = TInt; body_typ = TUnit })

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
