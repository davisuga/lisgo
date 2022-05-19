type bin_operation = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq
type import = { name : string option; path : string }

module Types = struct
  type t =
    | TInt8
    | TInt16
    | TInt32
    | TInt64
    | TUint8
    | TUint16
    | TUint32
    | TUint64
    | TUintptr
    | TFloat32
    | TFloat64
    | TStr
    | TBool
    | TList of t
    | TStruct of { name : string }
    | TPointer of t
    | TVoid
    | TFunction of { args : func_type_parameter list; ret : t }

  and func_type_parameter = { name : string; typ : t }
end

open Types

type expr =
  | Int of int
  | String of string
  | Ident of string
  | Application of { func_name : string; args : expr list }
  | BinExpr of { op : bin_operation; left : expr; right : expr }

type stmt =
  | DeclStmt of { name : string; value : expr }
  | AssignStmt of { left : string; right : expr }
  | ExprStmt of expr
  | ReturnStmt of { results : expr list }

type gen_declaration =
  | ImportList of { imports : import list }
  | TypeDeclaration of { name : string; typ : string }

type fun_declaration =
  { body : stmt list; name : string; params : func_type_parameter list; ret : Types.t }

type declaration = GenDeclaration of gen_declaration | FunDeclaration of fun_declaration
type file = { name : string; declarations : declaration list }

let empty_file =
  { name = "main.go"
  ; declarations =
      [ FunDeclaration { name = "main"; body = []; params = []; ret = TVoid } ]
  }

module CodeGen = struct
  let rec of_expr expr =
    match expr with
    | Int i -> string_of_int i
    | String s -> "\"" ^ s ^ "\""
    | Ident s -> s
    | Application { func_name; args } ->
      Printf.sprintf "%s(%s)" func_name (List.map of_expr args |> String.concat " ")
    | BinExpr { op; left; right } ->
      Printf.sprintf
        "%s %s %s"
        (of_expr left)
        (match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Mod -> "%"
        | Eq -> "=="
        | Neq -> "!="
        | Lt -> "<"
        | Leq -> "<="
        | Gt -> ">"
        | Geq -> ">=")
        (of_expr right)

  let rec of_typ typ =
    match typ with
    | TInt8 -> "int8"
    | TInt16 -> "int16"
    | TInt32 -> "int32"
    | TInt64 -> "int64"
    | TUint8 -> "uint8"
    | TUint16 -> "uint16"
    | TUint32 -> "uint32"
    | TUint64 -> "uint64"
    | TUintptr -> "uintptr"
    | TFloat32 -> "float32"
    | TFloat64 -> "float64"
    | TVoid -> ""
    | TStr -> "string"
    | TBool -> "bool"
    | TList t -> Printf.sprintf "[]%s" (of_typ t)
    | TStruct { name } -> name
    | TPointer t -> "*" ^ of_typ t
    | TFunction { args; ret } ->
      Printf.sprintf
        "func(%s)%s"
        (List.map of_type_parameter args |> String.concat ", ")
        (of_typ ret)

  and of_type_parameter (param : func_type_parameter) =
    Printf.sprintf "%s %s" param.name (of_typ param.typ)

  let of_statement stmt =
    match stmt with
    | DeclStmt { name; value } -> Printf.sprintf "var %s = %s\n" name (of_expr value)
    | AssignStmt { left; right } -> left ^ " = " ^ of_expr right ^ "\n"
    | ExprStmt expr -> of_expr expr ^ "\n"
    | ReturnStmt { results } ->
      "return " ^ (results |> List.map of_expr |> String.concat " ")

  let of_import imp = Printf.sprintf "import \"%s\"\n" imp.path

  let of_declaration (declr : declaration) =
    match declr with
    | FunDeclaration { name; params; body; ret } ->
      Printf.sprintf
        "func %s(%s) %s {\n%s}\n"
        name
        (params |> List.map of_type_parameter |> String.concat ", ")
        (of_typ ret)
        (body |> List.map of_statement |> String.concat "")
    | GenDeclaration (ImportList { imports }) ->
      Printf.sprintf "import (%s)\n" (imports |> List.map of_import |> String.concat ", ")
    | GenDeclaration (TypeDeclaration { name; typ }) ->
      Printf.sprintf "type %s %s\n" name typ

  let of_file (file : file) =
    Printf.sprintf
      "package %s\n\n%s"
      file.name
      (file.declarations |> List.map of_declaration |> String.concat "")
end