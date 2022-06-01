let format = Printf.sprintf

type bin_operation = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq
type import = { name : string option; path : string }

module Types = struct
  type t =
    | TInt
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

module AST = struct
  open Types

  type expr =
    | Int of int
    | String of string
    | Ident of string
    | Application of { func : expr; args : expr list }
    | BinExpr of { op : bin_operation; left : expr; right : expr }
    | True
    | False
    | Float of float

  type stmt =
    | DeclStmt of { name : string; value : expr }
    | AssignStmt of { left : string; right : expr }
    | ExprStmt of expr
    | ReturnStmt of { results : expr list }
    | IfStmt of { cond : expr; body : stmt list; else_block : stmt list option }

  type gen_declaration =
    | ImportList of { imports : import list }
    | TypeDeclaration of { name : string; typ : string }

  type fun_declaration =
    { body : stmt list; name : string; params : func_type_parameter list; ret : Types.t }

  type declaration =
    | GenDeclaration of gen_declaration
    | FunDeclaration of fun_declaration

  type file = { name : string; declarations : declaration list }

  let empty_file =
    { name = "main";
      declarations =
        [ FunDeclaration { name = "main"; body = []; params = []; ret = TVoid } ]
    }

  let hello_word =
    { name = "main";
      declarations =
        [ GenDeclaration (ImportList { imports = [ { name = None; path = "fmt" } ] });
          FunDeclaration
            { name = "main";
              body =
                [ ExprStmt
                    (Application
                       { func = Ident "fmt.Println"; args = [ String "Hello, World!" ] })
                ];
              params = [];
              ret = TVoid
            }
        ]
    }

  let go_fibbonacci =
    { name = "main";
      declarations =
        [ GenDeclaration (ImportList { imports = [ { name = None; path = "fmt" } ] });
          FunDeclaration
            { name = "fib";
              params = [ { name = "n"; typ = TInt } ];
              ret = TInt;
              body =
                [ IfStmt
                    { cond = BinExpr { left = Ident "n"; op = Leq; right = Int 1 };
                      body = [ ReturnStmt { results = [ Ident "n" ] } ];
                      else_block = None
                    };
                  ReturnStmt
                    { results =
                        [ BinExpr
                            { left =
                                Application
                                  { func = Ident "fib";
                                    args =
                                      [ BinExpr
                                          { left = Ident "n"; op = Sub; right = Int 1 }
                                      ]
                                  };
                              op = Add;
                              right =
                                Application
                                  { func = Ident "fib";
                                    args =
                                      [ BinExpr
                                          { left = Ident "n"; op = Sub; right = Int 2 }
                                      ]
                                  }
                            }
                        ]
                    }
                ]
            };
          FunDeclaration
            { name = "main";
              body =
                [ DeclStmt { name = "x"; value = Int 0 };
                  ExprStmt (Application { func = Ident "fib"; args = [ Ident "x" ] })
                ];
              params = [];
              ret = TVoid
            }
        ]
    }
end

module CodeGen = struct
  open AST

  let rec of_expr expr =
    match expr with
    | Int i -> string_of_int i
    | String s -> "\"" ^ s ^ "\""
    | True -> "true"
    | False -> "false"
    | Float f -> string_of_float f
    | Ident s -> s
    | Application { func; args } ->
      format "%s(%s)" (of_expr func) (List.map of_expr args |> String.concat " ")
    | BinExpr { op; left; right } ->
      format
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

  open Types

  let rec of_typ typ =
    match typ with
    | TInt -> "int"
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
    | TList t -> format "[]%s" (of_typ t)
    | TStruct { name } -> name
    | TPointer t -> "*" ^ of_typ t
    | TFunction { args; ret } ->
      format
        {|func(%s) %s|}
        (List.map of_type_parameter args |> String.concat ", ")
        (of_typ ret)

  and of_type_parameter (param : func_type_parameter) =
    format "%s %s" param.name (of_typ param.typ)

  let rec of_statement stmt =
    match stmt with
    | DeclStmt { name; value } -> format "var %s = %s\n" name (of_expr value)
    | AssignStmt { left; right } -> left ^ " = " ^ of_expr right ^ "\n"
    | ExprStmt expr -> of_expr expr ^ "\n"
    | ReturnStmt { results } ->
      "return " ^ (results |> List.map of_expr |> String.concat " ")
    | IfStmt { cond; body; else_block } ->
      format
        "if %s {\n%s\n}%s\n"
        (of_expr cond)
        (body |> List.map of_statement |> String.concat "")
        (match else_block with
        | Some else_block ->
          format
            "\nelse {\n%s\n}"
            (else_block |> List.map of_statement |> String.concat "")
        | None -> "")

  let of_import imp = format " \"%s\"\n" imp.path

  let of_declaration (declr : declaration) =
    match declr with
    | FunDeclaration { name; params; body; ret } ->
      format
        "func %s(%s) %s {\n%s}\n"
        name
        (params |> List.map of_type_parameter |> String.concat ", ")
        (of_typ ret)
        (body |> List.map of_statement |> String.concat "")
    | GenDeclaration (ImportList { imports }) ->
      format "import (\n%s)\n" (imports |> List.map of_import |> String.concat ", ")
    | GenDeclaration (TypeDeclaration { name; typ }) -> format "type %s %s\n" name typ

  let of_file (file : file) =
    format
      "package %s\n\n%s"
      file.name
      (file.declarations |> List.map of_declaration |> String.concat "")
end
