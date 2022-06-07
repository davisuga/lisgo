module GoAST = struct
  open Expr

  let of_expression expr =
    match expr with
    | Int myInt -> Golang.AST.Int myInt
    | String myString -> Golang.AST.String myString
    | Variable ident -> Golang.AST.String ident
    | True -> Golang.AST.True
    | Float myFloat -> Golang.AST.Float myFloat
    | False -> Golang.AST.False
    | Abstraction _myAbstraction -> Golang.AST.String ""
    | Application _myApplication -> Golang.AST.String ""
    | List _myList -> Golang.AST.String ""
    (* | SExpr _mySExpr -> Golang.AST.String "" *)
    | Unit -> Golang.AST.String ""
  (* let of_application ({ func; arg } : Expr.application_expr) =  *)
  let rec of_typ my_typ =
    let open Golang.Types in
    match my_typ with
    | Typ.TBool -> Golang.Types.TBool
    | Typ.TInt -> Golang.Types.TInt
    | Typ.TStr -> Golang.Types.TStr
    | Typ.TUnit -> Golang.Types.TVoid
    | Typ.TFloat -> Golang.Types.TFloat32
    | Typ.TArrow arrow_typ ->
      Golang.Types.TFunction
        { args =
            Typ.unwrap_type_params arrow_typ.param_type
            |> List.mapi (fun index typ ->
                   { typ = of_typ typ; name = "arg" ^ string_of_int index });
          ret = of_typ arrow_typ.body_typ
        }
    | Typ.TList list_typ -> Golang.Types.TList (of_typ list_typ)
end