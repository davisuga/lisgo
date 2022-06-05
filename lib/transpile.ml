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
end