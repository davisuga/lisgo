%{ open Typ
   open Expr %}
%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token TRUE
%token FALSE
// %token NULL
// %token LEFT_BRACE
// %token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_PAREN
%token RIGHT_PAREN
%token COLON
%token COMMA
%token EOF
// %token DOT
// %token <string> DEFN
%token FN
%start <Expr.expr option> expr_opt
%start <Typ.typ option> typ_opt
%token ARROW

%%

let typ_opt :=
  | EOF; { None }
  | t = typ; EOF; { Some t }

let sub_typ ==
  | i = IDENT;
    { match i with 
      |"int" -> TInt 
      |"float" -> TFloat 
      |"string" -> TStr 
      |"bool" -> TBool 
      | _ -> failwith "invalid type" 
     }
  | LEFT_PAREN; t = typ; RIGHT_PAREN; { t }

let typ :=
  | sub_typ
  | t1 = sub_typ; ARROW; t2 = typ;
    { TArrow { param_type = t1; body_typ = t2 } }

let expr_opt :=
  | EOF; { None }
  | e = expr; EOF; { Some e }

let list_fields ==
    vl = separated_list(COMMA, value); { vl } 

let value ==
  | i = INT; { Int i }
  | s = STRING; { String s }
  | i = IDENT; { Variable i }
  | f = FLOAT; { Float f }
  | TRUE; { True }
  | FALSE; { False }
  | FN; param = IDENT; COLON; param_type = typ; body = expr;
    { Abstraction { param; param_type ; body } }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK; { List vl  }

  
let expr :=
  | value
  | LEFT_PAREN; e = expr; RIGHT_PAREN; { e }
  | LEFT_PAREN; elist = exprs; RIGHT_PAREN; { SExpr elist }
  

let exprs := list(expr)

// (fn [parameter:int] (println ( + 1 2)))
// let abstraction ==
//   | FN; param = IDENT; COLON; param_type = typ; body = expr;
//     { Abstraction { param; param_type ; body } }

let sub_expr :=
  | value
  | LEFT_PAREN; e = expr; RIGHT_PAREN; { e }

let application :=
  | sub_expr
  | LEFT_PAREN; e1 = application; e2 = sub_expr;
    { Application { func = e1; arg = e2 } }

