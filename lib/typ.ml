type typ =
  | TArrow of { param_type : typ; body_typ : typ }
  | TInt
  | TBool
  | TFloat
  | TUnit
  | TStr
  | TList of typ
[@@deriving show { with_path = false }]

let rec equal a b =
  match a, b with
  | a, b when a == b -> true
  | TList a, TList b -> equal a b
  | TInt, TInt -> true
  | ( TArrow { param_type = param_a; body_typ = body_a },
      TArrow { param_type = param_b; body_typ = body_b } ) ->
    equal param_a param_b && equal body_a body_b
  | _ -> false
