type typ =
  | TArrow of { param_type : typ; body_typ : typ }
  | TInt
  | TBool
  | TFloat
  | TUnit
  | TStr
  | TList of typ
[@@deriving show { with_path = false }]

(* a -> (b -> (c -> d)) to a; b; c; d *)
let unwrap_type_params type_params =
  let rec loop acc type_params' =
    match type_params' with
    | TArrow { param_type; body_typ } ->
      let new_acc = List.cons param_type acc in
      loop new_acc body_typ
    | a -> List.cons a acc
  in
  loop [] type_params

let wrap_type_params params =
  List.fold_right
    (fun type_a type_b -> TArrow { param_type = type_a; body_typ = type_b })
    params
    (List.hd params)

let rec equal a b =
  match a, b with
  | a, b when a == b -> true
  | TList a, TList b -> equal a b
  | TInt, TInt -> true
  | ( TArrow { param_type = param_a; body_typ = body_a },
      TArrow { param_type = param_b; body_typ = body_b } ) ->
    List.equal equal (unwrap_type_params param_a) (unwrap_type_params param_b)
    && equal body_a body_b
  | _ -> false
