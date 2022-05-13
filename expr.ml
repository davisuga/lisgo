open Typ

type expr =
  | Int of int
  | String of string
  | Variable of string
  | True
  | Float of float
  | False
  | Abstraction of { param_type : typ; param : string; body : expr }
  | Application of { func : expr; arg : expr }
  | List of expr list
  | SExpr of expr list
  | Unit
[@@deriving show { with_path = false }]
