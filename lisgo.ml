let () = print_endline "Hey"

module Ctx = Map.Make (String)

type expr =
  | Variable of string
  | Abstraction of { param : string; body : expr }
  | Application of { func : expr; arg : expr }

type value = Closure of { context : value Ctx.t; param : string; body : expr }

let rec interpret current_context expr =
  match expr with
  | Variable name -> Ctx.find name current_context
  | Abstraction { param; body } ->
      Closure { context = current_context; param; body }
  | Application { func; arg } ->
      let argument = interpret current_context arg in
      let (Closure { context; param; body }) = interpret current_context func in
      interpret (Ctx.add param argument context) body
