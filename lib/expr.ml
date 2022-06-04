open Typ

type expr =
  | Int of int
  | String of string
  | Variable of string
  | True
  | Float of float
  | False
  | Abstraction of abstraction_expr
  | Application of application_expr
  | List of expr list
  (* | SExpr of expr list *)
  | Unit
[@@deriving show { with_path = false }]

and abstraction_expr = { param_type : typ; param : string; body : expr }
and application_expr = { func : expr; arg : expr }

let parse_sexpr expr_list =
  match expr_list with
  | [] -> Unit
  | [ x ] -> x
  | [ func; args ] ->
    (match func with
    | Application _ | Abstraction _ | Variable _ -> Application { func; arg = args }
    | _ ->
      failwith
        (Format.sprintf "You can't apply a %s to %s" (show_expr func) (show_expr args)))
  | func :: args -> Application { func; arg = List args }

let _ =
  Application
    { func = Variable "def";
      arg =
        List
          [ Variable "fib";
            Abstraction
              { param_type = TInt;
                param = "n";
                body =
                  Application
                    { func = Variable "if";
                      arg =
                        List
                          [ Application
                              { func = Variable "<";
                                arg = List [ Variable "="; Variable "n"; Int 1 ]
                              };
                            Variable "n";
                            Application
                              { func = Variable "+";
                                arg =
                                  List
                                    [ Application
                                        { func = Variable "fib";
                                          arg =
                                            Application
                                              { func = Variable "-";
                                                arg = List [ Variable "n"; Int 1 ]
                                              }
                                        };
                                      Application
                                        { func = Variable "fib";
                                          arg =
                                            Application
                                              { func = Variable "-";
                                                arg = List [ Variable "n"; Int 2 ]
                                              }
                                        }
                                    ]
                              }
                          ]
                    }
              }
          ]
    }


