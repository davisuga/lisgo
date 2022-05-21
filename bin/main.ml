let f = Printf.sprintf

let if_code = {|(if (<= n 1) 
  (n)
  (+ (fib (- n 1)) (fib (- n 2))))|}

let sum_fn_code = "(fn n:int (+ n n))"
let sum_app_code = f "(%s 5)" sum_fn_code

let _ =
  [ "1";
    "\"1\"";
    "+";
    "1.1";
    "true";
    "false";
    "(+ 1)";
    if_code;
    sum_app_code;
    sum_fn_code;
    f "(def (sum) (fun n:int %s)))" sum_fn_code
  ]
  |> List.map Lisgo.Lib.print_prog_info
