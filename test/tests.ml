open Lisgo.Golang

let write_file file message =
  (* Write message to file *)
  let oc = open_out file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s" message;
  (* write something *)
  close_out oc

let expected_typ_print_pairs =
  let open Types in
  [ TInt8, "int8";
    TInt16, "int16";
    TInt32, "int32";
    TInt64, "int64";
    TUint8, "uint8";
    TUint16, "uint16";
    TUint32, "uint32";
    TUint64, "uint64";
    TUintptr, "uintptr";
    TStr, "string";
    TFunction { args = []; ret = TInt32 }, "func() int32";
    ( TFunction { args = [ { name = "input"; typ = TInt32 } ]; ret = TInt32 },
      "func(input int32) int32" );
    ( TFunction
        { args = [ { name = "x"; typ = TInt32 }; { name = "y"; typ = TInt32 } ];
          ret = TInt32
        },
      "func(x int32, y int32) int32" );
    TStruct { name = "User" }, "User";
    TPointer (TStruct { name = "User" }), "*User"
  ]

let expected_declaration_print_pairs =
  [ ( FunDeclaration { name = "main"; body = []; params = []; ret = TVoid },
      "func main()  {\n}\n" );
    ( FunDeclaration
        { name = "main";
          body = [ DeclStmt { name = "myvar"; value = Int 1 } ];
          params = [];
          ret = TInt32
        },
      "func main() int32 {\nvar myvar = 1\n}\n" )
  ]

[@@@ocamlformat "disable=true"]

let expected_file_print_pairs =
[ empty_file, "package main\n\nfunc main()  {\n}\n";
  ( hello_word,
    "package main\n\n\
     import (\n\
    \ \"fmt\"\n\
     )\n\
     func main()  {\n\
     fmt.Println(\"Hello, World!\")\n\
     }\n" );
     
  ( go_fibbonacci,
    "package main\n\nimport (\n \"fmt\"\n)\nfunc fib(n int) int {\nif n <= 1 {\nreturn n\n}\nreturn fib(n - 1) + fib(n - 2)}\nfunc main()  {\nvar x = 0\nfib(x)\n}\n" ) 
]

let test_codegen_result generator_fn (ast_node, expected_res) =
  Alcotest.(check string) "Same string" expected_res (generator_fn ast_node)

let test_type_print = test_codegen_result CodeGen.of_typ
let test_declaration_print = test_codegen_result CodeGen.of_declaration
let test_file_print = test_codegen_result CodeGen.of_file

let test_codegen_results
    (tester_fn : 'a * string -> unit)
    (expectation_list : ('a * string) list)
  =
  expectation_list
  |> List.mapi (fun i (a, b) ->
         write_file ("./sas" ^ string_of_int (i + 1) ^ ".go") b;
         a, b)
  |> List.map tester_fn
  |> ignore

let test_declarations_print () =
  test_codegen_results test_declaration_print expected_declaration_print_pairs

let test_file_prints () = test_codegen_results test_file_print expected_file_print_pairs
let test_types_print () = expected_typ_print_pairs |> List.map test_type_print |> ignore

let expected_asts =   
  let open Lisgo.Expr in
  let if_fib_ast =  (SExpr
    [(Variable "if");
      (SExpr [(Variable "<"); (Variable "="); (Variable "n"); (Int 1)]);
      (Variable "n");
      (SExpr
         [(Variable "+");
           (SExpr
              [(Variable "fib");
                (SExpr [(Variable "-"); (Variable "n"); (Int 1)])]);
           (SExpr
              [(Variable "fib");
                (SExpr [(Variable "-"); (Variable "n"); (Int 2)])])
           ])
      ]) in
  [ "1", Int 1;
"\"1\"", String "1";
"+", Variable "+";
"1.1", Float 1.1;
"true",True ;
"false", False;
"(+ 1)", (SExpr [(Variable "+"); (Int 1)]);
{|(if (<= n 1) 
  (n)
  (+ (fib (- n 1)) (fib (- n 2))))|},if_fib_ast;
"(fn n:int (+ n n))", Abstraction {param_type = TInt; param = "n";
body = (SExpr [(Variable "+"); (Variable "n"); (Variable "n")])};
"(def fib (fun n:int (if (<= n 1) (n)  (+ (fib (- n 1)) (fib (- n 2))))))", (SExpr [])
]

let test_info () =  
  
  [ "1"; "\"1\""; "+"; "1.1"; "true"; "false"; "(+ 1)" ]
|> List.map Lisgo.Lib.print_prog_info

(* Run it *)
let () =
  let open Alcotest in
  run
    "Codegen"
    [ "type codegen", [ test_case "Type printing" `Quick test_types_print ];
      ( "declaration codegen",
        [ test_case "Declaration printing" `Quick test_declarations_print ] );
      "file codegen ", [ test_case "File printing" `Quick test_file_prints ]
    ];
  run "Parsing" 
  [
    "some expressions", [
      test_case "Parsing of simple expressions" `Quick Parser.parse_expression "1 + 2"
    
    ]
  ]
