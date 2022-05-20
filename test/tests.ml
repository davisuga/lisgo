open Lisgo.Golang

let expected_typ_print_pairs =
  let open Types in
  [ TInt8, "int8"
  ; TInt16, "int16"
  ; TInt32, "int32"
  ; TInt64, "int64"
  ; TUint8, "uint8"
  ; TUint16, "uint16"
  ; TUint32, "uint32"
  ; TUint64, "uint64"
  ; TUintptr, "uintptr"
  ; TStr, "string"
  ; TFunction { args = []; ret = TInt32 }, "func() int32"
  ; ( TFunction { args = [ { name = "input"; typ = TInt32 } ]; ret = TInt32 }
    , "func(input int32) int32" )
  ; TStruct { name = "User" }, "User"
  ; TPointer (TStruct { name = "User" }), "*User"
  ]

let test_type_print (typ, result) =
  Alcotest.(check string) "same string" result (CodeGen.of_typ typ)

let test_types_print () = expected_typ_print_pairs |> List.map test_type_print |> ignore

(* Run it *)
let () =
  let open Alcotest in
  run "Codegen" [ "type-print", [ test_case "Type printing" `Quick test_types_print ] ]
