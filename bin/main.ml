let _ =
  [ "1"; "\"1\""; "+"; "1.1"; "true"; "false"; "(+ 1)" ]
  |> List.map Lisgo.Lib.print_prog_info
