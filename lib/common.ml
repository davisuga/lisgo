module Option = struct
  include Option

  let expect message opt =
    match opt with
    | Some x -> x
    | None -> failwith message
end