let run _props =
  let open Cmdliner in
  let check_term =
    let open Term.Syntax in
    let+ () = Term.const () in
    Error "not implemented"
  in
  let main_cmd =
    let check_cmd = Cmd.v (Cmd.info "check") check_term in

    Cmd.group
      (Cmd.info (Filename.basename Sys.executable_name))
      ~default:check_term [ check_cmd ]
  in
  main_cmd |> Cmdliner.Cmd.eval_result
