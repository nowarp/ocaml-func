open! IStd
open Cmdliner

let run' filename = Func.parse filename
let run filename = match run' filename with
  | Ok _ -> 0
  | Error e -> eprintf "Error: %s\n" e; 1

let is_func_file path =
  let has_extension ext = String.is_suffix ~suffix:ext path in
  has_extension ".fc" || has_extension ".func"

let rec process_dir ?(parse_only=false) dirname =
  let handle_entry entry =
    let full_path = Filename.concat dirname entry in
    match Core_unix.stat full_path with
    | { Core_unix.st_kind = Core_unix.S_DIR; _ }
      when not @@ String.equal entry "." &&
           not @@ String.equal entry ".." ->
        process_dir ~parse_only full_path |> fun x -> x
    | { Core_unix.st_kind = Core_unix.S_REG; _ } ->
        if is_func_file full_path then
        (match run' full_path with
         | Ok _ -> 0
         | Error e -> eprintf "Error in %s: %s\n" full_path e; 1)
        else (
          eprintf "%s is not a FunC file\n" full_path; 1
        )
    | _ -> 0 in
  try
    let dir = Core_unix.opendir dirname in
    let rec loop acc =
      match Core_unix.readdir_opt dir with
      | Some entry -> loop (max acc (handle_entry entry))
      | None -> Core_unix.closedir dir; acc in
    loop 0
  with Core_unix.Unix_error(e, _, _) ->
    eprintf "Directory error: %s\n" (Core_unix.Error.message e);
    1

let parse_only =
  let doc = "Stop after parsing stage" in
  Arg.(value & flag & info ["parse-only"] ~doc)

let file_or_dir =
  let doc = "Input file or directory" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE_OR_DIR" ~doc)

let cmd =
  let doc = "ocaml-func driver" in
  let info = Cmd.info "ocaml-func-driver" ~doc in
  Cmd.v info
    Term.(ret (const (fun parse_only path ->
      `Ok (match Core_unix.stat path with
      | { st_kind = S_DIR; _ } -> process_dir ~parse_only path
      | { st_kind = S_REG; _ } -> run path
      | _ ->
          eprintf "Error: %s is neither a file nor a directory\n" path;
          1
      | exception Core_unix.Unix_error(e, _, _) ->
          eprintf "Error: %s\n" (Core_unix.Error.message e);
          1))
    $ parse_only $ file_or_dir))

let () =
  (match Cmd.eval_value cmd with
   | Ok(_) -> 0
   | _ -> 1)
  |> exit
