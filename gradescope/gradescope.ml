open! Core

let gradescope_config_file =
  let homedir = Sys.getenv_exn "HOME" in
  Filename.concat homedir ".gradescope-submit/signed_token"
;;

let get_new_token () =
  match Core_unix.fork () with
  | `In_the_child -> Core_unix.exec ~prog:"gradescope-submit" ~argv:[] () |> never_returns
  | `In_the_parent pid -> Core_unix.waitpid_exn pid
;;

let get_signed_token () =
  if not (Sys_unix.file_exists_exn gradescope_config_file) then get_new_token ();
  In_channel.read_all gradescope_config_file
;;
