open! Core
open Lwt.Syntax
open Cohttp_lwt_unix

type auth_token = string
type t = {
  repo: string;
  auth_token: auth_token;
}


let client_id = "bd1606f7ca00fe537411"

let rec poll_token device_code interval =
  let* () = Lwt_unix.sleep (Int.to_float interval) in
  let poll_url = "https://github.com/login/oauth/access_token" in
  let* (resp, body) = Client.post
    ~headers:(Cohttp.Header.of_list [("Accept", "application/json")])
    ~body:(Cohttp_lwt.Body.of_string
      ("client_id=" ^ client_id ^ "&device_code=" ^ device_code ^ "&grant_type=urn:ietf:params:oauth:grant-type:device_code"))
    (Uri.of_string poll_url) in
  let* body_string = Cohttp_lwt.Body.to_string body in
  let response_code = resp |> Response.status |> Cohttp.Code.code_of_status in
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_string body_string in
  if response_code <> 200 || json |> member "error" |> to_string_option |> Option.is_some then
    poll_token device_code interval
  else
    json |> member "access_token" |> to_string |> Lwt.return


let get_new_auth () =
  let auth_lwt =
    let scope = "repo" in
    let device_code_url = "https://github.com/login/device/code" in
    let* (_, body) = Client.post
      ~headers:(Cohttp.Header.of_list [("Accept", "application/json")])
      ~body:(Cohttp_lwt.Body.of_string
        ("client_id=" ^ client_id ^ "&scope=" ^ scope))
      (Uri.of_string device_code_url) in
    let* body_str = Cohttp_lwt.Body.to_string body in
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string body_str in
    let device_code = json |> member "device_code" |> to_string in
    let user_code = json |> member "user_code" |> to_string in
    let verification_url = json |> member "verification_uri" |> to_string in
    let interval = json |> member "interval" |> to_int in
    let* _ = Lwt_io.printf "open %s in your browser and enter the folowing code:\n %s" verification_url user_code in
    poll_token device_code interval in
  Lwt_main.run auth_lwt

let config_file =
  let homedir = Sys.getenv_exn "HOME" in
  Filename.concat homedir ".CMSC330CONFIG";;

let save_token tok =
  Out_channel.write_all config_file ~data:tok

let read_tok () =
  if Sys_unix.file_exists_exn config_file then
    Some (In_channel.read_all config_file)
  else None

let get_auth () =
  match read_tok () with
  | Some tok -> tok
  | None -> 
    let auth = get_new_auth () in
    save_token auth;
    auth

let set_secret _ ~key:_ ~value:_ = failwith "unimplemented"

let get_current_repo _ = 
  let command = "git config --get remote.origin.url" in
  let remote_origin = Core_unix.open_process_in command |> In_channel.input_all in
  let origin_regex = Str.regexp "git@github\\.com:(.+)\\.git|https:\\/\\/github\\.com\\/(.+)\\.git" in
  assert (Str.string_match origin_regex remote_origin 0);
  try
    Str.matched_group 1 remote_origin
  with Not_found -> Str.matched_group 2 remote_origin
