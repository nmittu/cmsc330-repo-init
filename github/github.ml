open! Core
open Lwt.Syntax
open Cohttp_lwt_unix

type auth_token = string
type t = {
  owner: string;
  repo: string;
  auth_token: auth_token;
}


let client_id = Sys.getenv_exn "CMSC330CLIENTID"

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
    let* _ = Lwt_io.printf "Open %s in your browser and enter the folowing code:\n %s\n" verification_url user_code in
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

let refresh_auth () =
  let auth = get_new_auth () in
  save_token auth

let get_auth () =
  match read_tok () with
  | Some tok -> tok
  | None -> 
    let auth = get_new_auth () in
    save_token auth;
    auth

let get_current_repo auth_token = 
  let command = "git config --get remote.origin.url" in
  let remote_origin = Core_unix.open_process_in command |> In_channel.input_all in
  let origin_regex = Str.regexp "git@github\\.com:\\(.+\\)\\.git\\|https:\\/\\/github\\.com\\/\\(.+\\)\\.git" in
  assert (Str.string_match origin_regex remote_origin 0);
  let repo = try
    Str.matched_group 1 remote_origin
  with _ -> Str.matched_group 2 remote_origin in
  match Str.bounded_split (Str.regexp "/") repo 2 with
  | [owner;repo] -> {owner; repo; auth_token}
  | _ -> failwith "unable to get repo name"

let get_repo_key t =
  let url = Printf.sprintf "https://api.github.com/repos/%s/%s/actions/secrets/public-key" t.owner t.repo in
  let* (_, body) = Client.get
    ~headers:(Cohttp.Header.of_list
      [ "Accept", "application/vnd.github+json"
      ; "Authorization", "Bearer " ^ t.auth_token
      ; "X-GitHub-Api-Version", "2022-11-28"])
    (Uri.of_string url) in
  let* body_string = Cohttp_lwt.Body.to_string body in
  let json = Yojson.Basic.from_string body_string in
  let open Yojson.Basic.Util in
  (json |> member "key" |> to_string,
   json |> member "key_id" |> to_string) |> Lwt.return

let set_secret t ~key ~value =
  let lwt =
    let* pk, key_id = get_repo_key t in
    let c = Sealed_box.seal 
      (Sealed_box.bytes_of_string value) 
      (Sealed_box.key_of_string pk) in
    let encoded = Sealed_box.base64_of_bytes c in
    let url = Printf.sprintf "https://api.github.com/repos/%s/%s/actions/secrets/%s" t.owner t.repo key in
    let* (resp, _) = Client.put
      ~headers:(Cohttp.Header.of_list
        [ "Accept", "application/vnd.github+json"
        ; "Authorization", "Bearer " ^ t.auth_token
        ; "X-Github-Api-Version", "2022-11-28"])
      ~body:(Cohttp_lwt.Body.of_string
        (Yojson.Basic.to_string 
          (`Assoc 
            [ "encrypted_value", `String encoded
            ; "key_id", `String key_id])))
      (Uri.of_string url) in
      let response_code = resp |> Response.status |> Cohttp.Code.code_of_status in
      Lwt.return (response_code = 201 || response_code = 204) in
    Lwt_main.run lwt

