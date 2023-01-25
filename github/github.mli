open! Core

type t
type auth_token

val refresh_auth : unit -> unit
val get_auth : unit -> auth_token
val get_current_repo : auth_token -> t
val set_secret : t -> key:string -> value:string -> bool
