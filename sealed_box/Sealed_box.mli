open! Core

type key
type bytes

val seal : bytes -> key -> bytes
val bytes_of_string : string -> bytes
val key_of_string : string -> key
val base64_of_bytes : bytes -> string
