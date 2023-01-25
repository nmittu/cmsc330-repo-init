open! Core
open Ttweetnacl.Crypto

type key = Box.Public_key.t
type bytes = Bytes.t

let set b v i =
  for j = 0 to (Bytes.length v) - 1 do
    Bytes.set b (i+j) (Bytes.get v j)
  done

let nonce_gen pk1 pk2 =
  let bytes = Bytes.create (Box.Public_key.length * 2) ~init:0 in
  set bytes (Box.Public_key.to_bytes pk1) 0;
  set bytes (Box.Public_key.to_bytes pk2) (Box.Public_key.length);
  Hacl_star.Hacl.Blake2b_32.hash bytes Box.Nonce.length |> Box.Nonce.of_bytes


let seal m pk =
  let overhead_length = Box.Public_key.length + Box.cipher_text_overhead_length in
  let c = Bytes.create (overhead_length + Bytes.length m) ~init:0 in
  let epk, esk = Box.keypair () in
  set c (Box.Public_key.to_bytes epk) 0;
  let nonce = nonce_gen epk pk in
  let boxed = Box.box
    ~nonce
    ~receiver:pk
    ~sender:esk
    ~plain_text:m in
  set c boxed (Box.Public_key.length);
  c

let bytes_of_string s =
  Bytes.of_string s


let base64_of_bytes b =
  Base64.encode_exn (Bytes.to_string b)

let key_of_string s =
  Box.Public_key.of_bytes (bytes_of_string (Base64.decode_exn s))
