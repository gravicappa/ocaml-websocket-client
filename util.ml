let init_random =
  let initialized = ref false in
  fun () ->
    if not !initialized then
      let () =
        Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna) in
      initialized := true

let random_string n =
  Mirage_crypto_rng.generate n

let random_bytes n = n |> random_string |> Bytes.unsafe_of_string

module Base64 = struct
  type t = Base64 of string

  let create str = Base64 str

  let of_string str = str |> Base64.encode_string |> create

  let as_string (Base64 str) = str
end

let sha1 str =
  let module Hash = Digestif.SHA1 in
  str
  |> Hash.digest_string
  |> Hash.to_raw_string
