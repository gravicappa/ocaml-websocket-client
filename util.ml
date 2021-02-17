let init_random =
  let initialized = ref false in
  fun () ->
    if !initialized then
      let () = Nocrypto_entropy_unix.initialize () in
      initialized := true

let random_bytes n =
  Nocrypto.Rng.generate n |> Cstruct.to_bytes

let random_string n =
  Nocrypto.Rng.generate n |> Cstruct.to_string

module Base64 = struct
  type t = Base64 of string

  let create str = Base64 str

  let of_string str =
    Cstruct.of_string str
    |> Nocrypto.Base64.encode
    |> Cstruct.to_string
    |> create

  let as_string (Base64 str) = str
end

let sha1 str =
  Cstruct.of_string str
  |> Nocrypto.Hash.SHA1.digest
  |> Cstruct.to_string
