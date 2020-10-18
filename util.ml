let random_bytes n =
  let rng = if Sys.win32 then
              Cryptokit.Random.system_rng ()
            else
              Cryptokit.Random.device_rng "/dev/urandom" in
  let bytes = Bytes.create n in
  rng#random_bytes bytes 0 n;
  bytes

let random_string n = random_bytes n |> Bytes.to_string

module Base64 = struct
  type t = Base64 of string

  let of_string str =
    let b = Cryptokit.Base64.encode_compact () in
    b#put_string str;
    b#finish;
    Base64 b#get_string

  let create str = Base64 str

  let as_string (Base64 str) = str
end

let sha1 str =
  let h = Cryptokit.Hash.sha1 () in
  h#add_string str;
  h#result


