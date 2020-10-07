type op =
  | Continuation
  | Text
  | Binary
  | Reserved of int
  | Close
  | Ping
  | Pong
[@@deriving show]

type mask = Masked | Unmasked
[@@deriving show]

let op_of_int = function
  | 0 -> Continuation
  | 1 -> Text
  | 2 -> Binary
  | 8 -> Close
  | 9 -> Ping
  | 10 -> Pong
  | n -> Reserved n

let int_of_op = function
  | Continuation -> 0
  | Text -> 1
  | Binary -> 2
  | Reserved n -> n
  | Close -> 8
  | Ping -> 9
  | Pong -> 10

type t = {
  finish: bool;
  op: op;
  masked: mask;
  payload_length: int;
  mask: bytes;
  payload: bytes;
}
[@@deriving show]

type frame = t
[@@deriving show]

module Response = struct
  type resp =
    | Frame of frame
    | End
    | Error of string
end

let create_mask = function
  | Masked -> Util.random_bytes 4
  | Unmasked -> Bytes.empty

let read_length input buf = function
  | 127 ->
      let%lwt () = Lwt_io.read_into_exactly input buf 0 8 in
      let a0 = Bytes.get_uint16_be buf 0 in
      let a1 = Bytes.get_uint16_be buf 2 in
      let a2 = Bytes.get_uint16_be buf 4 in
      let a3 = Bytes.get_uint16_be buf 6 in
      if a0 > 0x7ffff then
        Lwt.return_error "Too large buffer"
      else
        Lwt.return_ok ((a0 lsl 48) lor (a1 lsl 32) lor (a2 lsl 16) lor a3)
  | 126 ->
      let%lwt () = Lwt_io.read_into_exactly input buf 0 2 in
      let a = Bytes.get_uint16_be buf 0 in
      if a > 0x7ffff then
        Lwt.return_error "Too large buffer"
      else
        Lwt.return_ok a
  | n -> Lwt.return_ok n

let read_mask input = function
  | Unmasked -> Lwt.return Bytes.empty
  | Masked ->
      let bytes = Bytes.create 4 in
      let%lwt () = Lwt_io.read_into_exactly input bytes 0 4 in
      Lwt.return bytes

let mask_payload ~mask ~src ~dst = function
  | Unmasked -> ()
  | Masked ->
      let n = Bytes.length src in
      let rec loop i j =
        if j < n then
          let b = Bytes.get_uint8 src j in
          Bytes.set_uint8 dst j (b lxor (Bytes.get_uint8 mask i));
          loop ((i + 1) land 3) (j + 1) in
      loop 0 0

let read input =
  try%lwt
    let buf = Bytes.create 8 in
    let%lwt () = Lwt_io.read_into_exactly input buf 0 2 in
    let h1 = Bytes.get_uint16_be buf 0 in
    let finish = h1 land 0x8000 <> 0 in
    let op = op_of_int ((h1 lsr 8) land 15) in
    let masked = match h1 land 0x80 with
                 | 0 -> Unmasked
                 | _ -> Masked in
    match%lwt read_length input buf (h1 land 0x7f) with
    | Error err -> Lwt.return (Response.Error err)
    | Ok payload_length ->
        let%lwt mask = read_mask input masked in
        let payload = Bytes.create payload_length in
        let%lwt () = Lwt_io.read_into_exactly input payload 0 payload_length in
        mask_payload ~mask ~src: payload ~dst: payload masked;
        Response.Frame { finish; op; masked; payload_length; payload; mask }
        |> Lwt.return
  with
  | End_of_file -> Lwt.return Response.End

let write output { finish; op; masked; payload_length; mask; payload } =
  let finish_bit finish w =
    match finish with
    | true -> w lor 0x8000
    | false -> w in

  let op_bits op w =
    let opcode = int_of_op op in
    w lor (opcode lsl 8) in

  let masked_bit masked w =
    match masked with
    | Masked -> w lor 0x80
    | Unmasked -> w in

  let len_byte payload_length w =
    match payload_length with
    | n when n > 65535 -> w lor 127
    | n when n > 127 -> w lor 126
    | n -> w lor n in

  let write_masked_payload mask payload output =
    let masked = Bytes.create (Bytes.length payload) in
    mask_payload ~mask ~src: payload ~dst: masked Masked;
    Lwt_io.write_from_exactly output masked 0 (Bytes.length payload) in

  let%lwt () = 0
               |> (finish_bit finish)
               |> (op_bits op)
               |> (masked_bit masked)
               |> (len_byte payload_length) 
               |> Lwt_io.BE.write_int16 output in

  let%lwt () = match payload_length with
               | n when n > 65535 ->
                   Int64.of_int n
                   |> Lwt_io.BE.write_int64 output
               | n when n > 127 -> Lwt_io.BE.write_int16 output n
               | _ -> Lwt.return_unit in

  let%lwt () = match masked with
               | Masked ->
                   let%lwt () = Lwt_io.write_from_exactly output mask 0 4 in
                   write_masked_payload mask payload output
               | Unmasked ->
                   Lwt_io.write_from_exactly output payload 0 payload_length in
  Lwt_io.flush output

let create ?(finish = true) ?(op = Text) ?(masked = Masked) payload =
  let payload_length = Bytes.length payload in
  let mask = create_mask masked in
  { finish; op; masked; mask; payload_length; payload }

module Control = struct
  let create ?(masked = Masked) ?(payload = Bytes.empty) op =
    create ~op ~masked payload

  let ping ?(payload = Bytes.empty) () = create ~payload Ping

  let pong ?(payload = Bytes.empty) () = create ~payload Pong

  let pong_of_ping { payload; _ } =
    create ~payload Pong
end
