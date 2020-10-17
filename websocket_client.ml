module Settings = struct
  type t = {
    tls: X509_lwt.authenticator option;
    ping_interval_s: float;
  }

  let default = { tls = None; ping_interval_s = 30. }

  let with_tls tls t = { t with tls = Some tls }

  let with_ping_interval_s seconds t = { t with ping_interval_s = seconds }
end

module Response = struct
  type t =
    | Text of string
    | Binary of string
    | Error of string
    | End

  let text str = Text str
  let binary str = Binary str
end

module Request = struct
  type t =
    | Frame of Frame.t
    | End

  let frame frame = Frame frame
end

type channel = {
  mqueue: Request.t Mqueue.t;
  input: Lwt_io.input_channel;
  output: Lwt_io.output_channel;
}

type t = channel

let handshake_suffix = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let is_key_ok (Util.Base64.Base64 key) resp_key =
  let e = (key ^ handshake_suffix)
          |> Util.sha1
          |> Util.Base64.of_string
          |> Util.Base64.as_string in
  e = resp_key

let uri_host uri = Uri.host_with_default ~default: "127.0.0.1" uri

let uri_port uri = Uri.port uri |> Option.value ~default: 443

let make_request_headers uri (Util.Base64.Base64 key) =
  let path = match Uri.path uri with
             | "" -> "/"
             | path -> path in
  Printf.sprintf "GET %s HTTP/1.1\r\n\
                  Host: %s:%d\r\n\
                  Upgrade: websocket\r\n\
                  Connection: Upgrade\r\n\
                  Sec-WebSocket-Key: %s\r\n\
                  Sec-WebSocket-Version: 13\r\n\
                  \r\n"
                 path (uri_host uri) (uri_port uri) key

let parse_response_header_field str =
  match String.index_opt str ':' with
  | Some pos ->
      let key = String.sub str 0 pos
                |> String.lowercase_ascii in
      let value = String.sub str (pos + 2) (String.length str - pos - 2)
                  |> String.lowercase_ascii in
      key, String.trim value
  | None -> str, ""

let parse_response input key =
  let error_for_field name value =
    Printf.sprintf "Unexpected value for %s: '%s'" name value
    |> Lwt.return_error in

  let rec loop () =
    let%lwt line = Lwt_io.read_line input in
    match parse_response_header_field line with
    | "", "" -> Lwt.return_ok ()
    | "upgrade", "websocket" -> loop ()
    | "upgrade", value -> error_for_field "Upgrade" value
    | "connection", "upgrade" -> loop ()
    | "connection", value -> error_for_field "Connection" value
    | "Sec-WebSocket-Accept", resp_key when is_key_ok key resp_key ->
        loop ()
    | "Sec-WebSocket-Accept", value -> error_for_field "Sec-WebSocket-Accept" value
    | _ -> loop () in

  match%lwt Lwt_io.read_line input with
  | "HTTP/1.1 101 Switching Protocols" -> loop ()
  | str -> Lwt.return_error ("Wrong response status: " ^ str)

let handshake uri input output =
  let key = Util.(random_string 16 |> Base64.of_string) in
  let hdr = make_request_headers uri key in
  let%lwt () =
    Lwt_io.write_from_string_exactly output hdr 0 (String.length hdr) in
  let%lwt () = Lwt_io.flush output in
  parse_response input key

let inet_addr_of_string host =
  try Unix.inet_addr_of_string host
  with Failure _ ->
    try (Unix.gethostbyname host).h_addr_list.(0)
    with Not_found ->
      Printf.sprintf "cannot resolve '%s'" host |> failwith 

let open_tcp_client (address, port) =
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.(setsockopt fd TCP_NODELAY true);
  let address = Unix.ADDR_INET (address, port) in
  let%lwt () = Lwt_unix.connect fd address in
  Lwt.return fd

let default_tls_settings = `Ca_dir "/etc/ssl/certs"

let connect ?(settings = None) uri =
  let settings = Option.value ~default: Settings.default settings in
  let uri = Uri.of_string uri in
  let host = uri_host uri in
  let port = uri_port uri in

  let use_tls () =
    let%lwt authenticator =
      match settings.Settings.tls with
      | Some authenticator -> Lwt.return authenticator
      | None -> X509_lwt.authenticator default_tls_settings in

    let tls_client = Tls.Config.client ~authenticator () in
    let%lwt fd = open_tcp_client (inet_addr_of_string host, port) in
    let%lwt tls = Tls_lwt.Unix.client_of_fd tls_client ~host fd in
    Tls_lwt.of_t tls
    |> Lwt.return_ok in

  let use_plain () =
    let%lwt fd = open_tcp_client (inet_addr_of_string host, port) in
    let input = Lwt_io.of_fd ~mode: Input fd in
    let output = Lwt_io.of_fd ~mode: Output fd in
    Lwt.return_ok (input, output) in

  let init_websocket uri proc =
    match%lwt proc () with
    | Error error -> Lwt.return_error error
    | Ok (input, output) ->
        match%lwt handshake uri input output with
        | Ok () -> Lwt.return_ok (input, output)
        | Error error -> Lwt.return_error error in

  match Uri.scheme uri with
  | Some "wss" -> init_websocket uri use_tls
  | Some "ws"
  | None -> init_websocket uri use_plain
  | Some _ -> Lwt.return_error "Invalid uri scheme"

let create ?settings uri proc =
  let buf = Buffer.create 64 in

  let safe_close f = try%lwt Lwt_io.close f with _ -> Lwt.return_unit in

  let final_payload payload =
    match Buffer.length buf with
    | 0 -> Bytes.to_string payload
    | _ ->
        Buffer.add_bytes buf payload;
        let resp = Buffer.contents buf in
        Buffer.reset buf;
        resp in

  let apply_payload t payload convert =
    let data = final_payload payload in
    convert data |> proc t in

  let rec read_loop t mq =
    match%lwt Frame.read t.input with
    | End ->
        Mqueue.put mq Request.End;
        let%lwt () = proc t Response.End in
        Lwt.return_unit

    | Error error ->
        Mqueue.put mq End;
        let%lwt () = Response.Error error |> proc t in
        Lwt.return_unit

    | Frame frame ->
        match frame.op with
        | Close ->
            Mqueue.put mq End;
            proc t Response.End
        | Ping ->
            Frame.Control.pong_of_ping frame
            |> Request.frame
            |> Mqueue.put mq;
            read_loop t mq
        | Pong -> read_loop t mq
        | Reserved _ -> read_loop t mq
        | Continuation ->
            Buffer.add_bytes buf frame.payload;
            read_loop t mq
        | Text ->
            let%lwt () = apply_payload t frame.payload Response.text in
            read_loop t mq
        | Binary ->
            let%lwt () = apply_payload t frame.payload Response.binary in
            read_loop t mq in

  let rec write_loop output mq =
    match%lwt Mqueue.take mq with
    | Request.Frame frame ->
        let%lwt () = Frame.write output frame in
        write_loop output mq
    | Request.End -> Lwt.return_unit in

  let rec ping_loop mq interval_s =
    let%lwt () = Lwt_unix.sleep interval_s in
    Frame.Control.ping ()
    |> Request.frame
    |> Mqueue.put mq;
    ping_loop mq interval_s in

  let wait input output promises =
    let%lwt () = try%lwt Lwt.pick promises
                 with Unix.Unix_error _ -> Lwt.return_unit in
    Lwt.join [safe_close input; safe_close output] in

  match%lwt connect ~settings uri with
  | Error error -> Lwt.return_error error
  | Ok (input, output) ->
      let mqueue = Mqueue.create () in
      let t = { mqueue; input; output; } in
      let ping_interval_s = match settings with
                            | Some { ping_interval_s; _ } -> ping_interval_s
                            | None -> 30. in
      let promises = [
        read_loop t mqueue;
        write_loop output mqueue;
        ping_loop mqueue ping_interval_s;
      ] in
      Lwt.async (fun () -> wait input output promises);
      Lwt.return_ok t

let create_exc ?settings uri proc =
  let settings = Option.value ~default: Settings.default settings in
  match%lwt create ~settings uri proc with
  | Ok t -> Lwt.return t
  | Error error -> Lwt.fail_with error

let send_op { mqueue; _ } finish op payload =
  Bytes.of_string payload
  |> Frame.create ~finish ~op
  |> Request.frame
  |> Mqueue.put mqueue

let close { mqueue; _ } = Mqueue.put mqueue Request.End

let send_text t payload = send_op t true Frame.Text payload

let send t payload = send_op t true Frame.Binary payload

module Partial = struct
  module type Type = sig
    type t
    val start: channel -> string -> t
    val next: t -> string -> unit
    val finish: t -> string -> unit
  end

  module type Op = sig
    val op: Frame.op
  end

  module Make(O: Op) = struct
    type t = channel

    let start t payload =
      send_op t false O.op payload;
      t

    let next t payload = send_op t false Continuation payload

    let finish t payload = send_op t true Continuation payload
  end

  module Text = Make(struct let op = Frame.Text end)
  module Binary = Make(struct let op = Frame.Binary end)
end
