(** Simple websocket client
    Usage example:
{[
let main uri =
  let module WS = Websocket_client in

  let process_message = function
    | WS.Response.End -> prerr_endline "<< END"
    | Error error -> Printf.eprintf "<< ERROR: %s\n%!" error
    | Text data -> Printf.eprintf "<< TEXT: %s\n%!" data
    | Binary data -> Printf.eprintf "<< BINARY: %s\n%!" data in

  let websocket_proc event =
    process_message event;
    Lwt.return_unit in

  match%lwt WS.create uri websocket_proc with
  | Ok channel ->
      WS.send_text channel "Ku!";
      let%lwt () = Lwt_unix.sleep 2. in

      (* fragmented message *)
      let t = WS.Partial.Text.start channel "One" in
      WS.Partial.Text.next t " Two";
      WS.Partial.Text.next t " Three";
      WS.Partial.Text.finish t " Four";
      let%lwt () = Lwt_unix.sleep 2. in
      WS.close channel;
      let%lwt () = Lwt_unix.sleep 2. in
      Lwt.return_unit
  | Error error ->
      Printf.eprintf "Connection error: %s\n%!" error;
      Lwt.return_unit

let () = main "ws://...." |> Lwt_main.run
]}
    *)

type t
type channel = t

module Settings : sig
  type t = {
    tls: X509.Authenticator.t option;
    ping_interval_s: float option;
  }
  val default: t
  val with_tls: X509.Authenticator.t -> t -> t
  val with_ping_interval_s: float -> t -> t
end

module Response : sig
  type t =
    | Text of string
    | Binary of string
    | Error of string
    | End
end

val create :
  ?settings:Settings.t
  -> string
  -> (t -> Response.t -> unit Lwt.t)
  -> (t, [> `Msg of string]) result Lwt.t

val create_exc :
  ?settings:Settings.t -> string -> (t -> Response.t -> unit Lwt.t) -> t Lwt.t

val close : t -> unit

val is_open : t -> bool

val send_text : t -> string -> unit

val send : t -> string -> unit

module Partial: sig
  module type Type = sig
    type t
    val start : channel -> string -> t
    val next : t -> string -> unit
    val finish : t -> string -> unit
  end
  module Text: Type
  module Binary: Type
end
