type op =
  | Continuation
  | Text
  | Binary
  | Reserved of int
  | Close
  | Ping
  | Pong

type mask = Masked | Unmasked

val op_of_int : int -> op

val int_of_op : op -> int

type t = {
  finish : bool;
  op : op;
  masked : mask;
  payload_length : int;
  mask : bytes;
  payload : bytes;
}

type frame = t

module Response : sig 
  type resp =
    | Frame of frame
    | End
    | Error of string
end

val read : Lwt_io.input_channel -> Response.resp Lwt.t
val write : Lwt_io.output_channel -> t -> unit Lwt.t
val create : ?finish:bool -> ?op:op -> ?masked:mask -> bytes -> t

module Control : sig 
  val create : ?masked:mask -> ?payload:bytes -> op -> t
  val ping : ?payload:bytes -> unit -> t
  val pong : ?payload:bytes -> unit -> t
  val pong_of_ping : t -> t
end
