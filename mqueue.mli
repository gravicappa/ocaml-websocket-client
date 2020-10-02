type 'a t
val create : unit -> 'a t
val put : 'a t -> 'a -> unit
val take : 'a t -> 'a Lwt.t
val take_available : 'a t -> 'a option
