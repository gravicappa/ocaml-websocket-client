(** Lwt-based event queue *)

type 'a t = {
  q: 'a Queue.t;
  cond: 'a Lwt_condition.t;
  mutable num_listeners: int;
}

let create () =
  {q = Queue.create (); cond = Lwt_condition.create (); num_listeners = 0}

let put t a =
  if t.num_listeners > 0 then
    let () = t.num_listeners <- t.num_listeners - 1 in
    Lwt_condition.signal t.cond a
  else
    Queue.add a t.q

let take t =
  if Queue.is_empty t.q then
    let () = t.num_listeners <- t.num_listeners + 1 in
    Lwt_condition.wait t.cond
  else
    Queue.take t.q |> Lwt.return

let take_available {q; _} =
  if Queue.is_empty q then
    None
  else
    Some (Queue.take q)
