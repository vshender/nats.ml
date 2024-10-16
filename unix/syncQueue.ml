(** A module for managing thread-safe queues. *)

open Compat

(** A type representing a thread-safe message queue. *)
type 'a t = {
  messages : 'a Queue.t;
  (** A queue of pending messages. *)
  mutex : Mutex.t;
  (** A mutex for synchronizing access to the message queue. *)
  empty : Condition.t;
  (** A condition variable for signaling when the queue becomes empty. *)
  nonempty : Condition.t;
  (** A condition variable for signaling when the queue becomes nonempty. *)
}

let create () = {
  messages = Queue.create ();
  mutex    = Mutex.create ();
  empty    = Condition.create ();
  nonempty = Condition.create ();
}

let length t =
  Mutex.protect t.mutex (fun () -> Queue.length t.messages)

let is_empty t =
  length t = 0

let try_get t =
  Mutex.protect t.mutex
    begin fun () ->
      let msg = Queue.take_opt t.messages in
      if Option.is_some msg && Queue.is_empty t.messages then
        Condition.signal t.empty;
      msg
    end

let get ?interrupt_cond t =
  let should_interrupt =
    Option.value interrupt_cond ~default:(Fun.const false) in
  Mutex.protect t.mutex
    begin fun () ->
      while Queue.is_empty t.messages && not (should_interrupt ()) do
        Condition.wait t.nonempty t.mutex
      done;
      let msg = Queue.take_opt t.messages in
      if Option.is_some msg && Queue.is_empty t.messages then
        Condition.signal t.empty;
      msg
    end

let clear t =
  Mutex.protect t.mutex
    begin fun () ->
      let was_empty = Queue.is_empty t.messages in
      Queue.clear t.messages;
      if not was_empty then
        Condition.signal t.empty
    end

let join ?interrupt_cond t =
  let should_interrupt =
    Option.value interrupt_cond ~default:(Fun.const false) in
  Mutex.protect t.mutex
    begin fun () ->
      while not (Queue.is_empty t.messages || should_interrupt ()) do
        Condition.wait t.empty t.mutex
      done;
      Queue.is_empty t.messages
    end

let put t msg =
  Mutex.protect t.mutex
    begin fun () ->
      Queue.add msg t.messages;
      if Queue.length t.messages = 1 then
        Condition.signal t.nonempty
    end

let signal_interrupt t =
  Mutex.protect t.mutex
    begin fun () ->
      Condition.signal t.empty;
      Condition.signal t.nonempty
    end
