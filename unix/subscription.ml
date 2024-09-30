open Compat

type callback = Message.t -> unit

type message_queue = {
  messages  : Message.t Queue.t;
  mutex     : Mutex.t;
  condition : Condition.t;
}

type message_delivery =
  | Callback of callback
  | Queue    of message_queue

type t = {
  sid                : int;
  subject            : string;
  group              : string option;
  delivery           : message_delivery;
  mutable closed     : bool;
  unsubscribe_cb     : (t -> unit) option;
  next_msg_start_cb  : (t -> float option -> unit) option;
  next_msg_finish_cb : (t -> unit) option;
}

let sid t = t.sid

let subject t = t.subject

let group t = t.group

let is_sync t =
  match t.delivery with
  | Callback _ -> false
  | Queue _    -> true

let create
    ?unsubscribe_cb ?next_msg_start_cb ?next_msg_finish_cb
    sid subject group callback =
  let delivery = match callback with
    | Some cb -> Callback cb
    | None    -> Queue {
        messages  = Queue.create ();
        mutex     = Mutex.create ();
        condition = Condition.create ();
      }
  in {
    sid;
    subject;
    group;
    delivery;
    closed = false;
    unsubscribe_cb;
    next_msg_start_cb;
    next_msg_finish_cb;
  }

let close t =
  t.closed <- true

let unsubscribe t =
  t.unsubscribe_cb |> Option.iter (fun cb -> cb t);
  close t

let signal_timeout t =
  match t.delivery with
  | Callback _ ->
    failwith "signal_timeout should only be called for synchronous subscription"
  | Queue q ->
    Mutex.protect q.mutex
      (fun () -> Condition.signal q.condition)

let handle_msg t msg =
  match t.delivery with
  | Callback cb -> cb msg
  | Queue q     ->
    Mutex.protect q.mutex
      begin fun () ->
        Queue.add msg q.messages;
        if Queue.length q.messages = 1 then
          Condition.signal q.condition
      end

let next_msg ?timeout t =
  match t.delivery with
  | Callback _ ->
    failwith "get_message should only be called for synchronous subscription"
  | Queue q ->
    if t.closed
    && Mutex.protect q.mutex (fun () -> Queue.is_empty q.messages) then
      failwith "subscription is closed";

    let timeout_time =
      timeout |> Option.map (fun timeout -> Unix.gettimeofday () +. timeout) in

    let timed_out () =
      match timeout_time with
      | Some timeout_time -> Unix.gettimeofday () > timeout_time
      | None              -> false
    in

    t.next_msg_start_cb |> Option.iter (fun cb -> cb t timeout_time);
    Fun.protect
      ~finally:(fun () -> t.next_msg_finish_cb |> Option.iter (fun cb -> cb t))
      begin fun () ->
        Mutex.protect q.mutex
          begin fun () ->
            while Queue.is_empty q.messages && not (timed_out ()) do
              Condition.wait q.condition q.mutex
            done;
            Queue.take_opt q.messages
          end
      end
