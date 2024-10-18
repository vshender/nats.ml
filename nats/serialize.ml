(** Serialization of NATS client protocol messages produced by a client. *)

open Faraday

open Protocol

(** [write_crlf serializer] writes CRLF into [serializer]. *)
let write_crlf t =
  write_string t "\r\n"

(** [headers_size h] is the size in bytes of the serialized representation of
    [h]. *)
let headers_size h =
  let (major, minor) = Headers.version h in
  Headers.fold
    (fun k v size -> String.(size + 2 + length k + 2 + length v))
    h
    (Printf.sprintf "NATS/%d.%d" major minor |> String.length)

(** [serialize_headers serializer h] serializes the NATS message headers [h]
    into [serializer]. *)
let serialize_headers t h =
  let (major, minor) = Headers.version h in
  write_string t "NATS/";
  write_string t (Int.to_string major);
  write_char t '.';
  write_string t (Int.to_string minor);
  Headers.iter
    (fun key value ->
       write_crlf t;
       write_string t key;
       write_string t ": ";
       write_string t value)
    h

(** [serialize_connect serializer msg] serializes the "CONNECT" protocol
    message [msg] into [serializer]. *)
let serialize_connect t msg =
  write_string t "CONNECT ";
  write_string t (msg |> Connect.to_yojson |> Yojson.Safe.to_string)

(** [serialize_pub serializer msg] serializes the "PUB" protocol message [msg]
    into [serializer]. *)
let serialize_pub t { Pub.subject; reply; payload } =
  write_string t "PUB ";
  write_string t subject; write_char t ' ';
  begin match reply with
    | Some reply -> write_string t reply; write_char t ' '
    | None       -> ()
  end;
  write_string t (Int.to_string @@ String.length payload);
  if String.length payload > 0 then begin
    write_crlf t;
    write_string t payload
  end

(** [serialize_hpub serializer msg] serializes the "HPUB" protocol message
    [msg] into [serializer]. *)
let serialize_hpub t { HPub.subject; reply; headers = hdrs; payload } =
  write_string t "HPUB ";
  write_string t subject; write_char t ' ';
  begin match reply with
    | Some reply -> write_string t reply; write_char t ' '
    | None       -> ()
  end;
  let hsize = headers_size hdrs in
  write_string t (Int.to_string @@ hsize + 4); write_char t ' ';
  write_string t (Int.to_string @@ hsize + 4 + String.length payload);
  write_crlf t;
  serialize_headers t hdrs;
  write_crlf t; write_crlf t;
  write_string t payload

(** [serialize_sub serializer msg] serializes the "SUB" protocol message [msg]
    into [serializer]. *)
let serialize_sub t { Sub.subject; group; sid } =
  write_string t "SUB ";
  write_string t subject; write_char t ' ';
  begin match group with
    | Some group -> write_string t group; write_char t ' '
    | None       -> ()
  end;
  write_string t sid

(** [serialize_unsub serializer msg] serializes the "UNSUB" protocol message
    [msg] into [serializer]. *)
let serialize_unsub t { UnSub.sid; max_msgs } =
  write_string t "UNSUB ";
  write_string t sid;
  match max_msgs with
  | Some max_msgs ->
    write_char t ' '; write_string t (Int.to_string max_msgs)
  | None ->
    ()

(** [serialize_ping serializer msg] serializes the "PING" protocol message
    [msg] into [serializer]. *)
let serialize_ping t =
  write_string t "PING"

(** [serialize_pong serializer msg] serializes the "PONG" protocol message
    [msg] into [serializer]. *)
let serialize_pong t =
  write_string t "PONG"

let serialize_client_message t (msg : ClientMessage.t) =
  begin match msg with
    | Connect arg -> serialize_connect t arg
    | Pub args    -> serialize_pub t args
    | HPub args   -> serialize_hpub t args
    | Sub args    -> serialize_sub t args
    | UnSub args  -> serialize_unsub t args
    | Ping        -> serialize_ping t
    | Pong        -> serialize_pong t
  end;
  write_crlf t
