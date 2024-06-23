(** Parsing of NATS client protocol messages sending by a server. *)

open Angstrom

open Protocol

(** A module containing helper parsing functions. *)
module P = struct
  (** [is_space c] is true if [c] is a whitespace character. *)
  let is_space = function
    | ' ' | '\t' -> true
    | _          -> false

  (** [is_cr c] is true if [c] is carriage return. *)
  let is_cr = Char.equal '\r'

  (** [is_token c] is true if [c] is a valid token character. *)
  let is_token = function
    | ' ' | '\t' | '\r' | '\n' -> false
    | _                        -> true
end

(** [skip_str_ci s] accepts the sting [s], ignoring case, and discards it.
    [skip_str_ci s] is equivalent to [string_ci] but discards the accepted
    string. *)
let skip_str_ci s =
  string_ci s *> return ()
  <|> fail (s ^ " expected")

(** [token] accepts a sequence of token characters and returns it as a
    string. *)
let token =
  take_while1 P.is_token
  <|> fail "token expected"

(** [spaces] accepts a sequence of zero or more whitespace characters and
    discards it. *)
let spaces =
  skip_while P.is_space

(** [spaces1] accepts a sequence of at least one whitespace character and
    discards it. *)
let spaces1 =
  satisfy P.is_space *> spaces
  <|> fail "space expected"

(** [crlf] accepts a CRLF ("\r\n") character sequence and discards the accepted
    characters. *)
let crlf =
  string "\r\n" *> return ()
  <|> fail "CRLF expected"

(** [single_quote] accepts a single quotation mark ('\'') and discards it. *)
let single_quote =
  char '\'' *> return ()
  <|> fail "single quote expected"

(** [info] parses an "INFO" protocol message. *)
let info =
  skip_str_ci "INFO" *> spaces1 *>
  take_till P.is_cr <* crlf >>= fun info ->
  match info |> Yojson.Safe.from_string |> Info.of_yojson with
  | Ok info                       -> return (ServerMessage.Info info)
  | Error _                       -> fail "malformed payload"
  | exception Yojson.Json_error _ -> fail "malformed payload"

(** [msg] parses a "MSG" protocol message. *)
let msg =
  skip_str_ci "MSG" *> spaces1 *>
  sep_by spaces1 token <* crlf >>= fun tokens ->
  try
    let subject, sid, reply, size =
      match tokens with
      | [subject; sid; size]        -> subject, sid, None, int_of_string size
      | [subject; sid; reply; size] -> subject, sid, Some reply, int_of_string size
      | _                           -> failwith "wrong number of args"
    in
    take size <* crlf >>| fun payload ->
    ServerMessage.Msg { Msg.subject; sid; reply; payload }
  with
  | Failure err -> fail err

(** [hmsg] parses an "HMSG" protocol message. *)
let hmsg =
  skip_str_ci "HMSG" *> spaces1 *>
  sep_by spaces1 token <* crlf >>= fun tokens ->
  try
    let subject, sid, reply, hdr_size, size =
      match tokens with
      | [subject; sid; hdr_size; size] ->
        subject, sid, None, int_of_string hdr_size, int_of_string size
      | [subject; sid; reply; hdr_size; size] ->
        subject, sid, Some reply, int_of_string hdr_size, int_of_string size
      | _ ->
        failwith "wrong number of args"
    in
    lift2
      (fun headers payload ->
         ServerMessage.HMsg { subject; sid; reply; headers; payload })
      (take (hdr_size - 4) <* crlf <* crlf)
      (take (size - hdr_size) <* crlf)
  with
  | Failure err -> fail err

(** [ping] parses a "PING" protocol message. *)
let ping = skip_str_ci "PING" *> crlf *> return ServerMessage.Ping

(** [pong] parses a "PONG" protocol message. *)
let pong = skip_str_ci "PONG" *> crlf *> return ServerMessage.Pong

(** [ok] parses an "+OK" protocol message. *)
let ok = skip_str_ci "+OK" *> crlf *> return ServerMessage.Ok

(** [err] parses an "-ERR" protocol message. *)
let err =
  skip_str_ci "-ERR" *> spaces1 *>
  single_quote *> take_till (Char.equal '\'') <* single_quote <* crlf >>| fun err ->
  ServerMessage.Err err

let server_message =
  peek_char_fail <?> "NATS" >>= function
  | 'I' | 'i'     -> info                    <?> "INFO"
  | 'M' | 'm'     -> msg                     <?> "MSG"
  | 'H' | 'h'     -> hmsg                    <?> "HMSG"
  | 'P' | 'p'     -> begin
      peek_string 2 <?> "NATS" >>= fun s ->
      match s.[1] with
      | 'i' | 'I' -> ping                    <?> "PING"
      | 'o' | 'O' -> pong                    <?> "PONG"
      | _         -> fail "unknown protocol" <?> "NATS"
    end
  | '+'           -> ok                      <?> "OK"
  | '-'           -> err                     <?> "ERR"
  | _             -> fail "unknown protocol" <?> "NATS"
