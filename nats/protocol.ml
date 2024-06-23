(** NATS client protocol messages. *)

module Info = struct
  type t = {
    server_id       : string;
    server_name     : string;
    version         : string;
    go              : string;
    host            : string;
    port            : int;
    headers         : bool;
    max_payload     : Int64.t;
    proto           : int;
    client_id       : (Int64.t option [@yojson.default None]);
    auth_required   : (bool [@default false]);
    tls_required    : (bool [@default false]);
    tls_verify      : (bool [@default false]);
    tls_available   : (bool [@default false]);
    connect_urls    : (string array [@default [||]]);
    ws_connect_urls : (string array [@default [||]]);
    lame_duck_mode  : (bool [@key "ldm"] [@default false]);
    git_commit      : (string option [@yojson.default None]);
    jetstream       : (bool [@default false]);
    ip              : (string option [@yojson.default None]);
    client_ip       : (string option [@yojson.default None]);
    nonce           : (string option [@yojson.default None]);
    cluster         : (string option [@yojson.default None]);
    domain          : (string option [@yojson.default None]);
  }
  [@@deriving
    eq, make, show { with_path = false }, of_yojson { strict = false }]
end

module Connect = struct
  type t = {
    verbose       : bool;
    pedantic      : bool;
    tls_required  : bool;
    auth_token    : string option;
    user          : string option;
    pass          : string option;
    name          : string option;
    lang          : string;
    version       : string;
    protocol      : int;
    echo          : bool;
    signature     : (string option [@key "sig"]);
    jwt           : string option;
    no_responders : bool;
    headers       : bool;
    nkey          : string option;
  }
  [@@deriving eq, make, show { with_path = false }, to_yojson]
end

module Pub = struct
  type t = {
    subject : string;
    reply   : string option;
    payload : string;
  }
  [@@deriving eq, make, show { with_path = false }]
end

module HPub = struct
  type t = {
    subject : string;
    reply   : string option;
    headers : string;
    payload : string;
  }
  [@@deriving eq, make, show { with_path = false }]
end

module Sub = struct
  type t = {
    subject : string;
    group   : string option;
    sid     : string;
  }
  [@@deriving eq, make, show { with_path = false }]
end

module UnSub = struct
  type t = {
    sid : string;
    max_msgs : int option;
  }
  [@@deriving eq, make, show { with_path = false }]
end

module Msg = struct
  type t = {
    subject : string;
    reply   : string option;
    sid     : string;
    payload : string;
  }
  [@@deriving eq, make, show { with_path = false }]
end

module HMsg = struct
  type t = {
    subject : string;
    reply   : string option;
    sid     : string;
    headers : string;
    payload : string;
  }
  [@@deriving eq, make, show { with_path = false }]
end

module ClientMessage = struct
  type t =
    | Connect of Connect.t
    | Pub of Pub.t
    | HPub of HPub.t
    | Sub of Sub.t
    | UnSub of UnSub.t
    | Ping
    | Pong
  [@@deriving eq, show { with_path = false }]
end

module ServerMessage = struct
  type t =
    | Info of Info.t
    | Msg of Msg.t
    | HMsg of HMsg.t
    | Ping
    | Pong
    | Ok
    | Err of string
  [@@deriving eq, show { with_path = false }]
end
