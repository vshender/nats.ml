(** A unique identifier generator that is high performance, very fast, and
    tries to be entropy pool friendly.

    NUID uses 12 bytes of crypto generated data (entropy draining), and 10
    bytes of pseudo-random sequential data that increments with a
    pseudo-random increment.

    Total length of a NUID string is 22 bytes of base 62 ASCII text, so there
    are 62^22 or 2707803647802660400290261537185326956544 possibilities.
*)

module State = struct
  (** The alphabet used to generate identifiers. *)
  let digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

  (** The base used for identifiers encoding. *)

  let base = String.length digits  (* 62 *)

  (** The length of the prefix component of NUID. *)
  let pre_len = 12

  (** The length of the sequential component of NUID. *)
  let seq_len = 10

  (** The maximum value the sequential component can reach. *)
  let max_seq = 839_299_365_868_340_224L  (* base^seqLen = 62^10 *)

  (** The minimum increment value to generate the next NUID. *)
  let min_inc = 33L

  (** The maximum increment value to generate the next NUID. *)
  let max_inc = 333L

  (** The total length of NUID. *)
  let total_len = pre_len + seq_len

  (** Type representing the state of a NUID generator. *)
  type t = {
    cur         : bytes;  (** the current NUID *)
    mutable seq : int64;  (** the sequential part of the current NUID *)
    mutable inc : int64;  (** the increment to generate the next NUID *)
  }

  (** An external function to retrieve an array of random numbers using kernel
      random number generator. *)
  external random_seed: unit -> int array = "caml_sys_random_seed"

  (** [random_numbers n] generates an array of [n] random numbers. *)
  let random_numbers n =
    let r = Array.make n 0
    and i = ref 0 in
    while !i < n do
      let seed = random_seed () in
      let seed_len = Array.length seed in
      Array.blit seed 0 r !i (min seed_len (n - !i));
      i := !i + seed_len
    done;
    r

  (** [randomize_prefix state] generates a new prefix for the NUID.
      This call *can* deplete the entropy pool and is called automatically when
      we exhaust the sequential range. *)
  let randomize_prefix state =
    let rn = random_numbers pre_len in
    for i = 0 to pre_len - 1 do
      Bytes.set state.cur i digits.[rn.(i) mod base]
    done

  (** [reset_sequential state] resets the sequential part of the NUID.
      This function is called automatically when we exhaust the sequential
      range. *)
  let reset_sequential state =
    state.seq <- Random.int64 max_seq;
    state.inc <- Int64.(add min_inc (Random.int64 (sub max_inc min_inc)))

  (** [create ()] creates a new NUID generation state and property initializes
      the prefix, sequential start, and sequential increment. *)
  let create () =
    let state = {
      cur = Bytes.create total_len;
      seq = Int64.zero;
      inc = Int64.zero;
    } in
    randomize_prefix state;
    reset_sequential state;
    state

  (** [copy state] returns a copy of the given NUID generation state. *)
  let copy state = {
    cur = state.cur;
    seq = state.seq;
    inc = state.inc
  }

  (** [fill_seq state] fills the sequential part [state.seq] of the NUID into
      the byte sequence [state.cur]. *)
  let fill_seq state =
    let l = ref state.seq in
    let base = Int64.of_int base in
    for i = total_len - 1 downto pre_len do
      Bytes.set state.cur i digits.[Int64.(rem !l base |> to_int)];
      l := Int64.div !l base
    done

  (** [next state] returns the next unique identifier. *)
  let next state =
    (* Increment the sequential part. *)
    state.seq <- Int64.(add state.seq state.inc);
    if Int64.compare state.seq max_seq >= 0 then begin
      randomize_prefix state;
      reset_sequential state;
    end;

    (* Copy in the seq in base [base]. *)
    fill_seq state;

    Bytes.to_string state.cur
end

(** The global NUID generation state. *)
let gstate =
  Random.self_init ();
  State.create ()

(** [gstate_mutex] is a mutex to ensure thread-safe access to the global NUID
    generation state. *)
let gstate_mutex = Mutex.create ()

(** [next ()] generates a next NUID string in a thread-safe manner from the
    global NUID state instance. *)
let next () =
  (* `Mutex.protect` is not used here because it only appeared in version
     5.1. *)
  Mutex.lock gstate_mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock gstate_mutex)
    (fun () -> State.next gstate)

module TestingInterface = struct
  module State = State
end
