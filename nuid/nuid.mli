(** A unique identifier generator that is high performance, very fast, and
    tries to be entropy pool friendly.

    Total length of a NUID string is 22 bytes of base 62 ASCII text, so there
    are 62{^22} or 2707803647802660400290261537185326956544 possibilities.

    NUID can generate identifiers as fast as 40ns, or ~25 million per second.
    There is an associated benchmark you can use to test performance on your
    own hardware.

    Basic usage:

    {[
      (* Utilize the global locked generator. *)
      # Nuid.next () ;;
      - : string = "TPxizw6qtJTWCsj6NQRTZt"

      (* Create a NUID generation state instance. *)
      # let nst = Nuid.State.create () ;;
      val nst : Nuid.State.t = <abstr>

      (* Generate unique identifiers. *)
      # Nuid.State.next nst ;;
      - : string = "0fmL4InTGejJB8FVu1lU2v"
      # Nuid.State.next nst ;;
      - : string = "0fmL4InTGejJB8FVu1lU6G"
      utop # Nuid.State.next nst ;;
      - : string = "0fmL4InTGejJB8FVu1lU9b"
    ]}
*)

(** The state of a NUID generator. *)
module State : sig
  (** Type representing the state of a NUID generator. *)
  type t

  (** [create ()] creates a new NUID generation state. *)
  val create : unit -> t

  (** [copy state] returns a copy of the given NUID generation state. *)
  val copy : t -> t

  (** [next state] returns the next unique identifier. *)
  val next : t -> string
end

(** [next ()] generates a next NUID string in a thread-safe manner from the
    global NUID state instance. *)
val next : unit -> string


(**/**)

(** Testing interface for the NUID implementation. *)
module TestingInterface : sig
  module State : sig
    val digits : string
    val base : int
    val pre_len : int
    val seq_len : int
    val total_len : int
    val max_seq : Int64.t

    type t = {
      cur         : bytes;
      mutable seq : int64;
      mutable inc : int64;
    }

    val create : unit -> t
    val copy : t -> t
    val next : t -> string
  end
end
