(** Reflective generators *)

(** {1 Traces} *)

module Trace_buf : sig
  (** A trace buffer records the sequence of random decisions made by a
      generator to produce a value. *)

  type t
  (** Represents a trace buffer. *)

  type trace_buf := t

  val create : unit -> trace_buf
  val copy : trace_buf -> trace_buf
  val reset : trace_buf -> unit
  val bits : trace_buf -> string
end

(** {1 Generators} *)

type ('sample, 'a) t
(** Represents a generator that produces values of type ['a] or retraces them
    from type ['sample]. Prefer the alias ['a simple = ('a, 'a) t] when
    possible. *)

type ('sample, 'a) gen := ('sample, 'a) t
type 'a simple = ('a, 'a) t

(** {2 Creating and combining generators} *)

val exact : 'a -> ('a -> 'a -> bool) -> 'a simple
val create_forwarded : unit -> ('a, 'b) gen * ('a, 'b) gen ref
val filter_map : ('a -> 'b option) -> ('c, 'a) gen -> ('c, 'b) gen
val map : ('a -> 'b) -> ('c, 'a) gen -> ('c, 'b) gen
val bind : ('a -> ('b, 'c) gen) -> ('b, 'a) gen -> ('b, 'c) gen
val choose : ('a, 'b) gen list -> ('a, 'b) gen

module Syntax : sig
  val ( let+ ) : ('a, 'b) gen -> ('b -> 'c) -> ('a, 'c) gen
  val ( let* ) : ('a, 'b) gen -> ('b -> ('a, 'c) gen) -> ('a, 'c) gen
end

(** {3 Focus combinators} *)

val try_focus : ('a -> 'b option) -> ('b, 'c) gen -> ('a, 'c) gen
val focus : ('a -> 'b) -> ('b, 'c) gen -> ('a, 'c) gen

(** {2 Generating basic types} *)

val bool : bool simple
val full_int : ?shrink:int -> [ `Strict | `Non_strict ] -> int -> int simple

val int_in_range :
  ?shrink:int -> [ `Strict | `Non_strict ] -> min:int -> max:int -> int simple

val small_int : int simple
val option : 'a simple -> 'a option simple
val result : ok:'ok simple -> error:'error simple -> ('ok, 'error) result simple
val list : int simple -> 'a simple -> 'a list simple

(** {2 Running generators} *)

val generate_from_rng :
  Random.State.t -> ('a, 'b) gen -> trace_buf:Trace_buf.t -> 'b option

val generate_from_bits :
  string -> ('a, 'b) gen -> trace_buf:Trace_buf.t -> 'b option

val retrace : 'a -> ('a, 'b) gen -> trace_buf:Trace_buf.t -> 'b option
