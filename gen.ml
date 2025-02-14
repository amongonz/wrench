module Trace_buf = struct
  type trace_pick =
    | Bool of bool
    | Int of {
        value : int;
        shrink : int;
      }

  type t = trace_pick Dynarray.t

  let create = Dynarray.create
  let copy = Dynarray.copy
  let reset = Dynarray.clear

  let bits trace_buf =
    let rand = Buffer.create 0 in

    Dynarray.iter
      (function
        | Bool value -> Buffer.add_uint8 rand (Bool.to_int value)
        | Int pick -> Buffer.add_int64_ne rand (Int64.of_int pick.value))
      trace_buf;

    Buffer.contents rand

  let add_bool trace_buf value = Dynarray.add_last trace_buf (Bool value)

  let add_int trace_buf value ~shrink =
    Dynarray.add_last trace_buf (Int { value; shrink })
end

exception Bad_input

module Input = struct
  type t =
    | Random of Random.State.t
    | Bits of {
        rand : string;
        mutable pos : int;
      }

  let bool = function
    | Random rng -> Random.State.bool rng
    | Bits repro ->
        if not (repro.pos < String.length repro.rand) then
          raise_notrace Bad_input;
        let p = repro.rand.[repro.pos] <> '\x00' in
        repro.pos <- repro.pos + 1;
        p

  let get_int rand pos =
    if not (pos + 8 <= String.length rand) then raise_notrace Bad_input;
    Int64.to_int (String.get_int64_ne rand pos)

  let full_int input bound =
    match input with
    | Random rng -> Random.State.full_int rng bound
    | Bits repro ->
        let n = get_int repro.rand repro.pos in
        if not (0 <= n && n < bound) then raise_notrace Bad_input;
        repro.pos <- repro.pos + 8;
        n

  let int_in_range input ~min ~max =
    match input with
    | Random rng -> Random.State.int_in_range rng ~min ~max
    | Bits repro ->
        let n = get_int repro.rand repro.pos in
        if not (min <= n && n <= max) then raise_notrace Bad_input;
        repro.pos <- repro.pos + 8;
        n
end

type ('sample, 'a) t = {
  forward : Trace_buf.t -> Input.t -> 'a;
  backward : Trace_buf.t -> 'sample -> 'a;
}

type 'a simple = ('a, 'a) t

let exact x (equal : 'a -> 'a -> bool) =
  let backward _trace sample =
    if not (equal sample x) then raise_notrace Bad_input;
    x
  in
  { forward = (fun _ _ -> x); backward }

let create_forwarded () =
  let gen_ref =
    let f _ _ = invalid_arg "generator not implemented" in
    ref { forward = f; backward = f }
  in
  let forward trace_buf input = !gen_ref.forward trace_buf input in
  let backward trace_buf sample = !gen_ref.backward trace_buf sample in
  ({ forward; backward }, gen_ref)

let bool =
  let forward trace_buf input =
    let value = Input.bool input in
    Trace_buf.add_bool trace_buf value;
    value
  in
  let backward trace_buf sample =
    Trace_buf.add_bool trace_buf sample;
    sample
  in
  { forward; backward }

let full_int ?shrink strictness bound =
  if not (0 < bound) then invalid_arg "expected 0 < bound";

  let shrink = Option.value shrink ~default:0 in
  if not (0 <= shrink && shrink < bound) then
    invalid_arg "expected 0 <= shrink < bound";

  let forward trace_buf input =
    let value = Input.full_int input bound in
    Trace_buf.add_int trace_buf value ~shrink;
    value
  in
  let backward =
    let non_strict trace_buf sample =
      Trace_buf.add_int trace_buf sample ~shrink;
      sample
    in
    let strict trace_buf sample =
      if not (0 <= sample && sample < bound) then raise_notrace Bad_input;
      non_strict trace_buf sample
    in
    match strictness with `Non_strict -> non_strict | `Strict -> strict
  in
  { forward; backward }

let int_in_range ?shrink strictness ~min ~max =
  if not (min < max) then invalid_arg "expected min < max";

  let shrink = Option.value shrink ~default:min in
  if not (min <= shrink && shrink <= max) then
    invalid_arg "expected min <= shrink <= max";

  let forward trace_buf input =
    let value = Input.int_in_range input ~min ~max in
    Trace_buf.add_int trace_buf value ~shrink;
    value
  in
  let backward =
    let non_strict trace_buf sample =
      Trace_buf.add_int trace_buf sample ~shrink;
      sample
    in
    let strict trace_buf sample =
      if not (min <= sample && sample <= max) then raise_notrace Bad_input;
      non_strict trace_buf sample
    in
    match strictness with `Non_strict -> non_strict | `Strict -> strict
  in
  { forward; backward }

let small_int = int_in_range `Non_strict ~min:0 ~max:99

let filter_map mapping gen =
  let forward trace_buf input =
    match mapping (gen.forward trace_buf input) with
    | Some x -> x
    | None -> raise_notrace Bad_input
  in
  let backward trace_buf sample =
    match mapping (gen.backward trace_buf sample) with
    | Some x -> x
    | None -> raise_notrace Bad_input
  in
  { forward; backward }

let map mapping gen =
  let forward trace_buf input = mapping (gen.forward trace_buf input) in
  let backward trace_buf sample = mapping (gen.backward trace_buf sample) in
  { forward; backward }

let bind binder gen =
  let forward trace_buf input =
    let x = gen.forward trace_buf input in
    (binder x).forward trace_buf input
  in
  let backward trace_buf sample =
    let x = gen.backward trace_buf sample in
    (binder x).backward trace_buf sample
  in
  { forward; backward }

let choose cases =
  let cases = Array.of_list cases in
  let index_gen = full_int `Strict (Array.length cases) in
  let forward trace_buf input =
    let i = index_gen.forward trace_buf input in
    cases.(i).forward trace_buf input
  in
  let rec backward i trace_buf sample =
    let checkpoint = Dynarray.length trace_buf in
    ignore (index_gen.backward trace_buf i);

    try cases.(i).backward trace_buf sample
    with Bad_input ->
      Dynarray.truncate trace_buf checkpoint;
      backward (i + 1) trace_buf sample
  in
  { forward; backward = backward 0 }

module Syntax = struct
  let ( let+ ) gen mapping = map mapping gen
  let ( let* ) gen binder = bind binder gen
end

let try_focus mapping gen =
  let backward trace_buf sample =
    match mapping sample with
    | None -> raise_notrace Bad_input
    | Some x -> gen.backward trace_buf x
  in
  { gen with backward }

let focus mapping gen =
  let backward trace_buf sample = gen.backward trace_buf (mapping sample) in
  { gen with backward }

let option elem_gen =
  let is_some_gen = bool in

  let forward trace_buf input =
    if is_some_gen.forward trace_buf input then
      Some (elem_gen.forward trace_buf input)
    else None
  in
  let backward trace_buf sample =
    ignore (is_some_gen.backward trace_buf (Option.is_some sample));
    Option.map (elem_gen.backward trace_buf) sample
  in
  { forward; backward }

let result ~ok:ok_gen ~error:error_gen =
  let is_ok_gen = bool in

  let forward trace_buf input =
    if is_ok_gen.forward trace_buf input then
      Ok (ok_gen.forward trace_buf input)
    else Error (error_gen.forward trace_buf input)
  in
  let backward trace_buf sample =
    ignore (is_ok_gen.backward trace_buf (Result.is_ok sample));

    match sample with
    | Ok ok -> Ok (ok_gen.backward trace_buf ok)
    | Error error -> Error (error_gen.backward trace_buf error)
  in
  { forward; backward }

let list len_gen elem_gen =
  let forward trace_buf input =
    let len = len_gen.forward trace_buf input in
    List.init len (fun _ -> elem_gen.forward trace_buf input)
  in
  let backward trace_buf sample =
    ignore (len_gen.backward trace_buf (List.length sample));
    List.map (elem_gen.backward trace_buf) sample
  in
  { forward; backward }

let generate_from_rng rng gen ~trace_buf =
  Trace_buf.reset trace_buf;

  match gen.forward trace_buf (Random rng) with
  | x -> Some x
  | exception Bad_input -> None

let generate_from_bits rand gen ~trace_buf =
  Trace_buf.reset trace_buf;

  match gen.forward trace_buf (Bits { rand; pos = 0 }) with
  | x -> Some x
  | exception Bad_input -> None

let retrace sample gen ~trace_buf =
  Trace_buf.reset trace_buf;

  match gen.backward trace_buf sample with
  | x -> Some x
  | exception Bad_input -> None
