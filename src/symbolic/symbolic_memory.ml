(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Int32Map = Map.Make (Int32)

type write = Symbolic_memory0.write =
  { addr : Symbolic_i32.t
  ; value : Smtml.Typed.Bitv8.t
  }

type t = Symbolic_memory0.t =
  { writes : write list
  ; chunks : Symbolic_i32.t Int32Map.t
  ; size : Symbolic_i32.t
  ; env_id : int
  ; id : int
  }

let replace memory = Symbolic_choice.map_state (Thread.replace_memory memory)

let zero_byte = Smtml.Typed.Bitv8.v (Smtml.Bitvector.of_int8 0)

type decoded_address =
  | Linear of Symbolic_i32.t
  | Heap of
      { base : int32
      ; offset : Symbolic_i32.t
      }

let decode_address addr =
  match Smtml.Typed.view addr with
  | Val (Bitv bv) ->
    Linear (Symbolic_i32.of_int32 (Smtml.Bitvector.to_int32 bv))
  | Ptr { base; offset } ->
    Heap
      { base = Smtml.Bitvector.to_int32 base
      ; offset = Smtml.Typed.Unsafe.wrap offset
      }
  | _ -> Linear addr

let validate_address m addr size =
  let open Symbolic_choice in
  match decode_address addr with
  | Linear a -> return a
  | Heap { base; offset } ->
    begin match Int32Map.find_opt base m.chunks with
    | None -> trap `Memory_leak_use_after_free
    | Some chunk_size ->
      let last = Symbolic_i32.add offset (Symbolic_i32.of_int (size - 1)) in

      let out =
        Symbolic_boolean.or_
          (Symbolic_i32.le_u chunk_size offset)
          (Symbolic_i32.le_u chunk_size last)
      in

      let* out =
        select out ~instr_counter_true:None ~instr_counter_false:None
      in

      if out then trap `Memory_heap_buffer_overflow
      else return (Symbolic_i32.add (Symbolic_i32.of_int32 base) offset)
    end

let store_byte memory addr value =
  { memory with writes = { addr; value } :: memory.writes }

let rec load_byte_from_writes addr = function
  | [] -> zero_byte
  | { addr = written_addr; value } :: rest ->
    Smtml.Typed.Bool.ite
      (Symbolic_i32.eq addr written_addr)
      value
      (load_byte_from_writes addr rest)

let load_byte addr memory = load_byte_from_writes addr memory.writes

let load_8_s m addr =
  let open Symbolic_choice in
  let* addr = validate_address m addr 1 in
  return (Smtml.Typed.Bitv32.of_int8_s (load_byte addr m))

let load_8_u m addr =
  let open Symbolic_choice in
  let* addr = validate_address m addr 1 in
  return (Smtml.Typed.Bitv32.of_int8_u (load_byte addr m))

let load_16_unchecked m addr =
  let b0 = load_byte addr m in
  let b1 = load_byte (Symbolic_i32.add addr (Symbolic_i32.of_int 1)) m in
  Smtml.Typed.Bitv8.concat b1 b0

let load_16_s m addr =
  let open Symbolic_choice in
  let* addr = validate_address m addr 2 in
  return (Smtml.Typed.Bitv32.of_int16_s (load_16_unchecked m addr))

let load_16_u m addr =
  let open Symbolic_choice in
  let* addr = validate_address m addr 2 in
  return (Smtml.Typed.Bitv32.of_int16_u (load_16_unchecked m addr))

let load_32_unchecked m addr =
  let low = load_16_unchecked m addr in
  let high =
    load_16_unchecked m (Symbolic_i32.add addr (Symbolic_i32.of_int 2))
  in
  Smtml.Typed.Bitv16.concat high low

let load_32 m addr =
  let open Symbolic_choice in
  let* addr = validate_address m addr 4 in
  return (Smtml.Typed.simplify (load_32_unchecked m addr))

let load_64_unchecked m addr =
  let low = load_32_unchecked m addr in
  let high =
    load_32_unchecked m (Symbolic_i32.add addr (Symbolic_i32.of_int 4))
  in
  Smtml.Typed.Bitv32.concat high low

let load_64 m addr =
  let open Symbolic_choice in
  let* addr = validate_address m addr 8 in
  return (Smtml.Typed.simplify (load_64_unchecked m addr))

let load_128 m addr =
  let open Symbolic_choice in
  let* addr = validate_address m addr 16 in
  let low = load_64_unchecked m addr in
  let high =
    load_64_unchecked m (Symbolic_i32.add addr (Symbolic_i32.of_int 8))
  in
  return (Symbolic_v128.concat high low)

let store_bytes m addr bytes =
  List.fold_left
    (fun m (i, b) ->
      store_byte m (Symbolic_i32.add addr (Symbolic_i32.of_int i)) b )
    m
    (List.mapi (fun i b -> (i, b)) bytes)

let store_8 m ~addr value =
  let open Symbolic_choice in
  let* addr = validate_address m addr 1 in
  let byte = Smtml.Typed.Bitv32.extract value ~high:7 ~low:0 in

  replace (store_byte m addr byte)

let store_16 m ~addr value =
  let open Symbolic_choice in
  let* addr = validate_address m addr 2 in
  let bytes = Smtml.Typed.Bitv32.to_bytes value in
  replace (store_bytes m addr bytes)

let store_32 m ~addr value =
  let open Symbolic_choice in
  let* addr = validate_address m addr 4 in
  replace (store_bytes m addr (Smtml.Typed.Bitv32.to_bytes value))

let store_64 m ~addr value =
  let open Symbolic_choice in
  let* addr = validate_address m addr 8 in
  replace (store_bytes m addr (Smtml.Typed.Bitv64.to_bytes value))

let store_128 m ~addr value =
  let open Symbolic_choice in
  let* addr = validate_address m addr 16 in
  replace (store_bytes m addr (Smtml.Typed.Bitv128.to_bytes value))

let page_size = Symbolic_i32.of_int 65_536

let grow m delta =
  let old_size = Symbolic_i32.mul m.size page_size in
  let new_size = Symbolic_i32.(div (add old_size delta) page_size) in
  let size =
    Symbolic_boolean.ite (Symbolic_i32.lt m.size new_size) new_size m.size
  in
  replace { m with size }

let size m = Symbolic_i32.mul m.size page_size

let size_in_pages m = m.size

let fill m ~pos ~len c =
  let open Symbolic_choice in
  let* len = select_i32 len in
  let len = Concrete_i32.to_int len in

  let byte = Smtml.Typed.Bitv8.v (Smtml.Bitvector.of_int8 (Char.code c)) in

  let rec loop m i =
    if i = len then return m
    else
      let addr = Symbolic_i32.add pos (Symbolic_i32.of_int i) in
      let* addr = validate_address m addr 1 in
      loop (store_byte m addr byte) (i + 1)
  in

  let* m = loop m 0 in
  replace m

let blit ~src ~src_idx ~dst ~dst_idx ~len =
  let open Symbolic_choice in
  let* len = select_i32 len in
  let len = Concrete_i32.to_int len in

  let rec loop dst i =
    if i = len then return dst
    else
      let src_addr = Symbolic_i32.add src_idx (Symbolic_i32.of_int i) in
      let dst_addr = Symbolic_i32.add dst_idx (Symbolic_i32.of_int i) in

      let* src_addr = validate_address src src_addr 1 in
      let* dst_addr = validate_address dst dst_addr 1 in

      let byte = load_byte src_addr src in
      loop (store_byte dst dst_addr byte) (i + 1)
  in

  let* dst = loop dst 0 in
  replace dst

let blit_string m str ~src ~dst ~len =
  let open Symbolic_choice in
  let* src = select_i32 src in
  let* dst = select_i32 dst in
  let* len = select_i32 len in

  let src = Int32.to_int src in
  let dst = Int32.to_int dst in
  let len = Int32.to_int len in

  let rec loop m i =
    if i = len then return m
    else
      let byte =
        Smtml.Typed.Bitv8.v
          (Smtml.Bitvector.of_int8 (Char.code (String.get str (src + i))))
      in
      let addr = Symbolic_i32.of_int (dst + i) in
      loop (store_byte m addr byte) (i + 1)
  in

  let* m = loop m 0 in
  replace m

let get_limit_max _m = None

let ptr v =
  let open Symbolic_choice in
  match Smtml.Typed.view v with
  | Ptr { base; _ } ->
    let base = Smtml.Bitvector.to_int32 base in
    return base
  | _ -> assert false

let address a =
  let open Symbolic_choice in
  match Smtml.Typed.view a with
  | Val (Bitv bv) when Smtml.Bitvector.numbits bv <= 32 ->
    return (Smtml.Bitvector.to_int32 bv)
  | Ptr { base; offset } ->
    let base =
      (* TODO: it seems possible to avoid this conversion *)
      Smtml.Bitvector.to_int32 base |> Symbolic_i32.of_int32
    in
    let offset = Smtml.Typed.Unsafe.wrap offset in
    let addr = Symbolic_i32.add base offset in
    select_i32 addr
  | _ -> select_i32 a

let free m p =
  let open Symbolic_choice in
  match Smtml.Typed.view p with
  | Val (Bitv bv) when Smtml.Bitvector.eqz bv -> return Symbolic_i32.zero
  | _ ->
    let* base = ptr p in

    if not (Int32Map.mem base m.chunks) then trap `Double_free
    else
      let chunks = Int32Map.remove base m.chunks in
      let* () = replace { m with chunks } in
      return (Symbolic_i32.of_int32 base)

let realloc m ~ptr ~size =
  let open Symbolic_choice in
  let* base = address ptr in
  let chunks = Int32Map.add base size m.chunks in
  let+ () = replace { m with chunks } in
  Smtml.Typed.ptr base Symbolic_i32.zero

let of_concrete ~env_id ~id original =
  let size = Concrete_memory.size_in_pages original in

  { writes = []
  ; chunks = Int32Map.empty
  ; size = Symbolic_i32.of_int32 size
  ; env_id
  ; id
  }
