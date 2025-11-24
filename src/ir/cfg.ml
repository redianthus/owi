(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Instructions *)

module Instruction = struct
  type t =
    (* Numeric Instructions *)
    | I32_const of Int32.t
    | I64_const of Int64.t
    | F32_const of Float32.t
    | F64_const of Float64.t
    | V128_const of V128.t
    | I_unop of Binary.nn * Binary.iunop
    | F_unop of Binary.nn * Binary.funop
    | I_binop of Binary.nn * Binary.ibinop
    | F_binop of Binary.nn * Binary.fbinop
    | V_ibinop of Binary.ishape * Binary.vibinop
    | I_testop of Binary.nn * Binary.itestop
    | I_relop of Binary.nn * Binary.irelop
    | F_relop of Binary.nn * Binary.frelop
    | I_extend8_s of Binary.nn
    | I_extend16_s of Binary.nn
    | I64_extend32_s
    | I32_wrap_i64
    | I64_extend_i32 of Binary.sx
    | I_trunc_f of Binary.nn * Binary.nn * Binary.sx
    | I_trunc_sat_f of Binary.nn * Binary.nn * Binary.sx
    | F32_demote_f64
    | F64_promote_f32
    | F_convert_i of Binary.nn * Binary.nn * Binary.sx
    | I_reinterpret_f of Binary.nn * Binary.nn
    | F_reinterpret_i of Binary.nn * Binary.nn
    (* Reference instructions *)
    | Ref_null of Binary.heap_type
    | Ref_is_null
    | Ref_func of Binary.indice
    (* Parametric instructions *)
    | Drop
    | Select of Binary.val_type list option
    (* Variable instructions *)
    | Local_get of Binary.indice
    | Local_set of Binary.indice
    | Local_tee of Binary.indice
    | Global_get of Binary.indice
    | Global_set of Binary.indice
    (* Table instructions *)
    | Table_get of Binary.indice
    | Table_set of Binary.indice
    | Table_size of Binary.indice
    | Table_grow of Binary.indice
    | Table_fill of Binary.indice
    | Table_copy of Binary.indice * Binary.indice
    | Table_init of Binary.indice * Binary.indice
    | Elem_drop of Binary.indice
    (* Memory instructions *)
    | I_load of Binary.nn * Binary.memarg
    | F_load of Binary.nn * Binary.memarg
    | I_store of Binary.nn * Binary.memarg
    | F_store of Binary.nn * Binary.memarg
    | I_load8 of Binary.nn * Binary.sx * Binary.memarg
    | I_load16 of Binary.nn * Binary.sx * Binary.memarg
    | I64_load32 of Binary.sx * Binary.memarg
    | I_store8 of Binary.nn * Binary.memarg
    | I_store16 of Binary.nn * Binary.memarg
    | I64_store32 of Binary.memarg
    | Memory_size
    | Memory_grow
    | Memory_fill
    | Memory_copy
    | Memory_init of Binary.indice
    | Data_drop of Binary.indice
    (* Control instructions *)
    | Nop
    | Unreachable
    | Block of string option * Binary.block_type option
    | Loop of string option * Binary.block_type option
    | If_else of string option * Binary.block_type option
    | Br of Binary.indice
    | Br_if of Binary.indice
    | Br_table of Binary.indice array * Binary.indice
    | Return
    | Return_call of Binary.indice
    | Return_call_indirect of Binary.indice * Binary.block_type
    | Return_call_ref of Binary.block_type
    | Call of Binary.indice
    | Call_indirect of Binary.indice * Binary.block_type
    | Call_ref of Binary.indice
    (* extern *)
    | Extern_externalize
    | Extern_internalize
end

module Expr = struct
  type t = Instruction.t Annotated.t list
end

module Vertex = struct
  (** A node of the control flow graph is an expression. The invariant is that
      it should not contain any "block". *)
  type t =
    { expr : Expr.t
    ; idx : int
    }

  let compare n1 n2 = Int.compare n1.idx n2.idx

  let hash n = Int.hash n.idx

  let equal n1 n2 = Int.equal n1.idx n2.idx
end

module Edge = struct
  (** An edge is a value on which we will branch. The option is used for the
      "default" case. *)
  type t = Int32.t option

  let compare n1 n2 = Option.compare Int32.compare n1 n2

  let default = None
end

module G = struct
  include Graph.Persistent.Digraph.ConcreteLabeled (Vertex) (Edge)

  let build nodes edges =
    let graph = empty in

    let tbl = Hashtbl.create 512 in

    (* adding all vertices *)
    let graph =
      List.fold_left
        (fun graph (idx, expr) ->
          let vertex = { Vertex.expr; idx } in
          Hashtbl.add tbl idx vertex;
          add_vertex graph vertex )
        graph nodes
    in

    (* adding all edges *)
    List.fold_left
      (fun graph (parent, child, branch) ->
        let parent =
          match Hashtbl.find_opt tbl parent with
          | None -> assert false
          | Some parent -> parent
        in
        let child =
          match Hashtbl.find_opt tbl child with
          | None -> assert false
          | Some child -> child
        in
        let edge = E.create parent branch child in
        add_edge_e graph edge )
      graph edges

  type to_add =
    | Ind of int
    | Next
    | End

  let increase x =
    let n, m, s = x in
    match m with Ind m -> (n, Ind (m + 1), s) | _ -> x

  let update_edges_end x edges (n, m, s) =
    match m with End -> (n, x, s) :: edges | _ -> edges

  let update_edges x next (edges, acc) (n, m, s) =
    match m with
    | Next -> ((n, next, s) :: edges, acc)
    | Ind 0 -> ((n, x, s) :: edges, acc)
    | Ind m -> (edges, (n, Ind (m - 1), s) :: acc)
    | _ -> (edges, (n, m, s) :: acc)

  let rec of_binary_expr (l : Binary.expr)
    (nodes : (int * Instruction.t Annotated.t list) list) (n : int)
    (node : Instruction.t list Annotated.t) edges
    (edges_to_add : (int * to_add * Int32.t option) list) continue =
    match l with
    | [] -> (
      match node.raw with
      | [] ->
        if continue then
          let nodes = (n, node) :: nodes in
          let edges_to_add = (n, Next, None) :: edges_to_add in
          (nodes, edges, n + 1, edges_to_add, true)
        else (nodes, edges, n, edges_to_add, true)
      | _ ->
        let nodes = (n, node) :: nodes in
        let edges_to_add = (n, Next, None) :: edges_to_add in
        (nodes, edges, n + 1, edges_to_add, true) )
    | instr :: l -> (
      match instr.raw with
      | Block (_, _, exp) ->
        let nodes, edges, n, edges_to_add, continue =
          of_binary_expr exp.raw nodes n node edges
            (List.map increase edges_to_add)
            continue
        in
        let edges, edges_to_add =
          List.fold_left (update_edges n n) (edges, []) edges_to_add
        in
        of_binary_expr l nodes n [] edges edges_to_add continue
      | Loop (id, bt, exp) ->
        let nodes, edges, n =
          match node with
          | [] -> (nodes, edges, n)
          | _ ->
            let nodes = (n, node) :: nodes in
            let edges = (n, n + 1, None) :: edges in
            (nodes, edges, n + 1)
        in
        let nodes, edges, n', edges_to_add, continue =
          of_binary_expr exp.raw nodes n
            [ Instruction.Loop (id, bt) ]
            edges
            (List.map increase edges_to_add)
            continue
        in
        let edges, edges_to_add =
          List.fold_left (update_edges n n') (edges, []) edges_to_add
        in
        of_binary_expr l nodes n' [] edges edges_to_add continue
      | If_else (id, t, e1, e2) ->
        let nodes = (n, Instruction.If_else (id, t) :: node) :: nodes in

        let edges = (n, n + 1, Some 1l) :: edges in
        let nodes, edges, n1, edges_to_add, continue' =
          of_binary_expr e1.raw nodes (n + 1) [] edges
            (List.map increase edges_to_add)
            continue
        in

        let edges = (n, n1, Some 0l) :: edges in
        let nodes, edges, n2, edges_to_add', continue =
          of_binary_expr e2.raw nodes n1 [] edges [] continue
        in

        let edges, edges_to_add =
          List.fold_left (update_edges n2 n2) (edges, [])
            (edges_to_add' @ edges_to_add)
        in
        of_binary_expr l nodes n2 [] edges edges_to_add (continue || continue')
      | Br i ->
        let nodes = (n, Instruction.Br i :: node) :: nodes in
        let edges_to_add = (n, Ind i, None) :: edges_to_add in
        (nodes, edges, n + 1, edges_to_add, false)
      | Br_if i ->
        let nodes = (n, Instruction.Br_if i :: node) :: nodes in
        let edges_to_add = (n, Ind i, Some 1l) :: edges_to_add in
        let edges = (n, n + 1, Some 0l) :: edges in
        of_binary_expr l nodes (n + 1) [] edges edges_to_add continue
      | Br_table (inds, i) ->
        let nodes = (n, Instruction.Br_table (inds, i) :: node) :: nodes in
        let edges_to_add = (n, Ind i, None) :: edges_to_add in
        let edges_to_add, _ =
          Array.fold_left
            (fun (acc, x) (i : Binary.indice) ->
              ((n, Ind i, Some (Int32.of_int x)) :: acc, x + 1) )
            (edges_to_add, 0) inds
        in
        (nodes, edges, n + 1, edges_to_add, false)
      | Return | Return_call _ | Return_call_indirect _ | Return_call_ref _ ->
        let nodes = (n, node) :: nodes in
        let edges_to_add = (n, End, None) :: edges_to_add in
        (nodes, edges, n + 1, edges_to_add, false)
      | Unreachable ->
        let nodes = (n, Instruction.Unreachable :: node) :: nodes in
        (nodes, edges, n + 1, edges_to_add, false)
      | Call f ->
        let nodes = (n, Instruction.Call f :: node) :: nodes in
        let edges = (n, n + 1, None) :: edges in
        of_binary_expr l nodes (n + 1) [] edges edges_to_add continue
      | Call_indirect (f, t) ->
        let nodes = (n, Instruction.Call_indirect (f, t) :: node) :: nodes in
        let edges = (n, n + 1, None) :: edges in
        of_binary_expr l nodes (n + 1) [] edges edges_to_add continue
        (*| _ ->
        of_binary_expr l nodes n (instr :: node) edges edges_to_add continue
*)
      | _ -> assert false )

  let of_binary_expr (expr : Binary.expr) =
    let nodes, edges, n, edges_to_add, _ =
      of_binary_expr expr [] 0 [] [] [] true
    in
    let nodes = (n, [ Annotated.dummy Binary.Return ]) :: nodes in
    let edges, edges_to_add =
      List.fold_left (update_edges n n) (edges, []) edges_to_add
    in
    let edges = List.fold_left (update_edges_end n) edges edges_to_add in
    build nodes edges
end

module Func = struct
  type t =
    { type_f : Binary.block_type
    ; locals : Binary.param list
    ; body : G.t Annotated.t
    ; id : string option
    }
end

module Global = struct
  module Type = struct
    type nonrec t = Binary.mut * Binary.val_type
  end

  type t =
    { typ : Type.t
    ; init : G.t Annotated.t
    ; id : string option
    }
end

module Data = struct
  module Mode = struct
    type t =
      | Passive
      | Active of int * G.t Annotated.t
  end

  type t =
    { id : string option
    ; init : string
    ; mode : Mode.t
    }
end

module Elem = struct
  module Mode = struct
    type t =
      | Passive
      | Declarative
      (* TODO: Elem_active binary+const expr*)
      | Active of int option * G.t Annotated.t
  end

  type t =
    { id : string option
    ; typ : Binary.ref_type (* TODO: init : binary+const expr*)
    ; init : G.t Annotated.t list
    ; mode : Mode.t
    }
end

module Module = struct
  type t =
    { id : string option
    ; types : Binary.Typedef.t array
    ; global : (Global.t, Global.Type.t) Origin.t array
    ; table : (Binary.Table.t, Binary.Table.Type.t) Origin.t array
    ; mem : (Binary.Mem.t, Binary.limits) Origin.t array
    ; func : (Func.t, Binary.block_type) Origin.t array
        (* TODO: switch to Binary.func_type *)
    ; elem : Elem.t array
    ; data : Data.t array
    ; exports : Binary.Module.Exports.t
    ; start : int option
    ; custom : Binary.Custom.t list
    }
end
