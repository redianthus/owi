(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Instructions *)

module Instruction : sig
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

module Expr : sig
  type t = Instruction.t Annotated.t list
end

module Vertex : sig
  type t =
    { expr : Expr.t
    ; idx : int
    }
end

module Edge : sig
  type t
end

module G : sig
  type t

  val of_binary_expr : Binary.expr -> t
end

module Func : sig
  type t =
    { type_f : Binary.block_type
    ; locals : Binary.param list
    ; body : G.t Annotated.t
    ; id : string option
    }
end

module Global : sig
  module Type : sig
    type nonrec t = Binary.mut * Binary.val_type
  end

  type t =
    { typ : Type.t
    ; init : G.t Annotated.t
    ; id : string option
    }
end

module Data : sig
  module Mode : sig
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

module Elem : sig
  module Mode : sig
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

module Module : sig
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
