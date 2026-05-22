(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module SVA = struct
  type i32 = Abstract_value.ADomain.binary

  type i64 = Abstract_value.ADomain.binary

  type f32 = Abstract_value.ADomain.binary

  type f64 = Abstract_value.ADomain.binary

  type v128 = Abstract_value.ADomain.binary
end

include Extern.Func.Make (SVA) (Result) (Concrete_memory)
