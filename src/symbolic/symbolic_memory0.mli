(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type write =
  { addr : Symbolic_i32.t
  ; value : Smtml.Typed.Bitv8.t
  }

type t =
  { writes : write list
  ; chunks : Symbolic_i32.t Map.Make(Int32).t
  ; size : Symbolic_i32.t
  ; env_id : int
  ; id : int
  }
