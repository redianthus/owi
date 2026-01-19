(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
module M = Scheduler.Schedulable

(* TODO:
   we could use a CPS version of the state monad which could be much more efficient in hot paths, something like:

   type ('a, s') state =
    's -> ('a -> 's -> 'r) -> 'r

   let return x =
    fun s k ->
      k x s

   let bind m f =
    fun s k ->
      m s (fun x s -> f x s k)
*)

type ('a, 's) t = { run : 'r. ('a -> 's -> 'r M.t) -> 's -> 'r M.t } [@@unboxed]

let[@inline] run mxf st = mxf.run (fun a _st -> M.return a) st

let[@inline] return x = { run = (fun k st -> k x st) }

let[@inline] lift (x : 'a M.t) : ('a, 's) t =
  { run = (fun k st -> M.bind x (fun x -> k x st)) }

let[@inline] bind mx f =
  { run = (fun k st -> mx.run (fun x new_st -> (f x).run k new_st) st) }

let[@inline] ( let* ) mx f = bind mx f

let[@inline] map x f =
  let* x in
  return (f x)

let[@inline] ( let+ ) x f = map x f

let[@inline] liftF2 f x y =
  let ( let* ) = M.( let* ) in
  { run =
      (fun k st ->
        let* fx = run x st in
        let* fy = run y st in
        k ((f fx) fy) st )
  }

let[@inline] with_state f = { run = (fun k st -> k (f st) st) }

let[@inline] modify_state f = { run = (fun k st -> k () (f st)) }
