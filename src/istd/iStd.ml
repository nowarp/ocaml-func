include Core
include Result.Let_syntax

let return = Option.some
let bind o f =
  match o with
  | None -> None
  | Some x -> f x
let ( >>= ) = bind
let ( let* ) = bind

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
