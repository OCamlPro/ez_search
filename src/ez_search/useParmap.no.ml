
(* In case Parmap is not available... *)

type 'a t = A of 'a array

let get_default_ncores () = 0
let parmap ~ncores:_ f t =
  match t with
  | A list ->
      Array.map f list |> Array.to_list
