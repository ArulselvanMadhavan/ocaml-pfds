include Heap_intf

module Leftist_heap (Element : ORDERED) = struct
  module Elem = Element

  type heap =
    | E
    | T of int * Elem.t * heap * heap

  let rank = function
    | E -> 0
    | T (r, _, _, _) -> r
  ;;

  let makeT (x, a, b) =
    if rank a >= rank b
    then T (rank b + 1, x, a, b)
    else T (rank a + 1, x, b, a)
  ;;

  let empty = E

  let is_empty = function
    | E -> true
    | _ -> false
  ;;

  (* merge two heaps. Heaps always have the minimum element at the root.
     So, we can compare the root elements and decide the new root.
     Branch that goes on the left - decided by comparing the rank of the recursive merge and Left child of the node with the minimum element *)
  let rec merge = function
    | h, E -> h
    | E, h -> h
    | (T (_, x, a, b) as h1), (T (_, y, c, d) as h2) ->
      if Elem.leq (x, y)
      then makeT (x, a, merge (b, h2))
      else makeT (y, c, merge (h1, d))
  ;;

  let insert (x, h) = merge (T (1, x, E, E), h)

  let find_min = function
    | E -> raise Empty
    | T (_, x, _, _) -> x
  ;;

  let delete_min = function
    | E -> raise Empty
    | T (_, _, a, b) -> merge (a, b)
  ;;
end
