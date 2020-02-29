include Bst_intf

module Unbalanced_FiniteMap (E : Ordered) : Finite_map = struct
  type key = E.t

  type 'a map =
    | Empty
    | Leaf of key * 'a
    | Node of 'a map * key * 'a * 'a map

  let empty = Empty

  let bind (key, a, map) =
    let rec upsert = function
      | Empty -> Leaf (key, a)
      | Leaf (k, v) ->
        if E.lt (k, key)
        then Node (Leaf (key, a), k, v, Empty)
        else if E.lt (key, k)
        then Node (Empty, k, v, Leaf (key, a))
        else Leaf (k, a)
      | Node (l, k, v, r) ->
        if E.lt (k, key)
        then Node (upsert l, k, v, r)
        else if E.lt (key, k)
        then Node (l, k, v, upsert r)
        else Node (l, k, a, r)
    in
    upsert map
  ;;

  let lookup (key, map) =
    let rec helper = function
      | Empty -> raise Not_found
      | Leaf (k, v) -> if E.eq (k, key) then v else raise Not_found
      | Node (l, k, v, r) ->
        if E.lt (key, k)
        then helper l
        else if E.lt (key, k)
        then helper r
        else v
    in
    helper map
  ;;
end
