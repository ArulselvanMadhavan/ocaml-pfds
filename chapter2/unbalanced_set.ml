include Bst_intf

module Unbalanced_set (E : Ordered) = struct
  type elem = E.t

  type tree =
    | E
    | T of tree * elem * tree

  type set = tree

  let empty = E

  let rec member = function
    | _, E -> false
    | x, T (l, y, r) ->
      if E.lt (x, y) (* Comparison#1 *)
      then member (x, l)
      else if E.lt (y, x) (* Comparison#2 *)
      then member (x, r)
      else true
  ;;

  let rec insert = function
    | x, E -> T (E, x, E)
    | x, (T (l, y, r) as s) ->
      if E.lt (x, y)
      then T (insert (x, l), y, r)
      else if E.lt (y, x)
      then T (l, y, insert (x, r))
      else s
  ;;
end
