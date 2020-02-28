include Bst_intf

module Exercise2_2 (E : Ordered) : Set with type elem = E.t = struct
  type elem = E.t

  type tree =
    | E
    | T of tree * elem * tree

  type set = tree

  let empty = E

  let rec member_less_comp = function
    | _, None, E -> false
    | x, Some m, E -> if E.eq (x, m) then true else false
    | x, m, T (l, c, r) ->
      if E.leq (c, x)
      then member_less_comp (x, Some c, r)
      else member_less_comp (x, m, l)
  ;;

  let member (x, t) = member_less_comp (x, None, t)

  let rec insert = function
    | x, E -> T (E, x, E)
    | x, (T (l, c, r) as s) ->
      if E.lt (x, c)
      then T (insert (x, l), c, r)
      else if E.lt (c, x)
      then T (l, c, insert (x, r))
      else s
  ;;
end

module Ordered_Int : Ordered with type t = int = struct
  type t = int

  let eq (x, y) = x = y
  let lt (x, y) = x < y
  let leq (x, y) = x <= y
end

module EO : Set with type elem = int = Exercise2_2 (Ordered_Int)

let%test _ =
  let t1 = EO.insert (4, EO.empty) in
  let t2 = EO.insert (5, t1) in
  let t3 = EO.insert (1, t2) in
  let t4 = EO.insert (10, t3) in
  let t5 = EO.insert (3, t4) in
  EO.member (4, t5)
  && EO.member (1, t5)
  && EO.member (3, t5)
  && EO.member (10, t5)
  && EO.member (5, t5)
  && EO.member (0, t5) = false
;;
