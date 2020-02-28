include Bst_intf

module Exercise2_3 (E : Ordered) : Set with type elem = E.t = struct
  type elem = E.t

  type tree =
    | E
    | T of tree * elem * tree

  type set = tree

  exception Duplicate of string

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

  (* Raises duplicate exception *)
  let rec insert_exn = function
    | x, E -> T (E, x, E)
    | x, T (l, c, r) ->
      if E.lt (x, c)
      then T (insert_exn (x, l), c, r)
      else if E.lt (c, x)
      then T (l, c, insert_exn (x, r))
      else raise @@ Duplicate "Element already exists"
  ;;

  let insert (x, t) =
    try insert_exn (x, t) with
    | Duplicate error_msg ->
      print_endline error_msg;
      t
  ;;
end

module Ordered_Int : Ordered with type t = int = struct
  type t = int

  let eq (x, y) = x = y
  let lt (x, y) = x < y
  let leq (x, y) = x <= y
end

module EO : Set with type elem = int = Exercise2_3 (Ordered_Int)
