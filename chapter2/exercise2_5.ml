type elem = int

type tree =
  | E
  | T of (tree * elem * tree)

exception InvalidTreeHeight

let rec complete = function
  | (_, 0) -> E
  | (x, d) ->
     let t = complete (x, d - 1) in
     T (t, x, t)

(* (m, m + 1) *)
let create2 m =
  let l = complete (m - 1, m - 1) in
  (l, T (l, m, l))

let create_bal = function
  | 0 -> E
  | 1 -> T (E, 1, E)
  | n when n < 0 -> raise InvalidTreeHeight
  | n ->
  let (m, m_plus_1) = create2 (n - 1) in
  T (m, n, m_plus_1)

let rec height = function
  | E -> 0
  | T (l, _, r) ->
     let h_l = height l in
     let h_r = height r in
     assert ((h_r - h_l) < 2);
     (max h_l h_r) + 1

let%test _ =
  let t1 = create_bal 0 in
  t1 = E

let%test _ =
  let t2 = create_bal 1 in
  t2 = T (E, 1, E)

let%test _ =
  let t3 = create_bal 3 in
  let h3 = height t3 in
  let t4 = create_bal 15 in
  let h4 = height t4 in
  h3 = 3 && h4 = 15

