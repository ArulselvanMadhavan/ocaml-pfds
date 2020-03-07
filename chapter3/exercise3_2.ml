include Leftist_heap

module Exercise3_2 (E : ORDERED with type t = int) :
  HEAP with type Elem.t = int = struct
  module H = Leftist_heap (E)
  include H

  let rec insert = function
    | x, H.E -> H.T (1, x, E, E)
    | x, T (r, y, a, b) ->
      if E.leq (x, y)
      then T (r + 1, x, a, insert (y, b))
      else T (r + 1, y, a, insert (x, b))
  ;;
end

module Int_Ordered : ORDERED with type t = int = struct
  type t = int

  let eq (a, b) = a = b
  let lt (a, b) = a < b
  let leq (a, b) = a <= b
end

module E = Exercise3_2 (Int_Ordered)

let%test _ =
  let h1 = E.insert (9, E.empty) in
  let h2 = E.insert (4, h1) in
  let h3 = E.insert (10, h2) in
  let h4 = E.insert (3, h3) in
  let h5 = E.delete_min h4 in
  let h6 = E.delete_min h5 in
  E.find_min h1 = 9
  && E.find_min h2 = 4
  && E.find_min h3 = 4
  && E.find_min h4 = 3
  && E.find_min h5 = 4
  && E.find_min h6 = 9
;;
