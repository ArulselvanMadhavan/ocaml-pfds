include Leftist_heap

module Exercise3_3 (E : ORDERED) = struct
  module H = Leftist_heap (E)
  include H

  let from_list xs =
    let rec hlist_to_hlist = function
      | [] -> []
      | [ h1 ] -> [ h1 ]
      | h1 :: h2 :: hs -> H.merge (h1, h2) :: hlist_to_hlist hs
    and helper = function
      | [] -> H.empty
      | [ h1 ] -> h1
      | hs -> helper (hlist_to_hlist hs)
    in
    helper (List.map (fun x -> H.T (1, x, E, E)) xs)
  ;;
end

module Int_Ordered : ORDERED with type t = int = struct
  type t = int

  let eq (a, b) = a = b
  let lt (a, b) = a < b
  let leq (a, b) = a <= b
end

module E = Exercise3_3 (Int_Ordered)

let%test _ =
  let h1 = E.from_list [ 1; 4; 9; 2; 7; 5 ] in
  let h2 = E.delete_min h1 in
  let h3 = E.delete_min h2 in
  let h4 = E.delete_min h3 in
  E.find_min h1 = 1
  && E.find_min h2 = 2
  && E.find_min h3 = 4
  && E.find_min h4 = 5
;;
