include Heap_intf

module Exercise3_4(E : ORDERED) = struct
  module Elem = E

  type heap =
    | E
    | T of int * int * Elem.t * heap * heap

  let weight = function
    | E -> 0
    | T(l, r, _, _, _) -> 1 + l + r
  
  (* Run merge with one top down pass *)
  (* In a lazy execution model, you can have heap nodes with branches
where the merge hasn't run yet.*)
  (* In a concurrent environment, you can traverse the tree and execute the merge functions in a seperate thread and accumulate the results without having to maintain state between threads *)
  let rec merge = function
    | h, E -> h
    | E, h -> h
    | T(_, _, x, _, _) as h1, (T(_, _, y, _, _) as h2) ->
       if E.leq(x, y) then
         check_weight_and_build x h1 h2
       else
         check_weight_and_build y h2 h1
  and check_weight_and_build elem (T(lsize, rsize, _, l, r)) h2 =
    let merged_weight = rsize + weight h2 in
    if lsize >= merged_weight then
      T(lsize, merged_weight, elem, l, merge(r, h2))
    else
      T(merged_weight, lsize, elem, merge(r, h2), l)
end
