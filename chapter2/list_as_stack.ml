include Stack_intf

module List_as_stack : Stack = struct
  type 'a t = 'a list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ :: _ -> false
  ;;

  let cons (x, xs) = x :: xs
  let head = List.hd
  let tail = List.tl
end
