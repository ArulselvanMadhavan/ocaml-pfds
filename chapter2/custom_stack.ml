include Stack_intf

module Custom_stack : Stack = struct
  type 'a t =
    | Nil
    | Cons of 'a * 'a t

  exception Empty of string

  let empty = Nil

  let is_empty = function
    | Nil -> true
    | Cons (_, _) -> false
  ;;

  let cons (x, xs) = Cons (x, xs)

  let head = function
    | Nil -> raise @@ Empty "empty stack"
    | Cons (x, _) -> x
  ;;

  let tail = function
    | Nil -> raise @@ Empty "empty stack"
    | Cons (_, xs) -> xs
  ;;
end
