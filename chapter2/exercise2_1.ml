let rec suffixes = function
  | [] -> [ [] ]
  | _ :: xs as s -> s :: suffixes xs
;;

let%test _ =
  suffixes [ 1; 2; 3; 4 ]
  = [ [ 1; 2; 3; 4 ]; [ 2; 3; 4 ]; [ 3; 4 ]; [ 4 ]; [] ]
;;
