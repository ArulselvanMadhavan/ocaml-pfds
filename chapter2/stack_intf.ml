module type Stack = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val cons : 'a * 'a t -> 'a t
  (* Raises if empty *)
  val head : 'a t -> 'a
  (* Raises if empty *)
  val tail : 'a t -> 'a t
end
