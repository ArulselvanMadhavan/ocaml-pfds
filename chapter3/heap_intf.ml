module type ORDERED = sig
  type t

  val eq : t * t -> bool
  val lt : t * t -> bool
  val leq : t * t -> bool
end

exception Empty

module type HEAP = sig
  module Elem : ORDERED

  type heap

  val empty : heap
  val is_empty : heap -> bool
  val insert : Elem.t * heap -> heap
  val merge : heap * heap -> heap

  (* raises Empty if heap is empty *)
  val find_min : heap -> Elem.t

  (* raises Empty if heap is empty *)
  val delete_min : heap -> heap
end
