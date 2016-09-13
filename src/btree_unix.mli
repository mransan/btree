(** Btree implementation using the [Unix] module for disk operations 
  *) 

module Make (Key:Btree.Key_sig) (Val:Btree.Val_sig) : sig 

  type t 

  val make : filename:string -> m:int -> unit -> t 

  val insert : t -> Key.t -> Val.t -> t 

  val append : t -> Key.t -> Val.t -> t 

  val find : t -> Key.t -> Val.t option

  val debug : t -> unit 

  val iter : t -> (Val.t -> unit) -> unit
end
