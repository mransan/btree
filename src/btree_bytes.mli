(** Btree implementation using the [Bytes] module for disk storage. 
    
    For testing only as it requires only in memory setup.
  *) 

module Make (Key:Btree.Key_sig) (Val:Btree.Val_sig) : sig 

  type t 

  val make : m:int -> unit -> t 

  val insert : t -> Key.t -> Val.t -> t 

  val find : t -> Key.t -> Val.t option

  val debug : t -> unit 

end
