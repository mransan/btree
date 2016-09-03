(** Btree implementation using the [Bytes] module for disk storage. 
    
    For testing only as it requires only in memory setup.
  *) 

module Make (Key:Btree.Key_sig) (Val:Btree.Val_sig) : sig 

  type t 

  val make : m:int -> unit -> t 

  val insert : t -> Key.t -> Val.t -> t 

  val append : t -> Key.t -> Val.t -> t 

  val find : t -> Key.t -> Val.t option

  val find_gt : t -> Key.t -> Val.t list 

  val debug : t -> unit 

  module Stats : sig 
    val reset : t -> unit 
    val read_count : t -> int 
    val write_count : t -> int 
    val node_length : t -> int 
    val storage_length : t -> int
  end 

end
