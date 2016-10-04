(** Btree implementation using the [Unix] module for disk operations 
  *) 

val do_read_op : Unix.file_descr -> Types.read_op -> bytes 

val do_write_ops : Unix.file_descr -> Types.write_op list -> unit 

val do_res : Unix.file_descr -> 'a Types.res -> 'a 

module Make (Key:Btree.Key_sig) (Val:Btree.Val_sig) : sig 

  type t 

  val make : filename:string -> m:int -> unit -> t 

  val insert : t -> Key.t -> Val.t -> t 

  val append : t -> Key.t -> Val.t -> t 

  val find : t -> Key.t -> Val.t option

  val debug : t -> unit 

  val iter : t -> (Val.t -> unit) -> unit
end
