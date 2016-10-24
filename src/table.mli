
module type Record_sig = sig 

  (** {2 Record type} *) 

  type t 

  val to_bytes : t -> bytes 
  (** [to_bytes t] encodes [t] to bytes *)

  val of_bytes : bytes -> t 
  (** [of_bytes bytes] decodes a value of type [t] from [bytes] *)

  val to_string : t -> string 
  (** [to_string t] returns a string for debugging purposes only *)

  (** {2 Indices} *) 

  module Key0 : Btree.Key_sig 

  val index0 : t -> Key0.t  
  (** [index1 t] access the key1 for a given record *)

  module Key1 : Btree.Key_sig 

  val index1 : t -> Key1.t 
  (** [index1 t] access the key2 for a given record *)

end (* Record_sig *) 

module Make(Record:Record_sig) : sig

  type t 

  val make_empty : unit -> (t * Types.write_op list )
  
  val make_from_file : unit -> t Types.res  

  val insert : t -> Record.t -> Types.write_op list Types.res  

  val debug : t -> unit Types.res 

  val to_string : t -> string 

end 
