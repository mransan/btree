(** B-Tree for fixed size key and value 

    This module implement a B-Tree for fixed sized key and fixed size
    value. In other word the size (in bytes) of both the key and the 
    value is known in advanced. 

    This implementation does not make any assumption about how the following
    disk operation are implemented:
    {ul
    {- Reads} 
    {- Writes}
    {- Allocation of new storage}
    } 

    The implementation of those operations are delegated to the client code
    through continuation functions. 
    This B-Tree should therefore never be used directly by application code 
    but it should rather be used as the core implementation of an opinionated
    B-Tree which implements the actual disk operation. 
  *)

(** Module signature for types which can be encoded in a fixed size array
    of bytes. 

    This module is crucial as the B-Tree implementation that all the pieces
    of data can be encoded in a fixed size so that the location on disk of 
    element of the tree can be computed efficiently.
 *)
module type Fixed_size_sig = sig 

  type t 
  (** type *)

  val length : int 
  (** length in bytes of the encoding key [t] *)

  val to_bytes : t -> bytes -> int -> unit  
  (** [to_bytes key bytes pos] encodes the [key] in [bytes] starting 
      at [pos].

      Invariants:

      {ul 
      {- [bytes] can be of at least length: [pos + length]} 
      {- [to_bytes] should only modify [bytes] in the interval [pos; pos+length[ }
      } *)

  val of_bytes : bytes -> int -> t 
  (** [of_bytes] decodes a key value of type [t] from [bytes] starting at [pos].
      
      Invariants:

      {ul 
      {- [bytes] can be of at least length: [pos + length]} 
      {- [to_bytes] should only read [bytes] in the interval [pos; pos+length[ }
      } *)

  val to_string : t -> string 
  (** debug *)

end (* Fixed_size_sig *)

(** Module signature for totally ordering values of a given type.  *)
module type Comparable_sig = sig 

  type t 
  (** type *)

  val compare : t -> t -> int
  (** comparaison function *)

end (* Comparable_sig *)  


(** Module signature for the key type of the B-Tree *)
module type Key_sig = sig

  type t 

  include Fixed_size_sig with type t := t 
  include Comparable_sig with type t := t 

end (* Key *) 

(** Module signature for the value type of the B-Tree *)
module type Val_sig = sig 
  type t 

  include Fixed_size_sig with type t := t 

end (* Val *) 

module Typed_bytes(FS:Fixed_size_sig) : sig 

  type t 

  val make : offset:int -> bytes:bytes -> unit -> t 

  val get : t -> int -> FS.t 
  val unsafe_get : t -> int -> FS.t 
  
  val set : t -> int -> FS.t -> unit  
  val unsafe_set : t -> int -> FS.t -> unit 

  val get_n : t -> int -> FS.t array 
  val unsafe_get_n : t -> int -> FS.t array 

  val set_n : t -> FS.t array -> unit 
  val unsafe_set_n : t -> FS.t array -> unit 

  val insert : t -> int -> int -> FS.t -> unit 
  val unsafe_insert : t -> int -> int -> FS.t -> unit 

  val blit : t -> int -> t -> int -> int -> unit 

end 

(** File block *)
type block = {
  offset: int; 
  length: int;
}

val string_of_block : block -> string 

type read_op = block 

type write_op = {
  offset: int; 
  bytes : bytes;
}

module Make (Key:Key_sig) (Val:Val_sig) : sig 

  type node_on_disk 

  val make_on_disk : offset:int -> m:int -> unit -> node_on_disk 

  val node_length_of_m : int -> int 
  
  type insert_res = 
    | Insert_res_done of (int option * write_op list)  
    | Insert_res_read_data of block * insert_res_read_data_continuation  
    | Insert_res_allocate of int * insert_res_allocate_continuation 
    | Insert_res_node_split of (Key.t * Val.t * int * write_op list) 

  and insert_res_read_data_continuation = bytes -> insert_res  

  and insert_res_allocate_continuation = int -> insert_res 

  val insert : ?is_root:bool -> node_on_disk -> Key.t -> Val.t -> insert_res
  
  type find_res = 
    | Find_res_not_found 
    | Find_res_val of Val.t 
    | Find_res_read_data of block * find_res_continuation  

  and find_res_continuation = bytes -> find_res 

  val find : node_on_disk -> Key.t -> find_res 

  type debug_res = 
    | Debug_res_read_data of block * debug_res_continuation 
    | Debug_res_done  

  and debug_res_continuation = bytes -> debug_res  

  val debug : ?indent:int -> node_on_disk -> debug_res 

  (** {2 Testing} *) 

  val write_leaf_node : 
    keys:Key.t array ->
    vals:Val.t array ->
    offset:int -> 
    m:int -> 
    unit ->
    write_op 
  
  val write_intermediate_node : 
    keys:Key.t array ->
    vals:Val.t array ->
    subs:int array ->
    offset:int -> 
    m:int -> 
    unit ->
    write_op 
end
