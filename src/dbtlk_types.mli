(** Module type for exposing a [to_string] conversion used 
    for debugging purposes only.  *)
module type Debug_sig = sig
  type t 

  val to_string : t -> string 
  (** [to_string t] returns a string for debugging purposes only *)
end 

(** Module signature for types which can be encoded in a fixed size array
    of bytes. 

    This module is crucial as this particular B-Tree implementation is based on 
    the assumption that each piece of data (key/values in particular) has a 
    fixed size on disk.  *)
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

end (* Fixed_size_sig *)

(** Module signature for totally ordering values of a given type.  *)
module type Comparable_sig = sig 

  type t 
  (** type *)

  val compare : t -> t -> int
  (** comparaison function *)

end (* Comparable_sig *)  

type file_offset = int

type block_length = int 
(** Length of a file block *)

type block = {
  offset: file_offset; 
  length: block_length;
}
(** File block *)

val string_of_block : block -> string 
(** [string_of_block block] returns a pretty-print string of [block] *)

type read_op = block 
(** Read operation *)

type write_op = {
  offset: file_offset; 
  bytes : bytes;
}
(** Write operation : write [bytes] starting at [offset] in the file *)
  
type 'a res = 
  | Res_done of 'a 
    (** Computation is done and the result returned *)
  | Res_read_data of block * 'a res_read_data_k 
    (** Reading a block of data is required *)
  | Res_allocate of block_length * 'a res_allocate_k 
    (** Allocating a new block of data is required *)

and 'a res_read_data_k = bytes -> 'a res 
  (** Continuation function after reading a block of data *)

and 'a res_allocate_k = file_offset -> 'a res 
  (** Continuation function after allocating a block of data *)

val res_done : 'a -> 'a res 

val res_read_data : block -> 'a res_read_data_k -> 'a res 

val res_allocate : block_length -> 'a res_allocate_k -> 'a res 

val res_bind : ('a -> 'b res) -> 'a res -> 'b res 

val res_map : ('a -> 'b) -> 'a res -> 'b res  
 
