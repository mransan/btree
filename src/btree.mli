(** B-Tree for fixed size key and value 

    This module implement a B-Tree for fixed sized key and fixed size
    value. In other word the size (in bytes) of both the key and the 
    value is known in advanced and is constant.

    The B-Tree is assumed to be entirely stored in a single continuous storage. 
    That storage can be a file of course but not required. In fact the module 
    [Btree_bytes] implement the B-Tree storage in an in-memory byte array. 
    The storage is not required to be entirely dedicated to storing the B-Tree; the 
    caller application is responsible to allocate new B-Tree node in the 
    storage and make sure no other part of the application will override
    that data.

    This implementation does not make any assumption about how the following
    disk operation are implemented; they are delegated to the caller 
    through the returned type of each module function:
    {ul
    {- Reads} 
    {- Writes}
    {- Allocation of new storage}
    } 

    This B-Tree should therefore never be used directly by application code 
    but it should rather be used as the core implementation of an opinionated
    B-Tree which implements the actual disk operation. 
  
    One can look at 2 of such implementation:
    {ul
    {- [Btree_bytes]: an in-memory byte array is used as storage. This is
       used for unit testing without having to write to a file. }
    {- [Btree_unix]: uses the [Unix] module part of the OCaml distribution 
       to perform all the file operations.}
    } 
  *)

(** Module signature for types which can be encoded in a fixed size array
    of bytes. 

    This module is crucial as this particular B-Tree implementation is based on 
    the assumption that each piece of data (key/values in particular) has a 
    fixed size on disk. 
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

module Make (Key:Key_sig) (Val:Val_sig) : sig 

  type t = {
    root_file_offset : file_offset;
    m : int;
  }
  (** A Binary tree is defined by the file offset of the root
      node as well as [m] the maximum number of subtree in a node 
      (with m-1 being the maximum value in a Btree node). 

      Both [root_file_offset] and [m] must be persisted by the caller
      application. 

      [m] cannot change once a node has been initialized. It is the 
      responsability of the caller to maintain that invariant. 

      [root_file_offset] initial value is defined by the caller application
      when calling [initialize]. [root_file_offset] is not constant and 
      as the BTree grows a new root node might be created and 
      [root_file_offset] will have a new value as defined in the 
      return by the following returned value of [insert]: 
      [Insert_res_done (Some new_root_file_offset), write_ops]. The calling
      application is responsible to persist this new value so that
      subsquent operation on the BTree succeeed. 

    *)
   
  val make : root_file_offset:file_offset -> m:int -> unit -> t 

  val initialize : t -> write_op  
  (** [initialize node] return [(length, write_op)]. [length] indicates 
    * the initial diskspace requires, and [write_op] the write operation 
    * to perform. 
    *)

  val node_length_of_m : int -> int 
  (** [node_length_of_m m] return the length in byte of a BTree node. 
      
      This function should only be used for information and is not crucial
      to interfacing with the BTree. 
    *)

  (** {2 Insertion} *)
  
  type insert_res = 
    | Insert_res_done of (file_offset option * write_op list)  
      (** [Insert_res_done (Some root_offset, write_ops], the key/value was 
          successfully inserted. [root_offset] is the new root offset of the 
          tree which should then be used in subsequent inserts/find/debug while 
          [write_ops] are the write operation necessary for the inserts to 
          take effect. *)

    | Insert_res_read_data of block * insert_res_read_data_continuation  
      (** Read a block of storage *)

    | Insert_res_allocate of block_length * insert_res_allocate_continuation 
      (** Allocate a new block of storage of given length *)

    | Insert_res_node_split of (Key.t * Val.t * int * write_op list) 
      (** Internal, should never be returned *)

  and insert_res_read_data_continuation = bytes -> insert_res  
    (** Continuation function which takes the bytes read from the read 
        operation *)

  and insert_res_allocate_continuation = file_offset -> insert_res 
    (** Continuation function which takes the storage offset allocated
        for a new node *)

  val insert : t -> Key.t -> Val.t -> insert_res
  (** [insert root_node key value] inserts the [key]/[value] pair in the 
      B-Tree. If [key] is found in the tree, then [value] replaces the previous
      value. *)

  val append : t -> Key.t -> Val.t -> insert_res
  (** [append root_node key value] appends the [key]/[value] pair in the 
      B-Tree assuming that the key is: 
      {ul
      {- Not in the Btree [t]}
      {- Greater than any of the previously appended/inserted keys}
      } 

      The caller is responsible to respect the invariant above.
      
      This method allows faster inserts when the keys are sequentially
      inserted.
     *)

  (** {2 Search} *)
  
  type find_res = 
    | Find_res_not_found 
      (** The given [key] was not found in the B-Tree *)

    | Find_res_val of Val.t 
      (** The given [key] was found in the B-Tree and the associated
          [value] is returned *)

    | Find_res_read_data of block * find_res_continuation  
      (** Read a block of storage *)

  and find_res_continuation = bytes -> find_res 
    (** Continuation function which takes the bytes read from the read
        operation *)

  val find : t -> Key.t -> find_res 
  (** [find root_node key] searches for the [key] in the B-Tree starting at 
      the [root_node] *)

  type find_gt_res = 
    | Find_gt_res_values of Val.t list 
      (** [Find_gt_res_values values] a non deterministic number 
          of values which are found to be greater than the given 
          key *)
    | Find_gt_res_read_data of block * find_gt_res_continuation  
      (** Read a block of storage *)

  and find_gt_res_continuation = bytes -> find_gt_res  
    (** Continuation function which takes the bytes read from the
        read operation *)

  val find_gt : t -> Key.t -> find_gt_res 
  (** [find_gt t key] finds values which are greater than [key] in 
      the tree. The number of returned values is undefined however if there is 
      at least one value greater than [key] it will be returned. 

      The number of values returned is maximized for a minimum number of read. 
    *)

  (** {2 Debugging} *)

  type debug_res = 
    | Debug_res_done  
      (** Debugging is done executing *)

    | Debug_res_read_data of block * debug_res_continuation 
      (** Read a block of storage *)

  and debug_res_continuation = bytes -> debug_res  

  val debug : t -> debug_res  
  (** [debug root_node] pretty-prints to stdout the B-Tree starting at
      [root_node] *)

end (* Make *)
