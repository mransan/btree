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

(** Module signature for the key type of the B-Tree *)
module type Key_sig = sig

  type t 

  include Dbtlk_types.Fixed_size_sig with type t := t 
  include Dbtlk_types.Comparable_sig with type t := t 
  include Dbtlk_types.Debug_sig with type t := t 

end (* Key *) 

(** Module signature for the value type of the B-Tree *)
module type Val_sig = sig 
  type t 

  include Dbtlk_types.Fixed_size_sig with type t := t 

end (* Val *) 


module Make (Key:Key_sig) (Val:Val_sig) : sig 

  type t = {
    root_file_offset : Dbtlk_types.file_offset;
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
   
  val make : root_file_offset:Dbtlk_types.file_offset -> m:int -> unit -> t 

  val initialize : t -> Dbtlk_types.write_op  
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

  type insert_res_data = 
    | Insert_res_done of (Dbtlk_types.file_offset option * Dbtlk_types.write_op list)  
      (** [Insert_res_done (Some root_offset, write_ops], the key/value was 
          successfully inserted. [root_offset] is the new root offset of the 
          tree which should then be used in subsequent inserts/find/debug while 
          [write_ops] are the write operation necessary for the inserts to 
          take effect. *)
    | Insert_res_node_split of (Key.t * Val.t * int * Dbtlk_types.write_op list) 
      (** Internal, should never be returned *)
  
  type insert_res = insert_res_data Dbtlk_types.res  

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
  
  type find_res_data = Val.t option 
  (** find result type. If [None] then no value was found. *) 

  type find_res = find_res_data Dbtlk_types.res 

  val find : t -> Key.t -> find_res 
  (** [find root_node key] searches for the [key] in the B-Tree starting at 
      the [root_node] *)

  type find_gt_res = Val.t list Dbtlk_types.res  

  val find_gt : t -> Key.t -> int -> find_gt_res 
  (** [find_gt t key max] finds at most [max] values which are greater than [key] in 
      the tree.
    *)

  (** {2 Debugging} *)

  type debug_res = unit Dbtlk_types.res  

  val debug : t -> debug_res  
  (** [debug root_node] pretty-prints to stdout the B-Tree starting at
      [root_node] *)

  (** {2 Iteration} *)

  type iter_res = unit Dbtlk_types.res 

  val iter : t -> (Val.t -> unit) -> iter_res 
  (** [iter root_node f] iterates over the B-Tree and applies [f] for each value 
      in the tree.  *)

  type last_res = ((Key.t * Val.t) option) Dbtlk_types.res 

  val last : t -> last_res

end (* Make *)
