(** Utility module to encode integer value with little endian 32 bit 
    encoding.

    It also implements the [Fixed_size_sig] module signature which is 
    later defined.
 *)
module Int = struct
  let byte pos bytes = 
    int_of_char (Bytes.unsafe_get bytes pos)
  
  type t = int

  let length = 4

  let to_string = string_of_int 
  
  let of_bytes bytes pos =
    let b1 = byte (pos + 0) bytes in
    let b2 = byte (pos + 1) bytes in
    let b3 = byte (pos + 2) bytes in
    let b4 = byte (pos + 3) bytes in
    Int32.(to_int @@ add (shift_left (of_int b4) 24)
           (add (shift_left (of_int b3) 16)
            (add (shift_left (of_int b2) 8)
             (of_int b1))))

  let to_bytes i bytes pos =
    let i = Int32.of_int i in
    Bytes.unsafe_set 
      bytes (pos + 0) (char_of_int Int32.(to_int (logand 0xffl i)));
    Bytes.unsafe_set 
      bytes (pos + 1) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 8))));
    Bytes.unsafe_set 
      bytes (pos + 2) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 16))));
    Bytes.unsafe_set 
      bytes (pos + 3) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 24))))
end 


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

(** Module signature for totally ordering values of a given type. 
 *)
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

(** File block *)
type block = {
  offset: int; 
  length: int;
}

let string_of_block {offset; length} = 
  Printf.sprintf "{offset: %i, length: %i}" offset length

module Array_or_bytes(FS:Fixed_size_sig) = struct 

  (** {2 Type} *)

  type t = 
    | Array_as_bytes of int * bytes 
    | Array of FS.t array
    | Array_on_disk of block 

  (** {2 Constructors} *)

  let bytes ~n ~bytes () = Array_as_bytes (n, bytes) 

  let array a = Array a
  
  let on_disk ~offset ~length () = Array_on_disk {offset;length;} 

  (** {2 Conversions} *)

  let to_array_from_bytes n bytes = 
      assert(n > 0); 
      let fst = FS.of_bytes bytes 0 in 
      let a = Array.make n fst in 
      for i = 1 to (n - 1) do 
        Array.unsafe_set a i (FS.of_bytes bytes (i * FS.length))
      done; 
      a

  type to_array_result = 
    | To_array_on_disk of block  
    | To_array_val of FS.t array 

  let to_array = function
    | Array a -> To_array_val a
    | Array_as_bytes (n, bytes) -> To_array_val (to_array_from_bytes n bytes) 
    | Array_on_disk block -> To_array_on_disk block 

  type to_bytes_result = 
    | To_bytes_on_disk of block  
    | To_bytes_val of bytes
  
  (** [to_bytes] should be more of the form [to_bytes t bytes pos] *)
  let to_bytes = function
    | Array a -> 
      let array_len = Array.length a in 
      let bytes_len = FS.length * array_len in 
      let bytes = Bytes.create bytes_len in 

      let rec aux offset = function 
        | i when i = array_len -> To_bytes_val bytes
        | i -> 
          begin 
            FS.to_bytes (Array.get a i) bytes offset; 
            aux (offset + FS.length) (i + 1) 
          end
      in 
      aux 0 0

    | Array_as_bytes (_, bytes) -> To_bytes_val bytes 
    | Array_on_disk block -> To_bytes_on_disk block

end

type op = 
  | Read of block
  | Write_bytes of block * bytes  

module Make (Key:Key_sig) (Val:Val_sig) = struct  

  type invariant_violation = 
    | K_can_never_be_zero 
    | Subtrees_access_in_leaf_node

  let string_of_invariant_violation = function
    | K_can_never_be_zero -> "[k] can never be zero" 
    | Subtrees_access_in_leaf_node -> "[subtrees] are empty in leaf node"

  exception Failure of invariant_violation

  let () = 
    Printexc.register_printer (function 
      | Failure iv -> Some (string_of_invariant_violation iv)
      | _ -> None 
    ) 

  let k_can_never_be_zero () = 
    raise (Failure K_can_never_be_zero) 

  let subtrees_access_in_leaf_node () = 
    raise (Failure Subtrees_access_in_leaf_node)

  module Keys = Array_or_bytes(Key) 
  module Vals = Array_or_bytes(Val)
  module Ints = Array_or_bytes(Int)

  type node = {

    offset : int; 
      (* offset in the database file, unique id *)

    k: int; 
      (* *)

    m: int;
      (* dimension of the btree node (maximum number of 
       * subtree) *)

    keys: Keys.t; 
      (* [k-1] keys encoded. 
         However bytes length is based on the largest number of keys 
         a node can hold:
         [ (m-1) * Key.length ] *)

    vals : Vals.t; 
      (* [k-1] data encoded. 
         However bytes length is based on the largest number of keys 
         a node can hold:
         [ (m-1) * Val.length ] *)

    subtrees: Ints.t;
       (* k substrees binary encoded. 
          
          A subtree is simply a file offset [int] and is encoded using 4 bytes.  *)
  }
  (** B-Tree node which binary layout has the following:
     
      |--k--|--keys--|--vals--|--subtrees--|

      [lengh = 4 + ((m-1) * Key.length) + ((m-1) * Val.length) + (m * 4)] 
    *)
  

  let is_leaf k = 
    (* we use the sign of k to determine whether the node is a
       leaf node or an intermediate node *)
    k < 0 

  let nb_of_vals = Pervasives.abs  

  let nb_of_subtrees k = 
    if k < 0 
    then subtrees_access_in_leaf_node ()
    else k + 1 

  let keys_offset offset _ = 
    offset + Int.length

  let vals_offset offset m  =
    keys_offset offset m + ((m - 1) * Key.length)

  let subtrees_offset offset m = 
    vals_offset offset m + ((m - 1) * Val.length)

  let keys_length m = 
    (m - 1) * Key.length

  let vals_length m = 
    (m - 1) * Val.length 

  let subtrees_length m = 
    m * Int.length

  let keys_block offset m = {
    offset = keys_offset offset m;
    length = keys_length m; 
  }
  
  let vals_block offset m = {
    offset = vals_offset offset m;
    length = vals_length m; 
  }
  
  let subtrees_block offset m = {
    offset = subtrees_offset offset m;
    length = subtrees_length m; 
  }

  let node_length_of_m m = 
    Int.length + (keys_length m) + (vals_length m) + (subtrees_length m) 

  let node_length {m; _} = 
     node_length_of_m m  

  type make_result = 
    | Make_result_node of node 
    | Make_result_invalid_number_of_values
    | Make_result_inconsistent_vals_keys 

  let make_leaf_node ?keys ?vals ~offset ~nb_of_vals ~m () = 
    if nb_of_vals > m - 1 || nb_of_vals < 1  
    then Make_result_invalid_number_of_values
    else 
      let k = - nb_of_vals in  

      let subtrees = 
        Ints.on_disk ~offset:(subtrees_offset offset m) ~length:(subtrees_length m) ()
      in 

      let make keys vals = 
        Make_result_node {m;offset;k;keys;vals;subtrees}
      in

      match keys, vals with
      | None, None -> 
        let keys = 
          Keys.on_disk ~offset:(keys_offset offset m) ~length:(keys_length m) () 
        in
        let vals = 
          Vals.on_disk ~offset:(vals_offset offset m) ~length:(vals_length m) () 
        in 
        make keys vals 

      | Some keys, Some vals -> 
        if Array.length keys <> Array.length vals 
        then Make_result_inconsistent_vals_keys 
        else 
          let keys = Keys.array keys in 
          let vals = Vals.array vals in 
          make keys vals

      | _ -> Make_result_inconsistent_vals_keys

  let make_intermediate_node ?keys ?vals ?subtrees ~offset ~nb_of_vals ~m () = 
    if nb_of_vals > m - 1 || nb_of_vals < 1  
    then Make_result_invalid_number_of_values
    else 
      let k = nb_of_vals in 
      let make keys vals subtrees = 
        Make_result_node {m;k;offset;keys;vals;subtrees}
      in 
      match keys, vals, subtrees with
      | None, None, None -> 
        let keys = 
          Keys.on_disk ~offset:(keys_offset offset m) ~length:(keys_length m) () 
        in
        let vals = 
          Vals.on_disk ~offset:(vals_offset offset m) ~length:(vals_length m) () 
        in 
        let subtrees = 
          Ints.on_disk ~offset:(subtrees_offset offset m) ~length:(subtrees_length m) () 
        in 
        make keys vals subtrees
      | Some keys, Some vals, Some subtrees -> 
        if Array.length keys <> Array.length vals || 
           Array.length keys <> (Array.length subtrees - 1)
        then 
          Make_result_inconsistent_vals_keys
        else 
          let keys = Keys.array keys in 
          let vals = Vals.array vals in 
          let subtrees = Ints.array subtrees in 
          make keys vals subtrees
      | _ -> Make_result_inconsistent_vals_keys

  let make_disk_node ~k ~m ~offset () = 
    let keys = 
      Keys.on_disk ~offset:(keys_offset offset m) ~length:(keys_length m) () 
    in
    let vals = 
      Vals.on_disk ~offset:(vals_offset offset m) ~length:(vals_length m) () 
    in 
    let subtrees = 
      Ints.on_disk ~offset:(subtrees_offset offset m) ~length:(subtrees_length m) () 
    in 
    {k;m;offset;keys;vals;subtrees;}

  let full_write {offset; k; m; keys; vals; subtrees; } = 
    let writes = [] in
    let writes = 
      (* K *)
      let b = Bytes.create Int.length in 
      Int.to_bytes k b 0;
      ({offset; length = Int.length}, b) ::  writes  
    in 
    
    let writes = 
      match Keys.to_bytes keys with
      | Keys.To_bytes_on_disk _ -> writes 
      | Keys.To_bytes_val b -> 
        (keys_block offset m, b)::writes
    in
    
    let writes = 
      match Vals.to_bytes vals with
      | Vals.To_bytes_on_disk _ -> writes 
      | Vals.To_bytes_val b -> 
        (vals_block offset m, b)::writes
    in

    let writes = 
      match Ints.to_bytes subtrees with
      | Ints.To_bytes_on_disk _ -> writes 
      | Ints.To_bytes_val b -> 
        (subtrees_block offset m, b)::writes
    in

    List.rev writes 

  type find_res = 
    | Find_res_not_found 
    | Find_res_val of Val.t 
    | Find_res_read_data of block * find_res_continuation  

  and find_res_continuation = bytes -> find_res 

  let rec find node key = 
    let {k; keys; _} = node in 
    
    match Keys.to_array keys with
    | Keys.To_array_on_disk block -> 
      let continutation = fun bytes -> 
        let node = {node with keys = Keys.bytes ~n:(nb_of_vals k) ~bytes ()} in 
        find node key 
      in 
      Find_res_read_data (block, continutation) 

    | Keys.To_array_val keys_array -> 
      if is_leaf k
      then (* this is a leaf node *)
        find_leaf_node node key keys_array
      else 
        find_leaf_internal_node node key keys_array

  and find_leaf_internal_node ({k; _ } as node) key keys_array = 
    let nb_of_keys = nb_of_vals k in 
    let rec aux = function
      | i when i = nb_of_keys -> find_in_subtree node key i 
      | i -> 
        let key' = Array.get keys_array i in
        match Key.compare key' key with
        | 0 ->  return_val_at node i 
        | c when c >  0 -> find_in_subtree node key i 
        | _ -> aux (i + 1)
    in  
    aux 0

  and find_in_subtree ({subtrees; k; m; _} as node) key subtree_i =  
    match Ints.to_array subtrees with
    | Ints.To_array_on_disk block -> 
      let continutation = fun bytes -> 
         let subtree_array = Ints.to_array_from_bytes (nb_of_subtrees k) bytes in 
         let node = {node with subtrees = Ints.array subtree_array} in 
         find_in_subtree node key subtree_i
      in 
      Find_res_read_data (block, continutation)

    | Ints.To_array_val (subtree_a :int array)-> 

      let subtree_offset = Array.get subtree_a subtree_i in 
      let continutation = fun bytes -> 
        let k = Int.of_bytes bytes 0 in 
        let node = make_disk_node ~k ~m ~offset:subtree_offset () in 
        find node key 
      in 

      Find_res_read_data (
        {offset  = subtree_offset; length = Int.length},
        continutation
      ) 
  
  and find_leaf_node node key keys_array =  
    let nb_of_keys = Array.length keys_array in 
      (* check invariant with [node.k] *)

    let rec aux = function
      | i when i = nb_of_keys -> 
        Find_res_not_found
        (* key of the leaf node is not matching, the key is 
         * therefore not found. *)

      | i -> 
        let key' = Array.get keys_array i in 
        Printf.printf "In leaf node, key: %s, key': %s\n" 
          (Key.to_string key) (Key.to_string key');
        if key = key' 
        then return_val_at node i 
        else aux (i + 1) 
    in
    aux 0
  
  and return_val_at {vals;k; _} i = 
    match Vals.to_array vals with
    | Vals.To_array_on_disk block -> 
      (* The values data was not read from disk, let's make sure
       * this is done first *)
      let continuation = fun bytes -> 
        let vals_array = Vals.to_array_from_bytes (nb_of_vals k) bytes in 
        Find_res_val (Array.get vals_array i) 
        (* Note we could have recursively called this [find] function
         * but it would have been ineficient and not that much more 
         * elegant. *)
      in 
      Find_res_read_data (block, continuation) 

    | Vals.To_array_val a -> 
      (* Values are already read from disk, simply return it *)
      Find_res_val (Array.get a i)

end 
