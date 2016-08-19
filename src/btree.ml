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

let cons_option o l = 
  match o with 
  | None -> l 
  | Some x -> x::l 

let array_insert a pos v = 
  let a_len = Array.length a in 
  let new_a_len = a_len + 1 in 
  let new_a = Array.make new_a_len v in 
  Array.blit a 0 new_a 0 pos; 
  Array.blit a pos new_a (pos + 1) (a_len - pos); 
  new_a  

let array_median_split a = 
  let a_len = Array.length a in 
  assert(a_len mod 2 = 1); 
  let median = a_len / 2 in 
  let median_val = Array.get a median in 
  let left = Array.sub a 0 median in 
  let right = Array.sub a (median + 1) median in 
  (median_val, left, right)  

let array_half_split a = 
  let a_len = Array.length a in 
  assert(a_len mod 2 = 0); 
  let half = a_len / 2 in 
  let left = Array.sub a 0 half in 
  let right = Array.sub a half half in 
  (left, right)  


  

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

let make_block ~offset ~length () = {offset; length; } 

let string_of_block {offset; length} = 
  Printf.sprintf "{offset: %i, length: %i}" offset length

type write_op = {
  offset : int; 
  bytes : bytes;
} 

let write_op ~offset ~bytes () = {offset; bytes} 

type read_op = block 

module Typed_bytes(FS:Fixed_size_sig) =  struct 

  type t = {
    offset: int; 
    bytes: bytes; 
  } 

  let make offset bytes = {offset; bytes}

  let unsafe_get {offset; bytes} i = 
    FS.of_bytes bytes (offset + i * FS.length)

  let get ({offset; bytes} as t) i = 
    if Bytes.length bytes < offset + (i + 1) * FS.length ||
       i < 0 
    then invalid_arg "Typed_bytes.get"
    else unsafe_get t i  

  let unsafe_set {offset; bytes} i v = 
    FS.to_bytes v bytes (offset + i * FS.length) 

  let set ({offset; bytes} as t) i v = 
    if Bytes.length bytes < offset + (i + 1) * FS.length || 
       i < 0 
    then invalid_arg "Typed_bytes.set"
    else unsafe_set t i v  
 
  let unsafe_get_n ({offset; bytes} as t) n = 
    if n = 0 
    then [||]
    else 
      let e0 =  FS.of_bytes bytes offset in 
      let a  = Array.make n e0 in 
      let rec aux = function
        | i when i = n -> a 
        | i -> 
          Array.set a i (unsafe_get t i) ; aux (i + 1) 
      in 
      aux 1  

  let get_n ({offset; bytes} as t) n = 
    if Bytes.length bytes < offset + (n) * FS.length ||
       n < 0 
    then invalid_arg "Typed_bytes.get_n"
    else unsafe_get_n t n 

  let unsafe_set_n t a = 
    Array.iteri (fun i e -> unsafe_set t i e ) a  

  let set_n ({offset; bytes} as t) a =  
    (*
    Printf.printf "bytes length: %i, offset: %i, array length: %i, FS length: %i\n"
    (Bytes.length bytes) offset (Array.length a) FS.length; 
    *)
    if Bytes.length bytes < offset + (Array.length a) * FS.length 
    then invalid_arg "Typed_bytes.set_n"
    else unsafe_set_n t a 

  let blit t1 o1 t2 o2 len = 
    (* TODO add checks *)
    let {offset = offset1; bytes = bytes1 } = t1 in 
    let {offset = offset2; bytes = bytes2 } = t2 in 
    let bo1 = offset1 + o1 * FS.length in 
    let bo2 = offset2 + o2 * FS.length in 
    Bytes.blit bytes1 bo1 bytes2 bo2 (len * FS.length)

  (* n is the number of element prior to the insert *)
  let unsafe_insert ({offset; bytes} as t) pos n v = 
    blit t pos t (pos + 1) (n - pos); 
    FS.to_bytes v bytes (offset + pos * FS.length)

  let insert ({offset; bytes} as t) pos n v = 
    if pos  < 0  ||
       pos  > n  ||   (* we allow append at the end *)
       (Bytes.length bytes < offset + (n + 1) * FS.length)
    then invalid_arg "Typed_bytes.insert"
    else unsafe_insert t pos n v 

end 

module Typed_block(FS:Fixed_size_sig) = struct 

  (** {2 Type} *)

  type data = 
    | Data_bytes of int * bytes 
      (* (nb_of_element, encoded elements) *)
    | Data_values of FS.t array
      (* decoded element *)
    | Data_on_disk of int
      (* nb_of_element on disk *)

  type t = {
    offset : int; 
    data : data; 
  }

  (** {2 Constructors} *)

  let values ~offset ~values () = {
    offset; 
    data = Data_values values;
  }
  
  let on_disk ~offset ~n () = {
    offset; 
    data = Data_on_disk n;
  }

  (** {2 Accessors} *)

  let offset {offset; _} = offset

  (** {2 Conversions} *)

  let to_values_from_bytes n bytes = 
    assert(n > 0); 
    let fst = FS.of_bytes bytes 0 in 
    let a = Array.make n fst in 
    for i = 1 to (n - 1) do 
      Array.unsafe_set a i (FS.of_bytes bytes (i * FS.length))
    done; 
    a

  type to_values_result = 
    | To_values_read_data of read_op
    | To_values_values  of FS.t array 

  let to_values {offset; data} = 
    match data with 
    | Data_values a -> To_values_values a
    | Data_bytes (n, bytes) -> To_values_values (to_values_from_bytes n bytes) 
    | Data_on_disk n -> 
      let length = n * FS.length in 
      To_values_read_data {offset; length} 

  let update_to_bytes ({data; _ } as t) bytes =
    let n = match data with 
      | Data_values values -> Array.length values 
      | Data_bytes (n , _)
      | Data_on_disk n -> n 
    in  
    {t with data = Data_bytes (n, bytes)} 

  let update_to_values_from_bytes t bytes = 
    match t.data with
    | Data_values _ -> assert(false) (* TODO handle this better *)
    | Data_bytes (n, _) 
    | Data_on_disk n -> 
      {t with data = Data_values (to_values_from_bytes n bytes)} 

  type write_result = (int * bytes) option  

  let to_bytes_from_values a = 
    (* TODO rename [a] to [values] *)
    let array_len = Array.length a in 
    let bytes_len = FS.length * array_len in 
    let bytes = Bytes.create bytes_len in 

    let rec aux o = function 
      | i when i = array_len -> bytes
      | i -> 
        begin 
          FS.to_bytes (Array.get a i) bytes o; 
          aux (o + FS.length) (i + 1) 
        end
    in 
    aux 0 0

  let write_from_values offset a = 
    let bytes = to_bytes_from_values a in 
    write_op ~offset ~bytes ()

  let write {offset; data} =  
    match data with
    | Data_values a -> 
      Some (write_from_values offset a ) 
    | Data_bytes (_, bytes) -> Some (write_op ~offset ~bytes ())
    | Data_on_disk _ -> None 

end

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

  module Keys = Typed_block(Key) 
  module Vals = Typed_block(Val)
  module Ints = Typed_block(Int)

  module Keys_bytes = Typed_bytes(Key) 
  module Vals_bytes = Typed_bytes(Val)
  module Ints_bytes = Typed_bytes(Int)


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
  
  let incr_k k = 
    if k < 0 then k - 1 else k + 1 

  let nb_of_vals = Pervasives.abs  

  let is_node_full ~k ~m () = 
    nb_of_vals k = m 

  let nb_of_subtrees k = 
    if k < 0 
    then subtrees_access_in_leaf_node ()
    else k + 1 
  
  let keys_offset_rel _ = 
    Int.length

  let vals_offset_rel m  =
    keys_offset_rel m + ((m - 1) * Key.length)

  let subtrees_offset_rel m = 
    vals_offset_rel  m + ((m - 1) * Val.length)

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

  let node_offset ({offset; _ } : node)= offset 

  type make_result = 
    | Make_result_node of node 
    | Make_result_invalid_number_of_values
    | Make_result_inconsistent_vals_keys 

  let on_disk_blocks ~node_offset ~m ~nb_of_vals () = 
    let keys = 
      Keys.on_disk ~offset:(keys_offset node_offset m) ~n:nb_of_vals () 
    in
    let vals = 
      Vals.on_disk ~offset:(vals_offset node_offset m) ~n:nb_of_vals () 
    in 
    let subtrees = 
      let n = nb_of_subtrees nb_of_vals in 
      Ints.on_disk ~offset:(subtrees_offset node_offset m) ~n () 
    in 
    (keys, vals, subtrees) 

  let write_leaf_node ~keys ~vals ~offset ~nb_of_vals ~m () = 
    let bytes = Bytes.create (node_length_of_m m) in 

    let k = - nb_of_vals in 


    let keys_bytes = Keys_bytes.make (keys_offset_rel m) bytes in 
    let vals_bytes = Vals_bytes.make (vals_offset_rel m) bytes in 

    Int.to_bytes k bytes 0; 
    Keys_bytes.set_n keys_bytes keys; 
    Vals_bytes.set_n vals_bytes vals;

    write_op ~offset ~bytes ()  

  let write_intermediate_node 
          ~keys ~vals ~subtrees ~offset ~nb_of_vals ~m () = 

    let bytes = Bytes.create (node_length_of_m m) in 
    let k = nb_of_vals in 
    
    let keys_bytes = Keys_bytes.make (keys_offset_rel m) bytes in 
    let vals_bytes = Vals_bytes.make (vals_offset_rel m) bytes in 
    let subs_bytes = Ints_bytes.make (subtrees_offset_rel m) bytes in 
    
    Int.to_bytes k bytes 0; 
    Keys_bytes.set_n keys_bytes keys; 
    Vals_bytes.set_n vals_bytes vals;
    Ints_bytes.set_n subs_bytes subtrees;

    write_op ~offset ~bytes ()  

  let make_disk_node ~k ~m ~offset () = 
    let nb_of_vals = abs k in 
    let keys, vals, subtrees = 
      on_disk_blocks ~node_offset:offset ~m ~nb_of_vals () 
    in 
    {k;m;offset;keys;vals;subtrees;}
  
  let k_write_op ~offset ~k () = 
    let bytes = Bytes.create Int.length in 
    Int.to_bytes k bytes 0; 
    write_op ~offset ~bytes ()  

  let full_write {offset; k; keys; vals; subtrees; _ } = 
    
    let writes =
      [k_write_op ~offset ~k ()]
      |> cons_option @@ Keys.write keys 
      |> cons_option @@ Vals.write vals 
      |> cons_option @@ Ints.write subtrees 
    in 

    List.rev writes 

  type find_res = 
    | Find_res_not_found 
    | Find_res_val of Val.t 
    | Find_res_read_data of block * find_res_continuation  

  and find_res_continuation = bytes -> find_res 

  let rec find node key = 
    let {k; keys; _} = node in 
    
    match Keys.to_values keys with
    | Keys.To_values_read_data block -> 
      let continutation = fun bytes -> 
        let node = {node with 
          keys = Keys.update_to_bytes keys bytes; 
        } in 
        find node key 
      in 
      Find_res_read_data (block, continutation) 

    | Keys.To_values_values keys_array -> 
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

  and find_in_subtree ({subtrees; m; _} as node) key subtree_i =  
    match Ints.to_values subtrees with
    | Ints.To_values_read_data block -> 
      let continutation = fun bytes -> 
        let node = {node with 
          subtrees = Ints.update_to_values_from_bytes subtrees bytes
        } in
        find_in_subtree node key subtree_i
      in 
      Find_res_read_data (block, continutation)

    | Ints.To_values_values (subtree_a :int array)-> 

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
        if key = key' 
        then return_val_at node i 
        else aux (i + 1) 
    in
    aux 0
  
  and return_val_at {vals;k; _} i = 
    match Vals.to_values vals with
    | Vals.To_values_read_data block -> 
      (* The values data was not read from disk, let's make sure
       * this is done first *)
      let continuation = fun bytes -> 
        let vals_array = Vals.to_values_from_bytes (nb_of_vals k) bytes in 
        Find_res_val (Array.get vals_array i) 
        (* Note we could have recursively called this [find] function
         * but it would have been ineficient and not that much more 
         * elegant. *)
      in 
      Find_res_read_data (block, continuation) 

    | Vals.To_values_values a -> 
      (* Values are already read from disk, simply return it *)
      Find_res_val (Array.get a i)

  type insert_res = 
    | Insert_res_done of (int option * write_op list)  
    | Insert_res_read_data of block * insert_res_read_data_continuation  
    | Insert_res_allocate of int * insert_res_allocate_continuation 
    | Insert_res_node_split of (Key.t * Val.t * int * write_op list) 
  
  and insert_res_read_data_continuation = bytes -> insert_res  

  and insert_res_allocate_continuation = int -> insert_res 

  (* returns the position index in which a new key/value should 
   * be inserted. *)
  let find_key_insert_position keys_values key = 
    let len = Array.length keys_values in 

    let rec aux = function 
      | i when i = len -> `Insert i
      | i -> 
        let key' = Array.get keys_values i in 
        match Key.compare key' key  with
        | 0 -> `Update i 
        | c when c > 0 -> `Insert i 
        |  _ -> aux (i + 1) 
    in 
    aux 0 

  let insert_split_leaf_node {m; keys; vals; offset; _ } keys_values vals_values = 

    assert(Array.length keys_values = Array.length vals_values);
    
    let nb_of_vals = Array.length keys_values in 

    let (
      median_key, 
      left_keys_values, 
      right_keys_values 
    ) = array_median_split keys_values in 

    let (
      median_value, 
      left_vals_values, 
      right_vals_values 
    ) = array_median_split vals_values in 

    assert(Array.length left_vals_values = nb_of_vals / 2);
    assert(Array.length right_vals_values = nb_of_vals / 2);
    assert(Array.length left_keys_values = nb_of_vals / 2);
    assert(Array.length right_keys_values = nb_of_vals / 2);

    let k = - (nb_of_vals / 2) in  

    let k_write_op = k_write_op ~offset ~k () in 

    let n_keys_write_op = 
      Keys.write_from_values (Keys.offset keys) left_keys_values 
    in 
    let n_vals_write_op = 
      Vals.write_from_values (Vals.offset vals) left_vals_values; 
    in 

    let continuation = (fun new_node_offset -> 
      let new_node_write_op = 
        write_leaf_node 
          ~keys:right_keys_values 
          ~vals:right_vals_values 
          ~offset:new_node_offset
          ~nb_of_vals:(nb_of_vals / 2)
          ~m () 
      in 
      let write_ops = 
        k_write_op      ::  
        n_keys_write_op :: 
        n_vals_write_op :: 
        new_node_write_op :: []
      in 
      Insert_res_node_split (
        median_key, 
        median_value, 
        new_node_offset, 
        write_ops) 
    ) in

    Insert_res_allocate (node_length_of_m m, continuation) 
  
  let insert_split_intermediate_node node keys_values vals_values subtrees_values write_ops = 

    let {m; keys; vals; subtrees; offset; _ }  = node in 

    assert(Array.length keys_values = Array.length vals_values);
    
    let nb_of_vals = Array.length keys_values in 

    let (
      median_key, 
      left_keys_values, 
      right_keys_values 
    ) = array_median_split keys_values in 

    (*
    Printf.printf  "insert_split_intermediate_node, median: %s\n" (Key.to_string median_key);
    *)

    let (
      median_value, 
      left_vals_values, 
      right_vals_values 
    ) = array_median_split vals_values in 

    let (
      left_subtrees_values, 
      right_subtrees_values
    ) = array_half_split subtrees_values in

    assert(Array.length left_vals_values = nb_of_vals / 2);
    assert(Array.length right_vals_values = nb_of_vals / 2);
    assert(Array.length left_keys_values = nb_of_vals / 2);
    assert(Array.length right_keys_values = nb_of_vals / 2);
    assert(Array.length left_subtrees_values = (nb_of_vals / 2) + 1);
    assert(Array.length right_subtrees_values = (nb_of_vals / 2) + 1);

    let k = (nb_of_vals / 2) in  
      (* the new node is not a leaf! *)

    let k_write_op = k_write_op ~offset ~k () in 

    (*
    Printf.printf "k write op: k: %i, offset: %06i\n" k offset;
    *)

    let n_keys_write_op = 
      Keys.write_from_values (Keys.offset keys) left_keys_values 
    in 

    let n_vals_write_op = 
      Vals.write_from_values (Vals.offset vals) left_vals_values; 
    in 

    let n_subtrees_write_op = 
      Ints.write_from_values (Ints.offset subtrees) left_subtrees_values; 
    in 

    let continuation = (fun new_node_offset -> 
      let new_node_write_op = write_intermediate_node 
        ~keys:right_keys_values 
        ~vals:right_vals_values
        ~subtrees:right_subtrees_values 
        ~offset:new_node_offset 
        ~m 
        ~nb_of_vals:(nb_of_vals / 2)  
        () in  

      let write_ops = 
        k_write_op      ::  
        n_keys_write_op :: 
        n_vals_write_op :: 
        n_subtrees_write_op ::
        new_node_write_op   :: write_ops
      in 
      Insert_res_node_split (median_key, median_value, new_node_offset, write_ops) 
    ) in

    Insert_res_allocate (node_length_of_m m, continuation) 

  let insert_make_root left_node right_node_offset key value write_ops = 
    let continuation = fun new_root_offset -> 
      let new_root_write_op = write_intermediate_node 
        ~keys:[| key |] 
        ~vals:[| value |] 
        ~subtrees:[| node_offset left_node; right_node_offset |] 
        ~offset:new_root_offset 
        ~nb_of_vals:1 
        ~m:left_node.m 
        ()  
      in

      let write_ops = new_root_write_op :: write_ops in 
      Insert_res_done (Some new_root_offset, write_ops)
    in
    Insert_res_allocate (node_length_of_m left_node.m, continuation) 
    
  let insert_handle_subtree_split_node 
            is_root
            ({vals; keys; subtrees; k; m; offset; _ } as node)  
            keys_values subtrees_values pos (key, value, n_offset, write_ops) = 

    (*
    Printf.printf "insert_handle_subtree_split_node: offset: %i, k: %i\n"  
      offset k; 
    *)

    let do_insert vals_values = 
      (*
      Printf.printf 
        "insert_handle_subtree_split_node: key: %s\n" (Key.to_string key); 
      Printf.printf 
        "insert_handle_subtree_split_node: val: %s\n" (Val.to_string value); 
      Printf.printf 
        "insert_handle_subtree_split_node: subtree: %i\n" (node_offset n);
       *)

      let keys_values = array_insert keys_values pos key in  

      (*
      Array.iteri (fun i key -> 
        Printf.printf "insert_handle_subtree_split_node: key[%02i] : %s\n" i
        (Key.to_string key)
      ) keys_values; 
      *)

      let vals_values = array_insert vals_values pos value in 
      let subtrees_values = 
        array_insert subtrees_values (pos + 1) n_offset 
      in 

      let k = incr_k k in
      (*
      Printf.printf "insert_handle_subtree_split_node: k : %i, m: %i \n" k m;
      *)
      if is_node_full ~k ~m () 
      then 

        let rec aux ret = 
          match ret with
          | Insert_res_done _ -> ret 
          | Insert_res_allocate (length, continuation) ->
            let continuation' = fun offset ->  
              continuation offset |> aux 
            in 
            Insert_res_allocate (length, continuation')
          | Insert_res_read_data (block, continuation) -> 
            let continuation' = fun bytes -> 
              continuation bytes |>  aux 
            in 
            Insert_res_read_data (block, continuation') 

          | Insert_res_node_split (key, value, right_node_offset, write_ops) -> 
            if is_root 
            then insert_make_root node right_node_offset key value write_ops 
            else ret  
        in
        aux @@ insert_split_intermediate_node 
          node keys_values vals_values subtrees_values write_ops  
      else 
        Insert_res_done (None, [
          Keys.write_from_values (Keys.offset keys) keys_values;
          Vals.write_from_values (Vals.offset vals) vals_values;
          Ints.write_from_values (Ints.offset subtrees) subtrees_values;
          k_write_op ~offset ~k ()
        ] @ write_ops )
    in 
    match Vals.to_values vals with 
    | Vals.To_values_values vals_values -> do_insert vals_values 
    | Vals.To_values_read_data block -> 
      let continuation = fun bytes -> 
        let vals_values = Vals.to_values_from_bytes (nb_of_vals k) bytes in 
        do_insert vals_values
      in
      Insert_res_read_data (block, continuation) 

  let rec insert ?is_root:(is_root = true) ({k; keys; vals; subtrees; m; offset; _} as node) key value = 
    if is_leaf k 
    then
      match Keys.to_values keys , Vals.to_values vals with
      | Keys.To_values_values keys_values , Vals.To_values_values vals_values -> 
        (* find the place to insert the new values and shift
         * all the ones after it by the size of the inserted 
         * key. *)
        assert(nb_of_vals k = Array.length keys_values);
        assert(nb_of_vals k = Array.length vals_values);

        begin match find_key_insert_position keys_values key with
        | `Update pos ->
          Array.set keys_values pos key; 
          Array.set vals_values pos value;
          Insert_res_done (None, [
            Keys.write_from_values (Keys.offset keys) keys_values; 
            Vals.write_from_values (Vals.offset vals) vals_values; 
          ])

        | `Insert pos -> 
          let keys_values = array_insert keys_values pos key in  
          let vals_values = array_insert vals_values pos value in 

          let k = incr_k k in  

          if is_node_full ~k ~m ()  
          then
            (* Node is full after the insertion. 
             *
             * - The median key/value should be extracted and returned 
             *   to caller
             * - The current node should be truncated in half
             * - A new node should be created with the remaining key/values.
             *)
            
            let rec aux ret = 
              match ret with
              | Insert_res_done _ -> ret 
              | Insert_res_allocate (length, continuation) ->
                let continuation' = fun offset ->  
                  continuation offset |> aux 
                in 
                Insert_res_allocate (length, continuation')
              | Insert_res_read_data (block, continuation) -> 
                let continuation' = fun bytes -> 
                  continuation bytes |>  aux 
                in 
                Insert_res_read_data (block, continuation') 
              | Insert_res_node_split (key, value, right_node, write_ops) -> 
                if is_root 
                then insert_make_root node right_node key value write_ops 
                else ret  
            in
            aux @@ insert_split_leaf_node node keys_values vals_values 
          else 
            Insert_res_done (None, [
              Keys.write_from_values (Keys.offset keys) keys_values;
              Vals.write_from_values (Vals.offset vals) vals_values;
              k_write_op ~offset ~k ()
            ])
        end
        
      | Keys.To_values_read_data block, _ -> 
        let continuation = fun bytes -> 
          let node = {node with keys = Keys.update_to_bytes keys bytes} in 
          insert ~is_root node key value 
        in 
        Insert_res_read_data (block, continuation) 

      | Keys.To_values_values _, Vals.To_values_read_data block -> 
        let continuation = fun bytes -> 
          let node = {node with vals = Vals.update_to_bytes vals bytes} in 
          insert ~is_root node key value 
        in 
        Insert_res_read_data (block, continuation) 

    else (* not a leaf *)  
      match Keys.to_values keys with
      | Keys.To_values_read_data block -> 
        let continuation = fun bytes -> 
          let node = {node with
            keys = Keys.update_to_values_from_bytes keys bytes; 
          } in 
          insert ~is_root node key value 
        in 
        Insert_res_read_data (block, continuation) 

      | Keys.To_values_values keys_values -> 
        begin match find_key_insert_position keys_values key with
        | `Update pos -> 

          let do_update vals_values = 
            Array.set keys_values pos key; 
            Array.set vals_values pos value; 
            Insert_res_done (None, [
              Keys.write_from_values (Keys.offset keys) keys_values;
              Vals.write_from_values (Vals.offset vals) vals_values;
            ])
          in
          begin match Vals.to_values vals with
          | Vals.To_values_values vals_values -> do_update vals_values 
          | Vals.To_values_read_data block -> 
            let continuation = fun bytes -> 
              let vals_values = Vals.to_values_from_bytes (nb_of_vals k) bytes in 
              do_update vals_values
            in
            Insert_res_read_data (block, continuation) 
          end

        | `Insert pos -> 

          let do_insert subtrees_values = 
            let subtree_offset = Array.get subtrees_values pos in 
            let continuation = fun bytes -> 
              let k = Int.of_bytes bytes 0 in 
              let child_node = make_disk_node ~k ~m ~offset:subtree_offset () in 
              let rec aux res = 
                match res with 
                | Insert_res_done _ -> res 

                | Insert_res_allocate (length, k) -> 
                  let continuation = fun offset -> 
                    k offset |>  aux 
                  in 
                  Insert_res_allocate (length, continuation) 

                | Insert_res_read_data (block, k) -> 
                  let continuation = fun bytes -> 
                    k bytes |> aux 
                  in 
                  Insert_res_read_data (block, continuation) 

                | Insert_res_node_split node_split -> 
                  insert_handle_subtree_split_node 
                    is_root node keys_values subtrees_values pos node_split
              in
              insert ~is_root:false child_node key value |> aux 
                     
            in 
            Insert_res_read_data (
              {offset = subtree_offset; length = Int.length}, 
              continuation
            ) 
          in
          begin match Ints.to_values subtrees with
          | Ints.To_values_values subtrees_values -> do_insert subtrees_values 
          | Ints.To_values_read_data block -> 
            let continuation = fun bytes -> 
              let subtrees_values = 
                Ints.to_values_from_bytes (nb_of_subtrees k) bytes 
              in  

              do_insert subtrees_values 
            in 
            Insert_res_read_data (block, continuation)
          end
        end

  type make_from_disk_result = 
    | Make_from_disk_node of node 
    | Make_from_disk_read_data of block * make_from_disk_continuation 
  
  and make_from_disk_continuation = bytes -> make_from_disk_result

  let make_node_from_offset ~offset ~m () = 

    let continuation = fun bytes ->
      let k = Int.of_bytes bytes 0 in 
      Make_from_disk_node (make_disk_node ~k ~m ~offset ()) 
    in 
    let block : block = { offset ; length = Int.length; } in 
    Make_from_disk_read_data (block, continuation)  


  type debug_res = 
    | Debug_res_read_data of block * debug_res_continuation 
    | Debug_res_done  

  and debug_res_continuation = bytes -> debug_res  

  let indent_string  n = String.make n ' ' 

  let rec debug indent offset m = 

    let continuation = fun bytes -> 
      let k = Int.of_bytes bytes 0 in 
      let keys_bytes = 
        Bytes.sub bytes (keys_offset offset m - offset) (keys_length m) 
      in 
      let keys = Keys.to_values_from_bytes (nb_of_vals k) keys_bytes in 

      if is_leaf k 
      then begin  
        let keys = String.concat ", " 
          (keys |> Array.to_list |> List.map Key.to_string) 
        in 
        Printf.printf "%s+- Leaf [%06i] : [%s]\n" (indent_string indent) offset keys;
        Debug_res_done
      end
      else begin 
        Printf.printf "%s+- Node [%06i] \n" (indent_string indent) offset;
        let subtrees_bytes = 
          Bytes.sub bytes (subtrees_offset offset m - offset) (subtrees_length m) 
        in 

        let subtrees = 
          Ints.to_values_from_bytes (nb_of_subtrees k) subtrees_bytes 
        in 

        let rec aux i = 
          let do_next () = 
            if i = (nb_of_vals k) 
            then Debug_res_done 
            else begin  
              Printf.printf "%s|- Key [%02i] : %s\n" 
                (indent_string (indent + 1)) i (Array.get keys i |> Key.to_string); 
              aux (i + 1)
            end 
          in

          let rec aux2 = function
            | Debug_res_done -> do_next () 
            | Debug_res_read_data (block, continuation) ->
              let continuation' = fun bytes -> 
                continuation bytes |> aux2 
              in 
              Debug_res_read_data (block, continuation') 
          in
          aux2 @@ debug (indent + 1) (Array.get subtrees i) m 
        in
        aux 0 
      end
    in 
    Debug_res_read_data (make_block ~offset ~length:(node_length_of_m m) (),
                         continuation) 

end 
