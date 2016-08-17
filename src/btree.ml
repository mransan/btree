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

let array_split_at a pos = 
  let left = Array.sub a 0 pos in
  let right = Array.sub a pos (Array.length a - pos) in 
  (left , right) 

let array_insert a pos v = 
  let a_len = Array.length a in 
  let new_a_len = a_len + 1 in 
  let new_a = Array.make new_a_len v in 
  Array.blit a 0 new_a 0 pos; 
  Array.blit a pos new_a (pos + 1) (a_len - pos); 
  new_a  

let array_insert_pop_left a pos v = 
  assert(pos > 0); 
  assert(pos <= Array.length a);
  let poped = a.(0) in 
  Array.blit a 1 a 0 (pos - 1);
  Array.set a (pos-1) v; 
  poped 

(** [array_insert_split a pos v] return the array 
 *  [ [| [v; a.(pos); a.(pos + 1) ...  * a.(n) |] ] ] *)
let array_insert_split a pos v = 
  let a_len = Array.length a in 
  assert(pos <= a_len); (* we allow append *)
  let reminder_len = a_len - pos in 
  let new_len = reminder_len + 1 in 
  let new_a = Array.make new_len v in 
  Array.blit a pos new_a 1 reminder_len; 
  new_a 
  
(** [array_update_split a pos v] return the array 
 *  [ [| [v; a.(pos + 1); a.(pos + 2) ...  * a.(n) |] ] ] *)
let array_update_split a pos v = 
  let a_len = Array.length a in 
  assert(pos < a_len); 
  let reminder_len = a_len - pos in 
  let new_a = Array.make reminder_len v in 
  Array.blit a (pos + 1) new_a 1 (reminder_len - 1); 
  new_a 


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

type write_op = {
  offset : int; 
  bytes : bytes;
} 

let write_op ~offset ~bytes () = {offset; bytes} 

type read_op = block 

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

  let bytes ~offset ~n ~bytes () = {
    offset; 
    data = Data_bytes (n, bytes);
  } 

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

  let write {offset; data} =  
    match data with
    | Data_values a -> 
      let bytes = to_bytes_from_values a in 
      Some (write_op ~offset ~bytes ()) 
    | Data_bytes (_, bytes) -> Some (write_op ~offset ~bytes ())
    | Data_on_disk _ -> None 

  (* TODO this simply return the write op, it would be worth
   * to also return the new [t] value with the inserted value
   *)
  let insert_write_op offset values pos v =  
    let values' = array_insert_split values pos v in 
    let offset = offset + (pos * FS.length) in 
    let bytes = to_bytes_from_values values' in 
    write_op ~offset ~bytes () 
  
  let update_write_op offset values pos v =  
    let values' = array_update_split values pos v in 
    let offset = offset + (pos * FS.length) in 
    let bytes = to_bytes_from_values values' in 
    write_op ~offset ~bytes () 

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

  let make_leaf_node ?keys ?vals ~offset ~nb_of_vals ~m () = 
    if nb_of_vals > m - 1 || nb_of_vals < 1  
    then Make_result_invalid_number_of_values
    else 
      let k = - nb_of_vals in  
      
      let make keys vals subtrees = 
        Make_result_node {m;offset;k;keys;vals;subtrees}
      in

      (* default ones are on disk *)
      let on_disk_keys, on_disk_vals, on_disk_substrees = 
        on_disk_blocks ~node_offset:offset ~m ~nb_of_vals () 
      in  

      match keys, vals with
      | None, None -> 
        make on_disk_keys on_disk_vals on_disk_substrees  

      | Some keys, Some vals -> 
        if Array.length keys <> Array.length vals 
        then Make_result_inconsistent_vals_keys 
        else 
          let keys = Keys.values ~offset:(keys_offset offset m)  ~values:keys () in 
          let vals = Vals.values ~offset:(vals_offset offset m)  ~values:vals () in 
          make keys vals on_disk_substrees 

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
        let keys, vals, subtrees =
          on_disk_blocks ~node_offset:offset ~m ~nb_of_vals () 
        in  
        make keys vals subtrees
      | Some keys, Some vals, Some subtrees -> 
        if Array.length keys <> Array.length vals || 
           Array.length keys <> (Array.length subtrees - 1)
        then 
          Make_result_inconsistent_vals_keys
        else 
          let keys = Keys.values ~offset:(keys_offset offset m) ~values:keys () in 
          let vals = Vals.values ~offset:(vals_offset offset m) ~values:vals () in 
          let subtrees = Ints.values ~offset:(subtrees_offset offset m) ~values:subtrees () in 
          make keys vals subtrees
      | _ -> Make_result_inconsistent_vals_keys

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
        Printf.printf "In leaf node, key: %s, key': %s\n" 
          (Key.to_string key) (Key.to_string key');
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
    | Insert_res_done of write_op list  
    | Insert_res_read_data of block * insert_res_read_data_continuation  
    | Insert_res_allocate of int * insert_res_allocate_continuation 
    | Insert_res_node_split of (Key.t * Val.t * node * write_op list) 
  
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

  let rec insert ({k; keys; vals; m; offset; _} as node) key value = 
    if is_leaf k 
    then
      let nb_of_vals = nb_of_vals k in 
      (* The key/val can be inserted in this node *)
      match Keys.to_values keys , Vals.to_values vals with
      | Keys.To_values_values keys_values , Vals.To_values_values vals_values -> 
        (* find the place to insert the new values and shift
         * all the ones after it by the size of the inserted 
         * key. *)
        assert(nb_of_vals = Array.length keys_values);

        begin match find_key_insert_position keys_values key with
        | `Update pos ->
          let keys_write_op = 
            Keys.update_write_op (Keys.offset keys) keys_values pos key 
          in  
          let vals_write_op = 
            Vals.update_write_op (Vals.offset vals) vals_values pos value
          in  
          Insert_res_done [ keys_write_op; vals_write_op;]

        | `Insert pos -> 
          let nb_of_vals = nb_of_vals + 1 in 
          if nb_of_vals = m 
          then begin  
            let median = nb_of_vals / 2 in 
            Printf.printf "pos: %i, median: %i\n" pos median;
            begin match Pervasives.compare pos median with
            | 0 -> 
              (* The new value is the median *)

              (* split the keys/values in half *)
              let (_, right_keys_values) = array_split_at keys_values median in 
              let (_, right_vals_values) = array_split_at vals_values median in 
              (* update k to be half *)

              let k = - median in  
              let k_write_op = k_write_op ~offset ~k () in  

              (* allocate a new node and initialize the new node with the
               * other half of the keys/values *)
              let continuation = (fun new_node_offset -> 
                let new_node = make_leaf_node 
                  ~keys:right_keys_values 
                  ~vals:right_vals_values
                  ~offset:new_node_offset 
                  ~m 
                  ~nb_of_vals:median
                  () in  

                let new_node = match new_node with
                  | Make_result_node n -> n
                  | _ -> failwith "Programmatic error"
                in  
                let write_ops = k_write_op :: (full_write new_node) in 
                Insert_res_node_split (key, value, new_node, write_ops) 
              ) in 
              Insert_res_allocate (node_length_of_m m, continuation) 

            | c when c > 0 ->
              (* the new value is in the right node which will be newly
               * created *)

              (* split the keys/values in half *)
              let (_, right_keys_values) = array_split_at keys_values median in 
              let (_, right_vals_values) = array_split_at vals_values median in 

              (* update k to be half
               * note that since we are only truncating the current node, 
               * only the new k value needs to be written to disk. *)
              let k = - median in  
              let k_write_op = k_write_op ~offset ~k () in  

              let pos = pos - median in 
                (* the position in the new leaf node array *)
              let median_key = array_insert_pop_left right_keys_values pos key in 
              let median_val = array_insert_pop_left right_vals_values pos value in 

              (* allocate a new node and initialize the new node with the
               * other half of the keys/values *)
              let continuation = (fun new_node_offset -> 
                let new_node = make_leaf_node 
                  ~keys:right_keys_values 
                  ~vals:right_vals_values
                  ~offset:new_node_offset 
                  ~m 
                  ~nb_of_vals:median
                  () in  

                let new_node = match new_node with
                  | Make_result_node n -> n
                  | _ -> failwith "Programmatic error"
                in  
                let write_ops = k_write_op :: (full_write new_node) in 
                Insert_res_node_split (median_key, median_val, new_node, write_ops) 
              ) in
              Insert_res_allocate (node_length_of_m m, continuation)
               

            | _ -> failwith "Not implemented"
            end
          
          end
          else 

            let keys_write_op = 
              Keys.insert_write_op (Keys.offset keys) keys_values pos key 
            in  
            let vals_write_op = 
              Vals.insert_write_op (Vals.offset vals) vals_values pos value
            in  
            let k = incr_k k in 
            Insert_res_done [
              keys_write_op;
              vals_write_op; 
              k_write_op ~offset ~k ()]
        end
        
      | Keys.To_values_read_data block, _ -> 
        let continuation = fun bytes -> 
          let node = {node with keys = Keys.update_to_bytes keys bytes} in 
          insert node key value 
        in 
        Insert_res_read_data (block, continuation) 
      | Keys.To_values_values _, Vals.To_values_read_data block -> 
        let continuation = fun bytes -> 
          let node = {node with vals = Vals.update_to_bytes vals bytes} in 
          insert node key value 
        in 
        Insert_res_read_data (block, continuation) 

    else 
      failwith "Not handled yet"


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

end 
