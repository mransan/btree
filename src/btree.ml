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

let make_write_op ~offset ~bytes () = {offset; bytes} 

type read_op = block 

module Typed_bytes(FS:Fixed_size_sig) =  struct 

  type t = {
    offset: int; 
    bytes: bytes; 
  } 

  let make ~offset ~bytes () = {offset; bytes}

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

  module Keys = Typed_bytes(Key) 
  module Vals = Typed_bytes(Val)
  module Ints = Typed_bytes(Int)

  type node_on_disk = {
    offset: int; 
    m : int; 
  }

  type node_as_byte = {
    on_disk : node_on_disk;
    k : int;
    keys: Keys.t; 
    vals: Vals.t;
    subs: Ints.t;
    bytes: bytes;
  }

  let make_on_disk ~offset ~m () = {offset; m}


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

  let subs_offset_rel m = 
    vals_offset_rel  m + ((m - 1) * Val.length)

  let keys_length m = 
    (m - 1) * Key.length

  let vals_length m = 
    (m - 1) * Val.length 

  let subtrees_length m = 
    m * Int.length

  let node_length_of_m m = 
    Int.length + (keys_length m) + (vals_length m) + (subtrees_length m) 

  let node_block {offset; m;} =
    make_block ~offset ~length:(node_length_of_m m) 

  let make_as_bytes ~on_disk:({m;_ } as on_disk) ~bytes () = {
    on_disk; 
    k = Int.of_bytes bytes 0; 
    keys = Keys.make ~offset:(keys_offset_rel m) ~bytes (); 
    vals = Vals.make ~offset:(vals_offset_rel m) ~bytes (); 
    subs = Ints.make ~offset:(subs_offset_rel m) ~bytes ();
    bytes;
  }

  let write_leaf_node 
          ~keys ~vals ~offset ~nb_of_vals ~m () = 

    let bytes = Bytes.create (node_length_of_m m) in 
    let k = - nb_of_vals in 

    let keys_bytes = Keys.make ~offset:(keys_offset_rel m) ~bytes () in 
    let vals_bytes = Vals.make ~offset:(vals_offset_rel m) ~bytes () in 

    Int.to_bytes k bytes 0; 
    Keys.set_n keys_bytes keys; 
    Vals.set_n vals_bytes vals;

    make_write_op ~offset ~bytes ()  

  let write_intermediate_node 
          ~keys ~vals ~subs ~offset ~nb_of_vals ~m () = 

    let bytes = Bytes.create (node_length_of_m m) in 
    let k = nb_of_vals in 
    
    let keys_bytes = Keys.make ~offset:(keys_offset_rel m) ~bytes () in 
    let vals_bytes = Vals.make ~offset:(vals_offset_rel m) ~bytes () in 
    let subs_bytes = Ints.make ~offset:(subs_offset_rel m) ~bytes () in 
    
    Int.to_bytes k bytes 0; 
    Keys.set_n keys_bytes keys; 
    Vals.set_n vals_bytes vals;
    Ints.set_n subs_bytes subs;

    make_write_op ~offset ~bytes ()  

  type insert_res = 
    | Insert_res_done of (int option * write_op list)  
    | Insert_res_read_data of block * insert_res_read_data_continuation  
    | Insert_res_allocate of int * insert_res_allocate_continuation 
    | Insert_res_node_split of (Key.t * Val.t * int * write_op list) 
  
  and insert_res_read_data_continuation = bytes -> insert_res  

  and insert_res_allocate_continuation = int -> insert_res 

  let rec intercept_node_split f ret = 
    match ret with 
    | Insert_res_done _ -> ret 

    | Insert_res_read_data (block, continuation) -> 
      let continuation' = fun bytes -> 
        continuation bytes |> intercept_node_split f 
      in 
      Insert_res_read_data (block, continuation') 

    | Insert_res_allocate (length, continuation) -> 
      let continuation' = fun offset -> 
        continuation offset |> intercept_node_split f 
      in 
      Insert_res_allocate (length, continuation') 

    | Insert_res_node_split node_split  -> f node_split 

  (* returns the position index in which a new key/value should 
   * be inserted. *)
  let find_key_insert_position node key = 
    let {keys; k; _ } = node in 
    let nb_of_keys = nb_of_vals k in

    let rec aux = function 
      | i when i = nb_of_keys -> `Insert i
      | i -> 
        let key' = Keys.get keys i in 
        match Key.compare key' key  with
        | 0 -> `Update i 
        | c when c > 0 -> `Insert i 
        |  _ -> aux (i + 1) 
    in 
    aux 0 

  let insert_split_leaf_node node keys_values vals_values = 

    let {
      on_disk = {m; offset; }; 
      keys; 
      vals; 
      bytes; _ } = node in 

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

    Int.to_bytes k bytes 0; 
    Keys.set_n keys left_keys_values; 
    Vals.set_n vals left_vals_values; 

    let left_write_op = make_write_op ~offset ~bytes () in

    let continuation = (fun right_node_offset -> 
      let right_write_op = 
        write_leaf_node 
          ~keys:right_keys_values 
          ~vals:right_vals_values 
          ~offset:right_node_offset
          ~nb_of_vals:(nb_of_vals / 2)
          ~m () 
      in 
      let write_ops = left_write_op :: right_write_op :: [] in 
      Insert_res_node_split (
        median_key, 
        median_value, 
        right_node_offset, 
        write_ops) 
    ) in

    Insert_res_allocate (node_length_of_m m, continuation) 
  
  let insert_split_intermediate_node node keys_values vals_values subs_values write_ops = 

    let {
      on_disk = {m; offset;}; 
      bytes; 
      keys; vals; subs; _ }  = node in 

    assert(Array.length keys_values = Array.length vals_values);
    assert(Array.length keys_values = Array.length subs_values - 1);
    
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

    let (
      left_subs_values, 
      right_subs_values
    ) = array_half_split subs_values in

    assert(Array.length left_vals_values = nb_of_vals / 2);
    assert(Array.length right_vals_values = nb_of_vals / 2);
    assert(Array.length left_keys_values = nb_of_vals / 2);
    assert(Array.length right_keys_values = nb_of_vals / 2);
    assert(Array.length left_subs_values = (nb_of_vals / 2) + 1);
    assert(Array.length right_subs_values = (nb_of_vals / 2) + 1);

    let k = (nb_of_vals / 2) in  
      (* the new node is not a leaf! *)

    Int.to_bytes k bytes 0; 
    Keys.set_n keys left_keys_values; 
    Vals.set_n vals left_vals_values; 
    Ints.set_n subs left_subs_values; 

    let left_write_op = make_write_op ~offset ~bytes () in 

    let continuation = (fun right_node_offset -> 
      let right_write_op = write_intermediate_node 
        ~keys:right_keys_values 
        ~vals:right_vals_values
        ~subs:right_subs_values 
        ~offset:right_node_offset 
        ~m 
        ~nb_of_vals:(nb_of_vals / 2)  
        () in  

      let write_ops = left_write_op :: right_write_op :: write_ops in  
        
      Insert_res_node_split (
        median_key, median_value, right_node_offset, write_ops) 
    ) in

    Insert_res_allocate (node_length_of_m m, continuation) 

  let insert_make_root left_node node_split = 
    let (key, value, right_node_offset, write_ops) = node_split in 
    let { on_disk = {offset; m; }; _} =  left_node in 
    let continuation = fun new_root_offset -> 
      let new_root_write_op = write_intermediate_node 
        ~keys:[| key |] 
        ~vals:[| value |] 
        ~subs:[| offset; right_node_offset |] 
        ~offset:new_root_offset 
        ~nb_of_vals:1 
        ~m
        ()  
      in

      let write_ops = new_root_write_op :: write_ops in 
      Insert_res_done (Some new_root_offset, write_ops)
    in
    Insert_res_allocate (node_length_of_m m, continuation) 
    
  let insert_handle_subtree_split_node 
            is_root
            node 
            keys_values subs_values pos node_split = 

    let {
      on_disk ={m; offset;}; 
      keys; vals; subs; 
      k;
      bytes;
    } = node in 

    let (key, value, n_offset, write_ops) = node_split in 

    let vals_values = Vals.get_n vals (nb_of_vals k) in  

    let keys_values = array_insert keys_values pos key in  
    let vals_values = array_insert vals_values pos value in 
    let subs_values = array_insert subs_values (pos + 1) n_offset in 

    let k = incr_k k in
    if is_node_full ~k ~m () 
    then 
      insert_split_intermediate_node node keys_values vals_values subs_values write_ops 
      |> intercept_node_split (fun node_split ->
        if is_root 
        then insert_make_root node node_split 
        else Insert_res_node_split node_split
      ) 

    else begin 
      Int.to_bytes k bytes 0;
      Keys.set_n keys keys_values; 
      Vals.set_n vals vals_values; 
      Ints.set_n subs subs_values; 

      let write_ops = make_write_op ~offset ~bytes () :: write_ops in 

      Insert_res_done (None, write_ops) 
    end

  let rec insert ?is_root (node:node_on_disk) key value = 
    let continuation = fun bytes -> 
      let node = make_as_bytes ~on_disk:node ~bytes () in 
      insert_as_bytes ?is_root node key value 
    in 
    Insert_res_read_data (node_block node (), continuation) 

  and insert_as_bytes ?is_root:(is_root = true) node key value = 

    let {
      on_disk = {m; offset;}; 
      k; 
      keys; vals; subs;
      bytes;
    } = node in 

    if is_leaf k 
    then

      match find_key_insert_position node key with
      | `Update pos -> begin 
        Vals.set vals pos value; 
        Keys.set keys pos key;
        let write_op = make_write_op ~offset ~bytes () in 
        Insert_res_done (None, [write_op]) 
      end

      | `Insert pos -> 
        let keys_values = Keys.get_n keys (nb_of_vals k) in 
        let vals_values = Vals.get_n vals (nb_of_vals k) in 
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
          insert_split_leaf_node node keys_values vals_values
          |> intercept_node_split (fun node_split -> 
            if is_root 
            then insert_make_root node node_split 
            else Insert_res_node_split node_split
          )

        else begin 
          (* Node is not full, only need to update the bytes on disk *) 
          Int.to_bytes k bytes 0; 
          Keys.set_n keys keys_values;
          Vals.set_n vals vals_values;
          let write_op = make_write_op ~offset ~bytes () in 
          Insert_res_done (None, [write_op]) 
        end

    else (* not a leaf *)  

      begin match find_key_insert_position node key with
      | `Update pos -> begin 
        Vals.set vals pos value; 
        Keys.set keys pos key;
        let write_op = make_write_op ~offset ~bytes () in 
        Insert_res_done (None, [write_op]) 
      end

      | `Insert pos -> 
        let sub_node = make_on_disk ~offset:(Ints.get subs pos) ~m () in 
        
        insert ~is_root:false sub_node key value 
        |> intercept_node_split (fun node_split -> 
          let keys_values = Keys.get_n keys (nb_of_vals k) in 
          let subs_values = Ints.get_n subs (nb_of_subtrees k) in 
          insert_handle_subtree_split_node 
            is_root node keys_values subs_values pos node_split
        )
      end

  type find_res = 
    | Find_res_not_found 
    | Find_res_val of Val.t 
    | Find_res_read_data of block * find_res_continuation  

  and find_res_continuation = bytes -> find_res 

  let rec find (node:node_on_disk) key = 
    Find_res_read_data (node_block node (), fun bytes -> 
      find_as_bytes (make_as_bytes ~on_disk:node ~bytes ()) key
    ) 
    
  and find_as_bytes node key = 
    let {k; _ } = node in 
    if is_leaf k
    then find_leaf_node node key
    else find_leaf_internal_node node key 

  and find_leaf_internal_node node key = 
    let { k; keys; vals; subs; on_disk = {m; _}; _ } = node in 
    let nb_of_keys = nb_of_vals k in 

    let find_in_subtree i = 
      let sub_offset = Ints.get subs i in 
      let sub_node = make_on_disk ~offset:sub_offset ~m () in 
      find sub_node key
    in

    let rec aux = function
      | i when i = nb_of_keys -> find_in_subtree i 
      | i -> 
        match Key.compare (Keys.get keys i) key with
        | 0 ->  
          Find_res_val (Vals.get vals i) 
        | c when c >  0 -> find_in_subtree i  
        | _ -> aux (i + 1)
    in  
    aux 0

  and find_leaf_node node key =  
    let {k; keys; vals; _ } = node in 
    let nb_of_keys = nb_of_vals k in 
    let rec aux = function
      | i when i = nb_of_keys -> Find_res_not_found
      | i -> 
        if key = (Keys.get keys i) 
        then Find_res_val (Vals.get vals i)
        else aux (i + 1) 
    in
    aux 0
  
  type debug_res = 
    | Debug_res_read_data of block * debug_res_continuation 
    | Debug_res_done  

  and debug_res_continuation = bytes -> debug_res  

  let indent_string  n = String.make n ' ' 


  let rec debug indent offset m = 

    let on_disk = {offset; m} in 

    let continuation = fun bytes -> 

      let {
        on_disk = {offset; m}; 
        k; 
        keys; vals; subs; _ } =  make_as_bytes ~on_disk ~bytes ()in 

      let nb_of_vals  = nb_of_vals k in 

      if is_leaf k 
      then begin  
        let keys = String.concat ", " (
          Keys.get_n keys nb_of_vals
          |> Array.to_list 
          |> List.map Key.to_string 
        ) in 
        let vals = String.concat ", " (
          Vals.get_n vals nb_of_vals
          |> Array.to_list 
          |> List.map Val.to_string 
        ) in 
        Printf.printf "%s+- Leaf [%06i] : [%s] | [%s] \n" (indent_string indent)
        offset keys vals;
        Debug_res_done
      end
      else begin 
        Printf.printf "%s+- Node [%06i] \n" (indent_string indent) offset;

        let rec aux i = 

          let rec aux2 = function
            | Debug_res_done -> 
              if i = nb_of_vals 
              then Debug_res_done 
              else begin  
                Printf.printf "%s|- Key [%02i] : %s\n" 
                  (indent_string (indent + 1)) i (Keys.get keys i |> Key.to_string); 
                aux (i + 1)
              end 

            | Debug_res_read_data (block, continuation) ->
              let continuation' = fun bytes -> 
                continuation bytes |> aux2 
              in 
              Debug_res_read_data (block, continuation') 
          in
          aux2 @@ debug (indent + 1) (Ints.get subs i) m 
        in
        aux 0 
      end
    in 

    let node_block = node_block on_disk () in 
    Debug_res_read_data (node_block, continuation) 

end 
