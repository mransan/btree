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
        bytes (pos + 0) 
        (char_of_int Int32.(to_int (logand 0xffl i)));
    Bytes.unsafe_set 
        bytes (pos + 1) 
        (char_of_int Int32.(to_int (logand 0xffl (shift_right i 8))));
    Bytes.unsafe_set 
        bytes (pos + 2) 
        (char_of_int Int32.(to_int (logand 0xffl (shift_right i 16))));
    Bytes.unsafe_set 
        bytes (pos + 3) 
        (char_of_int Int32.(to_int (logand 0xffl (shift_right i 24))))
end 

module type Fixed_size_sig = sig 
  type t 
  val length : int 
  val to_bytes : t -> bytes -> int -> unit  
  val of_bytes : bytes -> int -> t 
  val to_string : t -> string 
end 

module type Comparable_sig = sig 
  type t 
  val compare : t -> t -> int
end 

module type Key_sig = sig
  type t 
  include Fixed_size_sig with type t := t 
  include Comparable_sig with type t := t 
end 

module type Val_sig = sig 
  type t 
  include Fixed_size_sig with type t := t 
end 

type file_offset = int

type block_length = int 

(** File block *)
type block = {
  offset: file_offset; 
  length: block_length;
}

let make_block ~offset ~length () = {offset; length; } 

let string_of_block {offset; length} = 
  Printf.sprintf "{offset: %i, length: %i}" offset length

type write_op = {
  offset : file_offset; 
  bytes : bytes;
} 

let make_write_op ~offset ~bytes () = {offset; bytes} 

type read_op = block 

(** This module provide similar interface as [Array] module for a 
    [bytes] array containing serialized [Fixed_size_sig] values. 
  *)
module FS_array(FS:Fixed_size_sig) =  struct 

  type t = {
    offset: file_offset;  (* where in [byte] does the array starts *) 
    bytes: bytes; (* contains [Fixed_size_sig] values starting at [offset] *) 
  } 

  let make ~offset ~bytes () = {offset; bytes}

  let unsafe_get {offset; bytes} i = 
    FS.of_bytes bytes (offset + i * FS.length)

  let get ({offset; bytes} as t) i = 
    if Bytes.length bytes < offset + (i + 1) * FS.length ||
       i < 0 
    then invalid_arg "FS_array.get"
    else unsafe_get t i  

  let unsafe_set {offset; bytes} i v = 
    FS.to_bytes v bytes (offset + i * FS.length) 

  let set ({offset; bytes} as t) i v = 
    if Bytes.length bytes < offset + (i + 1) * FS.length || 
       i < 0 
    then invalid_arg "FS_array.set"
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
    then invalid_arg "FS_array.get_n"
    else unsafe_get_n t n 

  let blit t1 o1 t2 o2 len = 
    (* TODO add checks *)
    let {offset = offset1; bytes = bytes1 } = t1 in 
    let {offset = offset2; bytes = bytes2 } = t2 in 
    let bo1 = offset1 + o1 * FS.length in 
    let bo2 = offset2 + o2 * FS.length in 
    Bytes.blit bytes1 bo1 bytes2 bo2 (len * FS.length)

  (* n is the number of element prior to the insert *)
  let unsafe_insert_shift_right ({offset; bytes} as t) pos n v = 
    blit t pos t (pos + 1) (n - pos); 
    FS.to_bytes v bytes (offset + pos * FS.length)

  let insert_shift_right ({offset; bytes} as t) pos n v = 
    if pos  < 0  ||
       pos  > n  ||   (* we allow append at the end *)
       (Bytes.length bytes < offset + (n + 1) * FS.length)
    then invalid_arg (Printf.sprintf "FS_array.insert (pos: %i, n: %i)" pos n)
    else unsafe_insert_shift_right t pos n v 

  let insert_pop_left t pos v = 
    let ret = get t 0 in 
    begin 
      if pos <> 0 
      then blit t 1 t 0 pos 
    end;
    set t pos v; 
    ret  
  
  let insert_pop_right t pos n v = 
    let ret = get t (n - 1) in 
    blit t pos t (pos + 1) (n - pos);
    set t pos v; 
    ret  

end 

module Make (Key:Key_sig) (Val:Val_sig) = struct  

  module Keys = FS_array(Key) 
  module Vals = FS_array(Val)
  module Ints = FS_array(Int)
  
  type 'a res = 
    | Res_done of 'a 
    | Res_read_data of block *'a res_read_data_k 
    | Res_allocate of block_length * 'a res_allocate_k 

  and 'a res_read_data_k = bytes -> 'a res 

  and 'a res_allocate_k = file_offset -> 'a res 

  let res_done x = Res_done x
  let res_read_data block k = Res_read_data (block, k) 
  let res_allocate block_length k = Res_allocate (block_length, k) 
  
  let rec res_bind f = function
    | Res_done x -> f x 

    | Res_read_data (block, k) -> 
      Res_read_data (block, fun bytes -> k bytes |> res_bind f) 

    | Res_allocate (length, k) -> 
      Res_allocate (length, fun offset -> k offset |> res_bind f)

  let rec res_map f = function
    | Res_done x -> Res_done (f x) 

    | Res_read_data (block, k) -> 
      Res_read_data (block, fun bytes -> k bytes |> res_map f) 

    | Res_allocate (length, k) -> 
      Res_allocate (length, fun offset -> k offset |> res_map f)

  type t = {
    root_file_offset : file_offset;
    m : int;
  }

  type node_on_disk = {
    offset: int; 
    m : int; 
  }

  type node_in_memory = {
    on_disk : node_on_disk;
    k : int;
    keys: Keys.t; 
    vals: Vals.t;
    subs: Ints.t;
    bytes: bytes;
  }

  (* Note on node representation 
   *
   * A B-Tree node has 2 representation: on disk and in memory. 
   *
   * The on disk representation solely contains the location 
   * on the storage. 
   * The in memory representation contains the byte read from the storage. Those
   * bytes are now in memory and read/manipulated to perform the various B-Tree 
   * algorithm. 
   *
   * Only the 'on disk' representation is exposed in the interface for the
   * client application to specify the on disk location of the root node of the
   * B-Tree.  *)

  let make_on_disk ~offset ~m () = {offset; m}

  let make ~root_file_offset ~m () = {root_file_offset; m;} 

  let node_of_t {root_file_offset; m} = {offset = root_file_offset; m;}

  let is_leaf k = 
    (* we use the sign of k to determine whether the node is a
       leaf node or an intermediate node *)
    k <= 0 
  
  let incr_k k = 
    if k <= 0 then k - 1 else k + 1 

  let nb_of_vals = Pervasives.abs  

  let is_node_full ~k ~m () = 
    nb_of_vals k = (m - 1) 

  let nb_of_subs k = 
    nb_of_vals k + 1 
  
  let keys_offset _ = 
    Int.length

  let vals_offset m  =
    keys_offset m + ((m - 1) * Key.length)

  let subs_offset m = 
    vals_offset  m + ((m - 1) * Val.length)

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
  
  let sub_node_at ~subs ~pos ~m () = 
    let sub_offset = Ints.get subs pos in 
    make_on_disk  ~offset:sub_offset ~m () 

  let keys_vals_subs ~m ~bytes () =
    let keys = Keys.make ~offset:(keys_offset m) ~bytes () in 
    let vals = Vals.make ~offset:(vals_offset m) ~bytes () in 
    let subs = Ints.make ~offset:(subs_offset m) ~bytes () in
    (keys, vals, subs) 

  let make_as_bytes ~on_disk:({m;_ } as on_disk) ~bytes () = 
    let keys, vals, subs = keys_vals_subs ~m ~bytes () in 
    { on_disk; k = Int.of_bytes bytes 0; keys; vals; subs; bytes; }

  let node_write_op {bytes; on_disk = {offset; _}; _ } = 
    make_write_op ~offset ~bytes ()
  
  let initialize {root_file_offset = offset; m} = 
    let length = node_length_of_m m in 
    let bytes = Bytes.create length in 
    let k = 0 in 
    Int.to_bytes k bytes 0; 
    (* no keys, vals, subs values -> we leave bytes with uninitialized 
     * values *)
    make_write_op ~offset ~bytes ()

  type insert_res_data = 
    | Insert_res_done of (file_offset option * write_op list)  
      (** [Insert_res_done (Some root_offset, write_ops], the key/value was 
          successfully inserted. [root_offset] is the new root offset of the 
          tree which should then be used in subsequent inserts/find/debug while 
          [write_ops] are the write operation necessary for the inserts to 
          take effect. *)
    | Insert_res_node_split of (Key.t * Val.t * int * write_op list) 
      (** Internal, should never be returned *)

  let insert_res_done ?root_file_offset ?write_ops:(write_ops = []) () = 
    Insert_res_done (root_file_offset, write_ops) 

  let insert_res_node_split k v offset write_ops = 
    Insert_res_node_split (k, v, offset,write_ops)

  type insert_res = insert_res_data res 
  
  (* returns the position index in which a new key/value should 
   * be inserted. *)
  let find_key_insert_position node key = 

    let rec aux keys key lower upper = 
      match upper - lower with
      | 0 | 1 -> `Insert_at upper
         (* The [upper] value is chosen because the [FS_array.insert] 
          * function will insert values by shifting the the value to the right *) 
      | _ -> 
        let median = (lower + upper) / 2  in 
        match Key.compare key (Keys.get keys median) with
        | 0 -> `Update_at median 
        | c when c > 0 -> aux keys key median upper 
        | _ -> aux keys key lower median 
    in
    
    let {keys; k; _ } = node in 
    let nb_of_vals = nb_of_vals k in

    (* Boundary condition check first *)

    if nb_of_vals = 0 
    then 
      (* When the btree is empty then the root node will 
       * have 0 key/values *)
      `Insert_at 0 
    else 
      let lower = 0 in 
      let upper = nb_of_vals - 1 in 

      (* Boundary check if the [key] is outside of the initial
       * [lower] and [upper] bound. The [aux] function which implements
       * the binary search relies on that *) 
      match Key.compare key (Keys.get keys lower) with
      | 0 -> `Update_at lower 
      | c when c < 0 -> `Insert_at lower 
      | _ -> 
         match Key.compare key (Keys.get keys upper) with
         | 0 -> `Update_at upper 
         | c when c > 0 -> `Insert_at (upper+1) 
         | _ -> aux keys key lower (upper + 1) 

  let make_new_root_node left_node right_node_offset key value write_ops = 
    let { on_disk = {offset; m; }; _} =  left_node in 
    let length = node_length_of_m m in  
    res_allocate length (fun root_file_offset -> 
      let bytes = Bytes.create length in 
      let k = 1 in 
      let keys, vals, subs = keys_vals_subs ~m ~bytes () in 
      
      Int.to_bytes k bytes 0; 
      Keys.set keys 0 key; 
      Vals.set vals 0 value;
      Ints.set subs 0 offset;
      Ints.set subs 1 right_node_offset;

      let new_root_write_op = 
        make_write_op ~offset:root_file_offset ~bytes () 
      in 
      let write_ops = new_root_write_op :: write_ops in 
      res_done @@ insert_res_done ~root_file_offset ~write_ops ()
    )

  let insert_at_pos 
      ~is_root ~node ~pos ~key ~value 
      ?right_sub:(right_sub = 0) ?write_ops:(write_ops = []) () = 

    let {
      on_disk = {m; _ };
      k; 
      keys;vals;subs; 
      bytes; _ 
    } = node in  

    let needs_split = is_node_full ~k ~m () in

    if needs_split 
    then begin  
      (* There must be an even number of values so that 
       * they can be evenly split between 2 nodes: the current
       * node which is full and the new node which will
       * be allocated/created below *)

      assert(nb_of_vals k mod 2 = 0); 

      let k = k / 2 in 
        (* the number of values is split in half between the 
         * current node and the newly allocated [right_node] *)

      res_allocate (node_length_of_m m) (fun right_node_offset -> 

        Int.to_bytes k bytes 0;

        let right_node_bytes = Bytes.create (node_length_of_m m) in 
        Int.to_bytes k right_node_bytes 0; 
        let (
          right_keys, 
          right_vals, 
          right_subs
        ) = keys_vals_subs ~m ~bytes:right_node_bytes () in 

        let right_median_i = nb_of_vals k in 
        let nb_of_vals' = nb_of_vals k in 
        let nb_of_subs' = nb_of_subs k in 

        Keys.blit keys (right_median_i) right_keys 0 nb_of_vals';  
        Vals.blit vals (right_median_i) right_vals 0 nb_of_vals';  
        Ints.blit subs (right_median_i) right_subs 0 nb_of_subs';

        let (
          median_key, 
          median_value
        ) = match Pervasives.compare pos right_median_i with
          | 0 -> begin  
            Ints.set right_subs 0 right_sub; 
            (key, value) 
          end
          | c when c > 0 -> begin 
            let pos = pos - (right_median_i + 1) in 
            let key = Keys.insert_pop_left right_keys pos key in 
            let value = Vals.insert_pop_left right_vals pos value in 
            let _ = Ints.insert_pop_left right_subs (pos + 1) right_sub in 
            (key, value)
          end  
          | _ -> begin
            let key = Keys.insert_pop_right keys pos nb_of_vals' key in 
            let value = Vals.insert_pop_right vals pos nb_of_vals' value in 
            let _ = Ints.insert_pop_right subs (pos + 1) nb_of_subs' right_sub in 
            (key, value)
          end
        in 

        let write_ops = 
          node_write_op node ::
          make_write_op ~offset:right_node_offset ~bytes:right_node_bytes () :: 
          write_ops 
        in 

        if is_root
        then 
          make_new_root_node 
              node right_node_offset median_key median_value write_ops 
        else 
          res_done @@ insert_res_node_split 
              median_key median_value right_node_offset write_ops
      ) 
    end
    else begin  
      (* no split needed *)
      Keys.insert_shift_right keys pos (nb_of_vals k) key;
      Vals.insert_shift_right vals pos (nb_of_vals k) value;
      Ints.insert_shift_right subs (pos + 1) (nb_of_subs k) right_sub; 
      Int.to_bytes (incr_k k) bytes 0; 
      res_done @@ insert_res_done ~write_ops:(node_write_op node :: write_ops) ()
    end

  let rec insert ?is_root (node:node_on_disk) key value = 
    res_read_data (node_block node ()) (fun bytes -> 
      let node =  make_as_bytes ~on_disk:node ~bytes () in 
      insert_as_bytes ?is_root node key value
    ) 

  and insert_as_bytes ?is_root:(is_root = true) node key value = 

    let {
      on_disk = {m; offset;}; 
      k; 
      keys; vals; subs;
      bytes;
    } = node in 

    match find_key_insert_position node key with
    | `Update_at pos -> begin 
      Vals.set vals pos value; 
      Keys.set keys pos key;
      let write_op = make_write_op ~offset ~bytes () in 
      res_done @@ insert_res_done ~write_ops:[write_op] ()
    end
    | `Insert_at pos -> begin 
      if is_leaf k 
      then 
        insert_at_pos ~is_root ~node ~pos ~key ~value () 
      else 
        insert ~is_root:false (sub_node_at ~subs ~pos ~m ()) key value 
        |> res_bind (fun insert_res -> 
          match insert_res with 
          | Insert_res_done _ -> res_done insert_res 
          | Insert_res_node_split (key, value, right_sub, write_ops) -> 
            insert_at_pos 
                ~is_root ~node ~pos ~key ~value ~right_sub ~write_ops ()  
        ) 
    end

  let insert t key value = insert (node_of_t t) key value 

  let rec append ?is_root (node:node_on_disk) key value = 
    res_read_data (node_block node ()) (fun bytes -> 
      let node =  make_as_bytes ~on_disk:node ~bytes () in 
      append_as_bytes ?is_root node key value
    ) 

  and append_as_bytes ?is_root:(is_root = true) node key value = 

    let {on_disk = {m; _;}; k; subs; _ } = node in 

    let pos = nb_of_vals k in 
    
    if is_leaf k 
    then 
      insert_at_pos ~is_root ~node ~pos ~key ~value () 
    else 
      let sub_node = make_on_disk ~offset:(Ints.get subs pos) ~m () in 
      append ~is_root:false sub_node key value 
      |> res_bind (fun insert_res -> 
        match insert_res with 
        | Insert_res_done _ -> res_done insert_res 
        | Insert_res_node_split (key, value, right_sub, write_ops) -> 
          insert_at_pos 
              ~is_root ~node ~pos ~key ~value ~right_sub ~write_ops ()  
      ) 

  let append t key value = append (node_of_t t) key value

  type find_res_data = Val.t option  

  type find_res = find_res_data res

  let rec find (node:node_on_disk) key = 
    res_read_data (node_block node ()) (fun bytes -> 
      find_as_bytes (make_as_bytes ~on_disk:node ~bytes ()) key
    ) 
    
  and find_as_bytes node key = 
    let {k; vals; subs; on_disk = {m; _}; _ } = node in 
    match find_key_insert_position node key with
    | `Update_at i -> res_done (Some (Vals.get vals i))
    | `Insert_at i -> 
      if is_leaf k 
      then res_done None 
      else 
        let sub_offset = Ints.get subs i in 
        let sub_node = make_on_disk ~offset:sub_offset ~m () in 
        find sub_node key

  type find_gt_res = Val.t list res  

  type counter = {
    mutable i : int ; 
    max : int; 
  }
  (* counter type to keep track (efficiently) of the 
   * current number of values collected and check that 
   * the requested [max] number is reached or not. 
   *
   * Note that this creates an invariant that the [i] 
   * value in the counter should be equal to the [List.length out_vals]
   * while collecting values. The counter is therefore used for 
   * efficiency reason since [List.length] has linear complexity. 
   *)

  let counter_make max = {i = 0; max }

  let counter_incr c = 
    c.i <- c.i + 1 
  
  let counter_is_max_reached {i; max} = 
    i >= max  

  let collect_gt out_vals counter vals pos nb_of_vals = 
    assert(pos <= nb_of_vals);
    assert(pos >=0);
      (* otherwise loop below won't terminate *)
    let rec aux out_vals = function 
      | i when i = nb_of_vals || counter_is_max_reached counter -> out_vals 
      | i -> begin 
        counter_incr counter; 
        aux ((Vals.get vals i) :: out_vals ) (i + 1)
      end
    in
    aux out_vals pos 

  let rec find_gt (node:node_on_disk) out_vals counter key : find_gt_res = 
    res_read_data (node_block node ()) (fun bytes -> 
      find_gt_as_bytes (make_as_bytes ~on_disk:node ~bytes ()) out_vals counter key
    ) 

  and find_gt_as_bytes node out_vals counter key = 
    let {k; vals; subs; on_disk = {m; _}; _ } = node in 
    let pos = 
      match find_key_insert_position node key with
      | `Update_at pos -> pos + 1 
      | `Insert_at pos -> pos 
    in 
    if is_leaf k 
    then 
      res_done (collect_gt out_vals counter vals pos (nb_of_vals k)) 
    else 

      let rec aux out_vals counter pos = 
        let sub_node = sub_node_at ~subs ~pos ~m () in 
        find_gt sub_node out_vals counter key |> res_bind (function
          | [] as out_vals 
          | out_vals when not (counter_is_max_reached counter) -> 
            (* If no values was found (or not enough values are collected 
             * so far) in the sub nodes then this means
             * that the first value greater than the key is in this 
             * intermediate node at [pos] and we should then 
             * continue the interation with the sub node next to the right
             * of this value. If this already was the right most 
             * sub node then this operation will be done by the upper
             * node (and so forth until the root).  
             *)
            if pos = nb_of_subs k - 1
            then res_done out_vals 
            else begin 
              let out_vals = (Vals.get vals pos) :: out_vals in 
              counter_incr counter; 
              aux out_vals counter (pos + 1) 
            end
          | out_vals -> res_done out_vals 
        ) 
      in 
      aux out_vals counter pos 

  let find_gt t key max = 
    let counter = counter_make max in 
    find_gt (node_of_t t) [] counter key |> res_map List.rev 

  let find t key = find (node_of_t t) key 
  
  type debug_res = unit res  

  let indent_string n = String.make n ' ' 
    (* add memoization *)

  let rec debug ?indent:(indent = 0) node = 
    let node_block = node_block node () in 
    res_read_data node_block (fun bytes ->

      let {
        on_disk = {offset = _ ; m}; 
        k; 
        keys; subs; _ 
      } =  make_as_bytes ~on_disk:node ~bytes () in 

      let nb_of_vals  = nb_of_vals k in 

      if is_leaf k 
      then begin  
        let keys_values = Keys.get_n keys nb_of_vals in 
        Array.iter (fun key -> 
          Printf.printf "%s  +-- %s\n" 
              (indent_string indent) (Key.to_string key)
        ) keys_values;
        res_done ()
      end
      else 
        let rec aux i = 
          let sub = make_on_disk ~offset:(Ints.get subs i) ~m () in 
          debug ~indent:(indent + 1) sub
          |> res_bind (fun () ->
            if i = nb_of_vals 
            then res_done ()
            else begin  
              Printf.printf "%s|- %s\n" 
                  (indent_string indent) (Keys.get keys i |> Key.to_string); 
              aux (i + 1) 
            end 
          )
        in
        aux 0 
    )

  let debug t = debug (node_of_t t)
end (* Make *) 
