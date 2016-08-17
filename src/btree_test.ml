let hex = function
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'A'
  | 11 -> 'B'
  | 12 -> 'C'
  | 13 -> 'D'
  | 14 -> 'E'
  | 15 -> 'F'
  | _ -> assert(false)

let string_of_bytes bytes : string = 
  let len = Bytes.length bytes in 
  let rec aux = function
    | i when i = len -> [] 
    | i -> 
      let v = int_of_char (Bytes.get bytes i) in 
      let b1 = v / 16 in 
      let b2 = v mod 16 in 
      let eol= if i mod 8 = 0 then "\n | " else "" in 
      (Printf.sprintf "%s%c%c" eol (hex b1) (hex b2)):: aux (i + 1)

  in 
  String.concat " " (aux 0) 

module String8 = struct 
  type t = string 

  let length = 8 

  let of_bytes bytes pos = 
    Bytes.sub_string bytes pos length

  let to_bytes s bytes pos = 
    assert(String.length s = length); 
    Bytes.blit_string s 0 bytes pos length

  let compare (l:string) (r:string) = Pervasives.compare l r  

  let to_string x = x 

end 

module S8BT = Btree.Make(String8)(String8) 

let do_write_op storage ({Btree.offset; bytes; } : Btree.write_op) = 
  Printf.printf "- writing to offset %i%s\n" 
    offset 
    (string_of_bytes bytes); 
  let length_to_write = Bytes.length bytes in 
  Bytes.blit 
    (* src *) bytes 0 
    (* dst *) storage offset 
    (* len *) length_to_write

let do_write_ops storage write_ops = 
  List.iter (fun write_op -> 
    do_write_op storage write_op
  ) write_ops

let do_read_op storage ({Btree.offset;length} as block) = 
  Printf.printf "- reading block: %s" (Btree.string_of_block block);
  let sub = Bytes.sub storage offset length in
  Printf.printf "%s\n" (string_of_bytes sub); 
  sub 

let do_allocate storage length = 
  Printf.printf "- allocating block of length: %i\n" length;
  let offset = Bytes.length storage in 
  let storage = Bytes.extend storage 0 length in 
  (storage, offset) 

let make_node_from_offset ~storage ~offset ~m () = 
  let rec aux = function
    | S8BT.Make_from_disk_node n -> n
    | S8BT.Make_from_disk_read_data ({Btree.offset; length}, k) -> 
      aux @@ k @@ Bytes.sub storage offset length
  in 
  aux (S8BT.make_node_from_offset ~offset ~m ()) 

let find ~storage ~offset ~m ~key () = 
  let n = make_node_from_offset ~storage ~offset ~m () in 
  let rec aux = function
    | S8BT.Find_res_val x -> Some x 
    | S8BT.Find_res_not_found -> None 
    | S8BT.Find_res_read_data (block, k) -> 
      do_read_op storage block |> k |> aux 
  in 
  aux (S8BT.find n key)

type insert_res = 
  | Insert_res_done of bytes 
  | Insert_res_node_split of bytes * string * string * S8BT.node * (Btree.write_op list)  

let insert ~storage ~offset ~m ~key ~value () = 
  let n = make_node_from_offset ~storage ~offset ~m () in
  let rec aux storage = function
    | S8BT.Insert_res_done write_ops -> 
      List.iter (fun write_op -> 
        do_write_op storage write_op 
      ) write_ops; 
      Insert_res_done storage
    | S8BT.Insert_res_read_data (block, k) ->
      do_read_op storage block |> k |> aux storage  
    | S8BT.Insert_res_node_split (k, v, n, write_ops)  -> 
      Insert_res_node_split (storage, k, v, n, write_ops)  
    | S8BT.Insert_res_allocate (length, k) -> 
      let storage, offset = do_allocate storage length in 
      k offset |> aux storage  
  in
  aux storage (S8BT.insert n key value)

let () = 
  Printf.printf "\n\n-- Test 01 -- \n\n";
  let m = 3 in 
  let n = 
    let keys =  [| "00000001" |] in
    let vals =  [| "0000000A" |] in 
    S8BT.make_leaf_node ~keys ~vals ~offset:0 ~nb_of_vals:1  ~m () 
  in 
  let n = match n with
    | S8BT.Make_result_node n -> n 
    | _ -> assert(false) 
  in 
  Printf.printf "Node created, length on disk: %i\n"
    (S8BT.node_length n);
  let writes = S8BT.full_write n in 
  
  let storage = Bytes.create (S8BT.node_length n) in 
  List.iter (fun write -> do_write_op storage write) writes; 

  Printf.printf "storage:%s\n" (string_of_bytes storage);

  let v  = find ~storage ~offset:0 ~m:3 ~key:"00000001" () in 
  match v with
  | None -> Printf.printf "Value not found"
  | Some x -> Printf.printf "Value found: %s\n" x

let make_test_key_val s = 
  assert(String.length s = 2);
  (Printf.sprintf "000000%s" s, Printf.sprintf "%s000000" s) 

let () = 
  Printf.printf "\n\n-- Test 02 -- \n\n";
   (* 
    *              |18-33|
    *  +--------------+------------+
    *  |              |            |
    * |12|         |23-30|        |48|
    *)
  let m = 3 in  (* 2-3 Btree *)
  let node_length = S8BT.node_length_of_m m in

  let make_test_key_val s = 
    assert(String.length s = 2);
    (Printf.sprintf "000000%s" s, Printf.sprintf "%s000000" s) 
  in

  let make_leaf_node offset key_strings  = 
    let nb_of_vals = List.length key_strings in 
    let keys, vals = List.fold_left (fun (keys, vals) s -> 
      let key, val_ = make_test_key_val s in
      (key::keys, val_::vals)
    ) ([], []) key_strings in 

    let keys = Array.of_list @@ List.rev keys in 
    let vals = Array.of_list @@ List.rev vals in 
    match S8BT.make_leaf_node ~keys ~vals ~nb_of_vals ~m ~offset () with
    | S8BT.Make_result_node n -> n 
    | _ -> assert(false) 
  in

  let n48_offset = 3 * node_length in 
  let n48 = make_leaf_node n48_offset ["48"] in 

  let n23_offset = 2 * node_length in 
  let n23 = make_leaf_node n23_offset ["23";"30"] in 

  let n12_offset = node_length in
  let n12 = make_leaf_node n12_offset ["12"] in 
  
  let nroot = 
    let offset = 0 in 
    let key1, val1 = make_test_key_val "18" in 
    let key2, val2 = make_test_key_val "33" in 
    let keys = [| key1; key2 |] in 
    let vals = [| val1; val2 |] in 
    let subtrees = [|n12_offset; n23_offset; n48_offset|] in 
    let res = 
      S8BT.make_intermediate_node ~keys ~vals ~subtrees ~offset ~nb_of_vals:2 ~m () 
    in 
    match res with
    | S8BT.Make_result_node n -> n
    | _ -> assert(false) 
  in 
  
  let all_nodes = [nroot; n12; n23; n48] in

  let storage = Bytes.create (4 * node_length) in 
  List.iter (fun n -> 
    List.iter (fun write -> 
      do_write_op storage write
    ) (S8BT.full_write n)
  ) all_nodes;

  Printf.printf "storage:%s\n" (string_of_bytes storage); 

  let find s expected = 
    assert(expected = find ~storage ~offset:0 ~m ~key:s ())
  in

  find "00000012" (Some "12000000"); 
  find "00000018" (Some "18000000"); 
  find "00000023" (Some "23000000"); 
  find "00000030" (Some "30000000"); 
  find "00000033" (Some "33000000"); 
  find "00000048" (Some "48000000"); 
  find "00000052" None;
  ()


let make_leaf_node_storage keys vals m = 
  assert(Array.length keys = Array.length vals); 
  let nb_of_vals = Array.length keys in 
  let offset = 0 in 
  let n = 
    S8BT.make_leaf_node ~keys ~vals ~nb_of_vals ~m ~offset () 
    |> function 
      | S8BT.Make_result_node n -> n 
      | _-> assert(false) 
  in  
  let node_length = S8BT.node_length n in 
  let storage = Bytes.create node_length in 
  List.iter (fun write_op -> 
    do_write_op storage write_op
  ) (S8BT.full_write n); 
  storage


let () = 
  Printf.printf "\n\n-- Test 03 -- \n\n";

  let m = 7 in 
  let key1, val1 = make_test_key_val "BB" in 
  let key2, val2 = make_test_key_val "DD" in 

  let storage = ref @@ 
    let keys, vals = ([| key1; key2 |], [| val1; val2 |])
    in 
    make_leaf_node_storage keys vals m 
  in 

  let offset = 0 in 
  
  Printf.printf "Leaf node created ... \n";

  let insert key value = 
    match insert ~storage:!storage ~offset ~m ~key ~value () with
    | Insert_res_done storage' -> storage := storage'
    | Insert_res_node_split _ -> assert(false)  
  in 

  let find s expected = 
    assert(expected = find ~storage:!storage ~offset ~m ~key:s ())
  in

  Printf.printf "Inserting EE\n";
  let key3, val3 = make_test_key_val "EE" in 
  insert key3 val3;  
  find key1 (Some val1); 
  find key2 (Some val2); 
  find key3 (Some val3); 
  Printf.printf "Inserting AA\n";
  let key4, val4 = make_test_key_val "AA" in 
  insert key4 val4;  
  find key1 (Some val1); 
  find key2 (Some val2); 
  find key3 (Some val3); 
  find key4 (Some val4); 
  Printf.printf "Inserting CC\n";
  let key5, val5 = make_test_key_val "CC" in 
  insert key5 val5;  
  find key1 (Some val1); 
  find key2 (Some val2); 
  find key3 (Some val3); 
  find key4 (Some val4); 
  find key5 (Some val5); 

  (* Make sure update works *)
  let val2' = "ZZ000000" in 
  insert key2 val2'; 
  find key2 (Some val2'); 

  () 

let () = 
  Printf.printf "\n\n-- Test 04 -- \n\n";
  (*
   * In this test we make sure the insertion of a new key/value on 
   * a full node with the new key being the median of all the values in 
   * the full node. 
   *
   * The expectation is that the key should be returned as being the median
   * value of the node splitting result. The existing key/values should be found
   * on either the previous or newly created node. 
   *)

  let m = 3 in 
  let key1, val1 = make_test_key_val "AA" in 
  let key2, val2 = make_test_key_val "CC" in 
  let storage = 
    let keys = [| key1; key2 |] in 
    let vals = [| val1; val2 |] in 
    make_leaf_node_storage keys vals m 
  in

  let node_length = S8BT.node_length_of_m m in 
  let offset = 0 in 
  
  Printf.printf "Leaf node created ... \n";

  let key3, val3 = make_test_key_val "BB" in 
  begin match insert ~storage ~offset ~m ~key:key3 ~value:val3 () with
  | Insert_res_done _ -> assert(false)
  | Insert_res_node_split (storage, k, v, n, write_ops) -> 
    let _ = write_ops in
    assert(Bytes.length storage = 2 * node_length);
      (* verify the allocation happened and storage resized *)

    assert(node_length = S8BT.node_offset n);
      (* verify that the offset was at the end of the initial storage size *)

    assert(k = key3); 
    assert(v = val3); 

    do_write_ops storage write_ops; 

    let find_first_node s expected = 
      assert(expected = find ~storage ~offset:0 ~m ~key:s ())
    in
    find_first_node key1 (Some val1);

    let find_second_node s expected = 
      assert(expected = find ~storage ~offset:node_length ~m ~key:s ())
    in
    find_second_node key2 (Some val2); 

  end;
  ()

let () = 
  let a = [|1;2;3;4|] in 
  let (s1, s2) = Btree.array_split_at a 2 in
  assert(s1 = [|1;2|]); 
  assert(s2 = [|3;4|]);
  let (s1, s2) = Btree.array_split_at a 1 in
  assert(s1 = [|1|]); 
  assert(s2 = [|2;3;4|]);
  let (s1, s2) = Btree.array_split_at a 0 in
  assert(s1 = [||]); 
  assert(s2 = [|1;2;3;4|]);
  let (s1, s2) = Btree.array_split_at a 3 in
  assert(s1 = [|1;2;3;|]); 
  assert(s2 = [|4|]);
  let (s1, s2) = Btree.array_split_at a 4 in
  assert(s1 = [|1;2;3;4|]); 
  assert(s2 = [||]);
  ()

let () =
  let a = [|1;2;3;4|] in 
  let p = Btree.array_insert_pop_left a 2 0 in 
  assert(p = 1); 
  assert(a = [|2;0;3;4|])

let () =
  let a = [|1;2;3;4|] in 
  let p = Btree.array_insert_pop_left a 1 0 in 
  assert(p = 1); 
  assert(a = [|0;2;3;4|])

let () =
  let a = [|1;2;3;4|] in 
  let p = Btree.array_insert_pop_left a 4 0 in 
  assert(p = 1); 
  assert(a = [|2;3;4;0|])

let () =
  let a = [|1;2;3;4|] in 
  let a'= Btree.array_insert a 0 0 in 
  assert(a' = [|0;1;2;3;4|]);
  let a'= Btree.array_insert a 1 0 in 
  assert(a' = [|1;0;2;3;4|]);
  let a'= Btree.array_insert a 4 0 in 
  assert(a' = [|1;2;3;4;0|]);
  ()

let () = 
  Printf.printf "\n\n-- Test 05 -- \n\n";
  (*
   * In this test we make sure the insertion of a new key/value on 
   * a full node with the key greater than the the median 
   * of all the keys of the full node. 
   *
   * The expectation is that the median value should be the node is split
   * and the key/value inserted in the newly created node
   *)

  let m = 3 in 
  let key1, val1 = make_test_key_val "AA" in 
  let key2, val2 = make_test_key_val "CC" in 
  let storage = 
    let keys = [| key1; key2 |] in 
    let vals = [| val1; val2 |] in 
    make_leaf_node_storage keys vals m 
  in

  let node_length = S8BT.node_length_of_m m in 
  let offset = 0 in 
  
  Printf.printf "Leaf node created ... \n";

  let key3, val3 = make_test_key_val "DD" in 
  begin match insert ~storage ~offset ~m ~key:key3 ~value:val3 () with
  | Insert_res_done _ -> assert(false)
  | Insert_res_node_split (storage, k, v, n, write_ops) -> 
    let _ = write_ops in
    assert(Bytes.length storage = 2 * node_length);
      (* verify the allocation happened and storage resized *)

    assert(node_length = S8BT.node_offset n);
      (* verify that the offset was at the end of the initial storage size *)

    assert(k = key2); 
    assert(v = val2); 

    do_write_ops storage write_ops; 

    let find_first_node s expected = 
      assert(expected = find ~storage ~offset:0 ~m ~key:s ())
    in
    find_first_node key1 (Some val1);

    let find_second_node s expected = 
      assert(expected = find ~storage ~offset:node_length ~m ~key:s ())
    in
    find_second_node key3 (Some val3); 

  end;
  ()

