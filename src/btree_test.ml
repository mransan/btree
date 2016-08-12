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

module Int = Btree.Int 

module S8BT = Btree.Make(String8)(String8) 

let write_block storage (block, bytes_to_write) = 
  Printf.printf "- writing block: %s%s\n" 
    (Btree.string_of_block block) 
    (string_of_bytes bytes_to_write); 
  let {Btree.offset; _ ; } = block in 
  let length_to_write = Bytes.length bytes_to_write in 
  Bytes.blit 
    (* src *) bytes_to_write 0 
    (* dst *) storage offset 
    (* len *) length_to_write

let find storage n key = 
  let rec aux = function
    | S8BT.Find_res_val x -> Some x 
    | S8BT.Find_res_not_found -> None 
    | S8BT.Find_res_read_data ({Btree.offset; length} as block, k) -> 
      let sub = Bytes.sub storage offset length in
      Printf.printf "- reading block: %s%s\n" 
      (Btree.string_of_block block) (string_of_bytes sub); 
      aux @@ k @@ sub 
  in 
  aux (S8BT.find n key)

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
  List.iter (fun write -> write_block storage write) writes; 

  Printf.printf "storage:%s\n" (string_of_bytes storage);

  let n' = 
    match S8BT.make_leaf_node ~offset:0 ~nb_of_vals:1 ~m () with
    | S8BT.Make_result_node n -> n 
    | _ -> assert(false) 
  in 
  let v  = find storage n' "00000001" in 
  match v with
  | None -> Printf.printf "Value not found"
  | Some x -> Printf.printf "Value found: %s\n" x


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
      write_block storage write
    ) (S8BT.full_write n)
  ) all_nodes;

  Printf.printf "storage:%s\n" (string_of_bytes storage); 

  let nroot = 
    let offset = 0 in 
    match S8BT.make_intermediate_node ~nb_of_vals:2 ~m ~offset () with
    | S8BT.Make_result_node n -> n
    | _ -> assert(false)
  in 

  let find s expected = 
    assert(expected = find storage nroot s)
  in

  find "00000012" (Some "12000000"); 
  find "00000018" (Some "18000000"); 
  find "00000023" (Some "23000000"); 
  find "00000030" (Some "30000000"); 
  find "00000033" (Some "33000000"); 
  find "00000048" (Some "48000000"); 
  find "00000052" None;
  ()
