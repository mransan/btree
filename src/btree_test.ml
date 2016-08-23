let is_debug = false

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

let printf = 
  if is_debug 
  then Printf.printf 
  else (fun fmt -> Printf.ifprintf stdout fmt)  

let print_test_banner i = 
  Printf.printf "\n-- Test [%03i] -- \n%!" i 

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
  printf "- writing to offset %i%s\n" 
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
  printf "- reading block: %s" (Btree.string_of_block block);
  let sub = Bytes.sub storage offset length in
  printf "%s\n" (string_of_bytes sub); 
  sub 

let do_allocate storage length = 
  printf "- allocating block of length: %i @ offset: %i\n" 
    length (Bytes.length storage);
  let offset = Bytes.length storage in 
  let storage = Bytes.extend storage 0 length in 
  (storage, offset) 

let find ~storage ~offset ~m ~key () = 
  
  let n = S8BT.make_on_disk ~offset ~m () in 

  let rec aux = function
    | S8BT.Find_res_val x -> Some x 
    | S8BT.Find_res_not_found -> None 
    | S8BT.Find_res_read_data (block, k) -> 
      do_read_op storage block |> k |> aux 
  in 
  aux (S8BT.find n key)

type insert_res = 
  | Insert_res_done of (int option * bytes) 
  | Insert_res_node_split of bytes * string * string * int * (Btree.write_op list)  

let insert ~storage ~offset ~m ~key ~value () = 
  let n = S8BT.make_on_disk ~offset ~m () in
  let rec aux storage = function
    | S8BT.Insert_res_done (new_root, write_ops) -> 
      List.iter (fun write_op -> 
        do_write_op storage write_op 
      ) write_ops; 
      Insert_res_done (new_root, storage)
    | S8BT.Insert_res_read_data (block, k) ->
      do_read_op storage block |> k |> aux storage  
    | S8BT.Insert_res_node_split (k, v, n_offset, write_ops)  -> 
      Insert_res_node_split (storage, k, v, n_offset, write_ops)  

    | S8BT.Insert_res_allocate (length, k) -> 
      let storage, offset = do_allocate storage length in 
      k offset |> aux storage  
  in
  aux storage (S8BT.insert n key value)

let debug storage offset m = 
  Printf.printf "\n-- btree dump --\n";
  let rec aux = function
    | S8BT.Debug_res_done  -> () 
    | S8BT.Debug_res_read_data (block, k) -> 
      do_read_op storage block |> k |> aux 
  in 
 aux @@ S8BT.debug (S8BT.make_on_disk ~offset ~m ()) 

let () = 
  print_test_banner 1; 
  let m = 3 in 
  let write = 
    let keys =  [| "00000001" |] in
    let vals =  [| "0000000A" |] in 
    S8BT.write_leaf_node ~keys ~vals ~offset:0 ~nb_of_vals:1  ~m () 
  in 
  
  let storage = Bytes.create (S8BT.node_length_of_m m) in 
  do_write_op storage write ; 

  printf "storage:%s\n" (string_of_bytes storage);

  let v  = find ~storage ~offset:0 ~m:3 ~key:"00000001" () in 
  match v with
  | None -> printf "Value not found"
  | Some x -> printf "Value found: %s\n" x

let make_test_key_val s = 
  assert(String.length s = 2);
  (Printf.sprintf "000000%s" s, Printf.sprintf "%s000000" s) 

let make_test_key_val4 s = 
  assert(String.length s = 4);
  (Printf.sprintf "0000%s" s, Printf.sprintf "%s0000" s) 


(* --------------------------------
 *
 *              |18-33|
 *  +--------------+------------+
 *  |              |            |
 * |12|         |23-30|        |48|
 *
 * --------------------------------
 *)
type btree23_01 = {
  storage : bytes;
  m : int;
}

let make_test_btree23_01 () =
  let m = 3 in  (* 2-3 Btree *)
  let node_length = S8BT.node_length_of_m m in

  let write_leaf_node offset key_strings  = 
    let nb_of_vals = List.length key_strings in 
    let keys, vals = List.fold_left (fun (keys, vals) s -> 
      let key, val_ = make_test_key_val s in
      (key::keys, val_::vals)
    ) ([], []) key_strings in 

    let keys = Array.of_list @@ List.rev keys in 
    let vals = Array.of_list @@ List.rev vals in 
    S8BT.write_leaf_node ~keys ~vals ~nb_of_vals ~m ~offset () 
  in

  let n48_offset = 3 * node_length in 
  let n48 = write_leaf_node n48_offset ["48"] in 

  let n23_offset = 2 * node_length in 
  let n23 = write_leaf_node n23_offset ["23";"30"] in 

  let n12_offset = node_length in
  let n12 = write_leaf_node n12_offset ["12"] in 
  
  let nroot = 
    let offset = 0 in 
    let key1, val1 = make_test_key_val "18" in 
    let key2, val2 = make_test_key_val "33" in 
    let keys = [| key1; key2 |] in 
    let vals = [| val1; val2 |] in 
    let subs = [|n12_offset; n23_offset; n48_offset|] in 
    S8BT.write_intermediate_node ~keys ~vals ~subs ~offset ~nb_of_vals:2 ~m () 
  in 
  
  let write_ops = [nroot; n48; n23; n12 ] in 
  let storage = Bytes.create (4 * node_length) in 
  do_write_ops storage write_ops; 

  {
    storage;
    m;
  }



let () = 
  print_test_banner 2; 
  let {
    storage;
    m;
    _
  }  = make_test_btree23_01 () in 
  
  printf "storage:%s\n" (string_of_bytes storage); 



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


let write_leaf_node_storage keys vals m = 
  assert(Array.length keys = Array.length vals); 
  let nb_of_vals = Array.length keys in 
  let offset = 0 in 
  let write_op = 
    S8BT.write_leaf_node ~keys ~vals ~nb_of_vals ~m ~offset () 
  in  
  let node_length = S8BT.node_length_of_m m in 
  let storage = Bytes.create node_length in 
  do_write_op storage write_op; 
  storage 

let assert_find ~storage ~offset ~m s expected = 
  match find ~storage ~offset ~m ~key:s () with
  | None -> begin 
    Printf.eprintf "- error key (%s) is not found \n" s; 
    assert(false)
  end 
  | Some v -> 
    if v = expected
    then () 
    else begin 
      Printf.eprintf "- unexpected value, got (%s), expected (%s)\n" 
        v expected; 
      assert(false)
    end 

let () = 
  print_test_banner 3;

  let m = 7 in 
  let key1, val1 = make_test_key_val "BB" in 
  let key2, val2 = make_test_key_val "DD" in 

  let storage = ref @@ 
    let keys, vals = ([| key1; key2 |], [| val1; val2 |])
    in 
    write_leaf_node_storage keys vals m 
  in 

  let offset = 0 in 
  
  printf "Leaf node created ... \n";

  let insert key value = 
    match insert ~storage:!storage ~offset ~m ~key ~value () with
    | Insert_res_done (None, storage') -> storage := storage'
    | _ -> assert(false)  
  in 

  let find s expected = 
    assert_find ~storage:!storage ~offset ~m s expected 
  in

  printf "Inserting EE\n";
  let key3, val3 = make_test_key_val "EE" in 
  insert key3 val3;  
  find key1 val1; 
  find key2 val2; 
  find key3 val3; 
  printf "Inserting AA\n";
  let key4, val4 = make_test_key_val "AA" in 
  insert key4 val4;  
  find key1 val1; 
  find key2 val2; 
  find key3 val3; 
  find key4 val4; 
  printf "Inserting CC\n";
  let key5, val5 = make_test_key_val "CC" in 
  insert key5 val5;  
  find key1 val1; 
  find key2 val2; 
  find key3 val3; 
  find key4 val4; 
  find key5 val5; 

  (* Make sure update works *)
  let val2' = "ZZ000000" in 
  insert key2 val2'; 
  find key2 val2'; 
  () 

let () = 

  print_test_banner 7;

  let {m; storage; _} = make_test_btree23_01 () in 
  
  let key, value = make_test_key_val "15" in 
  begin match insert ~storage ~offset:0 ~m ~key ~value () with
  | Insert_res_done (None, storage) -> 
    let find' s expected = 
      assert(expected = find ~storage ~offset:0 ~m ~key:s ())
    in
    find' "00000012" (Some "12000000"); 
    find' "00000015" (Some "15000000"); 
    find' "00000018" (Some "18000000"); 
    find' "00000023" (Some "23000000"); 
    find' "00000030" (Some "30000000"); 
    find' "00000033" (Some "33000000"); 
    find' "00000048" (Some "48000000"); 
    find' "00000052" None;

    let key, value = make_test_key_val "50" in 
    begin match insert ~storage ~offset:0 ~m ~key ~value () with
    | Insert_res_done (None, storage) ->
      let find' s expected = 
        assert(expected = find ~storage ~offset:0 ~m ~key:s ())
      in
      find' "00000012" (Some "12000000"); 
      find' "00000015" (Some "15000000"); 
      find' "00000018" (Some "18000000"); 
      find' "00000023" (Some "23000000"); 
      find' "00000030" (Some "30000000"); 
      find' "00000033" (Some "33000000"); 
      find' "00000048" (Some "48000000"); 
      find' "00000050" (Some "50000000"); 
      find' "00000052" None;
      ()
    | _ -> assert(false)
    end
  | _ -> assert(false) 
  end; 
  () 

(* --------------------------------
 *
 *               |18-|
 *  +--------------+------------+
 *  |              |            |
 * |12-|          |23-|        
 *
 * --------------------------------
 *)
type btree23_02 = {
  storage : bytes;
  m : int;
}

let make_test_btree23_02 () : btree23_02 =
  let m = 3 in  (* 2-3 Btree *)
  let node_length = S8BT.node_length_of_m m in

  let write_leaf_node offset key_strings  = 
    let nb_of_vals = List.length key_strings in 
    let keys, vals = List.fold_left (fun (keys, vals) s -> 
      let key, val_ = make_test_key_val s in
      (key::keys, val_::vals)
    ) ([], []) key_strings in 

    let keys = Array.of_list @@ List.rev keys in 
    let vals = Array.of_list @@ List.rev vals in 
    S8BT.write_leaf_node ~keys ~vals ~nb_of_vals ~m ~offset () 
  in

  let n23_offset = 2 * node_length in 
  let n23 = write_leaf_node n23_offset ["23"] in 

  let n12_offset = node_length in
  let n12 = write_leaf_node n12_offset ["12"] in 
  
  let nroot = 
    let offset = 0 in 
    let key1, val1 = make_test_key_val "18" in 
    let keys = [| key1 |] in 
    let vals = [| val1 |] in 
    let subs = [|n12_offset; n23_offset |] in 
    S8BT.write_intermediate_node 
      ~keys ~vals ~subs ~offset ~nb_of_vals:1 ~m () 
  in 
  
  let storage = Bytes.create (3 * node_length) in 
  let write_ops = [nroot; n12; n23;] in
  do_write_ops storage write_ops; 

  {
    storage;
    m;
  }

let () = 
  print_test_banner 8; 
  let {storage; m; _} = make_test_btree23_02 () in 

  let storage = ref storage in 
  
  let find s expected = 
    assert_find ~storage:!storage ~offset:0 ~m s expected 
  in


  let key15, val15 = make_test_key_val "15" in 
  begin match insert ~storage:!storage ~offset:0 ~m ~key:key15 ~value:val15 () with
  | Insert_res_done (None, storage') -> begin  
    storage := storage';
    find "00000012" "12000000"; 
    find "00000015" "15000000"; 
    find "00000018" "18000000"; 
    find "00000023" "23000000"; 
  end
  | _ -> assert(false)
  end;

  let key16, val16 = make_test_key_val "16" in 
  begin match insert ~storage:!storage ~offset:0 ~m ~key:key16 ~value:val16 () with
  | Insert_res_done (None, storage') -> 
    storage := storage';
    find "00000012" "12000000"; 
    find "00000015" "15000000"; 
    find "00000016" "16000000"; 
    find "00000018" "18000000"; 
    find "00000023" "23000000"; 
  | _ -> assert(false)
  end;
  ()

let () = 
  print_test_banner 9; 
  let {m; storage; _ }:btree23_01= make_test_btree23_01 () in 
    
  let make_test_key_val i = 
    let s = Printf.sprintf "%02i" i in 
    make_test_key_val s 
  in 
  
  let key24, val24 = make_test_key_val 24 in 
  begin match insert ~storage ~offset:0 ~m ~key:key24 ~value:val24 () with
  | Insert_res_done (Some new_root, storage) -> 
    let find s expected = 
      assert_find ~storage ~offset:new_root ~m s expected
    in 
    find "00000012" "12000000"; 
    find "00000018" "18000000"; 
    find "00000023" "23000000"; 
    find "00000024" "24000000"; 
    find "00000033" "33000000"; 
    find "00000048" "48000000"; 
    () 
  | _ -> assert(false)
  end

let run_random_inserts ~m ~nb_of_inserts () = 
  let root_offset = ref 0 in 
  
  let make_test_key_val i = 
    let s = Printf.sprintf "%04i" i in 
    make_test_key_val4 s 
  in 

  let write = 
    S8BT.write_leaf_node 
      ~keys:[||] ~vals:[||] ~offset:!root_offset ~nb_of_vals:0 ~m ()
  in 

  let storage = ref @@ Bytes.create (S8BT.node_length_of_m m) in 
  do_write_op !storage write;

  let inserts = Array.make nb_of_inserts 0 in  
  
  for i = 0 to nb_of_inserts - 1 do 
    let nb = Random.int 9999 in 
    Array.set inserts i nb;
    let key, value = make_test_key_val nb in 
    (*
    Printf.printf "inserting key: %s \n%!" key;
    *)
    begin match insert ~storage:!storage  ~offset:!root_offset ~m ~key ~value () with
    | Insert_res_done (new_root, storage') -> 
      storage := storage'; 
      begin match new_root with
      | None -> () 
      | Some new_root -> root_offset := new_root
      end
    | _ -> assert(false)
    end;
    printf "[%02i] root_offset: %06i, storage length: %06i\n" 
      i !root_offset (Bytes.length !storage); 
  done; 

  let find s expected = 
    match find ~storage:!storage ~offset:!root_offset ~m ~key:s () with
    | None -> begin 
      Printf.eprintf "- error key (%s) is not found \n" s; 
      assert(false)
    end 
    | Some v -> 
      if v = expected
      then () 
      else begin 
        Printf.eprintf "- unexpected value, got (%s), expected (%s)\n" 
          v expected; 
        assert(false)
      end 
  in
  
  Array.iter (fun nb -> 
    let key, value = make_test_key_val nb in 
    find key value
  ) inserts

let nb_of_inserts = 1000

let () = 
  Random.self_init ()

let () = 
  print_test_banner 11; 
  run_random_inserts ~m:3 ~nb_of_inserts ()

let () = 
  print_test_banner 12; 
  run_random_inserts ~m:5 ~nb_of_inserts ()

let () = 
  print_test_banner 13;
  run_random_inserts ~m:11 ~nb_of_inserts ()

let () = 
  print_test_banner 14;
  run_random_inserts ~m:51 ~nb_of_inserts ()

module TypedS8 = Btree.Typed_bytes(String8) 

let make_typeds8 n = 
  TypedS8.make ~offset:0 ~bytes:(Bytes.create (n * String8.length)) ()

let () = 
  
  let t = make_typeds8 5 in
  
  TypedS8.set t 0 "12345678"; 
  assert(TypedS8.get t 0 = "12345678");
  
  TypedS8.set t 4 "12345678"; 
  assert(TypedS8.get t 4 = "12345678");

  begin match TypedS8.set t 5 "12345678" with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false) 
  end;

  begin match TypedS8.set t (-1) "12345678" with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false) 
  end;

  begin match TypedS8.get t 5 with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false) 
  end;
  
  begin match TypedS8.get t (- 1) with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false) 
  end
let a1to5 = [|
  "00000001";
  "00000002";
  "00000003";
  "00000004";
  "00000005";
|]

let () =

  let t = make_typeds8 5 in

  TypedS8.set_n t a1to5; 
  let a' = TypedS8.get_n t 5 in 
  assert(a1to5 = a'); 

  begin match TypedS8.set_n t (Array.make 6 "") with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false);
  end;

  let t1 = make_typeds8 5 in 
  let t2 = make_typeds8 5 in

  let a1 = a1to5 in 
  let a2 = [|
    "0000000A";
    "0000000B";
    "0000000C";
    "0000000D";
    "0000000E";
  |] in
  TypedS8.set_n t1 a1;
  TypedS8.set_n t2 a2;

  TypedS8. blit t1 0 t2 0 5; 
  assert(a1 = TypedS8.get_n t2 5);
  
  TypedS8.set_n t1 a1;
  TypedS8.set_n t2 a2;
  TypedS8. blit t1 2 t2 3 2; 
  assert("0000000A" = TypedS8.get t2 0);
  assert("0000000B" = TypedS8.get t2 1);
  assert("0000000C" = TypedS8.get t2 2);
  assert("00000003" = TypedS8.get t2 3);
  assert("00000004" = TypedS8.get t2 4);

  assert(a1 = TypedS8.get_n t1 5);
  ()

let () =

  let t = make_typeds8 5 in 
    (* reset the bytes *)

  let a_minus3 = [|
    "00000001";
    "00000002";
    "00000004";
    "00000005";
  |] in 
  TypedS8.set_n t a_minus3; 

  TypedS8.insert t 2 (Array.length a_minus3) "00000003"; 
  assert(a1to5 = TypedS8.get_n t 5);
  () 

let () = 
  
  let t = make_typeds8 5 in
    (* reset the bytes *)

  let a_minus5 = [|
    "00000001";
    "00000002";
    "00000003";
    "00000004";
  |] in 
  TypedS8.set_n t a_minus5; 

  TypedS8.insert t 4 (Array.length a_minus5) "00000005"; 
  assert(a1to5 = TypedS8.get_n t 5)

let () =

  let t = make_typeds8 5 in 
    (* reset the bytes *)

  let a_minus5 = [|
    "00000001";
    "00000002";
    "00000003";
    "00000004";
  |] in 
  TypedS8.set_n t a_minus5; 

  TypedS8.insert t 4 (Array.length a_minus5) "00000005"; 
  assert(a1to5 = TypedS8.get_n t 5)

let () =

  let t = make_typeds8 5 in 
    (* reset the bytes *)

  let a_minus1 = [|
    "00000002";
    "00000003";
    "00000004";
    "00000005";
  |] in 
  TypedS8.set_n t a_minus1; 

  TypedS8.insert t 0 (Array.length a_minus1) "00000001"; 
  assert(a1to5 = TypedS8.get_n t 5)


