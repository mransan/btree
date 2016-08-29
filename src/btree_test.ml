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

let of_bytes_counter = ref 0 
let compare_counter = ref 0 

module String8 = struct 
  type t = string 

  let length = 8 

  let of_bytes bytes pos = 
    incr of_bytes_counter;
    Bytes.sub_string bytes pos length

  let to_bytes s bytes pos = 
    assert(String.length s = length); 
    Bytes.blit_string s 0 bytes pos length

  let compare (l:string) (r:string) = 
    incr compare_counter; 
    Pervasives.compare l r  

  let to_string x = x 

end 

module S8BT = Btree.Make(String8)(String8) 
module S8Btree_bytes = Btree_bytes.Make(String8)(String8)


let write_op_counter = ref 0 
let do_write_op storage ({Btree.offset; bytes; } : Btree.write_op) = 
  incr write_op_counter;
  printf "- writing to offset %i%s\n" 
    offset 
    (string_of_bytes bytes); 
  let length_to_write = Bytes.length bytes in 
  Bytes.blit 
    (* src *) bytes 0 
    (* dst *) storage offset 
    (* len *) length_to_write


let read_op_counter = ref 0 
let do_read_op storage ({Btree.offset;length} as block) = 
  incr read_op_counter;
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

let () = 
  print_test_banner 1; 
  let m = 3 in 

  let key =  "00000001" in
  let value =  "0000000A" in 
  let t = S8Btree_bytes.make ~m () in 
  let t = S8Btree_bytes.insert t key value in  
  
  (* printf "storage:%s\n" (string_of_bytes storage);
   *)

  let v  = S8Btree_bytes.find t "00000001" in 
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
let make_test_btree23_01 () =

  let m = 3 in  (* 2-3 Btree *)

  let insert s t = 
    let key, value = make_test_key_val s in 
    S8Btree_bytes.insert t key value 
  in 

  let t = 
    S8Btree_bytes.make ~m ()
    |> insert "12"
    |> insert "23"
    |> insert "18"
    |> insert "48"
    |> insert "33"
    |> insert "30"
  in
  t 

let () = 
  print_test_banner 2; 
  
  let t = make_test_btree23_01 () in 
  
  (* printf "storage:%s\n" (string_of_bytes storage); 
   *)

  let find s expected = 
    assert(expected = S8Btree_bytes.find t s)
  in

  find "00000012" (Some "12000000"); 
  find "00000018" (Some "18000000"); 
  find "00000023" (Some "23000000"); 
  find "00000030" (Some "30000000"); 
  find "00000033" (Some "33000000"); 
  find "00000048" (Some "48000000"); 
  find "00000052" None;
  ()

let assert_find2 t s expected = 
  match S8Btree_bytes.find t s with
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
  
  let insert s t = 
    let key, value = make_test_key_val s in 
    S8Btree_bytes.insert t key value 
  in 

  let t = 
    S8Btree_bytes.make ~m () 
    |> insert "BB"
    |> insert "DD" 
  in

  let find t s = 
    let key, value = make_test_key_val s in 
    assert_find2 t key value 
  in

  let t = insert "EE" t in
  find t "BB"; 
  find t "DD"; 
  find t "EE"; 

  let t = insert "AA" t in
  find t "BB"; 
  find t "DD"; 
  find t "EE"; 
  find t "AA"; 
  
  let t = insert "CC" t in 
  find t "BB"; 
  find t "DD"; 
  find t "EE"; 
  find t "AA"; 
  find t "CC"; 

  (* Make sure update works *)
  let key, _ = make_test_key_val "BB" in 
  let value' = "ZZ000000" in 
  let t = S8Btree_bytes.insert t key value' in 
  assert((Some value') = S8Btree_bytes.find t key);
  () 

let () = 

  print_test_banner 7;

  let t = make_test_btree23_01 () in 
  
  let key, value = make_test_key_val "15" in 
  let t = S8Btree_bytes.insert t key value in
  let find' s expected = 
    assert(expected = S8Btree_bytes.find t s)
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
  let t = S8Btree_bytes.insert t key value in 
  let find' s expected = 
    assert(expected = S8Btree_bytes.find t s) 
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

(* --------------------------------
 *
 *               |18-|
 *  +--------------+------------+
 *  |              |            |
 * |12-|          |23-|        
 *
 * --------------------------------
 *)

let make_test_btree23_02 () =
  let m = 3 in  (* 2-3 Btree *)

  let insert s t = 
    let key, value = make_test_key_val s in 
    S8Btree_bytes.insert t key value 
  in 

  let t = 
    S8Btree_bytes.make ~m ()
    |> insert "12"
    |> insert "23"
    |> insert "18"
  in
  t 

let () = 
  print_test_banner 8; 
  let t = make_test_btree23_02 () in 

  let find t s = 
    let key, value = make_test_key_val s in 
    assert_find2 t key value 
  in
  
  let insert t s  = 
    let key, value = make_test_key_val s in 
    S8Btree_bytes.insert t key value 
  in 

  let t = insert t "15" in  
  find t "12"; 
  find t "15"; 
  find t "18"; 
  find t "23"; 
  
  let t = insert t "16" in  
  find t "12"; 
  find t "15"; 
  find t "18"; 
  find t "23"; 
  find t "16"; 
  ()

let () = 
  print_test_banner 9; 
  let t = make_test_btree23_01 () in 
    
  let make_test_key_val i = 
    let s = Printf.sprintf "%02i" i in 
    make_test_key_val s 
  in 
  
  let key24, val24 = make_test_key_val 24 in 
  let t = S8Btree_bytes.insert t key24 val24 in 
  let find s expected = 
    assert_find2 t s expected
  in 
  find "00000012" "12000000"; 
  find "00000018" "18000000"; 
  find "00000023" "23000000"; 
  find "00000024" "24000000"; 
  find "00000033" "33000000"; 
  find "00000048" "48000000"; 
  () 

let run_random_inserts ~m ~nb_of_inserts () = 

  let t0 = Unix.gettimeofday () in 

  of_bytes_counter := 0; 
  compare_counter := 0;
  write_op_counter := 0;
  read_op_counter := 0;
  let root_offset = ref 0 in 
  
  let make_test_key_val i = 
    let s = Printf.sprintf "%04i" i in 
    make_test_key_val4 s 
  in 

  let write = 
    S8BT.write_leaf_node 
      ~keys:[||] ~vals:[||] ~offset:!root_offset ~m ()
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
  
  let t1 = Unix.gettimeofday () in 
  
  Array.iter (fun nb -> 
    let key, value = make_test_key_val nb in 
    find key value
  ) inserts;
  
  let t2 = Unix.gettimeofday () in 

  Printf.printf ( 
    "m = %03i, node: %06i, storage: %010i, of_bytes: %06i, compare: %06i, " ^^ 
    "write_op: %06i, read_op: %06i, " ^^ 
    "time insert: %08.4f, time find: %08.4f\n"
    )  
    m (S8BT.node_length_of_m m) (Bytes.length !storage) !of_bytes_counter !compare_counter 
    !write_op_counter !read_op_counter 
    (t1 -. t0) (t2 -. t1) 

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

let () = 
  print_test_banner 15;
  run_random_inserts ~m:101 ~nb_of_inserts ()


