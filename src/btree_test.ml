let print_test_banner i = 
  Printf.printf "\n-- Test [%03i] -- \n%!" i 


module String8 = struct 
  type t = string 

  let length = 8 

  let of_bytes_counter = ref 0 

  let compare_counter = ref 0 

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

module S8BT = Btree_bytes.Make(String8)(String8)

let () = 
  print_test_banner 1; 
  let m = 3 in 

  let key =  "00000001" in
  let value =  "0000000A" in 
  let t = S8BT.make ~m () in 
  let t = S8BT.insert t key value in  
  
  (* printf "storage:%s\n" (string_of_bytes storage);
   *)

  let v  = S8BT.find t "00000001" in 
  match v with
  | None -> assert(false)
  | Some v -> assert(v = "0000000A") 

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
    S8BT.insert t key value 
  in 

  let t = 
    S8BT.make ~m ()
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
    assert(expected = S8BT.find t s)
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
  match S8BT.find t s with
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
    S8BT.insert t key value 
  in 

  let t = 
    S8BT.make ~m () 
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
  let t = S8BT.insert t key value' in 
  assert((Some value') = S8BT.find t key);
  () 

let () = 

  print_test_banner 7;

  let t = make_test_btree23_01 () in 
  
  let key, value = make_test_key_val "15" in 
  let t = S8BT.insert t key value in
  let find' s expected = 
    assert(expected = S8BT.find t s)
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
  let t = S8BT.insert t key value in 
  let find' s expected = 
    assert(expected = S8BT.find t s) 
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
    S8BT.insert t key value 
  in 

  let t = 
    S8BT.make ~m ()
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
    S8BT.insert t key value 
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
  let t = S8BT.insert t key24 val24 in 
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


  String8.of_bytes_counter := 0; 
  String8.compare_counter := 0;
  
  let make_test_key_val i = 
    let s = Printf.sprintf "%04i" i in 
    make_test_key_val4 s 
  in 

  let t = S8BT.make ~m () in

  S8BT.Stats.reset t; 

  let inserts = Array.make nb_of_inserts 0 in  
  
  let rec aux t = function 
    | i when i = nb_of_inserts -> t 
    | i -> begin  
      let nb = Random.int 9999 in 
      Array.set inserts i nb;
      let key, value = make_test_key_val nb in 
      let t = S8BT.insert t key value in
      aux t (i + 1)
    end
  in 
  let t0 = Unix.gettimeofday () in 
  let t = aux t 0 in 
  let t1 = Unix.gettimeofday () in 

  let find s expected = 
    assert_find2 t s expected 
  in
  
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
    m (S8BT.Stats.node_length t) 0 
    !String8.of_bytes_counter !String8.compare_counter 
    (S8BT.Stats.write_count t) (S8BT.Stats.read_count t)
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


