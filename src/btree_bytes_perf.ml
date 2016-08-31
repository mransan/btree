
let make_test_key_val4 s = 
  assert(String.length s = 4);
  (Printf.sprintf "0000%s" s, Printf.sprintf "%s0000" s) 


module String8 = struct 
  type t = string 

  let of_bytes_counter = ref 0 
  let compare_counter = ref 0 

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

module S8BT = Btree_bytes.Make(String8)(String8) 

let run_random_inserts ~m ~nb_of_inserts () = 


  String8.of_bytes_counter := 0; 
  String8.compare_counter := 0;
  
  let make_test_key_val i = 
    let s = Printf.sprintf "%04i" i in 
    make_test_key_val4 s 
  in 

  let t = S8BT.make ~m () in 
  S8BT.Stats.reset t ; 

  let inserts = Array.make nb_of_inserts 0 in  
  
  let rec aux t = function
    | i when i = nb_of_inserts -> t 
    | i  -> 
      let nb = Random.int 9999 in 
      Array.set inserts i nb;
      let key, value = make_test_key_val nb in 
      let t  = S8BT.insert t key value in 
      aux t (i + 1)
  in 

  let t0 = Unix.gettimeofday () in 
  let t = aux t 0 in
  let t1 = Unix.gettimeofday () in 

  let find s expected = 
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
    m (S8BT.Stats.node_length t) (S8BT.Stats.storage_length t)
    !String8.of_bytes_counter !String8.compare_counter 
    (S8BT.Stats.write_count t) (S8BT.Stats.read_count t) 
    (t1 -. t0) (t2 -. t1);; 


let () = 
  if Array.length Sys.argv <> 2 
  then failwith "m argument missing"
  else 
    let m = int_of_string Sys.argv.(1) in 
    run_random_inserts ~m ~nb_of_inserts:100000 () 
