
let make_test_key_val7 s = 
  assert(String.length s = 7);
  (Printf.sprintf "0%s" s, Printf.sprintf "%s0" s) 


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
    let s = Printf.sprintf "%07i" i in 
    make_test_key_val7 s 
  in 

  let t = S8BT.make ~m () in 
  S8BT.Stats.reset t ; 

  let inserts = Array.make nb_of_inserts 0 in  

  let max_random = 10 * nb_of_inserts in 
  
  let rec aux t = function
    | i when i = nb_of_inserts -> t 
    | i  -> 
      let nb = Random.int max_random in 
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


let run_test ~m () = 
  let cmd = Printf.sprintf "%s %s run" Sys.argv.(0) Sys.argv.(1) in 
  Printf.printf "running: %s\n%!" cmd;
  match Sys.command cmd with
  | 0 -> 
    let cmd = Printf.sprintf "mv ocamlprof.dump btree_%03i.dump" m in 
    Printf.printf "running: %s\n%!" cmd;
    begin match Sys.command cmd with
    | 0 -> 
      let cmd1 = Printf.sprintf 
          "ocamlprof -f btree_%03i.dump src/btree.ml > btree_%03i.ml" m m in
      let cmd2 = Printf.sprintf 
          "ocamlprof -f btree_%03i.dump src/btree_bytes_perf.ml > btree_bytes_perf_%03i.ml" m m in
      
      let cmd = Printf.sprintf "%s && %s" cmd1 cmd2 in 
      Printf.printf "running: %s\n%!" cmd;
      exit (Sys.command cmd)
    | n -> exit n 
    end
  | n -> exit n

let () = 
  if Array.length Sys.argv < 2 
  then failwith "m argument missing"
  else 
    let m = int_of_string Sys.argv.(1) in 
    if Array.length Sys.argv = 3 
    then
      if Sys.argv.(2) = "run"
      then 
        run_random_inserts ~m ~nb_of_inserts:20000 () 
      else 
        failwith "Error, 2nd cmd line argument can only be 'run'"
    else
      run_test ~m () 
