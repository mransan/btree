
module String8 = struct 
  type t = string 

  let length = 10

  let of_bytes bytes pos = 
    Bytes.sub_string bytes pos length

  let to_bytes s bytes pos = 
    assert(String.length s = length); 
    Bytes.blit_string s 0 bytes pos length

  let compare (l:string) (r:string) = 
    Pervasives.compare l r  

  let to_string x = x 

end 

module S8BT = Btree_unix.Make(String8)(String8) 

let make_test_key_val i = 
  (Printf.sprintf "%010i" i, Printf.sprintf "%010i" i) 

let run ~m () = 
  let filename = Printf.sprintf "%04i.data" m in 
  let btree = S8BT.make ~filename ~m () in 
  
  let nb_of_inserts = 400_000 in 
  let max_random = 10 * nb_of_inserts in 

  let rec insert_aux btree = function
    | 0 -> btree 
    | i -> 
      let key, value = make_test_key_val (Random.int max_random) in 
      let btree = S8BT.insert btree key value in 
      insert_aux btree (i - 1) 
  in 
  
  let t0 =  Unix.gettimeofday () in 
  let btree = insert_aux btree nb_of_inserts in
  let t1 = Unix.gettimeofday () in 
  let insert_rate = (float_of_int nb_of_inserts ) /. (t1 -. t0) in 
  
  let rec append_aux btree = function
    | i when i = (max_random + nb_of_inserts) -> btree 
    | i -> 
      let key, value = make_test_key_val i in 
      let btree = S8BT.append btree key value in 
      append_aux btree (i + 1)
  in 
  let btree = append_aux btree max_random  in   
  let t2 = Unix.gettimeofday () in 

  let append_rate = (float_of_int nb_of_inserts) /. (t2 -. t1) in 
  
  let rec read_aux = function
    | 0 -> () 
    | i -> 
      let key, value = make_test_key_val (Random.int max_random) in 
      begin match S8BT.find btree key with
      | None -> () 
      | Some v -> assert (v = value) 
      end; 
      read_aux (i - 1) 

  in 

  let nb_of_reads = 2 * nb_of_inserts in 
  read_aux nb_of_reads; 

  let t3 = Unix.gettimeofday () in
  let read_rate = (float_of_int nb_of_reads) /. (t3 -. t2) in 
  Printf.printf "m: %03i : insert rate: %15.2f | append rate: %15.2f | read rate : %15.2f\n" 
      m insert_rate append_rate read_rate

let () =  
  if Array.length Sys.argv < 2 
  then failwith "m argument missing"
  else 
    run ~m:(int_of_string Sys.argv.(1)) () 
