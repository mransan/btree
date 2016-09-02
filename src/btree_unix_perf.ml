
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
  
  let nb_of_inserts = 100_000 in 
  let max_random = 10 * nb_of_inserts in 

  let rec aux btree = function
    | 0 -> btree 
    | i -> 
      let key, value = make_test_key_val (Random.int max_random) in 
      let btree = S8BT.insert btree key value in 
      aux btree (i - 1) 
  in 
  
  let t0 =  Unix.gettimeofday () in 
  let btree = aux btree nb_of_inserts in
  let t1 = Unix.gettimeofday () in 
  let write_rate = (float_of_int nb_of_inserts ) /. (t1 -. t0) in 
  
  let rec aux = function
    | 0 -> () 
    | i -> 
      let key, value = make_test_key_val (Random.int nb_of_inserts) in 
      begin match S8BT.find btree key with
      | None -> () 
      | Some v -> assert (v = value) 
      end; 
      aux (i - 1) 

  in 
  aux nb_of_inserts; 

  let t2 = Unix.gettimeofday () in
  let read_rate = (float_of_int nb_of_inserts ) /. (t2 -. t1) in 
  Printf.printf "m: %03i : write rate: %15.2f | read rate : %15.2f\n" 
      m write_rate read_rate

let () =  
  if Array.length Sys.argv < 2 
  then failwith "m argument missing"
  else 
    run ~m:(int_of_string Sys.argv.(1)) () 
