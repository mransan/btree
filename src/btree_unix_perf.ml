
module String8 = struct 
  type t = string 

  let length = 8 

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
  (Printf.sprintf "0%07i" i, Printf.sprintf "%07i0" i) 

let () = 

  let filename = "data" in 
  let btree = S8BT.make ~filename ~m:201 () in 
  
  let nb_of_inserts = 200_000 in 

  let rec aux btree = function
    | 0 -> btree 
    | i -> 
      let key, value = make_test_key_val (Random.int nb_of_inserts) in 
      let btree = S8BT.insert btree key value in 
      aux btree (i - 1) 
  in 
  
  let t0 =  Unix.gettimeofday () in 
  let btree = aux btree nb_of_inserts in
  let t1 = Unix.gettimeofday () in 
  let rate = (float_of_int nb_of_inserts ) /. (t1 -. t0) in 
  Printf.printf "Write Rate: %f\n" rate;
  
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
  let rate = (float_of_int nb_of_inserts ) /. (t2 -. t1) in 
  Printf.printf "Read Rate: %f\n" rate

