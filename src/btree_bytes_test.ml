let test_type = `Fast

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

let run_insert_find_test ?verify_at_end ~m ~l () = 

  let verify_at_end = match verify_at_end with
    | None -> false
    | Some () -> true
  in 

  let make_test_key_val i = 
    let key = Printf.sprintf "0000%04i" i in 
    let value = Printf.sprintf "%04i0000" i in 
    (key, value) 
  in 

  let rec verify_inserted t = function
    | [] -> () 
    | i :: tl -> 
      let k, v = make_test_key_val i in 
      match S8BT.find t k with
      | None -> begin 
        Printf.eprintf "Error, key: %s not found \n" k; 
        assert(false)
      end
      | Some v' when v' <> v -> begin 
        Printf.eprintf "Error, mismatch value for key: %s, expected: %s, got: %s \n" 
          k v v'; 
        assert(false)
      end 
      | _ -> verify_inserted t tl 
  in 
  
  let rec aux t inserted = function
    | [] -> 
      if verify_at_end 
      then verify_inserted t inserted 
      else () 

    | i::tl -> begin  
      let k, v = make_test_key_val i in 
      let t = S8BT.insert t k v in 
      let inserted = i :: inserted in 
      begin 
        if not verify_at_end 
        then verify_inserted t inserted
        else ()
      end;
      aux t inserted tl  
    end
  in 

  aux (S8BT.make ~m ()) [] l 

let () = 
  Printf.printf "Unit tests ...\n%!"

let () = 
  run_insert_find_test ~m:3 ~l:[1] () 

let () = 
  run_insert_find_test ~m:3 ~l:[4321] () 

let () = 
  run_insert_find_test ~m:7 ~l:[4321] () 

let () = 
  run_insert_find_test ~m:3 ~l:[1;2] () 

let () = 
  run_insert_find_test ~m:3 ~l:[2;1] () 

(* Node split + creation of new root *)

(* case when the newly inserted value is the median *)
let () = 
  run_insert_find_test ~m:3 ~l:[1;3;2] () 

(* case when the median is in the left node *)
let () = 
  run_insert_find_test ~m:3 ~l:[2;1;3] () 

(* case when the median is the right node *)
let () = 
  run_insert_find_test ~m:3 ~l:[1;2;3] () 

(* case when the median is the left node and new 
 * value is in the left node *)
let () = 
  run_insert_find_test ~m:3 ~l:[3;2;1] () 

(* Right most child is filling up [3;4] *)
let () = 
  run_insert_find_test ~m:3 ~l:[1;2;3;4] () 

(* Right mode child should split and root will have 2 values [2;4]
 *           2-------4
 *           |       |
 *        +--+--+ +--+--+
 *        1     3 5     6
 *)
let () = 
  run_insert_find_test ~m:3 ~l:[1;2;3;4;5] () 

(* Right node (ie 3rd sub node of root) is filling up to 2 values [5;6] *)
let () = 
  run_insert_find_test ~m:3 ~l:[1;2;3;4;5;6] () 


(* Right node (ie 3rd sub node of root) is splitting up on median value 6, 
 * then the root node is splitting up on medain value 4 and therefore 
 * a new root is created with a single value 4 and 2 child node with 
 * a single values [2] and [6]:
 *               4 
 *           +---+---+ 
 *           |       |
 *        +--2--+ +--6--+
 *        1     3 5     7
 *)
let () = 
  run_insert_find_test ~m:3 ~l:[1;2;3;4;5;6;7] () 

let generate_n_list ~n ~max () = 
  let rec aux l = function
    | i when i = n -> l 
    | i -> 
      aux ((Random.int max) :: l) (i + 1)
  in 
  aux [] 0 

let () = 
  Printf.printf "Random tests ...\n%!"

let n = if test_type = `Fast then 100 else 1000 

let () = 
  run_insert_find_test ~verify_at_end:() ~m:3 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~verify_at_end:() ~m:5 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~verify_at_end:() ~m:7 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~verify_at_end:() ~m:51 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~verify_at_end:() ~m:101 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~verify_at_end:() ~m:1001 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  Printf.printf "Permutations tests ...\n%!"

let permutation_values = 
  if test_type = `Fast 
  then [1;2;3;4;5;6;7]
  else [1;2;3;4;5;6;7;8;9] 

let sub_lists l = 
  let rec aux prev ret = function
    | [] -> ret 
    | hd::tl -> 
      aux (hd :: prev) ((hd, prev @ tl) :: ret) tl 
  in
  aux [] [] l 

let rec permute l = 
  let sub_lists = sub_lists l in 
  List.fold_left (fun acc (i, l') -> 
   match permute l' with
   | [] -> [i] :: acc 
   | all_permutation' -> 
     let all_permutation  = 
       List.map (fun permutation' -> i::permutation') all_permutation'  
     in 
     all_permutation @ acc
  ) [] sub_lists 

let () = 
  List.iter (fun test -> 
    run_insert_find_test ~verify_at_end:() ~m:3 ~l:test ()
  ) (permute permutation_values)

let () = 
  List.iter (fun test -> 
    run_insert_find_test ~verify_at_end:() ~m:5 ~l:test ()
  ) (permute permutation_values)
