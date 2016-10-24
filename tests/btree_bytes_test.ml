module Encoding = Dbtlk_encoding
let test_type = `Fast

(*  Helps in debugging 
 
 *)
let print_string_list l = 
  Printf.printf "["; 
  List.iter (fun e -> Printf.printf "%s, " e) l; 
  Printf.printf "]"


module String8 = Encoding.MakeFixedLengthString(struct 
  let length = 8 
end)
  
module S8BT = Btree_bytes.Make(String8)(String8)

let make_test_key_val i = 
  let key = Printf.sprintf "0%07i" i in 
  let value = Printf.sprintf "%07i0" i in 
  (key, value) 


let assert_bool b = 
  assert b 

let verify_inserted t inserted_numbers = 

  let not_found_msg k = 
    Printf.sprintf "Error, key: %s not found \n" k
  in

  let mismatched_msg k v v' = 
    Printf.sprintf 
        "Error, mismatch value for key: %s, expected: %s, got: %s \n" 
        k v v'
  in

  let rec aux t inserted_numbers = 
    match inserted_numbers with 
    | [] -> () 
    | i :: tl -> 
      let k, v = make_test_key_val i in 
      match S8BT.find t k with
      | None -> begin 
        print_endline @@ not_found_msg k;
        assert_bool(false)
      end
      | Some v' when v' <> v -> begin 
        print_endline @@ mismatched_msg k v v';
        assert_bool(false)
      end 
      | _ -> aux t tl 
  in
  aux t inserted_numbers
  
let make_prefix_msg prefix n m = 
  Printf.sprintf "%25s [%05i/%05i] ..." prefix n m 

let run_insert_find_test ?verify_at_end ~prefix ~m ~l () = 

  let l_length = List.length l in

  let verify_at_end = match verify_at_end with
    | None -> false
    | Some () -> true
  in 

  let rec aux t ith inserted = function
    | [] -> 
      if verify_at_end 
      then verify_inserted t inserted 
      else () 

    | i::tl -> begin  
      print_string @@ make_prefix_msg prefix ith l_length;
      let k, v = make_test_key_val i in 
      let t = S8BT.insert t k v in 
      print_endline " insert Ok";
      let inserted = i :: inserted in 
      begin 
        if not verify_at_end 
        then verify_inserted t inserted
        else ()
      end;
      aux t (ith + 1) inserted tl  
    end
  in 

  aux (S8BT.make ~m ()) 1 [] l 

let () = 
  print_endline "Unit tests ..."

let prefix = "Unit test"

let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[1] () 

let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[4321] () 

let () = 
  run_insert_find_test ~prefix ~m:7 ~l:[4321] () 

let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[1;2] () 

let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[2;1] () 

(* Node split + creation of new root *)

(* case when the newly inserted value is the median *)
let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[1;3;2] () 

(* case when the median is in the left node *)
let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[2;1;3] () 

(* case when the median is the right node *)
let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[1;2;3] () 

(* case when the median is the left node and new 
 * value is in the left node *)
let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[3;2;1] () 

(* Right most child is filling up [3;4] *)
let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[1;2;3;4] () 

(* Right mode child should split and root will have 2 values [2;4]
 *
 *           +--2--+--4--+
 *           |     |     |
 *           1     2     5--6 
 *)
let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[1;2;3;4;5] () 

(* Right node (ie 3rd sub node of root) is filling up to 2 values [5;6] *)
let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[1;2;3;4;5;6] () 

(* Right node (ie 3rd sub node of root) is splitting up on median value 6, 
 * then the root node is splitting up on medain value 4 and therefore 
 * a new root is created with a single value 4 and 2 child node with 
 * a single values [2] and [6]:
 *               
 *           +---4---+ 
 *           |       |
 *        +--2--+ +--6--+
 *        |     | |     |
 *        1     3 5     7
 *)
let () = 
  run_insert_find_test ~prefix ~m:3 ~l:[1;2;3;4;5;6;7] () 

let generate_n_list ~n ~max () = 
  let rec aux l = function
    | i when i = n -> l 
    | i -> 
      aux ((Random.int max) :: l) (i + 1)
  in 
  aux [] 0 

let () = 
  print_endline "Random tests ..."

let make_random_prefix i = 
  Printf.sprintf "Random test %02i" i 

let n = if test_type = `Fast then 102 else 1000 

let () = 
  run_insert_find_test ~prefix:(make_random_prefix 0) 
      ~verify_at_end:() ~m:3 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~prefix:(make_random_prefix 1) 
      ~verify_at_end:() ~m:5 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~prefix:(make_random_prefix 2) 
      ~verify_at_end:() ~m:7 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~prefix:(make_random_prefix 3) 
      ~verify_at_end:() ~m:51 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~prefix:(make_random_prefix 4) 
      ~verify_at_end:() ~m:101 ~l:(generate_n_list ~n ~max:1000 ()) ()

let () = 
  run_insert_find_test ~prefix:(make_random_prefix 5) 
      ~verify_at_end:() ~m:1001 ~l:(generate_n_list ~n ~max:1000 ()) ()

let prefix = "Permutation test"

let () = 
  print_endline "Permutations tests ..."

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

let make_permutation_prefix i = 
  Printf.sprintf "%s %05i" prefix i

let () = 
  List.iteri (fun i test -> 
    let prefix = make_permutation_prefix i in 
    run_insert_find_test ~prefix ~verify_at_end:() ~m:3 ~l:test ()
  ) (permute permutation_values)

let () = 
  List.iteri (fun i test -> 
    let prefix = make_permutation_prefix i in 
    run_insert_find_test ~prefix ~verify_at_end:() ~m:5 ~l:test ()
  ) (permute permutation_values)

let () = 
  print_endline "Append tests ..." 

let prefix = "Append test"

let run_append_find_test ~prefix ~m ~l () = 

  let l_length = List.length l in 

  let rec aux t ith inserted = function
    | [] -> () 

    | i::tl -> begin  
      print_string @@ make_prefix_msg prefix ith l_length;
      let k, v = make_test_key_val i in 
      let t = S8BT.append t k v in 
      print_endline " append Ok";
      let inserted = i :: inserted in 
      verify_inserted t inserted ;
      aux t (ith + 1) inserted tl  
    end
  in 

  aux (S8BT.make ~m ()) 1 [] l 

let () = 
  run_append_find_test ~prefix ~m:3 ~l:[1] () 

let () = 
  run_append_find_test ~prefix ~m:3 ~l:[4321] () 

let () = 
  run_append_find_test ~prefix ~m:7 ~l:[4321] () 

let () = 
  run_append_find_test ~prefix ~m:3 ~l:[1;2] () 

let () = 
  run_append_find_test ~prefix ~m:7 ~l:[1;2] () 

(* Node split + creation of new root *)

(* case when the newly appended value is the median *)
let () = 
  run_append_find_test ~prefix ~m:3 ~l:[1;2;3] () 

(* Right most child is filling up [3;4] *)
let () = 
  run_append_find_test ~prefix ~m:3 ~l:[1;2;3;4] () 

let () = 
  print_endline "Find gt tests ..." 

let prefix = "Find gt test"

let () =
  let max = 1_000 in 
  let btree = S8BT.make ~m:3 () in 
  let insert_l t i =
    let k, v = make_test_key_val i in 
    S8BT.insert t k v 
  in
  let key1, _ = make_test_key_val 1 in 
  assert_bool([] = S8BT.find_gt btree key1 max);

  let btree = insert_l btree 1 in 
  assert_bool([] = S8BT.find_gt btree key1 max);

  let btree = insert_l btree 2 in 
  let key2, val2 = make_test_key_val 2 in 
  assert_bool([val2] = S8BT.find_gt btree key1 max);

  let btree = insert_l btree 3 in 
  let key3, val3 = make_test_key_val 3 in 
  assert_bool(val2::val3::[] = S8BT.find_gt btree key1 max); 

  let btree = insert_l btree 4 in 
  let key4, val4 = make_test_key_val 4 in 
  assert_bool(val2::val3::val4::[] = S8BT.find_gt btree key1 max); 

  (* With the 5th value the leaf containing [3;4] has split 
   * and the tree has now the following structure;
   *
   *          +--2--+--4--+
   *          |     |     |
   *          1     3     5
   *
   * Therefore the [find_gt] algorithm will collect both 2 and 
   * 3 but will not go further and read more nodes (ie 4 & 5). 
   * This should be left to next iteration *)
  
  let btree = insert_l btree 5 in 
  let key5, val5 = make_test_key_val 5 in 
  assert_bool(val2::val3::val4::val5::[] = S8BT.find_gt btree key1 max); 
  assert_bool(val3::val4::val5::[]       = S8BT.find_gt btree key2 max); 
  assert_bool(val4::val5::[] = S8BT.find_gt btree key3 max);
  assert_bool(val5::[]       = S8BT.find_gt btree key4 max);
  assert_bool([]             = S8BT.find_gt btree key5 max);
  ()

let run_insert_find_gt_test ~m ~l () = 

  let rec aux min max btree = function 
    | [] -> (min, max, btree) 
    | i :: tl -> 
      let max = Pervasives.max max i in 
      let min = Pervasives.min min i in 
      let k, v = make_test_key_val i in 
      let btree = S8BT.insert btree k v in 
      aux min max btree tl 
  in 

  let min, max, btree = aux max_int min_int (S8BT.make ~m ()) l in 
  
  for i = min to max - 1 do 
    let k, v = make_test_key_val i in 
    match S8BT.find_gt btree k 1 with
    | v'::_ when v' > v -> () 
    | _ -> assert(false) 
  done; 

  (* 
   * Make sure that no values is returned if we search for the 
   * max value
   *)

  let k, _ = make_test_key_val max in 
  begin match S8BT.find_gt btree k 1_000 with
  | [] -> ()
  | _ -> assert(false)
  end;

  let sorted_l = 
    l 
    |> List.map (fun i -> make_test_key_val i) 
    |> List.sort_uniq (fun (x, _) (y, _) -> String8.compare x y) 
    |> List.map snd 
  in
  
  let k, _ = make_test_key_val (min - 1) in 
  begin match S8BT.find_gt btree k (List.length sorted_l) with
  | l' when l' = sorted_l -> () 
  | _ -> assert(false)
  end;
 
  (* 
   * Make sure that the right number of values is returned 
   *)

  let sorted_l =
    (* get the first half of the list *)
    let rec aux l = function
      | 0 -> []
      | i -> (List.hd l) :: (aux (List.tl l) (i - 1)) 
    in 
    aux sorted_l (List.length sorted_l / 2) 
  in  
  
  begin match S8BT.find_gt btree k (List.length sorted_l) with
  | l' when l' = sorted_l -> () 
  | _ -> assert(false)
  end

let () = 
  run_insert_find_gt_test ~m:3 ~l:(generate_n_list ~n:5_000 ~max:50_000 ()) ()

let () = 
  run_insert_find_gt_test ~m:3 ~l:(generate_n_list ~n:1_000 ~max:1_000 ()) ()

let () = 
  run_insert_find_gt_test ~m:7 ~l:(generate_n_list ~n:1_000 ~max:10_000 ()) ()

let () = 
  run_insert_find_gt_test ~m:2001 ~l:(generate_n_list ~n:1_000 ~max:10_000 ()) ()

(** Debug *) 

let () = 
  print_endline "Debug test ... " 

let () = 
  let rec aux t = function 
    | 11 -> S8BT.debug t  
    | i -> 
      let k,v  = make_test_key_val i in 
      aux (S8BT.insert t k v) (i + 1) 
  in 
  aux (S8BT.make ~m:3 ()) 0

(** iter *)

let () = 
  print_endline "Iter test ..." 

let () = 

  let n = 1_000 in

  let rec aux t = function
    | 0 -> begin 
      let s = ref 0 in 
      S8BT.iter t (fun v -> s := !s + (String.length v)); 
      assert(n * String8.length = !s) 
    end
    | i -> begin 
      let k, v = make_test_key_val i in 
      aux (S8BT.insert t k v) (i - 1)
    end
  in 

  aux (S8BT.make ~m:3 ()) n 
