
let make_test_key_val4 s = 
  assert(String.length s = 4);
  (Printf.sprintf "0000%s" s, Printf.sprintf "%s0000" s) 

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


let write_op_counter = ref 0 
let do_write_op storage ({Btree.offset; bytes; } : Btree.write_op) = 
  incr write_op_counter;
  let length_to_write = Bytes.length bytes in 
  Bytes.blit 
    (* src *) bytes 0 
    (* dst *) storage offset 
    (* len *) length_to_write

let do_write_ops storage write_ops = 
  List.iter (fun write_op -> 
    do_write_op storage write_op
  ) write_ops


let read_op_counter = ref 0 
let do_read_op storage {Btree.offset;length} =
  incr read_op_counter;
  let sub = Bytes.sub storage offset length in
  sub 

let do_allocate storage length = 
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


let () = 
  if Array.length Sys.argv <> 2 
  then failwith "m argument missing"
  else 
    let m = int_of_string Sys.argv.(1) in 
    run_random_inserts ~m ~nb_of_inserts:100000 () 
