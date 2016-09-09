(*
let is_debug = false
let printf = 
  if is_debug 
  then Printf.printf 
  else (fun fmt -> Printf.ifprintf stdout fmt)  

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
*)

module Make (Key:Btree.Key_sig) (Val:Btree.Val_sig) = struct 

  module Internal = Btree.Make(Key)(Val)
  
  let write_op_counter = ref 0 
  let read_op_counter = ref 0 

  type t = {
    storage : bytes;
    root_offset: int;
    m : int;
  }

  let do_read_op storage {Btree.offset;length}= 
    incr read_op_counter;
    (*printf "- reading block: %s" (Btree.string_of_block block);
     *)
    let sub = Bytes.sub storage offset length in
    (*
    printf "%s\n" (string_of_bytes sub); 
    *)
    sub 

  let do_write_op storage {Btree.offset; bytes; } = 
    incr write_op_counter;
    (*printf "- writing to offset %i%s\n" 
      offset 
      (string_of_bytes bytes); 
     *)
    let length_to_write = Bytes.length bytes in 
    Bytes.blit 
      (* src *) bytes 0 
      (* dst *) storage offset 
      (* len *) length_to_write

  let int_compare (x:int) (y:int) = Pervasives.compare x y 

  let do_write_ops storage  write_ops = 
    List.sort (fun {Btree.offset = lhs; _} {Btree.offset = rhs; _} -> 
      int_compare lhs rhs
    ) write_ops
    |>  List.iter (fun write -> do_write_op storage write) 

  let do_allocate storage length = 
    (*
    printf "- allocating block of length: %i @ offset: %i\n" 
      length (Bytes.length storage);
    *)
    let offset = Bytes.length storage in 
    let storage = Bytes.extend storage 0 length in 
    (storage, offset) 

  let rec do_res storage = function
    | Internal.Res_done x -> (storage, x)  
    | Internal.Res_read_data (block, k) -> 
      do_read_op storage block 
      |> k 
      |> do_res storage  

    | Internal.Res_allocate (block_length, k) ->
      let storage, offset = do_allocate storage block_length in 
      k offset |> do_res storage 
  
  let make ~m () = 

    let node = Internal.make ~root_file_offset:0 ~m () in 
    let {Btree.offset; bytes;} as write_op = Internal.initialize node in 
    let storage = bytes in 
    assert(offset = 0); 
    do_write_op storage write_op; 
    { storage; root_offset = 0; m;}

  let node_on_disk {root_offset; m; _} = 
    Internal.make ~root_file_offset:root_offset ~m () 

  let insert_aux ({storage; _ } as t) res = 
    let storage, res = do_res storage res in 
    match res with 
    | Internal.Insert_res_done (root_offset, write_ops) -> begin  
      do_write_ops storage write_ops;
      match root_offset with
      | None -> {t with storage} 
      | Some root_offset -> {t with root_offset; storage} 
    end 
    | _ -> assert(false)

  let insert t key value = 
    Internal.insert (node_on_disk t) key value |> insert_aux t 
  
  let append t key value = 
    Internal.append (node_on_disk t) key value |> insert_aux t 

  let debug ({storage ; _} as t)= 
    let storage', () = do_res storage @@ Internal.debug (node_on_disk t) in 
    assert(storage == storage');
      (* ignore storage since we know that storage should not be modified 
       * by a find operation *)
    () 

  let find ({storage; _} as t) key = 
    let storage', res = do_res storage @@ Internal.find (node_on_disk t) key in  
    assert(storage == storage');
      (* ignore storage since we know that storage should not be modified 
       * by a find operation *)
    res 
  
  let find_gt ({storage; _} as t) key = 
    let storage', res = do_res storage @@ Internal.find_gt (node_on_disk t) key in 
    assert(storage == storage');
      (* ignore storage since we know that storage should not be modified 
       * by a find operation *)
    res 

  module Stats = struct 

    let reset _ = 
      read_op_counter := 0; 
      write_op_counter := 0

    let read_count _  = 
      !read_op_counter 

    let write_count _ = 
      !write_op_counter

    let node_length {m; _} = 
      Internal.node_length_of_m m 

    let storage_length {storage; _ } = 
      Bytes.length storage 

  end
end (* Make *) 
