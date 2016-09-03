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

  type t = {
    storage : bytes;
    root_offset: int;
    m : int;
    write_op_counter : int ref;
    read_op_counter : int ref;
  }

  let do_read_op read_op_counter storage {Btree.offset;length}= 
    incr read_op_counter;
    (*printf "- reading block: %s" (Btree.string_of_block block);
     *)
    let sub = Bytes.sub storage offset length in
    (*
    printf "%s\n" (string_of_bytes sub); 
    *)
    sub 

  let do_write_op write_op_counter storage {Btree.offset; bytes; } = 
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

  let do_write_ops write_op_counter storage  write_ops = 
    List.sort (fun {Btree.offset = lhs; _} {Btree.offset = rhs; _} -> 
      int_compare lhs rhs
    ) write_ops
    |>  List.iter (fun write -> do_write_op write_op_counter storage write) 

  let do_allocate storage length = 
    (*
    printf "- allocating block of length: %i @ offset: %i\n" 
      length (Bytes.length storage);
    *)
    let offset = Bytes.length storage in 
    let storage = Bytes.extend storage 0 length in 
    (storage, offset) 
  
  let make ~m () = 

    let node = Internal.make ~root_file_offset:0 ~m () in 
    let {Btree.offset; bytes;} as write_op = Internal.initialize node in 
    let storage = bytes in 
    assert(offset = 0); 
    let write_op_counter = ref 0 in
    do_write_op write_op_counter storage write_op; 
    { storage; root_offset = 0; m; write_op_counter; read_op_counter = ref 0}

  let node_on_disk {root_offset; m; _} = 
    Internal.make ~root_file_offset:root_offset ~m () 

  let rec insert_aux ({storage; write_op_counter; read_op_counter;_ } as t) = function 
    | Internal.Insert_res_done (root_offset, write_ops) -> begin  
      do_write_ops write_op_counter storage write_ops;
      match root_offset with
      | None -> t 
      | Some root_offset -> {t with root_offset} 
    end 
    | Internal.Insert_res_read_data (block, k) ->  
      do_read_op read_op_counter storage block |> k |> insert_aux t 
    | Internal.Insert_res_allocate (length, k) -> 
      let storage, offset = do_allocate storage length  in 
      k offset |> insert_aux {t with storage}  
    | _ -> assert(false)

  let insert t key value = 
    Internal.insert (node_on_disk t) key value |> insert_aux t 
  
  let append t key value = 
    Internal.append (node_on_disk t) key value |> insert_aux t 

  let debug ({storage ; read_op_counter; _} as t)= 
    let rec aux = function
      | Internal.Debug_res_read_data (block, k) -> 
        do_read_op read_op_counter storage block |> k |> aux 
      | Internal.Debug_res_done  -> () 
    in 
    Internal.debug (node_on_disk t) |> aux 

  let find ({storage; read_op_counter; _} as t) key = 
    let rec aux = function
      | Internal.Find_res_not_found -> None 
      | Internal.Find_res_read_data (block, k) -> 
        do_read_op read_op_counter storage block |> k |>  aux 
      | Internal.Find_res_val v -> Some v 
    in
    Internal.find (node_on_disk t) key |> aux 
  
  let find_gt ({storage; read_op_counter; _} as t) key = 
    let rec aux = function
      | Internal.Find_gt_res_values values -> values 
      | Internal.Find_gt_res_read_data (block, k) -> 
        do_read_op read_op_counter storage block |> k |>  aux 
    in
    Internal.find_gt (node_on_disk t) key |> aux 

  module Stats = struct 

    let reset {read_op_counter; write_op_counter; _ } = 
      read_op_counter := 0; 
      write_op_counter := 0

    let read_count {read_op_counter; _ } = 
      !read_op_counter 

    let write_count {write_op_counter; _} = 
      !write_op_counter

    let node_length {m; _} = 
      Internal.node_length_of_m m 

    let storage_length {storage; _ } = 
      Bytes.length storage 

  end
end (* Make *) 
