

module Make (Key:Btree.Key_sig) (Val:Btree.Val_sig) = struct 

  module Internal = Btree.Make(Key)(Val)

  type t = {
    fd : Unix.file_descr; 
    root_offset: int;
    m : int;
  }

  let do_read_op fd {Btree.offset; length} = 
    ignore @@ Unix.lseek fd offset Unix.SEEK_SET; 
    let bytes = Bytes.create length in 
    begin match Unix.read fd bytes 0 length with
    |  nb_of_bytes when nb_of_bytes = length -> () 
    | _ -> failwith "read incomplete"
    end;
    bytes

  let do_write_op fd {Btree.offset; bytes; } = 
    ignore @@ Unix.lseek fd offset Unix.SEEK_SET; 
    let length = Bytes.length bytes in 
    begin match Unix.write fd bytes 0 length with
    | nb_of_bytes when nb_of_bytes = length -> () 
    | _ -> failwith "write incomplete"
    end

  let int_compare (x:int) (y:int) = Pervasives.compare x y 

  let do_write_ops fd write_ops = 
    List.sort (fun {Btree.offset = lhs; _} {Btree.offset = rhs; _} -> 
      int_compare lhs rhs
    ) write_ops
    |>  List.iter (fun write -> do_write_op fd write) 

  let do_allocate fd length = 
    let offset = Unix.lseek fd 0 Unix.SEEK_END in 
    let write_op = {
      Btree.offset; 
      bytes = Bytes.make length (char_of_int 0)
    } in 
    do_write_op fd write_op; 
    offset
  
  let make ~filename ~m () = 
    let write_op = Internal.write_leaf_node 
      ~keys:[||] ~vals:[||] ~offset:0 ~m () in 
    let fd = Unix.openfile filename [Unix.O_RDWR; Unix.O_CREAT] 0o640 in 
    do_write_op fd write_op; 
    { fd; root_offset = 0; m}

  let node_on_disk {root_offset; m; _} = 
    Internal.make_on_disk ~offset:root_offset ~m () 

  let insert ({fd;_} as t) key value = 

    let rec aux = function 
      | Internal.Insert_res_done (root_offset, write_ops) -> begin  
        do_write_ops fd write_ops;
        match root_offset with
        | None -> t 
        | Some root_offset -> {t with root_offset} 
      end 
      | Internal.Insert_res_read_data (block, k) ->  
        k (do_read_op fd block) |> aux 
      | Internal.Insert_res_allocate (length, k) -> 
        k (do_allocate fd length) |> aux 
      | _ -> assert(false)
    in 
    Internal.insert (node_on_disk t) key value |> aux 

  let debug ({fd; _} as t)= 
    let rec aux = function
      | Internal.Debug_res_read_data (block, k) -> 
        k (do_read_op fd block) |> aux 
      | Internal.Debug_res_done  -> () 
    in 
    Internal.debug (node_on_disk t) |> aux 

  let find ({fd; _} as t)key = 
    let rec aux = function
      | Internal.Find_res_not_found -> None 
      | Internal.Find_res_read_data (block, k) -> 
        do_read_op fd block |> k |>  aux 
      | Internal.Find_res_val v -> Some v 
    in
    Internal.find (node_on_disk t) key |> aux 

end (* Make *) 
