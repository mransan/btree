module type Table_sig = sig 

  (** {2 Record type} *) 

  type t 

  val to_bytes : t -> bytes 
  (** [to_bytes t] encodes [t] to bytes *)

  val of_bytes : bytes -> t 
  (** [of_bytes bytes] decodes a value of type [t] from [bytes] *)

  val to_string : t -> string 
  (** [to_string t] returns a string for debugging purposes only *)

  (** {2 Indices} *) 

  module Key1 : Btree.Key_sig 

  val index1 : t -> Key1.t  
  (** [index1 t] access the key1 for a given record *)

end  (* Table_sig  *) 

module T = Types 


(* The meta data of a table simply consists in the collection of all 
 * the index information that the table contains. 
 *
 * Index information consists in the btree root offset and the btree [m] size
 * see Btree module for more detailed information. 
 *)
module MetaData = struct 

  type index_info = {
    btree_root_offset : int; 
    btree_m : int; 
  } 

  type t = index_info array 

  let max_number_of_indices = 10 

  let length = Encoding.Int64.length * (2 * max_number_of_indices + 1)  

  let to_bytes t bytes pos = 
    Encoding.Int64.to_bytes (Array.length t) bytes pos; 
    let pos = pos + Encoding.Int64.length in 

    let _ = Array.fold_left (fun pos {btree_root_offset; btree_m} ->
      Encoding.Int64.to_bytes btree_root_offset bytes pos; 
      Encoding.Int64.to_bytes btree_m bytes (pos + Encoding.Int64.length); 
      pos + (2 * Encoding.Int64.length)
    ) pos t in 
    ()

  let of_bytes bytes pos = 
    let n = Encoding.Int64.of_bytes bytes pos in 
    let pos = pos + Encoding.Int64.length in

    let rec aux pos = function 
      | 0 -> [] 
      | i -> 
        let btree_root_offset = Encoding.Int64.of_bytes bytes pos in 
        let btree_m = Encoding.Int64.of_bytes bytes (pos + Encoding.Int64.length) in
        let pos = pos + 2 * Encoding.Int64.length in 
        {btree_root_offset; btree_m} :: aux pos (i - 1) 
    in 
    Array.of_list @@ aux pos n  
end 

module Make(Table:Table_sig) = struct 

  module RecordIDIndex = Btree.Make(Encoding.Int64)(Encoding.Int64) 
  module Index1 = Btree.Make(Table.Key1)(Encoding.Int64)

  (* -- Abbreviation -- 
   * 
   * +-- [bro] stands for btree root offset 
   *)

  type db = {
    mutable last_record_id : int; 
    mutable record_id_bro : int;  
    mutable index1_bro : int; 
    file_name : string;
  }

  let btree_m = 41

  (* --- DB Meta Data ---   
   *
   * The DB meta data is at the beginning of the file and consists
   * in all the B-Tree root offsets. 
   *)
  
  let db_meta_length = Encoding.Int64.length * 2
  
  let record_id_bro_offset = 0 
  
  let index1_bro_offset =  Encoding.Int64.length 
  
  let write_db_meta record_id_bro index1_bro = 
    let db_meta_bytes = Bytes.create db_meta_length in 
    Encoding.Int64.to_bytes 
        record_id_bro db_meta_bytes record_id_bro_offset;  
    Encoding.Int64.to_bytes 
        index1_bro db_meta_bytes index1_bro_offset; 
    T.{offset = 0;  bytes = db_meta_bytes}

  let read_db_meta () = 
    let db_meta_block = T.{offset = 0; length = db_meta_length} in 
    T.res_read_data db_meta_block (fun db_meta_bytes ->
      T.res_done ( 
        Encoding.Int64.of_bytes db_meta_bytes record_id_bro_offset,
        Encoding.Int64.of_bytes db_meta_bytes index1_bro_offset
      )
    )

  let open_empty_db file_name =  
    
    let last_record_id = 0 in 
    let record_id_bro = db_meta_length in 
    let index1_bro = 
      record_id_bro + RecordIDIndex.node_length_of_m btree_m 
    in 
    
    let write_ops = 
      (write_db_meta record_id_bro index1_bro) :: 
      (
        RecordIDIndex.make ~root_file_offset:record_id_bro ~m:btree_m ()
        |> RecordIDIndex.initialize
      ) ::
      (  
        Index1.make ~root_file_offset:index1_bro ~m:btree_m ()
        |> Index1.initialize
      ) :: []
    in
    (
      {last_record_id; record_id_bro; index1_bro; file_name;}, 
      write_ops
    );; 
  
  let record_id_btree_of_record_id_bro record_id_bro = 
    RecordIDIndex.make ~root_file_offset:record_id_bro ~m:btree_m ()
  
  let open_from_file file_name = 
  
    read_db_meta () |> T.res_bind (fun (record_id_bro, index1_bro) -> 
  
      let res = 
        let btree = record_id_btree_of_record_id_bro record_id_bro in 
        RecordIDIndex.last btree
      in 
      res |> T.res_map (function 
        | None ->
          {last_record_id = 0; record_id_bro; index1_bro; file_name}
        | Some (last_record_id, _) -> 
          {last_record_id; record_id_bro; index1_bro; file_name}
      )
    )
  
  let append_record record = 
    let record_bytes = Table.to_bytes record in 
    let record_length = Bytes.length record_bytes in 

    let full_record_bytes = 
      Bytes.create (Encoding.Int64.length + record_length)
    in 

    (* first encode the length of the record , then the record itself *)
    Encoding.Int64.to_bytes record_length full_record_bytes 0; 
    Bytes.blit record_bytes 0 
        full_record_bytes Encoding.Int64.length record_length;  
  
    T.res_append full_record_bytes (fun record_offset -> 
      T.res_done record_offset
    ) 

  let read_record record_offset = 

    let record_length_block = T.{
      offset = record_offset; 
      length = Encoding.Int64.length
    } in 

    T.res_read_data record_length_block (fun record_length_bytes ->
      let record_length = Encoding.Int64.of_bytes record_length_bytes 0 in 

      let record_block = T.{
        offset = record_offset + Encoding.Int64.length; 
        length = record_length;
      } in 

      T.res_read_data record_block (fun record_bytes ->
        T.res_done @@ Table.of_bytes record_bytes 
      ) 
    )
  
  let record_id_btree {record_id_bro; _} = 
    RecordIDIndex.make ~root_file_offset:record_id_bro ~m:btree_m ()
  
  let index1_btree {index1_bro; _} = 
    Index1.make ~root_file_offset:index1_bro ~m:btree_m ()

  let insert db record =
  
    append_record record |> T.res_bind (fun record_offset ->
      let record_id = db.last_record_id + 1 in

      let btree = record_id_btree db in 
      RecordIDIndex.append btree record_id record_offset
      |> T.res_bind (fun append_res ->
        
        let record_id_bro, write_ops =
          match append_res with
          | RecordIDIndex.Insert_res_done (None, write_ops) ->  (db.record_id_bro, write_ops) 
          | RecordIDIndex.Insert_res_done (Some record_id_bro, write_ops) -> (record_id_bro, write_ops)
          | _ -> assert(false)
        in
        let btree = index1_btree db in 
        Index1.insert btree (Table.index1 record) record_id
        |> T.res_map (fun insert_res -> 
          let index1_bro, write_ops =  
            match insert_res with
            | Index1.Insert_res_done (None, write_ops') ->  
              (db.index1_bro, write_ops' @ write_ops) 
            | Index1.Insert_res_done (Some index1_bro, write_ops') -> 
              (index1_bro, write_ops' @ write_ops)
            | _ -> assert(false)
          in
          let write_ops = 
            if record_id_bro <> db.record_id_bro || index1_bro <> db.index1_bro
            then 
              write_db_meta record_id_bro index1_bro :: write_ops 
            else 
              write_ops
          in 
          db.last_record_id <- record_id; 
          db.record_id_bro <- record_id_bro;
          db.index1_bro <- index1_bro;
          write_ops 
        )
      )
    )
  
  let debug db = 

    let all_records_ids = ref [] in 
  
    Index1.iter (index1_btree db) (fun record_id -> 
      all_records_ids := record_id :: ! all_records_ids 
    ) 
    |> T.res_bind (fun () -> 

       let rec aux = function
         | [] -> T.res_done () 
         | record_id :: tl -> 
           RecordIDIndex.find (record_id_btree db) record_id
           |> T.res_bind (function
             | None -> assert(false) 
             | Some record_offset -> 
               read_record record_offset
               |> T.res_bind (fun record -> 
                 Printf.printf 
                     ("Record Index: %010i -> " ^^ 
                      "Record Offset: %010i -> " ^^ 
                      "Record: %s\n")
                     record_id record_offset (Table.to_string record); 
                 aux tl 
               )
           )
       in
       aux (List.rev !all_records_ids) 
    )
  
  let to_string {last_record_id; record_id_bro; index1_bro; file_name; _ } = 
    Printf.sprintf 
        ("\n- last_record_id: %i" ^^ 
         "\n- record_id_bro: %i" ^^ 
         "\n- index1_bro: %i" ^^ 
         "\n- file_name: %s\n") 
        last_record_id record_id_bro index1_bro file_name 
  
  let close _ =  ()
end 
