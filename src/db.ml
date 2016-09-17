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

module Make(Table:Table_sig) = struct 

  module RecordIDIndex = Btree.Make(Encoding.Int64)(Encoding.Int64) 
  module Index1 = Btree.Make(Table.Key1)(Encoding.Int64)

  (* -- Abbreviation -- 
   * 
   * + [bro] stands for btree root offset 
   *)

  type db = {
    mutable last_record_id : int; 
    mutable record_id_bro : int;  
    mutable index1_bro : int; 
    file_name : string;
    fd : Unix.file_descr;
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
  
  let write_db_meta fd record_id_bro index1_bro = 
    let db_meta_bytes = Bytes.create db_meta_length in 
    Encoding.Int64.to_bytes 
        record_id_bro db_meta_bytes record_id_bro_offset;  
    Encoding.Int64.to_bytes 
        index1_bro db_meta_bytes index1_bro_offset; 
    let offset = Unix.lseek fd 0 Unix.SEEK_SET in 
    assert(offset = 0);
    begin match Unix.write fd db_meta_bytes 0 db_meta_length with
    | nb_of_bytes when nb_of_bytes = db_meta_length -> () 
    | _ -> failwith "write incomplete"
    end

  let read_db_meta fd = 

    let offset = Unix.lseek fd 0 Unix.SEEK_SET in
    assert(offset = 0);

    let db_meta_bytes = Bytes.create db_meta_length in 
  
    begin match Unix.read fd db_meta_bytes 0 db_meta_length with
    | nb_of_bytes when nb_of_bytes = db_meta_length -> () 
    | _ -> failwith "read incomplete"
    end;
  
    ( 
      Encoding.Int64.of_bytes db_meta_bytes record_id_bro_offset,
      Encoding.Int64.of_bytes db_meta_bytes index1_bro_offset
    )

  let db_file_perm = 0o640 
  
  let open_empty_db file_name =  
    let fd = 
      let oflags = [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] in 
      Unix.openfile file_name oflags db_file_perm 
    in 
    
    let last_record_id = 0 in 
    let record_id_bro = db_meta_length in 
    let index1_bro = 
      record_id_bro + RecordIDIndex.node_length_of_m btree_m 
    in 
    
    write_db_meta fd record_id_bro index1_bro; 
  
    let write_ops = 
      (
        RecordIDIndex.make ~root_file_offset:record_id_bro ~m:btree_m ()
        |>RecordIDIndex.initialize
      ) ::
      (  
        Index1.make ~root_file_offset:index1_bro ~m:btree_m ()
        |>Index1.initialize
      ) :: []
    in
    Btree_unix.do_write_ops fd write_ops; 
  
    {last_record_id; record_id_bro; index1_bro; file_name; fd;} 
  
  let record_id_btree_of_record_id_bro record_id_bro = 
    RecordIDIndex.make ~root_file_offset:record_id_bro ~m:btree_m ()
  
  let open_from_file file_name = 
  
    let fd = Unix.openfile file_name [Unix.O_RDWR;] db_file_perm in 

    let record_id_bro, index1_bro = read_db_meta fd in
  
    let last_record_id = 
      let res = RecordIDIndex.last (record_id_btree_of_record_id_bro record_id_bro) in 
      match Btree_unix.do_res fd res with
      | None -> 0 
      | Some last_record_id -> fst last_record_id
    in 
  
    {last_record_id; record_id_bro; index1_bro; file_name; fd}
  
  let append_record fd record = 
    let record_bytes = Table.to_bytes record in 
    let record_length = Bytes.length record_bytes in 
    let record_offset = Unix.lseek fd 0 Unix.SEEK_END in 
  
    (* first encode the length of the record , then the record itself *)
    let record_length_bytes = Bytes.create Encoding.Int64.length in 
    Encoding.Int64.to_bytes record_length record_length_bytes 0; 
    
    begin match Unix.write fd record_length_bytes 0 Encoding.Int64.length with 
    | nb_of_bytes when nb_of_bytes = Encoding.Int64.length -> () 
    | _ -> failwith "write incomplete"
    end;
  
    begin match Unix.write fd record_bytes 0 record_length with 
    | nb_of_bytes when nb_of_bytes = record_length -> () 
    | _ -> failwith "read incomplete"
    end;

    record_offset 

  let read_record fd record_offset = 
    let record_length_bytes  = Bytes.create Encoding.Int64.length in 
    let offset = Unix.lseek fd record_offset Unix.SEEK_SET in 
    assert(offset = record_offset);

    let record_length = 
      match Unix.read fd record_length_bytes 0 Encoding.Int64.length with
      | nb_of_bytes when nb_of_bytes = Encoding.Int64.length -> 
        Encoding.Int64.of_bytes record_length_bytes 0 
      | _ -> failwith "read failed"
    in 
  
    let record_bytes = Bytes.create record_length in 
    match Unix.read fd record_bytes 0 record_length with
    | nb_of_bytes when nb_of_bytes = record_length -> 
      Table.of_bytes record_bytes
    | _ -> failwith "read failed"
  
  let record_id_btree {record_id_bro; _} = 
    RecordIDIndex.make ~root_file_offset:record_id_bro ~m:btree_m ()
  
  let index1_btree {index1_bro; _} = 
    Index1.make ~root_file_offset:index1_bro ~m:btree_m ()


  let insert db record =
  
    let fd = db.fd  in
  
    let record_id = db.last_record_id + 1 in
    let record_offset = append_record fd record in
    
    let record_id_bro, write_ops =
      let btree = record_id_btree db in 
      let insert_res = RecordIDIndex.insert btree record_id record_offset in
      match Btree_unix.do_res fd insert_res with
      | RecordIDIndex.Insert_res_done (None, write_ops) ->  (db.record_id_bro, write_ops) 
      | RecordIDIndex.Insert_res_done (Some record_id_bro, write_ops) -> (record_id_bro, write_ops)
      | _ -> assert(false)
    in
  
    let index1_bro, write_ops =  
      let btree = index1_btree db in 
      let insert_res = Index1.insert btree (Table.index1 record) record_id in
      match Btree_unix.do_res fd insert_res with
      | Index1.Insert_res_done (None, write_ops') ->  
        (db.index1_bro, write_ops' @ write_ops) 
      | Index1.Insert_res_done (Some index1_bro, write_ops') -> 
        (index1_bro, write_ops' @ write_ops)
      | _ -> assert(false)
    in
  
    begin 
      if record_id_bro <> db.record_id_bro || index1_bro <> db.index1_bro
      then write_db_meta fd record_id_bro index1_bro; 
    end; 
  
    Btree_unix.do_write_ops fd write_ops;
  
    db.last_record_id <- record_id; 
    db.record_id_bro <- record_id_bro;
    db.index1_bro <- index1_bro;
    ()
  
  let debug db = 
  
    Btree_unix.do_res db.fd @@ Index1.iter (index1_btree db) (fun record_id -> 

      let res = RecordIDIndex.find (record_id_btree db) record_id  in 

      match Btree_unix.do_res db.fd res with 
      | None -> assert(false) 
      | Some record_offset -> 

        let record = read_record db.fd record_offset in
  
        Printf.printf 
            ("Record Index: %010i -> " ^^ 
             "Record Offset: %010i -> " ^^ 
             "Record: %s\n")
            record_id record_offset (Table.to_string record); 
        let _ = record in 
        ()
    ) 
  
  let to_string {last_record_id; record_id_bro; index1_bro; file_name; _ } = 
    Printf.sprintf 
        ("\n- last_record_id: %i" ^^ 
         "\n- record_id_bro: %i" ^^ 
         "\n- index1_bro: %i" ^^ 
         "\n- file_name: %s\n") 
        last_record_id record_id_bro index1_bro file_name 
  
  let close {fd;_} = Unix.close fd 
end 
