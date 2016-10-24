(*
 The MIT License (MIT)
 Copyright (c) 2016 Maxime Ransan (maxime.ransan@gmail.com)
 
 Permission is hereby granted, free of charge, to any person obtaining a copy 
 of this software and associated documentation files (the "Software"), to deal 
 in the Software without restriction, including without limitation the rights 
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
 copies of the Software, and to permit persons to whom the Software is 
 furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included 
 in all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
 THE SOFTWARE.
*)

module Encoding = Dbtlk_encoding

module type Record_sig = sig 

  (** {2 Record type} *) 

  type t 

  val to_bytes : t -> bytes 
  (** [to_bytes t] encodes [t] to bytes *)

  val of_bytes : bytes -> t 
  (** [of_bytes bytes] decodes a value of type [t] from [bytes] *)

  val to_string : t -> string 
  (** [to_string t] returns a string for debugging purposes only *)

  (** {2 Indices} *) 

  module Key0 : Dbtlk_btree.Key_sig 

  val index0 : t -> Key0.t  
  (** [index1 t] access the key1 for a given record *)

  module Key1 : Dbtlk_btree.Key_sig 

  val index1 : t -> Key1.t 
  (** [index1 t] access the key2 for a given record *)

end  (* Record_sig  *) 

module T = Dbtlk_types 

(* The meta data of a table simply consists in the collection of all 
 * the index information that the table contains. 
 *
 * Index information consists in the btree root offset and the btree [m] size
 * see Dbtlk_btree module for more detailed information. 
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
    assert(Array.length t <= max_number_of_indices);
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
    assert(n <= max_number_of_indices);
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

  let set_btree_root_offset t i btree_root_offset = 
    let index_info = t.(i) in 
    t.(i) <- {index_info with btree_root_offset}
end 

module Make(Table:Record_sig) = struct 

  module Index0 = Dbtlk_btree.Make(Table.Key0)(Encoding.Int64)
    (* stored @ index 0 in meta data *)
  module Index1 = Dbtlk_btree.Make(Table.Key1)(Encoding.Int64)
    (* stored @ index 1 in meta data *)

  type t = {
    metadata : MetaData.t; 
  }

  let btree_m = 31

  (* --- DB Meta Data ---   
   *
   * The DB meta data is at the beginning of the file and consists
   * in all the B-Tree root offsets. 
   *)
  
  let write_db_meta metadata =
    let bytes = Bytes.create MetaData.length in 
    MetaData.to_bytes metadata bytes 0;
    T.{offset = 0;  bytes}

  let read_db_meta () = 
    let db_meta_block = T.{offset = 0; length = MetaData.length} in 
    T.res_read_data db_meta_block (fun db_meta_bytes ->
      T.res_done @@ MetaData.of_bytes db_meta_bytes 0 
    )
  
  let index0_btree_of_metadata metadata = 
    let {
      MetaData.btree_root_offset; 
      btree_m;
    } = metadata.(0) in 
    Index0.make ~root_file_offset:btree_root_offset ~m:btree_m ()

  let index1_btree_of_metadata metadata = 
    let {
      MetaData.btree_root_offset; 
      btree_m;
    } = metadata.(1) in 
    Index1.make ~root_file_offset:btree_root_offset ~m:btree_m ()

  let make_empty () =  
    
    let index0_index_info = MetaData.{
      btree_root_offset = MetaData.length; 
      btree_m;
    } in 

    let index1_index_info = MetaData.{
      btree_root_offset = MetaData.length + Index0.node_length_of_m btree_m; 
      btree_m;
    } in 

    let metadata = [| index0_index_info; index1_index_info |] in 
    
    let write_ops = 
      (write_db_meta metadata) :: 
      (Index0.initialize (index0_btree_of_metadata metadata)) ::
      (Index1.initialize (index1_btree_of_metadata metadata)) :: []
    in
    (
      {metadata;}, 
      write_ops
    )
  
  let make_from_file () = 
    read_db_meta () |> T.res_map (fun metadata -> 
      {metadata}
    )
  
  let append_record record = 
    let record_bytes = Table.to_bytes record in 
    let record_length = Bytes.length record_bytes in 

    let full_record_length = Encoding.Int64.length + record_length in 
    let full_record_bytes = Bytes.create full_record_length in 

    (* first encode the length of the record... *) 
    Encoding.Int64.to_bytes record_length full_record_bytes 0; 
    (* ... then the record itself *)
    Bytes.blit record_bytes 0 
        full_record_bytes Encoding.Int64.length record_length;  
  
    T.res_allocate full_record_length (fun record_offset -> 
      T.res_done T.{offset = record_offset; bytes = full_record_bytes}
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
  
  let insert db record =

    let metadata = db.metadata in
  
    append_record record |> T.res_bind (fun record_write_op  ->

      let {T.offset = record_offset; _ } = record_write_op in 

      let write_ops = [record_write_op] in

      let btree = index0_btree_of_metadata metadata in 
      Index0.append btree (Table.index0 record) record_offset
      |> T.res_map (fun append_res ->
        match append_res with
        | Index0.Insert_res_done (None, index0_write_ops) ->  
          (false, write_ops @ index0_write_ops) 
        | Index0.Insert_res_done (Some record_id_bro, index0_write_ops) -> 
          MetaData.set_btree_root_offset metadata 0 record_id_bro; 
          (true, write_ops @ index0_write_ops)
        | _ -> assert(false)
      )
      |> T.res_bind (fun (write_meta_data, write_ops)-> 
        let btree = index1_btree_of_metadata metadata in 
        Index1.insert btree (Table.index1 record) record_offset
        |> T.res_map (fun insert_res -> 
          match insert_res with
          | Index1.Insert_res_done (None, index1_write_ops) ->  
            (write_meta_data, index1_write_ops @ write_ops) 
          | Index1.Insert_res_done (Some index1_bro, index1_write_ops) -> 
            MetaData.set_btree_root_offset metadata 1 index1_bro;
            (true, index1_write_ops @ write_ops)
          | _ -> assert(false)
        )
      )
      |> T.res_map (fun (write_meta_data, write_ops) ->
        let write_ops = 
          if write_meta_data
          then write_db_meta metadata :: write_ops 
          else write_ops
        in 
        write_ops 
      )  
    )
  
  let debug db = 

    let all_records_ids = ref [] in 
  
    Index1.iter (index1_btree_of_metadata db.metadata) (fun record_id -> 
      all_records_ids := record_id :: ! all_records_ids 
    ) 
    |> T.res_bind (fun () -> 

       let rec aux = function
         | [] -> T.res_done () 
         | record_offset :: tl -> 
           read_record record_offset
           |> T.res_bind (fun record -> 
               (*
             Printf.printf 
                 ("Record Offset: %010i -> " ^^ 
                  "Record: %s\n")
                 record_offset (Table.to_string record); 
                *)
             let _ = record in 
             aux tl 
           )
       in
       aux (List.rev !all_records_ids) 
    )
  
  let to_string {metadata} = 
    let { MetaData.btree_root_offset = index0_bro; _} = metadata.(0) in 
    let { MetaData.btree_root_offset = index1_bro; _} = metadata.(1) in 
    Printf.sprintf 
        ("\n- index0_bro: %i" ^^ 
         "\n- index1_bro: %i") 
        index0_bro index1_bro  
end 
