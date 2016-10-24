module Encoding = Dbtlk_encoding 

module PersonTable = struct

  type t = Table_test_pb.person 

  let to_string {Table_test_pb.first_name; _} = 
    Printf.sprintf "First Name: %s" first_name

  let to_bytes person = 
    let encoder = Pbrt.Encoder.create () in 
    Table_test_pb.encode_person person encoder; 
    Pbrt.Encoder.to_bytes encoder 

  let of_bytes bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Table_test_pb.decode_person decoder

  module Key0 = Encoding.MakeMaxLengthString256(struct 
    let length = 64
  end) 

  let index0 {Table_test_pb.last_name; _} = last_name 
  
  module Key1 = Key0 

  let index1 {Table_test_pb.first_name; _} = first_name 

end (* PersonTable *) 

module Table = Dbtlk_table.Make (PersonTable) 

let next_person =  
  let counter = ref 0 in 
  fun () -> 
    incr counter; 
    let first_name = Printf.sprintf "Maxime_%010i" !counter in 
    let last_name = Printf.sprintf "Ransan_%010i" !counter in 
    let phone_number = "9179298071" in 
    Table_test_pb.default_person ~first_name ~last_name ~phone_number ()

type db = {
  db : Table.t;
  fd : Unix.file_descr;
}

let open_empty_db file_name = 
  let fd = Unix.openfile file_name [Unix.O_RDWR; Unix.O_CREAT] 0o640 in 
  let db, write_ops = Table.make_empty () in 
  Btree_unix.do_write_ops fd write_ops;
  {db;fd;}

let open_from_file file_name =  
  let fd = Unix.openfile file_name [Unix.O_RDWR; Unix.O_CREAT] 0o640 in 
  let db = Btree_unix.do_res fd @@ Table.make_from_file () in 
  {db;fd;}

let insert {db;fd} record = 
  Table.insert db record
  |> Btree_unix.do_res fd 
  |> Btree_unix.do_write_ops fd  

let close {fd; _ } = 
  Unix.close fd  

let to_string {db; _ } = 
  Table.to_string db

let debug {db; fd } = 
  Btree_unix.do_res fd (Table.debug db) 

let () = 
  let t0 = Unix.gettimeofday () in
  let n = 200_000 in 

  let rec aux db = function
    | 0 -> close db;
    | i -> 
      let db = 
        if i mod 100_000_000 = 0 
        then begin 
          close db; 
          open_from_file "db.data"
        end
        else db 
      in 
      insert db (next_person ()); 
      aux db (i - 1)
  in 
  aux (open_empty_db "db.data") n;

  let t1 = Unix.gettimeofday () in 
  Printf.printf ">> write rate: %f\n" @@ (float_of_int n) /. (t1 -. t0);

  let db = open_from_file "db.data" in 
  print_endline @@ to_string db;
  debug db;
  let t2 = Unix.gettimeofday () in 
  Printf.printf ">> read rate: %f\n" @@ (float_of_int n) /. (t2 -. t1);
  close db;
  let db = open_from_file "db.data" in 
  print_endline @@ to_string db;
  close db;
  ()
