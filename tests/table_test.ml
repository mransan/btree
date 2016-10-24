module Encoding = Dbtlk_encoding 

module PersonTable = struct

  type t = {
    first_name : string;
    last_name : string; 
    phone_number : string;
  }

  let to_string {first_name; _} = 
    Printf.sprintf "First Name: %s" first_name

  let to_bytes {first_name;last_name;phone_number} = 
    let fn_len = String.length first_name in 
    let ln_len = String.length last_name in 
    let pn_len = String.length phone_number in 
    let len = fn_len + ln_len + pn_len + 2 in 

    let bytes = Bytes.create len in 
    Bytes.blit_string first_name 0 bytes 0 fn_len; 
    Bytes.set bytes fn_len ','; 
    Bytes.blit_string last_name 0 bytes (fn_len + 1) ln_len; 
    Bytes.set bytes (fn_len + 1 + ln_len) ','; 
    Bytes.blit_string phone_number 0 bytes (fn_len + ln_len + 2) pn_len; 
    bytes

  let of_bytes bytes = 
    let c1 = Bytes.index_from bytes 0 ',' in 
    let c2 = Bytes.index_from bytes (c1 + 1) ',' in 
    let first_name = 
      Bytes.sub_string bytes 0 c1 
    in
    let last_name = 
      Bytes.sub_string bytes (c1 + 1) (c2 - c1 - 1) 
    in 
    let phone_number = 
      Bytes.sub_string bytes (c2 + 1) (Bytes.length bytes - c2 - 1) 
    in 
    {first_name; last_name; phone_number};;

  module Key0 = Encoding.MakeMaxLengthString256(struct 
    let length = 64
  end) 

  let index0 {last_name; _} = last_name 
  
  module Key1 = Key0 

  let index1 {first_name; _} = first_name 

end (* PersonTable *) 

module Table = Dbtlk_table.Make (PersonTable) 

let next_person =  
  let counter = ref 0 in 
  fun () -> 
    incr counter; 
    let first_name = Printf.sprintf "Maxime_%010i" !counter in 
    let last_name = Printf.sprintf "Ransan_%010i" !counter in 
    let phone_number = "9179298071" in 
    PersonTable.{first_name; last_name;phone_number}

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
        if i mod 1_000 = 0 
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
