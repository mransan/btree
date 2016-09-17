
module PersonTable = struct

  type t = Db_test_pb.person 

  let to_string {Db_test_pb.first_name; _} = 
    Printf.sprintf "First Name: %s" first_name

  let to_bytes person = 
    let encoder = Pbrt.Encoder.create () in 
    Db_test_pb.encode_person person encoder; 
    Pbrt.Encoder.to_bytes encoder 

  let of_bytes bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Db_test_pb.decode_person decoder

  module Key1 = Encoding.MakeMaxLengthString256(struct 
    let length = 64
  end) 

  let index1 {Db_test_pb.last_name; _} = last_name 

end (* PersonTable *) 

module Db = Db.Make (PersonTable) 

let next_person =  
  let counter = ref 0 in 
  fun () -> 
    incr counter; 
    let first_name = Printf.sprintf "Maxime_%010i" !counter in 
    let last_name = Printf.sprintf "Ransan_%010i" !counter in 
    let phone_number = "9179298071" in 
    Db_test_pb.default_person ~first_name ~last_name ~phone_number ()

let () = 
  let t0 = Unix.gettimeofday () in
  let n = 10_000 in 

  let rec aux db = function
    | 0 -> Db.close db;
    | i -> 
      
      let db = 
        if i mod 1_000 = 0 
        then begin 
          Db.close db; 
          Db.open_from_file "db.data"
        end
        else db 
      in 
      Db.insert db (next_person ()); 
      aux db (i - 1)
  in 
  aux (Db.open_empty_db "db.data") n;

  let t1 = Unix.gettimeofday () in 
  Printf.printf ">> rate: %f\n" @@ (float_of_int n) /. (t1 -. t0);

  let db = Db.open_from_file "db.data" in 
  print_endline @@ Db.to_string db;
  Db.debug db;
  let t2 = Unix.gettimeofday () in 
  Printf.printf ">>  rate: %f\n" @@ (float_of_int n) /. (t2 -. t1);
  Db.close db;
  let db = Db.open_from_file "db.data" in 
  print_endline @@ Db.to_string db;
  Db.close db;
  ()
