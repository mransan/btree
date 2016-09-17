[@@@ocaml.warning "-27-30-39"]

type person = {
  first_name : string;
  last_name : string;
  phone_number : string;
}

and person_mutable = {
  mutable first_name : string;
  mutable last_name : string;
  mutable phone_number : string;
}

let rec default_person 
  ?first_name:((first_name:string) = "")
  ?last_name:((last_name:string) = "")
  ?phone_number:((phone_number:string) = "")
  () : person  = {
  first_name;
  last_name;
  phone_number;
}

and default_person_mutable () : person_mutable = {
  first_name = "";
  last_name = "";
  phone_number = "";
}

let rec decode_person d =
  let v = default_person_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.first_name <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(person), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.last_name <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(person), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.phone_number <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(person), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:person = Obj.magic v in
  v

let rec encode_person (v:person) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.first_name encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.last_name encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.phone_number encoder;
  ()

let rec pp_person fmt (v:person) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "first_name" Pbrt.Pp.pp_string fmt v.first_name;
    Pbrt.Pp.pp_record_field "last_name" Pbrt.Pp.pp_string fmt v.last_name;
    Pbrt.Pp.pp_record_field "phone_number" Pbrt.Pp.pp_string fmt v.phone_number;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
