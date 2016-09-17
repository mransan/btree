(** db_test.proto Generated Types and Encoding *)


(** {2 Types} *)

type person = {
  first_name : string;
  last_name : string;
  phone_number : string;
}


(** {2 Default values} *)

val default_person : 
  ?first_name:string ->
  ?last_name:string ->
  ?phone_number:string ->
  unit ->
  person
(** [default_person ()] is the default value for type [person] *)


(** {2 Protobuf Decoding} *)

val decode_person : Pbrt.Decoder.t -> person
(** [decode_person decoder] decodes a [person] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_person : person -> Pbrt.Encoder.t -> unit
(** [encode_person v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_person : Format.formatter -> person -> unit 
(** [pp_person v] formats v *)
