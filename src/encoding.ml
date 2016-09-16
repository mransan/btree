
module type CoreSig = sig 

  type t 
  
  val length : int 
  (** length in bytes of the encoding *)
  
  val to_string : t -> string 
  (** [to_string t] returns a debugging string *)
  
  val of_bytes : bytes -> int -> t 
  (** [of_bytes bytes pos] decodes a value of type [t] in [bytes] starting 
      at [pos]. Undefined behavior is [bytes] length is less than [pos + 8] *)
  
  val to_bytes : t -> bytes -> int -> unit 
  (** [to_bytes t bytes pos] encoded [t] in [bytes] starting at [pos]. 
      
      Undefined behavior if [bytes] length is less than [pos + 8].  *)
  
  val compare : t -> t -> int 
end 

let byte pos bytes = 
  int_of_char (Bytes.unsafe_get bytes pos)

module Int64 = struct 

  type t = int
  
  let length = 8
  
  let to_string = string_of_int 
  
  let of_bytes bytes pos =
    let b1 = byte (pos + 0) bytes in
    let b2 = byte (pos + 1) bytes in
    let b3 = byte (pos + 2) bytes in
    let b4 = byte (pos + 3) bytes in
    let b5 = byte (pos + 4) bytes in
    let b6 = byte (pos + 5) bytes in
    let b7 = byte (pos + 6) bytes in
    let b8 = byte (pos + 7) bytes in
    Int64.(to_int @@ 
           add (shift_left (of_int b8) 56)
           (add (shift_left (of_int b7) 48)
            (add (shift_left (of_int b6) 40)
             (add (shift_left (of_int b5) 32)
              (add (shift_left (of_int b4) 24)
               (add (shift_left (of_int b3) 16)
                (add (shift_left (of_int b2) 8)
                 (of_int b1))))))))
  
  let to_bytes i bytes pos =
    let i = Int64.of_int i in
    Bytes.unsafe_set 
        bytes (pos + 0) 
        (char_of_int Int64.(to_int (logand 0xffL i)));
    Bytes.unsafe_set 
        bytes (pos + 1) 
        (char_of_int Int64.(to_int (logand 0xffL (shift_right i 8))));
    Bytes.unsafe_set 
        bytes (pos + 2) 
        (char_of_int Int64.(to_int (logand 0xffL (shift_right i 16))));
    Bytes.unsafe_set 
        bytes (pos + 3) 
        (char_of_int Int64.(to_int (logand 0xffL (shift_right i 24))));
    Bytes.unsafe_set 
        bytes (pos + 4) 
        (char_of_int Int64.(to_int (logand 0xffL (shift_right i 32))));
    Bytes.unsafe_set 
        bytes (pos + 5) 
        (char_of_int Int64.(to_int (logand 0xffL (shift_right i 40))));
    Bytes.unsafe_set 
        bytes (pos + 6) 
        (char_of_int Int64.(to_int (logand 0xffL (shift_right i 48))));
    Bytes.unsafe_set 
        bytes (pos + 7) 
        (char_of_int Int64.(to_int (logand 0xffL (shift_right i 56))))
  
  let compare (x:int) (y:int) = Pervasives.compare x y 

end (* Int64 *)

module type StringLength = sig 
  val length : int 
end (* StringLength *) 

module MakeFixedLengthString (SL:StringLength) = struct 
  
  type t = string 

  let length = SL.length 

  let of_bytes_counter = ref 0 

  let compare_counter = ref 0 

  let of_bytes bytes pos = 
    incr of_bytes_counter;
    Bytes.sub_string bytes pos length

  let to_bytes s bytes pos = 
    assert(String.length s = length); 
    Bytes.blit_string s 0 bytes pos length

  let compare (l:string) (r:string) = 
    incr compare_counter; 
    Pervasives.compare l r  

  let to_string x = x 
  
  module Stats = struct 
    let of_bytes_count () = !of_bytes_counter
    let compare_count () =  !compare_counter
    let reset () = 
      of_bytes_counter := 0; 
      compare_counter := 0 
  end 

end (* MakeFixedLengthString *) 

module MakeMaxLengthString256 (SL:StringLength) = struct 

  type t = string 

  let length = SL.length + 1 
    (* first character is the length of the string *)

  let of_bytes bytes pos = 
    let s_length = int_of_char @@ Bytes.unsafe_get bytes pos in 
    assert(s_length <= SL.length); 
    let pos = pos + length - s_length in 
    Bytes.sub_string bytes pos s_length 

  let to_bytes s bytes pos = 
    let s_length = String.length s in
    assert(s_length < SL.length); 
    Bytes.unsafe_set bytes pos (char_of_int s_length);
    Bytes.blit_string s 0 bytes (pos + length - s_length) s_length

  let compare (l:string) (r:string) = 
    Pervasives.compare l r  

  let to_string x = x 
end
