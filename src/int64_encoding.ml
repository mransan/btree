
let byte pos bytes = 
  int_of_char (Bytes.unsafe_get bytes pos)

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
