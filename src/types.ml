module type Debug_sig = sig
  type t 
  val to_string : t -> string 
end 

module type Fixed_size_sig = sig 

  type t 
  val length : int 
  val to_bytes : t -> bytes -> int -> unit  
  val of_bytes : bytes -> int -> t 
end 

module type Comparable_sig = sig 
  type t 
  val compare : t -> t -> int
end 

type file_offset = int

type block_length = int 

type block = {
  offset: file_offset; 
  length: block_length;
}

let string_of_block {offset; length} = 
  Printf.sprintf "offset: %i, length: %i" offset length

type read_op = block 

type write_op = {
  offset: file_offset; 
  bytes : bytes;
}
  
type 'a res = 
  | Res_done of 'a 
  | Res_read_data of block * 'a res_read_data_k 
  | Res_allocate of block_length * 'a res_allocate_k 
  | Res_append of bytes * 'a res_append_k 

and 'a res_read_data_k = bytes -> 'a res 

and 'a res_allocate_k = file_offset -> 'a res 

and 'a res_append_k = file_offset -> 'a res 

let res_done x = Res_done x
let res_read_data block k = Res_read_data (block, k) 
let res_allocate block_length k = Res_allocate (block_length, k) 
let res_append bytes k = Res_append (bytes, k) 

let rec res_bind f = function
  | Res_done x -> f x 

  | Res_read_data (block, k) -> 
    Res_read_data (block, fun bytes -> k bytes |> res_bind f) 

  | Res_allocate (length, k) -> 
    Res_allocate (length, fun offset -> k offset |> res_bind f)
  
  | Res_append (bytes, k) -> 
    Res_append (bytes, fun offset -> k offset |> res_bind f)

let rec res_map f = function
  | Res_done x -> Res_done (f x) 

  | Res_read_data (block, k) -> 
    Res_read_data (block, fun bytes -> k bytes |> res_map f) 

  | Res_allocate (length, k) -> 
    Res_allocate (length, fun offset -> k offset |> res_map f)
  
  | Res_append (bytes, k) -> 
    Res_append (bytes, fun offset -> k offset |> res_map f)
