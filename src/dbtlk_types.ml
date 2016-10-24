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

and 'a res_read_data_k = bytes -> 'a res 

and 'a res_allocate_k = file_offset -> 'a res 

let res_done x = Res_done x
let res_read_data block k = Res_read_data (block, k) 
let res_allocate block_length k = Res_allocate (block_length, k) 

let rec res_bind f = function
  | Res_done x -> f x 

  | Res_read_data (block, k) -> 
    Res_read_data (block, fun bytes -> k bytes |> res_bind f) 

  | Res_allocate (length, k) -> 
    Res_allocate (length, fun offset -> k offset |> res_bind f)
  
let rec res_map f = function
  | Res_done x -> Res_done (f x) 

  | Res_read_data (block, k) -> 
    Res_read_data (block, fun bytes -> k bytes |> res_map f) 

  | Res_allocate (length, k) -> 
    Res_allocate (length, fun offset -> k offset |> res_map f)
