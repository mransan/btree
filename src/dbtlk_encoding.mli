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

(** {2 Common Interface} *)

module type CoreSig = sig 
  type t 

  include Dbtlk_types.Fixed_size_sig with type t := t 
  include Dbtlk_types.Comparable_sig with type t := t 
  include Dbtlk_types.Debug_sig with type t := t 
end 

(** {2 Int Encoding} *)

(** Utility module to encode integer value with little endian 32 bit 
    encoding.

    It also implements the [Fixed_size_sig] module signature which is 
    later defined.
 *)
module Int64 : CoreSig with type t = int

(** {2 String Encoding} *)

module type StringLength = sig 

  val length : int 

end (* StringLength *) 

module MakeFixedLengthString (SL:StringLength) : sig 
  
  include (CoreSig with type t = string) 

  module Stats : sig 
    val of_bytes_count : unit -> int 
    (** number of times [of_bytes] was called since last [reset] *)

    val compare_count : unit -> int 
    (** number of times [compare] was called since last [reset] *)

    val reset : unit -> unit 
    (** [reset ()] resets all the counts to [0] *)
  end 

end (* MakeFixedLengthString *) 

(** [MakeMaxLengthString256] provide the encoding of a string which maximum
    length is known (and must be less or equal than 256). The string length
    does not have to be of a fixed size, only the upper bound is fixed. 
 
    -- Implementation detail -- 
 
    The string is encoded with a fixed size of [length + 1] with the first byte
    of the encoding containing the length of the string. 
 
    +---------------+---------------------+--------------+
    | String Length | Uninitialized Bytes | String value | 
    + --------------+---------------------+--------------+
    |   1 Byte      |            Max Length              | 
    + --------------+------------------------------------+
 *)
module MakeMaxLengthString256 (SL:StringLength) : CoreSig with type t = string
