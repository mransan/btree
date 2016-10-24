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

module type Record_sig = sig 

  (** {2 Record type} *) 

  type t 

  val to_bytes : t -> bytes 
  (** [to_bytes t] encodes [t] to bytes *)

  val of_bytes : bytes -> t 
  (** [of_bytes bytes] decodes a value of type [t] from [bytes] *)

  val to_string : t -> string 
  (** [to_string t] returns a string for debugging purposes only *)

  (** {2 Indices} *) 

  module Key0 : Dbtlk_btree.Key_sig 

  val index0 : t -> Key0.t  
  (** [index1 t] access the key1 for a given record *)

  module Key1 : Dbtlk_btree.Key_sig 

  val index1 : t -> Key1.t 
  (** [index1 t] access the key2 for a given record *)

end (* Record_sig *) 

module Make(Record:Record_sig) : sig

  type t 

  val make_empty : unit -> (t * Dbtlk_types.write_op list )
  
  val make_from_file : unit -> t Dbtlk_types.res  

  val insert : t -> Record.t -> Dbtlk_types.write_op list Dbtlk_types.res  

  val debug : t -> unit Dbtlk_types.res 

  val to_string : t -> string 

end 
