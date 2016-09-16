(** {2 Common Interface} *)

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

(** {2 Int Encoding} *)

(** Utility module to encode integer value with little endian 32 bit 
    encoding.

    It also implements the [Fixed_size_sig] module signature which is 
    later defined.
 *)
module Int64 : sig 
  
  include (CoreSig with type t = int)

end (* Int64 *)

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
module MakeMaxLengthString256 (SL:StringLength) : sig

  include (CoreSig with type t = string) 

end 
