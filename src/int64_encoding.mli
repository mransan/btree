(** Utility module to encode integer value with little endian 32 bit 
    encoding.

    It also implements the [Fixed_size_sig] module signature which is 
    later defined.
 *)

type t = int 

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
