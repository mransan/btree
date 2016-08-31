### Part 1 - API design review and introduction to continuation
-----


#### I) API Design Review

Designing a library requires the creation of a protocol with the calling application. 
The protocol complexity varies with the genericity of the API and how much configuration is implemented. 

Let’s take the example of a library reading some kind of record on disk:

**1. Stateless purely functional**

```Javascript
let record = get_record_by_first_name("john")
```

This type of API is the simplest and most likely what we should aim for unless requirements imposes more genericity. 

**2. Stateful**

```Javascript
let handle = create_handle () ; 
let record = handle.get_record_by_first_name("john");
```

or in a more C or functional way but completely equivalent way:

```Javascript
let handle = create_handle () in 
let record = get_record_by_first_name(handle, "john"); 
```

A typical use of this API is to increase performance, `create_handle` create a state which can then be re-use for multiple calls to `get_record_by_first_name`. 

Looking at the protocol we can see that it is self explanatory. The API enforces the caller to first create a handle and then call the `get_record_by_first_name`. 


**3. State with protocol** 

```Javascript
let handle = create_handle (); 
handle.start(); 
let record = handle.get_record_by_first_name("john");
```

We've all seen that type of API and here the big difference is that `start()` must be called before `get_record_by_first_name()`. The API now introduces a protocol
which is definitely not enforced by the API and if you are lucky it will be clearly documented. However we all figure it out pretty quickly, fix the code and most likely rant about the API to our friends. 

Note that language with type system you can actually enforce `start()` to be called before `get_record_by_first_name` by simply returning a type the handle with a new type alias. (Another blog post for that). 


**4. Code injection - Simple case** 

The most complex libraries require some form of code injection, usually to achieve more generic behavior and advance configuration. 

During an internal (private) computation, the library requires the client application to do specific computation. 
The computation can range from a single function to a more complex set of functions and types. 

Here is a Javascript example:

```Javascript
let handle = create_handle (); 
handle.start(); 
let record = handle.get_record(function (name, age) {
  return (name === "john"); 
}) 
```

This is the simplest case of course. This type of design is used extensively in a wide range of languages, from Ruby with each function capable of accepting a code block to eearlies version of C++ STL in function like `std::find_if` and finally OCaml with API like `List.find`. 

Depending on how advance the type system of the language is, the API will be more or less self documented. 

**5. Code injection - Complex case**

More complex code injection patterns can look like this (Javascript):

```Javascript
let disk_operation = {
  read : function (offset, length) {  /** Do IO operations **/ return bytes; } 
  write: function (offset, bytes)  {  /** Do IO operations **/ }
} 
let handle = create_handle(disk_operation); 
handle.start(); 
let record = handle.get_record(function (name, age) {
  return (name === "john");
});
```

In the example above the library designer decided to abstract its library 
from the implementation details of disk operations. 

Our Object-Oriented brain wiring kicks in and we automatically starts writing interfaces 
and various patterns associated with it. 

Indeed if your language does provide some support for interface the library will end up with something of the like (here C++):

```C++
class disk_operation_interface {
  virtual std::vector<char> read(int offset, int length) = 0; 
  virtual void write(int offset, std::vector<char> const& bytes) = 0; 
};

class handle {
  handle(disk_operation_interface *ops);
  /** ... **/
};
```

#### II) Code injection issues 
--

**1. Concurrency**

The previous `disk_operation_interface` assumes that both `read` and `write` are synchronous. However in modern asynchronous programming those 2 operations are not. Futures and promises are now the norm to deal with such operation. We could then change the interface to support promise but then the following question arise:
* Which type of promise to use. It's quite often that you would find multiple implementation. (C++ 11 has them but other C++ framework implement their own ([seastar](http://www.seastar-project.org/), OCaml has Lwt and Async).
* Why should a synchronous client be affected by the possibility to use asynchronous type of disk operations. 

Furthermore you do get the feeling that you're not really solving the problem of the API but rather making it work.

**2. Code injection and error handling**

In the example below the library designer will have to clearly defined the behavior of his API with the `read` or `write` function fails by throwing an exception. 
* Should the exception be catched with a 'catch-all' statement and transform to one of the exception defined by this library. 
* Should any exception be ignored and pass through to the caller (which knows the type of exception his code could throw).  


#### III) Continuation function
--

Functional programming techniques could be used instead to create a scalable alternative to the code injection design above.

The first thing is to introduce a new returned type to the `get_record` function: 

```OCaml
type record_result = 
  | Record of record 
  | Read_data of {offset: int; length: int; k : record_result_k } 

and  record_result_k = bytes -> record_result 

val get_record : handle -> string -> record_result 
```

The first choice the function can return is the record value itself. The second choice is a read operation value, 
made of the expected `offset` and `length` but also `k` a continuation function. 

This function `k` which takes as an input the `bytes` read by the caller and return the same output as the `get_record` function itself. 

The protocol that this API defines expects the caller to invoke the function `k` after having successfully read the requested bytes. 

The caller code should look something very similar to the following code for synchronous disk operation. 

```OCaml
let get_record_sync handle name = 
  let rec aux = function
    | Record r -> Ok (Some r) 
    | Read_data {offset; length; k} 
      match read_data_sync offset length with
      | bytes -> begin 
        match k bytes with
        | ret -> aux ret 
        | exception Not_found -> Ok None 
      end 
      | exception _ -> Error "error reading file"  
   in 
   get_record handle name |> aux 
```

Using the Lwt library in OCaml the asynchronous version would look like (without error handling):

```OCaml
open Lwt.Infix 

let get_record_sync handle name = 
  let rec aux = function
    | Record r -> r 
    | Read_data {offset; length; k} 
      read_data_sync offset length >|= k >|= aux 
   in 
   get_record handle name |> aux 
```

From those 2 examples we can see that 
a) No code is injected between the caller and the library
b) The library can be integrated in both synchronous and asynchronous way 
c) Error handling of read error is completely separated from the library error handling letting 
the caller decide how to handle those 2. 
