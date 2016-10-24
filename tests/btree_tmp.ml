
(* insert [v] at [pos]. Returns an array [a'] with 
   {ul
   {- [Array.lwength 'a = Array.length a + 1] }
   {- [a.(0) -> a.(pos-1) = a'.(0) -> a'.(pos -1)]} 
   {- [a.(pos) -> a.(len - 1) = a'.(pos + 1) -> a'.(len)]} 
   }
 *)
let array_insert a pos v = 
  let a_len = Array.length a in 
  let new_a_len = a_len + 1 in 
  let new_a = Array.make new_a_len v in 
  Array.blit a 0 new_a 0 pos; 
  Array.blit a pos new_a (pos + 1) (a_len - pos); 
  new_a  

let array_split_in_half a = 
  let half = Array.length a / 2 in 
  array_split_at a half


(* Unit test for used to be exposed FS_array *)

module TypedS8 = Btree.FS_array(String8) 

let make_typeds8 n = 
  TypedS8.make ~offset:0 ~bytes:(Bytes.create (n * String8.length)) ()

let () = 
  
  let t = make_typeds8 5 in
  
  TypedS8.set t 0 "12345678"; 
  assert(TypedS8.get t 0 = "12345678");
  
  TypedS8.set t 4 "12345678"; 
  assert(TypedS8.get t 4 = "12345678");

  begin match TypedS8.set t 5 "12345678" with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false) 
  end;

  begin match TypedS8.set t (-1) "12345678" with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false) 
  end;

  begin match TypedS8.get t 5 with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false) 
  end;
  
  begin match TypedS8.get t (- 1) with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false) 
  end
let a1to5 = [|
  "00000001";
  "00000002";
  "00000003";
  "00000004";
  "00000005";
|]

let () =

  let t = make_typeds8 5 in

  TypedS8.set_n t a1to5; 
  let a' = TypedS8.get_n t 5 in 
  assert(a1to5 = a'); 

  begin match TypedS8.set_n t (Array.make 6 "") with
  | exception Invalid_argument _ -> () 
  | _ -> assert(false);
  end;

  let t1 = make_typeds8 5 in 
  let t2 = make_typeds8 5 in

  let a1 = a1to5 in 
  let a2 = [|
    "0000000A";
    "0000000B";
    "0000000C";
    "0000000D";
    "0000000E";
  |] in
  TypedS8.set_n t1 a1;
  TypedS8.set_n t2 a2;

  TypedS8. blit t1 0 t2 0 5; 
  assert(a1 = TypedS8.get_n t2 5);
  
  TypedS8.set_n t1 a1;
  TypedS8.set_n t2 a2;
  TypedS8. blit t1 2 t2 3 2; 
  assert("0000000A" = TypedS8.get t2 0);
  assert("0000000B" = TypedS8.get t2 1);
  assert("0000000C" = TypedS8.get t2 2);
  assert("00000003" = TypedS8.get t2 3);
  assert("00000004" = TypedS8.get t2 4);

  assert(a1 = TypedS8.get_n t1 5);
  ()

let () =

  let t = make_typeds8 5 in 
    (* reset the bytes *)

  let a_minus3 = [|
    "00000001";
    "00000002";
    "00000004";
    "00000005";
  |] in 
  TypedS8.set_n t a_minus3; 

  TypedS8.insert_shift_right t 2 (Array.length a_minus3) "00000003"; 
  assert(a1to5 = TypedS8.get_n t 5);
  () 

let () = 
  
  let t = make_typeds8 5 in
    (* reset the bytes *)

  let a_minus5 = [|
    "00000001";
    "00000002";
    "00000003";
    "00000004";
  |] in 
  TypedS8.set_n t a_minus5; 

  TypedS8.insert_shift_right t 4 (Array.length a_minus5) "00000005"; 
  assert(a1to5 = TypedS8.get_n t 5)

let () =

  let t = make_typeds8 5 in 
    (* reset the bytes *)

  let a_minus5 = [|
    "00000001";
    "00000002";
    "00000003";
    "00000004";
  |] in 
  TypedS8.set_n t a_minus5; 

  TypedS8.insert_shift_right t 4 (Array.length a_minus5) "00000005"; 
  assert(a1to5 = TypedS8.get_n t 5)

let () =

  let t = make_typeds8 5 in 
    (* reset the bytes *)

  let a_minus1 = [|
    "00000002";
    "00000003";
    "00000004";
    "00000005";
  |] in 
  TypedS8.set_n t a_minus1; 

  TypedS8.insert_shift_right t 0 (Array.length a_minus1) "00000001"; 
  assert(a1to5 = TypedS8.get_n t 5)

