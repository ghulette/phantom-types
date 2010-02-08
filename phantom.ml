(* 

Example illustrates use of a ``phantom type'', as described in [1]. The idea
is to set up a one-to-one correspondence between values and types in order to
statically check certain properties. The encoding shown here sets up a family
of types, one for each non-negative integer, expressed in binary (the same
idea would work for decimal numbers). These types can be used to describe
static bounds; for example, in the paper they use them to assign unique types
to fixed size arrays in C.

[1] M. Blume. No-longer-foreign: Teaching an ML compiler to speak C natively.
    In Electronic Notes in Theoretical Computer Science, volume 59, 2001. 

*)

module type BINARY =
sig
  (* Statically preclude leading zeros *)
  type zero
  type nonzero
  
  (* *)
  type bin
  type 'a dg0 and 'a dg1
  type ('a, 'b) dim
  val bin : (bin, zero) dim
  val dg0 : ('a, nonzero) dim -> ('a dg0, nonzero) dim
  val dg1 : ('a, 'b) dim -> ('a dg1, nonzero) dim
  val to_int : ('a, 'b) dim -> int
end

module Binary : BINARY =
struct
  type zero = unit
  type nonzero = unit
  type bin = unit
  type 'a dg0 = unit
  type 'a dg1 = unit
  type ('a, 'b) dim = int
  let bin = 0
  let dg0 d = 2 * d
  let dg1 d = 2 * d + 1
  let to_int d = d
end

open Binary
open Printf

(* Encode the value 4 as a type.  Note the encoding is right to left. *)
let four = dg0 (dg0 (dg1 bin));;

(* Convert the singluar value of that type to a integer. *) 
printf "%d\n" (to_int four);;
