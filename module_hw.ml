module type Arith = sig
  type t
  val zero : t
  val one : t
  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
  val (~-) : t -> t
end


module Ints : Arith = struct
  type t = int
  let zero = 0
  let one = 1
  let (+) a b = a + b
  let ( * ) a b = a * b
  let (~-) a = -a
end
