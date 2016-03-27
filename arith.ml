module type Ring = sig
  type t
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val mult : t -> t -> t
end

module type Field = sig
  include Ring
  val neg : t -> t
  val div : t -> t -> t
end

module FloatRing = struct
  type t = float
  let zero = 0.
  let one = 1.
  let add = (+.)
  let mult = ( *. )
end

module FloatField = struct
  include FloatRing
  let neg = (~-.)
  let div = (/.)
end

module FR : Ring = FloatRing 
module FF : Field = FloatField
  
module type Arith = sig
  type t
  val zero : t
  val one : t
  val(+) : t-> t-> t
  val ( * ) : t->  t -> t

  val to_string : t -> string
end

module Ints : Arith = struct
  type t = int
  let zero  = 0
  let one   =  1
  let (+) a b = a + b
  let ( * ) a b = a * b
  let to_string a = string_of_int a
end

module ExtendArith (A : Arith) : Arith = struct
  include A
  let rec of_int = function
    | 0 -> A.zero
    | 1 -> A.one
    | x -> A.(one + of_int (x-1)) 
end

  
