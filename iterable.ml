module type Queue = sig
  type 'a queue
  exception Empty
  val empty : 'a queue
  val is_empty : 'a queue -> bool
  val enqueue : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a
  val dequeue : 'a queue -> 'a queue
  val fold : 'a queue -> 'acc -> ('a -> 'acc -> 'acc) -> 'acc
end

module ListQueue : Queue = struct
  type 'a queue = 'a list
  exception Empty
  let empty = []
  let is_empty q = q = []
  let enqueue x q = q @ [x]
  let peek = function
    | [] -> raise Empty
    | x::_ -> x
  let dequeue = function
    | [] -> raise Empty
    | _::q -> q
  let fold q acc func = List.fold_right q ~f:func ~init:acc
end

