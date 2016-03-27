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

module TwoListQueue : Queue = struct
  type 'a queue = {front:'a list; back: 'a list}
  exception Empty
  let empty = {front = []; back = []}
  let is_empty = function
    | {front = []; back = []} -> true
    | _ -> false
  let norm = function
    | {front = []; back} -> {front = List.rev back; back = []}
    | q -> q
  let enqueue x q = norm {q with back = x::q.back}
  let peek = function
    | {front=[]; _} -> raise Empty
    | {front=x::_; _} -> x
  let dequeue = function 
    | {front=[]; _} -> raise Empty
    | {front=_::xs; back} -> norm {front=xs; back}
  let fold {front; back} acc f = 
    let mid = List.fold_right front ~f:f ~init:acc in
    List.fold_right (List.rev back) ~f:f ~init:mid
end

module type Foldable = sig
  type 'a t
  val fold : 'a t -> 'acc -> ('a -> 'acc -> 'acc) -> 'acc
end


(* module type Iterable = sig
  type 'a t
  val iter : 'a t -> ('a -> unit) -> unit
end
*)

module MakeIterable (F:Foldable) = struct
  type 'a t = 'a F.t
  let iter x f = F.fold x () (fun b () -> f b)
end

module QueueWithT(Q:Queue) = struct 
  include Q
  type 'a t = 'a Q.queue
end

module IterableListQueue' = MakeIterable(QueueWithT(ListQueue))

module MakeIterableQueue(Q:Queue) = struct
  include Q
  include MakeIterable(QueueWithT(Q))
end

module IterableListQueue = MakeIterableQueue(ListQueue)
module IterableTwoListQueue = MakeIterableQueue(TwoListQueue)

let q1 = IterableListQueue.(enqueue 10 (enqueue 5 empty))
let _  = IterableListQueue.iter q1 print_int

