type ('k, 'v) bstree =
  | None
  | Node of 'k * 'v * ('k, 'v) bstree * ('k, 'v) bstree

let rec lookup key tree = 
  match tree with
  | Node (k, value, lt, rt) -> if k = key then value 
     else (if k > key then lookup key lt else lookup key rt)
  | None -> None

let rec insert key value tree =
  match tree with
  | None -> Node (key, value, None, None)
  | Node (k, v, lt, rt) -> if k = key then 
    Node (k, value, lt, rt)
   else (
     if k > key then Node (k, v, insert key value lt, rt)
     else Node (k, v, lt, insert key value rt)
   ) 
