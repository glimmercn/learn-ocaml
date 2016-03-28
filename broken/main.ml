#directory "_build";;

#load "listDict.cmo";;

#load "bstDict.cmo";;

#load "sortedListDict.cmo";;

let x = BstDict.(insert empty 0 'x')
let y = ListDict.(insert empty 1 'y')
let z = SortedListDict.(insert empty 2 'z')
