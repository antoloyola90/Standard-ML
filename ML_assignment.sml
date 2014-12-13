Control.Print.printDepth := 100;
Control.Print.printLength := 100;
     
(* question 1 *)
fun getSmaller [] pivot = [] | 
	getSmaller (x::xs) pivot = (if x <= pivot then x:: (getSmaller xs pivot) else (getSmaller xs pivot))

fun getBigger [] pivot = [] | 
	getBigger (x::xs) pivot = (if x > pivot then x:: (getBigger xs pivot) else (getBigger xs pivot))
							
fun intPartitionSort [] = [] | 
	intPartitionSort (x::xs) = (intPartitionSort (getSmaller xs x)) @ [x] @ (intPartitionSort (getBigger xs x))
									
(* question 2 *)
fun getSmallerOp comp [] pivot = [] | 
	getSmallerOp comp (x::xs) pivot = (if comp(x,pivot) then x:: (getSmallerOp comp xs pivot) else (getSmallerOp comp xs pivot))

fun getBiggerOp comp [] pivot = [] | 
	getBiggerOp comp (x::xs) pivot = (if comp(x,pivot) then (getBiggerOp comp xs pivot) else x:: (getBiggerOp comp xs pivot))
							
fun partitionSort comp [] = [] | 
	partitionSort comp (x::xs) = (partitionSort comp (getSmallerOp comp xs x)) @ [x] @ (partitionSort comp (getBiggerOp comp xs x))

(* question 3 *)
datatype 'a tree = leaf of 'a | node of ('a tree list) 

(* question 4 *)
fun sortTree comp (leaf x) = leaf (partitionSort comp x) |  
	sortTree comp (node x) =  node (map (sortTree comp) x) 
 									
(* question 5 *)
fun merge comp [ ] y = y | 
	merge comp (x::xs) [ ] = (merge comp (getSmallerOp comp xs x) []) @ [x] @ (merge comp (getBiggerOp comp xs x) []) | 
	merge comp (x::xs) y =  (merge comp (getSmallerOp comp (xs@y) x) []) @ [x] @ (merge comp (getBiggerOp comp (xs@y) x) []) 

(* question 6 *)
fun getLeaf (leaf x) = x
fun mergeTree comp (leaf x) = getLeaf (sortTree comp (leaf x)) | 
	mergeTree comp (node x) = foldr (fn(a,b) => merge comp a b) [] (map (mergeTree comp) x) 
 					