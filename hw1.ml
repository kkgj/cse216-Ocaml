(*
	Gordon Zhong
	112192897
*)

(*1.1*)
let rec pow x n = match n with
	| 0 -> 1
	| 1 -> x
	| _ -> x * pow x (n - 1);;

let rec float_pow x n = match n with
	| 0 -> 1.
	| 1 -> x
	| _ -> x *. float_pow x (n - 1);; 

(*1.2*)
let rec compress = function
	| [] -> []
    | [x] -> [x]
    | a :: b :: t -> if a = b then compress (b::t) else a :: compress (b::t);;

(*1.3*)
let rec remove_if l f = match l with
	| [] -> []
	| a :: b -> if f a then remove_if b f else a :: remove_if b f;;

(*1.4*) 
let rec helper lst num f = match lst with
	| [] -> [[num]]
	| a :: b -> if (f (List.hd a) num) then (num :: a) :: b else a :: helper b num f;;

let equivs f l = match l with
	| [] -> [[]]
	| _  -> let rec helper2 f l answer = match l with
				| [] -> answer
				| a :: b -> helper2 f b (helper answer a f) 
			in helper2 f l [];;

(*1.5*)
let rec head x lst = match lst with
    | [] -> []
    | a :: b -> if x <= 0 then [] else a :: head (x-1) b;;

let rec tail y lst = match lst with
      | [] -> []
      | a :: b -> if y = 0 then a :: b else tail (y-1) b;;      

let slice l i j = head (j - i) (tail i l);; 

(*1.6*)
let composition f1 f2 a = f1(f2 a);;

(*1.7*)
let equiv_on f g lst = if List.map f lst = List.map g lst then true else false;;

(*1.8*)
let rec pairwisefilter f lst = match lst with
	| [] -> []
	| [a] -> [a]
	| a :: b :: c -> (f a b) :: pairwisefilter f c;;

(*1.9*)
let rec polynomial lst z = match lst with
	| [] -> 0
	| a :: b -> ((fun (x,y) -> x * (pow z y)) a) + (polynomial b z);; 

(*1.10*)
let rec powerset = function
    | [] -> [[]]
    | a :: b -> powerset b @ List.map (fun x -> a :: x) (powerset b);; 





