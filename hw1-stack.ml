(*
	Gordon Zhong
	112192897
*)

(*2.2*)
type stack = 
	| Node of int list;;

let start func = func(Node([]));; 

let push n lst f = match lst with
	| Node(l) -> f(Node(n::l));;

let pop lst f = match lst with
	| Node([]) -> f(Node([]))
	| Node(a :: b) -> f(Node(b));;

let add lst f = match lst with 
	| Node([]) -> f(Node([]))
	| Node([a]) -> f(Node([a]))
	| Node(a::b::c) -> f(Node((a + b) :: c));;

let mult lst f = match lst with 
	| Node([]) -> f(Node([]))
	| Node([a]) -> f(Node([a]))
	| Node(a::b::c) -> f(Node((a * b) :: c));;	

let clone lst f = match lst with 
	| Node([]) -> f(Node([]))
	| Node(a::b) -> f(Node(a :: a :: b));;

let rec kHelp a lst f = match lst with
	| Node([]) -> f(Node([]))
	| Node(s :: t) -> if a = 0 then f(Node(s::t)) else kHelp (a-1) (Node(t)) f;;

let kpop lst f = match lst with
	| Node([]) -> f(Node([]))
	| Node(k :: b) -> if k > 0 then kHelp k (Node(b)) f else f(Node(b));;	 

let halt = function 
	| Node(l) -> l;;