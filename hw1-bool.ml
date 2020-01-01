(*
	Gordon Zhong
	112192897
*)

(*2.1*)
type bool_expr =
	| Lit of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or of bool_expr * bool_expr;;

let rec solve a av b bv exp = match exp with
	| Lit s -> if s = a then av else bv
	| Or(s, t) -> solve a av b bv s || solve a av b bv t
    | Not e -> not(solve a av b bv e)
    | And(s, t) -> solve a av b bv s && solve a av b bv t;;

let truth_table a b expression =
    [(true,  true,  solve a true  b true  expression); 
     (true,  false, solve a true  b false expression);
     (false, true,  solve a false b true  expression);
     (false, false, solve a false b false expression)];;