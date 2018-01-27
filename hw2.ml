(*Question 1*)

let rec convert_helper n_term rules =
	match rules with
	| [] -> []
	| (lhs, rhs)::t -> if n_term = lhs then rhs::(convert_helper n_term t)
			   else (convert_helper n_term t)

let convert_grammar gram1 =
	(fst gram1, (function x -> convert_helper x (snd gram1)))

(*Question 2*)
let parse_prefix_helper gram acceptor frag =
	match frag with
	[] -> None
	h::t -> if(acceptor h (snd gram)=None) then None
		else (fun x y -> parse_prefix_helper gram acceptor flag)  

let parse_prefix gram acceptor frag =
	
