(*Question 1*)

let rec convert_helper n_term rules =
	match rules with
	| [] -> []
	| (lhs, rhs)::t -> if n_term = lhs then rhs@(convert_helper n_term t)
			   else (convert_helper nterm t)

let rec convert_grammar gram1 =
	(fst gram1, convert_helper n_term rules)

