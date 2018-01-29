(*Question 1*)

let rec convert_helper n_term rules =
	match rules with
	| [] -> []
	| (lhs, rhs)::t -> if n_term = lhs then rhs::(convert_helper n_term t)
			   else (convert_helper n_term t)

let convert_grammar gram1 =
	(fst gram1, (function x -> convert_helper x (snd gram1)))

(*Question 2*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* An acceptor accepts a fragment and returns whether that fragment is 
*  1. Need to figure out a way to return prefixes given a fragment
*
*)

(*returns a list of tuple containing all (prefix, suffix) pairs*)
let rec get_pre_suf frag head = 
	match frag with
	| [] -> [(head, [])]
	| h::t -> (head@[h], t)::(get_pre_suf t (head@[h]))

let rec match_pre_rule pre rule rules=
	match rule with 
	| [] -> if pre=[] then None
	| h::t -> match pre with
		| [] -> None
		| (T tval)::rhs -> if tval=h then (match_pre_rule rhs t rules) else None 
		| (N nval)::rhs -> (matcher nval (rules nval) rules 

let parse_prefix gram acceptor frag =
	let tuple_lst = get_pre_suf frag [] in
	match tuple_lst with
	| []-> []
	| (pre, suf)::t -> if(match_pre_rule pre (snd gram)pre (snd gram)) then
				let derivation_pre = derive_pre prefix rules in
				acceptor rules suf
