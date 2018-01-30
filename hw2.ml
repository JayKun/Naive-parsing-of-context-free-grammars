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
	| [] -> []
	| h::t -> (head@[h], t)::(get_pre_suf t (head@[h]))

(*given a rule and a prefix we check whether it matches*)
let rec match_prefix pre rule rule_func derivation =
        match rule with
	| [] -> Some(derivation)
	| _ -> match pre with
		| [] -> None
		| h::t -> match rule with
			| [] -> None
			| (N nval)::sym_r -> (match_nt_rules nval (rule_func nval) rule_func derivation) 
			| (T tval)::sym_r -> if tval=h then (match_prefix t sym_r rule_func derivation) 
and match_nt_rules nt rule_list rule_func derivation =
	match rule_list with
        | [] -> None
        | (rule::rule_lst) -> match (match_prefix nt rule rules derivation@[(nt, rule_list)]) with
			      (*Dead End: Try next rule if match_prefix returns None*)
                              | None -> match_nt_rules nt rule_lst rule_func derivation
			      (*Only returns when match_prefix returns a derivation*) 
                              | Some v -> Some v

let parse_prefix gram acceptor frag =
        let tuple_lst = get_pre_suf frag [] in
        let start = fst gram in
        let rule_func = snd gram in
        match tuple_lst with
        | []-> []
        | (pre, suf)::t -> match (match_nt_rules start (rule_func start) rule_func []) with
			| None ->  None
			| Some v -> let derivation = Some (v) in
			(accept derivation suf)
			
