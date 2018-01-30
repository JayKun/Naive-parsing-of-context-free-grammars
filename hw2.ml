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

(*returns a list of tuple containing all (prefix, suffix) pairs*)
let rec get_pre_suf frag head = 
	match frag with
	| [] -> []
	| h::t -> (head@[h], t)::(get_pre_suf t (head@[h]))

let is_terminal sym =
	match sym with
	| T _ -> true
	| _ -> false

(*given a rule and a prefix we check whether it matches*)
let rec match_frag frag rule rule_func derivation acceptor=
        if rule = [] then Some derivation
	else
	match frag with
		| [] -> None
		| h::t -> match rule with
			| [] -> None
			| (N nval)::sym_r -> (match_nt_rules nval (rule_func nval) rule_func derivation acceptor t) 
			| (T tval)::sym_r -> if tval=h then (match_frag t sym_r rule_func derivation acceptor) 
					     else None (*Invalid Terminal*)

and match_nt_rules nt rule_list rule_func derivation acceptor frag = match rule_list with
        | [] -> None (*Could not find non-terminal in grammar*)
        | rule::rule_lst -> match (match_frag frag rule rule_func (derivation@[(nt, rule_list)]) acceptor) with
			      (*Dead End: Try next rule if match_prefix returns None*)
                              | None -> match_nt_rules nt rule_lst rule_func derivation acceptor frag
			      (*Only returns when match_prefix returns a legit derivation*) 
                              | Some v -> Some v


let parse_prefix gram acceptor frag =
        let start = fst gram and
        rule_func = snd gram in
	
	match_nt_rules start (rule_func start) rule_func [] acceptor frag
		
