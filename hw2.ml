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

(*given a rule and a frag we check whether it matches*)
let rec match_frag rule rule_func acceptor derivation frag=
        if rule = [] then acceptor derivation frag
	else
	match frag with
		| [] -> if rule = [] then acceptor derivation frag
			else None 
		| h::t -> match rule with
			| [] -> None
			| (T tval)::sym_r -> if tval=h then(match_frag sym_r rule_func acceptor derivation t) 
					     else None (*Invalid Terminal*)
			| (N nval)::sym_r -> 
			let in_accept = match_frag sym_r rule_func acceptor in  
			(match_nt_rules nval (rule_func nval) rule_func derivation frag in_accept) 
and match_nt_rules nt rule_list rule_func derivation frag acceptor= 
	match rule_list with
        | [] -> None (*Could not find non-terminal in grammar*)
        | rule::rule_lst-> 
        (*Dead End: Try next rule if match_prefix returns None*)
	let res = (match_frag rule rule_func acceptor (derivation@[(nt, rule)]) frag) in 
	if res = None
        then (match_nt_rules nt rule_lst rule_func derivation frag acceptor)
	(*Only returns when match_prefix returns a legit derivation*) 
        else res


let parse_prefix gram acceptor frag =
        let start = fst gram and
        rule_func = snd gram in
	
	let res = (match_nt_rules start (rule_func start) rule_func [] frag acceptor) in
	res
