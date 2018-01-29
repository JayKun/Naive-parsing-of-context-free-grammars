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

(*given a rule and *)
let rec match_prefix pre rule rules derivation =
        match rule with
        | [] -> if pre=[] then None
	| (N nval)::sym_r -> match pre with
			     | [] -> None
			     | h::t -> match_nt_rules nt (rules nt) derivation 
        | (T tval)::sym_r -> match pre with
                	     | [] -> None
                	     | h::t-> if tval=sym then (match_prefix rhs sym_r rules derivation) 
                                   else None

let rec match_nt_rules nt rule_list derivation= match rule_list with
        | [] -> None
        | (rule::rule_lst) -> match (match_prefix nt rule rules derivation@[(nt, rule)]) with
                              None -> None
                              Some v -> v

let parse_prefix gram acceptor frag =
        let tuple_lst = get_pre_suf frag [] in
        let start = fst gram in
        let rule_func = snd gram in
        match tuple_lst with
        | []-> []
        | (pre, suf)::t -> if(match_prefix start (rule_func start) rule_func [] ) then 
