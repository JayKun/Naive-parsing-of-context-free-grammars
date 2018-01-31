type my_nonterminals = 
| S | Obj | OS | W | Adj | U | P

let accept_all derivation string = Some (derivation, string)

let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None
 
let grammar_test_1 =     
(S, function
        | S -> [[N Obj; N U; N W]; 
		[N Obj]; 
		[N Adj]]
        | Obj ->[[N OS]; 
		[N OS; N S]]
        | W -> [[N U; N Obj; N Adj]; 
		[N Adj; N Adj; N P];
		[N Adj; N S]]
	| Adj -> [[T "great"];
		 [T "cool"];
		 [T "hot"]]
	| U -> [[T"is"];
		[T "will be"]]
        | OS -> [[T"Human"]; 
		[T"Computers"]; 
		[T"Peanuts"]; 
		[T "Jelly"]; 
		[T"cs131"];
		[T"Eggert"]]
        | P -> [[T"?"];
		[T"!"]; 
		[T"??"]]
) 

let test_1 = parse_prefix grammar_test_1 accept_all ["Eggert"; "is"; "hot"; "cool"; "!"; "??"]=
Some
   ([(S, [N Obj; N U; N W]); (Obj, [N OS]); (OS, [T "Eggert"]);
     (U, [T "is"]); (W, [N Adj; N Adj; N P]); (Adj, [T "hot"]);
     (Adj, [T "cool"]); (P, [T "!"])],
    ["??"])

let test_2 = parse_prefix grammar_test_1 accept_all ["Jelly"; "will be"; "great"; "Human"; "Computers"; "??"; "I"; "am"; "tired"]=
 Some
   ([(S, [N Obj; N U; N W]); (Obj, [N OS]); (OS, [T "Jelly"]);
     (U, [T "will be"]); (W, [N Adj; N S]); (Adj, [T "great"]); (S, [N Obj]);
     (Obj, [N OS]); (OS, [T "Human"])],
    ["Computers"; "??"; "I"; "am"; "tired"])
