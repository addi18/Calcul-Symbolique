type gdnb = {signe : bool; abs : (int*int) list};;
let l = {signe = false;abs = [(0, 4754); (1, 6231); (2, 895); (4, 4567); (5, 4)]};;
let s = "-445670000089562314754";;

(*Renvoie l'equivalent d'un string de nombre en gdnb.*)
(*Le parcours part de la fin de la chaine et prend a chaque tour 4 chiffre et construit *)
(*la liste jusqu'a ce que le string soit parcouru jusqu'au String.length % 4*)
let abs_of_string s = 
	let len = String.length s in
		if len = 0 then [] 
		else 
		let r = len mod 4 in
		let rec aux i acc k =
			if (k <= r + 1) then let str = (String.sub s 0 r) in if (String.length str) > 0 then List.rev ((i, int_of_string str) :: acc) else List.rev acc
			else
				let sub = String.sub s (k - 4) 4 in let num = int_of_string sub in
					if num <> 0 then aux (i + 1) ((i, num) :: acc) (k - 4) else aux (i + 1) acc (k - 4)
		in aux 0 [] (String.length s);;

(*Renvoie un string de nombre correspondant au gdnb.*)
(*On parcourt la liste avec un indice qui compte le nombre de coefficient a avoir et equivalent au degre*)
(* et s'il manque un degre dans la liste on ajoute 4 zeros et s'il ya des coefficients qui ne sont pas composes*)
(* de 4 chiffres on ajoute des zeros pour completes.*)
let string_of_abs l = 
	let last = fst (List.nth l (List.length l - 1)) in
	let rec aux acc l i = 
		match l with
			| [] -> acc
			| h :: t when (fst h) = i && i <> last -> let n = string_of_int (snd h) in let len = String.length n in
																								if len = 4 then aux (n ^ acc) t (i + 1)
																								else 
																									if (4 - len = 3) then aux ("000" ^ n ^ acc) t (i + 1) 
																									else if (4 - len) = 2 then aux ("00" ^ n ^ acc) t (i + 1)
																									else aux ("0" ^ n ^ acc) t (i + 1)
			| h :: t when (fst h) = i && i = last -> aux (string_of_int (snd h) ^ acc) t (i + 1)
			| h :: t -> aux ("0000" ^ acc) (h :: t) (i + 1)
	in aux "" l 0;;

(*exo1*)
let string_to_gdnb s = 
	let c = String.get s 0 in 
	if c <> '-' && c <> '+' then {signe = true; abs = abs_of_string s}
	else
		 let list = abs_of_string (String.sub s 1 ((String.length s) -1)) in
		 if c = '-' then {signe = false; abs = list}
		 else {signe = true; abs = list};;

let gdnb_to_string l = let s = l.signe in let string = string_of_abs l.abs in
	if s = false then "-"^string
	else "+"^string;;

(*exo2*)
let rec degre (l:(int*int) list) = 
	match l with
		| [] -> 0
		| t :: [] -> fst t
		| _ :: q -> degre q;;

(*Renvoie le nombre de degre plus grand*)
let last_element l =
	if (List.length l.abs = 0) then failwith "Impossible de trouver le dernier element"
	else snd (List.nth l.abs ((List.length l.abs) - 1));;

(*Renvoie le nombre d'elemnt d'une liste <=> List.length*)
let nbr_element (l:(int*int) list) =
	let rec aux l n = 
		match l with
		| [] -> n
		| _ :: q -> aux q n + 1
	in aux l 0;;

(*Compare l1 et l2 de meme degre*)
let compare_gdnb_same_deg l1 l2 = 
	let nb1 = nbr_element l1 and nb2 = nbr_element l2 in 
	if nb1 <> nb2 then 
		let last1 = last_element l1 and last2 = last_element l2 in 
		if last1 > last2 then 1 
		else if last1 = last2 then
			if nb1 > nb2 then 1 else -1
		else -1
	else
		let rec aux l1 l2 acc = match
			l1, l2 with
			| [], [] -> acc
			| t1 :: q1, t2 :: q2 when (snd t1) > (snd t2) -> let acc = 1 in aux q1 q2 acc
			| t1 :: q1, t2 :: q2 when (snd t1) < (snd t2) -> let acc = -1 in aux q1 q2 acc
			| _ :: q1, _ :: q2 -> aux q1 q2 acc
			| (_::_, [])|([], _::_) -> failwith "Impossible de faire cette operation!"
		in aux l1 l2 0;;

let compare_gdnb_same_deg2 (l1:(int*int) list) (l2:(int*int) list) = 
	let nb1 = List.length l1 and nb2 = List.length l2 in 
	if nb1 <> nb2 then 
		let last1 = snd (List.nth l1 (nb1 - 1)) and last2 = snd (List.nth l2 (nb2 - 1)) in 
		if last1 > last2 then 1 
		else if last1 = last2 then
			if nb1 > nb2 then 1 else -1
		else -1
	else
		let rec aux l1 l2 acc = match
			l1, l2 with
			| [], [] -> acc
			| t1 :: q1, t2 :: q2 when (snd t1) > (snd t2) -> let acc = 1 in aux q1 q2 acc
			| t1 :: q1, t2 :: q2 when (snd t1) < (snd t2) -> let acc = -1 in aux q1 q2 acc
			| _ :: q1, _ :: q2 -> aux q1 q2 acc
			| (_::_, [])|([], _::_) -> failwith "Impossible de faire cette operation!"
		in aux l1 l2 0;;

let compare_gdnb_abs l1 l2 = 
	let d1 = degre l1 and d2 = degre l2 in 
		if d1 > d2 then 1 else if d1 < d2 then -1 else compare_gdnb_same_deg2 l1 l2;;

let compare_gdnb l1 l2 = 
	if l1.signe <> l2.signe then
		if l1.signe = false then -1
		else 1
	else
		let d1 = degre l1.abs and d2 = degre l2.abs in 
		if l1.signe = false && l2.signe = false then
			if d1 > d2 then -1 else if d1 < d2 then 1 else -compare_gdnb_same_deg2 l1.abs l2.abs
		else
			if d1 > d2 then 1 else if d1 < d2 then -1 else compare_gdnb_same_deg2 l1.abs l2.abs;;

let compare_gdnb_bool l1 l2 = 
	let r = compare_gdnb l1 l2 in
	if r = 1 || r = 0 then true
	else false;;

let g1=(string_to_gdnb "1234567890098765");;
let g2=(string_to_gdnb "1234567891198765");;
(compare_gdnb g1 g2);;
let g2=(string_to_gdnb "-1234567891198765");;
(compare_gdnb g1 g2);;

(*exo3*)
(*on peut remplacer les fx auxiliares par n/ 10000 ou n mod 10000*)
(*Renvoie le nombre de chiffre d'un nombre*)
let nbr_chiffre n =
	let rec aux n acc = 
		match n with
		| t when t < 10 -> acc + 1
		| t -> aux (t / 10) (acc + 1)
	in aux n 0;;

(*Renvoie le chiffre le plus a gauche d'un nombre*)
let rec get_left_number n =
		match n with
		| t when t < 10 -> t
		| t -> get_left_number (t / 10);;

(*On a deux listes, l'accumulateur qui contiendra le resultat final et lesurplus r d'une addition*)
(*On parcourt l1 et l2 et a chaque fois on additionne les coefficients de r et s'il ya un surplus on *)
(* le stocke dans r pour la prochaine addition*)
(*incorrecte*)
let add_gdnb_abs2 (l1:(int*int) list) (l2:(int*int) list) = 
	let rec aux l1 l2 acc r = 
		match l1, l2 with
		| [], [] -> List.rev acc
		| [], (d2, c2) :: q -> if (List.length q) = 0 then 
														let n = r + c2 in let nb = (nbr_chiffre n) in
													 	if nb > 4 then let r = (get_left_number n) in let re = (n mod 10000) in
															if re = 0 then List.rev acc else List.rev ((d2 + 1, r)::(d2, re)::acc)
														else List.rev ((d2, n)::acc)
													 else 
													 	let n = r + c2 in let nb = (nbr_chiffre n) in
													 	if nb > 4 then let r = (get_left_number n) in let re = (n mod 10000) in 
															if re = 0 then (aux [] q acc r) else (aux [] q ((d2, re)::acc) r)
													 	else List.append (List.rev ((d2, n)::acc)) q
		| (d1, c1) :: q, [] -> if (List.length q) = 0 then 
														let n = r + c1 in let nb = (nbr_chiffre n) in
													 	if nb > 4 then let r = (get_left_number n) in let re = (n mod 10000) in
															if re = 0 then List.rev acc else List.rev ((d1 + 1, r)::(d1, re)::acc)
														else List.rev ((d1, n)::acc)
													 else 
														let n = r + c1 in let nb = (nbr_chiffre n) in
													 	if nb > 4 then let r = (get_left_number n) in let re = (n mod 10000) in 
															if re = 0 then (aux q [] acc r) else (aux q [] ((d1, re)::acc) r)
													 	else List.append (List.rev ((d1, n)::acc)) q
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 < d2 -> let n = r + c1 in let nb = (nbr_chiffre n) in
																											if nb > 4 then let r = (get_left_number n) in let re = (n mod 10000) in 
																												if re = 0 then (aux q1 ((d2, c2)::q2) acc r) else (aux q1 ((d2, c2)::q2)  ((d1, re)::acc) r)
																											else let r = 0 in aux q1 ((d2, c2)::q2)  ((d1, n)::acc) r
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 > d2 -> let n = r + c2 in let nb = (nbr_chiffre n) in
																											if nb > 4 then let r = (get_left_number n) in let re = (n mod 10000) in 
																												if re = 0 then aux ((d1, c1) :: q1) q2 acc r else aux ((d1, c1) :: q1) q2  ((d1, re)::acc) r
																											else let r = 0 in aux ((d1, c1) :: q1) q2  ((d1, n)::acc) r
		| (d1, c1) :: q1, (d2, c2) :: q2 -> let n = r + c1 + c2 in let nb = (nbr_chiffre n) in 
																				if (nb > 4) then let r = (get_left_number n) in let re = (n mod 10000) in
																					if re = 0 then aux q1 q2 ((d1 + 1, r) :: acc) r else aux q1 q2 ((d2 + 1, r)::(d2,re)::acc) r 
																				else let r = 0 in aux q1 q2 ((d2, n)::acc) r
	in aux l1 l2 [] 0;; 

let add_gdnb_abs (l1:(int*int) list) (l2:(int*int) list) = 
	let div = 10000 in
	let rec aux l1 l2 acc r = 
		match l1, l2 with
		| [], [] -> List.rev acc
		| [], (d2, c2) :: q -> if (List.length q) = 0 then 
														let n = r + c2 in
													 	if n >= div then let r = (n / div) in let re = (n mod div) in
															if re = 0 then (List.rev) acc else List.rev ((d2 + 1, r) :: (d2, re) :: acc)
														else List.rev ((d2, n)::acc)
													 else 
													 	let n = r + c2 in
													 	if n >= div then let r = n / div in let re = n mod div in 
															if re = 0 then (aux [] q acc r) else (aux [] q ((d2, re)::acc) r)
													 	else List.append (List.rev ((d2, n)::acc)) q
		| (d1, c1) :: q, [] -> if (List.length q) = 0 then 
														let n = r + c1  in
													 	if n >= div then let r = n / div in let re = (n mod div) in
															if re = 0 then List.rev acc else List.rev ((d1 + 1, r)::(d1, re)::acc)
														else List.rev ((d1, n)::acc)
													 else 
														let n = r + c1 in
													 	if n >= div then let r = (n / div) in let re = (n mod div) in 
															if re = 0 then (aux q [] acc r) else (aux q [] ((d1, re)::acc) r)
													 	else List.append (List.rev ((d1, n)::acc)) q
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 < d2 -> let n = r + c1 in 
																											if n >= div then let r = n / div in let re = (n mod div) in 
																												if re = 0 then (aux q1 ((d2, c2)::q2) acc r) else (aux q1 ((d2, c2)::q2)  ((d1, re)::acc) r)
																											else let r = 0 in aux q1 ((d2, c2)::q2)  ((d1, n)::acc) r
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 > d2 -> let n = r + c2 in
																											if n >= div then let r = n / div in let re = (n mod div) in 
																												if re = 0 then aux ((d1, c1) :: q1) q2 acc r else aux ((d1, c1) :: q1) q2  ((d1, re)::acc) r
																											else let r = 0 in aux ((d1, c1) :: q1) q2  ((d1, n)::acc) r
		| (d0, c0) :: (d1, c1) :: q1, (d2, c2) :: (d3, c3) :: q2 -> let n = r + c0 + c2 in 
																				if (n >= div) then let r = (n / div) in let re = (n mod div) in
																					if (d0 + 1 = d1 && d1 = d3) || (d0 + 1 = d1 && d1 <> d3) then 
																						if re = 0 then aux ((d1, c1 + r)::q1) ((d3, c3)::q2) acc 0  else aux ((d1, c1 + r)::q1) ((d3, c3)::q2) ((d0, re)::acc) 0
																					else
																						if re = 0 then aux ((d1, c1)::q1) ((d3, c3 + r)::q2) acc 0  else aux ((d1, c1 + r)::q1) ((d3, c3)::q2) ((d0, re)::acc) 0
																				else let r = 0 in aux ((d1, c1)::q1) ((d3, c3)::q2) ((d0, n)::acc) r
		|(d0, c0) :: (d1, c1) :: q1, (d2, c2) :: q2 -> let n = r + c0 + c2 in
																				if (n > div) then let r = n / div in let re = (n mod div) in
																					if re = 0 then if d0 + 1 = d1 then aux ((d1,c1 + r)::q1) q2 acc 0 else aux ((d1,c1)::q1) q2 ((d0 + 1, r) :: acc) 0
																					else 
																						if d0 + 1 = d1 then aux ((d1,c1 + r)::q1) q2 ((d0,re)::acc) 0 else aux ((d1,c1)::q1) q2 ((d0 + 1, r)::(d0,re)::acc) 0 
																				else let r = 0 in aux ((d1, c1)::q1) q2 ((d0, n)::acc) r
		| (d1, c1) :: q1, (d2, c2) :: (d3, c3) :: q2 -> let n = r + c1 + c2 in
																				if (n > div) then let r = n / div in let re = (n mod div) in
																					if re = 0 then if d2 + 1 = d3 then aux q1 ((d3,c3 + r)::q2) acc 0 else aux q1 ((d3,c3)::q2) ((d2 + 1, r) :: acc) 0
																					else 
																						if d2 + 1 = d3 then aux q1 ((d3,c3 + r)::q2) ((d2,re)::acc) 0 else aux q1 ((d3,c3)::q2) ((d2 + 1, r)::(d2,re)::acc) 0
																				else let r = 0 in aux ((d1, c1)::q1) q2 ((d2, n)::acc) r
		| (d1, c1) :: q1, (d2, c2) :: q2 -> let n = r + c1 + c2 in
																				if (n > div) then let r = n / div in let re = (n mod div) in
																					if re = 0 then aux q1 q2 ((d1 + 1, r) :: acc) r else aux q1 q2 ((d2 + 1, r)::(d2,re)::acc) 0 
																				else let r = 0 in aux q1 q2 ((d2, n)::acc) r
	in aux l1 l2 [] 0;; 

let addition_gdnb l1 l2 = 
	if l1.signe = true && l2.signe = true then {signe = l1.signe; abs = add_gdnb_abs l1.abs l2.abs}
	else let c = compare_gdnb_abs l1.abs l2.abs in
		if l1.signe = false && l2.signe = false then
			{signe = false; abs = add_gdnb_abs l1.abs l2.abs}
		else 
			if c = 0 then {signe = false; abs = []}
			else if c = 1 then {signe = l1.signe; abs = sub_abs_gdnb l1.abs l2.abs}
			else {signe = l2.signe; abs = sub_abs_gdnb l2.abs l1.abs};;

let oppose_gdnb g = {signe = not g.signe; abs=g.abs};;

let g3=string_to_gdnb "22407312490701";;
let g4=string_to_gdnb "999602519048";;
addition_gdnb g3 g4;;
addition_gdnb (oppose_gdnb g3) g4 ;;

(*Renvoie le nombre compose de chiffres : 1 et d1 - d2 fois 4 zeros*)
let get_one_and_zeros_between d1 d2 = 
	let rec aux d1 d2 acc = 
		if d1 = d2 then int_of_string ("1"^acc)
		else aux (d1-1) d2 ("0000"^acc)
	in aux d1 d2 "";;

(*Renvoie une liste de couple de degre commencant par d1 et de nombre*)
let abs_of_String s d1 = 
		let rec aux i acc k =
			if (k <= 1) then List.rev acc
			else
				let sub = String.sub s (k - 4) 4 in let num = int_of_string sub in
					if num <> 0 then aux (i + 1) ((i, num) :: acc) (k - 4) else aux (i + 1) acc (k - 4)
		in aux d1 [] (String.length s);;

(*Parcourt l1 l2 si les degres sont # on compte le nombre de zero entre les deux degres avec un 1 au debut et on fait *)
(* la difference entre les coefficient ces 0000 puis on obtient une liste du resultats dont le premier element constitue *)
(* le resultat de la difference et le reste de la liste est rajute a la liste pour les prochaines soustractions*)
(* Si d1 = d2, on consulte les coefficient des degres et on effectue les calculs qu'il faut.*)
let sub_abs_gdnb l1 l2 = 
	let rec aux l1 l2 acc = 
		match l1, l2 with
		| [], [] -> List.rev acc
		| q, [] -> List.append (List.rev acc) q
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 < d2 -> aux q1 ((d2, c2) :: q2) ((d1, c1) :: acc)
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 > d2 -> let d = d1 - d2 and m = (get_one_and_zeros_between d1 d2) in 
																										 if d = 1 then aux ((d1, c1 - 1) :: q1) q2 ((d2, m - c2)::acc)
																										 else let di = m - c2 in let li = abs_of_String (string_of_int di) d2 
																										   in aux (List.append (List.tl li) ((d1, c1 - 1) :: q1)) q2 ((List.hd li) :: acc)
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 = d2 && c1 > c2 -> aux q1 q2 ((d2, c1 - c2)::acc)
		| (d0, c0) :: (d1, c1) :: q1, (d2, c2) :: q2 when d0 = d2 && c0 < c2 -> let d = d1 - d0 and m = (get_one_and_zeros_between d1 d0) 
																																							in if (d = 1) then aux ((d1, c1 - 1) :: q1) q2 ((d2, (m + c0) - c2)::acc)
																																							   else let di = m - c2 in let li = abs_of_String (string_of_int di) d0 
																										   													 in aux (List.append (List.tl li) ((d1, c1 - 1) :: q1)) q2 ((List.hd li) :: acc)
		| (d1, c1) :: q1, (d2, c2) :: q2 -> aux q1 q2  acc
		| [], _::_ ->  failwith "Impossible de faire cette operation!"
	in aux l1 l2 [];;

let soustraction_gdnb l1 l2 = let c = compare_gdnb_abs l1.abs l2.abs in
	if l1.signe = false && l2.signe = false then
		if c = 0 then {signe = false; abs = []}
		else if c = 1 then {signe = l1.signe; abs = sub_abs_gdnb l1.abs l2.abs}
		else {signe = true; abs = sub_abs_gdnb l2.abs l1.abs}
	else if l1.signe = true && l2.signe = true then
		if c = 0 then {signe = false; abs = []}
		else if c = 1 then {signe = l1.signe; abs = sub_abs_gdnb l1.abs l2.abs}
		else {signe = false; abs = sub_abs_gdnb l2.abs l1.abs}
	else if l1.signe = false && l2.signe = true then {signe = false; abs = add_gdnb_abs l1.abs l2.abs}
	else {signe = true; abs = add_gdnb_abs l1.abs l2.abs};;

let g1=string_to_gdnb "22407312490701";;
let g2=string_to_gdnb "-999602519048";;
soustraction_gdnb g1 g2;;

(*exo4*)
let multCoeff2 (p:(int*int) list) a = 
	let div = 10000  in
	let rec aux p =
		match p with
			| [] -> []
			| (d, c) :: q -> let n = a * c in 
											 if n < div then (d, n) :: aux q
											 else add_gdnb_abs [(d, n mod div);(d + 1, n / div)] (aux q)
	in aux p;;

let multCoeff p a = 
	let rec aux acc a =
		match a with
			| 0 -> acc
			| n -> aux (addition_gdnb acc p) (n - 1)
	in aux {signe=p.signe; abs=[]} a;;

let degre p = let n = (List.length p.abs) in if n = 0 then 0 else fst (List.nth p.abs (n - 1));;

let multXn p n =
	let a = p.abs in
	 let rec aux a acc = 
		 match a with
			| [] -> {signe=acc.signe; abs = List.rev acc.abs}
			| (d1, c1)::q -> aux q {signe = acc.signe; abs=((d1 + n, c1)::(acc.abs))}
	 in aux a {signe=p.signe; abs=[]};;
	
let cut p n = 
	let a = p.abs in
	let rec aux a acc1 acc2 = 
		match a with
			| [] ->  {signe=acc1.signe; abs = List.rev acc1.abs}, {signe=acc2.signe; abs = List.rev acc2.abs}
			| (d, c) :: q when d < n -> aux q {signe=acc1.signe;abs = ((d, c) :: acc1.abs)} acc2
			| (d, c) :: q -> aux q acc1 {signe=acc2.signe;abs=((d - n, c) :: acc2.abs)}
	in aux a {signe=p.signe; abs=[]} {signe=p.signe; abs=[]};;

let rec multNaive p1 p2 =
	if (degre p1 > 1 || degre p2 > 1) then failwith "Impossible de faire la multiplication naive!"
	else
		let a = p1.abs in
			let rec aux a acc =
				match a with
					| []-> acc
					| (d, c) :: q -> addition_gdnb (aux q (multCoeff (multXn p2 d) c)) acc
			in if p1.signe = p2.signe then aux a {signe=true; abs=[]} else aux a {signe=false; abs=[]};;

let rec karatsuba p1 p2 =
	if degre p1 < 2 && degre p2 < 2 then multNaive p1 p2
	else
			let i = (1 + max (degre p1) (degre p2))/2 in 
			let a0, a1 = cut p1 i in let b0, b1 = cut p2 i in
			let c0 = karatsuba a0 b0 in let c2 = karatsuba a1 b1 in
			let u = karatsuba (addition_gdnb a0 a1) (addition_gdnb b0 b1) in 
			let c1 = soustraction_gdnb u (addition_gdnb c0 c2) in 
			addition_gdnb (addition_gdnb c0 (multXn c1 i)) (multXn c2 (i * 2));;
		