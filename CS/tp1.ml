(*Exercice1*)
(*let x = ref [(0, 0)];;
print_int (fst (List.hd !x));;
x := !x + 5;;
print_int !x;;*)
(*exo1*)
type poly = (int * float) list;;
let a = [(0, 1.);(1, -1.); (2, 1.); (3, 1.); (4, 2.)];;
let p1 = [(0, 1.);(1, 2.); (2, 1.); (3, 1.)];;
let p2 = [(0,2.);(3, 3.); (5, -1.)];;
let rec calc_coeff i (l:poly) = match l with
	| [] -> 0.
	| (x, y) :: q when x > i -> 0.
	| (x, y) :: q when x = i -> y
	| (_, _) :: q -> calc_coeff i q;;

(*exo2*)
let somme_poly (p1:poly) (p2:poly) : poly = 
	let rec aux p1 p2 acc = match p1, p2 with
		| [], [] -> List.rev acc
		| [], q -> List.append (List.rev acc) q 
		| q, [] -> List.append (List.rev acc) q
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 < d2 -> aux q1 ((d2, c2)::q2)  ((d1, c1)::acc)
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 > d2 -> aux ((d1, c1) :: q1) q2  ((d2, c2)::acc)
		| (d1, c1) :: q1, (d2, c2) :: q2 when d1 = d2 -> if (c1 +. c2) <> 0. then aux q1 q2  ((d2, c1 +. c2)::acc) else aux q1 q2  acc
		| (_::_, _::_) -> failwith "Impossible de calculer la somme"
	in aux p1 p2 [];;

let soustr_poly (p1:poly) (p2:poly) : poly =
	somme_poly p1 (multCoeff p2 (-1.));;

(*exo3*)
let multCoeff (p:poly) a : poly = 
	let rec aux p acc =
		match p with
			| [] -> List.rev acc
			| (d, c) :: q -> aux q ((d, a *. c) :: acc)
	in aux p [];;

let rec degre (p: poly) = 
	match p with
		| [] -> 0
		| (d1, c1)::[]-> d1
		| (_, _) :: q-> degre q;;

let multXn (p:poly) n:poly =
	let rec aux p acc = 
		match p with
			| [] -> List.rev acc
			| (d1, c1)::q -> aux q ((d1 + n, c1)::acc)
	in aux p [];;
	
let cut (p:poly) n : poly * poly = 
	let rec aux p acc1 acc2 = 
		match p with
			| [] ->  List.rev acc1, List.rev acc2
			| (d, c) :: q when d < n -> aux q ((d, c) :: acc1) acc2
			| (d, c) :: q -> aux q acc1 ((d - n, c) :: acc2)
	in aux p [] [];;

let multNaiv (p1:poly) (p2:poly) =
	let rec aux p1 p2 acc =
		match p1 with
			| []-> List.rev acc
			| (d, c) :: q ->  somme_poly (List.rev(aux q p2 (multCoeff (multXn p2 d) c))) acc
	in aux p1 p2 [];;

let rec multNaive (p1:poly) (p2:poly) =
	let rec aux p1 acc =
		match p1 with
			| []-> acc
			| (d, c) :: q ->  somme_poly (aux q (multCoeff (multXn p2 d) c)) acc
	in aux p1 [];;

let rec karatsuba (p1:poly) (p2:poly) =
	if degre p1 < 2 && degre p2 < 2 then multNaive p1 p2
	else
			let i = (1 + max (degre p1) (degre p2))/2 in 
			let a0, a1 = cut p1 i in let b0, b1 = cut p2 i in
			let c0 = karatsuba a0 b0 in let c2 = karatsuba a1 b1 in
			let u = karatsuba (somme_poly a0 a1) (somme_poly b0 b1) in 
			let c1 = soustr_poly u (somme_poly c0 c2) in 
			somme_poly (somme_poly c0 (multXn c1 i)) (multXn c2 (i * 2));;
	
	(*exo4*)
let renv (p:poly):poly = 
		let rec aux p acc =
			match p with
			| [] -> acc
			| (d, c) :: q -> aux q ((-d, c) :: acc)
		in aux p [];;

let renverse k (p:poly) = 
	if k >= degre p then
		multXn (renv p) k
	else failwith "Impossible de calculer le renverse de p";;

let moduloXn (p:poly) n = 
	if n > degre p then p
		else fst (cut p n);;

let inverse_mod (p:poly) m = 
	let rec aux (acc:poly) acc2 = 
		if acc2 > m && ((degre acc) = (degre p)) then acc
		else 
			aux (moduloXn (soustr_poly (multCoeff acc 2.) (multNaive p (multNaive acc acc))) acc2) (2 * acc2)
	in aux [(0, 1.)] 1;;
	
let inverse_mod2 (p:poly) m = 
	let rec aux (acc:poly) acc2 = 
		if acc2 > m && ((degre acc) = (degre p)) then acc
		else 
			aux (moduloXn (soustr_poly (multCoeff acc 2.) (karatsuba p (karatsuba acc acc))) acc2) (2 * acc2)
	in aux [(0, 1.)] 1;;

let rec get_coeff_degre_poly (p: poly) = 
	match p with
		| [] -> 0.
		| (d1, c1) :: []-> c1
		| (_, _) :: q-> get_coeff_degre_poly q;;

let get_coeff_degre_poly2 (p: poly) = 
	if (List.length p = 0) then 0.
	else snd (List.hd (List.rev p));;

let inverse_mod_polys (p1:poly) (p2:poly) = 
	let bn = 1. /. (get_coeff_degre_poly p2) in
	let p = multCoeff (renverse (degre p2) p2) bn in
	let rec aux (acc:poly) acc2 = 
		if acc2 > (degre p1) - (degre p2) + 1 then acc
		else 
			aux (moduloXn (soustr_poly (multCoeff acc 2.) (karatsuba p (karatsuba acc acc))) acc2) (2 * acc2)
	in aux [(0, 1.)] 1;;

let quotient_poly (p1:poly) (p2:poly) = 
	let bn = 1. /. (get_coeff_degre_poly p2) in
	let g = inverse_mod_polys p1 p2 in let m = degre p1 and n = degre p2 in
	renverse (m - n) (moduloXn (karatsuba (renverse m p1) (multCoeff g bn)) (m - n + 1));;

let quotient_poly2 (p3:poly) (p4:poly) =
	let g = inverse_mod p3 (degre p4) in let m = degre p3 in let n = degre p4 in
	renverse (m - n) (moduloXn (karatsuba (renverse m p3) (multCoeff g 1.)) (m - n + 1));;

(*exo 5*)
let eval_poly (p:poly) x = 
	let rec aux p acc =
		match p with 
			| [] -> acc
			| (d, c)::q  -> aux q (c +. x *. acc)
	in aux p (get_coeff_degre_poly p);;

let rec get_coeff (p:poly) i =
	 match p with
	| [] -> 0.
	| (x, y) :: [] when x < i -> 0.
	| (x, y) :: q when x > i -> 0.
	| (x, y) :: q when x = i -> y
	| (_, _) :: q -> get_coeff q i;;

let eval_poly2 (p:poly) x = 
	let e = ref (get_coeff_degre_poly p) in
	for i = 0 to (degre p) do (
		e := (get_coeff p i) +. x *. !e
	)
	done;
	!e;;

let eval_poly3 (p:poly) x = 
	let e = ref 0. in
	for i = 0 to (degre p) do (
		let c = ref (get_coeff p i) in
		for j = 1 to i do (
			c := x *. !c
		)
		done;
		e := !c +. !e
	)
	done;
	!e;;

(*Exercice2*)
(*1eval_poly (renverse (degre p1) p1) 0.;;*)

	
