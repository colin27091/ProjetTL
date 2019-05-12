(* Projet Caml Automates déterministes*)



(*****************************************************************************)
(*				Bibliothèque sur les chaînes de caractères 					 *)

(* Fonctions usuelles sur les chaînes de caractères *)
let string_of_char = String.make 1 ;;

let tetec = function
| "" -> failwith "Erreur : chaine vide"
| s -> s.[0] ;;
(*val tetec : string -> char = <fun>*)

let tetes = fun s -> string_of_char (tetec(s));;

let reste = function 
| "" -> failwith "Erreur : chaine vide"
| s -> String.sub s 1  ((String.length s) - 1 ) ;;
(*val reste : string -> string = <fun>*)


(**********************************************************************************)	
		
	
(**********************************************************************************)	
(*                      Bibliothèque sur les listes                             *)  

let rec appartient = function 
(a,b::l)-> if a=b then true else appartient(a,l)
|(_,[])-> false;;
(*appartient : 'a * 'a list -> bool = <fun>*)

let rec union l = function 
(a::l2)-> if appartient(a,l) then union l l2 else a:: (union l l2)
| []->l;;
(*union : 'a list -> 'a list -> 'a list = <fun>*)

let rec enleve a = function
 x::q -> if x = a then q else x::(enleve a q)
 | [] -> [] ;;
 (* val enleve : 'a -> 'a list -> 'a list = <fun> *)

let rec intersection l1 = function
	| [] -> []
	| a :: l2 -> if appartient(a,l1) then a::(intersection (enleve a l1) l2) else intersection l1 l2 ;;
(* val intersection : 'a list -> 'a list -> 'a list = <fun> *)
	
let rec long = function
(_::l)->1+long(l)
|[]-> 0;;
(* val long : 'a list -> int = <fun>*)
(**********************************************************************************)

(* Automates déterministes *)

type etat = {accept : bool ; t : char -> int} ;;		
(* type etat = { accept : bool; t : char -> int; } *)


type afd = {sigma : char list ;nQ : int ;init : int ;e : int -> etat} ;;
(* type afd = { sigma : char list; nQ : int; init : int; e : int -> etat; } *)

let a1 = {sigma= ['a';'b'] ; nQ = 3; init = 1 ; 
			e = function	
			    1 -> {accept = false ;
				      t = function 
					       'a'->2
						   |'b'-> 1  }
				|2 -> {accept = false ;
				      t = function 
					       'a'->2
						   |'b'-> 3  }		   
				|3 -> {accept = true ;
				      t = function 
					       'a'->3
						   |'b'->3   }		   				
		};;
(* val a1 : afd = {sigma = ['a'; 'b']; nQ = 3; init = 1; e = <fun>} *)
		

(* Fonctionnement avec recherche sur 'abb' dans une instance *)

let abb = {sigma = ['a'; 'b'; 'c']; nQ = 4 (*longueur w +1*); init = 1;
			e = function
				1 -> {accept = false;
						t = function
							'a' -> 2
							|'b' -> 1
							|'c' -> 1 }
				|2 -> {accept = false;
						t = function
							'a' -> 2
							|'b' -> 3
							|'c' -> 1 }
				|3 -> {accept = false;
						t = function
							'a' -> 2
							|'b' -> 4
							|'c' -> 1 }
				|4 -> {accept = true;
						t = function
							_ -> 4}
		};;
(* val abb : afd = {sigma = ['a'; 'b'; 'c']; nQ = 4; init = 1; e = <fun>} *)

(* Lecture AFD*)

let transit (a : afd) (i :int) (c : char) = (a.e i).t c;;
(* val transit : afd -> int -> char -> int = <fun> *)

let rec lire (a : afd) (i :int) (str : string) = match str with
"" -> (a.e i).accept
|str -> (a.e i).accept || lire a (transit a i (tetec(str))) (reste(str));;
(* val lire : afd -> int -> string -> bool = <fun> *)

let accept (a : afd) (str : string) = lire a a.init str;;
(* val lecture : afd -> string -> bool = <fun> *)


(* Creation automate (depart w) *)


let t_empty = function _ -> failwith "Erreur dans le texte";;
(* val t_empty : char -> 'a = <fun> *)

let e_empty = function _ -> failwith "Etat inconnu";;
(* val e_empty : int -> etat = <fun> *)

let chooseEtat (car : char) (w : string) (etat : int) = match car with
c when (c = w.[0]) -> if c != w.[etat-1] 
						then 2
						else etat + 1
|c -> if c = w.[etat-1]
	then etat + 1
	else 1;;
(* val chooseEtat : char -> string -> int -> int = <fun> *)

let ajoute_transition (t_function : char -> int) (w : string) (c : char) (etat : int) = 
	let (t_function2 : char -> int) = function
		y -> if y = c
			then chooseEtat (c) (w) (etat)
			else t_function y
		in t_function2;;
(* val ajoute_transition : (char -> int) -> string -> char -> int -> char -> int = <fun> *)

let rec t_automate (t_function : char -> int) (etat : int) (alphabet : char list) (w : string) = match alphabet with
[] -> t_function
|(a::l) -> t_automate (ajoute_transition (t_function) (w) a etat) etat l w;;
(* val t_automate : (char -> int) -> int -> char list -> string -> char -> int = <fun> *)

let ajoute_etat (e_function : int -> etat) (alphabet : char list) (w : string) (num : int) =
	let (e_function2 : int -> etat) = function
		y -> if y = num
			then if y = ((String.length w)+1)
				then {accept = true; t = function _ -> y}
				else {accept = false; t = t_automate (t_empty) (num) (alphabet) (w)}
			else e_function y
		in e_function2;;
(* val ajoute_etat : (int -> etat) -> char list -> string -> int -> int -> etat = <fun> *)

let rec e_automate (e_function : int -> etat)(alphabet : char list)(w : string)(len : int) = match len+1 with
0 -> e_function
| i -> e_automate (ajoute_etat (e_function) (alphabet) (w) (i)) (alphabet) (w) (len-1);;
(* val e_automate : (int -> etat) -> char list -> string -> int -> int -> etat = <fun> *)


let create_automate (alphabet : char list) (w: string) = {sigma = alphabet; nQ = (String.length w)+1; init = 1;
		e = e_automate (e_empty) (alphabet) (w) (String.length w)};;
(* val create_automate : char list -> string -> afd = <fun> *)



(* TEST *)

(* Données ADN  *)
let nomFic = "C:/Users/c/Documents/L3Info/TheorieLang/ProjetTL/adn.txt";; (* 500 000 caractères *)

(* Extraction donnée *)
let str = input_line (open_in nomFic);;
(*fin extraction*)

let alph = ['A';'C';'G';'T'];;

let chaine = ["TGTACACATATTGACCAAATCAGGGTAATT";(*fin de str*)
				"CCTGGACAACCTCAAGGGCACCTTTGCCACA";(*millieu str*)
				"CCTGGACAACCTCAAGGGCACCTTTGCCACB";(*caractères inconnu*)
				"CCTGGACAACCTCAACCTGGACAACCTCAA";(*repetition*)
				"";(*w null*)
				"AACCTCAA";(*debut même lettre*)
				"CAACTCCTAAGCCAGTGCCA";(*debut de str*)
				"TGTACACATATTGACCAAATCAGGGTAATTAAA"];; (*non présent*)

;; (*dernière chaine du document*)

let test (str : string) (alph : char list) (w : string) =
	let start = Sys.time() in
		let acc = accept (create_automate alph w) str in
			let finish = Sys.time() in
				(w, acc, finish -. start);;
(* val test : string -> char list -> string -> bool * float = <fun> *)

let rec test_tab (str : string) (alph : char list) (w : string list) = match w with
[] -> []
|(a::l) -> (test str alph a) :: test_tab str alph l;;
(* val test_tab : string -> char list -> string list -> (bool * float) list = <fun> *)

(* test_tab str alph chaine;; *)


