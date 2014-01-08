(* USAGE:

      ocamlc myStream.ml

      ledit ocaml dynlink.cma camlp4o.cma myStream.cmo

      #use "parser.ml";;
 *)


(*
#load "dynlink.cma"
#load "camlp4o.cma"
#load "myStream.cmo"
#load "tokenizer.cmo"
*)
open MyStream
open Tokenizer

type frequency = int*string

    (* Gere les bonus accordés aux balises*)
let fctValue : (string -> int) = fun balise ->
  match balise with
	|"title" -> 100
	|"h1" -> 70
	|"h2" -> 50
	|"h3" -> 30
	|"b" -> 10
	|"i" -> 5
	|_ -> 0

(* Convertie un string en une char list *)
let string_to_list : string -> char list = fun s ->
let rec string_to_list_aux : string -> char list -> int -> int -> char list= fun s l a aa->
  if a = 0 then l
  else (String.get s (aa-a)) :: (string_to_list_aux s l (a-1) aa)
in string_to_list_aux s [] (String.length s) (String.length s)

(* Convertie une char list en un string*)
let (char_list_to_string: char list -> string) = fun charS ->
      let string = String.create (List.length charS) in 
	begin
	  iter_counter 0 (fun i c -> String.set string i c) charS ;
	  string 
	end

(* Gere la suppression des pluriels : aux -> al, oux -> ou, ees -> e, es -> e*)
let desaccords : string -> string -> (bool*string) = fun m1 m2 ->
  let diff = (String.length m2) - (String.length m1) in
  let mot1 = string_to_list (if diff < 0 then m2 else m1) in
  let mot2 = string_to_list (if diff < 0 then m1 else m2) in

  let rec desaccords_aux: char list -> char list -> char list -> (bool*char list) = fun mot1 mot2 acc ->
  
  if abs diff <= 3 then
    match (mot1,mot2) with
      |([],[]) -> (true,acc)
      |([],m) when m = ['e'] or m=['s'] or m = ['e';'s'] -> (true,acc)
      |(a::n,b::m) when (a = 'l' && b = 'u' && (n=[] or n=['s']) && m = ['x']) -> (true,acc@['l'])
      |['o';'u'], ['o';'u';'x'] -> (true,acc@['o';'u'])
      |['s'],['e';'s'] -> (true,acc)
      |(a::n,b::m) when a = b -> (desaccords_aux n m (acc@[a]))
      |[('s'|'e')], [('s'|'e')] -> (true,acc)
      |_ -> (false,[])
  else
    (false,[]) in
  let (b,s)=desaccords_aux mot1 mot2 [] in (b,char_list_to_string s)

(* Ajoute un element (de type frequency) a une frequency list, avec modification du mot so pluriel existe deja*)
let rec ajout: frequency -> frequency list -> frequency list = fun couple list ->  let (n,w) = couple in
  match list with
    |[] -> [(n,w)]
    |(n1,w1)::tail -> let  (b,s)=desaccords w1 w in if b then (n1+n,s)::tail else (n1,w1)::(ajout couple tail) 

(* Idem que ajout, sans gestion des accords, et pluriels*)
let rec ajout_with_occ: frequency -> frequency list -> frequency list = fun couple list ->  let (n,w) = couple in
  match list with
    |[] -> [(n,w)]
    |(n1,w1)::tail -> if w=w1 then (n1+n,w)::tail else (n1,w1)::(ajout_with_occ couple tail) 

(* Liste a partir d'un fichier Prepositions.txt, l'ensemble des mots interdits*)
let prepositions = ["''";"``";" "]@ (List.map (fun (Word x) -> x) (MyStream.to_list (snd (Tokenizer.tokenize_file "test/Prepositions"))))

(* 
                                 Grammaire 
(* Variables *)
bonus : int, 
L : frequency list, 
_ : Symbol of string
    | Integer of int
    | Mark of string
    | String of string
    | Ident of string
    | Comment 
    | Space

(* Grammaire *)
{bonus} Html {L} -> Open x . {bonus + fctVlaue x} Html {L'} ; {L:=L'}
		  | Close x . {bonus - fctVlaue x} Html {L'} ; {L:=L'}
		  | Word x . {bonus} Html {L'} ; //Calculs: if x (not €) prepositions then L := ajout (x,bonus+1) L' else L:= L'
		  | _ . {bonus} Html {L'} ; {L:=L'}
		  | Epsilon ; {L:=[]}

*)
let rec html: Tokenizer.token Stream.t -> int -> (frequency -> frequency list -> frequency list) -> frequency list = fun stream accPoint ajout ->
  match stream with parser
    |[< '(Open o); s >] -> html s (accPoint+fctValue o) ajout 
    |[< '(Close o); s >] -> html s (accPoint-fctValue o) ajout
    |[< '(Word w); s >] -> let wl = String.lowercase w in if not( List.exists (fun x -> wl = x) prepositions) then (ajout (accPoint+1,wl) (html s accPoint ajout)) else html s accPoint ajout
    |[< 'x; s>] -> html s accPoint ajout
    |[<>] -> []



(* Index une page html *)      
let (indexer: string -> frequency list) = fun filename ->
   let (_,token_stream) = Tokenizer.tokenize_file filename in 
   List.sort (fun x y -> if x=y then 0 else if x<y then 1 else -1) (html token_stream 0 ajout)

	
(* Renvoie les 20 premiers mots de plus haute fréquence *)      
let top_vingt: string -> frequency list = fun filename ->
  let frequency_list=indexer filename and resultat=ref [] in
  for i=0 to 20-1 do
    resultat:=!resultat @ [(List.nth frequency_list i)];
  done;  !resultat

(* Renvoie la liste des mots indexes et la liste des mots modifies*)
let main : string -> (frequency list*frequency list) = fun s-> 
  let (indexer_occ: string -> frequency list) = fun filename ->
  let (_,token_stream) = Tokenizer.tokenize_file filename in 
   List.sort (fun x y -> if x=y then 0 else if x<y then 1 else -1) (html token_stream 0 ajout_with_occ) in

  let rec modified_words_aux gt lt =
     match gt with
  |[] -> []
  |(n,s)::tail -> if not (List.exists (fun (nb,string) -> string=s) lt) then (n,s)::modified_words_aux tail lt else modified_words_aux tail lt
  in
let freq=(indexer s) in (freq,modified_words_aux (indexer_occ s) freq)

let _ = indexer "test/page1.html"
let _ = main "test/page1.html"
let _ = top_vingt "test/page1.html" 

  
(*

(* TEST *)


MyStream.to_list (snd (Tokenizer.tokenize_file "test/page1.html")) ;;
Tokenizer.print_tokenize_file "test/page1.html" ;; 
indexer "test/page1.html" ;;


MyStream.to_list (snd (Tokenizer.tokenize_file "test/page2.html")) ;; 
Tokenizer.print_tokenize_file "test/page2.html" ;; 
indexer "test/page2.html" ;;

MyStream.to_list (snd (Tokenizer.tokenize_file "test/page3.html")) ;; 
Tokenizer.print_tokenize_file "test/page3.html" ;; 
indexer "test/page3.html" ;;

MyStream.to_list (snd (Tokenizer.tokenize_file "test/page4.html")) ;; 
Tokenizer.print_tokenize_file "test/page4.html" ;; 
indexer "test/page4.html" ;;

MyStream.to_list (snd (Tokenizer.tokenize_file "test/page5.html")) ;; 
Tokenizer.print_tokenize_file "test/page5.html" ;; 
indexer "test/page5.html" ;;

*)
