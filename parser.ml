(* USAGE:

      ocamlc myStream.ml

      ledit ocaml dynlink.cma camlp4o.cma myStream.cmo

      #use "parser.ml";;
 *)



#load "dynlink.cma"
#load "camlp4o.cma"
#load "myStream.cmo"
#load "tokenizer.cmo"
open MyStream
open Tokenizer

type frequency = int*string

let rec (occurence: token list -> token -> int) = fun token_list token -> 
  match token_list with
    |[]->0
    |hd::tail -> if hd=token then 1 + occurence tail token else occurence tail token


let (indexer: string -> frequency list) = fun filename ->
  let rec indexer_aux : token list -> frequency list = fun token_list ->
  match token_list with
    |[] -> []
    |Word s ::tail -> let i=occurence token_list (Word s) and suite=List.filter (fun x -> x<>Word s) tail in (i,s)::indexer_aux suite
    |_::tail -> indexer_aux tail

in    let (_,token_stream) = Tokenizer.tokenize_file filename in 
      let token_list= MyStream.to_list (token_stream) in
      List.sort (fun x y -> if x=y then 0 else if x<y then 1 else -1) (indexer_aux token_list)
;;


let top_vingt: string -> frequency list = fun filename ->
  let frequency_list=indexer filename and resultat=ref [] in
  for i=0 to 20-1 do
    resultat:=!resultat @ [(List.nth frequency_list i)];
  done;  !resultat
  
let rec filter_prepositions : frequency list -> frequency list = fun frequency_list -> match frequency_list with
  |[] -> []
  |(i,m)::b -> if not (List.mem m (prepositions)) then  (i,m)::filter_prepositions b else filter_prepositions b

;;
let prepositions = List.map (fun (Word x) -> x) [Word "a"; Word "devers"; Word "pendant"; Word "apres"; Word "dixit"; Word "pour"; Word "attendu"; Word "durant"; Word "pres"; Word "au-dedans";Word "emmi"; Word "proto-"; Word "au-dehors"; Word "en"; Word "quant";
 Word "au-dela"; Word "endeans"; Word "revoici"; Word "au-dessous";
 Word "entre"; Word "revoila"; Word "au-dessus"; Word "envers"; Word "rez";
 Word "au-devant"; Word "es"; Word "sans"; Word "aupres"; Word "excepte";
 Word "sauf"; Word "autour"; Word "fors"; Word "selon"; Word "avant";
 Word "hormis"; Word "sous"; Word "avec"; Word "hors"; Word "sub";
 Word "chez"; Word "juridique"; Word "suivant"; Word "concernant";
 Word "jusque"; Word "sur"; Word "contre"; Word "lez"; Word "vers"; Word "d";
 Word "malgre"; Word "versus"; Word "dans"; Word "moyennant"; Word "via";
 Word "de"; Word "nonobstant"; Word "vis-a-vis"; Word "depuis"; Word "outre";
 Word "voici"; Word "derriere"; Word "par"; Word "voile"; Word "des";
 Word "parmi"; Word "devant"; Word "passe"];;

MyStream.to_list (snd (Tokenizer.tokenize_file "test/page1.html")) ;; 
Tokenizer.print_tokenize_file "test/page1.html" ;; 
indexer "test/page1.html" ;;
top_vingt "test/page1.html" ;;
filter_prepositions (indexer "test/page1.html") ;;

MyStream.to_list (snd (Tokenizer.tokenize_file "test/Prepositions")) ;; 

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

