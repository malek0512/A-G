(* USAGE:

 * in ocaml interpreter
      ocamlc myStream.ml
      ledit ocaml dynlink.cma camlp4o.cma myStream.cmo
      #use "<file>_parser.ml";;

 * Pour interpréter un fichier contenant "parser"
      ledit ocaml dynlink.cma camlp4o.cma 
      #use "<file>_parser.ml";;

 * Pour afficher la traduction en ocaml: 
      camlp4o -impl <file>_parser.ml

 * Pour générer  la traduction dans un fichier .ml:
      camlp4o -impl <File>_parser.ml -o <file>.ml

 * Pour interpréter le fichier ocaml généré: 
      ledit ocaml dynlink.cma camlp4o.cma myStream.cmo
      #use "<file>.ml";;
 *)


(* === function on string === *)
(*
#load "dynlink.cma"
#load "camlp4o.cma"
#load "myStream.cmo"
*)
let (split_at: int -> string -> string * string) = fun k ch ->
      let l = String.length ch in 
	if l < k
	then ("", ch)
	else (String.sub ch 0 k, String.sub ch k (l-k) )


(* === Definition of the significant categories === *)

type token = 
  | Word of string  
  | Symbol of string
  | Integer of int
  | Close of string
  | Open of string
  | Mark of string
  | String of string
  | Ident of string
  | Comment 
  | Space

let (pretty_token: token -> string) = function
  | Word s 
  | Symbol s 
  | String s 
  | Ident  s -> s
  | Integer i -> string_of_int i
  | Close _
  | Open _ -> ""
  | Comment -> ""
  | Space -> " "

let (print_token: token -> unit) = fun token -> 
      print_string ((pretty_token token) ^ " ")



(* === Grammar of integers ==================
   Integer -1->  At_least_one_digit
   At_least_one_digit -2-> Digit . Some_Digit
   Some_Digit -3-> ""
   Some_Digit -4-> Digit . Some_Digit
  =============================================== *)

let rec (integer: char Stream.t -> int) = fun stream -> at_least_one_digit 0 stream

and (at_least_one_digit: int -> char Stream.t -> int) = fun i ->
   parser
    | [< d = digit ; r = some_digit (10*i+d) >] -> r
		  
and (digit: char Stream.t -> int) = 
  parser
    | [< ' ('0'..'9' as char) >] -> (int_of_char char) - (int_of_char '0')
	      
and (some_digit: int -> char Stream.t -> int) = fun i ->
      parser
	| [< d = digit ; r = some_digit (10*i+d) >] -> r
	| [< >] -> i
	  

		  
	      
(* === Grammar of Identifiers ===================
   Ident  -> Letter Ident_Cont
   Ident_Cont -> "" 
   Ident_Cont -> (Letter | Digit | "_")  Ident_Cont

   === Grammar of Word ===================
   Word  -> Letter Word_Cont
   Word_Cont -> "" 
   Word_Cont -> (Letter |"-")  Word_Cont
  =================================================== *)

let (tiret: char Stream.t -> string) = 
   parser
     | [< ' ('-') >] -> "-"

let (underscore: char Stream.t -> string) = 
   parser
   | [< ' ('_') >] -> "_"

let (column: char Stream.t -> string) = 
   parser
   | [< ' (':') >] -> ":"

let (chiffre: char Stream.t -> string) = 
  parser
    | [< ' ('0'..'9' as char) >] -> (String.make 1 char)

(* String.lowercase renvoie un string avec toutes majuscules en minuscules*)
let (letter: char Stream.t -> string) = 
  parser
    | [< ' ('a'..'z' | 'A'..'Z' as char) >] -> String.lowercase (String.make 1 char)

let rec (some_letters: char Stream.t -> string) = 
  parser
    | [< l = letter ; stream >] -> l ^ (some_letters stream)
    | [< >] -> ""

let  (at_least_one_letter: char Stream.t -> string) = 
  parser
    | [< l = letter ; stream >] -> l ^ (some_letters stream)

let (accent: char Stream.t -> string) = 
   parser
     | [< ' ('\013') >] -> " " (* CR *)
     | [< ' ('\092') >] -> "" (* \ *)
     | [< ' ('\038') >] -> "&" (* & *)
     | [< ' ('\233') >] -> "e" (* é *)
     | [< ' ('&')    >] -> "&" 
     | [< ' ('\127'..'\255' as c) >] ->
             (match c with
	     | '\171' -> "``" (* « *)
	     | '\187' -> "''" (* » *)
	     | '\184' -> "," (* , *)
	     | '\224' -> "a" (* à *)
	     | '\226' -> "a" (* â *)
	     | '\232' -> "e" (* è *)
	     | '\233' -> "e" (* é *)
	     | '\234' -> "e" (* ê *) 
	     | '\235' -> "e" (* ë *) 
	     | '\236' -> "i" (* ì *) 
	     | '\237' -> "i" (* í *) 
	     | '\238' -> "i" (* î *) 
	     | '\239' -> "i" (* ï *) 
	     | '\244' -> "o" (* ô *) 
	     | '\249' -> "u" (* ù *) 
	     | '\250' -> "u" (* ú *) 
	     | '\251' -> "u" (* û *) 
	     | '\252' -> "u" (* ü *) 
	     | '\192' -> "A" (* À *) 
	     | '\201' -> "E" (* É *) 
	     | '\212' -> "O" (* Ô *) 
	     | '\219' -> "C" (* Ç *) 
	     | '\231' -> "c" (* ç *) 
	     |  _ ->"?"
	     )
let rec (ident: char Stream.t -> token) =
  parser
    | [< l = letter     ; stream >] -> ident_cont l stream
    | [< u = underscore ; stream >] -> ident_cont u stream

and (ident_cont: string -> char Stream.t -> token) = fun string ->
      parser
	| [< l = letter     ; stream >] -> ident_cont (string ^ l) stream
	| [< c = chiffre    ; stream >] -> ident_cont (string ^ c) stream
	| [< u = underscore ; stream >] -> ident_cont (string ^ u) stream
	| [< c = column     ; stream >] -> ident_cont (string ^ c) stream
	| [< >] -> Ident string
(*
Looping recursion 
let rec fact n = if n=1 then 1 else n * fact (n-1) * fact2 (n-1)
and fact2 r = if r=0 then 0 else r*fact (r-1)
*)

let rec (ident: char Stream.t -> token) =
  parser
    | [< l = letter     ; stream >] -> ident_cont l stream
    | [< u = underscore ; stream >] -> ident_cont u stream

and (ident_cont: string -> char Stream.t -> token) = fun string ->
      parser
	| [< l = letter     ; stream >] -> ident_cont (string ^ l) stream
	| [< c = chiffre    ; stream >] -> ident_cont (string ^ c) stream
	| [< u = underscore ; stream >] -> ident_cont (string ^ u) stream
	| [< c = column     ; stream >] -> ident_cont (string ^ c) stream
	| [< >] -> Ident string
		  
and (word: char Stream.t -> token) = 
  parser
    | [< l = letter     ; stream >] -> word_cont l stream
    | [< a = accent     ; stream >] -> word_cont a stream
    | [< u = underscore ; stream >] -> word_cont u stream

and (word_cont: string -> char Stream.t -> token) = fun string ->
      parser
	| [< l = letter  ; stream >] -> word_cont (string ^ l) stream
	| [< a = accent  ; stream >] -> word_cont (string ^ a) stream
	| [< t = tiret   ; stream >] -> word_cont (string ^ t) stream
	| [< c = chiffre ; stream >] -> word_cont (string ^ c) stream
	| [< >] -> Word string
		  
and (word_or_ident: char Stream.t -> token) = 
  parser
    | [< l = letter     ; stream >] -> word_or_ident_cont l stream
    | [< a = accent     ; stream >] -> word_cont          a stream
    | [< u = underscore ; stream >] -> ident_cont         u stream
	      
and (word_or_ident_cont: string -> char Stream.t -> token) = fun string ->
      parser
	| [< l = letter     ; stream >] -> word_or_ident_cont (string ^ l) stream
	| [< c = chiffre    ; stream >] -> word_or_ident_cont (string ^ c) stream
	| [< a = accent     ; stream >] -> word_cont          (string ^ a) stream
	| [< t = tiret      ; stream >] -> word_cont          (string ^ t) stream
	| [< u = underscore ; stream >] -> ident_cont         (string ^ u) stream
	| [< c = column     ; stream >] -> ident_cont         (string ^ c) stream
	| [< >] -> Ident string
		  

(* === Symbols ==== *)

let rec (symbol: char Stream.t -> string) = 
  parser
    | [< ' ('$'|'#'|'('|')'|'['|']'|'{'|'}'|','|'!'|'%'|'@'|'*'|'+'|'/'|'&'|'|'|'\''|'~'|'?'|'>' as char) >] -> (String.make 1 char)
    | [< ' (':'|';'|'='|'<'|'.'|'-' as char) ; stream >] -> symbol_cont (String.make 1 char) stream

and (symbol_cont: string -> char Stream.t -> string) = fun prefix ->
  parser
    | [< ' (':'|';'|'='|'.'|'/'|'>' as char) >] -> prefix ^ (String.make 1 char) 
    | [< ' ('!'|'-' as char) ; stream >] -> symbol_cont (prefix ^ (String.make 1 char)) stream
    | [< >] ->  prefix

(* === Quoted Strings ==== *)


let (quoted: char Stream.t -> string) = 
  parser
    | [< ' ('\"') ; chars = MyStream.anything_but ['\"'] ; ' ('\"') >] -> MyStream.char_list_to_string chars

(* === Html Mark ============================

   html_mark    -> "<" . (html_comment | closing_mark | opening_mark) . ">"

   html_comment -> "!" . anything_but {<,>} 

   closing_mark -> "/" . Ident

   opening_mark -> Ident . anything_but {<,>}  

 ============================================== *)

let (opening_mark_end: token list -> string -> token list -> token Stream.t -> token list) = fun prefix mark parameters ->
      parser
      | [< '(Symbol ">") >] -> [ Open mark ]
      | [< >] -> prefix @ [ Ident mark ] @ parameters


let (opening_mark_cont: token list -> token Stream.t -> token list) = fun prefix ->
  parser
    | [< '(Space) >] ->  prefix @ [ Space ]
    | [< '(Ident mark) ; parameters = MyStream.anything_but [ Symbol "<" ; Symbol ">"] ; stream >] -> opening_mark_end prefix mark parameters stream	      

let (html_mark: token Stream.t -> token list) = 
   parser
    | [< '(Symbol "<!")   ; _ = MyStream.anything_but [Symbol ">"  ] ; '(Symbol ">")   >] -> []
    | [< '(Symbol "<!--") ; _ = MyStream.anything_but [Symbol "-->"] ; '(Symbol "-->") >] -> []
    | [< '(Symbol "</") ; '(Ident mark) ; '(Symbol ">")  >] -> [ Close mark ]
    | [< '(Symbol "<") ; stream >] -> opening_mark_cont [ Symbol "<" ] stream


(* === Html Accent =============================
    Html_Accent -> "&" . Accent_Name . ";"
    Accent_Name -> nbsp | egrave | eacute | ...
   ======================================== *)

let (convert_accent: string -> string -> char Stream.t -> string) = fun prefix accent ->
      parser
	| [< ' (';') >] -> 
		(match split_at 1 accent with
		| (l,"grave") 
		| (l,"acute") 
		| (l,"circ") 
		| (l,"tilde") 
		| (l,"uml") 
		| (l,"ring") 
		| (l,"Elig") 
		| (l,"elig") 
		| (l,"cedil") -> l
		| _ -> 
			(match accent with
			| "nbsp" -> " "
                        | "acute" -> "'"
                        | "deg" -> ""
                        | "raquo" -> "''"
                        | "rsquo" -> "'"
                        | "laquo" -> "``"
			| _ -> prefix ^ accent ^ ";"
			)
		)
	| [< >] -> prefix ^ accent

let (accent_cont: string -> char Stream.t -> string) = fun prefix ->
  parser
    | [< accent = at_least_one_letter ; stream >] -> convert_accent prefix accent stream 
    | [< >] -> prefix

let (html_accent: char Stream.t -> string) = 
  parser
   | [< ' ('&') ; stream >] -> accent_cont "&" stream 


(* === How to transform a stream of char into a stream of token (= typed words) with space elimination *)

(*
MyStream.to_list (Stream.icons 0 (Stream.icons 1 (Stream.icons 2 (Stream.of_list [3])))) ;;
MyStream.to_list (Stream.iapp (Stream.of_list [0;1;2]) (Stream.of_list [3])) ;;
*)

(* === How to parse a string with a stream parser ? === *)

let (parse_with: (char Stream.t -> 'result) -> string -> bool * 'result) =
  fun my_parser string ->
	begin
	  print_string (String.concat "" [ "\n parser \"" ; string ; "\" = \n"]) ;
	  MyStream.parse_with my_parser (Stream.of_string string)
	end


let rec (tokenizer1: char Stream.t -> char Stream.t) = 
  parser
    | [< s = html_accent ; stream >] -> MyStream.append s (tokenizer1 stream) 
    | [< 'char           ; stream >] -> Stream.icons char (tokenizer1 stream)
    | [< >] -> Stream.sempty

let rec (tokenizer2: char Stream.t -> token Stream.t) = 
  parser
    | [< ' (' '|'\t'|'\n') ; stream >] -> Stream.icons Space (tokenizer2 stream)
    | [< t = word_or_ident ; tokens = tokenizer2 >] -> Stream.icons t tokens
    | [< i = integer       ; tokens = tokenizer2 >] -> Stream.icons (Integer i) tokens
    | [< s = quoted        ; tokens = tokenizer2 >] -> Stream.icons (String s) tokens
    | [< s = symbol        ; tokens = tokenizer2 >] -> Stream.icons (Symbol s) tokens
    | [< >] -> Stream.sempty

let rec (tokenizer3: token Stream.t -> token Stream.t) = 
  parser
    | [< tokens = html_mark ; stream >] -> Stream.iapp (Stream.of_list tokens) (tokenizer3 stream)
    | [< 't ; stream >] -> Stream.icons t (tokenizer3 stream)
    | [< >] -> Stream.sempty

let rec (tokenizer4: token Stream.t -> token Stream.t) = 
  parser
    | [< '(Ident string) ; stream >] -> Stream.icons (Word string) (tokenizer4 stream)
    | [< '(Symbol _) ; stream >] -> tokenizer4 stream
    | [< '(Space) ; stream >] -> tokenizer4 stream
    | [< '(Open"script") ; _ = MyStream.anything_but [ Close "script" ] ;' (Close "script") ; stream >] -> tokenizer4 stream
    | [< 't ; stream >] -> Stream.icons t (tokenizer4 stream)
    | [< >] -> Stream.sempty


let (* USER *) (tokenizer: char Stream.t -> token Stream.t) = fun stream -> tokenizer4 (tokenizer3 (tokenizer2 (tokenizer1 stream)))

let (* USER *) (tokenize_file: string -> bool * 't Stream.t) = fun filename -> MyStream.parse_file_with tokenizer filename


(*
(* === TESTS on STRING === *)

let (test_tokenizer: (char Stream.t -> 't Stream.t) -> string -> bool * 't list) = fun this_tokenizer string ->
      let (bool, t_stream) = parse_with this_tokenizer string 
      in (bool, MyStream.to_list t_stream)
;;

let ($) f g x = f (g x) ;;

let input = "& Given x<y & z<t &four  &nbsp; <B>integers</B>" ;;
test_tokenizer tokenizer1 input ;; 
test_tokenizer (tokenizer2 $ tokenizer1) input ;; 
test_tokenizer tokenizer input  ;;

let input = "&eacute;tant donn&eacute;s a/b>y <!-- comment -->" ;;
test_tokenizer tokenizer1 input ;; 
test_tokenizer (tokenizer2 $ tokenizer1) input ;; 
test_tokenizer tokenizer input  ;;

let input = "<!DOCTYPE html PUBLIC -//W3C//DTD XHTML 1.0 Strict//EN http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd><html xmlns=http://www.w3.org/1999/xhtml lang=fr xml:lang=fr>" ;;
test_tokenizer tokenizer1 input ;; 
test_tokenizer (tokenizer2 $ tokenizer1) input ;; 
test_tokenizer tokenizer input  ;;
*)

(* === TESTS ON HTML PAGES === *)

let (print_tokenize_file: string -> unit) = fun filename ->
      Stream.iter print_token (snd (tokenize_file filename)) 

(*
;;
MyStream.to_list (snd (tokenize_file "test/page1.html")) ;; 
print_tokenize_file "test/page1.html" ;; 

MyStream.to_list (snd (tokenize_file "test/page2.html")) ;; 
print_tokenize_file "test/page2.html" ;; 

MyStream.to_list (snd (tokenize_file "test/page3.html")) ;;  
print_tokenize_file "test/page3.html" ;;

MyStream.to_list (snd (tokenize_file "test/page4.html")) ;;  
print_tokenize_file "test/page4.html" ;;

MyStream.to_list (snd (tokenize_file "test/page5.html")) ;;  
print_tokenize_file "test/page5.html" ;; 

*)
