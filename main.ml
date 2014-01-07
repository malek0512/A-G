(*
#load "dynlink.cma"
#load "camlp4o.cma"
#load "myStream.cmo"
#load "tokenizer.cmo"
#load "parser.cmo"
*)

open MyStream
open Tokenizer
open Parser

(*Verifie le nombre d'argument*)
let check_arg=
      if Array.length Sys.argv < 1 then (
              print_endline ("Ce programme prend 1 arguments.\n");
                  exit 1
                    )

(* Recupere le nom de fichier donner en agument*)          
let file_name= Sys.argv.(1)
  

(*Ouvre un fichier de nom donne en parametre *)
let (a,b) = Parser.main file_name

let print_list f lst =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; print_string ";"; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]"

let () = check_arg ;
         print_newline ();
         print_string (" LISTE DES MOTS INDEXÉS ");
         print_newline ();
         print_list (fun (a, b) -> print_string "("; print_int a; print_string ", "; print_string b; print_string ")") a;
         print_newline (); print_newline (); print_newline ();
         print_string (" LISTE DES MOTS MODIFIÉS ");
         print_newline ();
         print_list (fun (a, b) -> print_string "("; print_int a; print_string ", "; print_string b; print_string ")") b
