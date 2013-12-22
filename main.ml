(*
#load "dynlink.cma"
#load "camlp4o.cma"
#load "myStream.cmo"
#load "tokenizer.cmo"
*)

(*Verifie le nombre d'argument*)
let check_arg=
      if Array.length Sys.argv < 1 then (
              print_endline ("Ce programme prend 1 arguments.\n");
                  exit 1
                    )

(* Recupere le nom de fichier donner en agument*)          
let file_name=Sys.argv.(1)
  

(*Ouvre un fichier de nom donne en parametre *)
let _= Tokenizer.tokenize_file file_name

let _= Tokenizer.tokenize_file "test/page1.html"
    



