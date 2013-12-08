(* USAGE:

      ocamlc myStream.ml

      ledit ocaml dynlink.cma camlp4o.cma myStream.cmo

      #use "parser.ml";;
 *)


#use "tokenizer.ml" ;;



let (indexer: string -> frequency list) = fun filename ->
      let (_,token_stream) = tokenize_file filename in

;;


(* TEST *)


MyStream.to_list (snd (tokenize_file "test/page1.html")) ;; print_tokenize_file "test/page1.html" ;; indexer "test/page1.html" ;;

MyStream.to_list (snd (tokenize_file "test/page2.html")) ;; print_tokenize_file "test/page2.html" ;; indexer "test/page2.html" ;;

MyStream.to_list (snd (tokenize_file "test/page3.html")) ;; print_tokenize_file "test/page3.html" ;; indexer "test/page3.html" ;;

MyStream.to_list (snd (tokenize_file "test/page4.html")) ;; print_tokenize_file "test/page4.html" ;; indexer "test/page4.html" ;;

MyStream.to_list (snd (tokenize_file "test/page5.html")) ;; print_tokenize_file "test/page5.html" ;; indexer "test/page5.html" ;;

