
(* USAGE:

 * compilation:
   ocamlc <file>.ml

 * usage:
   ledit ocaml <file>.cmo 
 *)

let (is_empty: 't Stream.t -> bool) = fun stream ->
      match Stream.peek stream with
      |	None -> true
      |	Some _ -> false

let rec (to_list: 't Stream.t -> 't list) = fun stream ->
      match Stream.peek stream with
      |	None -> []
      |	Some t -> begin Stream.junk stream ; t::(to_list stream) end

let rec (to_string: char Stream.t -> string) = fun stream ->
      match Stream.peek stream with
      |	None -> ""
      |	Some c -> begin Stream.junk stream ; (String.make 1 c) ^ (to_string stream) end

(*stream.icons c ie return Some c
  stream.junk stream ie supprime le premier element du stream
*)
let rec (get_rid_of_spaces: char Stream.t -> char Stream.t) = fun stream ->
      match Stream.peek stream with
      |	None -> Stream.sempty
      |	Some c -> 
	      begin 
		Stream.junk stream ;
		match c with
		| ' ' | '\t' | '\n' -> get_rid_of_spaces stream
		| _ -> Stream.icons c (get_rid_of_spaces stream)
	      end

let rec (anything_but: 't list -> 't Stream.t -> 't list) = fun forbidden stream ->
      match Stream.peek stream with
      |	None -> []
      |	Some t -> 
	      if List.mem t forbidden 
	      then []
	      else 
		begin 
		  Stream.junk stream ; 
		  t :: (anything_but forbidden stream) 
		end

;;
(*Renvoie une liste d'un stream privé d'une certain list d'elements*)
(*anything_but [2] (Stream.of_list [3;4;2]);;*)

let rec (iter_counter: int -> (int -> 't -> unit) -> 't list -> unit) = fun i action ts ->
      match ts with
      | [] -> ()
      |	t::ts -> begin (action i t) ; iter_counter (i+1) action ts end

let (char_list_to_string: char list -> string) = fun charS ->
      let string = String.create (List.length charS) in 
	begin
	  iter_counter 0 (fun i c -> String.set string i c) charS ;
	  string 
	end


let (apply_on_file: (char Stream.t -> 'result) -> string -> 'result) = fun fonction filename ->
      let channel = (open_in_bin filename) in
	let result = fonction (Stream.of_channel channel) in
	  begin
	    close_in channel ;
	    result
	  end

let (append: string -> char Stream.t -> char Stream.t) = fun string stream ->
      let rec (cons: int -> char Stream.t -> char Stream.t) = fun i s ->
	    if i<0 
	    then s
	    else cons (i-1) (Stream.icons (String.get string i) s)
      in cons ((String.length string) -1) stream

(* PARSER *)

let (parse_with:  ('a Stream.t -> 'result) -> 'a Stream.t -> bool * 'result) = fun my_parser stream ->
      let result = my_parser stream 
      and bool = is_empty stream 
      in (bool,result)

let (parse_file_with:  (char Stream.t -> 'result) -> string -> bool * 'result) = fun my_parser filename ->
      apply_on_file (parse_with my_parser) filename
