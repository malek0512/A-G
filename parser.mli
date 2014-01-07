type frequency = int * string
val fctValue : string -> int
val string_to_list : string -> char list
val char_list_to_string : char list -> string
val desaccords : string -> string -> bool * string
val ajout : frequency -> frequency list -> frequency list
val ajout_with_occ : frequency -> frequency list -> frequency list
val prepositions : string list
val html :
  Tokenizer.token Stream.t ->
  int -> (frequency -> frequency list -> frequency list) -> frequency list
val indexer : string -> frequency list
val top_vingt : string -> frequency list
val main : string -> frequency list * frequency list
