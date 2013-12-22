val split_at : int -> string -> string * string
type token =
    Word of string
  | Symbol of string
  | Integer of int
  | Close of string
  | Open of string
  | Mark of string
  | String of string
  | Ident of string
  | Comment
  | Space
val pretty_token : token -> string
val print_token : token -> unit
val integer : char Stream.t -> int
val at_least_one_digit : int -> char Stream.t -> int
val digit : char Stream.t -> int
val some_digit : int -> char Stream.t -> int
val tiret : char Stream.t -> string
val underscore : char Stream.t -> string
val column : char Stream.t -> string
val chiffre : char Stream.t -> string
val letter : char Stream.t -> string
val some_letters : char Stream.t -> string
val at_least_one_letter : char Stream.t -> string
val accent : char Stream.t -> string
val ident : char Stream.t -> token
val ident_cont : string -> char Stream.t -> token
val word : char Stream.t -> token
val word_cont : string -> char Stream.t -> token
val word_or_ident : char Stream.t -> token
val word_or_ident_cont : string -> char Stream.t -> token
val symbol : char Stream.t -> string
val symbol_cont : string -> char Stream.t -> string
val quoted : char Stream.t -> string
val opening_mark_end :
  token list -> string -> token list -> token Stream.t -> token list
val opening_mark_cont : token list -> token Stream.t -> token list
val html_mark : token Stream.t -> token list
val convert_accent : string -> string -> char Stream.t -> string
val accent_cont : string -> char Stream.t -> string
val html_accent : char Stream.t -> string
val parse_with : (char Stream.t -> 'a) -> string -> bool * 'a
val tokenizer1 : char Stream.t -> char Stream.t
val tokenizer2 : char Stream.t -> token Stream.t
val tokenizer3 : token Stream.t -> token Stream.t
val tokenizer4 : token Stream.t -> token Stream.t
val tokenizer : char Stream.t -> token Stream.t
val tokenize_file : string -> bool * token Stream.t
