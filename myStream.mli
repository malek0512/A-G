val is_empty : 'a Stream.t -> bool
val to_list : 'a Stream.t -> 'a list
val to_string : char Stream.t -> string
val get_rid_of_spaces : char Stream.t -> char Stream.t
val anything_but : 'a list -> 'a Stream.t -> 'a list
val iter_counter : int -> (int -> 'a -> unit) -> 'a list -> unit
val char_list_to_string : char list -> string
val apply_on_file : (char Stream.t -> 'a) -> string -> 'a
val append : string -> char Stream.t -> char Stream.t
val parse_with : ('a Stream.t -> 'b) -> 'a Stream.t -> bool * 'b
val parse_file_with : (char Stream.t -> 'a) -> string -> bool * 'a
