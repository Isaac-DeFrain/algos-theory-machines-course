(** reads file at [path] returns sorted list of lines and writes this to [path]
    closes file handle when failed or finished *)

val read_and_clean : path:string -> string list
