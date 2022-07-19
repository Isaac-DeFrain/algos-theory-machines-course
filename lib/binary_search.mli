(** the return type contains the number of halvings required to find the entry
    and the index of the entry *)
type t =
  { halvings : int
  ; index : int
  }

(** pure binary search *)
val search : key:string -> sorted:string list -> t

(** reads and cleans file before calling [search] on the cleaned contents *)
val search_file : key:string -> path:string -> t
