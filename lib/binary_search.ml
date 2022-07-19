open! Base

type t =
  { halvings : int
  ; index : int
  }

let search ~key ~sorted =
  let rec aux acc start = function
    | [] -> { halvings = acc; index = -1 }
    | l -> (
      let len = List.length l in
      let mid = List.nth_exn l (len / 2) in
      let smalls, bigs = List.split_n l (len / 2) in
      match String.compare key mid with
      | -1 -> aux (acc + 1) start smalls
      | 0 -> { halvings = acc; index = start + (len / 2) }
      | 1 -> aux (acc + 1) (start + (len / 2)) bigs
      | _ -> assert false)
  in
  aux 1 0 sorted

let search_file ~key ~path =
  let sorted = File_ops.read_and_clean ~path in
  search ~key ~sorted
