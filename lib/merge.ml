open! Base

let rec sort l =
  let len = List.length l in
  let rec merge acc = function
    | [], [] -> List.rev acc
    | x :: xs, [] -> merge (x :: acc) (xs, [])
    | [], y :: ys -> merge (y :: acc) ([], ys)
    | x :: xs, y :: ys ->
      if x <= y then merge (x :: acc) (xs, y :: ys)
      else merge (y :: acc) (x :: xs, ys)
  in
  let lo, hi = List.split_n l (len / 2) in
  if len <= 1 then l else merge [] (sort lo, sort hi)
