open! Base
open! QCheck
open! Algorithms_theory_machines

let n = 1000

let sorted =
  let l = Gen.(generate ~n printable) in
  List.dedup_and_sort l ~compare:Char.compare |> List.map ~f:Char.to_string

let simple_test0 () =
  let key = List.hd_exn sorted in
  let t = Binary_search.search ~key ~sorted in
  Alcotest.(check int) "Search hd" 0 t.index

let simple_test_case0 =
  let open Alcotest in
  test_case "Search hd" `Quick simple_test0

let simple_test_rand () =
  let i = Random.int n in
  match List.nth sorted i with
  | None -> ()
  | Some key ->
    let t = Binary_search.search ~key ~sorted in
    Alcotest.(check int) "Search random" i t.index

let simple_test_case_rand =
  let open Alcotest in
  test_case "Search random" `Quick simple_test_rand

let qcheck_test =
  Test.make ~count:100 ~name:"QCheck search" printable_char (fun c ->
      let key = Char.to_string c in
      let t = Binary_search.search ~key ~sorted in
      t.index = -1 || (String.( = ) key @@ List.nth_exn sorted t.index))

(* convert to Alcotest *)
let suite =
  [ simple_test_case0
  ; simple_test_case_rand
  ; QCheck_alcotest.to_alcotest qcheck_test
  ]
