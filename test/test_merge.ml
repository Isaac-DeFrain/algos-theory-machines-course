open! Base
open! QCheck
open! Algorithms_theory_machines

let simple_test () =
  let res = Merge.sort [] in
  Alcotest.(check @@ list int) "empty list sort" [] res

let test0 =
  Test.make ~count:100 ~name:"QCheck merge" (list int) (fun l ->
      let ls = List.sort l ~compare:Int.compare in
      let ms = Merge.sort l in
      let pairs = List.zip_exn ls ms in
      List.for_all pairs ~f:(fun (a, b) -> a = b))

let gen_sorted =
  let open Gen in
  let* size = int_bound 1000 in
  range_subset ~size 0 1000 >|= Array.to_list

let test_sorted =
  Test.make ~count:100 ~name:"QCheck merge sorted" (make gen_sorted) (fun l ->
      let m = Merge.sort l in
      let pairs = List.zip_exn l m in
      List.for_all pairs ~f:(fun (a, b) -> a = b))

let suite =
  [ QCheck_alcotest.to_alcotest test0; QCheck_alcotest.to_alcotest test_sorted ]
