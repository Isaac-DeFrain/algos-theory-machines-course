open! Base

(* all keys in the left subtree of a node are smaller than its key all keys in
   the right subtree of a node are larger than its key *)

type 'a bst =
  | Empty
  | Node of
      { key : string ref
      ; value : 'a ref
      ; left : 'a bst ref
      ; right : 'a bst ref
      }
[@@deriving compare]

let leaf key value =
  Node { key = ref key; value = ref value; left = ref Empty; right = ref Empty }

let is_empty = function
  | Empty -> true
  | _ -> false

let rec get k = function
  | Empty -> None
  | Node { key; value; left; right } -> (
    match String.compare k !key with
    | 0 -> Some !value
    | -1 -> get k !left
    | 1 -> get k !right
    | _ -> assert false)

let rec put k v = function
  | Empty -> leaf k v
  | Node { key; value; left; right } -> (
    match String.compare k !key with
    | 0 ->
      value := v;
      Node { key; value; left; right }
    | -1 -> put k v !left
    | 1 -> put k v !right
    | _ -> assert false)

let rec contains k = function
  | Empty -> false
  | Node { key; left; right; _ } -> (
    match String.compare k !key with
    | 0 -> true
    | -1 -> contains k !left
    | 1 -> contains k !right
    | _ -> assert false)

let to_sorted t =
  let q = Queue.create () in
  let rec aux = function
    | Empty -> ()
    | Node { key; left; right; _ } ->
      aux !left;
      Queue.enqueue q !key;
      aux !right
  in
  aux t;
  Queue.to_list q

let iter ~f t = List.iter ~f (to_sorted t)

(* unit tests *)

let t =
  Node
    { key = ref "it"
    ; value = ref 2
    ; left = ref @@ leaf "best" 1
    ; right =
        ref
        @@ Node
             { key = ref "was"
             ; value = ref 2
             ; left =
                 ref
                 @@ Node
                      { key = ref "the"
                      ; value = ref 1
                      ; left = ref @@ leaf "of" 1
                      ; right = ref @@ leaf "times" 1
                      }
             ; right = ref Empty
             }
    }

let () =
  assert (
    match get "times" t with
    | Some 1 -> true
    | _ -> false)

let%expect_test "iter" =
  iter ~f:(Caml.Printf.fprintf Stdio.stdout "%s ") t;
  [%expect {| best it of the times was |}]

(* red-black trees for efficiency *)
