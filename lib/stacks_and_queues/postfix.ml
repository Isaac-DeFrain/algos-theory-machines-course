open! Base

type expr =
  | Val of int
  | Op of op

and op =
  | Plus
  | Minus
  | Mult

type stack = string list

let parse = function
  | '+' -> Op Plus
  | '-' -> Op Minus
  | '*' -> Op Mult
  | c -> Val (Char.get_digit_exn c)

let translate = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Mult -> ( * )

let eval s =
  try
    let rec aux stk = function
      | [] -> List.hd_exn stk
      | hd :: tl -> (
        match parse hd with
        | Val hd -> aux (hd :: stk) tl
        | Op op -> (
          match stk with
          | x :: y :: rest -> aux (translate op x y :: rest) tl
          | _ -> raise (Failure "binary operations need two args")))
    in
    Ok (aux [] String.(to_list s))
  with Failure err -> Error err

(* unit tests *)

let assert_test ~name ~expect comp =
  let fail ~res () =
    Stdio.printf "%s failed with unexpected result: %d\n" name res
  in
  let fail_with_error err () =
    Stdio.printf "%s failed with error: %s\n" name err
  in
  match comp with
  | Ok res ->
    if res <> expect then (
      fail ~res ();
      assert false)
  | Error err ->
    fail_with_error err ();
    assert false

let _ =
  assert_test ~name:"2 + 3 = 5" ~expect:5 (eval "23+");
  assert_test ~name:"2 * 3 = 6" ~expect:6 (eval "23*");
  assert_test ~name:"3 - 2 = 1" ~expect:1 (eval "23-");
  assert_test ~name:"2 * (1 + 3) = 8" ~expect:8 (eval "213+*");
  assert_test ~name:"2 * 3 + 1 = 7" ~expect:7 (eval "23*1+");
  assert_test ~name:"((1 + 1 + 1) * 1 + 1) * 1 = 4" ~expect:4
    (eval "111111++*+*");
  assert_test ~name:"(1 + 1) * (1 + 1) + 1 + 1 = 6" ~expect:6
    (eval "11+11+*1+1+")
