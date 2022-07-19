open! Base

let read_file ~path =
  let open Stdio.In_channel in
  let file = create path in
  Exn.protect ~f:(fun () -> input_lines file) ~finally:(fun () -> close file)

let write_file ~path sorted_data =
  let open Stdio.Out_channel in
  let outc = create path in
  List.iter sorted_data ~f:(fprintf outc "%s\n");
  close outc

let read_and_clean ~path =
  let data = read_file ~path in
  if not @@ List.is_sorted_strictly data ~compare:String.compare then (
    let sorted_data = List.dedup_and_sort data ~compare:String.compare in
    write_file ~path sorted_data;
    sorted_data)
  else data
