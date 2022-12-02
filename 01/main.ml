let ( / ) = Filename.concat
let file = Filename.current_dir_name / "input1.txt"

(* answer 1: 70116 *)
(* answer 2: 206582 *)

let () =
  In_channel.with_open_text file @@ fun ic ->
  let rec aux current acc =
    let finish () = Array.append [| current |] acc in
    match In_channel.input_line ic with
    | None -> finish ()
    | Some "" -> aux 0 @@ finish ()
    | Some c -> aux (current + int_of_string c) acc
  in
  let calories = aux 0 [||] in
  Array.sort Int.compare calories;
  Fmt.pr "\nall calories: %a" Fmt.(array ~sep:Fmt.comma int) calories;
  let highest_three = Array.sub calories (Array.length calories - 3) 3 in
  let sum = Array.fold_left ( + ) 0 highest_three in
  Fmt.pr "\ntop three: %d" sum
