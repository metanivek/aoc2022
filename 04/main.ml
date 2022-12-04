let ( // ) = Filename.concat
let file = Filename.current_dir_name // "input.txt"

(* answer 1 is 471 *)

module Range = struct
  type t = { lower : int; upper : int }

  let parse input =
    match String.split_on_char '-' input with
    | [ a; b ] -> { lower = int_of_string a; upper = int_of_string b }
    | _ -> failwith "invalid input"

  let contains t d = t.lower <= d && d <= t.upper

  (** [subsumes a b] returns [true] if [b] is fully contained in [a] *)
  let subsumes a b = contains a b.lower && contains a b.upper
end

module AssignmentPair = struct
  type t = { one : Range.t; two : Range.t }

  let parse input =
    match String.split_on_char ',' input with
    | [ a; b ] -> { one = Range.parse a; two = Range.parse b }
    | _ -> failwith "invalid input"

  let redundant t = Range.subsumes t.one t.two || Range.subsumes t.two t.one
end

let () =
  In_channel.with_open_text file @@ fun ic ->
  let rec aux acc =
    match In_channel.input_line ic with
    | None -> acc
    | Some s -> aux (AssignmentPair.parse s :: acc)
  in
  aux []
  |> List.map AssignmentPair.redundant
  |> List.filter Fun.id
  |> List.length
  |> Fmt.pr "redundant assignments %d"
