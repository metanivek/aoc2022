let ( // ) = Filename.concat
let file = Filename.current_dir_name // "input.txt"

(* answer 1 = 7737 *)

module Item = struct
  type t = { v : char; priority : int }

  let parse input =
    let priority =
      match Char.code input with
      | n when n >= 65 && n <= 90 -> n - (65 - 27) (* uppercase *)
      | n when n >= 97 && n <= 122 -> n - (97 - 1) (* lowercase *)
      | _ -> failwith "invalid input"
    in
    { v = input; priority }

  let priority t = t.priority
  let compare a b = Char.compare a.v b.v
end

module Compartment = struct
  module ItemSet = Set.Make (Item)

  type t = ItemSet.t

  let parse input =
    String.fold_left
      (fun acc c -> ItemSet.add (Item.parse c) acc)
      ItemSet.empty input

  let duplicates a b =
    ItemSet.fold
      (fun item acc ->
        match ItemSet.mem item b with false -> acc | true -> item :: acc)
      a []
end

module Rucksack = struct
  type t = { one : Compartment.t; two : Compartment.t }

  let parse input : t =
    let length = String.length input in
    let half_length = length / 2 in
    let one = String.sub input 0 half_length in
    let two = String.sub input half_length half_length in
    { one = Compartment.parse one; two = Compartment.parse two }

  let duplicates t = Compartment.duplicates t.one t.two
end

let () =
  In_channel.with_open_text file @@ fun ic ->
  let rec aux acc =
    match In_channel.input_line ic with
    | None -> acc
    | Some s -> aux (Rucksack.parse s :: acc)
  in
  let rucksacks = aux [] in
  let sum =
    rucksacks
    |> List.map Rucksack.duplicates
    |> List.flatten
    |> List.map Item.priority
    |> List.fold_left ( + ) 0
  in
  Fmt.pr "priority sum = %d\n" sum
