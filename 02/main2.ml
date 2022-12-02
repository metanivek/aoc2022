let ( / ) = Filename.concat
let file = Filename.current_dir_name / "input.txt"

(* answer 2 : 14184 *)

module Strategy = struct
  type t = Win | Lose | Draw

  let parse = function
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "invalid input"
end

module Move = struct
  type t = Rock | Paper | Scissors

  let parse = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "invalid input"

  let select opponent (strategy : Strategy.t) =
    match (opponent, strategy) with
    | Rock, Lose -> Scissors
    | Paper, Lose -> Rock
    | Scissors, Lose -> Paper
    | Rock, Win -> Paper
    | Paper, Win -> Scissors
    | Scissors, Win -> Rock
    | (_ as m), Draw -> m
end

module Score = struct
  let shape : Move.t -> int = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let versus ~(opponent : Move.t) ~(self : Move.t) =
    match (opponent, self) with
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> 0
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6

  let total ~opponent ~self = shape self + versus ~opponent ~self
end

let () =
  In_channel.with_open_text file @@ fun ic ->
  let rec aux score =
    match In_channel.input_line ic with
    | None -> score
    | Some c ->
        let moves = String.split_on_char ' ' c in
        let opponent = List.nth moves 0 |> Move.parse in
        let self = List.nth moves 1 |> Strategy.parse |> Move.select opponent in
        aux (score + Score.total ~opponent ~self)
  in
  let score = aux 0 in
  Fmt.pr "\nscore = %d" score
