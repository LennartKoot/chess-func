namespace Chess.IO

open System
open Chess.Domain.Entities

module Console =
      let write string color =
            Console.BackgroundColor <- ConsoleColor.Black
            Console.ForegroundColor <- color
            printf $"%s{string}"
            Console.ResetColor ()

      let writeGame state =
            let colour piece =
                 match piece.Colour with
                 | White -> ConsoleColor.Green
                 | Black -> ConsoleColor.Red

            let ranks =
                  match state.NextMove with
                  | Black -> Rank.List
                  | White -> Rank.List |> List.rev

            ranks |> List.map (fun rank ->
                  File.List |> List.map (fun file ->
                        let piece = state.Board |> Map.find (file, rank)
                        write "|" ConsoleColor.White
                        match piece with
                        | Some p ->
                            match p.Type with
                            | Pawn -> write "🨅" (colour p)
                            | Bishop -> write "🨃" (colour p)
                            | Knight -> write "🨄" (colour p)
                            | Rook -> write "🨂" (colour p)
                            | King -> write "🨀" (colour p)
                            | Queen -> write "🨁" (colour p)
                        | None -> write " " ConsoleColor.White
                  ) |> ignore
                  write "|\n" ConsoleColor.White
            ) |> ignore

      let parseSquare (s: string) : Square =
            let file =
                  match s[0] with
                  | ('a'|'A') -> File.A
                  | ('b'|'B') -> File.B
                  | ('c'|'C') -> File.C
                  | ('d'|'D') -> File.D
                  | ('e'|'E') -> File.E
                  | ('f'|'F') -> File.F
                  | ('g'|'G') -> File.G
                  | ('h'|'H') -> File.H
                  | c -> failwith $"Unrecognized file '{c}'"
            let rank =
                  match s[1] with
                  | '1' -> Rank.One
                  | '2' -> Rank.Two
                  | '3' -> Rank.Three
                  | '4' -> Rank.Four
                  | '5' -> Rank.Five
                  | '6' -> Rank.Six
                  | '7' -> Rank.Seven
                  | '8' -> Rank.Eight
                  | c -> failwith $"Unrecognized rank '{c}'"
            (file, rank)

      let askMove () =
            Console.WriteLine("Type your move as [FromSquare] [ToSquare] (e.g. e2 e4)")
            let args = Console.ReadLine()
            let squares = args.Split(" ")
            let fromSquare = parseSquare squares[0]
            let toSquare = parseSquare squares[1]
            { From = fromSquare; To = toSquare }
