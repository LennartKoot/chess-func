// For more information see https://aka.ms/fsharp-console-apps
open System
open Chess.Domain.Entities
open Chess.Domain.Logic
open Chess.IO.Console

let rec loop (state: GameState) =
    Console.Clear ()
    match state.Error with
    | Some msg -> write $"{msg}\n" ConsoleColor.Yellow
    | None -> ()
    printfn $"%A{state.NextMove} is to move"
    writeGame state |> ignore
    let move = askMove ()
    let newState = Chess.Domain.Logic.move state move
    loop newState

loop (initGame ())