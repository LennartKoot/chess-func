namespace Chess.Domain

module Entities =
    type Colour = | White | Black

    type Rank = One | Two | Three | Four | Five | Six | Seven | Eight
        with static member List = [One;Two;Three;Four;Five;Six;Seven;Eight]
    type File = A | B | C | D | E | F | G | H
        with static member List = [A;B;C;D;E;F;G;H]

    type PieceType = | King | Queen | Rook | Bishop | Knight | Pawn
    type Piece = { 
        Type: PieceType
        Colour: Colour
    }

    type Square = File * Rank
    type Board = Map<Square,Piece option>

    type GameProgress = InProgress | WhiteWon | BlackWon | Draw
    type GameState = {
        Board: Board
        NextMove: Colour
        Progress: GameProgress
        Error: string option
    }

    type AttemptedMove = {
        From: Square
        To: Square
    }
    type Move = {
        Piece: Piece
        From: Square
        To: Square
    }

module Logic =
    open Entities

    let initGame () =
        let white piece = Some { Type = piece; Colour = White }
        let black piece = Some { Type = piece; Colour = Black }
        let whitePawn = white Pawn
        let blackPawn = black Pawn

        let createRow rank pieces : (Square * Piece option) list =
             let squares = File.List |> List.map (fun file -> (file, rank))
             List.zip squares pieces

        let board = Map(
            (createRow Rank.One [white Rook; white Knight; white Bishop; white Queen; white King; white Bishop; white Knight; white Rook]) @
            (createRow Rank.Two (List.replicate 8 whitePawn)) @
            (createRow Rank.Three (List.replicate 8 None)) @
            (createRow Rank.Four (List.replicate 8 None)) @
            (createRow Rank.Five (List.replicate 8 None)) @
            (createRow Rank.Six (List.replicate 8 None)) @
            (createRow Rank.Seven (List.replicate 8 blackPawn)) @
            (createRow Rank.Eight [black Rook; black Knight; black Bishop; black Queen; black King; black Bishop; black Knight; black Rook])
        )
        { Board = board; NextMove = White; Progress = InProgress; Error = None }

    let getHorizontalMoveDist move =
        let fromIndex = Rank.List |> List.findIndex (fun e -> e = (snd move.From))
        let toIndex = Rank.List |> List.findIndex (fun e -> e = (snd move.To))
        toIndex - fromIndex

    let getVerticalMoveDist move =
        let fromIndex = File.List |> List.findIndex (fun e -> e = (fst move.From))
        let toIndex = File.List |> List.findIndex (fun e -> e = (fst move.To))
        toIndex - fromIndex

    let validateMovePiece state (move: AttemptedMove) =
        match state.Board |> Map.find move.From with
        | Some piece ->
            if piece.Colour = state.NextMove
            then Ok { Piece = piece; From = move.From; To = move.To }
            else Error "This piece does not belong to you"
        | None -> Error "No piece selected"

    let validateMoveShape state move =
        let vertical = getVerticalMoveDist move
        let horizontal = getHorizontalMoveDist move
        
        let isL () =
            match (abs vertical, abs horizontal) with
            | (1,2) -> true
            | (2,1) -> true
            | _ -> false

        let isDiagonal () =
            abs vertical = abs horizontal

        let validateKnight () =
            if isL ()
            then Ok move
            else Error "Knight can only move in an L pattern"

        let validateBishop () =
            if isDiagonal ()
            then Ok move
            else Error "Bishop can only move diagonally"

        match move.Piece.Type with
        | Knight -> validateKnight ()
        | Bishop -> validateBishop ()
        | _ -> Ok move // TODO Others

    let updateBoard board move =
        board |> Map.add move.From None |> Map.add move.To (Some move.Piece)

    let updateNextMove color =
        match color with
        | Black -> White
        | White -> Black

    let validateMove state attemptedMove =
        attemptedMove
        |> validateMovePiece state
        |> Result.bind (validateMoveShape state)

    let move (state: GameState) (attemptedMove: AttemptedMove) =
        let validateMove = validateMove state attemptedMove
        match validateMove with
        | Ok move ->
            { state with
                Board = updateBoard state.Board move
                NextMove = updateNextMove state.NextMove
                Error = None
            }
        | Error msg -> { state with Error = Some msg }
