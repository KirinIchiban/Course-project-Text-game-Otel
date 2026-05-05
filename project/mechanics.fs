// Монада Dialog/сохранение/работа с консолью
module HotelGame.Mechanics

open System
open System.IO
open System.Text.Json

open HotelGame.Types

type Dialog<'a> = GameState -> Async<'a * GameState>

let bind (m: Dialog<'a>) (f: 'a -> Dialog<'b>) : Dialog<'b> =
    fun state -> async {
        let! (a, newState) = m state
        return! f a newState
    }

let (>>=) = bind

type DialogBuilder() =
    member _.Bind(d, f) = bind d f
    member _.Return(x) = fun state -> async { return x, state }
    member _.ReturnFrom(d) = d
    member _.Zero() = fun state -> async { return (), state }
    member _.Combine(d1, d2) = bind d1 (fun () -> d2)
    member _.Delay(f) = f()
    member _.TryWith(body, handler) = 
        fun state -> async {
            try
                return! body state
            with ex ->
                return! handler ex state
        }
    member _.For(sequence: seq<'a>, body: 'a -> Dialog<unit>) : Dialog<unit> =
        fun state -> async {
            let mutable currentState = state
            for item in sequence do
                let! ((), newState) = body item currentState
                currentState <- newState
            return (), currentState
        }

let dialog = DialogBuilder()

let readLine : Dialog<string> = fun state -> async {
    let! line = Console.In.ReadLineAsync() |> Async.AwaitTask
    return line, state
}

let writeLine (text: string) : Dialog<unit> = fun state -> async {
    do! Console.Out.WriteLineAsync(text) |> Async.AwaitTask
    return (), state
}

let write (text: string) : Dialog<unit> = fun state -> async {
    do! Console.Out.WriteAsync(text) |> Async.AwaitTask
    return (), state
}

let writeLineColored (text: string) (color: ConsoleColor) : Dialog<unit> = fun state -> async {
    let oldColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    do! Console.Out.WriteLineAsync(text) |> Async.AwaitTask
    Console.ForegroundColor <- oldColor
    return (), state
}

let getState : Dialog<GameState> = fun state -> async { return state, state }

let setState newState : Dialog<unit> = fun _ -> async { return (), newState }

let updateState (updater: GameState -> GameState) : Dialog<unit> =
    getState >>= (updater >> setState)

let handleResult action valid : Dialog<unit> = dialog {
    let! result = action
    match result with
    | Ok value -> do! valid value
    | Error err -> do! writeLine err
}

let (>>!) action valid = handleResult action valid

let handleUnit (action: Dialog<unit>) (next: Dialog<unit>) : Dialog<unit> = dialog {
    do! action
    do! next
}

let (>>.) action next = handleUnit action next

let autoSave : Dialog<unit> = fun state -> async {
    let json = JsonSerializer.Serialize(state)
    do! File.WriteAllTextAsync("save.json", json) |> Async.AwaitTask
    return (), state
}

let saveGame : Dialog<unit> = dialog {
    let! state = getState
    let json = JsonSerializer.Serialize(state)
    do! fun state -> async {
        do! File.WriteAllTextAsync("savegame.json", json) |> Async.AwaitTask
        return (), state
    }
    do! writeLine "Игра сохранена"
}

let loadGame : Dialog<unit> = dialog {
    let! json = fun state -> async {
        let! text = File.ReadAllTextAsync("savegame.json") |> Async.AwaitTask
        return text, state
    }
    let loaded : GameState = JsonSerializer.Deserialize<GameState>(json)
    do! setState loaded
    do! writeLine "Игра загружена"
}