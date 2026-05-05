// Монада Dialog/сохранение/работа с консолью
module HotelGame.Mechanics


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

let dialog = DialogBuilder()

let autoSave : Dialog<unit> = fun state -> async {
    let json = JsonSerializer.Serialize(state)
    do! File.WriteAllTextAsync("save.json", json) |> Async.AwaitTask
    return (), state
}

let saveGame : Dialog<unit> = dialog {
    let! state = getState
    let json = JsonSerializer.Serialize(state)
    do! File.WriteAllTextAsync("savegame.json", json) |> Async.AwaitTask
    do! writeLine "Игра сохранена"
}

let loadGame : Dialog<unit> = dialog {
    let! json = File.ReadAllTextAsync("savegame.json") |> Async.AwaitTask
    let loaded = JsonSerializer.Deserialize<GameState>(json)
    do! setState loaded
    do! writeLine "Игра загружена"
}

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

let handleResult action valid : Dialog<unit> = dialog {
    let! result = action
    do! (function
        | Ok value -> valid value
        | Error err -> writeLine err
    ) result
}

let (>>!) action valid = handleResult action valid
let writeLineColored text color : Dialog<unit> = fun state -> async {
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