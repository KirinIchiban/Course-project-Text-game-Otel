// Игровой цикл 
module HotelGame.GameLoop

open System
open System.IO
open System.Text.Json

open HotelGame.Types
open HotelGame.Mechanics
open HotelGame.Artefacts
open HotelGame.PlotFunctions
open HotelGame.Movements
open HotelGame.Dialogs

let rec gameLoop () : Dialog<unit> = dialog {
    do! write "\n> "
    let! input = readLine
    let parts = input.Trim().Split(' ') |> Array.toList

    match parts with
    | ["осмотреться"] -> do! look
    | ["инвентарь"] -> do! showInventory
    | ["улики"] -> do! showClues  

    | "идти" :: rest ->
        let target = String.Join(" ", rest).Trim()
        do! goto target >>! (fun () -> look)

    | "взять" :: rest ->
        let item = String.Join(" ", rest).Trim()
        do! takeItem item >>! (fun _ -> dialog { return () })

    | "открыть" :: rest ->
        let door = String.Join(" ", rest).Trim()
        do! openDoor door >>! (fun _ -> dialog { return () })

    | "осмотреть" :: rest ->
        let item = String.Join(" ", rest).Trim()
        do! examineItem item >>! (fun text -> writeLine text)

    | "говорить" :: rest ->
        let character = String.Join(" ", rest).Trim()
        do! talkTo character >>! (fun text -> writeLine text)

    | "допрос" :: rest ->
        let character = String.Join(" ", rest).Trim()
        do! interrogate character >>! (fun text -> writeLine text)

    | ["расследовать"] -> 
        let! text = investigate
        do! writeLine text

    | "закончить" :: rest ->
        let choiceStr = String.Join(" ", rest).Trim()
        let! text = finish choiceStr
        do! writeLine text

    | ["сохранить"] -> do! saveGame
    | ["загрузить"] -> 
        do! loadGame
        do! look
    | ["помощь"] -> do! welcome
    | ["остановить"] ->
        do! writeLine "До свидания!"
        return ()

    | _ ->
        do! writeLine "Неизвестная команда. Введите 'помощь' для списка команд."

    return! gameLoop ()
}

let start () : Async<unit> =
    let game = dialog {
        do! welcome
        do! look
        return! gameLoop ()
    }
    async {
        let! (_, _) = game initialState
        Console.WriteLine "\nИгра завершена."
    }

[<EntryPoint>]
let main _ =
    Console.WriteLine "Загрузка игры..."
    start() |> Async.RunSynchronously
    Console.WriteLine "\nНажмите любую клавишу..."
    Console.ReadKey() |> ignore
    0
