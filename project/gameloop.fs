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
    let parts = input.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList

    match parts with
    | ["осмотреться"] -> do! look
    | ["инвентарь"] -> do! showInventory
    | ["улики"] -> do! showClues  

    | ["идти"; target] ->
        do! goto target >>! (fun () -> look)

    | ["взять"; item] ->
        do! takeItem item >>! (fun _ -> dialog { return () })

    | ["открыть"; door] ->
        do! openDoor door >>! (fun _ -> dialog { return () })

    | ["осмотреть"; item] ->
        do! examineItem item >>! (fun text -> writeLine text)

    | ["говорить"; character] ->
        do! talkTo character >>! (fun text -> writeLine text)

    | ["допрос"; character] ->
        do! interrogate character >>! (fun text -> writeLine text)

    | ["расследовать"] -> 
        let! text = investigate
        do! writeLine text

    | ["закончить"; choiceStr] ->
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
