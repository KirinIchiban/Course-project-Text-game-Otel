// Игровой цикл 
module HotelGame.GameLoop

open HotelGame.Artefacts
open HotelGame.Mechanics
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
        do! (takeItem item) >>! ignore

    | ["осмотреть"; item] ->
        do! (examineItem item) >>! (fun text -> writeLine text)

    | ["открыть"; door] ->
        do! (openDoor door) >>! ignore

    | ["говорить"; character] ->
        do! (talkTo character) >>! (fun text -> writeLine text)

    | ["допрос"; character] ->
        do! (interrogate character) >>! (fun text -> writeLine text)

    | ["расследовать"] -> 
        let! text = investigate
        do! writeLine text

    | ["закончить"; choiceStr] ->
        match Int32.TryParse choiceStr with
        | true, choice ->
            let! text = finish choice
            do! writeLine text
        | _ ->
            do! writeLine "Введите число: закончить 1 или закончить 2"

    | ["сохранить"] -> do! saveGame
    | ["загрузить"] -> 
        do! loadGame; 
        do! look
    | ["помощь"] -> do! welcome
    | ["остановить"] ->
        do! writeLine "До свидания!"
        return ()

    | _ ->
        do! writeLine "Неизвестная команда. Введите 'помощь' для списка команд."

    return! gameLoop ()
}


let start () = async {
    let! _ = dialog {
        do! welcome
        do! look
        return! gameLoop ()
    } initialState
    printfn "\nИгра завершена."
}

[<EntryPoint>]
let main _ =
    printfn "Загрузка игры..."
    start() |> Async.RunSynchronously
    printfn "Нажмите любую клавишу..."
    Console.ReadKey() |> ignore
    0