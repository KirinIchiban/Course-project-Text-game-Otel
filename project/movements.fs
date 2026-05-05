//  Перемещения/взаимодействия с предметами
module HotelGame.Movements


let add x lst =
    match List.contains x lst with
    | true -> lst
    | false -> x :: lst

let getConnections place state =
    state.Doors
    |> List.collect (fun (a, b) ->
        [ if a = place then yield b
          if b = place then yield a ])

let isConnected current target state =
    getConnections current state |> List.contains target

let isLocked current target state =
    state.LockedDoors |> List.contains (current, target) || List.contains (target, current)

let canGo current target state =
    match () with
    | _ when current = target -> Error "Вы уже здесь."
    | _ when not (isConnected current target state) -> Error "Нельзя попасть отсюда."
    | _ when isLocked current target state -> Error "Дверь заперта."
    | _ -> Ok ()

let openDoor door : Dialog<Result<unit, string>> = dialog {
    let! state = getState
    let current = state.Location
    
    if not (List.contains "Связка ключей" state.Inventory) then
        return Error "У вас нет ключей"
    elif isLocked current door state then
        let newLockedDoors = 
            state.LockedDoors 
            |> List.filter (fun (a, b) -> not ((a = current && b = door) || (a = door && b = current)))
        do! setState { state with LockedDoors = newLockedDoors }
        do! writeLine $"Вы открыли дверь в {door}"
        return Ok ()
    else
        return Error "Эта дверь не заперта или её нельзя открыть"
}

let goto (place: string) : Dialog<Result<unit, string>> = dialog {
    let! state = getState

    match canGo state.Location place state with
    | Error e ->
        return Error e

    | Ok _ ->
        do! triggerEvents place

        do! updateState (fun s ->
            let count =
                match Map.tryFind place s.RoomEnteredCount with
                | Some c -> c + 1
                | None -> 1

            { s with
                Location = place
                RoomEnteredCount = Map.add place count s.RoomEnteredCount })

        do! autoSave

        return Ok ()
}


let getDescription place =
    match Map.tryFind place descriptions with
    | Some desc -> desc
    | None -> "Вы осматриваетесь вокруг, но ничего особенного не замечаете."

let getCharactersAt place state =
    state.Characters
    |> Map.toList
    |> List.choose (fun (c, loc) -> if loc = place then Some c else None)

let getCluesAt place state =
    allClues
    |> List.choose (fun (clue, loc) ->
        if loc = place && not (List.contains (clueToString clue) state.CluesFound)
        then Some (clueToString clue)
        else None)


let look : Dialog<unit> = dialog {
    let! state = getState
    let place = state.Location

    do! writeLine $"\n=== {place} ==="
    do! writeLine (getDescription place)

    match getItemsAt place state with
    | [] -> ()
    | items -> do! writeLine $"\nПредметы: {String.concat ", " items}"

    match getCharactersAt place state with
    | [] -> ()
    | chars -> do! writeLine $"\nПерсонажи: {String.concat ", " chars}"

    match getCluesAt place state with
    | [] -> ()
    | clues -> 
        do! writeLine $"\n🔍 Возможные улики для осмотра:"
        clues |> List.iter (fun c -> do! writeLine $"  - {c}")

    match getConnections place state with
    | [] -> ()
    | exits -> do! writeLine $"\nВыходы: {String.concat ", " exits}"
}


let getItemsAt place state =
    state.Items
    |> Map.toList
    |> List.choose (fun (item, loc) -> if loc = place then Some item else None)

let takeItem item : Dialog<Result<unit, string>> = dialog {
    let! state = getState

    let updateInventory newState message = dialog {
        do! setState newState
        do! writeLine message
        return Ok ()
    }

    let basePickup () =
        let newItems = Map.remove item state.Items
        let newInventory = item :: state.Inventory

        dialog {
            do! setState { state with Items = newItems; Inventory = newInventory }
            do! writeLine $"Вы взяли: {item}"

            match item with
            | "Связка ключей" ->
                do! writeLine "Теперь вы можете открывать запертые двери!"

            | "Записка о Филине" ->
                do! writeLine "\nСодержание записки:"
                do! writeLine "\"Инспектору Глебски. В отеле под именем Хинкус скрывается..."
                do! writeLine "Примите срочные меры.\""

            | _ -> ()

            return Ok ()
        }

    match Map.tryFind item state.Items with
    | None ->
        return Error "Здесь нет такого предмета"

    | Some loc when loc <> state.Location ->
        return Error "Этот предмет не здесь"

    | Some _ when item = "Пистолет Люгер" && state.Location = "Крыша" && state.LugerAvailable ->
        let newItems = Map.remove item state.Items
        let newInventory = item :: state.Inventory

        return!
            updateInventory
                { state with
                    Items = newItems
                    Inventory = newInventory
                    LugerAvailable = false }
                $"Вы взяли: {item}. Пистолет заряжен серебряными пулями!"

    | Some _ when item = "Пистолет Люгер" && state.Location = "Крыша" ->
        return Error "Пистолет пока недоступен"

    | Some _ ->
        return! basePickup ()
}

let examineItem item : Dialog<Result<string, string>> = dialog {
    let! state = getState
    
    let hasItem =
    List.contains item state.Inventory ||
    match Map.tryFind item state.Items with
    | Some loc when loc = state.Location -> true
    | _ -> false

    match hasItem with
    | false ->
        return Error "У вас нет этого предмета"
    | true ->
        match item with
        | "Баул Хинкуса" when state.Location = "Номер Хинкуса" ->
            let wasExamined = state.HinkusBagExamined
            do! examineBagEvent
            let! newState = getState

            if newState.HinkusBagExamined && not wasExamined then
                return Ok "Вы открываете баул Хинкуса и находите:\nИ хачем Хинкусу понадобились золотые часы?\n"
            else
                return Ok "Баул уже осмотрен"
                
        | "Тело Олафа" when state.Location = "Номер Олафа" ->
            let wasExamined = state.OlafBodyExamined

            do! examineBodyEvent

            let! newState = getState

            if newState.OlafBodyExamined && not wasExamined then
                return Ok "Вы бросаете взгляд на тело Олафа ещё раз, после на всю комнату и осматриваетесь снова.\nОкно в комнате широко распахнуто."
            else
                return Ok "Тело уже осмотрено"
                
        | "Ожерелье Кайсы" ->
            return Ok "Деревянные бусы Кайсы. Найдены в руке убитого Олафа.\nВозможно, убийца хотел подставить Кайсу."
            
        | "Чемодан Олафа" when state.Location = "Номер Олафа" ->
            if not state.OlafSuitcaseExamined then
                do! updateState (fun s ->
                    { s with
                        OlafSuitcaseExamined = true
                        OlafSuitcaseMoved = true })

                return Ok "Вы открываете чемодан Олафа. Внутри странное устройство — электроника неизвестного назначения.\n
                Решив, что это может быть важной уликой, вы относите чемодан в свой номер."
            else
                return Ok "Вы уже осматривали чемодан."
            
        | _ ->
            return Ok "Ничего особенного не видно"
}


let showInventory : Dialog<unit> = dialog {
    let! state = getState
    match state.Inventory with
    | [] -> do! writeLine "Инвентарь пуст"
    | items -> 
        do! writeLine "📦 Инвентарь:"
        items |> List.iter (fun item -> do! writeLine $"  - {item}")
}

let showClues : Dialog<unit> = dialog {
    let! state = getState
    if List.isEmpty state.CluesFound then
        do! writeLine "Улики пока не найдены"
    else
        do! writeLine "🔍 Найденные улики:"
        state.CluesFound |> List.iter (fun clue -> do! writeLine $"  • {clue}")
}
