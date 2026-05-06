//  Сюжетные события
module HotelGame.PlotFunctions

open System
open System.IO
open System.Text.Json

open HotelGame.Types
open HotelGame.Mechanics
open HotelGame.Artefacts


let add x lst =
    match List.contains x lst with
    | true -> lst
    | false -> x :: lst

let runOnce eventName (action: Dialog<unit>) : Dialog<unit> = dialog {
    let! state = getState

    if List.contains eventName state.EventsTriggered then
        return ()
    else
        do! action
        do! updateState (fun s ->
            { s with EventsTriggered = add eventName s.EventsTriggered })
}

let murderSceneEvent : Dialog<unit> =
    runOnce "Убийство обнаружено" (dialog {
        do! updateState (fun state ->
            { state with
                InvestigationStarted = true
                Characters = state.Characters |> Map.add "Хинкус" "Каминная"
                Items = state.Items |> Map.add "Пистолет Люгер" "Крыша"
                LugerAvailable = true
            })

        do! writeLine "\nВНИМАНИЕ: СЦЕНА УБИЙСТВА!"
        do! writeLine "Вы входите в номер Олафа. В комнате холодно."
        do! writeLine "На полу лежит тело Олафа Андварафорса. Его шея вывернута под неестественным углом."
        do! writeLine "Необходимо осмотреть тело"
    })


let glebskiRoomEvent : Dialog<unit> =
    runOnce "Записка прочитана" (dialog {
        let! state = getState

        if state.Location = "Номер Глебски (4)" && state.OlafSuitcaseMoved then

            do! updateState (fun s ->
                { s with
                    NoteFound = true
                    NoteRead = true
                    MuseumNoiseTriggered = true })

            do! writeLine "\nВ вашем номере вы замечаете странную записку..."

            do! writeLine ""
            do! writeLine "СОДЕРЖАНИЕ ЗАПИСКИ:"
            do! writeLine "\"Инспектору Глебски. В отеле под именем Хинкус скрывается"
            do! writeLine "опасный гангстер, маньяк и садист Филин. Он вооружен и угрожает одному из"
            do! writeLine "постояльцев. Примите срочные меры.\""
            do! writeLine ""
            do! writeLine "Записка анонимная, написана отпечатанными буквами."
            do! writeLine "Теперь вы знаете о настоящей личности Хинкуса или о том, что его кто-то хочет подставить."
    })

let museumNoiseEvent : Dialog<unit> = dialog {
    let! state = getState

    if state.MuseumNoiseTriggered then
        do! updateState (fun s -> { s with MuseumNoiseTriggered = false })

        do! writeLine "\nИз соседнего номера-музея доносятся странные звуки"
        do! writeLine "Похоже, там что-то происходит."
        do! writeLine "Слышны приглушенные стоны, раздается стук."
        do! writeLine "Кто-то явно там есть! Стоит проверить номер-музей."
}

let hinkleFoundEvent : Dialog<unit> =
    runOnce "Хинкус найден" (dialog {
        do! updateState (fun state ->
            { state with
                HinkleFound = true
                CluesFound =
                    state.CluesFound
                    |> add "Часовая стрелка часов Хинкуса сломана"
                    |> add "Синяки на шее Хинкуса"
                Characters = state.Characters |> Map.add "Хинкус" "Столовая"
            })

        do! writeLine "\nХинкус найден!"
        do! writeLine "Под столом вы находите связанного Хинкуса с кляпом во рту."
        do! writeLine "Он в ужасе, на шее у него синяки."
        do! writeLine "Вы освобождаете его. Хинкуса бьет дрожь, он ничего не может внятно объяснить."
        do! writeLine "Он молчит, трясясь от страха."
        do! writeLine "Странно, сегодня он весь день провел на крыше."
        do! writeLine "Стрелка на его часах застыла, показывая время нападения. В этот момент все были в столовой на празднике,"
        do! writeLine "но некоторые люди все же выходили. Например, Брюн и Олаф."
        do! writeLine "Хинкус благодарно кивает и пытается прийти в себя, всё еще потрясенный случившимся."
        do! writeLine "Он уходит в сторону столовой за успокоительным бренди. У вас есть отличный шанс заглянуть в его номер и обыскать вещи."
        do! writeLine "🔍 Обнаружены улики: Часовая стрелка часов Хинкуса сломана, Синяки на шее Хинкуса"
    })

let roofDummyEvent : Dialog<string> = dialog {
    let! state = getState
    let newCount = state.SnowDummyDiscoveryCount + 1

    let alreadyTriggered = List.contains "Снежное чучело" state.EventsTriggered
    if not alreadyTriggered then
        do! updateState (fun s ->
            { s with
                SnowDummyDiscoveryCount = newCount
                CluesFound = add "Снежное чучело Хинкуса на крыше" s.CluesFound
                EventsTriggered = add "Снежное чучело" s.EventsTriggered
            })
  
    let message = 
        match newCount with
            | 1 -> "ЧТО ЭТО?! На шезлонге сидит Хинкус!\n\
            Вы осторожно подходите ближе... но это всего лишь снежное чучело!\n\
            Кто-то сделал его, чтобы создать иллюзию присутствия Хинкуса на крыше.\n\
            У вас мурашки по коже от этой находки. Кто и зачем это сделал?\n\
            🔍 Обнаружена улика: Снежное чучело Хинкуса на крыше"
            | 2 -> "Снежное чучело все еще здесь. При свете дня оно выглядит еще более жутко.\n\
                Кто-то потратил немало времени, чтобы создать такую реалистичную копию.\n\
                Но зачем? Чтобы все думали, что Хинкус был на крыше?"
            | _ -> "Чучело Хинкуса по-прежнему сидит в шезлонге.\n\
                Вы уже привыкли к этому жуткому зрелищу, но вопросы остаются:\n\
                кто и зачем создал эту иллюзию?"

    return message
}

let examineBodyEvent : Dialog<unit> =
    runOnce "Осмотр тела" (dialog {
        do! updateState (fun state ->
            { state with
                OlafBodyExamined = true
                CluesFound =
                    state.CluesFound
                    |> add "Химический запах изо рта Олафа"
                    |> add "Открытое окно в комнате Олафа"
            })

        do! writeLine "Вы осматриваете тело..."
        do! writeLine "🔍 Обнаружены улики:"
        do! writeLine "Химический запах изо рта Олафа"
        do! writeLine "В руке он сжимает ожерелье Кайсы."
        do! writeLine "Открытое окно в комнате Олафа"
    })

let examineBagEvent : Dialog<unit> =
    runOnce "Осмотр баула" (dialog {
        do! updateState (fun state ->
            { state with
                HinkusBagExamined = true
                CluesFound =
                    add "Пропавшие золотые часы Мозеса" state.CluesFound
            })
        do! writeLine "Вы открываете баул Хинкуса и находите фальшивый багаж - тряпье и случайные книги, \
         явно муляж, собранный на скорую руку. Отодвигая хлам, ваше внимание цепляет предмет на дне."
        do! writeLine "Вы находите пропавшие часы Мозеса, которые он искал днём!"
        do! writeLine "🔍 Обнаружена улика: Пропавшие золотые часы Мозеса"
    })

let examineDummy : Dialog<string> = dialog {
        let! state = getState

        if state.Location <> "Крыша" then
            return "Здесь нет чучела"
        else
            match state.SnowDummyDiscoveryCount with
            | 1 -> 
                do! updateState (fun s ->
                    { s with
                        LugerAvailable = true
                        CluesFound =
                            s.CluesFound
                            |> add "Под шезлонгом найден пистолет Люгер с серебряными пулями" })
                return "Вы внимательно осматриваете чучело. Оно сделано из снега и старой одежды.\nНа голове - шляпа Хинкуса. Кто-то знал, во что он был одет.\nЭто не случайность - это тщательно спланированная инсценировка.\nПод одним из шезлонгов находится пистолет Люгер с серебряными пулями!\nЛюгер калибра 0.45 с удлиненной рукоятью. Вот это было настоящее гангстерское оружие.\nПохоже, Хинкус держал его при себе до нападения, но зачем?..\n\n🔍 Обнаружена улика: Под шезлонгом найден пистолет Люгер с серебряными пулями"
            | _ -> 
                return "Чучело начинает подтаивать, но все еще сохраняет форму.\nИнтересно, сколько еще оно простоит?"
    }
