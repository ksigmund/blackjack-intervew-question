let getTotals (cards: string list) =
    let mutable total = 0
    let mutable aceCount = 0

    for card in cards do
        if (card = "J" || card = "Q" || card = "K") then
            total <- total + 10
        elif (card = "A") then
            aceCount <- aceCount + 1
        else
            let cardValue = string card |> System.Int32.Parse
            total <- total + cardValue

    seq {
        if aceCount = 0 then
            yield total
        elif aceCount = 1 then
            yield total + 1
            yield total + 11
        elif aceCount = 2 then
            yield total + 2
            yield total + 12
            yield total + 22
        elif aceCount = 3 then
            yield total + 3
            yield total + 13
            yield total + 23
            yield total + 33
        elif aceCount = 4 then
            yield total + 4
            yield total + 14
            yield total + 24
            yield total + 34
            yield total + 44
    } |> List.ofSeq

let total =
    getTotals [ "A"; "Q"; "3"; "10"; "A", "asfasdf"]
