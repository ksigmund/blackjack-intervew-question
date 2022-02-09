type Suit =
    | Heart
    | Spade
    | Club
    | Diamond

type Rank =
    | Number of int
    | Jack
    | Queen
    | King
    | Ace

type Card = { Rank: Rank; Suit: Suit }

let getTotals (cards: Card list) =
    cards
    |> List.fold
        (fun (totals: int list) card ->
            match card.Rank with
            | Number x -> totals |> List.map (fun total -> total + x)
            | Jack
            | Queen
            | King -> totals |> List.map (fun total -> total + 10)
            | Ace ->
                totals
                |> List.map (fun total -> [ total + 1; total + 11 ])
                |> List.collect (fun x -> x))
        [0]

let total =
    getTotals [ 
                { Rank = Ace; Suit = Spade }
                { Rank = Ace; Suit = Heart }
                { Rank = Number 13; Suit = Club }
                { Rank = Number 2; Suit = Club } ]
