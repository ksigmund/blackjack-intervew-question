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

// Active Pattern - returns a pattern case with it associated data (i.e. a single value or a list of values)
let (|Number|Face|Ace|) card =
    match card.Rank with
    | Number x when x >= 2 && x <= 10 -> Number x
    | Number x -> failwith "Number cards must have values from 2 and 10" // *shouldn't* happen...right???
    | Jack
    | Queen
    | King -> Face 10
    | Ace -> Ace [ 1; 11 ]

let getTotals (cards: Card list) =
    cards
    |> List.fold
        (fun (totals: Set<int>) (card: Card) ->
            match card with
            | Number value -> totals |> Set.map ((+) value)
            | Face value -> totals |> Set.map ((+) value)
            | Ace values ->
                values
                |> List.map (fun value -> totals |> Set.map ((+) value))
                |> Set.unionMany)
        (Set.empty.Add(0))

let total =
    getTotals [ { Rank = Ace; Suit = Heart }
                { Rank = Number 8; Suit = Club }
                { Rank = Number 13; Suit = Club } ]
