type Card =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    | Joker

    // Add two cards
    static member (+)(c1: Card, c2: Card) = c1.Values + c2

    // Add a sequence of ints and a card
    static member (+)(values: seq<int>, card: Card) =
        seq {
            for x in values do
                for y in card.Values do
                    yield x + y
        }
        |> Set

    member this.Values =
        match this with
        | Two -> [ 2 ]
        | Three -> [ 3 ]
        | Four -> [ 4 ]
        | Five -> [ 5 ]
        | Six -> [ 6 ]
        | Seven -> [ 7 ]
        | Eight -> [ 8 ]
        | Nine -> [ 9 ]
        | Ten
        | Jack
        | Queen
        | King -> [ 10 ]
        | Ace -> [ 1; 11 ]
        | Joker -> [ 0 ]

let total =
    Ace + Five + Nine + Ace + Queen + Joker


