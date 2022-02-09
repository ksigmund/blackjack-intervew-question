module Blackjack =
    type Suit =
        | Heart
        | Spade
        | Club
        | Diamond

    type Rank =
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

    type Card =
        { Rank: Rank
          Suit: Suit }

        static member (+)(c1: Card, c2: Card) = c1.Values + c2

        static member (+)(values: seq<int>, card: Card) =
            seq {
                for x in values do
                    for y in card.Values do
                        yield x + y
            }
            |> Set

        member this.Values =
            match this.Rank with
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

module Demo =
    open Blackjack

    let total =
        { Rank = Ace; Suit = Heart }
        + { Rank = Eight; Suit = Club }
        + { Rank = Three; Suit = Club }
        + { Rank = Ace; Suit = Club }
        + { Rank = Jack; Suit = Club }
