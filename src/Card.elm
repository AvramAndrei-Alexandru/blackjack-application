-----------------------
-- Avram Andrei-Alexandru
-- 13.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (Card(..), Face(..), Suit(..), cardValue, viewCard, cardToString, deck, cardToUnicode)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card Face Suit

faceToString : Face -> String
faceToString face =
    case face of
        Ace -> "ACE"
        Two -> "TWO"
        Three -> "THREE"
        Four -> "FOUR"
        Five -> "FIVE"
        Six -> "SIX"
        Seven -> "SEVEN"
        Eight -> "EIGHT"
        Nine -> "NINE"
        Ten -> "TEN"
        Jack -> "JACK"
        Queen -> "QUEEN"
        King -> "KING"

suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs -> "CLUBS"
        Diamonds -> "DIAMONDS"
        Hearts -> "HEARTS"
        Spades -> "SPADES"

cardToString : Card -> String
cardToString card =
  let
      (Card face suit) = card
  in
      (faceToString face) ++ " " ++(suitToString suit)

cardValue : Card -> List Int
cardValue c =
    let
        (Card face _) = c
    in
         case face of
             Ace -> [1, 11]
             Two -> [2]
             Three -> [3]
             Four -> [4]
             Five -> [5]
             Six -> [6]
             Seven -> [7]
             Eight -> [8]
             Nine -> [9]
             _ -> [10]

deck : List Card
deck =
    let
       createCard : Face -> Suit -> Card
       createCard f s = Card f s

       suitHelper : Int -> Suit
       suitHelper s =
           case s of
               0 -> Clubs
               1 -> Diamonds
               2 -> Hearts
               _ -> Spades

       faceHelper : Int -> Face
       faceHelper f =
           case f of
               0 -> Ace
               1 -> Two
               2 -> Three
               3 -> Four
               4 -> Five
               5 -> Six
               6 -> Seven
               7 -> Eight
               8 -> Nine
               9 -> Ten
               10 -> Jack
               11 -> Queen
               _ -> King

       createList : Int -> Int -> List Card -> List Card
       createList currentFaceIndex currentSuitIndex list =
           if currentFaceIndex == 13 then list else
           if currentSuitIndex < 3 then createList currentFaceIndex (currentSuitIndex + 1) ((createCard (faceHelper currentFaceIndex) (suitHelper currentSuitIndex))::list) else
           createList (currentFaceIndex + 1) (0) ((createCard (faceHelper currentFaceIndex) (suitHelper currentSuitIndex))::list)
    in
        createList 0 0 []


{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode card =
    let
        (Card face suit) = card
    in
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     Four -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard card =
   let
     (Card face suit) = card
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
       div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode]
       ,div [style "font-size" "0.6em"]  [text (cardToString card)]
     ]