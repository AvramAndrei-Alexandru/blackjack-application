-----------------------
-- Avram Andrei-Alexandru
-- 13.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore, list2)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Random
import Debug


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool,
    score : Int,
    won : Bool,
    lost : Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True 0 False False

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToogleDeck
  | NewGame


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        updateModel: Model -> Model
        updateModel oldModel =
            if oldModel.score == 21 then {oldModel | won = True} else if oldModel.score > 21 then {oldModel | lost = True} else oldModel
    in
  case msg of
    Draw ->
      ( model
      , drawCard model
      )

    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( updateModel {model | hand = List.reverse (newCard::(List.reverse model.hand)), deck = List.filter (\x -> x /= newCard) model.deck, score = calculateScore (newCard::model.hand)}
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      (
        {model | showDeck = not model.showDeck}
      , Cmd.none
      )
    NewGame ->
      (
         startingModel
        , Cmd.none
       )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none



{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hears, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hears, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}
-- calculateScore [Card {face = TWO, suit = HEARTS}]
-- calculateScore [Card {face = TWO, suit = DIAMONDS}, Card {face = ACE, suit = SPADES}, Card {face = ACE, suit = SPADES}]
-- calculateScore [Card {face = KING, suit = DIAMONDS}, Card {face = KING, suit = DIAMONDS}, Card {face = ACE, suit = SPADES}, Card {face = ACE, suit = SPADES}]
calculateScore : List Card -> Int
calculateScore cards =
    let
        aceList = List.filter (\x -> (List.length (cardValue x)) == 2) cards
        computeScoreWithoutAce : List Card -> Int
        computeScoreWithoutAce list =
            let
                listWithoutAce = List.filter (\x -> (List.length (cardValue x)) == 1) list
                getValueFromList : List Int -> Int
                getValueFromList l =
                    case l of
                        x::_ -> x
                        [] -> 0
            in
                List.map (\x -> getValueFromList (cardValue x)) listWithoutAce |> List.sum
        computeScoreWithAce : Int -> Int -> Int
        computeScoreWithAce scoreWithoutAce numberOfAce =
            let
                isSumSmallerOrEqWith10 =
                    if scoreWithoutAce <= 10 then True else False
            in
                case isSumSmallerOrEqWith10 of
                    True -> if scoreWithoutAce + 11 + numberOfAce - 1 <= 21 then scoreWithoutAce + 11 + numberOfAce - 1 else scoreWithoutAce + numberOfAce
                    False -> scoreWithoutAce + numberOfAce
    in
        case List.length aceList of
            0 -> computeScoreWithoutAce cards
            _ -> computeScoreWithAce (computeScoreWithoutAce cards) (List.length aceList)

list2 = [Card Ace Spades, Card Ace Clubs, Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs,Card Ace Clubs]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"

    renderHand : List Card -> Html Msg
    renderHand list =
        list
        |> List.map(\x -> viewCard x)
        |> ul []

    getHtml : String -> Html Msg
    getHtml message =
        div [style "display" "inline-block"] [
        div [style "font-size" "6em"] [text message]
        ]

    renderDeck : List Card -> Html Msg
    renderDeck list =
        if model.showDeck == False then (getHtml "") else
        list
        |> List.map (\x -> cardToUnicode x)
        |> List.map (\x -> getHtml x)
        |> ul []

    checkForGameOver : Html Msg
    checkForGameOver =
        if model.won then getHtml "The player won." else if model.lost then getHtml "The player lost, please start a new game." else getHtml ""


  in
    div []
      [ div [] [ h1 [] [text appName] ]
      , div [] [text ("The player score is " ++ (String.fromInt(model.score)))]
      , button [ onClick Draw, disabled(model.lost || model.won)]  [ text "Draw a card" ]
      , button [ onClick ToogleDeck]  [ text "Toggle the visibility of the remaining cards in the deck" ]
      , button [ onClick NewGame]  [ text "Start a new game" ]
      , renderHand model.hand
      , renderDeck model.deck
      , checkForGameOver
      ]