module Main exposing (..)

import Html exposing (Html, text, div, h1, h2)
import Html.Attributes exposing (style)
import Http
import Task
import Random
import Material
import Material.Chip as Chip
import Material.Color
import Material.Options
import Material.Scheme
import Material.Layout as Layout


main : Program Never Model Msg
main =
    Html.program
        { init = createModel
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



-- Types


type Color
    = Red
    | Green
    | Blue
    | Cyan
    | Yellow
    | Empty


type alias Combination =
    List Color


type Hint
    = CorrectPosition
    | WrongPosition


type alias Guess =
    ( Combination, List Hint )


type alias Index =
    Int


type GameState
    = Playing Combination (Maybe Index)
    | GameOver
    | Surrender



-- Model


guessSize : Int
guessSize =
    4


colors : List Color
colors =
    [ Red, Green, Blue, Cyan, Yellow ]


emptyCombination : Combination
emptyCombination =
    List.repeat guessSize Empty


type alias Model =
    { correct : Combination
    , guesses : List Guess
    , state : GameState
    , mdl : Material.Model
    }


createModel : ( Model, Cmd Msg )
createModel =
    let
        startingState =
            Playing emptyCombination Nothing

        guesses =
            []
    in
        Model emptyCombination guesses startingState Material.model
            ! [ randomListFromWeb ShuffleWeb (guessSize * guessSize) ]



-- View


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader ]
            { header = [ header ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewBody model ]
            }


header : Html Msg
header =
    h1 [ style [ ( "padding", "2rem" ) ] ] [ text "Mastermind" ]


viewBody : Model -> Html Msg
viewBody model =
    div [] (List.map peg model.correct)


peg : Color -> Html Msg
peg color =
    let
        materialColor =
            Material.Color.color (colorToMDLColor color) Material.Color.S500
    in
        Chip.span
            [ Material.Color.background materialColor
            , Material.Color.text materialColor
            , Material.Options.css "margin" "1rem"
            ]
            [ Chip.content []
                [ text "O" ]
            ]


colorToMDLColor : Color -> Material.Color.Hue
colorToMDLColor color =
    case color of
        Red ->
            Material.Color.Red

        Green ->
            Material.Color.Green

        Blue ->
            Material.Color.Blue

        Cyan ->
            Material.Color.Cyan

        Yellow ->
            Material.Color.Yellow

        Empty ->
            Material.Color.Grey



-- Update


type Msg
    = NoOp
    | Mdl (Material.Msg Msg)
    | Shuffle (List Int)
    | ShuffleWeb (Result Http.Error (List Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model

        Shuffle random ->
            { model
                | correct = randomCombination guessSize colors random
            }
                ! []

        ShuffleWeb (Ok random) ->
            { model
                | correct = randomCombination guessSize colors random
            }
                ! []

        ShuffleWeb (Err err) ->
            let
                _ =
                    Debug.log "Error! - " err
            in
                model ! []


randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 0 100
        |> Random.list len
        |> Random.generate msg


randomListFromWeb : (Result Http.Error (List Int) -> Msg) -> Int -> Cmd Msg
randomListFromWeb msg len =
    let
        url =
            "https://www.random.org/integers/?num="
                ++ toString len
                ++ "&min=1&max=100&col=1&base=10&format=plain&rnd=new"

        parser =
            String.split "\n"
                >> List.map String.toInt
                >> List.filterMap Result.toMaybe

        request =
            Http.getString url
    in
        request
            |> Http.toTask
            |> Task.map parser
            |> Task.attempt msg


shuffle : List comparable -> List a -> List a
shuffle random list =
    List.map2 (,) list random
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.first


randomCombination : Int -> List a -> List comparable -> List a
randomCombination size xs random =
    List.map (List.repeat size) xs
        |> List.concat
        |> shuffle random
        |> List.take size
