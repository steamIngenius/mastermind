module Main exposing (..)

import Html exposing (Html, text, div, h1, br)
import Html.Attributes exposing (style)
import Http
import Task
import Random
import Material
import Material.Color
import Material.Options
import Material.Scheme
import Material.Chip as Chip
import Material.Elevation as Elevation
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
            ! [ randomList Shuffle (guessSize * guessSize) ]



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
    div [ style [ ( "padding", "1rem" ) ] ]
        [ h1 [] [ text "Mastermind" ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    div []
        [ div [] <|
            List.map (peg model) <|
                List.indexedMap (,) model.correct
        ]


peg : Model -> ( Index, Color ) -> Html Msg
peg model ( index, color ) =
    let
        materialColor =
            Material.Color.color (colorToMDLColor color) Material.Color.S500
    in
        Chip.span
            [ Material.Color.background materialColor
            , Material.Color.text materialColor
            , Material.Options.css "margin" "1rem"
            , raisedState index model
            , Elevation.transition 300
            , Material.Options.onMouseEnter (Raise index)
            , Material.Options.onMouseLeave (Raise -1)
            ]
            [ Chip.content []
                [ text "O" ]
            ]


raisedState index model =
    case model.state of
        Playing _ (Just raise) ->
            if raise == index then
                Elevation.e8
            else
                Elevation.e2

        _ ->
            Elevation.e2


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
    | Raise Int


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

        Raise index ->
            case model.state of
                Playing current _ ->
                    { model
                        | state = Playing current (Just index)
                    }
                        ! []

                _ ->
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
