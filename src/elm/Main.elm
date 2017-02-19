module Main exposing (..)

import Html exposing (Html, Attribute, text, div, h1, br)
import Html.Attributes exposing (style, attribute, draggable)
import Html.Events exposing (onWithOptions, onClick)
import Http
import Json.Decode as Decode
import Task
import Random
import Material
import Material.Color
import Material.Scheme
import Material.Options as Options
import Material.Chip as Chip
import Material.Menu as Menu
import Material.Grid as Grid
import Material.Button as Button
import Material.Icon as Icon
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
    | Win


type alias Guess =
    ( Combination, List Hint )


type alias Index =
    Int


type GameState
    = Playing Combination
    | GameOver
    | Surrender


type PegType
    = Draggable
    | Droppable
    | NoAction



-- Model


type alias Model =
    { correct : Combination
    , guesses : List Guess
    , state : GameState
    , hoverIndex : Index
    , dragging : Maybe Color
    , mdl : Material.Model
    }


guessSize : Int
guessSize =
    4


colors : List Color
colors =
    [ Red, Green, Blue, Cyan, Yellow ]


emptyCombination : Combination
emptyCombination =
    List.repeat guessSize Empty


createModel : ( Model, Cmd Msg )
createModel =
    let
        startingState =
            Playing emptyCombination

        guesses =
            []
    in
        Model emptyCombination guesses startingState -1 Nothing Material.model
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
    div [ style [ ( "padding", "1rem" ), ( "text-align", "center" ) ] ]
        [ h1 [ onClick Cheat ] [ text "mastermind" ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.state of
        Playing current ->
            playBody current model

        GameOver ->
            div [] [ playBody emptyCombination model ]

        _ ->
            text "I don't know any other states yet"


playBody : Combination -> Model -> Html Msg
playBody current model =
    Grid.grid
        [ Options.css "display" "flex"
        , Options.css "flex-direction" "row"
        , Options.css "justify-content" "space-around"
        ]
        [ colorPicker model
        , guessField current model
        ]


submitButton : Model -> Html Msg
submitButton model =
    let
        mdlColor =
            Material.Color.color Material.Color.Green Material.Color.S500
    in
        Button.render Mdl
            [ 0 ]
            model.mdl
            [ Material.Color.background mdlColor
            , Button.fab
            , Button.ripple
            , if not (validCurrentGuess model) then
                Button.disabled
              else
                Elevation.e16
            , Options.onClick SubmitGuess
            , Options.css "margin" "0 0 35 35"
            ]
            [ Icon.i "compare_arrows" ]


resetButton : Model -> Html Msg
resetButton model =
    let
        mdlColor =
            Material.Color.color Material.Color.Green Material.Color.S500
    in
        Button.render Mdl
            [ 0 ]
            model.mdl
            [ Material.Color.background mdlColor
            , Button.fab
            , Button.ripple
            , Elevation.e16
            , Options.onClick Reset
            , Options.css "margin" "0 0 35 35"
            ]
            [ Icon.i "restore" ]


validCurrentGuess : Model -> Bool
validCurrentGuess model =
    case model.state of
        Playing currentGuess ->
            let
                empties =
                    List.filter (\x -> x == Empty) currentGuess
            in
                if List.length empties > 0 then
                    False
                else
                    True

        _ ->
            False


guessField : Combination -> Model -> Grid.Cell Msg
guessField current model =
    let
        currentGuess =
            List.indexedMap (peg Droppable -1) current

        pastGuesses =
            List.map renderPastGuess model.guesses

        button =
            case model.state of
                GameOver ->
                    resetButton model

                _ ->
                    submitButton model
    in
        Grid.cell
            [ Grid.size Grid.All 6 ]
            [ div [] <| List.append currentGuess [ button ]
            , div [] <| pastGuesses
            ]


renderPastGuess : Guess -> Html Msg
renderPastGuess ( guess, hint ) =
    let
        pegs =
            List.indexedMap (peg NoAction -1) guess
    in
        List.map renderHint hint
            |> (::) (Icon.i "keyboard_arrow_right")
            |> List.append pegs
            |> div []


renderHint : Hint -> Html Msg
renderHint hint =
    case hint of
        CorrectPosition ->
            Icon.i "lock_open"

        WrongPosition ->
            Icon.i "lock"

        Win ->
            Icon.i "vpn_key"


colorPicker : Model -> Grid.Cell Msg
colorPicker model =
    Grid.cell
        [ Grid.size Grid.All 1 ]
    <|
        List.indexedMap (peg Draggable model.hoverIndex) colors


peg : PegType -> Index -> Index -> Color -> Html Msg
peg pegType hoverIndex index color =
    let
        mdlColor =
            Material.Color.color (colorToMDLColor color) Material.Color.S300
    in
        case pegType of
            Draggable ->
                Chip.span
                    [ Options.attribute <| draggable "true"
                    , onDragStart (Dragging color)
                    , Material.Color.background mdlColor
                    , Options.css "height" "33px"
                    , Options.css "width" "9px"
                    , Options.css "border-radius" "1.5rem"
                    , Options.css "margin" "8"
                    , Options.css "border" "3px solid grey"
                    , if index == hoverIndex then
                        Elevation.e2
                      else
                        Elevation.e8
                    , Elevation.transition 300
                    , Options.onMouseEnter (Raise index)
                    , Options.onMouseLeave (Raise -1)
                    ]
                    []

            Droppable ->
                Chip.span
                    [ Options.attribute <| attribute "ondragover" "return false"
                    , onDrop (DroppedOn index)
                    , Material.Color.background mdlColor
                    , Elevation.e4
                    , Options.css "height" "33px"
                    , Options.css "width" "9px"
                    , Options.css "border-radius" "1.5rem"
                    , Options.css "margin" "8"
                    ]
                    []

            NoAction ->
                Chip.span
                    [ Material.Color.background mdlColor
                    , Options.css "height" "33px"
                    , Options.css "width" "9px"
                    , Options.css "border-radius" "1.5rem"
                    , Options.css "margin" "8"
                    , Elevation.e2
                    ]
                    []


renderMenuItem : Color -> Menu.Item Msg
renderMenuItem color =
    Menu.item
        [ Menu.onSelect NoOp ]
        [ text <| toString color ]


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
            Material.Color.BlueGrey



-- Update


type Msg
    = NoOp
    | Mdl (Material.Msg Msg)
    | Shuffle (List Int)
    | ShuffleWeb (Result Http.Error (List Int))
    | Dragging Color
    | DroppedOn Index
    | Raise Int
    | SubmitGuess
    | Cheat
    | Reset


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
            updateHoverIndex index model ! []

        Dragging color ->
            let
                _ =
                    Debug.log "Dragging a " color
            in
                { model | dragging = Just color } ! []

        DroppedOn index ->
            let
                _ =
                    Debug.log "Dropped on index " index
            in
                updateCurrentGuess index model ! []

        SubmitGuess ->
            case model.state of
                Playing currentGuess ->
                    if currentGuess == model.correct then
                        win currentGuess model ! []
                    else
                        submitGuess currentGuess model ! []

                _ ->
                    model ! []

        Cheat ->
            let
                _ =
                    Debug.log "Answer" model.correct
            in
                { model | state = Playing model.correct } ! []

        Reset ->
            createModel


submitGuess : Combination -> Model -> Model
submitGuess currentGuess model =
    let
        _ =
            Debug.log "Submitting guess for evaluation" currentGuess

        correctPositions =
            List.map2 (,) currentGuess model.correct
                |> List.filter (\( a, b ) -> a == b)
                |> List.length
                |> flip List.repeat CorrectPosition

        wrongPositions =
            List.filter (\a -> List.member a model.correct) currentGuess
                |> List.length
                |> flip (-) (List.length correctPositions)
                |> flip List.repeat WrongPosition

        hint =
            List.append correctPositions wrongPositions

        newGuesses =
            ( currentGuess, hint ) :: model.guesses
    in
        { model
            | guesses = newGuesses
            , state = Playing emptyCombination
        }


win : Combination -> Model -> Model
win guess model =
    let
        guesses =
            ( guess, [ Win ] ) :: model.guesses
    in
        { model
            | guesses = guesses
            , state = GameOver
        }


updateCurrentGuess : Index -> Model -> Model
updateCurrentGuess index model =
    case model.state of
        Playing currentGuess ->
            let
                updateGuess i c =
                    if i == index then
                        Maybe.withDefault Empty model.dragging
                    else
                        c

                newGuess =
                    List.indexedMap updateGuess currentGuess
            in
                { model | state = Playing newGuess }

        _ ->
            model


updateHoverIndex : Int -> Model -> Model
updateHoverIndex index model =
    { model | hoverIndex = index }


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


onDragStart : msg -> Options.Property c msg
onDragStart message =
    onDragHelper "dragstart" message



-- when another dragged element is dropped over this element


onDrop : msg -> Options.Property c msg
onDrop message =
    onPreventHelper "drop" message



-- helpers


onDragHelper : String -> msg -> Options.Property c msg
onDragHelper eventName message =
    Options.onWithOptions
        eventName
        { preventDefault = False
        , stopPropagation = False
        }
        (Decode.succeed message)


onPreventHelper : String -> msg -> Options.Property c msg
onPreventHelper eventName message =
    Options.onWithOptions
        eventName
        { preventDefault = True
        , stopPropagation = False
        }
        (Decode.succeed message)
