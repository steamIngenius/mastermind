module Main exposing (..)

import Html exposing (Html, text, div, h1, h2)
import Html.Attributes exposing (style)
import Material
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


type Msg
    = NoOp
    | Mdl (Material.Msg Msg)



-- Model


type alias Model =
    { state : Int
    , mdl : Material.Model
    }


createModel : ( Model, Cmd Msg )
createModel =
    Model 0 Material.model ! []



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
    div [] [ text "This is the body" ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
