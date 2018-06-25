module TimeEditor
    exposing
        ( TimeEditor
        , TimeEvent(..)
        , Settings
        , Msg
        , init
        , defaultSettings
        , view
        , update
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


type TimeEditor
    = TimeEditor Model


type alias Settings =
    { use24Hours : Bool
    , disabled : Bool
    , inputId : Maybe String
    }


type alias Model =
    { value : String
    }


init : TimeEditor
init =
    TimeEditor
        { value = ""
        }


defaultSettings : Settings
defaultSettings =
    { use24Hours = False
    , disabled = False
    , inputId = Nothing
    }


type alias Time =
    { hours : Int
    , minutes : Int
    }


type Segment
    = Hours
    | Minutes
    | AmPm


type TimeEvent
    = NoChange
    | Changed (Maybe Time)


type Msg
    = NoOp
    | Clear
    | Input String


view : Settings -> TimeEditor -> Html Msg
view settings (TimeEditor model) =
    let
        attrList =
            [ disabled settings.disabled
            , onInput Input
            , style
                [ ( "text-indent", "-9999em" )
                , ( "cursor", "default" )
                ]
            , value ""
            ]
                ++ (case settings.inputId of
                        Nothing ->
                            []

                        Just idStr ->
                            [ id idStr ]
                   )
    in
        div
            [ class "time-editor clear-button-wrapper"
            ]
            [ input attrList []
            , div
                [ style
                    [ ( "font-family", "monospace" )
                    , ( "position", "absolute" )
                    , ( "top", "0.4em" )
                    , ( "left", "0.6em" )
                    , ( "cursor", "default" )
                    ]
                ]
                [ text <| parseValue settings.use24Hours "" ]
            , if model.value /= "" then
                button
                    [ onClick Clear
                    , title "Clear"
                    ]
                    [ i [ class "fa fa-times" ] [] ]
              else
                text ""
            ]


update : Msg -> TimeEditor -> ( TimeEditor, TimeEvent )
update msg (TimeEditor model) =
    case msg of
        NoOp ->
            ( TimeEditor model, NoChange )

        Input str ->
            ( TimeEditor { model | value = str }, NoChange )

        Clear ->
            ( TimeEditor { model | value = "" }, Changed Nothing )


parseValue : Bool -> String -> String
parseValue use24Hours value =
    if use24Hours then
        "--:--"
    else
        "--:-- --"
