module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import CellGrid exposing (CellGrid)
import CellGrid.Render exposing (CellRenderer)
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font exposing (wordSpacing)
import Element.Input as Input
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Random
import Time exposing (Posix)
import TypedSvg.Types exposing (Fill(..))
import WorldGrid exposing (State(..))


type alias WorldChange =
    { crops : Int
    , cities : Int
    , nature : Int
    }


tickInterval : Float
tickInterval =
    5000


initialDensity =
    0.3


initialSeed =
    3771


gridWidth =
    6


gridDisplayWidth =
    450.0


lowDensityThreshold =
    0.0


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    , counter : Int
    , trial : Int
    , appState : AppState
    , density : Float
    , densityString : String
    , currentDensity : Float
    , seed : Int
    , seedString : String
    , randomPair : ( Int, Int )
    , cellMap : CellGrid State
    , message : String
    , chosenState : State
    , worldChange : WorldChange
    }


type AppState
    = Ready
    | Running
    | Paused



--
-- MSG
--


type Msg
    = NoOp
    | InputBeta String
    | InputSeed String
    | Step
    | Tick Posix
    | AdvanceAppState
    | Reset
    | NewPair ( Int, Int )
    | CellGrid CellGrid.Render.Msg
    | ChooseCity
    | ChooseCrop
    | ChooseNature
    | ChooseUnoccupied


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "Test"
      , output = "Test"
      , counter = 0
      , trial = 0
      , appState = Ready
      , density = initialDensity
      , densityString = String.fromFloat initialDensity
      , currentDensity = initialDensity
      , seed = initialSeed
      , seedString = String.fromInt initialSeed
      , randomPair = ( 0, 0 )
      , cellMap = initialCellGrid initialSeed initialDensity
      , message = "Click to make cell."
      , chosenState = City
      , worldChange = { crops = 0, cities = 0, nature = 0 }
      }
    , Cmd.none
    )


initialCellGrid : Int -> Float -> CellGrid State
initialCellGrid seed density =
    WorldGrid.emptyGrid gridWidth gridWidth


subscriptions model =
    Time.every tickInterval Tick


updateWordChange : Maybe State -> State -> WorldChange -> WorldChange
updateWordChange maybeSelectedCell chosenState worldChange =
    if maybeSelectedCell /= Just chosenState then
        case chosenState of
            Crop ->
                { worldChange | crops = worldChange.crops + 1 }

            City ->
                { worldChange | cities = worldChange.cities + 1 }

            Nature ->
                { worldChange | nature = worldChange.nature + 1 }

            Unoccupied ->
                worldChange

    else
        case chosenState of
            Crop ->
                { worldChange | crops = worldChange.crops - 1 }

            City ->
                { worldChange | cities = worldChange.cities - 1 }

            Nature ->
                { worldChange | nature = worldChange.nature - 1 }

            Unoccupied ->
                worldChange


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputBeta str ->
            case String.toFloat str of
                Nothing ->
                    ( { model | densityString = str }, Cmd.none )

                Just density_ ->
                    ( { model | densityString = str, density = density_ }, Cmd.none )

        InputSeed str ->
            case String.toInt str of
                Nothing ->
                    ( { model | seedString = str }, Cmd.none )

                Just seed_ ->
                    ( { model | seedString = str, seed = seed_ }, Cmd.none )

        Step ->
            ( { model | counter = model.counter + 1, cellMap = WorldGrid.updateCells model.cellMap }, Cmd.none )

        Tick t ->
            case model.appState == Running of
                True ->
                    ( { model
                        | counter = model.counter + 1
                      }
                    , Random.generate NewPair generatePair
                    )

                False ->
                    ( model, Cmd.none )

        AdvanceAppState ->
            let
                nextAppState =
                    case model.appState of
                        Ready ->
                            Running

                        Running ->
                            Paused

                        Paused ->
                            Running
            in
            ( { model | appState = nextAppState }, Cmd.none )

        Reset ->
            ( { model
                | counter = 0
                , trial = model.trial + 1
                , appState = Ready
                , cellMap = initialCellGrid (model.seed + model.trial + 1) model.density
              }
            , Cmd.none
            )

        NewPair ( i, j ) ->
            ( { model | randomPair = ( i, j ) }, Cmd.none )

        CellGrid msg_ ->
            case msg_ of
                CellGrid.Render.MouseClick ( i, j ) ( x, y ) ->
                    let
                        message =
                            "(i,j) = (" ++ String.fromInt i ++ ", " ++ String.fromInt j ++ ")"

                        maybeSelectedCell =
                            CellGrid.cellAtMatrixIndex ( i, j ) model.cellMap
                    in
                    ( { model
                        | message = message
                        , worldChange = updateWordChange maybeSelectedCell model.chosenState model.worldChange
                        , cellMap = WorldGrid.toggleState model.chosenState ( i, j ) model.cellMap
                      }
                    , Cmd.none
                    )

        ChooseCity ->
            ( { model | chosenState = City }, Cmd.none )

        ChooseCrop ->
            ( { model | chosenState = Crop }, Cmd.none )

        ChooseNature ->
            ( { model | chosenState = Nature }, Cmd.none )

        ChooseUnoccupied ->
            ( { model | chosenState = Unoccupied }, Cmd.none )


generatePair =
    Random.pair (Random.int 0 (gridWidth - 1)) (Random.int 0 (gridWidth - 1))



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [ Background.color dark ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, spacing 20 ]
            [ title "WorldGrid's Game of Life"
            , row [ centerX, spacing 40 ]
                [ CellGrid.Render.renderAsHtml gridDisplayWidth gridDisplayWidth cellrenderer model.cellMap
                    |> Element.html
                    |> Element.map CellGrid
                , palette model
                ]
            , row [ spacing 18 ]
                [ resetButton
                , runButton model
                , row [ spacing 8 ] [ stepButton, counterDisplay model ]
                , inputDensity model
                ]
            , row [ Font.size 14, centerX, spacing 24 ]
                [ el [ width (px 150), Font.color light ] (text <| "Current density = " ++ String.fromFloat model.currentDensity)
                , Element.newTabLink [ Font.size 14, centerX, Font.color <| Element.rgb 0.4 0.4 1 ]
                    { url = "https://github.com/jxxcarlson/elm-cell-grid/tree/master/examples/GameOfLife"
                    , label = el [] (text "Code on GitHub")
                    }
                , el [ width (px 100), Font.color light ] (text <| model.message)
                ]
            ]
        ]


palette : Model -> Element Msg
palette model =
    column [ spacing 10, paddingXY 40 0 ]
        [ row [ spacing 10, Font.color (Element.rgb 1 1 1) ] [ chooseCityButton model, text <| String.fromInt model.worldChange.cities ]
        , row [ spacing 10, Font.color (Element.rgb 1 1 1) ] [ chooseCropButton model, text <| String.fromInt model.worldChange.crops ]
        , row [ spacing 10, Font.color (Element.rgb 1 1 1) ] [ chooseNatureButton model, text <| String.fromInt model.worldChange.nature ]
        , chooseUnoccupiedButton model
        ]


chooseCityButton : Model -> Element Msg
chooseCityButton model =
    Input.button (paletButtonStyle 0 0 1)
        { onPress = Just ChooseCity
        , label = el [ centerX, centerY ] (text <| "City")
        }


chooseCropButton : Model -> Element Msg
chooseCropButton model =
    Input.button (paletButtonStyle 1 1 0)
        { onPress = Just ChooseCrop
        , label = el [ centerX, centerY ] (text <| "Crop")
        }


chooseNatureButton : Model -> Element Msg
chooseNatureButton model =
    Input.button (paletButtonStyle 0 0.8 0)
        { onPress = Just ChooseNature
        , label = el [ centerX, centerY ] (text <| "Nature")
        }


chooseUnoccupiedButton : Model -> Element Msg
chooseUnoccupiedButton model =
    Input.button (paletButtonStyle 0.1 0.1 0.1)
        { onPress = Just ChooseUnoccupied
        , label = el [ centerX, centerY ] (text <| "Unoccupied")
        }


paletButtonStyle r g b =
    [ Background.color (Element.rgb r g b)
    , width (px 100)
    , Font.color (Element.rgb 0.5 0.5 0.5)
    , paddingXY 8 8
    , Font.size buttonFontSize
    ]



--onMouseClick : Attribute Msg
--onMouseClick =
--    Mouse.onClick (.clientPos >> MouseClick)


currentDensity : Model -> Float
currentDensity model =
    let
        population =
            WorldGrid.numberOccupied model.cellMap |> toFloat

        capacity =
            gridWidth * gridWidth |> toFloat
    in
    population / capacity |> roundTo 2


roundTo : Int -> Float -> Float
roundTo places x =
    let
        k =
            10 ^ places |> toFloat
    in
    (round (k * x) |> toFloat) / k


cellrenderer =
    { cellSize = gridDisplayWidth / toFloat gridWidth
    , cellColorizer =
        \state ->
            case state of
                Crop ->
                    Color.rgb 1 1 0

                City ->
                    Color.rgb 0 0 1

                Nature ->
                    Color.rgb 0 1 0

                Unoccupied ->
                    Color.rgb 0 0 0
    , defaultColor = Color.rgb 0 0 0
    , gridLineWidth = 0.5
    , gridLineColor = Color.rgb 0 0 1
    }


counterDisplay : Model -> Element Msg
counterDisplay model =
    el [ Font.size 18, width (px 30), Font.color light ] (text <| String.fromInt model.counter)


title : String -> Element msg
title str =
    row [ centerX, Font.bold, Font.color light ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]


buttonFontSize =
    16


inputDensity : Model -> Element Msg
inputDensity model =
    Input.text [ width (px 60), height (px 30), Font.size buttonFontSize, Background.color light ]
        { onChange = InputBeta
        , text = model.densityString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ Font.size buttonFontSize, moveDown 8.5, Font.color light ] (text "Initial density ")
        }


inputSeed : Model -> Element Msg
inputSeed model =
    Input.text [ width (px 60), Font.size buttonFontSize ]
        { onChange = InputSeed
        , text = model.seedString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ Font.size buttonFontSize, moveDown 12 ] (text "seed ")
        }


stepButton : Element Msg
stepButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just Step
            , label = el [ centerX, centerY ] (text "Step")
            }
        ]


runButton : Model -> Element Msg
runButton model =
    row [ centerX, width (px 80) ]
        [ Input.button (buttonStyle ++ [ activeBackgroundColor model ])
            { onPress = Just AdvanceAppState
            , label = el [ centerX, centerY, width (px 60) ] (text <| appStateAsString model.appState)
            }
        ]


activeBackgroundColor model =
    case model.appState of
        Running ->
            Background.color (Element.rgb 0.65 0 0)

        _ ->
            Background.color (gray 0.2)


resetButton : Element Msg
resetButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just Reset
            , label = el [ centerX, centerY ] (text <| "Reset")
            }
        ]


appStateAsString : AppState -> String
appStateAsString appState =
    case appState of
        Ready ->
            "Ready"

        Running ->
            "Running"

        Paused ->
            "Paused"



--
-- STYLE
--


dark =
    gray 0.1


light =
    gray 0.8


gray g =
    rgb g g g


mainColumnStyle =
    [ centerX
    , centerY
    , Background.color dark
    , paddingXY 20 20
    ]


buttonStyle =
    [ Background.color (gray 0.2)
    , Font.color (gray 0.7)
    , paddingXY 15 8
    , Font.size buttonFontSize
    ]
