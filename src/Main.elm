module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , padding
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy3)
import Fraction exposing (Fraction)
import Html exposing (Html)
import Html.Attributes
import Random exposing (Generator)
import Task exposing (Task)
import Time
import Time.Extra



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type GameState
    = MainMenu
    | Started StartedModel


type alias StartedModel =
    { question : Question
    , difficulty : Difficulty
    , streak : Int
    , correct : Int
    , incorrect : Int
    , numeratorAnswer : String
    , denominatorAnswer : String
    , questionStartTime : Time.Posix
    , questionElapsedTime : Int
    }


type alias StartingModel =
    { difficulty : Difficulty
    , streak : Int
    , correct : Int
    , incorrect : Int
    , numeratorAnswer : String
    , denominatorAnswer : String
    }


startingModelAndQuestionAndTime : StartingModel -> Question -> Time.Posix -> StartedModel
startingModelAndQuestionAndTime startingModel question time =
    { question = question
    , difficulty = startingModel.difficulty
    , streak = startingModel.streak
    , correct = startingModel.correct
    , incorrect = startingModel.incorrect
    , numeratorAnswer = startingModel.numeratorAnswer
    , denominatorAnswer = startingModel.denominatorAnswer
    , questionStartTime = time
    , questionElapsedTime = 0
    }


type alias Model =
    { state : GameState
    , zone : Time.Zone
    }


initialModel : Model
initialModel =
    { state = MainMenu
    , zone = Time.utc
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Task.perform AdjustTimeZone Time.here
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 30) Tick



-- UPDATE


type Difficulty
    = Easy
    | Intermediate
    | Hard


{-| Time for each level of difficulty per fraction (in milliseconds).
-}
timeForDifficulty : Difficulty -> Int
timeForDifficulty difficulty =
    case difficulty of
        Easy ->
            25000

        Intermediate ->
            20000

        Hard ->
            15000


type Msg
    = GetNewQuestion
    | GetStartingQuestion StartingModel
    | NewQuestion Question Time.Posix
    | FirstQuestion StartingModel Question Time.Posix
    | StartGame StartingModel
    | UpdateNumeratorAnswer String
    | UpdateDenominatorAnswer String
    | SubmitCalculationAnswer Question
    | BackToMainMenu
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNewQuestion ->
            case model.state of
                MainMenu ->
                    ( model
                    , Cmd.none
                    )

                Started startedModel ->
                    ( model
                    , getQuestionAndTime
                        startedModel.difficulty
                        (\question time -> NewQuestion question time)
                    )

        GetStartingQuestion startingModel ->
            case model.state of
                Started _ ->
                    ( model
                    , Cmd.none
                    )

                MainMenu ->
                    ( model
                    , getQuestionAndTime
                        startingModel.difficulty
                        (\question time -> FirstQuestion startingModel question time)
                    )

        NewQuestion newQuestion time ->
            case model.state of
                MainMenu ->
                    ( model
                    , Cmd.none
                    )

                Started startedModel ->
                    let
                        newStartedModel =
                            { startedModel
                                | question = newQuestion
                                , questionStartTime = time
                            }
                    in
                    ( { model | state = Started newStartedModel }
                    , highlightNumeratorInput
                    )

        FirstQuestion startingModel question time ->
            case model.state of
                Started _ ->
                    ( model
                    , Cmd.none
                    )

                MainMenu ->
                    ( { model | state = Started <| startingModelAndQuestionAndTime startingModel question time }
                    , highlightNumeratorInput
                    )

        StartGame startingModel ->
            case model.state of
                MainMenu ->
                    update (GetStartingQuestion startingModel) model

                Started _ ->
                    ( model
                    , Cmd.none
                    )

        UpdateNumeratorAnswer numeratorAnswer ->
            case model.state of
                MainMenu ->
                    ( model
                    , Cmd.none
                    )

                Started startedModel ->
                    let
                        newStartedModel =
                            { startedModel | numeratorAnswer = numeratorAnswer }
                    in
                    ( { model | state = Started newStartedModel }
                    , Cmd.none
                    )

        UpdateDenominatorAnswer denominatorAnswer ->
            case model.state of
                MainMenu ->
                    ( model
                    , Cmd.none
                    )

                Started startedModel ->
                    let
                        newStartedModel =
                            { startedModel | denominatorAnswer = denominatorAnswer }
                    in
                    ( { model | state = Started newStartedModel }
                    , Cmd.none
                    )

        SubmitCalculationAnswer question ->
            case model.state of
                MainMenu ->
                    ( model
                    , Cmd.none
                    )

                Started startedModel ->
                    let
                        maybeFraction : Maybe Fraction
                        maybeFraction =
                            Maybe.map2
                                (\numerator denominator -> Fraction.create numerator denominator)
                                (String.toInt startedModel.numeratorAnswer)
                                (String.toInt startedModel.denominatorAnswer)
                                |> Maybe.andThen identity
                    in
                    case maybeFraction of
                        Just fraction ->
                            if Fraction.equal question.answer fraction then
                                let
                                    newStartedModel =
                                        { startedModel
                                            | streak = increment startedModel.streak
                                            , correct = increment startedModel.correct
                                            , numeratorAnswer = ""
                                            , denominatorAnswer = ""
                                        }
                                in
                                update
                                    GetNewQuestion
                                    { model | state = Started newStartedModel }

                            else
                                let
                                    newStartedModel =
                                        { startedModel
                                            | streak = 0
                                            , incorrect = increment startedModel.incorrect
                                            , numeratorAnswer = ""
                                            , denominatorAnswer = ""
                                        }
                                in
                                update
                                    GetNewQuestion
                                    { model | state = Started newStartedModel }

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

        BackToMainMenu ->
            init ()

        Tick newTime ->
            case model.state of
                MainMenu ->
                    ( model
                    , Cmd.none
                    )

                Started startedModel ->
                    let
                        timeAllowedPerQuestion =
                            timeForDifficulty startedModel.difficulty

                        timeDifference =
                            Time.posixToMillis newTime - Time.posixToMillis startedModel.questionStartTime

                        timeRemaining =
                            timeAllowedPerQuestion - timeDifference
                    in
                    if timeRemaining < 0 then
                        let
                            newStartedModel =
                                { startedModel
                                    | streak = 0
                                    , incorrect = increment startedModel.incorrect
                                    , numeratorAnswer = ""
                                    , denominatorAnswer = ""
                                }
                        in
                        update
                            GetNewQuestion
                            { model | state = Started newStartedModel }

                    else
                        let
                            newStartedModel =
                                { startedModel | questionElapsedTime = timeRemaining }
                        in
                        ( { model | state = Started newStartedModel }
                        , Cmd.none
                        )

        AdjustTimeZone newTimeZone ->
            ( { model | zone = newTimeZone }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


getQuestionAndTime : Difficulty -> (Question -> Time.Posix -> Msg) -> Cmd Msg
getQuestionAndTime difficulty f =
    Task.perform identity
        (Task.map2
            f
            (questionGenerator difficulty |> generatorToTask)
            Time.now
        )


highlightNumeratorInput : Cmd Msg
highlightNumeratorInput =
    Task.attempt (\_ -> NoOp) (Dom.focus numeratorInputId)



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        []
        (case model.state of
            MainMenu ->
                mainMenuView

            Started gameModel ->
                gameStartedView model.zone gameModel
        )


mainMenuView : Element Msg
mainMenuView =
    column
        [ centerX
        , centerY
        ]
        [ row
            [ centerX
            , spacing 20
            , padding 10
            ]
            [ el
                [ Font.size 64
                , Font.family
                    [ Font.sansSerif
                    ]
                ]
                (text "Quickflash")
            ]
        , row
            [ centerX
            , spacing 20
            , padding 10
            ]
            [ setDifficultyButton Easy
            , setDifficultyButton Intermediate
            , setDifficultyButton Hard
            ]
        ]


gameStartedView : Time.Zone -> StartedModel -> Element Msg
gameStartedView zone model =
    column
        [ centerX
        , centerY
        ]
        [ row
            [ centerX
            , centerY
            , width fill
            , padding 10
            ]
            [ Input.button
                [ Background.color elmOrange
                , padding 10
                , Border.rounded 4
                , Element.alignRight
                ]
                { onPress = Just BackToMainMenu
                , label = text "Back to Main Menu"
                }
            ]
        , lazy3 scoreTrackerView model.correct model.incorrect model.streak
        , row
            [ Element.alignLeft
            , centerY
            , padding 10
            ]
            [ el
                [ Font.alignLeft
                ]
                (model.questionElapsedTime
                    |> Time.millisToPosix
                    |> Time.Extra.posixToParts zone
                    |> posixPartsToString
                    |> String.padLeft 4 ' '
                    |> (++) "Time remaining: "
                    |> text
                )
            ]
        , lazy3 questionView model.question model.numeratorAnswer model.denominatorAnswer
        ]


posixPartsToString : Time.Extra.Parts -> String
posixPartsToString parts =
    let
        centisecond =
            parts.millisecond // 100
    in
    String.fromInt parts.second ++ "." ++ String.fromInt centisecond


scoreTrackerView : Int -> Int -> Int -> Element msg
scoreTrackerView correct incorrect streak =
    row
        [ centerX
        , spacing 20
        , padding 10
        ]
        [ questionCounter "Correct" correct
        , questionCounter "Incorrect" incorrect
        , questionCounter "Streak" streak
        ]


questionCounter : String -> Int -> Element msg
questionCounter counterText count =
    column
        [ centerX
        , Font.center
        ]
        [ el
            [ Font.bold
            , Font.center
            ]
            (text counterText)
        , el
            [ centerX ]
            (String.fromInt count |> text)
        ]


setDifficultyButton : Difficulty -> Element Msg
setDifficultyButton difficulty =
    Input.button
        [ Background.color elmBlue
        , padding 10
        , Border.rounded 4
        ]
        { onPress =
            { difficulty = difficulty
            , streak = 0
            , correct = 0
            , incorrect = 0
            , numeratorAnswer = ""
            , denominatorAnswer = ""
            }
                |> StartGame
                |> Just
        , label = text <| difficultyToString difficulty
        }


fractionView : Attribute msg -> Fraction -> Element msg
fractionView fontSize fraction =
    let
        numeratorAndDenominatorView num =
            el
                [ width fill
                , Font.center
                , fontSize
                ]
                (num
                    |> String.fromInt
                    |> text
                )
    in
    column
        [ Element.alignRight
        , spacing 10
        ]
        [ fraction
            |> Fraction.getNumerator
            |> numeratorAndDenominatorView
        , el
            [ width fill
            , height <| px 6
            , Background.color black
            ]
            Element.none
        , fraction
            |> Fraction.getDenominator
            |> numeratorAndDenominatorView
        ]


numeratorInputId : String
numeratorInputId =
    "numerator-input"


questionView : Question -> String -> String -> Element Msg
questionView calculation numeratorAnswer denominatorAnswer =
    let
        fontSize =
            Font.size 96
    in
    column
        [ width fill ]
        [ row
            [ centerX
            , spacing 30
            , padding 50
            , Border.color elmGray
            , Border.width 2
            , Border.rounded 10
            ]
            [ fractionView fontSize calculation.fraction1
            , el
                [ fontSize ]
                (calculation.mathOperation
                    |> operationToString
                    |> text
                )
            , fractionView fontSize calculation.fraction2
            ]
        , column
            [ spacing 20
            , padding 20
            , width fill
            ]
            [ Input.text
                [ Element.htmlAttribute (Html.Attributes.id numeratorInputId)
                ]
                { onChange = UpdateNumeratorAnswer
                , text = numeratorAnswer
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Numerator")
                }
            , Input.text
                []
                { onChange = UpdateDenominatorAnswer
                , text = denominatorAnswer
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Denominator")
                }
            , Input.button
                [ width fill
                , Background.color elmGreen
                , Border.rounded 4
                , spacing 20
                , padding 20
                ]
                { onPress = Just <| SubmitCalculationAnswer calculation
                , label =
                    el
                        [ centerX
                        , Font.color white
                        ]
                        (text "Submit")
                }
            ]
        ]


type MathOperation
    = Add
    | Subtract
    | Multiply
    | Divide


mathOperationGenerator : Generator MathOperation
mathOperationGenerator =
    Random.uniform
        Add
        [ Subtract
        , Multiply
        , Divide
        ]


type alias Question =
    { fraction1 : Fraction
    , mathOperation : MathOperation
    , fraction2 : Fraction
    , answer : Fraction
    }


difficultyFractionBounds : Difficulty -> ( Int, Int )
difficultyFractionBounds difficulty =
    let
        easyBound =
            5

        intermediateBound =
            10

        hardBound =
            20

        getBounds bound =
            ( negate bound, bound )
    in
    case difficulty of
        Easy ->
            getBounds easyBound

        Intermediate ->
            getBounds intermediateBound

        Hard ->
            getBounds hardBound


fractionIntGenerator : Difficulty -> Generator Int
fractionIntGenerator difficulty =
    let
        ( lowBound, highBound ) =
            difficultyFractionBounds difficulty
    in
    Random.uniform (Random.int lowBound -1) [ Random.int 1 highBound ]
        |> Random.andThen identity


fractionGenerator : Difficulty -> Generator Fraction
fractionGenerator difficulty =
    let
        intGenerator =
            fractionIntGenerator difficulty
    in
    Random.map2
        (\numerator denominator -> Fraction.createUnsafe numerator denominator |> Fraction.simplify)
        intGenerator
        intGenerator


questionGenerator : Difficulty -> Generator Question
questionGenerator difficulty =
    let
        fracGenerator =
            fractionGenerator difficulty
    in
    Random.map3
        (\fraction1 mathOperation fraction2 ->
            Question
                fraction1
                mathOperation
                fraction2
                (getCalculationAnswer fraction1 mathOperation fraction2)
        )
        fracGenerator
        mathOperationGenerator
        fracGenerator


getCalculationAnswer : Fraction -> MathOperation -> Fraction -> Fraction
getCalculationAnswer fraction1 mathOperation fraction2 =
    let
        fractionOperationFunction =
            case mathOperation of
                Add ->
                    Fraction.add

                Subtract ->
                    Fraction.subtract

                Multiply ->
                    Fraction.multiply

                Divide ->
                    fractionUnsafeDivision
    in
    fractionOperationFunction fraction1 fraction2



-- GENERAL HELPERS


increment : Int -> Int
increment int =
    int + 1


{-| Turns a generator into a `Task` using the current time as the seed.
-}
generatorToTask : Generator a -> Task Never a
generatorToTask generator =
    Time.now
        |> Task.map (Tuple.first << Random.step generator << Random.initialSeed << Time.posixToMillis)



-- APP HELPERS


difficultyToString : Difficulty -> String
difficultyToString difficulty =
    case difficulty of
        Easy ->
            "Easy"

        Intermediate ->
            "Intermediate"

        Hard ->
            "Hard"


operationToString : MathOperation -> String
operationToString mathOperation =
    case mathOperation of
        Add ->
            "+"

        Subtract ->
            "-"

        Multiply ->
            "×"

        Divide ->
            "÷"



-- FRACTION HELPERS


fractionUnsafeReciprocal : Fraction -> Fraction
fractionUnsafeReciprocal fraction =
    Fraction.createUnsafe
        (Fraction.getDenominator fraction)
        (Fraction.getNumerator fraction)


fractionUnsafeDivision : Fraction -> Fraction -> Fraction
fractionUnsafeDivision fraction1 fraction2 =
    Fraction.multiply fraction1 <| fractionUnsafeReciprocal fraction2



-- COLOR HELPERS


white : Color
white =
    Element.rgb 1 1 1


black : Color
black =
    Element.rgb 0 0 0


elmBlue : Color
elmBlue =
    Element.rgb255 96 181 204


elmGreen : Color
elmGreen =
    Element.rgb255 127 209 59


elmOrange : Color
elmOrange =
    Element.rgb255 240 173 0


elmGray : Color
elmGray =
    Element.rgb255 90 99 120
