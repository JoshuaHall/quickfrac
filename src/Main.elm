module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
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
        , maximum
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
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Fraction exposing (Fraction)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Random exposing (Generator)
import Task exposing (Task)
import Time
import Time.Extra



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initWithFlags
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
    , answerValidationFeedback : String
    , questionHistory : List QuestionHistory
    , questionStartTime : Time.Posix
    , questionElapsedTime : Int
    }


type alias QuestionHistory =
    { question : Question
    , submittedAnswer : Maybe Fraction
    }


startingQuestionCounter : Int
startingQuestionCounter =
    0


startingModelAndQuestionAndTime : Difficulty -> Question -> Time.Posix -> StartedModel
startingModelAndQuestionAndTime difficulty question time =
    { question = question
    , difficulty = difficulty
    , streak = startingQuestionCounter
    , correct = startingQuestionCounter
    , incorrect = startingQuestionCounter
    , numeratorAnswer = ""
    , denominatorAnswer = ""
    , answerValidationFeedback = ""
    , questionHistory = []
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


initWithFlags : flags -> ( Model, Cmd Msg )
initWithFlags _ =
    init


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.perform AdjustTimeZone Time.here
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (millisecondsPerSecond / 30) Tick
        , Decode.field "key" Decode.string
            |> Decode.map HandleKeyboardEvent
            |> Events.onKeyDown
        ]



-- UPDATE


type Difficulty
    = Easy
    | Intermediate
    | Hard


{-| Time for each level of difficulty per fraction (in milliseconds).
-}
millisecondsPerQuestion : Difficulty -> Int
millisecondsPerQuestion difficulty =
    let
        secondsPerQuestion =
            case difficulty of
                Easy ->
                    30

                Intermediate ->
                    25

                Hard ->
                    20
    in
    secondsPerQuestion * millisecondsPerSecond


type Msg
    = StartGame Difficulty
    | SubmitCalculationAnswer Question
    | BackToMainMenu
    | HandleKeyboardEvent String
    | UpdateNumeratorAnswer String
    | UpdateDenominatorAnswer String
    | GetNewQuestion
    | FirstQuestion Difficulty Question Time.Posix
    | NewQuestion Question Time.Posix
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
                        NewQuestion
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
                    , focusNumeratorInput
                    )

        FirstQuestion difficulty question time ->
            case model.state of
                Started _ ->
                    ( model
                    , Cmd.none
                    )

                MainMenu ->
                    ( { model | state = Started <| startingModelAndQuestionAndTime difficulty question time }
                    , focusNumeratorInput
                    )

        StartGame difficulty ->
            case model.state of
                Started _ ->
                    ( model
                    , Cmd.none
                    )

                MainMenu ->
                    ( model
                    , getQuestionAndTime
                        difficulty
                        (FirstQuestion difficulty)
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
                                Fraction.create
                                (String.toInt startedModel.numeratorAnswer)
                                (String.toInt startedModel.denominatorAnswer)
                                |> Maybe.andThen identity
                    in
                    case maybeFraction of
                        Just fraction ->
                            if Fraction.equal question.answer fraction then
                                let
                                    questionHistory =
                                        { question = startedModel.question
                                        , submittedAnswer = Just fraction
                                        }

                                    newStartedModel =
                                        { startedModel
                                            | streak = increment startedModel.streak
                                            , correct = increment startedModel.correct
                                            , numeratorAnswer = ""
                                            , denominatorAnswer = ""
                                            , answerValidationFeedback = ""
                                            , questionHistory = questionHistory :: startedModel.questionHistory
                                        }
                                in
                                update
                                    GetNewQuestion
                                    { model | state = Started newStartedModel }

                            else
                                let
                                    questionHistory =
                                        { question = startedModel.question
                                        , submittedAnswer = Just fraction
                                        }

                                    newStartedModel =
                                        { startedModel
                                            | streak = startingQuestionCounter
                                            , incorrect = increment startedModel.incorrect
                                            , numeratorAnswer = ""
                                            , denominatorAnswer = ""
                                            , answerValidationFeedback = ""
                                            , questionHistory = questionHistory :: startedModel.questionHistory
                                        }
                                in
                                update
                                    GetNewQuestion
                                    { model | state = Started newStartedModel }

                        Nothing ->
                            let
                                newStartedModel =
                                    { startedModel
                                        | answerValidationFeedback = "Invalid Fraction input. Check your input and try again."
                                    }
                            in
                            ( { model | state = Started newStartedModel }
                            , Cmd.none
                            )

        BackToMainMenu ->
            init

        Tick newTime ->
            case model.state of
                MainMenu ->
                    ( model
                    , Cmd.none
                    )

                Started startedModel ->
                    let
                        timeAllowedPerQuestion =
                            millisecondsPerQuestion startedModel.difficulty

                        timeDifference =
                            Time.posixToMillis newTime - Time.posixToMillis startedModel.questionStartTime

                        timeRemaining =
                            timeAllowedPerQuestion - timeDifference
                    in
                    if timeRemaining < 0 then
                        let
                            questionHistory =
                                { question = startedModel.question
                                , submittedAnswer = Nothing
                                }

                            newStartedModel =
                                { startedModel
                                    | streak = startingQuestionCounter
                                    , incorrect = increment startedModel.incorrect
                                    , numeratorAnswer = ""
                                    , denominatorAnswer = ""
                                    , questionHistory = questionHistory :: startedModel.questionHistory
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

        HandleKeyboardEvent key ->
            case model.state of
                MainMenu ->
                    ( model
                    , Cmd.none
                    )

                Started startedModel ->
                    if key == "Enter" then
                        update
                            (SubmitCalculationAnswer startedModel.question)
                            model

                    else
                        ( model
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


focusNumeratorInput : Cmd Msg
focusNumeratorInput =
    Task.attempt
        (\_ -> NoOp)
        (Dom.focus numeratorInputId)



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ centerX
        , centerY
        , width fill
        , height fill
        ]
        (case model.state of
            MainMenu ->
                mainMenuView

            Started gameModel ->
                row
                    [ centerX
                    , centerY
                    , width (fill |> maximum 1260)
                    , height fill
                    ]
                    [ el
                        [ width fill ]
                        Element.none
                    , lazy2 gameStartedView model.zone gameModel
                    , lazy questionHistoryView gameModel.questionHistory
                    ]
        )


questionHistoryView : List QuestionHistory -> Element msg
questionHistoryView questions =
    Keyed.column
        [ Element.scrollbarY
        , padding 10
        , spacing 10
        , width (fill |> maximum 450)
        , height (fill |> maximum 750)
        ]
        (questions
            |> reversedIndexesIndexedMapPlusOne questionHistoryIndividualView
        )


questionHistoryIndividualView : Int -> QuestionHistory -> ( String, Element msg )
questionHistoryIndividualView number history =
    let
        fraction1 =
            fractionToSimpleString history.question.fraction1

        mathOperation =
            mathOperationToString history.question.mathOperation

        fraction2 =
            fractionToSimpleString history.question.fraction2

        actualAnswer =
            fractionToSimpleString history.question.answer

        submittedAnswer =
            case history.submittedAnswer of
                Just userAnswer ->
                    fractionToSimpleString userAnswer

                Nothing ->
                    "Time limit reached"

        correct =
            case history.submittedAnswer of
                Just validAnswer ->
                    Fraction.equal history.question.answer validAnswer

                Nothing ->
                    False

        colorForAnswer =
            if correct then
                bootstrapGreen

            else
                bootstrapRed

        borderWidth =
            2

        indexString =
            number
                |> String.fromInt
    in
    ( indexString
    , row
        [ centerY
        , Border.widthEach
            { bottom = borderWidth
            , left = borderWidth
            , right = 0
            , top = 0
            }
        , Border.color elmGray
        , Border.solid
        , spacing 10
        , padding 5
        ]
        [ el
            [ Font.bold ]
            (text indexString)
        , column
            [ spacing 5 ]
            [ (fraction1 ++ " " ++ mathOperation ++ " " ++ fraction2 ++ " = " ++ actualAnswer)
                |> text
            , el
                [ Font.color colorForAnswer ]
                (text submittedAnswer)
            ]
        ]
    )


fractionToSimpleString : Fraction -> String
fractionToSimpleString fraction =
    let
        numerator =
            Fraction.getNumerator fraction

        denominator =
            Fraction.getDenominator fraction
    in
    "(" ++ String.fromInt numerator ++ " / " ++ String.fromInt denominator ++ ")"


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
                (text "Quickfrac")
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
                , label =
                    el
                        [ centerX
                        , Font.color white
                        ]
                        (text "Back to Main Menu")
                }
            ]
        , lazy3 scoreTrackerView model.correct model.incorrect model.streak
        , el
            [ Font.alignLeft
            , padding 10
            ]
            (model.questionElapsedTime
                |> Time.millisToPosix
                |> Time.Extra.posixToParts zone
                |> posixPartsToString
                |> String.padLeft 4 ' '
                |> (++) "Time remaining: "
                |> text
            )
        , lazy4 questionView model.question model.numeratorAnswer model.denominatorAnswer model.answerValidationFeedback
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
            (count
                |> String.fromInt
                |> text
            )
        ]


setDifficultyButton : Difficulty -> Element Msg
setDifficultyButton difficulty =
    Input.button
        [ Background.color elmBlue
        , padding 10
        , Border.rounded 4
        ]
        { onPress =
            difficulty
                |> StartGame
                |> Just
        , label =
            el
                [ centerX
                , Font.color white
                ]
                (difficulty
                    |> difficultyToString
                    |> text
                )
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
        [ spacing 10
        , centerX
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


questionView : Question -> String -> String -> String -> Element Msg
questionView question numeratorAnswer denominatorAnswer answerValidationFeedback =
    let
        fontSize =
            Font.size 96
    in
    column
        [ width fill
        , centerX
        ]
        [ row
            [ centerX
            , spacing 30
            , padding 30
            , Border.color elmGray
            , Border.width 2
            , Border.rounded 10
            , width (px 450)
            ]
            [ fractionView fontSize question.fraction1
            , el
                [ fontSize
                , centerX
                ]
                (question.mathOperation
                    |> mathOperationToString
                    |> text
                )
            , fractionView fontSize question.fraction2
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
            , if String.isEmpty answerValidationFeedback then
                Element.none

              else
                el
                    [ Font.color bootstrapRed ]
                    (text answerValidationFeedback)
            , Input.button
                [ width fill
                , Background.color elmGreen
                , Border.rounded 4
                , spacing 20
                , padding 20
                ]
                { onPress =
                    question
                        |> SubmitCalculationAnswer
                        |> Just
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
        bound =
            case difficulty of
                Easy ->
                    5

                Intermediate ->
                    10

                Hard ->
                    20
    in
    ( negate bound, bound )


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
        |> Fraction.simplify



-- GENERAL HELPERS


increment : Int -> Int
increment =
    (+) 1


{-| Turns a generator into a `Task` using the current time as the seed.
-}
generatorToTask : Generator a -> Task Never a
generatorToTask generator =
    Time.now
        |> Task.map (Tuple.first << Random.step generator << Random.initialSeed << Time.posixToMillis)


{-| The same as `List.indexedMap`, except that the indexes are reversed and incremented by one.
-}
reversedIndexesIndexedMapPlusOne : (Int -> a -> b) -> List a -> List b
reversedIndexesIndexedMapPlusOne f xs =
    List.map2 f (reverseRange (List.length xs) 1) xs


reverseRange : Int -> Int -> List Int
reverseRange =
    reverseRangeHelp []


reverseRangeHelp : List Int -> Int -> Int -> List Int
reverseRangeHelp list hi lo =
    if hi >= lo then
        reverseRangeHelp (lo :: list) hi (increment lo)

    else
        list



-- TIME HELPERS


millisecondsPerSecond : number
millisecondsPerSecond =
    1000



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


mathOperationToString : MathOperation -> String
mathOperationToString mathOperation =
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


bootstrapGreen : Color
bootstrapGreen =
    Element.rgb255 92 184 92


bootstrapRed : Color
bootstrapRed =
    Element.rgb255 217 83 79
