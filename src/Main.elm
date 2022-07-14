module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Canvas exposing (Point, Renderable, Shape, path, quadraticCurveTo, shapes)
import Canvas.Settings exposing (stroke)
import Canvas.Settings.Line exposing (LineCap(..), LineJoin(..), lineCap, lineJoin, lineWidth)
import Color exposing (Color)
import Fraction exposing (Fraction, FractionCreationError(..))
import Html exposing (Html, button, div, h1, h3, input, label, p, strong, text)
import Html.Attributes exposing (class, id, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Json.Decode as Decode exposing (Decoder)
import List.Extra exposing (reverseRange)
import Ports
import Random exposing (Generator)
import Task exposing (Task)
import Time exposing (Posix)
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
    , answerValidationFeedback : Maybe AnswerFeedback
    , questionHistory : List QuestionResult
    , questionStartTime : Posix
    , questionElapsedTime : Int
    , pending : Array Renderable
    , toDraw : List Renderable
    , drawingPointer : Maybe DrawingPointer
    , color : Color
    , canvasLineSize : Int
    }


type AnswerFeedback
    = NumeratorFeedback String
    | DenominatorFeedback String
    | BothFeedback String String


type alias DrawingPointer =
    { previousMidpoint : Point
    , lastPoint : Point
    }


type alias QuestionResult =
    { question : Question
    , submittedAnswer : Maybe Fraction
    }


startingQuestionCounter : Int
startingQuestionCounter =
    0


startingNumeratorAndDenominatorAnswer : String
startingNumeratorAndDenominatorAnswer =
    ""


{-| Canvas width in pixels.
-}
canvasWidth : number
canvasWidth =
    500


{-| Canvas height in pixels.
-}
canvasHeight : number
canvasHeight =
    333


startingModelAndQuestionAndTime : Difficulty -> Question -> Posix -> StartedModel
startingModelAndQuestionAndTime difficulty question time =
    { question = question
    , difficulty = difficulty
    , streak = startingQuestionCounter
    , correct = startingQuestionCounter
    , incorrect = startingQuestionCounter
    , numeratorAnswer = startingNumeratorAndDenominatorAnswer
    , denominatorAnswer = startingNumeratorAndDenominatorAnswer
    , answerValidationFeedback = Nothing
    , questionHistory = []
    , questionStartTime = time
    , questionElapsedTime = 0
    , pending = Array.empty
    , toDraw = []
    , drawingPointer = Nothing
    , color = Color.black
    , canvasLineSize = 5
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
subscriptions model =
    case model.state of
        Started startedModel ->
            let
                createAnimationFrameEvent =
                    always AnimationFrame
            in
            case startedModel.difficulty of
                Practice ->
                    Sub.batch
                        [ Decode.field "key" Decode.string
                            |> Decode.map HandleKeyboardEvent
                            |> Events.onKeyDown
                        , Events.onAnimationFrameDelta createAnimationFrameEvent
                        ]

                _ ->
                    Sub.batch
                        [ Time.every (millisecondsPerSecond / 30) Tick
                        , Decode.field "key" Decode.string
                            |> Decode.map HandleKeyboardEvent
                            |> Events.onKeyDown
                        , Events.onAnimationFrameDelta createAnimationFrameEvent
                        ]

        MainMenu ->
            Sub.none



-- UPDATE


type Difficulty
    = Practice
    | Easy
    | Intermediate
    | Hard


{-| Time for each level of difficulty per fraction (in milliseconds).
-}
millisecondsPerQuestion : Difficulty -> Maybe Int
millisecondsPerQuestion difficulty =
    case difficulty of
        Practice ->
            Nothing

        Easy ->
            Just 55000

        Intermediate ->
            Just 40000

        Hard ->
            Just 25000


type Msg
    = StartGame Difficulty
    | FirstQuestion Difficulty Question Posix
    | GetNewQuestion
    | NewQuestion Question Posix
    | UpdateNumeratorAnswer String
    | UpdateDenominatorAnswer String
    | SubmitCalculationAnswer Question
    | BackToMainMenu
    | Tick Posix
    | AdjustTimeZone Time.Zone
    | HandleKeyboardEvent String
    | AnimationFrame
    | StartAt ( Float, Float )
    | MoveAt ( Float, Float )
    | EndAt ( Float, Float )
    | ClearCanvas
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( StartGame difficulty, MainMenu ) ->
            ( model
            , getQuestionAndTime
                difficulty
                (FirstQuestion difficulty)
            )

        ( FirstQuestion difficulty question time, MainMenu ) ->
            ( { model | state = Started <| startingModelAndQuestionAndTime difficulty question time }
            , focusNumeratorInput
            )

        ( GetNewQuestion, Started startedModel ) ->
            ( model
            , Cmd.batch
                [ getQuestionAndTime
                    startedModel.difficulty
                    NewQuestion
                , Ports.clearCanvas ()
                ]
            )

        ( NewQuestion newQuestion time, Started startedModel ) ->
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

        ( UpdateNumeratorAnswer numeratorAnswer, Started startedModel ) ->
            let
                newStartedModel =
                    { startedModel | numeratorAnswer = numeratorAnswer }
            in
            ( { model | state = Started newStartedModel }
            , Cmd.none
            )

        ( UpdateDenominatorAnswer denominatorAnswer, Started startedModel ) ->
            let
                newStartedModel =
                    { startedModel | denominatorAnswer = denominatorAnswer }
            in
            ( { model | state = Started newStartedModel }
            , Cmd.none
            )

        ( SubmitCalculationAnswer question, Started startedModel ) ->
            let
                fractionResult =
                    fractionFromAnswer (String.trim startedModel.numeratorAnswer) (String.trim startedModel.denominatorAnswer)
            in
            case fractionResult of
                Ok fraction ->
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
                                    , numeratorAnswer = startingNumeratorAndDenominatorAnswer
                                    , denominatorAnswer = startingNumeratorAndDenominatorAnswer
                                    , answerValidationFeedback = Nothing
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
                                    , numeratorAnswer = startingNumeratorAndDenominatorAnswer
                                    , denominatorAnswer = startingNumeratorAndDenominatorAnswer
                                    , answerValidationFeedback = Nothing
                                    , questionHistory = questionHistory :: startedModel.questionHistory
                                }
                        in
                        update
                            GetNewQuestion
                            { model | state = Started newStartedModel }

                Err answerFeedback ->
                    let
                        newStartedModel =
                            { startedModel
                                | answerValidationFeedback = Just answerFeedback
                            }
                    in
                    ( { model | state = Started newStartedModel }
                    , Cmd.none
                    )

        ( BackToMainMenu, Started _ ) ->
            init

        ( Tick newTime, Started startedModel ) ->
            let
                timeAllowedPerQuestion =
                    millisecondsPerQuestion startedModel.difficulty

                maybeTimeRemaining =
                    Maybe.map
                        (\x ->
                            let
                                timeDifference =
                                    Time.posixToMillis newTime - Time.posixToMillis startedModel.questionStartTime
                            in
                            x - timeDifference
                        )
                        timeAllowedPerQuestion
            in
            case maybeTimeRemaining of
                Just timeRemaining ->
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
                                    , numeratorAnswer = startingNumeratorAndDenominatorAnswer
                                    , denominatorAnswer = startingNumeratorAndDenominatorAnswer
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

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        ( AdjustTimeZone newTimeZone, _ ) ->
            ( { model | zone = newTimeZone }
            , Cmd.none
            )

        ( HandleKeyboardEvent key, Started startedModel ) ->
            if key == "Enter" then
                update
                    (SubmitCalculationAnswer startedModel.question)
                    model

            else
                ( model
                , Cmd.none
                )

        ( AnimationFrame, Started startedModel ) ->
            let
                newStartedModel =
                    startedModel
                        |> flushPendingToDraw
            in
            ( { model | state = Started newStartedModel }
            , Cmd.none
            )

        ( StartAt point, Started startedModel ) ->
            let
                newStartedModel =
                    initialPoint point startedModel
            in
            ( { model | state = Started newStartedModel }
            , Cmd.none
            )

        ( MoveAt point, Started startedModel ) ->
            let
                newStartedModel =
                    case startedModel.drawingPointer of
                        Just pointer ->
                            drawPoint point pointer startedModel

                        Nothing ->
                            startedModel
            in
            ( { model | state = Started newStartedModel }
            , Cmd.none
            )

        ( EndAt point, Started startedModel ) ->
            let
                newStartedModel =
                    case startedModel.drawingPointer of
                        Just pointer ->
                            finalPoint point pointer startedModel

                        Nothing ->
                            startedModel
            in
            ( { model | state = Started newStartedModel }
            , Cmd.none
            )

        ( ClearCanvas, Started _ ) ->
            ( model
            , Ports.clearCanvas ()
            )

        _ ->
            ( model
            , Cmd.none
            )


getQuestionAndTime : Difficulty -> (Question -> Posix -> Msg) -> Cmd Msg
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
    div
        [ class "container is-fluid" ]
        (case model.state of
            MainMenu ->
                [ h1
                    [ class "title has-text-centered" ]
                    [ text "quickfrac" ]
                , div
                    [ class "columns is-centered is-vcentered is-multiline" ]
                    [ setDifficultyButton Practice
                    , setDifficultyButton Easy
                    , setDifficultyButton Intermediate
                    , setDifficultyButton Hard
                    ]
                ]

            Started gameModel ->
                [ button
                    [ onClick BackToMainMenu
                    , class "button is-warning"
                    ]
                    [ text "Back to Main Menu" ]
                , div
                    [ class "columns" ]
                    [ lazy scratchpadView gameModel
                    , lazy2 gameStartedView model.zone gameModel
                    , lazy questionHistoryView gameModel.questionHistory
                    ]
                ]
        )


scratchpadView : StartedModel -> Html Msg
scratchpadView model =
    div
        [ class "column is-narrow is-hidden-mobile" ]
        [ h3
            [ class "title" ]
            [ text "Scratchpad" ]
        , div
            [ class "scratchpad" ]
            [ Canvas.toHtml
                ( canvasWidth, canvasHeight )
                [ style "touch-action" "none"
                , class "quickfracCanvas"
                , Mouse.onDown (.offsetPos >> StartAt)
                , Mouse.onMove (.offsetPos >> MoveAt)
                , Mouse.onUp (.offsetPos >> EndAt)
                , Mouse.onLeave (.offsetPos >> EndAt)
                , Mouse.onContextMenu (.offsetPos >> EndAt)
                , onTouch "touchstart" (touchCoordinates >> StartAt)
                , onTouch "touchmove" (touchCoordinates >> MoveAt)
                , onTouch "touchend" (touchCoordinates >> EndAt)
                ]
                model.toDraw
            ]
        , button
            [ class "button is-danger"
            , onClick ClearCanvas
            ]
            [ text "Clear Scratchpad" ]
        ]


questionHistoryView : List QuestionResult -> Html msg
questionHistoryView questions =
    Keyed.node "div"
        [ class "column is-narrow questionHistory" ]
        (reversedIndexesIndexedMapPlusOne questionHistoryIndividualView questions)


questionHistoryIndividualView : Int -> QuestionResult -> ( String, Html msg )
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
                "has-text-success"

            else
                "has-text-danger"

        indexString =
            String.fromInt number
    in
    ( indexString
    , div
        [ class "columns is-mobile is-vcentered" ]
        [ div
            [ class "column is-narrow" ]
            [ strong
                []
                [ text indexString ]
            ]
        , div
            [ class "column is-narrow" ]
            [ p
                []
                [ (fraction1 ++ " " ++ mathOperation ++ " " ++ fraction2 ++ " = " ++ actualAnswer)
                    |> text
                ]
            , p
                [ class colorForAnswer ]
                [ text submittedAnswer ]
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


gameStartedView : Time.Zone -> StartedModel -> Html Msg
gameStartedView zone model =
    div
        [ class "column is-narrow is-half-desktop" ]
        [ lazy difficultyHeader model.difficulty
        , lazy3 scoreTrackerView model.correct model.incorrect model.streak
        , case model.difficulty of
            Practice ->
                emptyNode

            _ ->
                p
                    [ class "content" ]
                    [ strong
                        []
                        [ text "Time remaining: " ]
                    , model.questionElapsedTime
                        |> Time.millisToPosix
                        |> Time.Extra.posixToParts zone
                        |> posixPartsToString
                        |> text
                    ]
        , lazy4 questionView model.question model.numeratorAnswer model.denominatorAnswer model.answerValidationFeedback
        ]


difficultyHeader : Difficulty -> Html msg
difficultyHeader difficulty =
    h3
        [ class "title" ]
        [ difficulty
            |> difficultyToString
            |> text
        ]


posixPartsToString : Time.Extra.Parts -> String
posixPartsToString parts =
    let
        centisecond =
            parts.millisecond // 100
    in
    String.fromInt parts.second ++ "." ++ String.fromInt centisecond


scoreTrackerView : Int -> Int -> Int -> Html msg
scoreTrackerView correct incorrect streak =
    div
        [ class "level is-mobile" ]
        [ div
            [ class "level-item" ]
            [ questionCounter "Correct" correct ]
        , div
            [ class "level-item" ]
            [ questionCounter "Incorrect" incorrect ]
        , div
            [ class "level-item" ]
            [ questionCounter "Streak" streak ]
        ]


questionCounter : String -> Int -> Html msg
questionCounter counterText count =
    div
        [ class "has-text-centered" ]
        [ strong
            []
            [ text counterText ]
        , p
            []
            [ count
                |> String.fromInt
                |> text
            ]
        ]


setDifficultyButton : Difficulty -> Html Msg
setDifficultyButton difficulty =
    div
        [ class "column is-narrow has-text-centered is-full-mobile" ]
        [ button
            [ class <| "button difficultyButton " ++ difficultyToButtonColor difficulty
            , onClick <| StartGame difficulty
            ]
            [ difficulty
                |> difficultyToString
                |> text
            ]
        ]


fractionView : Fraction -> Html msg
fractionView fraction =
    let
        numeratorAndDenominatorView num =
            p
                [ class "has-text-weight-bold has-text-centered is-size-1" ]
                [ num
                    |> String.fromInt
                    |> text
                ]
    in
    div
        [ class "column is-narrow" ]
        [ fraction
            |> Fraction.getNumerator
            |> numeratorAndDenominatorView
        , div [ class "fractionLine" ] []
        , fraction
            |> Fraction.getDenominator
            |> numeratorAndDenominatorView
        ]


numeratorInputId : String
numeratorInputId =
    "numerator-input"


feedbackToHtml : String -> Html msg
feedbackToHtml feedback =
    p
        [ class "help is-danger" ]
        [ text feedback ]


questionView : Question -> String -> String -> Maybe AnswerFeedback -> Html Msg
questionView question numeratorAnswer denominatorAnswer answerValidationFeedback =
    div
        []
        [ div
            [ class "columns is-centered" ]
            [ div
                [ class "column is-narrow" ]
                [ div
                    [ class "columns is-mobile is-centered is-vcentered" ]
                    [ fractionView question.fraction1
                    , div
                        [ class "column is-narrow" ]
                        [ strong
                            [ class "is-size-1" ]
                            [ question.mathOperation
                                |> mathOperationToString
                                |> text
                            ]
                        ]
                    , fractionView question.fraction2
                    ]
                ]
            ]
        , div
            [ class "field" ]
            [ label
                [ class "label" ]
                [ text "Numerator" ]
            , div
                [ class "control" ]
                [ input
                    [ class "input is-medium"
                    , type_ "text"
                    , onInput UpdateNumeratorAnswer
                    , value numeratorAnswer
                    , id numeratorInputId
                    ]
                    []
                , case answerValidationFeedback of
                    Just (NumeratorFeedback feedback) ->
                        feedbackToHtml feedback

                    Just (BothFeedback feedback _) ->
                        feedbackToHtml feedback

                    _ ->
                        emptyNode
                ]
            ]
        , div
            [ class "field" ]
            [ label
                [ class "label" ]
                [ text "Denominator" ]
            , div
                [ class "control" ]
                [ input
                    [ class "input is-medium"
                    , type_ "text"
                    , onInput UpdateDenominatorAnswer
                    , value denominatorAnswer
                    ]
                    []
                , case answerValidationFeedback of
                    Just (DenominatorFeedback feedback) ->
                        feedbackToHtml feedback

                    Just (BothFeedback _ feedback) ->
                        feedbackToHtml feedback

                    _ ->
                        emptyNode
                ]
            ]
        , div
            [ class "field" ]
            [ div
                [ class "control is-expanded" ]
                [ button
                    [ onClick (SubmitCalculationAnswer question)
                    , class "button is-success"
                    ]
                    [ text "Submit" ]
                ]
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
                Practice ->
                    5

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


{-| The same as `List.indexedMap`, except that the indexes are reversed and start at one instead of zero.
-}
reversedIndexesIndexedMapPlusOne : (Int -> a -> b) -> List a -> List b
reversedIndexesIndexedMapPlusOne f xs =
    List.map2 f (reverseRange (List.length xs) 1) xs



-- TIME HELPERS


millisecondsPerSecond : number
millisecondsPerSecond =
    1000



-- APP HELPERS


difficultyToString : Difficulty -> String
difficultyToString difficulty =
    case difficulty of
        Practice ->
            "Practice"

        Easy ->
            "Easy"

        Intermediate ->
            "Intermediate"

        Hard ->
            "Hard"


difficultyToButtonColor : Difficulty -> String
difficultyToButtonColor difficulty =
    case difficulty of
        Practice ->
            "is-info"

        Easy ->
            "is-success"

        Intermediate ->
            "is-warning"

        Hard ->
            "is-danger"


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


fractionCreationErrorToAnswerFeedback : FractionCreationError -> AnswerFeedback
fractionCreationErrorToAnswerFeedback error =
    case error of
        NumeratorError err ->
            NumeratorFeedback err

        DenominatorError err ->
            DenominatorFeedback err


fractionFromAnswer : String -> String -> Result AnswerFeedback Fraction
fractionFromAnswer numeratorStr denominatorStr =
    let
        denominatorErr =
            "Denominator is not an integer."

        numeratorErr =
            "Numerator is not an integer."
    in
    case ( String.toInt numeratorStr, String.toInt denominatorStr ) of
        ( Just numerator, Just denominator ) ->
            Fraction.createWithFeedback numerator denominator
                |> Result.mapError fractionCreationErrorToAnswerFeedback

        ( Just _, Nothing ) ->
            Err <| DenominatorFeedback denominatorErr

        ( Nothing, Just _ ) ->
            Err <| NumeratorFeedback numeratorErr

        ( Nothing, Nothing ) ->
            Err <| BothFeedback numeratorErr denominatorErr



-- FRACTION HELPERS


fractionUnsafeReciprocal : Fraction -> Fraction
fractionUnsafeReciprocal fraction =
    Fraction.createUnsafe
        (Fraction.getDenominator fraction)
        (Fraction.getNumerator fraction)


fractionUnsafeDivision : Fraction -> Fraction -> Fraction
fractionUnsafeDivision fraction1 fraction2 =
    Fraction.multiply fraction1 <| fractionUnsafeReciprocal fraction2



-- HTML HELPERS


emptyNode : Html msg
emptyNode =
    text ""



-- CANVAS HELPERS


touchCoordinates : { event : Touch.Event, targetOffset : ( Float, Float ) } -> ( Float, Float )
touchCoordinates { event, targetOffset } =
    List.head event.changedTouches
        |> Maybe.map
            (\touch ->
                let
                    ( x, y ) =
                        touch.pagePos

                    ( x2, y2 ) =
                        targetOffset
                in
                ( x - x2, y - y2 )
            )
        |> Maybe.withDefault ( 0, 0 )


onTouch : String -> ({ event : Touch.Event, targetOffset : Point } -> msg) -> Html.Attribute msg
onTouch event tag =
    eventDecoder
        |> Decode.map
            (\ev ->
                { message = tag ev
                , preventDefault = True
                , stopPropagation = True
                }
            )
        |> Html.Events.custom event


eventDecoder : Decoder { event : Touch.Event, targetOffset : Point }
eventDecoder =
    Decode.map2
        (\event offset ->
            { event = event
            , targetOffset = offset
            }
        )
        Touch.eventDecoder
        offsetDecoder


offsetDecoder : Decoder Point
offsetDecoder =
    Decode.field "target"
        (Decode.map2 (\top left -> ( left, top ))
            (Decode.field "offsetTop" Decode.float)
            (Decode.field "offsetLeft" Decode.float)
        )


flushPendingToDraw : StartedModel -> StartedModel
flushPendingToDraw model =
    { model
        | pending = Array.empty
        , toDraw = Array.toList model.pending
    }


initialPoint : ( Float, Float ) -> StartedModel -> StartedModel
initialPoint ( x, y ) model =
    let
        newDrawingPointer =
            Just <| DrawingPointer ( x, y ) ( x, y )
    in
    { model
        | drawingPointer = newDrawingPointer
    }


drawPoint : Point -> DrawingPointer -> StartedModel -> StartedModel
drawPoint newPoint { previousMidpoint, lastPoint } ({ pending } as model) =
    let
        newMidPoint =
            controlPoint lastPoint newPoint
    in
    { model
        | drawingPointer = Just { previousMidpoint = newMidPoint, lastPoint = newPoint }
        , pending =
            Array.push
                (drawLine model
                    [ path previousMidpoint [ quadraticCurveTo lastPoint newMidPoint ] ]
                )
                pending
    }


finalPoint : Point -> DrawingPointer -> StartedModel -> StartedModel
finalPoint point { previousMidpoint, lastPoint } ({ pending } as model) =
    { model
        | drawingPointer = Nothing
        , pending =
            Array.push
                (drawLine model
                    [ path previousMidpoint [ quadraticCurveTo lastPoint point ] ]
                )
                pending
    }


controlPoint : Point -> Point -> Point
controlPoint ( x1, y1 ) ( x2, y2 ) =
    ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )


drawLine : StartedModel -> List Shape -> Renderable
drawLine { color, canvasLineSize } line =
    line
        |> shapes
            [ lineCap RoundCap
            , lineJoin RoundJoin
            , lineWidth (toFloat canvasLineSize)
            , stroke color
            ]
