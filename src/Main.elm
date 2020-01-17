module Main exposing (main)

import Browser
import Element exposing (Element, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Fraction exposing (Fraction)
import Html exposing (Html)
import Random exposing (Generator)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { question : Maybe CalculationQuestion
    , difficulty : Difficulty
    , streak : Int
    , correct : Int
    , incorrect : Int
    , numeratorAnswer : String
    , denominatorAnswer : String
    }


initialModel : Model
initialModel =
    { question = Nothing
    , difficulty = Easy
    , streak = 0
    , correct = 0
    , incorrect = 0
    , numeratorAnswer = ""
    , denominatorAnswer = ""
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Difficulty
    = Easy
    | Intermediate
    | Hard


type Msg
    = GetNewQuestion
    | NewQuestion CalculationQuestion
    | SetDifficulty Difficulty
    | UpdateNumeratorAnswer String
    | UpdateDenominatorAnswer String
    | SubmitCalculationAnswer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNewQuestion ->
            ( model, Random.generate NewQuestion (calculationGenerator model.difficulty) )

        NewQuestion question ->
            ( { model | question = Just question }, Cmd.none )

        SetDifficulty difficulty ->
            ( { model | difficulty = difficulty }, Cmd.none )

        UpdateNumeratorAnswer numeratorAnswer ->
            ( { model | numeratorAnswer = numeratorAnswer }, Cmd.none )

        UpdateDenominatorAnswer denominatorAnswer ->
            ( { model | denominatorAnswer = denominatorAnswer }, Cmd.none )

        SubmitCalculationAnswer ->
            let
                maybeFraction : Maybe Fraction
                maybeFraction =
                    Maybe.map2
                        (\numerator denominator -> ( numerator, denominator ))
                        (String.toInt model.numeratorAnswer)
                        (String.toInt model.denominatorAnswer)
                        |> Maybe.andThen Fraction.fromTuple
            in
            case maybeFraction of
                Just fraction ->
                    if Fraction.equal model.question.answer fraction then
                        let
                            newModel =
                                { question = model.question
                                , difficulty = model.difficulty
                                , streak = increment model.streak
                                , correct = increment model.correct
                                , incorrect = model.incorrect
                                , numeratorAnswer = ""
                                , denominatorAnswer = ""
                                }
                        in
                        ( newModel, GetNewQuestion )

                    else
                        let
                            newModel =
                                { question = model.question
                                , difficulty = model.difficulty
                                , streak = 0
                                , correct = model.correct
                                , incorrect = increment model.incorrect
                                , numeratorAnswer = ""
                                , denominatorAnswer = ""
                                }
                        in
                        ( newModel, GetNewQuestion )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        []
        (mainView model)


mainView : Model -> Element Msg
mainView model =
    column
        [ Element.centerX
        , Element.centerY
        ]
        [ row
            [ spacing 10, padding 10 ]
            [ Element.paragraph
                []
                [ el [ Font.bold ] <| text "Correct: "
                , text <| String.fromInt model.correct
                ]
            , Element.paragraph
                []
                [ el [ Font.bold ] <| text "Incorrect: "
                , text <| String.fromInt model.incorrect
                ]
            , Element.paragraph
                []
                [ el [ Font.bold ] <| text "Streak: "
                , text <| String.fromInt model.streak
                ]
            ]
        , row
            [ spacing 20, padding 10 ]
            [ setDifficultyButton model.difficulty Easy
            , setDifficultyButton model.difficulty Intermediate
            , setDifficultyButton model.difficulty Hard
            ]
        , row
            [ Element.centerX ]
            [ case model.question of
                Just question ->
                    questionView question model.numeratorAnswer model.denominatorAnswer

                Nothing ->
                    row
                        []
                        [ Input.button
                            []
                            { onPress = Just GetNewQuestion
                            , label = text "Start"
                            }
                        ]
            ]
        ]


setDifficultyButton : Difficulty -> Difficulty -> Element Msg
setDifficultyButton modelDifficulty difficulty =
    Input.button
        [ Background.color <|
            if modelDifficulty == difficulty then
                Element.rgb255 60 60 60

            else
                Element.rgb255 128 128 128
        , padding 10
        ]
        { onPress = Just <| SetDifficulty difficulty
        , label = text <| difficultyToString difficulty
        }


fractionView : Fraction -> Element msg
fractionView fraction =
    column
        [ Element.alignRight, spacing 10 ]
        [ fraction
            |> Fraction.getNumerator
            |> String.fromInt
            |> text
        , el
            [ width fill
            , height <| px 2
            , Background.color <| Element.rgb255 0 0 0
            ]
            Element.none
        , fraction
            |> Fraction.getDenominator
            |> String.fromInt
            |> text
        ]


questionView : CalculationQuestion -> String -> String -> Element Msg
questionView calculation numeratorAnswer denominatorAnswer =
    column
        [ width fill ]
        [ row
            [ Element.centerX, spacing 10 ]
            [ column
                []
                [ fractionView calculation.fraction1 ]
            , Element.paragraph
                []
                [ calculation.operation
                    |> operationToString
                    |> text
                ]
            , column
                []
                [ fractionView calculation.fraction2 ]
            ]
        , row
            [ width fill ]
            [ column
                [ spacing 20
                , padding 20
                , width fill
                ]
                [ Input.text
                    []
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
                    , Background.color <| Element.rgb255 90 60 120
                    , Border.rounded 4
                    ]
                    { onPress = Just SubmitCalculationAnswer
                    , label = text "Submit"
                    }
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
    Random.uniform Add [ Subtract, Multiply, Divide ]


type alias CalculationQuestion =
    { fraction1 : Fraction
    , operation : MathOperation
    , fraction2 : Fraction
    , answer : Fraction
    }


difficultyFractionBounds : Difficulty -> ( Int, Int )
difficultyFractionBounds difficulty =
    case difficulty of
        Easy ->
            ( -5, 5 )

        Intermediate ->
            ( -10, 10 )

        Hard ->
            ( -20, 20 )


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
    Random.map2
        (\numerator denominator -> Fraction.createUnsafe numerator denominator)
        (fractionIntGenerator difficulty)
        (fractionIntGenerator difficulty)


calculationGenerator : Difficulty -> Generator CalculationQuestion
calculationGenerator difficulty =
    Random.map3
        (\fraction1 mathOperation fraction2 ->
            CalculationQuestion
                fraction1
                mathOperation
                fraction2
                (getCalculationAnswer fraction1 mathOperation fraction2)
        )
        (fractionGenerator difficulty)
        mathOperationGenerator
        (fractionGenerator difficulty)


getCalculationAnswer : Fraction -> MathOperation -> Fraction -> Fraction
getCalculationAnswer fraction1 mathOperation fraction2 =
    case mathOperation of
        Add ->
            Fraction.add fraction1 fraction2

        Subtract ->
            Fraction.subtract fraction1 fraction2

        Multiply ->
            Fraction.multiply fraction1 fraction2

        Divide ->
            fractionUnsafeDivision fraction1 fraction2



-- HELPERS


increment : Int -> Int
increment int =
    int + 1


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
operationToString operation =
    case operation of
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
    Fraction.createUnsafe (Fraction.getDenominator fraction) (Fraction.getNumerator fraction)


fractionUnsafeDivision : Fraction -> Fraction -> Fraction
fractionUnsafeDivision fraction1 fraction2 =
    Fraction.multiply fraction1 <| fractionUnsafeReciprocal fraction2
