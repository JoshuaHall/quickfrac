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
    | SubmitCalculationAnswer CalculationQuestion


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNewQuestion ->
            ( model, Random.generate NewQuestion <| calculationGenerator model.difficulty )

        NewQuestion question ->
            ( { model | question = Just question }, Cmd.none )

        SetDifficulty difficulty ->
            let
                newModel : Model
                newModel =
                    { question = model.question
                    , difficulty = difficulty
                    , streak = 0
                    , correct = 0
                    , incorrect = 0
                    , numeratorAnswer = ""
                    , denominatorAnswer = ""
                    }
            in
            update GetNewQuestion newModel

        UpdateNumeratorAnswer numeratorAnswer ->
            ( { model | numeratorAnswer = numeratorAnswer }, Cmd.none )

        UpdateDenominatorAnswer denominatorAnswer ->
            ( { model | denominatorAnswer = denominatorAnswer }, Cmd.none )

        SubmitCalculationAnswer question ->
            let
                maybeFraction =
                    Maybe.map2
                        (\numerator denominator -> ( numerator, denominator ))
                        (String.toInt model.numeratorAnswer)
                        (String.toInt model.denominatorAnswer)
                        |> Maybe.andThen Fraction.fromTuple
            in
            case maybeFraction of
                Just fraction ->
                    if Fraction.equal question.answer fraction then
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
                        update GetNewQuestion newModel

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
                        update GetNewQuestion newModel

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
            [ Element.centerX
            , spacing 20
            , padding 10
            ]
            [ questionCounter "Correct" model.correct
            , questionCounter "Incorrect" model.incorrect
            , questionCounter "Streak" model.streak
            ]
        , row
            [ Element.centerX
            , spacing 20
            , padding 10
            ]
            [ setDifficultyButton model.difficulty model.question Easy
            , setDifficultyButton model.difficulty model.question Intermediate
            , setDifficultyButton model.difficulty model.question Hard
            ]
        , case model.question of
            Just question ->
                questionView question model.numeratorAnswer model.denominatorAnswer

            Nothing ->
                text "Choose a difficulty from above to get started!"
        ]


questionCounter : String -> Int -> Element msg
questionCounter counterText count =
    column
        [ Element.centerX
        , Font.center
        ]
        [ el
            [ Font.bold
            , Font.center
            ]
            (text counterText)
        , el
            [ Element.centerX ]
            (String.fromInt count
                |> text
            )
        ]


setDifficultyButton : Difficulty -> Maybe CalculationQuestion -> Difficulty -> Element Msg
setDifficultyButton modelDifficulty maybeQuestion difficulty =
    let
        difficultyButtonColor =
            if modelDifficulty == difficulty && isJust maybeQuestion then
                elmBlue

            else
                elmGray
    in
    Input.button
        [ Background.color difficultyButtonColor
        , padding 10
        , Border.rounded 4
        ]
        { onPress = Just <| SetDifficulty difficulty
        , label = text <| difficultyToString difficulty
        }


fractionView : Fraction -> Element msg
fractionView fraction =
    column
        [ Element.alignRight
        , spacing 10
        ]
        [ el
            [ width fill
            , Font.center
            ]
            (fraction
                |> Fraction.getNumerator
                |> String.fromInt
                |> text
            )
        , el
            [ width fill
            , height <| px 2
            , Background.color black
            ]
            Element.none
        , el
            [ width fill
            , Font.center
            ]
            (fraction
                |> Fraction.getDenominator
                |> String.fromInt
                |> text
            )
        ]


questionView : CalculationQuestion -> String -> String -> Element Msg
questionView calculation numeratorAnswer denominatorAnswer =
    column
        [ width fill ]
        [ row
            [ Element.centerX
            , spacing 10
            ]
            [ fractionView calculation.fraction1
            , calculation.operation
                |> operationToString
                |> text
            , fractionView calculation.fraction2
            ]
        , column
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
                , Background.color elmGreen
                , Border.rounded 4
                , spacing 20
                , padding 20
                ]
                { onPress = Just <| SubmitCalculationAnswer calculation
                , label =
                    el
                        [ Element.centerX
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


type alias CalculationQuestion =
    { fraction1 : Fraction
    , operation : MathOperation
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
    Random.map2
        (\numerator denominator -> Fraction.createUnsafe numerator denominator |> Fraction.simplify)
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



-- GENERAL HELPERS


increment : Int -> Int
increment int =
    int + 1


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False



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
    Fraction.createUnsafe
        (Fraction.getDenominator fraction)
        (Fraction.getNumerator fraction)


fractionUnsafeDivision : Fraction -> Fraction -> Fraction
fractionUnsafeDivision fraction1 fraction2 =
    Fraction.multiply fraction1 <| fractionUnsafeReciprocal fraction2



-- COLOR HELPERS


white : Element.Color
white =
    Element.rgb 1 1 1


black : Element.Color
black =
    Element.rgb 0 0 0


elmBlue : Element.Color
elmBlue =
    Element.rgb255 96 181 204


elmGreen : Element.Color
elmGreen =
    Element.rgb255 127 209 59


elmOrange : Element.Color
elmOrange =
    Element.rgb255 240 173 0


elmGray : Element.Color
elmGray =
    Element.rgb255 90 99 120
