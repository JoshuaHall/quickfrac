module Main exposing (main)

import Browser
import Element
import Fraction exposing (Fraction)
import Html exposing (Html)
import Random
import Random.Extra


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    String


initialModel : Model
initialModel =
    "Ready"


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type DifficultyLevel
    = Easy


type Msg
    = Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [] Element.none


type MathOperation
    = Add
    | Subtract
    | Multiply
    | Divide


mathOperationGenerator : Random.Generator MathOperation
mathOperationGenerator =
    Random.uniform Add [ Subtract, Multiply, Divide ]


type alias Calculation =
    { fraction1 : Fraction
    , fraction2 : Fraction
    , operation : MathOperation
    , answer : Fraction
    }


fractionIntGenerator : Random.Generator Int
fractionIntGenerator =
    Random.uniform (Random.int Random.minInt -1) [ Random.int 1 Random.maxInt ]
        |> Random.andThen identity


fractionGenerator : Random.Generator Fraction
fractionGenerator =
    Random.map2
        (\numerator denominator -> Fraction.createUnsafe numerator denominator)
        fractionIntGenerator
        fractionIntGenerator


calculationGenerator : Random.Generator Calculation
calculationGenerator =
    Random.map3
        (\fraction1 fraction2 mathOperation ->
            Calculation
                fraction1
                fraction2
                mathOperation
                (getCalculationAnswer fraction1 fraction2 mathOperation)
        )
        fractionGenerator
        fractionGenerator
        mathOperationGenerator


getCalculationAnswer : Fraction -> Fraction -> MathOperation -> Fraction
getCalculationAnswer fraction1 fraction2 mathOperation =
    case mathOperation of
        Add ->
            Fraction.add fraction1 fraction2

        Subtract ->
            Fraction.subtract fraction1 fraction2

        Multiply ->
            Fraction.multiply fraction1 fraction2

        Divide ->
            Maybe.withDefault (Fraction.createUnsafe 4000 4000) (Fraction.divide fraction1 fraction2)


type alias Comparison =
    { fraction1 : Fraction
    , fraction2 : Fraction
    , operation : Order
    , answer : Bool
    }


type Question
    = CalculationQuestion Calculation
    | ComparisonQuestion Comparison
