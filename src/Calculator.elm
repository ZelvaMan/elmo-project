module Calculator exposing (..)

import Browser
import Bulma.CDN exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Layout exposing (..)
import Html exposing ( Html, main_, text)
import Html.Events exposing (onClick)
import Basics exposing (..)
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import String exposing (String)
import Char exposing (Char)
import Tuple
import Debug

import Platform exposing ( Program )
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )
main =
  Browser.sandbox { init = init, update = update, view = view }



type alias Model = {text:String, firstNumber:Float , sign:Signs, secondNumber:Float }




init : Model
init = Model "" 0 Sum 0



-- UPDATE

 
  type Msg 
  = AddNumber String
  |AddOperator Signs
  |Delete 
  |Solve
  |Reset

  type Signs 
  = Sum
  | Subtract
  | Divide
  | Multiply

solve : Signs ->Float -> Float -> Float 
solve sign firstNumber secondNumber  =
  case sign of
    Sum ->
      firstNumber + secondNumber

    Substract ->
      firstNumber - second number

    Divide ->
      firstNumber/SecondNumber

    Multiply ->
      firstNumber * SecondNumber


    
     

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddNumber addChar ->
      {model | text= model.text ++ addChar}

    Delete ->
     {model|text  = ""}

    Solve ->
      model.secondNumber = Maybe.withDefault 0 (String.toFloat model.text);
      model.text = String.fromFloat (solve model.sign model.firstNumber model.secondNumber)
       {model| firstNumber = 0, secondNumber = 0, sign = }

    AddOperator selectedOperator-> 
      {model| sign = selectedOperator , firstNumber = Maybe.withDefault 0 (String.toFloat model.text) , text = ""}

    Reset ->
      {model| Signs = }



-- VIEW


view : Model -> Html Msg
view model = container []
      [ columns columnsModifiers []
        [ column columnModifiers [] [ text "First Column" ]
        , column columnModifiers [] [ text "Second Column" ]
        , column columnModifiers [] [ text "Third Column" ]
        ]]
  
    
