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



type alias Model = {text:String, firstNumber:Float , operator:Operator, secondNumber:Int}

type Operator
  = Sum
  | Subtract
  | Divide
  | Multiply


init : Model
init = Model "" 0 Sum 0



-- UPDATE

 
type Msg
  = AddNumber String
  |AddOperator Operator
  |Delete 
  |Solve



update : Msg -> Model -> Model
update msg model =
  case msg of
    AddNumber addChar ->
      {model | text= model.text ++ addChar}

    Delete ->
     {model|text  = ""}

    Solve ->
      {model| text =  model.text}

    AddOperator selectedOperator-> 
      {model| operator = selectedOperator , firstNumber = String.toInt text , text = ""}



-- VIEW


view : Model -> Html Msg
view model =
   div []
    [ 
     div [] [ text  model.text ], 
     div[][
      button [ onClick (AddNumber "(") ] [ text "(" ],
      button [onClick (AddNumber "(")] [text "("],
      button[onClick Delete] [text "DEL"],
      button[onClick(AddNumber"/")] [text "/"]
     ],
     div[][
      button [ onClick (AddNumber "7") ] [ text "7" ],
      button [onClick (AddNumber "8")] [text "8"],
      button[onClick (AddNumber "9")] [text "9"],
      button[onClick(AddNumber"*")] [text "*"]
     ],
     div[][
      button [ onClick (AddNumber "4") ] [ text "4" ],
      button [onClick (AddNumber "5")] [text "5"],
      button[onClick (AddNumber "6")] [text "6"],
      button[onClick(AddNumber"-")] [text "-"]
     ],
     div[][
      button [ onClick (AddNumber "1") ] [ text "1" ],
      button [onClick (AddNumber "2")] [text "2"],
      button[onClick (AddNumber "3")] [text "3"],
      button[onClick(AddNumber"+")] [text "+"]
     ],
     div[][
      button [ ] [ text ":)" ],
      button [onClick (AddNumber "0")] [text "0"],
      button[onClick (AddNumber ".")] [text "."],
      button[onClick Solve] [text "="]
     ]
     
    ]
