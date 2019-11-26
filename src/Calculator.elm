module Calculator exposing (..)

import Browser
import Bulma.CDN exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick,onInput)
import Html.Attributes exposing(..)
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



type alias Model = {text:String, firstNumber:Float , sign:Signs }




init : Model
init = Model "" 0 Sum 


type Msg = AddNumber String
  |AddOperator Signs
  |Delete 
  |Solve
  |Reset
  
type Signs = Sum
  | Subtract
  | Divide
  | Multiply
 
-- UPDATE

  

solve : Signs ->Float -> Float -> Float 
solve sign a b  =
  case sign of
    Sum ->
      a + b

    Subtract ->
      a - b

    Divide ->
      a/b

    Multiply ->
      a * b


    
     

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddNumber addChar ->
      {model | text= model.text ++ addChar}

    Delete ->
     {model|text  = ""}

    Solve ->
      let
        secondNumber = Maybe.withDefault 0 (String.toFloat model.text)
      in
        {model| firstNumber = 0,  
        text = String.fromFloat (solve model.sign model.firstNumber secondNumber)}

    AddOperator selectedOperator-> 
      {model| sign = selectedOperator , 
      firstNumber = Maybe.withDefault 0 (String.toFloat model.text) , text = ""}

    Reset ->
      {model| sign = Sum, text = "", firstNumber = 0}



-- VIEW


view : Model -> Html Msg
view model
 = main_ []
    [ stylesheet
    
    , myhero model
    
    ]

myhero : Model -> Html Msg
myhero model
  = div [ class "hero is-fullheight is-default is-bold"] [
    div [ class "hero-head"] [ 
      myNavbar
    ],
    div [class "hero-body"] [
      div [class "columns is-fullwidth is-fullheight"] [
        div [class "column is-8"] [
          myForm model
        ],
        div [class "column is-4"] [
          text "history"
        ]
      ]
    ],
    div [ class "hero-foot"] [
      div [class "tabs is-centered"] [
        h1[] [text "And this is bottom."]
      ]
    ]

  ]
        
myNavbar: Html Msg 
myNavbar 
  = nav [class "navbar"] [
    div [ class "container"] [
      div [ class "navbar-brand"] [
        a [ class "navbar-item"][
          h1 [] [text "Calculator 2077"]
        ]
      ]
    ]
  ]

myForm: Model -> Html Msg
myForm model
  = div [class "container"] [
      input [ type_"text", class "input is-rounded is-fullwidth",  value model.text, readonly True][]
    ,
    div [class "columns is-fullwidth is-gapeless is-mobile"] [
      div[class "column is-3"] [
       btn "(",
       nbtn "7",
       nbtn "4",
       nbtn "1",
       btn ":D"
      ],
      div[class "column is-3"] [
        btn ")",
        nbtn "8",
        nbtn "5",
        nbtn "2",
        nbtn "0"
      ],
      div[class "column is-3"] [
       fbtn Delete "Del",
       nbtn "9",
       nbtn "6",
       nbtn "3",
       nbtn "."
      ],
      div[class "column is-3"] [
        obtn "/" Divide,
        obtn "*" Multiply,
        obtn "-" Subtract,
        obtn "+" Sum,
        fbtn Solve "="
      ]
      
    ]
  ]

nbtn: String  -> Html Msg
nbtn txt
  = button [ class "button is-fullwidth", onClick (AddNumber txt)][
    text txt
   ]

obtn: String -> Signs  -> Html Msg
obtn txt sign
  = button [ class "button is-fullwidth", onClick (AddOperator sign)][
    text txt
   ]

btn: String  -> Html Msg
btn txt
  = button [ class "button is-fullwidth"][
    text txt
   ]

fbtn: Msg -> String -> Html Msg
fbtn msg txt 
  = button [ class "button is-fullwidth" ,onClick msg][
    text txt
   ]