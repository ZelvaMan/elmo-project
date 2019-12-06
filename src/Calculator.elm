module Calculator exposing (..)

import Browser

import Html exposing (..)
import Html.Events exposing (onClick)
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



type alias Model = {text:String, 
                    firstNumber:Float , 
                    sign:Signs , 
                    history : List String, 
                    scienceCalculator: Bool}




init : Model
init = Model "" 0 None [] False


type Msg = AddNumber String
  |AddOperator Signs
  |DeleteText 
  |Solve
  |Reset
  |Nothing
  |DeleteAll
  |ScienceOperation ScienceOperations
  |ChangeCalculatorType

  
type Signs = Sum
  |Subtract
  |Divide
  |Multiply
  |Xn
  |None
 

type ScienceOperations = 
   VCube
  |SCube
  |VSphere
  |SSphere
  |Factorial
-- UPDATE



getTypeText : Signs -> String
getTypeText sign  =
  case sign of
    Sum ->
      "+"

    Subtract ->
      "-"

    Divide ->
      "*"

    Multiply ->
      "*"

    Xn -> 
      "^"

    None ->
      "None"
     

count : Float -> Float ->  Signs -> Float
count f s sign 
  =     case sign of
      Sum ->
        f + s

      Subtract ->
        f - s

      Divide ->
        f/s

      Multiply ->
        f * s

      Xn -> 
        f^s

      None ->
        0


solveExpresion : Model -> Model 
solveExpresion mod  =
  let 
    f = mod.firstNumber
    s = Maybe.withDefault 0 (String.toFloat mod.text)
    msign = mod.sign
    result = count f s msign
  in
    {mod| text = String.fromFloat result, sign = None, firstNumber = 0,history =  createHistoryString f s msign result::mod.history}  



factorial : Int -> Int
factorial n =
  if n < 1 then
    1
  else
    n*factorial(n-1)
solveScience : Model -> ScienceOperations -> Model
solveScience mod sOperation =
  let
      f= mod.firstNumber
  in
  
  case sOperation of 
    VCube -> 
      {mod| text = String.fromFloat(f^3),history =  createSOHistoryStr f "V 🧊"::mod.history , firstNumber = 0, sign = None}

    SCube ->
      {mod| text = String.fromFloat(6*f^2), history = createSOHistoryStr f "S 🧊"::mod.history , firstNumber = 0, sign = None}

    VSphere ->
      {mod| text = String.fromFloat(4/3 * pi * f^3) , history =  createSOHistoryStr f "V 🔴"::mod.history , firstNumber = 0, sign = None}

    SSphere ->
      {mod| text = String.fromFloat(4 * pi * f^2) , history = createSOHistoryStr f "S 🔴"::mod.history , firstNumber = 0, sign = None}

    Factorial -> 
      {mod| text = String.fromInt(factorial (round  f)) ,history = createSOHistoryStr f "!"::mod.history , firstNumber = 0, sign = None}

createHistoryString: Float -> Float -> Signs -> Float -> String
createHistoryString first second sign result=
  let  
    f = String.fromFloat first
    s = String.fromFloat second 
    res = String.fromFloat result
    sgn = getTypeText sign 
  in
   f ++ " " ++  sgn ++ " " ++ s ++ " = "++ res
     
createSOHistoryStr: Float -> String -> Float -> String
createSOHistoryStr f  function res=
  String.fromFloat f ++ " " ++ function 

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddNumber addChar ->
      {model | text= model.text ++ addChar}
-----------------------
    DeleteText ->
     {model|text  = ""}
-------------------------
    Solve ->
      if model.sign == None then 
        {model| history = model.text::model.history}
      else
        solveExpresion model
----------------------------
    AddOperator selectedOperator->
      if model.sign == None then
        {model| sign = selectedOperator , 
        firstNumber = Maybe.withDefault 0 (String.toFloat model.text) , text = ""}
      else
        let
          result = count model.firstNumber (Maybe.withDefault 0 (String.toFloat model.text)) model.sign
        in
          {model| sign = selectedOperator , firstNumber =   result , text = ""}

----------------------------------
    Reset ->
      {model| sign = None, text = "", firstNumber = 0}
-------------------------
    ScienceOperation sOperation-> 
      solveScience {model|firstNumber = Maybe.withDefault 0 (String.toFloat model.text) , text = ""} sOperation 
------------------------
    DeleteAll ->
      {model| sign = None, text = "", firstNumber = 0, history = []}
-----------------------------
    Nothing ->
      model
---------------------------------------------------------
    ChangeCalculatorType ->
      {model|scienceCalculator = not model.scienceCalculator}

      


stylesheet: Html msg
stylesheet =
  node "link"
  [ rel  "stylesheet"
  , href "https://unpkg.com/bulma@0.8.0/css/bulma.min.css"
  ]
  []

-- VIEW
renderList : List String -> Html msg
renderList lst =
    lst
       |> List.map (li [] << List.singleton << text)
       |> ul []

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
      div [class "columns is-fullhd is-fullheight"] [
        div [class "column is-8"] [
          calculator model 
        ],
        div [class "column is-4"] [
           renderList model.history
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
          h1 [] [text "calculator 2077"]
        ]
      ]
    ]
  ]

calculator: Model -> Html Msg
calculator model 
  = div [class "container is-fullwidth is-fullheight"] [
    btn (if model.scienceCalculator == True then "switch to basic calculator" else "switch to science calculatr") ChangeCalculatorType,
    input [ type_"text", class "input is-rounded is-fullwidth",  value model.text, readonly True][]
    ,
    table [class "table is-fullwidth"] [
      tbody [] [
        if model.scienceCalculator == True then
        tr [] [
          tbtn "V 🧊" (ScienceOperation VCube),
          tbtn "S  🧊" (ScienceOperation SCube),
          tbtn "V 🔴" (ScienceOperation VSphere),
          tbtn "S 🔴" (ScienceOperation SSphere)
        ] else div[][]
        ,
        tr [] [
          tbtn (if model.scienceCalculator == True then "x^n" else "  ")
          (if model.scienceCalculator == True then AddOperator Xn else Nothing),
          tbtn (if model.scienceCalculator == True then "X!" else "  ")
          (if model.scienceCalculator == True then ScienceOperation Factorial else Nothing),
          tbtn "C" DeleteText,
          tbtn "/" (AddOperator Divide)
        ],
        tr [] [
          tbtn "7" (AddNumber "7"),
          tbtn "8" (AddNumber "8"),
          tbtn "9" (AddNumber "9"),
          tbtn "*" (AddOperator Multiply)
        ],
        tr [] [
          tbtn "4" (AddNumber "4"),
          tbtn "5" (AddNumber "5"),
          tbtn "6" (AddNumber "6"),
          tbtn "-" (AddOperator Subtract)
        ],
        tr [] [
          tbtn "1" (AddNumber "1"),
          tbtn "2" (AddNumber "2"),
          tbtn "3" (AddNumber "3"),
          tbtn "+" (AddOperator Sum)
        ],
        tr [] [
          tbtn "AC" DeleteAll,
          tbtn "0" (AddNumber "0"),
          tbtn "   " Nothing,
          tbtn "=" Solve
        ]
      ]
    ]
  ]

tbtn: String -> Msg -> Html Msg
tbtn txt msg
  = th[] [
    btn txt msg
  ]

btn : String -> Msg -> Html Msg
btn txt msg 
  = button [class "button is fullwidth", onClick msg] [
    text txt
  ]
