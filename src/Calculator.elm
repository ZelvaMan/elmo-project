module Calculator exposing (..)

import Browser
import Html exposing (Html, button, div, text)
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



type alias Model = String


init : Model
init =
  ""



-- UPDATE


type Msg
  = ChangeText String
  |Delete



update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeText addChar ->
      model ++ addChar

    Delete ->
     model = ""



-- VIEW


view : Model -> Html Msg
view model =
   div []
    [ 
     div [] [ text (  model) ], 
     div[][
      button [ onClick (ChangeText "(") ] [ text "(" ],
      button [onClick (ChangeText "(")] [text "("],
      button[onClick Delete] [text "DEL"],
      button[onClick(ChangeText"/")] [text "/"]
     ]
    ]