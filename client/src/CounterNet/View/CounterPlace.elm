module CounterNet.View.CounterPlace exposing(..)
import CounterNet.Static.Types.CounterPlace exposing(Msg(..))
import CounterNet.Static.Types exposing(CounterPlace(..))
import CounterNet.Static.Helpers.CounterPlace exposing(..)
import CounterNet.Static.ExtraTypes exposing(..)

import Html exposing(..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)

import GraphicSVG exposing(filled,red,blue,circle)
import GraphicSVG.Widget as Widget

view : CounterPlace -> Html Msg
view (CounterPlace counter) =
  div [] [
    div [style "width" "10%", style "height" "10%"] [Widget.icon "myCircle" 50 50
      [
        circle (abs <| toFloat counter)
          |> filled (if counter < 0 then blue else red)
      ]]
  , div []
        [ button [ onClick DecrementCounter ] [ text "-" ]
        , div [] [ text (String.fromInt counter) ]
        , button [ onClick IncrementCounter ] [ text "+" ]
        ]
  , div []
        [ button [ onClick GoToMainMenu ] [ text "Exit" ]
        ]
  ]

title : CounterPlace -> String
title counterPlace =
    "COUNTING IS EXCITING"

subs : CounterPlace -> Sub Msg
subs _ = Sub.none