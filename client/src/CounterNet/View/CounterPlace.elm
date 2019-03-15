module CounterNet.View.CounterPlace exposing(..)
import CounterNet.Static.Types.CounterPlace exposing(Msg(..))
import CounterNet.Static.Types exposing(CounterPlace(..))
import CounterNet.Static.Helpers.CounterPlace exposing(..)
import CounterNet.Static.ExtraTypes exposing(..)

import Html exposing(..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import GraphicSVG.Widget as Widget

import CounterNet.CounterSVG as CounterSVG

view : CounterPlace -> Html Msg
view counterPlace =
  let
    counter = (getCounterState counterPlace)
  in
  div []
    [
      div [style "width" "25%", style "height" "25%"] 
        [Html.map CounterMsg <| Widget.view (counter.widgetState)
        (CounterSVG.view counter)]
       ,       
      div [] [
        div []
            [ button [ onClick DecrementCounter ] [ text "-" ]
            , div [] [ text (String.fromInt counter.counter) ]
            , button [ onClick IncrementCounter ] [ text "+" ]
            ]
      , div []
            [ button [ onClick GoToMainMenu ] [ text "Exit" ]
            ]
      ]
    ]

title : CounterPlace -> String
title counterPlace =
    "COUNTING IS EXCITING"

subs : CounterPlace -> Sub Msg
subs _ = Sub.none