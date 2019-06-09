{-# LANGUAGE OverloadedStrings #-}

module ClientServerSpec where

import Types
import TypeHelpers

--directory to output the generated files
outputDirectory = "."
--where the generator is
generatorRoot = "../elm-haskell-state-diagram"


clientCounterData = dt (IntRangeT (-1000000) 1000000) "clientCounterData" "client side counter data"

counterNet :: Net
counterNet =
    let
        mainMenu =
            Place "MainMenu" 
                    [] --server state
                    []                  --player state
                    []                          --client state
                    Nothing
                    

        counterPlace =
            Place "CounterPlace"
                    [dt (IntRangeT (-1000000) 1000000) "serverCounterData" "server side counter data"
                    ] --server state
                    []                  --player state
                    [clientCounterData]                          --client state
                    Nothing
                    

        goToCounterPlace =                 
            Transition
                OriginClientOnly
                (constructor "GoToCounterPlace" [])
                [("MainMenu", Just ("CounterPlace", constructor "WentToCounterPlace" [clientCounterData], Nothing))
                ,("MainMenu", Nothing) --some people will stay
                ]
                Nothing

        goToMainMenu =                 
            Transition
                OriginClientOnly
                (constructor "GoToMainMenu" [])
                [("CounterPlace", Just ("MainMenu", constructor "WentToMainMenu" [], Nothing))
                ,("CounterPlace", Nothing) --some people will stay
                ]
                Nothing

        incrementCounter =
            Transition
                OriginClientOnly
                (constructor "IncrementCounter" [])
                [("CounterPlace", Just ("CounterPlace", constructor "CounterIncremented" [clientCounterData], Nothing))
                ]
                Nothing

        decrementCounter =
            Transition
                OriginClientOnly
                (constructor "DecrementCounter" [])
                [("CounterPlace", Just ("CounterPlace", constructor "CounterDecremented" [clientCounterData], Nothing))
                ]
                Nothing
    in
        Net
            "CounterNet"
            "MainMenu"
            [mainMenu, counterPlace]
            [goToCounterPlace,goToMainMenu,incrementCounter,decrementCounter]
            []


clientServerApp :: ClientServerApp
clientServerApp =
    ( "CounterNet"            --starting net for a client
    , [counterNet]             --all the nets in this client/server app
    , []              --extra client types used in states or messages
    )

-- main menu view

module CounterNet.View.MainMenu exposing(..)
import CounterNet.Static.Types.MainMenu exposing(Msg(..))
import CounterNet.Static.Types exposing(MainMenu(..))
import CounterNet.Static.Helpers.MainMenu exposing(..)
import CounterNet.Static.ExtraTypes exposing(..)

import Html exposing(..)
import Html.Events exposing (..)

view : MainMenu -> Html Msg
view mainMenu =
    div []
    [   h1 [] [ text "A Simple Multiplayer Counting App"]
    ,   button [ onClick GoToCounterPlace ] [ text "Let's Start!"]
    ]

title : MainMenu -> String
title mainMenu = "Simple Counter App"

subs : MainMenu -> Sub Msg
subs _ = Sub.none

-- counter place view

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

-- client update
module CounterNet.Update exposing(..)
import CounterNet.Static.Types exposing(..)
import CounterNet.Static.FromSuperPlace exposing(..)
import CounterNet.Static.ExtraTypes exposing(..)
import CounterNet.Static.Helpers.MainMenu as MainMenu
import CounterNet.Static.Helpers.CounterPlace as CounterPlace
import Utils.Utils
import Debug exposing(todo)

updateMainMenuWentToCounterPlaceCounterPlace : FromSuperPlace -> WentToCounterPlace -> MainMenu -> CounterPlace
updateMainMenuWentToCounterPlaceCounterPlace fsp (WentToCounterPlace clientCounterData)  mainMenu =
    CounterPlace clientCounterData

updateCounterPlaceWentToMainMenuMainMenu : FromSuperPlace -> WentToMainMenu -> CounterPlace -> MainMenu
updateCounterPlaceWentToMainMenuMainMenu fsp WentToMainMenu  counterPlace =
    MainMenu

updateCounterPlaceCounterIncrementedCounterPlace : FromSuperPlace -> CounterIncremented -> CounterPlace -> CounterPlace
updateCounterPlaceCounterIncrementedCounterPlace fsp (CounterIncremented clientCounterData)  counterPlace =
    CounterPlace clientCounterData

updateCounterPlaceCounterDecrementedCounterPlace : FromSuperPlace -> CounterDecremented -> CounterPlace -> CounterPlace
updateCounterPlaceCounterDecrementedCounterPlace fsp (CounterDecremented clientCounterData)  counterPlace =
    CounterPlace clientCounterData


--server update

module CounterNet.Update where
import CounterNet.Static.Types
import CounterNet.Static.FromSuperPlace
import CounterNet.Static.Helpers.MainMenu as MainMenu
import CounterNet.Static.Helpers.CounterPlace as CounterPlace

-- import CounterNet.Static.Helpers.MainMenuPlayer as MainMenuPlayer
-- import CounterNet.Static.Helpers.CounterPlacePlayer as CounterPlacePlayer

import Static.List
import Utils.Utils
import Static.ServerTypes
import Static.Cmd (Cmd(..))
import qualified Static.Cmd as Cmd

-- function called when new client connects (do not delete)
clientConnect :: FromSuperPlace -> ClientID -> MainMenu -> (MainMenu, MainMenuPlayer)
clientConnect fsp clientID mainMenu =
    (mainMenu,MainMenuPlayer)

-- functions called when a client disconnects (do not delete)
clientDisconnectFromCounterPlace :: FromSuperPlace -> ClientID -> CounterPlace -> CounterPlacePlayer -> CounterPlace
clientDisconnectFromCounterPlace fsp clientID counterPlace counterPlacePlayer =
    counterPlace

clientDisconnectFromMainMenu :: FromSuperPlace -> ClientID -> MainMenu -> MainMenuPlayer -> MainMenu
clientDisconnectFromMainMenu fsp clientID mainMenu mainMenuPlayer =
    mainMenu

-- functions for each transition
updateGoToCounterPlace :: FromSuperPlace -> 
    ClientID ->
    GoToCounterPlace ->
    MainMenu -> 
    CounterPlace -> 
    List MainMenuPlayer -> 
    ( MainMenu,
      CounterPlace,
      (ClientID, MainMenuPlayer) -> GoToCounterPlacefromMainMenu
    )
updateGoToCounterPlace fsp clientId GoToCounterPlace mainMenu counterPlace lstMainMenu =
    let
        fromMainMenu :: (ClientID, MainMenuPlayer) -> GoToCounterPlacefromMainMenu
        fromMainMenu (pId, pmainMenu) = 
            if clientId == pId
                then GoToCounterPlace_MainMenutoCounterPlace CounterPlacePlayer (WentToCounterPlace (getServerCounterData counterPlace))
                else GoToCounterPlace_Stay_MainMenu MainMenuPlayer


    in
        (mainMenu, counterPlace, fromMainMenu)

updateGoToMainMenu :: FromSuperPlace -> 
    ClientID ->
    GoToMainMenu ->
    CounterPlace -> 
    MainMenu -> 
    List CounterPlacePlayer -> 
    ( CounterPlace,
      MainMenu,
      (ClientID, CounterPlacePlayer) -> GoToMainMenufromCounterPlace
    )
updateGoToMainMenu fsp clientId GoToMainMenu counterPlace mainMenu lstCounterPlace =
    let
        fromCounterPlace :: (ClientID, CounterPlacePlayer) -> GoToMainMenufromCounterPlace
        fromCounterPlace (pId, pcounterPlace) =
            if clientId == pId
                then GoToMainMenu_CounterPlacetoMainMenu MainMenuPlayer WentToMainMenu
                else GoToMainMenu_Stay_CounterPlace CounterPlacePlayer


    in
        (counterPlace, mainMenu, fromCounterPlace)

updateIncrementCounter :: FromSuperPlace -> 
    ClientID ->
    IncrementCounter ->
    CounterPlace -> 
    List CounterPlacePlayer -> 
    ( CounterPlace,
      (ClientID, CounterPlacePlayer) -> IncrementCounterfromCounterPlace
    )
updateIncrementCounter fsp clientId IncrementCounter counterPlace lstCounterPlace =
    let
        newServerCounterData@(CounterPlace counter) = alterServerCounterData (+1) counterPlace

        fromCounterPlace :: (ClientID, CounterPlacePlayer) -> IncrementCounterfromCounterPlace
        fromCounterPlace (pId, pcounterPlace) = 
            IncrementCounter_CounterPlacetoCounterPlace CounterPlacePlayer (CounterIncremented counter)


    in
        (newServerCounterData, fromCounterPlace)

updateDecrementCounter :: FromSuperPlace -> 
    ClientID ->
    DecrementCounter ->
    CounterPlace -> 
    List CounterPlacePlayer -> 
    ( CounterPlace,
      (ClientID, CounterPlacePlayer) -> DecrementCounterfromCounterPlace
    )
updateDecrementCounter fsp clientId DecrementCounter counterPlace lstCounterPlace =
    let
        newServerCounterData@(CounterPlace counter) = alterServerCounterData (flip (-) 1) counterPlace

        fromCounterPlace :: (ClientID, CounterPlacePlayer) -> DecrementCounterfromCounterPlace
        fromCounterPlace (pId, pcounterPlace) = 
            DecrementCounter_CounterPlacetoCounterPlace CounterPlacePlayer (CounterDecremented counter)

    in
        (newServerCounterData, fromCounterPlace)


