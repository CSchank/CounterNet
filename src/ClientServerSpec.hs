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
                    [
                     dt (ExistingT "Model" "CounterNet.CounterSVG") "counterState" ""
                    ]                          --client state
                    Nothing
                    

        goToCounterPlace =                 
            Transition
                OriginClientOnly
                (constructor "GoToCounterPlace" [])
                [("MainMenu", Just ("CounterPlace", constructor "WentToCounterPlace" [clientCounterData]))
                ,("MainMenu", Nothing) --some people will stay
                ]
                Nothing

        goToMainMenu =                 
            Transition
                OriginClientOnly
                (constructor "GoToMainMenu" [])
                [("CounterPlace", Just ("MainMenu", constructor "WentToMainMenu" []))
                ,("CounterPlace", Nothing) --some people will stay
                ]
                Nothing

        incrementCounter =
            Transition
                OriginClientOnly
                (constructor "IncrementCounter" [])
                [("CounterPlace", Just ("CounterPlace", constructor "CounterIncremented" [clientCounterData]))
                ]
                Nothing

        decrementCounter =
            Transition
                OriginClientOnly
                (constructor "DecrementCounter" [])
                [("CounterPlace", Just ("CounterPlace", constructor "CounterDecremented" [clientCounterData]))
                ]
                Nothing

        counterMsg =
            ClientTransition
                (msg "CounterMsg" [dt (ExistingT "Msg" "CounterNet.CounterSVG") "counterMsg" ""])
                "CounterPlace"
                (Just "CounterMsg")
    in
        Net
            "CounterNet"
            "MainMenu"
            [mainMenu, counterPlace]
            [goToCounterPlace,goToMainMenu,incrementCounter,decrementCounter,counterMsg]
            []


clientServerApp :: ClientServerApp
clientServerApp =
    ( "CounterNet"            --starting net for a client
    , [counterNet]             --all the nets in this client/server app
    , []              --extra client types used in states or messages
    )
