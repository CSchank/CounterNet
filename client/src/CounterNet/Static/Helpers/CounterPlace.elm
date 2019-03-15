module CounterNet.Static.Helpers.CounterPlace exposing (..)
import Dict exposing (Dict)

import CounterNet.Static.ExtraTypes exposing(..)
import CounterNet.Static.Types exposing(..)
import CounterNet.Static.Types
import CounterNet.CounterSVG


getCounterState : CounterPlace -> CounterNet.CounterSVG.Model
getCounterState (CounterPlace counterState)  = counterState



updateCounterState : CounterNet.CounterSVG.Model -> CounterPlace -> CounterPlace
updateCounterState newcounterState (CounterPlace counterState)  = (CounterPlace newcounterState) 


alterCounterState : (CounterNet.CounterSVG.Model -> CounterNet.CounterSVG.Model) -> CounterPlace -> CounterPlace
alterCounterState f (CounterPlace counterState)  = 
    let
        newcounterState = f counterState
    in
        (CounterPlace newcounterState) 



