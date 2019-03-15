module CounterNet.Static.Wrappers.CounterPlace exposing(..)
import CounterNet.Static.Types.CounterPlace exposing(..)
import CounterNet.Static.ExtraTypes exposing(..)
import CounterNet.Static.Types exposing(..)
import CounterNet.CounterSVG

import Dict exposing (Dict)

unwrap : Msg -> Transition
unwrap msg =
    case msg of
        CounterNet.Static.Types.CounterPlace.GoToMainMenu  -> External TGoToMainMenu 
        CounterNet.Static.Types.CounterPlace.IncrementCounter  -> External TIncrementCounter 
        CounterNet.Static.Types.CounterPlace.DecrementCounter  -> External TDecrementCounter 
        (CounterNet.Static.Types.CounterPlace.CounterMsg counterMsg)  -> Internal (TCounterMsg counterMsg) 

