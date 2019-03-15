module CounterNet.Static.Update exposing(..)
import CounterNet.Static.Types exposing(..)
import CounterNet.Static.Wrappers exposing(..)
import CounterNet.Static.FromSuperPlace exposing (FromSuperPlace)
import CounterNet.Update exposing(..)
import Static.Types exposing(..)
import Dict

update : FromSuperPlace -> IncomingMessage -> NetState -> (NetState,Cmd Transition)
update fsp trans state =
    case (trans,state) of
        ((MWentToCounterPlace clientCounterData) , SMainMenu st) -> (SCounterPlace <| updateMainMenuWentToCounterPlaceCounterPlace fsp (WentToCounterPlace clientCounterData)  st, Cmd.none)

        (MWentToMainMenu , SCounterPlace st) -> (SMainMenu <| updateCounterPlaceWentToMainMenuMainMenu fsp WentToMainMenu  st, Cmd.none)

        ((MCounterIncremented clientCounterData) , SCounterPlace st) -> (SCounterPlace <| updateCounterPlaceCounterIncrementedCounterPlace fsp (CounterIncremented clientCounterData)  st, Cmd.none)

        ((MCounterDecremented clientCounterData) , SCounterPlace st) -> (SCounterPlace <| updateCounterPlaceCounterDecrementedCounterPlace fsp (CounterDecremented clientCounterData)  st, Cmd.none)

        ((MCounterMsg counterMsg) , SCounterPlace st) -> Tuple.mapBoth SCounterPlace (Cmd.map <| Internal  << unwrapCounterMsg) <| updateCounterMsgCounterPlace fsp (CounterMsg counterMsg)  st

        _ -> (state, Cmd.none)
outgoingToIncoming : Transition -> Maybe IncomingMessage
outgoingToIncoming trans =
    case trans of
        Internal (TCounterMsg counterMsg)  -> Just (MCounterMsg counterMsg) 

        _ -> Nothing
