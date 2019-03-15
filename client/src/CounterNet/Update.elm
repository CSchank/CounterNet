module CounterNet.Update exposing(..)
import CounterNet.Static.Types exposing(..)
import CounterNet.Static.FromSuperPlace exposing(..)
import CounterNet.Static.ExtraTypes exposing(..)
import CounterNet.Static.Helpers.MainMenu as MainMenu
import CounterNet.Static.Helpers.CounterPlace as CounterPlace
import Utils.Utils
import Debug exposing(todo)

import GraphicSVG.Widget exposing(init)
import CounterNet.CounterSVG as CounterSVG

updateMainMenuWentToCounterPlaceCounterPlace : FromSuperPlace -> WentToCounterPlace -> MainMenu -> CounterPlace
updateMainMenuWentToCounterPlaceCounterPlace fsp (WentToCounterPlace clientCounterData)  mainMenu =      
    CounterPlace (Tuple.first <| CounterSVG.init clientCounterData)

updateCounterPlaceWentToMainMenuMainMenu : FromSuperPlace -> WentToMainMenu -> CounterPlace -> MainMenu
updateCounterPlaceWentToMainMenuMainMenu fsp WentToMainMenu  counterPlace =
    MainMenu

updateCounterPlaceCounterIncrementedCounterPlace : FromSuperPlace -> CounterIncremented -> CounterPlace -> CounterPlace
updateCounterPlaceCounterIncrementedCounterPlace fsp (CounterIncremented clientCounterData)  counterPlace =
    counterPlace
        |> CounterPlace.alterCounterState (\m -> { m | counter = clientCounterData} )

updateCounterPlaceCounterDecrementedCounterPlace : FromSuperPlace -> CounterDecremented -> CounterPlace -> CounterPlace
updateCounterPlaceCounterDecrementedCounterPlace fsp (CounterDecremented clientCounterData)  counterPlace =
    counterPlace
        |> CounterPlace.alterCounterState (\m -> { m | counter = clientCounterData} )

updateCounterMsgCounterPlace : FromSuperPlace -> CounterMsg -> CounterPlace -> (CounterPlace, Cmd CounterMsg)
updateCounterMsgCounterPlace fsp (CounterMsg cMsg) counterPlace =
    let
        (newCState, cCmd) = CounterSVG.update cMsg (CounterPlace.getCounterState counterPlace)
    in
        (counterPlace
            |> CounterPlace.updateCounterState newCState
        , Cmd.map CounterMsg cCmd
        )