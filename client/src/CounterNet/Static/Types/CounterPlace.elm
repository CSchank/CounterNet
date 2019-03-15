module CounterNet.Static.Types.CounterPlace exposing(..)
import CounterNet.Static.ExtraTypes exposing(..)
import CounterNet.CounterSVG


type Msg  =
      GoToMainMenu
    | IncrementCounter
    | DecrementCounter
    | CounterMsg CounterNet.CounterSVG.Msg
