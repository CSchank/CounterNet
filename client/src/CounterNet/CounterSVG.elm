module CounterNet.CounterSVG exposing(..)

import GraphicSVG exposing(..)
import GraphicSVG.Widget as Widget
import Debug
{-import CounterNet.Static.Types exposing (CounterPlace(..))
import CounterNet.Static.Types.CounterPlace exposing (Msg(..))-}

type alias Model = 
    {
        counter : Int
    ,   widgetState : Widget.Model Msg
    }

init n =
    let
        (wState, wCmd) = Widget.init 200 200 WidgetMsg "counter"
    in
    ({
        counter = n
    ,   widgetState = wState
    }, wCmd
    )
type Msg =
      Increment
    | Decrement
    | WidgetMsg (Widget.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Debug.log "CounterSVG.msg" msg of
        Increment ->
            ({ model | counter = model.counter + 1 }, Cmd.none )
        Decrement ->
            ({ model | counter = model.counter - 1 }, Cmd.none )
        WidgetMsg wMsg ->
            let
                (wModel, wCmd) = Widget.update wMsg model.widgetState
            in
                ( { model | widgetState = wModel } , wCmd )


view : Model -> List (Shape Msg)
view model =
    [
        text (String.fromInt model.counter) |> filled black
    ,   circle (toFloat model.counter) |> filled red
    ,   circle 20 |> filled black |> move (50,30) |> notifyTap Increment
    ,   circle 20 |> filled black |> move (50,-30) |> notifyTap Decrement
    ]