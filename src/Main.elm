module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task
import Time exposing (Month(..), Posix, Zone)
import Time exposing (Month(..), Posix, Zone, monthToNumber, posixToMillis)

-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW

{-| Calculate the day of the year (1-366) for a given date
-}
dayOfYear : Zone -> Posix -> Int
dayOfYear zone time =
    let
        year =
            Time.toYear zone time

        month =
            Time.toMonth zone time

        day =
            Time.toDay zone time

        isLeap =
            (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))

        -- Get cumulative days at the start of each month
        daysBeforeMonth =
            cumulativeDaysByMonth isLeap month
    in
    daysBeforeMonth + day

{-| Cumulative days at the start of each month (days before the first day of the month)
-}
cumulativeDaysByMonth : Bool -> Month -> Int
cumulativeDaysByMonth isLeap month =
    let
        feb = if isLeap then 29 else 28
    in
    case month of
        Jan -> 0
        Feb -> 31
        Mar -> 31 + feb
        Apr -> 31 + feb + 31
        May -> 31 + feb + 31 + 30
        Jun -> 31 + feb + 31 + 30 + 31
        Jul -> 31 + feb + 31 + 30 + 31 + 30
        Aug -> 31 + feb + 31 + 30 + 31 + 30 + 31
        Sep -> 31 + feb + 31 + 30 + 31 + 30 + 31 + 31
        Oct -> 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30
        Nov -> 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31
        Dec -> 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30




dayOfYear : Time.Month -> Int -> Int
dayOfYear month day =
    (case month

    ) + day

view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)

        day =
            String.fromInt (Time.toDay model.zone model.time)
    in
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second ++ ", " ++ day) ]


{-| Calculate the day of the year (1-366) for a given date
-}
dayOfYear : Zone -> Posix -> Int
dayOfYear zone time =
    let
        year =
            Time.toYear zone time

        month =
            Time.toMonth zone time

        day =
            Time.toDay zone time

        isLeap =
            (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))

        -- Get cumulative days at the start of each month
        daysBeforeMonth =
            cumulativeDaysByMonth isLeap month
    in
    daysBeforeMonth + day


{-| Days in each month
-}
daysInMonth : Month -> Bool -> Int
daysInMonth month isLeap =
    case month of
        Jan -> 31
        Feb -> if isLeap then 29 else 28
        Mar -> 31
        Apr -> 30
        May -> 31
        Jun -> 30
        Jul -> 31
        Aug -> 31
        Sep -> 30
        Oct -> 31
        Nov -> 30
        Dec -> 31


{-| Cumulative days at the start of each month (days before the first day of the month)
-}
cumulativeDaysByMonth : Bool -> Month -> Int
cumulativeDaysByMonth isLeap month =
    let
        feb = if isLeap then 29 else 28
    in
    case month of
        Jan -> 0
        Feb -> 31
        Mar -> 31 + feb
        Apr -> 31 + feb + 31
        May -> 31 + feb + 31 + 30
        Jun -> 31 + feb + 31 + 30 + 31
        Jul -> 31 + feb + 31 + 30 + 31 + 30
        Aug -> 31 + feb + 31 + 30 + 31 + 30 + 31
        Sep -> 31 + feb + 31 + 30 + 31 + 30 + 31 + 31
        Oct -> 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30
        Nov -> 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31
        Dec -> 31 + feb + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30
        --     Jan  Feb   Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov
