module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import List
import RunTable exposing (..)
import Task
import Time exposing (Month(..), Posix, Zone)



-- MAIN


main : Program () Model Msg
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
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


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
            String.fromInt (dayOfYear model.zone model.time)
    in
    div []
        [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second ++ ", " ++ day) ]
        , renderTable False 1000
        ]


renderFoo : Time.Month -> Html msg
renderFoo month =
    tr []
        [ td [] [ text (String.fromInt (cumulativeDaysByMonth False month)) ] ]


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
