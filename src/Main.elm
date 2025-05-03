module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Task
import Time exposing (Month(..), Posix, Zone)



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


{-| Cumulative days at the start of each month (days before the first day of the month)
-}
cumulativeDaysByMonth : Bool -> Month -> Int
cumulativeDaysByMonth isLeap month =
    let
        days =
            case month of
                Jan ->
                    0

                Feb ->
                    31

                Mar ->
                    59

                Apr ->
                    90

                May ->
                    120

                Jun ->
                    151

                Jul ->
                    181

                Aug ->
                    212

                Sep ->
                    243

                Oct ->
                    273

                Nov ->
                    304

                Dec ->
                    334

        leapAdjustment =
            if isLeap then
                case month of
                    Jan ->
                        0

                    Feb ->
                        0

                    _ ->
                        1

            else
                0
    in
    days + leapAdjustment
