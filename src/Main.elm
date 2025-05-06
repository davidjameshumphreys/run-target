module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
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
    , isLeap : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
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
            let
                year =
                    Time.toYear model.zone newTime

                isLeap =
                    (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))
            in
            ( { model | time = newTime, isLeap = isLeap }
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
    div []
        [ renderTable model.isLeap 1000
        ]


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
