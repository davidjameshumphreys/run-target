module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
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
    , currentMarker : Marker
    , isLeap : Bool
    , distance : Int
    }


{-| -}
type alias Marker =
    { m : Month
    , days : Int
    , cDays : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        zone =
            Time.utc

        time =
            Time.millisToPosix 0
    in
    ( Model zone time (asMarker zone time) False 1000
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | AdjustDistance Int


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
            ( { model
                | time = newTime
                , isLeap = isLeap
                , currentMarker = asMarker model.zone newTime
              }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        AdjustDistance newDistance ->
            ( { model | distance = newDistance }
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
        [ renderTable model ]


asMarker : Zone -> Posix -> Marker
asMarker zone time =
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
    { m = month
    , days = day
    , cDays = daysBeforeMonth + day
    }


{-| List all Months as Markers

Calculates the cumulativeDays, accounting for the leap year

-}
allMonths : Bool -> List Marker
allMonths isLeap =
    let
        adj =
            if isLeap then
                1

            else
                0

        feb =
            if isLeap then
                29

            else
                28
    in
    [ { m = Jan, days = 31, cDays = 31 }
    , { m = Feb, days = feb, cDays = 59 + adj }
    , { m = Mar, days = 31, cDays = 90 + adj }
    , { m = Apr, days = 30, cDays = 120 + adj }
    , { m = May, days = 31, cDays = 151 + adj }
    , { m = Jun, days = 30, cDays = 181 + adj }
    , { m = Jul, days = 31, cDays = 212 + adj }
    , { m = Aug, days = 31, cDays = 243 + adj }
    , { m = Sep, days = 30, cDays = 273 + adj }
    , { m = Oct, days = 31, cDays = 304 + adj }
    , { m = Nov, days = 30, cDays = 334 + adj }
    , { m = Dec, days = 31, cDays = 365 + adj }
    ]


renderLine : Bool -> Int -> Marker -> Html msg
renderLine isLeap distance m =
    let
        actC =
            m.cDays

        y =
            if isLeap then
                366

            else
                365
    in
    tr []
        [ td [] [ text (monthName m.m ++ " " ++ String.fromInt m.days) ]
        , td [] [ text (String.fromInt actC) ]
        , td [] [ text (String.fromInt (ceiling ((toFloat distance / y) * toFloat actC))) ]
        ]


renderTable : Model -> Html msg
renderTable model =
    table []
        [ thead []
            [ tr []
                [ td []
                    [ text "Month" ]
                , td []
                    [ text "Cumulative days" ]
                , td []
                    [ text "Distance" ]
                ]
            ]
        , tbody []
            (List.map
                (renderLine model.isLeap model.distance)
                (allMonths model.isLeap)
            )
        ]


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


monthName : Month -> String
monthName month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
