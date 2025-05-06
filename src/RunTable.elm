module RunTable exposing (..)

import Html exposing (..)
import Time exposing (Month(..))


months : List Month
months =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


type alias M =
    { name : String
    , m : Month
    , days : Int
    , cDays : Int
    }


allMonths : Bool -> List M
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
    [ { name = "Jan", m = Jan, days = 31, cDays = 31 }
    , { name = "Feb", m = Feb, days = feb, cDays = 59 + adj }
    , { name = "Mar", m = Mar, days = 31, cDays = 90 + adj }
    , { name = "Apr", m = Apr, days = 30, cDays = 120 + adj }
    , { name = "May", m = May, days = 31, cDays = 151 + adj }
    , { name = "Jun", m = Jun, days = 30, cDays = 181 + adj }
    , { name = "Jul", m = Jul, days = 31, cDays = 212 + adj }
    , { name = "Aug", m = Aug, days = 31, cDays = 243 + adj }
    , { name = "Sep", m = Sep, days = 30, cDays = 273 + adj }
    , { name = "Oct", m = Oct, days = 31, cDays = 304 + adj }
    , { name = "Nov", m = Nov, days = 30, cDays = 334 + adj }
    , { name = "Dec", m = Dec, days = 31, cDays = 365 + adj }
    ]


renderLine : Bool -> Int -> M -> Html msg
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
        [ td [] [ text (m.name ++ " " ++ String.fromInt m.days) ]
        , td [] [ text (String.fromInt actC) ]
        , td [] [ text (String.fromInt (ceiling ((toFloat distance / y) * toFloat actC))) ]
        ]


renderTable : Bool -> Int -> Html msg
renderTable isLeap distance =
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
                (renderLine isLeap distance)
                (allMonths isLeap)
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
