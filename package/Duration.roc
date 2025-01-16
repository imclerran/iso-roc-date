## The duration modules provides the `Duration` type and associated functions for representing time durations and performing date/time arithmetic.
module [
    add_durations,
    Duration,
    from_days,
    from_hours,
    from_minutes,
    from_nanoseconds,
    from_seconds,
    to_days,
    to_hours,
    to_minutes,
    to_nanoseconds,
    to_seconds,
]

import Const
import Unsafe exposing [unwrap] # for unit testing only

## ```
## Duration : {
##     days : I64,
##     hours : I8,
##     minutes : I8,
##     seconds : I8,
##     nanoseconds : I32
## }
## ```
Duration : { days : I64, hours : I8, minutes : I8, seconds : I8, nanoseconds : I32 }

## Create a `Duration` object from nanoseconds.
from_nanoseconds : Int * -> Result Duration [DurationOverflow]
from_nanoseconds = |nanos|
    if
        (nanos // Const.nanos_per_day |> Num.toI128) > (Num.maxI64 |> Num.toI128)
    then
        Err DurationOverflow
    else
        Ok {
            days: nanos // (Const.nanos_per_day) |> Num.toI64,
            hours: (nanos % (Const.nanos_per_day)) // Const.nanos_per_hour |> Num.toI8,
            minutes: (nanos % Const.nanos_per_hour) // Const.nanos_per_minute |> Num.toI8,
            seconds: (nanos % Const.nanos_per_minute) // Const.nanos_per_second |> Num.toI8,
            nanoseconds: nanos % Const.nanos_per_second |> Num.toI32,
        }

## Convert a `Duration` object to nanoseconds.
to_nanoseconds : Duration -> I128
to_nanoseconds = |duration|
    (Num.to_i128(duration.nanoseconds))
    + (Num.to_i128(duration.seconds))
    * Const.nanos_per_second
    + (Num.toI128 duration.minutes)
    * Const.nanos_per_minute
    + (Num.toI128 duration.hours)
    * Const.nanos_per_hour
    + (Num.toI128 duration.days)
    * (Const.nanos_per_hour * 24)

## Create a `Duration` object from seconds.
from_seconds : Int * -> Result Duration [DurationOverflow]
from_seconds = |seconds|
    if
        (seconds // Const.seconds_per_day |> Num.toI128) > (Num.maxI64 |> Num.toI128)
    then
        Err DurationOverflow
    else
        Ok {
            days: seconds // (Const.seconds_per_day) |> Num.toI64,
            hours: (seconds % (Const.seconds_per_day)) // Const.seconds_per_hour |> Num.toI8,
            minutes: (seconds % Const.seconds_per_hour) // Const.seconds_per_minute |> Num.toI8,
            seconds: seconds % Const.seconds_per_minute |> Num.toI8,
            nanoseconds: 0,
        }

## Convert a `Duration` object to seconds (truncates nanoseconds).
to_seconds : Duration -> I128
to_seconds = |duration|
    (Num.to_i128(duration.seconds))
    + (Num.to_i128(duration.minutes))
    * Const.seconds_per_minute
    + (Num.toI128 duration.hours)
    * Const.seconds_per_hour
    + (Num.toI128 duration.days)
    * (Const.seconds_per_hour * 24)

## Create a `Duration` object from minutes.
from_minutes : Int * -> Result Duration [DurationOverflow]
from_minutes = |minutes|
    if
        (minutes // Const.minutes_per_day |> Num.toI128) > (Num.maxI64 |> Num.toI128)
    then
        Err DurationOverflow
    else
        Ok {
            days: minutes // (Const.minutes_per_day) |> Num.toI64,
            hours: (minutes % (Const.minutes_per_day)) // Const.minutes_per_hour |> Num.toI8,
            minutes: minutes % Const.minutes_per_hour |> Num.toI8,
            seconds: 0,
            nanoseconds: 0,
        }

## Convert a `Duration` object to minutes (truncates seconds and lower).
to_minutes : Duration -> I128
to_minutes = |duration|
    (Num.to_i128(duration.minutes))
    + (Num.to_i128(duration.hours))
    * Const.minutes_per_hour
    + (Num.toI128 duration.days)
    * (Const.minutes_per_hour * 24)

## Create a `Duration` object from hours.
from_hours : Int * -> Result Duration [DurationOverflow]
from_hours = |hours|
    if
        (hours // 24 |> Num.toI128) > (Num.maxI64 |> Num.toI128)
    then
        Err DurationOverflow
    else
        Ok {
            days: hours // 24 |> Num.toI64,
            hours: hours % 24 |> Num.toI8,
            minutes: 0,
            seconds: 0,
            nanoseconds: 0,
        }

## Convert a `Duration` object to hours (truncates minutes and lower).
to_hours : Duration -> I128
to_hours = |duration|
    (Num.to_i128(duration.hours))
    + (Num.to_i128(duration.days))
    * 24

## Create a `Duration` object from days.
from_days : Int * -> Result Duration [DurationOverflow]
from_days = |days|
    if
        days |> Num.toI128 > Num.maxI64 |> Num.toI128
    then
        Err DurationOverflow
    else
        Ok {
            days: days |> Num.toI64,
            hours: 0,
            minutes: 0,
            seconds: 0,
            nanoseconds: 0,
        }

## Convert a `Duration` object to days (truncates hours and lower).
to_days : Duration -> I64
to_days = |duration| duration.days

## Add two `Duration` objects.
add_durations : Duration, Duration -> Result Duration [DurationOverflow]
add_durations = |d1, d2|
    nanos1 = to_nanoseconds(d1)
    nanos2 = to_nanoseconds(d2)
    from_nanoseconds((nanos1 + nanos2))

expect
    days = Num.maxI64
    duration = from_days days |> unwrap "will not overflow"
    duration |> to_days == days

expect
    d1 = from_days(Num.maxI64 // 2) |> unwrap "will not overflow"
    d2 = from_days(Num.maxI64 // 2) |> unwrap "will not overflow"
    d3 = from_days((Num.maxI64 // 2) * 2) |> unwrap "will not overflow"
    add_durations d1 d2 == Ok d3

expect
    d1 = from_days Num.minI64 |> unwrap "will not overflow"
    d2 = from_days Num.maxI64 |> unwrap "will not overflow"
    d3 = from_days -1 |> unwrap "will not overflow"
    add_durations d1 d2 == Ok d3

expect
    duration = from_days Num.maxI64 |> unwrap "will not overflow"
    add_durations duration duration == Err DurationOverflow

