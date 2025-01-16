## The Time module provides the `Time` type as well as functions for working with time values.
##
## These functions include functions for creating `Time` objects from various numeric values, converting `Time`s to and from ISO 8601 strings, and performing arithmetic operations on `Time`s.
module [
    add_duration_and_time,
    add_hours,
    add_minutes,
    add_nanoseconds,
    add_seconds,
    add_time_and_duration,
    from_hms,
    from_hmsn,
    from_iso_str,
    from_iso_u8,
    from_nanos_since_midnight,
    midnight,
    normalize,
    Time,
    to_iso_str,
    to_iso_u8,
    to_nanos_since_midnight,
]

import Const
import Const exposing [
    nanos_per_hour,
    nanos_per_minute,
    nanos_per_second,
]
import Duration
import Duration exposing [Duration]
import Utils exposing [
    expand_int_with_zeros,
    split_list_at_indices,
    split_utf8_and_keep_delimiters,
    utf8_to_frac,
    utf8_to_int_signed,
    validate_utf8_single_bytes,
]
import Unsafe exposing [unwrap] # for unit testing only

## ```
## Time : {
##     hour : I8,
##     minute : U8,
##     second : U8,
##     nanosecond : U32
## }
## ```
Time : { hour : I8, minute : U8, second : U8, nanosecond : U32 }

## `Time` object representing 00:00:00.
midnight : Time
midnight = { hour: 0, minute: 0, second: 0, nanosecond: 0 }

normalize : Time -> Time
normalize = |time|
    h_normalized = time.hour |> Num.rem(Const.hours_per_day) |> Num.add(Const.hours_per_day) |> Num.rem(Const.hours_per_day)
    from_hmsn(h_normalized, time.minute, time.second, time.nanosecond)

expect Time.normalize (from_hms -1 0 0) == from_hms 23 0 0
expect Time.normalize (from_hms 24 0 0) == from_hms 0 0 0
expect Time.normalize (from_hms 25 0 0) == from_hms 1 0 0

## Create a `Time` object from the hour, minute, and second.
from_hms : Int *, Int *, Int * -> Time
from_hms = |hour, minute, second| { hour: Num.to_i8(hour), minute: Num.to_u8(minute), second: Num.to_u8(second), nanosecond: 0u32 }

## Create a `Time` object from the hour, minute, second, and nanosecond.
from_hmsn : Int *, Int *, Int *, Int * -> Time
from_hmsn = |hour, minute, second, nanosecond|
    { hour: Num.to_i8(hour), minute: Num.to_u8(minute), second: Num.to_u8(second), nanosecond: Num.to_u32(nanosecond) }

## Convert nanoseconds since midnight to a `Time` object.
to_nanos_since_midnight : Time -> I64
to_nanos_since_midnight = |time|
    h_nanos = time.hour |> Num.to_i64 |> Num.mul(Const.nanos_per_hour) |> Num.to_i64
    m_nanos = time.minute |> Num.to_i64 |> Num.mul(Const.nanos_per_minute) |> Num.to_i64
    s_nanos = time.second |> Num.to_i64 |> Num.mul(Const.nanos_per_second) |> Num.to_i64
    nanos = time.nanosecond |> Num.to_i64
    h_nanos + m_nanos + s_nanos + nanos

## Convert a `Time` object to the number of nanoseconds since midnight.
from_nanos_since_midnight : Int * -> Time
from_nanos_since_midnight = |nanos|
    nanos1 = nanos |> Num.rem(Const.nanos_per_day) |> Num.add(Const.nanos_per_day) |> Num.rem(Const.nanos_per_day) |> Num.to_u64
    nanos2 = nanos1 % nanos_per_hour
    minute = nanos2 // nanos_per_minute |> Num.toU8
    nanos3 = nanos2 % nanos_per_minute
    second = nanos3 // nanos_per_second |> Num.toU8
    nanosecond = nanos3 % nanos_per_second |> Num.toU32
    hour = (nanos - Num.intCast (Num.toI64 minute * nanos_per_minute + Num.toI64 second * nanos_per_second + Num.toI64 nanosecond)) // nanos_per_hour |> Num.toI8 # % Const.hoursPerDay |> Num.toI8
    { hour, minute, second, nanosecond }

## Add nanoseconds to a `Time` object.
add_nanoseconds : Time, Int * -> Time
add_nanoseconds = |time, nanos|
    to_nanos_since_midnight(time) + Num.to_i64(nanos) |> from_nanos_since_midnight

## Add seconds to a `Time` object.
add_seconds : Time, Int * -> Time
add_seconds = |time, seconds| add_nanoseconds(time, (seconds * Const.nanos_per_second))

## Add minutes to a `Time` object.
add_minutes : Time, Int * -> Time
add_minutes = |time, minutes| add_nanoseconds(time, (minutes * Const.nanos_per_minute))

## Add hours to a `Time` object.
add_hours : Time, Int * -> Time
add_hours = |time, hours| add_nanoseconds(time, (hours * Const.nanos_per_hour))

## Add a `Duration` object to a `Time` object.
add_duration_and_time : Duration, Time -> Time
add_duration_and_time = |duration, time|
    duration_nanos = Duration.to_nanoseconds(duration)
    time_nanos = to_nanos_since_midnight(time) |> Num.to_i128
    (duration_nanos + time_nanos) |> from_nanos_since_midnight

## Add a `Time` object to a `Duration` object.
add_time_and_duration : Time, Duration -> Time
add_time_and_duration = |time, duration| add_duration_and_time(duration, time)

strip_tand_z : List U8 -> List U8
strip_tand_z = |bytes|
    when bytes is
        ['T', .. as tail] -> strip_tand_z tail
        [.. as head, 'Z'] -> head
        _ -> bytes

## Convert a `Time` object to an ISO 8601 string.
to_iso_str : Time -> Str
to_iso_str = |time|
    expand_int_with_zeros(time.hour, 2)
    |> Str.concat(":")
    |> Str.concat(expand_int_with_zeros(time.minute, 2))
    |> Str.concat(":")
    |> Str.concat(expand_int_with_zeros(time.second, 2))
    |> Str.concat(nanos_to_frac_str(time.nanosecond))

nanos_to_frac_str : U32 -> Str
nanos_to_frac_str = |nanos|
    length = count_frac_width(nanos, 9)
    untrimmed_str = (if nanos == 0 then "" else Str.concat(",", expand_int_with_zeros(nanos, length)))
    when untrimmed_str |> Str.to_utf8 |> List.take_first((length + 1)) |> Str.from_utf8 is
        Ok(str) -> str
        Err(_) -> untrimmed_str

count_frac_width : U32, Int _ -> Int _
count_frac_width = |num, width|
    if num == 0 then
        0
    else if num % 10 == 0 then
        count_frac_width (num // 10) (width - 1)
    else
        width

## Convert a `Time` object to an ISO 8601 list of UTF-8 bytes.
to_iso_u8 : Time -> List U8
to_iso_u8 = |time| to_iso_str(time) |> Str.to_utf8

## Convert an ISO 8601 string to a `Time` object.
from_iso_str : Str -> Result Time [InvalidTimeFormat]
from_iso_str = |str| Str.to_utf8(str) |> from_iso_u8

## Convert an ISO 8601 list of UTF-8 bytes to a `Time` object.
from_iso_u8 : List U8 -> Result Time [InvalidTimeFormat]
from_iso_u8 = |bytes|
    if validate_utf8_single_bytes(bytes) then
        stripped_bytes = strip_tand_z(bytes)
        when (split_utf8_and_keep_delimiters(stripped_bytes, ['.', ',', '+', '-']), List.last(bytes)) is
            # time.fractionaltime+timeoffset / time,fractionaltime-timeoffset
            ([time_bytes, [byte1], fractional_bytes, [byte2], offset_bytes], Ok last_byte) if last_byte != 'Z' ->
                time_res = parse_fractional_time time_bytes (List.join [[byte1], fractional_bytes])
                offset_res = parse_time_offset (List.join [[byte2], offset_bytes])
                combine_time_and_offset_results time_res offset_res

            # time+timeoffset / time-timeoffset
            ([time_bytes, [byte1], offset_bytes], Ok last_byte) if (byte1 == '+' || byte1 == '-') && last_byte != 'Z' ->
                time_res = parse_whole_time time_bytes
                offset_res = parse_time_offset (List.join [[byte1], offset_bytes])
                combine_time_and_offset_results time_res offset_res

            # time.fractionaltime / time,fractionaltime
            ([time_bytes, [byte1], fractional_bytes], _) if byte1 == ',' || byte1 == '.' ->
                parse_fractional_time time_bytes (List.join [[byte1], fractional_bytes])

            # time
            ([time_bytes], _) -> parse_whole_time time_bytes
            _ -> Err InvalidTimeFormat
    else
        Err InvalidTimeFormat

combine_time_and_offset_results = |time_res, offset_res|
    when (time_res, offset_res) is
        (Ok time, Ok offset) ->
            Time.add_time_and_duration time offset |> Ok

        (_, _) -> Err InvalidTimeFormat

parse_whole_time : List U8 -> Result Time [InvalidTimeFormat]
parse_whole_time = |bytes|
    when bytes is
        [_, _] -> parse_local_time_hour bytes # hh
        [_, _, _, _] -> parse_local_time_minute_basic bytes # hhmm
        [_, _, ':', _, _] -> parse_local_time_minute_extended bytes # hh:mm
        [_, _, _, _, _, _] -> parse_local_time_basic bytes # hhmmss
        [_, _, ':', _, _, ':', _, _] -> parse_local_time_extended bytes # hh:mm:ss
        _ -> Err InvalidTimeFormat

parse_fractional_time : List U8, List U8 -> Result Time [InvalidTimeFormat]
parse_fractional_time = |whole_bytes, fractional_bytes|
    combine_duration_res_and_time = |duration_res, time|
        when duration_res is
            Ok duration -> Time.add_time_and_duration time duration |> Ok
            Err _ -> Err InvalidTimeFormat
    when (whole_bytes, utf8_to_frac fractional_bytes) is
        ([_, _], Ok frac) -> # hh
            time = parse_local_time_hour? whole_bytes
            frac * Const.nanos_per_hour |> Num.round |> Duration.from_nanoseconds |> combine_duration_res_and_time time

        ([_, _, _, _], Ok frac) -> # hhmm
            time = parse_local_time_minute_basic? whole_bytes
            frac * Const.nanos_per_minute |> Num.round |> Duration.from_nanoseconds |> combine_duration_res_and_time time

        ([_, _, ':', _, _], Ok frac) -> # hh:mm
            time = parse_local_time_minute_extended? whole_bytes
            frac * Const.nanos_per_minute |> Num.round |> Duration.from_nanoseconds |> combine_duration_res_and_time time

        ([_, _, _, _, _, _], Ok frac) -> # hhmmss
            time = parse_local_time_basic? whole_bytes
            frac * Const.nanos_per_second |> Num.round |> Duration.from_nanoseconds |> combine_duration_res_and_time time

        ([_, _, ':', _, _, ':', _, _], Ok frac) -> # hh:mm:ss
            time = parse_local_time_extended? whole_bytes
            frac * Const.nanos_per_second |> Num.round |> Duration.from_nanoseconds |> combine_duration_res_and_time time

        _ -> Err InvalidTimeFormat

parse_time_offset : List U8 -> Result Duration [InvalidTimeFormat]
parse_time_offset = |bytes|
    when bytes is
        ['-', h1, h2] ->
            parse_time_offset_help h1 h2 '0' '0' 1

        ['+', h1, h2] ->
            parse_time_offset_help h1 h2 '0' '0' -1

        ['-', h1, h2, m1, m2] ->
            parse_time_offset_help h1 h2 m1 m2 1

        ['+', h1, h2, m1, m2] ->
            parse_time_offset_help h1 h2 m1 m2 -1

        ['-', h1, h2, ':', m1, m2] ->
            parse_time_offset_help h1 h2 m1 m2 1

        ['+', h1, h2, ':', m1, m2] ->
            parse_time_offset_help h1 h2 m1 m2 -1

        _ -> Err InvalidTimeFormat

parse_time_offset_help : U8, U8, U8, U8, I64 -> Result Duration [InvalidTimeFormat]
parse_time_offset_help = |h1, h2, m1, m2, sign|
    is_valid_offset = |offset| if offset >= -14 * Const.nanos_per_hour && offset <= 12 * Const.nanos_per_hour then Valid else Invalid
    when (utf8_to_int_signed([h1, h2]), utf8_to_int_signed([m1, m2])) is
        (Ok(hour), Ok(minute)) ->
            offset_nanos = sign * (hour * Const.nanos_per_hour + minute * Const.nanos_per_minute)
            when is_valid_offset(offset_nanos) is
                Valid -> Duration.from_nanoseconds(offset_nanos) |> Result.map_err(|_| InvalidTimeFormat)
                Invalid -> Err(InvalidTimeFormat)

        (_, _) -> Err InvalidTimeFormat

parse_local_time_hour : List U8 -> Result Time [InvalidTimeFormat]
parse_local_time_hour = |bytes|
    when utf8_to_int_signed(bytes) is
        Ok(hour) if hour >= 0 && hour <= 24 ->
            Time.from_hms(hour, 0, 0) |> Ok

        Ok _ -> Err InvalidTimeFormat
        Err _ -> Err InvalidTimeFormat

parse_local_time_minute_basic : List U8 -> Result Time [InvalidTimeFormat]
parse_local_time_minute_basic = |bytes|
    when split_list_at_indices(bytes, [2]) is
        [hour_bytes, minute_bytes] ->
            when (utf8_to_int_signed hour_bytes, utf8_to_int_signed minute_bytes) is
                (Ok hour, Ok minute) if hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 ->
                    Time.from_hms hour minute 0 |> Ok

                (Ok 24, Ok 0) ->
                    Time.from_hms 24 0 0 |> Ok

                (_, _) -> Err InvalidTimeFormat

        _ -> Err InvalidTimeFormat

parse_local_time_minute_extended : List U8 -> Result Time [InvalidTimeFormat]
parse_local_time_minute_extended = |bytes|
    when split_list_at_indices(bytes, [2, 3]) is
        [hour_bytes, _, minute_bytes] ->
            when (utf8_to_int_signed hour_bytes, utf8_to_int_signed minute_bytes) is
                (Ok hour, Ok minute) if hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 ->
                    Time.from_hms hour minute 0 |> Ok

                (Ok 24, Ok 0) ->
                    Time.from_hms 24 0 0 |> Ok

                (_, _) -> Err InvalidTimeFormat

        _ -> Err InvalidTimeFormat

parse_local_time_basic : List U8 -> Result Time [InvalidTimeFormat]
parse_local_time_basic = |bytes|
    when split_list_at_indices(bytes, [2, 4]) is
        [hour_bytes, minute_bytes, second_bytes] ->
            when (utf8_to_int_signed hour_bytes, utf8_to_int_signed minute_bytes, utf8_to_int_signed second_bytes) is
                (Ok h, Ok m, Ok s) if h >= 0 && h <= 23 && m >= 0 && m <= 59 && s >= 0 && s <= 59 ->
                    Time.from_hms h m s |> Ok

                (Ok 24, Ok 0, Ok 0) ->
                    Time.from_hms 24 0 0 |> Ok

                (_, _, _) -> Err InvalidTimeFormat

        _ -> Err InvalidTimeFormat

parse_local_time_extended : List U8 -> Result Time [InvalidTimeFormat]
parse_local_time_extended = |bytes|
    when split_list_at_indices(bytes, [2, 3, 5, 6]) is
        [hour_bytes, _, minute_bytes, _, second_bytes] ->
            when (utf8_to_int_signed hour_bytes, utf8_to_int_signed minute_bytes, utf8_to_int_signed second_bytes) is
                (Ok h, Ok m, Ok s) if h >= 0 && h <= 23 && m >= 0 && m <= 59 && s >= 0 && s <= 59 ->
                    Time.from_hms h m s |> Ok

                (Ok 24, Ok 0, Ok 0) ->
                    Time.from_hms 24 0 0 |> Ok

                (_, _, _) -> Err InvalidTimeFormat

        _ -> Err InvalidTimeFormat

# <===== TESTS ====>
# <---- addNanoseconds ---->
expect add_nanoseconds (from_hmsn 12 34 56 5) Const.nanos_per_second == from_hmsn 12 34 57 5
expect add_nanoseconds (from_hmsn 12 34 56 5) -Const.nanos_per_second == from_hmsn 12 34 55 5

# <---- addSeconds ---->
expect add_seconds (from_hms 12 34 56) 59 == from_hms 12 35 55
expect add_seconds (from_hms 12 34 56) -59 == from_hms 12 33 57

# <---- addMinutes ---->
expect add_minutes (from_hms 12 34 56) 59 == from_hms 13 33 56
expect add_minutes (from_hms 12 34 56) -59 == from_hms 11 35 56

# <---- addHours ---->
expect add_hours (from_hms 12 34 56) 1 == from_hms 13 34 56
expect add_hours (from_hms 12 34 56) -1 == from_hms 11 34 56
expect add_hours (from_hms 12 34 56) 12 == from_hms 24 34 56

# <---- addTimeAndDuration ---->
expect
    add_time_and_duration (from_hms 0 0 0) (Duration.from_hours 1 |> unwrap "will not overflow") == from_hms 1 0 0

# <---- fromNanosSinceMidnight ---->
expect from_nanos_since_midnight -123 == from_hmsn -1 59 59 999_999_877
expect from_nanos_since_midnight 0 == midnight
expect from_nanos_since_midnight (24 * Const.nanos_per_hour) == from_hms 24 0 0
expect from_nanos_since_midnight (25 * nanos_per_hour) == from_hms 25 0 0
expect from_nanos_since_midnight (12 * nanos_per_hour + 34 * nanos_per_minute + 56 * nanos_per_second + 5) == from_hmsn 12 34 56 5

# <---- toIsoStr ---->
expect to_iso_str (from_hmsn 12 34 56 5) == "12:34:56,000000005"
expect to_iso_str midnight == "00:00:00"
expect
    str = to_iso_str (from_hmsn 0 0 0 500_000_000)
    str == "00:00:00,5"

# <---- fromNanosSinceMidnight ---->
expect from_nanos_since_midnight -123 == from_hmsn -1 59 59 999_999_877
expect from_nanos_since_midnight 0 == midnight
expect from_nanos_since_midnight (24 * Const.nanos_per_hour) == from_hms 24 0 0
expect from_nanos_since_midnight (25 * Const.nanos_per_hour) == from_hms 25 0 0

# <---- toNanosSinceMidnight ---->
expect to_nanos_since_midnight { hour: 12, minute: 34, second: 56, nanosecond: 5 } == 12 * nanos_per_hour + 34 * nanos_per_minute + 56 * nanos_per_second + 5
expect to_nanos_since_midnight (from_hmsn 12 34 56 5) == 12 * Const.nanos_per_hour + 34 * Const.nanos_per_minute + 56 * Const.nanos_per_second + 5
expect to_nanos_since_midnight (from_hmsn -1 0 0 0) == -1 * Const.nanos_per_hour
