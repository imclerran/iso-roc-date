## The Date module provides the `Date` type, as well as various functions for working with dates.
##
## These functions include functions for creating dates from varioius numeric values, converting dates to and from ISO 8601 strings, and performing arithmetic operations on dates.
module [
    Date,
    add_date_and_duration,
    add_days,
    add_duration_and_date,
    add_months,
    add_years,
    days_in_month,
    from_iso_str,
    from_iso_u8,
    from_nanos_since_epoch,
    from_yd,
    from_ymd,
    from_yw,
    from_ywd,
    is_leap_year,
    to_iso_str,
    to_iso_u8,
    to_nanos_since_epoch,
    unix_epoch,
    weekday,
]

import Const
import Duration exposing [Duration, to_nanoseconds, from_days]
import Utils exposing [
    expand_int_with_zeros,
    split_list_at_indices,
    utf8_to_int,
    utf8_to_int_signed,
    validate_utf8_single_bytes,
]
import Unsafe exposing [unwrap] # for unit testing only

## ```
## Date : {
##     year: I64,
##     month: U8,
##     day_of_month: U8,
##     day_of_year: U16
## }
## ```
Date : {
    year : I64,
    month : U8,
    day_of_month : U8,
    day_of_year : U16,
}

## `Date` object representing the Unix epoch (1970-01-01).
unix_epoch : Date
unix_epoch = { year: 1970, month: 1, day_of_month: 1, day_of_year: 1 }

## Create a `Date` object from the given year and day of the year.
from_yd : Int *, Int * -> Date
from_yd = |year, day_of_year|
    List.range({ start: At(1), end: At(12) })
    |> List.map(|m| Const.month_days({ month: Num.to_u64(m), is_leap: is_leap_year(year) }))
    |> List.walk_until({ days_remaining: Num.to_u16(day_of_year), month: 1 }, walk_until_month_func)
    |> |md| { year: Num.to_i64(year), month: Num.to_u8(md.month), day_of_month: Num.to_u8(md.days_remaining), day_of_year: Num.to_u16(day_of_year) }

## Check whether the given year is a leap year.
is_leap_year = |year|
    (
        year
        % Const.leap_interval
        == 0
        && year
        % Const.leap_exception
        != 0
    )
    || year
    % Const.leap_non_exception
    == 0

## Walk through the months of a year to find the month and day of the month
walk_until_month_func : { days_remaining : U16, month : U8 }, U64 -> [Break { days_remaining : U16, month : U8 }, Continue { days_remaining : U16, month : U8 }]
walk_until_month_func = \state, curr_month_days ->
    if state.days_remaining <= Num.to_u16(curr_month_days) then
        Break({ days_remaining: state.days_remaining, month: state.month })
    else
        Continue { days_remaining: state.days_remaining - Num.toU16 curr_month_days, month: state.month + 1 }

## Create a `Date` object from the given year, month, and day of the month.
from_ymd : Int *, Int *, Int * -> Date
from_ymd = |year, month, day|
    { year: Num.to_i64(year), month: Num.to_u8(month), day_of_month: Num.to_u8(day), day_of_year: ymd_to_days_in_year(year, month, day) }

## Convert the given year, month, and day of the month to the day of the year.
ymd_to_days_in_year : Int *, Int *, Int * -> U16
ymd_to_days_in_year = |year, month, day|
    List.range({ start: At(0), end: Before(month) })
    |> List.map(|m| Const.month_days({ month: Num.to_u64(m), is_leap: is_leap_year(year) }))
    |> List.sum
    |> Num.add (Num.toU64 day)
    |> Num.toU16

## Create a `Date` object from the given year, week, and day of the week.
from_ywd : Int *, Int *, Int * -> Date
from_ywd = |year, week, day|
    days_in_year = if is_leap_year(year) then 366 else 365
    d = calendar_week_to_days_in_year(week, year) |> Num.add(Num.to_u64(day))
    if d > days_in_year then
        from_yd (year + 1) (d - days_in_year)
    else
        from_yd year d

## Convert the given calendar week and year to the day of the year.
calendar_week_to_days_in_year : Int *, Int * -> U64
calendar_week_to_days_in_year = |week, year|
    # Week 1 of a year is the first week with a majority of its days in that year
    # https://en.wikipedia.org/wiki/ISO_week_date#First_week
    y = year |> Num.toU64
    w = week |> Num.toU64
    length_of_maybe_first_week =
        if y >= Const.epoch_year then
            Const.epoch_week_offset - (num_days_since_epoch_until_year (Num.toI64 y) |> Num.toU64) % 7
        else
            (Const.epoch_week_offset + (num_days_since_epoch_until_year (Num.toI64 y) |> Num.abs |> Num.toU64)) % 7
    if length_of_maybe_first_week >= 4 && w == 1 then
        0
    else
        (w - 1) * Const.days_per_week + length_of_maybe_first_week

## Calculate the number of leap years since the epoch.
num_leap_years_since_epoch : I64, [IncludeCurrent, ExcludeCurrent] -> I64
num_leap_years_since_epoch = |year, inclusive|
    leap_incr = is_leap_year(year) |> |is_leap| if is_leap && inclusive == IncludeCurrent then 1 else 0
    next_year = if year > Const.epoch_year then year - 1 else year + 1
    when inclusive is
        ExcludeCurrent if year != Const.epoch_year -> num_leap_years_since_epoch next_year IncludeCurrent
        ExcludeCurrent -> 0
        IncludeCurrent if year != Const.epoch_year -> leap_incr + num_leap_years_since_epoch next_year inclusive
        IncludeCurrent -> leap_incr

## Calculate the number of days since the epoch.
num_days_since_epoch : Date -> I64
num_days_since_epoch = |date|
    num_leap_years = num_leap_years_since_epoch(date.year, ExcludeCurrent)
    get_month_days = |m| Const.month_days({ month: m, is_leap: is_leap_year(date.year) })

    if date.year >= Const.epoch_year then
        days_in_years = num_leap_years * 366 + (date.year - Const.epoch_year - num_leap_years) * 365
        List.map (List.range { start: At 1, end: Before date.month }) get_month_days
        |> List.sum
        |> Num.toI64
        |> Num.add (days_in_years + Num.toI64 date.day_of_month - 1)
    else
        days_in_years = num_leap_years * 366 + (Const.epoch_year - date.year - num_leap_years - 1) * 365
        List.map (List.range { start: After date.month, end: At 12 }) get_month_days
        |> List.sum
        |> Num.toI64
        |> Num.add (days_in_years + Num.toI64 (get_month_days date.month) - Num.toI64 date.day_of_month + 1)
        |> Num.mul -1

## Calculate the number of days since the epoch until the given year.
num_days_since_epoch_until_year = |year|
    num_days_since_epoch({ year, month: 1, day_of_month: 1, day_of_year: 1 })

## Return the day of the week, from 0=Sunday to 6=Saturday
weekday : I64, U8, U8 -> U8
weekday = |year, month, day|
    year2xxx = (year % 400) + 2400 # to handle years before the epoch
    date = Date.from_ymd year2xxx month day
    days_since_epoch = Date.to_nanos_since_epoch date // Const.nanos_per_day
    (days_since_epoch + 4) % 7 |> Num.toU8

## Returns the number of days in the given month of the given year.
days_in_month : I64, U8 -> U8
days_in_month = |year, month|
    Const.month_days({ month, is_leap: is_leap_year(year) }) |> Num.to_u8

## Create a `Date` object from the given year and week.
from_yw : Int *, Int * -> Date
from_yw = |year, week|
    from_ywd(year, week, 1)

## Convert the given `Date` to nanoseconds since the epoch.
to_nanos_since_epoch : Date -> I128
to_nanos_since_epoch = |date|
    days = num_days_since_epoch(date)
    days |> Num.to_i128 |> Num.mul(Const.nanos_per_day)

## Create a `Date` object from the given nanoseconds since the epoch.
from_nanos_since_epoch : Int * -> Date
from_nanos_since_epoch = |nanos|
    days = nanos // Const.nanos_per_day |> |d| if nanos % Const.nanos_per_day < 0 then d - 1 else d
    from_nanos_helper(Num.to_i128(days), 1970)

from_nanos_helper : I128, I64 -> Date
from_nanos_helper = |days, year|
    if days < 0 then
        from_nanos_helper (days + if is_leap_year (year - 1) then 366 else 365) (year - 1)
    else
        days_in_year = if is_leap_year year then 366 else 365
        if days >= days_in_year then
            from_nanos_helper (days - days_in_year) (year + 1)
        else
            from_yd year (days + 1)

# TODO: allow for negative years
## Add the given number of years to the given `Date`.
add_years : Date, Int * -> Date
add_years = |date, years| from_ymd((date.year + Num.to_i64(years)), date.month, date.day_of_month)

# TODO: allow for negative months
## Add the given number of months to the given `Date`.
add_months : Date, Int * -> Date
add_months = |date, months|
    new_month_with_overflow = date.month + Num.to_u8(months)
    new_year = date.year + Num.to_i64((new_month_with_overflow // 12))
    new_month = new_month_with_overflow % 12
    new_day = (
        if date.day_of_month > Num.toU8 (Const.month_days { month: new_month, is_leap: is_leap_year new_year }) then
            Num.toU8 (Const.month_days { month: new_month, is_leap: is_leap_year new_year })
        else
            date.day_of_month
    )
    from_ymd new_year new_month new_day

## Add the given number of days to the given `Date`.
add_days : Date, Int * -> Date
add_days = |date, days|
    add_days_helper(date, Num.to_i16(days))

add_days_helper : Date, I16 -> Date
add_days_helper = |date, days|
    days_in_year = if is_leap_year(date.year) then 366 else 365
    new_day_of_year = (Num.to_i16(date.day_of_year)) + days

    if new_day_of_year > days_in_year then
        add_days_helper { year: date.year + 1, month: 1, day_of_month: 1, day_of_year: 0 } (new_day_of_year - days_in_year)
    else if new_day_of_year < 1 then
        days_in_prev_year = if is_leap_year (date.year - 1) then 366 else 365
        add_days_helper { year: date.year - 1, month: 12, day_of_month: 31, day_of_year: 0 } (new_day_of_year + Num.toI16 days_in_prev_year)
    else
        from_yd date.year new_day_of_year

## Add the given `Duration` to the given `Date`.
add_duration_and_date : Duration, Date -> Date
add_duration_and_date = |duration, date|
    duration_nanos = to_nanoseconds(duration)
    date_nanos = to_nanos_since_epoch(date) |> Num.to_i128
    duration_nanos + date_nanos |> from_nanos_since_epoch

## Add the given `Date` and `Duration`.
add_date_and_duration : Date, Duration -> Date
add_date_and_duration = |date, duration| add_duration_and_date(duration, date)

## Convert the given `Date` to an ISO 8601 string.
to_iso_str : Date -> Str
to_iso_str = |date|
    expand_int_with_zeros(date.year, 4)
    |> Str.concat("-")
    |> Str.concat(expand_int_with_zeros(date.month, 2))
    |> Str.concat("-")
    |> Str.concat(expand_int_with_zeros(date.day_of_month, 2))

## Convert the `Date` to an ISO 8601 string as a list of UTF-8 bytes.
to_iso_u8 : Date -> List U8
to_iso_u8 = |date| to_iso_str(date) |> Str.to_utf8

## Convert the given ISO 8601 string to a `Date`.
from_iso_str : Str -> Result Date [InvalidDateFormat]
from_iso_str = |str| Str.to_utf8(str) |> from_iso_u8

# TODO: More efficient parsing method?
## Convert the given ISO 8601 list of UTF-8 bytes to a `Date`.
from_iso_u8 : List U8 -> Result Date [InvalidDateFormat]
from_iso_u8 = |bytes|
    if validate_utf8_single_bytes(bytes) then
        when bytes is
            [_, _] -> parse_calendar_date_century bytes # YY
            [_, _, _, _] -> parse_calendar_date_year bytes # YYYY
            [_, _, _, _, 'W', _, _] -> parse_week_date_reduced_basic bytes # YYYYWww
            [_, _, _, _, '-', _, _] -> parse_calendar_date_month bytes # YYYY-MM
            [_, _, _, _, _, _, _] -> parse_ordinal_date_basic bytes # YYYYDDD
            [_, _, _, _, '-', 'W', _, _] -> parse_week_date_reduced_extended bytes # YYYY-Www
            [_, _, _, _, 'W', _, _, _] -> parse_week_date_basic bytes # YYYYWwwD
            [_, _, _, _, '-', _, _, _] -> parse_ordinal_date_extended bytes # YYYY-DDD
            [_, _, _, _, _, _, _, _] -> parse_calendar_date_basic bytes # YYYYMMDD
            [_, _, _, _, '-', 'W', _, _, '-', _] -> parse_week_date_extended bytes # YYYY-Www-D
            [_, _, _, _, '-', _, _, '-', _, _] -> parse_calendar_date_extended bytes # YYYY-MM-DD
            _ -> Err InvalidDateFormat
    else
        Err InvalidDateFormat

parse_calendar_date_basic : List U8 -> Result Date [InvalidDateFormat]
parse_calendar_date_basic = |bytes|
    when split_list_at_indices(bytes, [4, 6]) is
        [year_bytes, month_bytes, day_bytes] ->
            when (utf8_to_int year_bytes, utf8_to_int month_bytes, utf8_to_int day_bytes) is
                (Ok y, Ok m, Ok d) if m >= 1 && m <= 12 && d >= 1 && d <= 31 ->
                    Date.from_ymd y m d |> Ok

                (_, _, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

parse_calendar_date_extended : List U8 -> Result Date [InvalidDateFormat]
parse_calendar_date_extended = |bytes|
    when split_list_at_indices(bytes, [4, 5, 7, 8]) is
        [year_bytes, _, month_bytes, _, day_bytes] ->
            when (utf8_to_int_signed year_bytes, utf8_to_int month_bytes, utf8_to_int day_bytes) is
                (Ok y, Ok m, Ok d) if m >= 1 && m <= 12 && d >= 1 && d <= 31 ->
                    Date.from_ymd y m d |> Ok

                (_, _, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

parse_calendar_date_century : List U8 -> Result Date [InvalidDateFormat]
parse_calendar_date_century = |bytes|
    when utf8_to_int_signed(bytes) is
        Ok(century) -> Date.from_ymd((century * 100), 1, 1) |> Ok
        Err(_) -> Err(InvalidDateFormat)

parse_calendar_date_year : List U8 -> Result Date [InvalidDateFormat]
parse_calendar_date_year = |bytes|
    when utf8_to_int_signed(bytes) is
        Ok(year) -> Date.from_ymd(year, 1, 1) |> Ok
        Err(_) -> Err(InvalidDateFormat)

parse_calendar_date_month : List U8 -> Result Date [InvalidDateFormat]
parse_calendar_date_month = |bytes|
    when split_list_at_indices(bytes, [4, 5]) is
        [year_bytes, _, month_bytes] ->
            when (utf8_to_int_signed year_bytes, utf8_to_int month_bytes) is
                (Ok year, Ok month) if month >= 1 && month <= 12 ->
                    Date.from_ymd year month 1 |> Ok

                (_, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

parse_ordinal_date_basic : List U8 -> Result Date [InvalidDateFormat]
parse_ordinal_date_basic = |bytes|
    when split_list_at_indices(bytes, [4]) is
        [year_bytes, day_bytes] ->
            when (utf8_to_int_signed year_bytes, utf8_to_int day_bytes) is
                (Ok year, Ok day) if day >= 1 && day <= 366 ->
                    Date.from_yd year day |> Ok

                (_, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

parse_ordinal_date_extended : List U8 -> Result Date [InvalidDateFormat]
parse_ordinal_date_extended = |bytes|
    when split_list_at_indices(bytes, [4, 5]) is
        [year_bytes, _, day_bytes] ->
            when (utf8_to_int_signed year_bytes, utf8_to_int day_bytes) is
                (Ok year, Ok day) if day >= 1 && day <= 366 ->
                    Date.from_yd year day |> Ok

                (_, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

parse_week_date_basic : List U8 -> Result Date [InvalidDateFormat]
parse_week_date_basic = |bytes|
    when split_list_at_indices(bytes, [4, 5, 7]) is
        [year_bytes, _, week_bytes, day_bytes] ->
            when (utf8_to_int year_bytes, utf8_to_int week_bytes, utf8_to_int day_bytes) is
                (Ok y, Ok w, Ok d) if w >= 1 && w <= 52 && d >= 1 && d <= 7 ->
                    Date.from_ywd y w d |> Ok

                (_, _, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

parse_week_date_extended : List U8 -> Result Date [InvalidDateFormat]
parse_week_date_extended = |bytes|
    when split_list_at_indices(bytes, [4, 6, 8, 9]) is
        [year_bytes, _, week_bytes, _, day_bytes] ->
            when (utf8_to_int year_bytes, utf8_to_int week_bytes, utf8_to_int day_bytes) is
                (Ok y, Ok w, Ok d) if w >= 1 && w <= 52 && d >= 1 && d <= 7 ->
                    Date.from_ywd y w d |> Ok

                (_, _, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

parse_week_date_reduced_basic : List U8 -> Result Date [InvalidDateFormat]
parse_week_date_reduced_basic = |bytes|
    when split_list_at_indices(bytes, [4, 5]) is
        [year_bytes, _, week_bytes] ->
            when (utf8_to_int year_bytes, utf8_to_int week_bytes) is
                (Ok year, Ok week) if week >= 1 && week <= 52 ->
                    Date.from_yw year week |> Ok

                (_, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

parse_week_date_reduced_extended : List U8 -> Result Date [InvalidDateFormat]
parse_week_date_reduced_extended = |bytes|
    when split_list_at_indices(bytes, [4, 6]) is
        [year_bytes, _, week_bytes] ->
            when (utf8_to_int year_bytes, utf8_to_int week_bytes) is
                (Ok year, Ok week) if week >= 1 && week <= 52 ->
                    Date.from_yw year week |> Ok

                (_, _) -> Err InvalidDateFormat

        _ -> Err InvalidDateFormat

# <==== TESTS ====>
# <---- fromYd ---->
expect from_yd 1970 1 == { year: 1970, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_yd 1970 31 == { year: 1970, month: 1, day_of_month: 31, day_of_year: 31 }
expect from_yd 1970 32 == { year: 1970, month: 2, day_of_month: 1, day_of_year: 32 }
expect from_yd 1970 60 == { year: 1970, month: 3, day_of_month: 1, day_of_year: 60 }
expect from_yd 1972 61 == { year: 1972, month: 3, day_of_month: 1, day_of_year: 61 }

# <---- calendarWeekToDaysInYear ---->
expect calendar_week_to_days_in_year 1 1965 == 3
expect calendar_week_to_days_in_year 1 1964 == 0
expect calendar_week_to_days_in_year 1 1970 == 0
expect calendar_week_to_days_in_year 1 1971 == 3
expect calendar_week_to_days_in_year 1 1972 == 2
expect calendar_week_to_days_in_year 1 1973 == 0
expect calendar_week_to_days_in_year 2 2024 == 7

# <---- numDaysSinceEpoch ---->
expect num_days_since_epoch (from_ymd 2024 1 1) == 19723 # Removed due to compiler bug with optional record fields
expect num_days_since_epoch (from_ymd 1970 12 31) == 365 - 1
expect num_days_since_epoch (from_ymd 1971 1 2) == 365 + 1
expect num_days_since_epoch (from_ymd 2024 1 1) == 19723
expect num_days_since_epoch (from_ymd 2024 2 1) == 19723 + 31
expect num_days_since_epoch (from_ymd 2024 12 31) == 19723 + 366 - 1
expect num_days_since_epoch (from_ymd 1969 12 31) == -1
expect num_days_since_epoch (from_ymd 1969 12 30) == -2
expect num_days_since_epoch (from_ymd 1969 1 1) == -365
expect num_days_since_epoch (from_ymd 1968 1 1) == -365 - 366

# <---- numDaysSinceEpochToYear ---->
expect num_days_since_epoch_until_year 1968 == -365 - 366
expect num_days_since_epoch_until_year 1970 == 0
expect num_days_since_epoch_until_year 1971 == 365
expect num_days_since_epoch_until_year 1972 == 365 + 365
expect num_days_since_epoch_until_year 1973 == 365 + 365 + 366
expect num_days_since_epoch_until_year 2024 == 19723

# <---- fromYmd ---->
expect from_ymd 1970 1 1 == { year: 1970, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_ymd 1970 12 31 == { year: 1970, month: 12, day_of_month: 31, day_of_year: 365 }
expect from_ymd 1972 3 1 == { year: 1972, month: 3, day_of_month: 1, day_of_year: 61 }

# <---- fromYwd ---->
expect from_ywd 1970 1 1 == { year: 1970, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_ywd 1970 52 5 == { year: 1971, month: 1, day_of_month: 1, day_of_year: 1 }

# <---- fromYw ---->
expect from_yw 1970 1 == { year: 1970, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_yw 1971 1 == { year: 1971, month: 1, day_of_month: 4, day_of_year: 4 }

# <---- fromNanosSinceEpoch ---->
expect from_nanos_since_epoch 0 == { year: 1970, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_nanos_since_epoch (Const.nanos_per_day * 365) == { year: 1971, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_nanos_since_epoch (Const.nanos_per_day * 365 * 2 + Const.nanos_per_day * 366) == { year: 1973, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_nanos_since_epoch (-Const.nanos_per_day) == { year: 1969, month: 12, day_of_month: 31, day_of_year: 365 }
expect from_nanos_since_epoch ((-Const.nanos_per_day) * 365) == { year: 1969, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_nanos_since_epoch ((-Const.nanos_per_day) * 365 - Const.nanos_per_day * 366) == { year: 1968, month: 1, day_of_month: 1, day_of_year: 1 }
expect from_nanos_since_epoch -1 == { year: 1969, month: 12, day_of_month: 31, day_of_year: 365 }

# <---- toNanosSinceEpoch ---->
expect to_nanos_since_epoch { year: 1970, month: 1, day_of_month: 1, day_of_year: 1 } == 0
expect to_nanos_since_epoch { year: 1970, month: 12, day_of_month: 31, day_of_year: 365 } == Const.nanos_per_hour * 24 * 364
expect to_nanos_since_epoch { year: 1973, month: 1, day_of_month: 1, day_of_year: 1 } == Const.nanos_per_hour * 24 * 365 * 2 + Const.nanos_per_hour * 24 * 366
expect to_nanos_since_epoch { year: 1969, month: 12, day_of_month: 31, day_of_year: 365 } == Const.nanos_per_hour * 24 * -1
expect to_nanos_since_epoch { year: 1969, month: 1, day_of_month: 1, day_of_year: 1 } == Const.nanos_per_hour * 24 * -365
expect to_nanos_since_epoch { year: 1968, month: 1, day_of_month: 1, day_of_year: 1 } == Const.nanos_per_hour * 24 * -365 - Const.nanos_per_hour * 24 * 366

# <---- toIsoStr ---->
expect to_iso_str unix_epoch == "1970-01-01"

# <---- addMonths ---->
expect add_months unix_epoch 12 == from_ymd 1971 1 1
expect add_months (from_ymd 1970 1 31) 1 == from_ymd 1970 2 28
expect add_months (from_ymd 1972 2 29) 12 == from_ymd 1973 2 28

# <---- addDays ---->
expect add_days unix_epoch 365 == from_ymd 1971 1 1
expect add_days unix_epoch (365 * 2) == from_ymd 1972 1 1
expect add_days unix_epoch (365 * 2 + 366) == from_ymd 1973 1 1
expect add_days unix_epoch (-1) == from_ymd 1969 12 31
expect add_days unix_epoch (-365) == from_ymd 1969 1 1
expect add_days unix_epoch (-365 - 1) == from_ymd 1968 12 31
expect add_days unix_epoch (-365 - 366) == from_ymd 1968 1 1

# <---- addDateAndDuration ---->
expect add_date_and_duration unix_epoch (from_days 1 |> unwrap "will not overflow") == from_ymd 1970 1 2

# <---- ymdToDaysInYear ---->
expect ymd_to_days_in_year 1970 1 1 == 1
expect ymd_to_days_in_year 1970 12 31 == 365
expect ymd_to_days_in_year 1972 3 1 == 61

# <---- weekday ---->
expect weekday 1964 10 10 == 6
expect weekday 1964 10 11 == 0
expect weekday 1964 10 12 == 1
expect weekday 2024 10 12 == 6

# <---- daysInMonth ---->
expect days_in_month 1969 1 == 31
expect days_in_month 1969 2 == 28
expect days_in_month 1969 3 == 31
expect days_in_month 1969 4 == 30
expect days_in_month 1969 5 == 31
expect days_in_month 1969 6 == 30
expect days_in_month 1969 7 == 31
expect days_in_month 1969 8 == 31
expect days_in_month 1969 9 == 30
expect days_in_month 1969 10 == 31
expect days_in_month 1969 11 == 30
expect days_in_month 1969 12 == 31
expect days_in_month 2024 2 == 29
