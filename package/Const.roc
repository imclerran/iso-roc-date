module [
    days_per_leap_year,
    days_per_non_leap_year,
    days_per_week,
    epoch_day,
    epoch_month,
    epoch_week_offset,
    epoch_year,
    hours_per_day,
    leap_exception,
    leap_interval,
    leap_non_exception,
    minutes_per_day,
    minutes_per_hour,
    month_days,
    nanos_per_day,
    nanos_per_hour,
    nanos_per_milli,
    nanos_per_minute,
    nanos_per_second,
    nanos_per_second,
    seconds_per_day,
    seconds_per_hour,
    seconds_per_minute,
    weeks_per_year,
]

epoch_year = 1970
epoch_month = 1
epoch_day = 1
epoch_week_offset = 4

hours_per_day = 24

minutes_per_hour = 60
minutes_per_day = hours_per_day * minutes_per_hour

nanos_per_milli = 1_000_000
nanos_per_second = 1_000_000_000
nanos_per_minute = 60 * nanos_per_second
nanos_per_hour = 60 * nanos_per_minute
nanos_per_day = 24 * nanos_per_hour

seconds_per_minute = 60
seconds_per_hour = 3600
seconds_per_day = 86_400

leap_interval = 4
leap_exception = 100
leap_non_exception = 400

days_per_non_leap_year = 365
days_per_leap_year = 366

days_per_week = 7
weeks_per_year = 52

month_days : { month : Int *, is_leap ?? Bool } -> U64
month_days = |{ month, is_leap ?? Bool.false }|
    when month is
        1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
        4 | 6 | 9 | 11 -> 30
        2 if is_leap -> 29
        2 -> 28
        _ -> 0
