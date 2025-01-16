module [
    from_hms,
    from_hmsn,
    from_ymd,
    from_ymd_hms,
    from_ymd_hmsn,
]

ContextualDuration : { years : I64, months : I8, days : I16, hours : I8, minutes : I8, seconds : I8, nanoseconds : I32 }

from_ymd : Int *, Int *, Int * -> ContextualDuration
from_ymd = |y, m, d| { years: Num.to_i64(y), months: Num.to_i8(m), days: Num.to_i16(d), hours: 0, minutes: 0, seconds: 0, nanoseconds: 0 }

from_hms : Int *, Int *, Int * -> ContextualDuration
from_hms = |h, m, s| { years: 0, months: 0, days: 0, hours: Num.to_i8(h), minutes: Num.to_i8(m), seconds: Num.to_i8(s), nanoseconds: 0 }

from_hmsn : Int *, Int *, Int *, Int * -> ContextualDuration
from_hmsn = |h, m, s, n| { years: 0, months: 0, days: 0, hours: Num.to_i8(h), minutes: Num.to_i8(m), seconds: Num.to_i8(s), nanoseconds: Num.to_i32(n) }

from_ymd_hms : Int *, Int *, Int *, Int *, Int *, Int * -> ContextualDuration
from_ymd_hms = |y, m, d, h, mi, s| { years: Num.to_i64(y), months: Num.to_i8(m), days: Num.to_i16(d), hours: Num.to_i8(h), minutes: Num.to_i8(mi), seconds: Num.to_i8(s), nanoseconds: 0 }

from_ymd_hmsn : Int *, Int *, Int *, Int *, Int *, Int *, Int * -> ContextualDuration
from_ymd_hmsn = |y, m, d, h, mi, s, n| { years: Num.to_i64(y), months: Num.to_i8(m), days: Num.to_i16(d), hours: Num.to_i8(h), minutes: Num.to_i8(mi), seconds: Num.to_i8(s), nanoseconds: Num.to_i32(n) }
