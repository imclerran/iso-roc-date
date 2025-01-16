module [
    expand_int_with_zeros,
    find_decimal_index,
    pad_left,
    split_list_at_indices,
    split_utf8_and_keep_delimiters,
    utf8_to_frac,
    utf8_to_int,
    utf8_to_int_signed,
    validate_utf8_single_bytes,
]

split_list_at_indices : List a, List U8 -> List (List a)
split_list_at_indices = |list, indices|
    split_list_at_indices_recur(list, List.sort_desc(indices))

split_list_at_indices_recur : List a, List U8 -> List (List a)
split_list_at_indices_recur = |list, indices|
    when indices is
        [x, .. as xs] if x != 0 && x != List.len list |> Num.toU8 ->
            { before, others } = List.splitAt list (Num.toU64 x)
            split_list_at_indices_recur before xs |> List.append others

        [_, .. as xs] ->
            split_list_at_indices_recur list xs

        [] -> [list]

split_utf8_and_keep_delimiters : List U8, List U8 -> List (List U8)
split_utf8_and_keep_delimiters = |u8_list, delimiters|
    compare_to_delimiters = |byte| List.contains(delimiters, byte) |> |is_found| if is_found then Found else NotFound
    result = List.walk(
        u8_list,
        [],
        |lists, byte|
            when lists is
                [.. as xs, []] ->
                    when compare_to_delimiters(byte) is
                        Found -> xs |> List.append([byte]) |> List.append([])
                        NotFound -> xs |> List.append([byte])

            [.. as xs, x] ->
                when compare_to_delimiters byte is
                    Found -> xs |> List.append x |> List.append [byte] |> List.append []
                    NotFound -> xs |> List.append (x |> List.append byte)

            [] ->
                when compare_to_delimiters byte is
                    Found -> [[byte], []]
                    NotFound -> [[byte]]
    when result is
        [.. as xs, []] -> xs
        _ -> result

validate_utf8_single_bytes : List U8 -> Bool
validate_utf8_single_bytes = |u8_list|
    if List.all(u8_list, |u8| Num.bitwise_and(u8, 0b10000000) == 0b00000000) then
        Bool.true
    else
        Bool.false

utf8_to_int : List U8 -> Result U64 [InvalidBytes]
utf8_to_int = |u8_list|
    u8_list
    |> List.reverse
    |> List.walk_with_index(
        Ok(0),
        |num_result, byte, index|
            Result.try(
                num_result,
                |num|
                    if 0x30 <= byte && byte <= 0x39 then
                        Ok((num + (Num.to_u64(byte) - 0x30) * (Num.to_u64(Num.pow_int(10, index)))))
                    else
                        Err(InvalidBytes),
            ),
    )

utf8_to_int_signed : List U8 -> Result I64 [InvalidBytes]
utf8_to_int_signed = |u8_list|
    when u8_list is
        ['-', .. as xs] -> utf8_to_int(xs) |> Result.map(|num| -1 * Num.to_i64(num))
        ['+', .. as xs] -> utf8_to_int(xs) |> Result.map(|num| Num.to_i64(num))
        _ -> utf8_to_int(u8_list) |> Result.map(|num| Num.to_i64(num))

utf8_to_frac : List U8 -> Result F64 [InvalidBytes]
utf8_to_frac = |u8_list|
    when find_decimal_index(u8_list) is
        Ok(decimal_index) ->
            when split_list_at_indices(u8_list, [decimal_index, (decimal_index + 1)]) is
                [head, [byte], tail] if byte == ',' || byte == '.' ->
                    when (utf8_to_int head, utf8_to_int tail) is
                        (Ok int_part, Ok frac_part) ->
                            decimal_shift = List.len tail |> Num.toU8
                            Num.toF64 int_part + move_decimal_point (Num.toF64 frac_part) decimal_shift |> Ok

                        (_, _) -> Err InvalidBytes

                [['.'], tail] -> # if byte == ',' || byte == '.' -> # crashes when using byte comparison
                    frac_part = utf8_to_int? tail
                    decimal_shift = List.len tail |> Num.toU8
                    Ok (move_decimal_point (Num.toF64 frac_part) decimal_shift)

                [[','], tail] -> # if byte == ',' || byte == '.' -> # crashes when using byte comparison
                    frac_part = utf8_to_int? tail
                    decimal_shift = List.len tail |> Num.toU8
                    Ok (move_decimal_point (Num.toF64 frac_part) decimal_shift)

                [head, [byte]] if byte == ',' || byte == '.' ->
                    int_part = utf8_to_int? head
                    Ok (Num.toF64 int_part)

                _ ->
                    int_part = utf8_to_int? u8_list
                    Ok (Num.toF64 int_part)

        Err NoDecimalPoint ->
            int_part = utf8_to_int? u8_list
            Ok (Num.toF64 int_part)

find_decimal_index : List U8 -> Result U8 [NoDecimalPoint]
find_decimal_index = |u8_list|
    List.walk_with_index_until(
        u8_list,
        Err(NoDecimalPoint),
        |_, byte, index|
            if byte == '.' || byte == ',' then
                Break(Ok(Num.to_u8(index)))
            else
                Continue(Err(NoDecimalPoint)),
    )

move_decimal_point : F64, U8 -> F64
move_decimal_point = |num, digits|
    when digits is
        0 -> num
        _ -> (move_decimal_point num (digits - 1)) / 10

pad_left : Str, U8, U64 -> Str
pad_left = |str, pad_char, target_length|
    strlen = Str.count_utf8_bytes(str)
    pad_length = if target_length > strlen then target_length - strlen else 0
    when List.repeat pad_char pad_length |> Str.fromUtf8 is
        Ok pad_str -> Str.concat pad_str str
        Err _ -> Str.repeat " " pad_length |> Str.concat str

expect pad_left "123" ' ' 5 == "  123"
expect pad_left "123" ' ' 2 == "123"
expect pad_left "123" 128 5  == "  123"
expect pad_left "123" '_' 5 == "__123"

expand_int_with_zeros : Int *, U64 -> Str
expand_int_with_zeros = |num, target_length|
    num |> Num.to_str |> pad_left('0', target_length)

expect expand_int_with_zeros 123 5 == "00123"
