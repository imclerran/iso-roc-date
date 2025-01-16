module [unwrap]

## Unsafe function which should be used only for debugging purposes only
unwrap : Result a _, Str -> a
unwrap = |x, message|
    when x is
        Ok v -> v
        Err _ -> crash message
