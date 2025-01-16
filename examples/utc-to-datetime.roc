app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    dt: "../package/main.roc",
}

import pf.Stdout
import pf.Utc
import dt.DateTime

main! = |_|
    utc_now = Utc.now!({})
    now_str =
        utc_now
        |> Utc.toNanosSinceEpoch
        |> DateTime.from_nanos_since_epoch
        |> DateTime.to_iso_str
    Stdout.line! "Hello, World! The current Zulu time is: $(now_str)"
