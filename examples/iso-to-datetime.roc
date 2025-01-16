app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    dt: "../package/main.roc",
}

import pf.Http
import pf.Stdout
import dt.DateTime

main! = |_|
    response = Http.send!(format_request("America/Chicago"))

    response_body =
        when response |> Http.handleStringResponse is
            Err err -> crash (Http.errorToString err)
            Ok body -> body

    iso_str = get_iso_string response_body

    dt_now =
        when DateTime.from_iso_str iso_str is
            Ok date_time -> date_time
            Err _ -> crash "Parsing Error"

    time_str = "$(Num.toStr dt_now.time.hour):$(Num.toStr dt_now.time.minute):$(Num.toStr dt_now.time.second)"
    date_str = "$(Num.toStr dt_now.date.year)-$(Num.toStr dt_now.date.month)-$(Num.toStr dt_now.date.day_of_month)"
    Stdout.line! "The current Zulut date is: $(date_str)"
    Stdout.line! "The current Zulu time is: $(time_str)"

format_request = |timezone| {
    method: GET,
    headers: [],
    url: "http://worldtimeapi.org/api/timezone/$(timezone).txt",
    mimeType: "",
    body: [],
    timeout: TimeoutMilliseconds 5000,
}

get_iso_string = |body|
    when Str.split_on(body, "\n") |> List.get(3) is
        Ok(line) ->
            when Str.split_first(line, ":") is
                Ok(line_parts) -> line_parts.after |> Str.trim
                Err(_) -> crash("Error splitting line at delimiter")

        Err _ -> crash "Error getting output line"
