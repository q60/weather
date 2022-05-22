-module(weather).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    application:ensure_all_started(hackney),

    case Args of
        [] ->
            City = "Vilnius";
        [H | _] ->
            City = H
    end,

    Params = "&aqi=no",
    Body = get_forecast(City, Params),
    JSON = jsone:decode(Body),

    Location = maps:get(<<"location">>, JSON),
    CityName = maps:get(<<"name">>, Location),
    Country = maps:get(<<"country">>, Location),

    Current = maps:get(<<"current">>, JSON),
    Condition = maps:get(<<"condition">>, Current),
    Weather = maps:get(<<"text">>, Condition),
    Temp = maps:get(<<"temp_c">>, Current),
    FeelsLike = maps:get(<<"feelslike_c">>, Current),
    Pressure = maps:get(<<"pressure_in">>, Current),
    Humidity = maps:get(<<"humidity">>, Current),
    WindKph = maps:get(<<"wind_kph">>, Current),
    WindDir = maps:get(<<"wind_dir">>, Current),

    Delimiter = lists:duplicate(string:length(<<CityName/binary, Country/binary>>) + 2, "="),

    Info =
        "\x1B[92m\x1b[1m~s, ~s\x1B[0m~n"
        ++ "\x1B[90m~s~n"
        ++ "\x1B[93m\x1B[1mWeather:\x1B[0m  \x1B[97m~s~n"
        ++ "\x1B[93m\x1B[1mTemp:\x1B[0m     \x1B[97m~.1f ~sC~n"
        ++ "\x1B[93m\x1B[1mFeels:\x1B[0m    \x1B[97m~.1f ~sC~n"
        ++ "\x1B[93m\x1B[1mPressure:\x1B[0m \x1B[97m~.1f mmHg~n"
        ++ "\x1B[93m\x1B[1mHumidity:\x1B[0m \x1B[97m~.10B%~n"
        ++ "\x1B[93m\x1B[1mWind:\x1B[0m     \x1B[97m~.1f m/s, ~s~n",
    InfoData =
        [Country,
         CityName,
         Delimiter,
         Weather,
         Temp,
         <<"°"/utf8>>,
         FeelsLike,
         <<"°"/utf8>>,
         Pressure * 25.4,
         Humidity,
         WindKph / 3.6,
         WindDir],

    io:format(Info, InfoData).

%%====================================================================
%% Internal functions
%%====================================================================
get_forecast(Location, Params) ->
    URI = "http://api.weatherapi.com/v1/current.json?",
    Key = os:getenv("WEATHER_TOKEN"),
    Headers = [],
    Payload = <<>>,
    Options = [],
    {ok, _StatusCode, _Headers, ClientRef} =
        hackney:get(URI ++ "key=" ++ Key ++ "&q=" ++ Location ++ Params,
                    Headers,
                    Payload,
                    Options),
    {ok, Body} = hackney:body(ClientRef),
    Body.
