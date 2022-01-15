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
        [] -> City = "Vilnius";
        [H | _] -> City = H
    end,

    Params = "&aqi=no",
    Body = get_forecast(City, Params),
    JSON = jsone:decode(Body),

    {ok, Location} = maps:find(<<"location">>, JSON),
    {ok, CityName} = maps:find(<<"name">>, Location),
    {ok, Country} = maps:find(<<"country">>, Location),

    {ok, Current} = maps:find(<<"current">>, JSON),
    {ok, Condition} = maps:find(<<"condition">>, Current),
    {ok, Weather} = maps:find(<<"text">>, Condition),
    {ok, Temp} = maps:find(<<"temp_c">>, Current),
    {ok, FeelsLike} = maps:find(<<"feelslike_c">>, Current),
    {ok, Pressure} = maps:find(<<"pressure_in">>, Current),
    {ok, Humidity} = maps:find(<<"humidity">>, Current),
    {ok, WindKph} = maps:find(<<"wind_kph">>, Current),
    {ok, WindDir} = maps:find(<<"wind_dir">>, Current),

    Delimiter = lists:duplicate(string:length(<<CityName/binary, Country/binary>>)+2, "="),

    io:format("\x1B[92m\x1b[1m~s, ~s\x1B[0m~n", [Country, CityName]),
    io:format("\x1B[90m~s~n", [Delimiter]),
    io:format("\x1B[93m\x1B[1mWeather:\x1B[0m  \x1B[97m~s~n", [Weather]),
    io:format("\x1B[93m\x1B[1mTemp:\x1B[0m     \x1B[97m~.1f ~sC~n", [Temp, <<"°"/utf8>>]),
    io:format("\x1B[93m\x1B[1mFeels:\x1B[0m    \x1B[97m~.1f ~sC~n", [FeelsLike, <<"°"/utf8>>]),
    io:format("\x1B[93m\x1B[1mPressure:\x1B[0m \x1B[97m~.1f mmHg~n", [Pressure*25.4]),
    io:format("\x1B[93m\x1B[1mHumidity:\x1B[0m \x1B[97m~.10B%~n", [Humidity]),
    io:format("\x1B[93m\x1B[1mWind:\x1B[0m     \x1B[97m~.1f m/s, ~s~n", [WindKph/3.6, WindDir]).

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
        hackney:get(
          URI ++ "key=" ++ Key ++ "&q=" ++ Location ++ Params,
          Headers,
          Payload,
          Options
         ),
    {ok, Body} = hackney:body(ClientRef),
    Body.
