-module(time_resource).
-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

resource_exists(ReqData, _Context) ->
    Unixtime =
        case string:tokens(wrq:disp_path(ReqData), "/") of
            [Time] ->
                case util:string_to_term(Time) of
                    NewTime when is_integer(NewTime) ->
                        NewTime;
                    _ ->
                        util:unixtime()
                end;
            _ ->
                util:unixtime()
        end,
    {{Year, Month, Day}, {Hour, Minute, Second}} = util:seconds_to_localtime(Unixtime),
    Year1 = util:term_to_string(Year),
    Month1 = util:term_to_string(Month),
    Day1 = util:term_to_string(Day),
    Hour1 = util:term_to_string(Hour),
    Minute1 = util:term_to_string(Minute),
    Second1 = util:term_to_string(Second),
    FormatTime = Year1 ++ "-" ++ Month1 ++ "-" ++ Day1 ++ " " ++ Hour1 ++ ":" ++ Minute1 ++ ":" ++ Second1,
    {true, ReqData, {struct, [{time, FormatTime}]}}.

to_json(ReqData, Result) ->
    {mochijson:encode(Result), ReqData, Result}.

