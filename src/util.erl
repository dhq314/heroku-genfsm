%%%-----------------------------------
%%% @Module  : util
%%% @Description: Common Function
%%%-----------------------------------
-module(util).
-compile(export_all).

-define(DIFF_SECONDS_0000_1900, 62167219200).





%% @doc 取得当前的unix时间戳（秒） 
unixtime() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    MegaSecs * 1000000 + Secs.


%% @doc term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1] 
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% @doc term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).



%% @doc 根据1970年以来的秒数获得日期
seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).


