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

%% 取得当前的unix时间戳（豪秒）
longunixtime() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    (MegaSecs * 1000000000000 + Secs * 1000000 + _MicroSecs) div 1000.

%% @doc term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1] 
string_to_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _EndLocation} ->
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


%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) -> 
	Msg;
to_atom(Msg) when is_binary(Msg) -> 
    list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) -> 
    list_to_atom2(Msg);
to_atom(_) -> 
    throw(other_value).  %%list_to_atom("").

list_to_atom2(List) when is_list(List) ->
	case catch(list_to_existing_atom(List)) of
		{'EXIT', _} -> erlang:list_to_atom(List);
		Atom when is_atom(Atom) -> Atom
	end.


%% @doc is_string(List)-> yes|no|unicode  
is_string([]) -> yes;
is_string(List) -> is_string(List, non_unicode).

is_string([C|Rest], non_unicode) when C >= 0, C =< 255 -> is_string(Rest, non_unicode);
is_string([C|Rest], _) when C =< 65000 -> is_string(Rest, unicode);
is_string([], non_unicode) -> yes;
is_string([], unicode) -> unicode;
is_string(_, _) -> no.

%% @doc 列表乱序
shuffle_list(List) -> shuffle_list(List, []).
shuffle_list([], Acc) -> Acc;
shuffle_list(List, Acc) ->
	{Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
	shuffle_list(Leading ++ T, [H | Acc]).

