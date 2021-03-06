-module(erlshell_server).
-behaviour(gen_server).


-record(state, {
    heart_time = 0,                     %% 心跳包时间
    heart_timer = undefined,            %% 检测心跳包的定时器 
    heart_time_interval = 10,           %% 心跳包的检测间隔
    bindings = [],                      %% 已经被绑定赋值的变量列表
    line_num = 1                        %% 行号
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, eval/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([ProcessName, HeartTimeInterval]) ->
    gen_server:start_link({local, ProcessName}, ?MODULE, [HeartTimeInterval], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([HeartTimeInterval]) ->
    HeartTime = util:unixtime(),
    HeartTimer = erlang:send_after(HeartTimeInterval * 1000, self(), 'DETECT_HEART'),
    Bindings = erl_eval:new_bindings(),
    State = #state{
        heart_time = HeartTime,
        heart_timer = HeartTimer,
        heart_time_interval = HeartTimeInterval,
        bindings = Bindings
    },
    {ok, State}.

handle_call({'EVAL_ERLSTR', ErlStr}, _From, State) ->
    LineNum = State#state.line_num + 1,
    {NewValue, NewBindings} = eval(ErlStr, State#state.bindings),
    NewState = State#state{
        bindings = NewBindings,
        line_num = LineNum
    },
    {reply, [{result, 1}, {value, NewValue}, {line_num, LineNum}], NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast('ERLSHELL_HEART', State) ->
    HeartTime = util:unixtime(),
    NewState = State#state{
        heart_time = HeartTime
    },
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc 心跳包检测
handle_info('DETECT_HEART', State) ->
    Now = util:unixtime(),
    case Now - State#state.heart_time > State#state.heart_time_interval * 2 of
        true ->
            {stop, normal, State};
        false ->
            HeartTimer = erlang:send_after(State#state.heart_time_interval * 1000, self(), 'DETECT_HEART'),
            NewState = State#state{
                heart_timer = HeartTimer
            },
            {noreply, NewState}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %io:format("Die_Reason ~p~n", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc 解析函数
eval(ErlStr) ->
	{Result, _Bindings} = eval(ErlStr, erl_eval:new_bindings()),
	[{result, 1}, {value, Result}].
eval(ErlStr, Bindings) ->
	{NewValue, RetBindings} = 
        case check_valid(ErlStr) of
            true ->
                try eval_action(ErlStr, Bindings) of
                    {value, Value, NewBindings} ->
                        {Value, NewBindings}
                catch
                    _:Error ->
                        {Error, Bindings}
                end;
            false ->
                {"illegal expression", Bindings}
        end,
    case io_lib:printable_list(NewValue) of
        true -> {NewValue, RetBindings};
        false -> {util:term_to_string(NewValue), RetBindings}
    end.
eval_action(ErlStr, Bindings) ->
	NewErlStr = io_format_convert_io_lib_format(ErlStr),
    {ok, Tokens, _EndLocation} = erl_scan:string(NewErlStr),
    %% 表达式字符串后面要以点号结束
    NewTokens = 
        case lists:reverse(Tokens) of
            [{dot, _} | _] -> Tokens;
            TokensReverse -> lists:reverse([{dot, 1} | TokensReverse])
        end,
    {ok, ExprList} = erl_parse:parse_exprs(NewTokens),
    erl_eval:exprs(ExprList, Bindings).

%% @doc 检查表达式是否含有非法语句
check_valid(ErlStr) ->
    REList = ["application(.*)stop", "os(.*)cmd(.*)rm"],
    check_valid(REList, ErlStr, true).
check_valid([], _ErlStr, Bool) ->
    Bool;
check_valid([RE | R], ErlStr, Bool) ->
    case re:run(ErlStr, RE) of
        {match, _Captured} ->
            false;
        _ ->
            check_valid(R, ErlStr, Bool)
    end.

%% @doc io:format 转成 io_lib:format 输出
io_format_convert_io_lib_format(ErlStr) ->
	RE = "io\\:format\\((.*)\\]\\)",
    case re:run(ErlStr, RE, [{capture, first, list}, ungreedy]) of
        {match, _Match} ->
            [Splitbefore, FormatParam, SplitAfter] = re:split(ErlStr, RE, [{return, list}, ungreedy, {parts, 3}]),
			lists:concat([Splitbefore, "lists:flatten(io_lib:format(", FormatParam, "]))", SplitAfter]);
        _ ->
			RE1 = "io\\:format\\((.*)\\)",
			case re:run(ErlStr, RE1, [{capture, first, list}, ungreedy]) of
        		{match, _Match} ->
					[Splitbefore, FormatParam, SplitAfter] = re:split(ErlStr, RE1, [{return, list}, ungreedy, {parts, 3}]),
					lists:concat([Splitbefore, "lists:flatten(io_lib:format(", FormatParam, ", []))", SplitAfter]);
				_ ->
					ErlStr
			end
    end.
