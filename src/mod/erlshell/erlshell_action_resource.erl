-module(erlshell_action_resource).
-export([
    init/1,
    allowed_methods/2,
    content_types_provided/2,
    to_jsonp/2
]).
-include_lib("webmachine/include/webmachine.hrl").

-define(HEART_TIME_INTERVAL, 30).


init(_) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_jsonp}], ReqData, Context}.

to_jsonp(ReqData, Context) ->
    Return =
        case wrq:get_qs_value("callback", ReqData) of           %% 获取回调函数名
            undefined ->
                "error";
            CallBack ->
                JsonPropList =
                    case wrq:get_qs_value("action", ReqData) of  
                        "1" ->
                            erlshell_create(ReqData);
                        "2" ->
                            erlshell_stop(ReqData); 
                        "3" ->
                            erlshell_eval(ReqData);
                        "4" ->
                            erlshell_heart(ReqData);
						"5" ->
							eval_real_time(ReqData);
                        _ ->
                            [{result, 2}]
                    end,
                CallBack ++ "(" ++ encode_json(JsonPropList) ++ ")"
        end,
    {Return, ReqData, Context}.

%% @doc 创建ErlShell
erlshell_create(ReqData) ->
    LongUnixTime = util:longunixtime(),
    ProcessName = create_process_name(?MODULE, [LongUnixTime]),
    case erlshell_server:start_link([ProcessName, ?HEART_TIME_INTERVAL]) of
        {ok, _Pid} ->
            {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
            Year1 = util:term_to_string(Year),
            Month1 = util:term_to_string(Month),
            Day1 = util:term_to_string(Day),
            Hour1 = util:term_to_string(Hour),
            Minute1 = util:term_to_string(Minute),
            Second1 = util:term_to_string(Second),
            StartTime = Year1 ++ "-" ++ Month1 ++ "-" ++ Day1 ++ " " ++ Hour1 ++ ":" ++ Minute1 ++ ":" ++ Second1,
            SystemInfo = erlang:system_info(system_version),
            [{result, 1}, {action, 1}, {pid, ProcessName}, {interval, ?HEART_TIME_INTERVAL}, {line_num, 1}, {start_time, StartTime}, {client_ip, ReqData#wm_reqdata.peer}, {system_info, SystemInfo}];
        _ ->
            [{result, 2}, {action, 1}]
    end.

%% @doc 关闭ErlShell
erlshell_stop(ReqData) ->
    case get_process_name(ReqData) of
        undefined ->
            skip;
        Pid ->
            exit(Pid, kill)
    end,
    [{result, 1}, {action, 2}].

%% @doc 解析 Erlang 表达式字符串
erlshell_eval(ReqData) ->
    case get_process_name(ReqData) of
        undefined ->
            %% 进程异常关闭，通知前端重新启动 ErlShell
            [{result, 31}, {action, 3}];
        Pid ->
            ErlStr = wrq:get_qs_value("erl_str", ReqData),
            Ret = gen_server:call(Pid, {'EVAL_ERLSTR', ErlStr}),
            [{action, 3} | Ret]
    end.

%% @doc 即时解析 Erlang 表达式字符串
eval_real_time(ReqData) ->
	ErlStr = wrq:get_qs_value("erl_str", ReqData),
	erlshell_server:eval(ErlStr).

%% @doc ErlShell 的心跳包
erlshell_heart(ReqData) ->
    case get_process_name(ReqData) of
        undefined ->
            %% 进程关闭，通知前端关掉定时器
            [{result, 41}, {action, 4}];
        Pid ->
            gen_server:cast(Pid, 'ERLSHELL_HEART'),
            [{result, 1}, {action, 4}]
    end.

%% @doc 获取进程名字
get_process_name(ReqData) ->
    case wrq:get_qs_value("pid", ReqData) of
        undefined ->
            undefined;
        ProcessNameStr ->
            ProcessName = util:string_to_term(ProcessNameStr),
            case whereis(ProcessName) of
                Pid when is_pid(Pid) ->
                    Pid;
                _ ->
                    undefined
            end
    end.

%% @doc 创建进程名
create_process_name(Prefix, List) ->
    util:to_atom(lists:concat(lists:flatten([Prefix | [['_', T] || T <- List]]))).

%% @doc 编成 json 格式
encode_json(QueryString) -> 
    mochijson:encode({struct, QueryString}).

