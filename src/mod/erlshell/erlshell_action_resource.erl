-module(erlshell_action_resource).
-export([
    init/1,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2,
    process_post/2
]).
-include_lib("webmachine/include/webmachine.hrl").

-define(HEART_TIME_INTERVAL, 10).


init(_) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% hit this with
%%   curl "http://localhost:8000/formjson?one=two&me=pope"
to_json(ReqData, Context) ->
    {json_body(wrq:req_qs(ReqData)), ReqData, Context}.

%% hit this with
%%   curl -X POST http://localhost:8000/formjson \
%%        -d "one=two&me=pope"
process_post(ReqData, Context) ->
    PostList = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    Body = 
        case get_post_value(PostList, action) of
            false ->
                [{result, 2}];
            ActionCode ->
                %io:format("Sta ~p~n", [ActionCode]),
                case util:string_to_term(ActionCode) of
                    1 ->
                        erlshell_create(ActionCode);
                    2 ->
                        erlshell_stop(PostList, ActionCode);
                    3 ->
                        erlshell_eval(PostList, ActionCode);
                    4 ->
                        erlshell_heart(PostList, ActionCode);
                    _ ->
                        [{result, 2}]
                end
        end,
    %io:format("Body ~p~n", [Body]),
    {true, wrq:append_to_response_body(json_body(Body), ReqData), Context}.

json_body(QS) -> mochijson:encode({struct, QS}).

%% @doc 创建ErlShell
erlshell_create(ActionCode) ->
    LongUnixTime = util:longunixtime(),
    ProcessName = create_process_name(?MODULE, [LongUnixTime]),
    case erlshell_server:start_link([ProcessName, ?HEART_TIME_INTERVAL]) of
        {ok, _Pid} ->
            [{result, 1}, {action, ActionCode}, {pid, ProcessName}, {interval, ?HEART_TIME_INTERVAL}, {line_num, 1}];
        _ ->
            [{result, 2}, {action, ActionCode}]
    end.


%% @doc 关闭ErlShell
erlshell_stop(PostList, ActionCode) ->
    case get_process_name(PostList, pid) of
        undefined ->
            skip;
        Pid ->
            %io:format("Exit Pid ~p~n", [Pid]),
            exit(Pid, kill)
    end,
    [{result, 1}, {action, ActionCode}].

%% @doc 解析erlstr
erlshell_eval(PostList, ActionCode) ->
    case get_process_name(PostList, pid) of
        undefined ->
            %% 进程异常关闭，通知前端重新启动 ErlShell
            [{result, 31}, {action, ActionCode}];
        Pid ->
            ErlStr = get_post_value(PostList, erl_str),
            %io:format("ErlStr ~p~n", [ErlStr]),
            Ret = gen_server:call(Pid, {'EVAL_ErlStr', ErlStr}),
            [{action, ActionCode}] ++ Ret
    end.

%% @doc ErlShell 的心跳包
erlshell_heart(PostList, ActionCode) ->
    case get_process_name(PostList, pid) of
        undefined ->
            %% 进程关闭，通知前端关掉定时器
            [{result, 41}, {action, ActionCode}];
        Pid ->
            gen_server:cast(Pid, 'ERLSHELL_HEART'),
            [{result, 1}, {action, ActionCode}]
    end.
    

%% @doc 获取 POST 过来的值
get_post_value(PostList, Key) ->
    case lists:keyfind(util:term_to_string(Key), 1, PostList) of
        false ->
            false;
        {_Key, Value} ->
            Value
    end.

%% @doc 获取进程名字
get_process_name(PostList, Key) ->
    ProcessNameStr = get_post_value(PostList, Key),
    ProcessName = util:string_to_term(ProcessNameStr),
    case whereis(ProcessName) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            undefined
    end.

%% @doc 创建进程名
create_process_name(Prefix, List) ->
    util:to_atom(lists:concat(lists:flatten([Prefix] ++ lists:map(fun(T) -> ['_', T] end, List)))).
	

