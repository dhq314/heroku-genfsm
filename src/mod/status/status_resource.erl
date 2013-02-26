%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(status_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    %io:format("~p~n~n~n", [ReqData]),
    {ok, ApplicationName} = application:get_application(?MODULE),
    Port = 
        case os:getenv("PORT") of
            false ->
                case os:getenv("WEBMACHINE_PORT") of
                    false -> 8000;
                    AnyPort -> AnyPort
                end;
            AnyPort -> list_to_integer(AnyPort)
        end,

    SchedulerId = erlang:system_info(scheduler_id),
    SchedulerNum = erlang:system_info(schedulers),
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    ProcessesMemUsed = erlang:memory(processes_used),
    ProcessesMemAlloc = erlang:memory(processes),
    MemTotal = erlang:memory(total),

    OTP = erlang:system_info(otp_release),
    OS = io_lib:format("~p", [erlang:system_info(os_type)]),

    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    RequestTime = io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Minute, Second]),

    HtmlData = [
        {application_name, ApplicationName}, 
        {port, Port},
        {scheduler_id, SchedulerId},
        {scheduler_num, SchedulerNum},
        {process_count, ProcessCount},
        {process_limit, ProcessLimit},
        {processes_used, ProcessesMemUsed},
        {processes, ProcessesMemAlloc},
        {memtotal, MemTotal},
        {otp_release, OTP},
        {os, OS},
        {client_ip, ReqData#wm_reqdata.peer}, 
        {request_time, RequestTime}
    ],

    {ok, Html} = genfsm_status_dtl:render(HtmlData),
    {Html, ReqData, State}.

