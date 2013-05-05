%% @author JoeDean <dhq314@gmail.com>
%% @doc Example webmachine_resource.

-module(status_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Domain = 
        case mochiweb_headers:get_value("Host", ReqData#wm_reqdata.req_headers) of
            undefined -> "genfsm.herokuapp.com";
            R -> R
        end,
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

	CutLen = 20,
	ModuleList = util:shuffle_list(code:all_loaded()),
	ModuleListLen = length(ModuleList),
	{RetModuleList, RemainModuleListLen} =
		case ModuleListLen > CutLen of
			true ->
				{lists:sublist(ModuleList, CutLen), ModuleListLen - CutLen};
			false ->
				{ModuleList, 0}
		end,
	
	ProcessList = util:shuffle_list(erlang:processes()),
	ProcessListLen = length(ProcessList),
	{RetProcessList, RemainProcessListLen} =
		case ProcessListLen > CutLen of
			true ->
				{lists:sublist(ProcessList, CutLen), ProcessListLen - CutLen};
			false ->
				{ProcessList, 0}
		end,
	RetProcessList1 = package_process_info(RetProcessList, []),
	
	
	AppList = application:which_applications(),
	
    HtmlData = [
        {domain, Domain},
        {application_name, ApplicationName}, 
        {port, Port},
		
		{os, OS},
		{compat_rel, erlang:system_info(compat_rel)},
		{wordsize, erlang:system_info(wordsize)},
		{smp_support, erlang:system_info(smp_support)},
		{heap_type, erlang:system_info(heap_type)},
		
		{scheduler_id, SchedulerId},
        {schedulers, SchedulerNum},
        {process_count, ProcessCount},
        {process_limit, ProcessLimit},
        {processes_used, ProcessesMemUsed},
        {processes, ProcessesMemAlloc},
        {memtotal, MemTotal},
		
		{hipe_architecture, erlang:system_info(hipe_architecture)},
		{machine, erlang:system_info(machine)},
		{otp_release, OTP},
        {client_ip, ReqData#wm_reqdata.peer}, 
        {request_time, RequestTime},

		{module_list, RetModuleList},
		{remain_module_list_len, RemainModuleListLen},
		
		{process_list, RetProcessList1},
		{remain_process_list_len, RemainProcessListLen},
		
		{app_list, AppList}
        
    ],

    {ok, Html} = genfsm_status_dtl:render(HtmlData),
    {Html, ReqData, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc 进程信息处理
package_process_info([], ProcessPidList) ->
	ProcessPidList;
package_process_info([Pid | L], ProcessPidList) ->
	ProcessInfoList = erlang:process_info(Pid),
%% 	io:format("~p~n", [ProcessInfo]),
%% 	ProcessText = "
%% 		current_function: ~p<br />
%% 		initial_call: ~p<br />
%% 		status: ~p<br />
%% 		message_queue_len: ~p<br />
%% 		links: ~p<br />
%% 		trap_exit: ~p<br />
%% 		error_handler: ~p<br />
%% 		priority: ~p<br />
%% 		group_leader: ~p<br />
%% 		heap_size: ~p<br />
%% 		total_heap_size: ~p<br />
%% 		stack_size: ~p<br />
%% 		reductions: ~p<br />
%% 	",
%% 	ProcessPropList = [
%% 		proplists:get_value(current_function, ProcessInfo),
%% 		proplists:get_value(initial_call, ProcessInfo),
%% 		proplists:get_value(status, ProcessInfo),
%% 		proplists:get_value(message_queue_len, ProcessInfo),
%% 		proplists:get_value(links, ProcessInfo),
%% 		proplists:get_value(trap_exit, ProcessInfo),
%% 	   	proplists:get_value(error_handler, ProcessInfo),
%% 		proplists:get_value(priority, ProcessInfo),
%% 		proplists:get_value(group_leader, ProcessInfo),
%% 		proplists:get_value(heap_size, ProcessInfo),
%% 		proplists:get_value(total_heap_size, ProcessInfo),
%% 		proplists:get_value(stack_size, ProcessInfo),
%% 		proplists:get_value(reductions, ProcessInfo)
%% 	],
	{ProcessText, ProcessPropList} = package_process_info(ProcessInfoList, "", []),
	Title = io_lib:format(ProcessText, ProcessPropList),
	package_process_info(L, [{util:term_to_string(Pid), Title} | ProcessPidList]).
package_process_info([], ProcessText, ProcessPropList) ->
	{ProcessText, lists:reverse(ProcessPropList)};
package_process_info([{ProcessKey, ProcessValue} | ProcessInfoList], ProcessText, ProcessPropList) ->
	case lists:member(ProcessKey, [dictionary]) of
		false ->
			package_process_info(ProcessInfoList, ProcessText ++ util:term_to_string(ProcessKey) ++ ": ~p<br />", [ProcessValue | ProcessPropList]);
		true ->
			package_process_info(ProcessInfoList, ProcessText, ProcessPropList)
	end.

														   
	