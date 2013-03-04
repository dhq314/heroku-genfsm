-module(ip_resource).
-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

%% @doc 定义返回数据的格式，以及最终对返回数据进行操作的函数 to_json
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.


%% @doc 处理 HTTP 请求的函数
resource_exists(ReqData, _Context) ->
    IpStr = wrq:disp_path(ReqData),
    Ip =
		case re:run(IpStr, "(\\d{1,3}\\.){3}\\d{1,3}", [{capture, first, list}]) of
			{match, [IpStr1]} ->
				IpStr1;
			_ ->
				ReqData#wm_reqdata.peer
		end,
	Result = egeoip_lookup(Ip),
    {true, ReqData, Result}.


%% @doc 对返回数据进行操作的函数
to_json(ReqData, Result) ->
    {mochijson:encode(Result), ReqData, Result}.



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc 上面所说的自定义的 IP 查询函数
egeoip_lookup(Ip) ->
	Result = 
		case egeoip:ip2long(Ip) of
	        {ok, Ip1} ->
				case whereis(egeoip) of
			        undefined ->
					    [{result, 3}];
					Pid ->	
					    {ok, EgeoIP} = gen_server:call(Pid, {lookup, Ip1}),
					    Country = egeoip:get(EgeoIP, country_name),
					    City = egeoip:get(EgeoIP, city),
					    [{result, 1}, {ip, Ip}, {country, Country}, {city, City}]
				end;
	        _Error ->
				[{result, 2}]
	    end,
	{struct, Result}.

