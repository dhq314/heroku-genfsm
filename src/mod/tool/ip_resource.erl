-module(ip_resource).
-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

resource_exists(ReqData, _Context) ->
	IpStr = wrq:disp_path(ReqData),
    Ip =
		case re:run(IpStr, "(\\d{1,3}\\.){3}\\d{1,3}", [{capture, first, list}]) of
			{match, [IpStr1]} ->
				IpStr1;
			_ ->
				ReqData#wm_reqdata.peer
		end,
	Ret = egeoip_lookup(Ip),
    {true, ReqData, Ret}.

to_json(ReqData, Result) ->
    {mochijson:encode(Result), ReqData, Result}.


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

