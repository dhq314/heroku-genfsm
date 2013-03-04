-module(phone_resource).
%-export([init/1, content_types_provided/2, resource_exists/2, to_json/2, test/0, priv_dir/1]).

-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("xmerl/include/xmerl.hrl").



init(_) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

resource_exists(ReqData, _Context) ->
    Result = 
        case ibrowse:send_req("http://life.tenpay.com/cgi-bin/mobile/MobileQueryAttribution.cgi?chgmobile=13570374550", [], get) of
            {ok, _, _, Xml} ->
                [{error, 1}];
            _ ->
                [{error, 1}]
        end,
    {true, ReqData, {struct, Result}}.

to_json(ReqData, Result) ->
    {mochijson:encode(Result), ReqData, Result}.



test() ->
    %{ok, Dispatch} = file:consult(filename:join([priv_dir(genfsm), "test.xml"])),
    %io:format("Dispatch2 ~p~n", [Dispatch]).
    {ok, _, _, Xml} = ibrowse:send_req("http://life.tenpay.com/cgi-bin/mobile/MobileQueryAttribution.cgi?chgmobile=13570374550", [], get),
    {R, _} = xmerl_scan:string(Xml),
    L = lists:flatten(test21(R, [])),
    L.




%%
%% @doc return the priv dir
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.



test21(R, L) when is_record(R, xmlElement) ->
    case R#xmlElement.name of
        city ->
            ItemData = lists:foldl(fun test21/2, [], R#xmlElement.content),
            [ ItemData | L ];
        province ->
            ItemData = lists:foldl(fun test21/2, [], R#xmlElement.content),
            [ ItemData | L ];
        retmsg ->
            ItemData = lists:foldl(fun test21/2, [], R#xmlElement.content),
            [ ItemData | L ];
        supplier ->
            ItemData = lists:foldl(fun test21/2, [], R#xmlElement.content),
            [ ItemData | L ];
        _ -> 
            lists:foldl(fun test21/2, L, R#xmlElement.content)
    end;
test21(#xmlText{ parents = [{city, _}, _], value = V}, L) ->
    [{city, V} | L];
test21(#xmlText{ parents = [{province, _}, _], value = V}, L) ->
    [{province, V} | L];
test21(#xmlText{ parents = [{retmsg, _}, _], value = V}, L) ->
    [{retmsg, V} | L];
test21(#xmlText{ parents = [{supplier, _}, _], value = V}, L) ->
    [{supplier, V} | L];

test21(#xmlText{}, L) -> 
    L.



