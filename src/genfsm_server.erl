-module(genfsm_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(INTERVAL, 60000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("genfsm_server start Args: ~p~n", [Args]),
    erlang:send_after(?INTERVAL, self(), 'DETECT_SERVER'),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info('DETECT_SERVER', State) ->
    spawn(fun()-> detect_server() end),
	spawn(fun()-> detect_genevent() end),
    spawn(fun()-> detect_luweb() end),
    erlang:send_after(?INTERVAL, self(), 'DETECT_SERVER'),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc 服务检测
detect_server() ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = 
        httpc:request("http://genfsm.herokuapp.com/").
detect_genevent() ->
	httpc:request("http://genevent.herokuapp.com/").
detect_luweb() ->
	httpc:request("http://luweb.herokuapp.com/").

