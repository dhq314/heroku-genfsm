%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the genfsm application.

-module(genfsm_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for genfsm.
start(_Type, _StartArgs) ->
    genfsm_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for genfsm.
stop(_State) ->
    ok.
