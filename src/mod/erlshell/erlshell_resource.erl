-module(erlshell_resource).
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
    HtmlData = [
        {domain, Domain},
        {application_name, ApplicationName} 
    ],
    {ok, Html} = genfsm_erlshell_dtl:render(HtmlData),
    {Html, ReqData, State}.


