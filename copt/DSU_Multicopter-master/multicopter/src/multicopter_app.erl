%%%-------------------------------------------------------------------
%% @doc multicopter public API
%% @end
%%%-------------------------------------------------------------------

-module(multicopter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    multicopter_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
