%%%-------------------------------------------------------------------
%% @doc copter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(copter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
   %%%%-define(CHILD(I, Type), {I, {I,  
 %%%% start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions      
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).  
%%todo MODULE

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 5, 10},
	 [{copter_server,
		 {copter_server, start_link, []},
		permanent, 2000, worker, [copter_server]}]}
 }.

%%====================================================================
%% Internal functions
%%====================================================================
