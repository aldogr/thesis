%%%----------------------------------------------------------------------------
%%% @doc P control
%%%----------------------------------------------------------------------------

-module(ctl).         


-export([new/3, new_/5, hello/0]).
-export([update/2, store/1, print/1]).
-export([timestamp/0]).

                               
-record(data,	{
	integral = 0,    %% Integrator state
		%timestamp,       %% previous timestamp
		%set_point = 0,       %% requested value
		%target_point = 0,%% current set_point

	kp,
	ki, 
	kd  %% Proportional/Integral/Derevative gain
	}).


%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
hello() ->
	io:format("Hello~n").
	
new(Kp,Ki,Kd)
  when is_number(Kp), is_number(Ki), is_number(Kd) ->
   new_(Kp,Ki,Kd,0,timestamp()).




new_(Kp,Ki,Kd,I,Ts) ->
	
    #data { kp = Kp, ki = Ki, kd = Kd, integral = 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


update(SetPoint, Feed) ->
    Error = SetPoint - Feed,
    
    Output = Error * #data.kp,
	K1 = #data.kp,
	K2 = #data.ki,
	K3 = #data.kd,
	I0 = #data.integral,
 
    {Output, Error, K1, K2, K3, I0}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




timestamp() ->
    os:timestamp().


