%%%----------------------------------------------------------------------------
%%% @doc P control
%%%----------------------------------------------------------------------------

-module(singlectrl).         

-define(Ts, 0.001).   %% time it takes in a full loop x 2 of the response rate of the sys (x3 or 5 better)

%% in seconds

-export([new/4, hello/3]).

-export([update/2]).


                               
-record(pid,          
	{ kp, ki, kd,
	  integral = 0,    %% Integrator acumulated (the old)
	imax = 10,
	imin = -10,
	  set_point,       %% requested position
	  current	   %% current angle position

	  
	  
	}).




%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
hello(Kp,Ki,Kd) ->
	io:format("Hello~n").
	

new(Kp,Ki,Kd,I)
  when is_number(Kp), is_number(Ki), is_number(Kd),
       is_number(I) ->
    new_(Kp,Ki,Kd,I).

new_(Kp,Ki,Kd,I) ->
	  
    #pid { kp = Kp, ki = Ki, kd = Kd,   

	   integral = I,   %%useless
	   set_point = 0,
	   current = 0
	   
	    }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
update(PID, Feed) when is_number(Feed) ->
    update_ef(PID, PID#pid.set_point, Feed).    

update_ef(PID, SetPoint, Feed) ->
    Error = SetPoint - Feed,

    I0 = PID#pid.integral,  %the old integral    
    I1 = I0 + Error*?Ts,		% the new integral  
                 %%%antiwindup
    
    
%	
	I2 = max(min(PID#pid.imax,I1),PID#pid.imin),    
%%%%%%%%%%%%antiwindup control

Output =  I2,			%(PID#pid.kp*Error)+(PID#pid.ki*I2),

    {Output,PID#pid { integral=I1} }.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





