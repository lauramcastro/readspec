%%% File    : register_eqc.erl
%%% Description : QuickCheck state machine example (erlang registration mechanism)
%%% Created : Dec 2010

-module(register_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(state, {processes, registered}).

%% Initialize the state
initial_state() ->
    #state{processes   = [],
		   registered  = []}.

%% Command generator, S is the state
command(S) ->
    frequency([{2, {call, ?MODULE, new_process, []}},
			   {1, oneof([{call, ?MODULE, register,   [name(), oneof(S#state.processes)]} || S#state.processes =/= []]
					  ++ [{call, ?MODULE, unregister, [name()]}]
			          ++ [{call, erlang,  whereis,    [name()]}]
			          ++ [])}]).

name() ->
	?LET(N, choose($a, $z), list_to_atom([N])).

new_process() ->
    spawn(?MODULE, idle_loop, []).

idle_loop() ->
    receive
		stop ->
			ok;
		_Else ->
			idle_loop()
    end.

%% Next state transformation, S is the current state
next_state(S,_V,{call,?MODULE,register,[Name, PID]}) ->
    case ((not lists:keymember(Name, 1, S#state.registered))
		  andalso (not lists:keymember(PID, 2, S#state.registered))) of
        true ->
            S#state{registered = [{Name, PID} | S#state.registered]};
        false ->
            S
    end;
next_state(S,_V,{call,?MODULE,unregister,[Name]}) ->
    case lists:keymember(Name, 1, S#state.registered) of
        true ->
            S#state{registered = lists:keydelete(Name, 1, S#state.registered)};
        false ->
            S
    end;
next_state(S, V,{call,?MODULE,new_process,[]}) ->
    S#state{processes = [V | S#state.processes]};
next_state(S,_V,{call,_,_,_}) ->
    S.

%% Precondition, checked before command is added to the command sequence
precondition(_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>) 
postcondition(S,{call,?MODULE,register,[Name, PID]},R) ->
    case (lists:keymember(Name, 1, S#state.registered)
	  orelse lists:keymember(PID, 2, S#state.registered)) of
        true ->
            is_exit(R);
        false ->
            R
    end;
postcondition(S,{call,?MODULE,unregister,[Name]},R) ->
    case lists:keymember(Name, 1, S#state.registered) of
        true  -> R;
        false -> is_exit(R)
    end;
postcondition(S,{call,erlang,whereis,[Name]},R) ->
    case lists:keysearch(Name, 1, S#state.registered) of
	{value, {Name, R}} -> true;
	false              -> R == undefined
    end;
postcondition(_S,{call,_,_,_},_R) ->
    true.

is_exit({'EXIT',_}) ->
    true;
is_exit(_Otro) ->
    false.



%% TEST PROPERTIES
prop_register() ->
    ?FORALL(Cmds,commands(?MODULE),
			begin
				{H,S,Res} = run_commands(?MODULE,Cmds),
				clean(S),
				?WHENFAIL(
				   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
				   Res == ok)
			end).

prop_parallel_register() ->
    ?FORALL(Cmds,parallel_commands(?MODULE),
            begin
                {HS,HP,Res} = run_parallel_commands(?MODULE,Cmds),
				clean_parallel(),
                ?WHENFAIL(
                   io:format("Secuential history: ~p\nParallel history: ~p\nRes: ~p\n",[HS,HP,Res]),
                   Res == ok)
            end).

clean(S) ->
    [ catch erlang:unregister(Name) || {Name, _PID} <- S#state.registered],
    [ PID ! stop || PID <- S#state.processes].

clean_parallel() ->
    Registered = [atom_to_list(Atomo) || Atomo <- registered()],
    RegisteredTest = [list_to_atom(Name) || Name <- Registered],
    Processes = [ erlang:whereis(Name) || Name <- RegisteredTest],
    [ catch erlang:unregister(Name) || Name <- RegisteredTest],
    [ PID ! stop || PID <- Processes].


%% WRAPPER FUNCTIONS
register(Name, PID) ->
    catch erlang:register(Name, PID).

unregister(Name) ->
    catch erlang:unregister(Name).
