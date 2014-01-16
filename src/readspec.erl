%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014, Laura M. Castro
%%% @doc
%%%     Generate human-readable versions of test models =>
%%%     by presenting human-readable versions of representative test
%%%     cases that cover the model
%%% @end
%%% Created :  8 Jan 2014 by Laura M. Castro <lcastro@udc.es>
%%%-------------------------------------------------------------------

-module(readspec).

-export([suite/3, counterexample/1]).
-export([cucumberise/1]). % EXPORTED ONLY FOR DEBUGGING PURPOSES

-define(DEBUG(IOString, Args), io:format(IOString, Args)).

suite(Model, Property, NumTests) ->
	?DEBUG("Compiling module ~p~n", [Model]),
	cover:reset(Model), % smother does not have this
	{ok, Model} = cover:compile(erlang:atom_to_list(Model) ++ ".erl"),
%	{module, Model} = smother:compile(erlang:atom_to_list(Model) ++ ".erl"),
	?DEBUG("Generating set of ~p test cases~n", [NumTests]),
%% THIS DOES NOT WORK WITH SMOTHER AT THE MOMENT
	Suite = eqc_suite:feature_based(eqc_suite:line_coverage(Model, eqc:numtests(NumTests, Property()))),
%%
	?DEBUG("Ensuring coverage~n", []), %%% [removable here downwards]
	ok = cover:reset(Model), % smother does not have this
	[] = eqc_suite:run(Property(), Suite),
	{ok, _CoverFile} = cover:analyse_to_file(Model), %%% [removable up to here]
%	{ok, _CoverFile} = smother:analyse_to_file(Model), %%% [removable up to here]
	?DEBUG("Cucumberising set of test cases~n", []),
	ok = file:write_file("suite.cucumberl", io_lib:fwrite("SCENARIO: ~p~n", [cucumberise_suite(eqc_suite:cases(Suite))])).

counterexample(Counterexample) ->
	cucumberise_teststeps_aux(Counterexample, []).

%%% -------------------------------------------------------------- %%%

cucumberise_suite(Suite) ->
	cucumberise_testcases(Suite, []).


cucumberise_testcases([], CucumberisedTestCases) ->
	CucumberisedTestCases;
cucumberise_testcases([TestCase | MoreTestCases], CucumberisedTestCases) ->
	cucumberise_testcases(MoreTestCases, [cucumberise_teststeps(TestCase) | CucumberisedTestCases]).


cucumberise_teststeps([TestCase]) ->
	cucumberise_teststeps_aux(TestCase, []);
cucumberise_teststeps(TestCase) ->
	cucumberise_teststeps_aux(TestCase, []).

cucumberise_teststeps_aux([], CucumberisedTestSteps) ->
	lists:flatten(cucumberise({scenario, lists:reverse(CucumberisedTestSteps)}));
% tests for QC properties
cucumberise_teststeps_aux(Values, []) when is_tuple(Values) ->
	cucumberise({scenario, Values});
% test steps for QC state machines
cucumberise_teststeps_aux([{set,_,Call={call,_Module,_Function,_Args}} | MoreSteps], CucumberisedTestSteps) ->
	cucumberise_teststeps_aux(MoreSteps, [Call | CucumberisedTestSteps]).


% we remove spureous cases such as {scenario, []}
cucumberise({scenario, []}) ->
	"";
% cucumberise QC property scenario
cucumberise({scenario, Values}) when is_tuple(Values) ->
	explain(Values, []);
% cucumberise QC state machine scenario
cucumberise({scenario, [Call={call,_Module,_Function,_Args} | MoreSteps]}) ->
	explain(Call, MoreSteps).


explain({call,_Module,Function,Args}, MoreSteps) ->
	"GIVEN " ++ enumerate_list([Args]) ++
    " WHEN " ++ io_lib:fwrite("~p", [Function]) ++
		explain_also(MoreSteps) ++
	" THEN ** insert property postcondition here ** \n";
explain(Values, []) ->
	"GIVEN " ++ enumerate_list(Values) ++
	" THEN ** insert property body here **".


explain_also([]) ->
	"";
explain_also([{call,_Module,Function,_ArgsNotUsedRightNow} | MoreSteps]) ->
	" AND " ++ io_lib:fwrite("~p", [Function]) ++
		explain_also(MoreSteps).

% ----- ----- ----- ----- ----- -----  ----- ----- ----- ----- ----- %

enumerate_list(Tuple) when is_tuple(Tuple) ->
	enumerate_list(erlang:tuple_to_list(Tuple));
enumerate_list(List) when is_list(List) ->
	L = [io_lib:fwrite("AND ~p ", [X]) || X <- List],
	[$A, $N, $D, 32 | T ] = lists:flatten(L),
	T.

