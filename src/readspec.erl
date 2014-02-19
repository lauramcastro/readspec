%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%%     Generate human-readable versions of test models =>
%%%     by presenting human-readable versions of representative test
%%%     cases that cover the model
%%% @end
%%%-------------------------------------------------------------------

-module(readspec).

-export([suite/3, counterexample/1]).

-include("readspec.hrl").

suite(Model, Property, NumTests) ->
	?DEBUG("Compiling module ~p~n", [Model]),
	ModelFile = erlang:atom_to_list(Model) ++ ".erl",
	FeatureFile = erlang:atom_to_list(Model) ++ ".feature",
	{ok, ModelName} = cover:compile(ModelFile), % {module, ModelName} = smother:compile(ModelFile),
	?DEBUG("Generating set of ~p test cases~n", [NumTests]),
	Suite = eqc_suite:feature_based(eqc_suite:line_coverage(Model, eqc:numtests(NumTests, Property()))),
    %%% [removable here downwards]
	?DEBUG("Ensuring coverage~n", []),
	ok = cover:reset(ModelName), % smother does not have this
	[] = eqc_suite:run(Property(), Suite),
	{ok, _CoverFile} = cover:analyse_to_file(ModelName), % {ok, _CoverFile} = smother:analyse_to_file(ModelName),
    %%% [removable up to here]
	?DEBUG("Cucumberising set of test cases~n", []),
	ok = file:write_file(FeatureFile,
						 clean(erl_prettypr:format(cucumberise_suite(Model, eqc_suite:cases(Suite)),
												   [{encoding, utf8}, {paper, 120}, {ribbon, 120}]))).

counterexample(Counterexample) ->
	cucumberise_teststeps_aux(Counterexample, []).

%%% -------------------------------------------------------------- %%%

cucumberise_suite(Model, Suite) ->
	FeatureName = erlang:atom_to_list(Model) -- "_eqc",
	Scenarios = cucumberise_testcases(Suite, []),
	erl_syntax:form_list([erl_syntax:string(?FEATURE ++ FeatureName),
						  erl_syntax:comment(2, [readspec_inspect:model_description(Model)])] ++
							 [ erl_syntax:form_list([erl_syntax:comment(?EMPTY),
													 erl_syntax:string(?SCENARIO ++ readspec_inspect:property_description(Model, "don't know the property")),
													 Scenario]) || Scenario <- Scenarios ] ).


cucumberise_testcases([], CucumberisedTestCases) ->
	CucumberisedTestCases;
cucumberise_testcases([TestCase | MoreTestCases], CucumberisedTestCases) ->
	cucumberise_testcases(MoreTestCases, [cucumberise_teststeps(TestCase) | CucumberisedTestCases]).


cucumberise_teststeps([TestCase]) ->
	cucumberise_teststeps_aux(TestCase, []);
cucumberise_teststeps(TestCase) ->
	cucumberise_teststeps_aux(TestCase, []).

cucumberise_teststeps_aux([], CucumberisedTestSteps) ->
	erl_syntax:form_list(cucumberise({scenario, lists:reverse(CucumberisedTestSteps)}));
% tests for QC properties
cucumberise_teststeps_aux(Values, []) when is_tuple(Values) ->
	erl_syntax:form_list(cucumberise({scenario, Values}));
% test steps for QC state machines
cucumberise_teststeps_aux([{set,_,Call={call,_Module,_Function,_Args}} | MoreSteps], CucumberisedTestSteps) ->
	cucumberise_teststeps_aux(MoreSteps, [Call | CucumberisedTestSteps]).


% we remove spureous cases such as {scenario, []}
cucumberise({scenario, []}) ->
	erl_syntax:nil();
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
	" THEN ** insert property postcondition here ** ";
explain(Values, []) ->
	[erl_syntax:string(?GIVEN),
	 erl_syntax:form_list(enumerate_list(Values)),
	 erl_syntax:string(?THEN  ++ 
						   readspec_inspect:property_definition("don't know the module here",
																"don't know the function here"))].


explain_also([]) ->
	"";
explain_also([{call,_Module,Function,_ArgsNotUsedRightNow} | MoreSteps]) ->
	" AND " ++ io_lib:fwrite("~p", [Function]) ++
		explain_also(MoreSteps).

% ----- ----- ----- ----- ----- -----  ----- ----- ----- ----- ----- %

enumerate_list(Tuple) when is_tuple(Tuple) ->
	enumerate_list(erlang:tuple_to_list(Tuple));
enumerate_list(List) when is_list(List) ->
	L = [[erl_syntax:string(?AND), erl_syntax:abstract(X)] || X <- List],
	[_H | T] = lists:flatten(L),
	T.


clean(StringStream) ->
	trim_lines(lists:filter(fun($") -> false;
							   ($%) -> false;
							   (_C) -> true   end,
							StringStream)).
						 
trim_lines([]) ->
	[];
trim_lines([H]) ->
	[H];
trim_lines([H1,H2]) ->
	[H1,H2];
trim_lines([H1,H2,H3]) ->
	[H1,H2,H3];
trim_lines([$\n, $\n | T]) ->
	[$\n | trim_lines(T)];
trim_lines([H|T]) ->
	[H | trim_lines(T)].

