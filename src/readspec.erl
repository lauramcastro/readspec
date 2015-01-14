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

-export([suite/2, counterexample/3]).

-include("readspec.hrl").

suite(Module, Property) ->
	suite(Module, Property, 50).

suite(Module, Property, NumTests) ->
    ?DEBUG("Cover-compiling module: ~p~n", [Module]),
    FeatureFile = erlang:atom_to_list(Module) ++ ".feature",
    {ok, Module} = cover:compile(Module),
    Suite = eqc_suite:coverage_based(Module,
				     eqc:numtests(NumTests, erlang:apply(Module, Property, []))),
    ?DEBUG("Cucumberising set of test cases: ~p~n", [Suite]),
    ok = file:write_file(FeatureFile,
			 clean(erl_prettypr:format(cucumberise_suite(Module, Property, eqc_suite:cases(Suite)),
                                                   ?PRETTYPR_OPTIONS))).

counterexample(Module, Property, [Counterexample]) ->
    FeatureFile = erlang:atom_to_list(Property) ++ ".counterexample.feature",
    ?DEBUG("Generating counterexample file: ~p~n", [FeatureFile]),
    Scenario = cucumberise_teststeps_aux(Module, Property, Counterexample, []),
    ?DEBUG("Reversing scenario: ~p~n", [readspec_inspect:falsify(Scenario)]),
    ok = file:write_file(FeatureFile, clean(erl_prettypr:format(readspec_inspect:falsify(Scenario),
                                                                ?PRETTYPR_OPTIONS))).

%%% -------------------------------------------------------------- %%%

cucumberise_suite(Module, Property, Suite) ->
    FeatureName = erlang:atom_to_list(Module) -- "_eqc",
    Scenarios = cucumberise_testcases(Module, Property, Suite, []),
    erl_syntax:form_list([erl_syntax:string(?FEATURE ++ FeatureName),
			  erl_syntax:comment(?EMPTY),
			  erl_syntax:comment(2, [readspec_inspect:model_description(Module)])] ++
			     [ erl_syntax:form_list([erl_syntax:comment(?EMPTY),
						     erl_syntax:comment(?EMPTY),
						     erl_syntax:string(?SCENARIO ++
									   readspec_inspect:property_description(Module, Property)),
						     Scenario,
						     erl_syntax:comment(?EMPTY)]) || Scenario <- Scenarios ] ).


cucumberise_testcases(_Module, _Property, [], CucumberisedTestCases) ->
    lists:reverse(CucumberisedTestCases);
cucumberise_testcases(Module, Property, [TestCase | MoreTestCases], CucumberisedTestCases) ->
    cucumberise_testcases(Module, Property, MoreTestCases, [cucumberise_teststeps(Module, Property, TestCase) | CucumberisedTestCases]).


cucumberise_teststeps(Module, Property, [TestCase]) ->
    cucumberise_teststeps_aux(Module, Property, TestCase, []);
cucumberise_teststeps(Module, Property, TestCase) ->
    cucumberise_teststeps_aux(Module, Property, TestCase, []).

cucumberise_teststeps_aux(Module, Property, [], CucumberisedTestSteps) ->
    erl_syntax:form_list(cucumberise(Module, Property,
				     {scenario, lists:reverse(CucumberisedTestSteps)}));
% tests for QC properties
cucumberise_teststeps_aux(Module, Property, Values, []) when is_tuple(Values) ->
    erl_syntax:form_list(cucumberise(Module, Property,
				     {scenario, Values}));
% === TODO: refactor these
cucumberise_teststeps_aux(Module, Property, Value, []) when is_integer(Value) ->
    erl_syntax:form_list(cucumberise(Module, Property,
				     {scenario, Value}));
cucumberise_teststeps_aux(Module, Property, Value, []) when is_atom(Value) ->
    erl_syntax:form_list(cucumberise(Module, Property,
				     {scenario, Value}));
cucumberise_teststeps_aux(Module, Property, Values, []) when is_list(Values) ->
    erl_syntax:form_list(cucumberise(Module, Property,
				     {scenario, Values}));
% === ==== ==== ====
% test steps for QC state machines
cucumberise_teststeps_aux(Module, Property, [{set,_,Call={call,_Module,_Function,_Args}} | MoreSteps], CucumberisedTestSteps) ->
    cucumberise_teststeps_aux(Module, Property, MoreSteps, [Call | CucumberisedTestSteps]).


% we remove spureous cases such as {scenario, []}
cucumberise(_Module, _Property, {scenario, []}) ->
    erl_syntax:nil();
% cucumberise QC property scenario
cucumberise(Module, Property, {scenario, Values}) when is_tuple(Values) ->
    explain(Module, Property, Values, []);
% === TODO: refactor these
cucumberise(Module, Property, {scenario, Value}) when is_integer(Value) ->
    explain(Module, Property, Value, []);
cucumberise(Module, Property, {scenario, Value}) when is_atom(Value) ->
    explain(Module, Property, Value, []);
cucumberise(Module, Property, {scenario, Values}) when is_list(Values) ->
    explain(Module, Property, Values, []);
% === ==== ==== ====
% cucumberise QC state machine scenario
cucumberise(Module, Property, {scenario, [Call={call,_,_,_} | MoreSteps]}) ->
    explain(Module, Property, Call, MoreSteps).


explain(_Module, _Property, {call,_,Function,Args}, MoreSteps) ->
    "GIVEN " ++ enumerate_list([Args]) ++
	" WHEN " ++ io_lib:fwrite("~p", [Function]) ++
	explain_also(MoreSteps) ++
	" THEN ** insert property postcondition here ** ";
explain(Module, Property, Values, []) ->
    [erl_syntax:comment(?EMPTY),
     erl_syntax:string(?GIVEN),
     erl_syntax:form_list(enumerate_list(Values)),
     erl_syntax:string(?THEN  ++ 
			   readspec_inspect:property_definition(Module, Property, Values)),
     erl_syntax:comment(?EMPTY)].


explain_also([]) ->
    "";
explain_also([{call,_Module,Function,_ArgsNotUsedRightNow} | MoreSteps]) ->
    " AND " ++ io_lib:fwrite("~p", [Function]) ++
	explain_also(MoreSteps).


% ----- ----- ----- ----- ----- -----  ----- ----- ----- ----- ----- %

% === TODO: refactor these
enumerate_list(Integer) when is_integer(Integer) ->
    identify(Integer);
enumerate_list(Atom) when is_atom(Atom) ->
    identify(Atom);
% === ==== ==== ====
enumerate_list(Tuple) when is_tuple(Tuple) ->
    enumerate_list(erlang:tuple_to_list(Tuple));
enumerate_list(List) when is_list(List) ->
    L = [identify(X) || X <- List],
    [_H | T] = lists:flatten(L),
    T.

identify(X) ->
    ASTofX = erl_syntax:abstract(X),
    [erl_syntax:string(?AND), 
     erl_syntax:string(?OPERAND),
     erl_syntax:string(type_of(X)), % === TODO: check consistency with property definition
     ASTofX,
     erl_syntax:comment(?EMPTY)].

type_of([]) ->
    ?LIST;
type_of(X) when is_integer(X) ->
    ?INTEGER;
type_of(X) when is_boolean(X) ->
    ?BOOLEAN;
type_of(X) when is_atom(X) ->
    ?ATOM;
type_of(X) when is_list(X) ->
    case is_string(X) of
	true  -> ?STRING;
	false -> ?LIST
    end;
type_of(X) when is_tuple(X) ->
    ?TUPLE;
type_of(_) ->
    ?UNKNOWN.

is_string(X) ->
    is_list(X) andalso lists:all(fun(C) -> is_char(C) end, X).

is_char(C) when is_integer(C) ->
    ((32 =< C) andalso (C =< 126)) orelse ((161 =< C) andalso (C =< 255)).


clean(StringStream) ->
    trim_lines(lists:filter(fun($") -> false;
			       ($%) -> false;
			       (_C) -> true   end,
			    StringStream)).

trim_lines([]) ->
    [];
trim_lines([$\n, $\n, $\n, $\n | T]) ->
    [$\n, $\n | trim_lines(T)];
trim_lines([$\n, $\n, $n | T]) ->
    [$\n | trim_lines(T)];
trim_lines([$\n, $\n | T]) ->
    trim_lines(T);
trim_lines([H|T]) ->
    [H | trim_lines(T)].
