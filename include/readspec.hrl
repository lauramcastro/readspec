%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%%     Generate human-readable versions of test models =>
%%%     by presenting human-readable versions of representative test
%%%     cases that cover the model (header file)
%%% @end
%%%-------------------------------------------------------------------

-define(DEBUG(IOString, Args), io:format(IOString, Args)).

-define(FEATURE,  "Feature: ").
-define(SCENARIO, "Scenario: ").
-define(GIVEN, "Given ").
-define(AND,   "And ").
-define(WHEN,  "When ").
-define(THEN,  "Then ").
-define(EMPTY, []).
