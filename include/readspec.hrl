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

%-define(DEBUG(IOString, Args), io:format(IOString, Args)).
-define(DEBUG(IOString, Args), ok).

-define(FEATURE,  "FEATURE: ").
-define(SCENARIO, "SCENARIO: ").
-define(GIVEN, "GIVEN ").
-define(AND,   "AND ").
-define(WHEN,  "WHEN ").
-define(THEN,  "THEN ").
-define(EMPTY, []).

-define(OPERAND, "I have ").
-define(INTEGER, "the integer ").
-define(BOOLEAN, "the boolean ").
-define(ATOM,    "the atom ").
-define(STRING,  "the string ").
-define(LIST,    "the list ").
-define(TUPLE,   "the tuple ").
-define(UNKNOWN, "the input value ").

-define(ISFALSE, " IS FALSE.").
-define(ISTRUE,  " IS TRUE.").

-define(NUMTESTS, 100).
-define(PRETTYPR_OPTIONS, [{encoding, utf8}, {paper, 120}, {ribbon, 120}]).
