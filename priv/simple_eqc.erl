%%% -*- coding: utf-8 -*-
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%% Simple QuickCheck properties
%%% @end
-module(simple_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_simple/0]).

%% @doc This is a property to test something interesting
%% @spec prop_simple() -> boolean()
%% @end
-spec prop_simple() -> boolean().
prop_simple() ->
	?FORALL({I, L}, {int(), list(int())},
			not lists:member(I, lists:delete(I, L))).
