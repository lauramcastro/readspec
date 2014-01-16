%%% File    : simple_eqc.erl
%%% Description : Simple QuickCheck properties
%%% Created : Jan 2014

-module(simple_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_simple() ->
	?FORALL({I, L}, {int(), list(int())},
			not lists:member(I, lists:delete(I, L))).
