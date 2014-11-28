%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%%     Unit tests for readSpec.
%%% @end
%%%-------------------------------------------------------------------
-module(readspec_test).

-include_lib("eunit/include/eunit.hrl").

suite_test() ->
    {setup,
     fun()  -> setup_ok    end,
     fun(_) -> teardown_ok end,
     fun(_) ->
	     {inparallel,
	      [?_assert(true)]}
     end}.

counterexample_test() ->
    ?assert(not false).
