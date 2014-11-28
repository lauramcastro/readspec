%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%%     PBTests for readSpec.
%%% @end
%%%-------------------------------------------------------------------
-module(readspec_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_suite() ->
    ?FORALL(X, bool(), X or not X).

prop_counterexample() ->
    ?FORALL(X, bool(), X or not X).
