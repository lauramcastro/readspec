%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%%     Utility module to inspect code of QC properties and models.
%%% @end
%%%-------------------------------------------------------------------

-module(readspec_inspect).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% @doc Extracts the module description from the edoc comments on the source code
%% @end
-spec model_description(ModelModule :: string()) -> ModuleDescription :: string().
model_description(ModelModule) ->
	"*** extract description of " ++ erlang:atom_to_list(ModelModule) ++ " ***".

%% @doc Extracts the body of a property as a string from the source code
%% @end
-spec property_definition(ModelModule :: string(),
						  PropertyName :: string()) -> PropertyBody :: string().
property_definition(_ModelModule, PropertyName) ->
	"*** extract property body of " ++ PropertyName ++ " ***".

%% @doc Extracts the property description from the edoc comments on the source code
%% @end
-spec property_description(ModelModule :: string(),
						   PropertyName :: string()) -> PropertyDescription :: string().
property_description(_ModelModule, PropertyName) ->
	"*** extract property description of " ++ PropertyName ++ " ***".
