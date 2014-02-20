%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%%     Utility module to inspect code of test properties and models.
%%% @end
%%%-------------------------------------------------------------------

-module(readspec_inspect).

-include_lib("xmerl/include/xmerl.hrl").

-export([model_description/1, property_definition/2, property_description/2]).

%% @doc Extracts the module description from the edoc comments on the source code
%% @end
-spec model_description(ModelModule :: string()) -> ModuleDescription :: string().
model_description(ModelModule) ->
	extract_module_description(ModelModule).

%% @doc Extracts the property description from the edoc comments on the source code
%% @end
-spec property_description(ModelModule :: string(),
						   PropertyName :: string()) -> PropertyDescription :: string().
property_description(ModelModule, PropertyName) ->
	extract_function_description(ModelModule, PropertyName).

%% @doc Extracts the body of a property as a string from the source code
%% @end
-spec property_definition(ModelModule :: string(),
						  PropertyName :: string()) -> PropertyBody :: string().
property_definition(_ModelModule, PropertyName) ->
	"*** extract property description of " ++ PropertyName ++ " ***".



%%% -------------------------------------------------------------- %%%

extract_module_description(ModelModule) ->
	XML = get_xml_version(ModelModule),
	module = XML#xmlElement.name,
	Descriptions = lists:flatten([ Element#xmlElement.content || Element <- XML#xmlElement.content,
																 Element#xmlElement.name == description ]),
	[FullDescription] = lists:flatten([ Element#xmlElement.content || Element <- Descriptions,
																	  Element#xmlElement.name == fullDescription ]),
	FullDescription#xmlText.value.


extract_function_description(ModelModule, Function) ->
	FunctionName = erlang:atom_to_list(Function),
	XML = get_xml_version(ModelModule),
	module = XML#xmlElement.name,
	Functions = lists:flatten([ Element#xmlElement.content || Element <- XML#xmlElement.content,
															  Element#xmlElement.name == functions ]),
	[FunctionDescription] = lists:filter(fun(Element) when is_record(Element, xmlElement) -> 
												 [] =/= [ Element#xmlElement.content || Attribute <- Element#xmlElement.attributes,
																						Attribute#xmlAttribute.name == name,
																						Attribute#xmlAttribute.value == FunctionName ]
										 end, Functions),
	function = FunctionDescription#xmlElement.name,
	Descriptions = lists:flatten([ Element#xmlElement.content || Element <- FunctionDescription#xmlElement.content,
																 Element#xmlElement.name == description ]),
	[FullDescription] = lists:flatten([ Element#xmlElement.content || Element <- Descriptions,
																	  Element#xmlElement.name == fullDescription ]),
	FullDescription#xmlText.value.

get_xml_version(Module) ->
	FileName = erlang:atom_to_list(Module) ++ ".erl",
	{Module, XML} = edoc_extract:source(FileName, edoc_lib:get_doc_env(FileName), []),
	XML.


% ----- ----- ----- ----- ----- -----  ----- ----- ----- ----- ----- %

%    {ok, Forms} = epp:parse_file(FileName, [], []),
%    Comments = erl_comment_scan:file(FileName),
%    AST = erl_recomment:recomment_forms(Forms, Comments),
