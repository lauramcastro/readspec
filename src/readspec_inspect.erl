%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014, Laura M. Castro
%%% @doc
%%%     Utility module to inspect code of QC properties and models.
%%% @end
%%% Created : 10 Jan 2014 by Laura M. Castro <lcastro@udc.es>
%%%-------------------------------------------------------------------

-module(readspec_inspect).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-spec property(PropertyModule :: string(),
			   PropertyName :: string()) -> PropertyTree :: tuple().
property(PropertyModule, PropertyName) ->
	{PropertyModule, PropertyName}.

-spec property_arguments(PropertyTree :: tuple()) -> [term()].
property_arguments(_PropertyTree) ->
	[].

-spec property_definition(PropertyTree :: tuple()) -> string().
property_definition(_PropertyTree) ->
	"true".
