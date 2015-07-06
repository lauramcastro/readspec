%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Laura M. Castro <lcastro@udc.es>
%%% @copyright (C) 2014
%%% @doc
%%%     Utility module to inspect code of test properties and models.
%%% @end
%%%-------------------------------------------------------------------

-module(readspec_inspect).

-include("readspec.hrl").
-include("records.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([model_description/1, property_description/2]).
-export([property_definition/3, property_definition/4]).
-export([falsify/1]).

%% @doc Extracts the module description from the edoc comments on the source code
%% @end
-spec model_description(ModelModule :: atom()) -> ModuleDescription :: string().
model_description(ModelModule) ->
    extract_module_description(ModelModule).

%% @doc Extracts the property description from the edoc comments on the source code
%% @end
-spec property_description(ModelModule :: atom(),
                           PropertyName :: atom()) -> PropertyDescription :: string().
property_description(ModelModule, PropertyName) ->
    extract_function_description(ModelModule, PropertyName).

%% @doc Extracts the body of a property as a string from the source code
%% @end
-spec property_definition(ModelModule :: atom(),
			  PropertyName :: atom(),
			  Values :: [any()]) -> {PropertyBody :: string(), N :: integer(), Aliases :: [atom()]}.
property_definition(ModelModule, PropertyName, Values) ->
    property_definition(ModelModule, PropertyName, [], Values).

-spec property_definition(ModelModule :: atom(),
			  PropertyName :: atom(),
			  Args :: term(),
			  Values :: [any()]) -> {PropertyBody :: string(), N :: integer(), Aliases :: [atom()]}.
property_definition(ModelModule, PropertyName, Arguments, Values) ->
    FullModelModuleName = erlang:list_to_atom(erlang:atom_to_list(ModelModule) ++ ".erl"),
    [Exp] = see:scan_func_str_args(FullModelModuleName, PropertyName, Arguments),
    extract_property_definition(Exp, Values).

%% @doc Reverses the truth value of a given property (for counterexample explanation).
%% @end
-type syntaxTree() :: any(). % as used in the OTP syntax tools (erl_syntax)
-spec falsify(Scenario :: syntaxTree()) -> syntaxTree().
falsify(Scenario) ->
    falsify_aux(Scenario).

%%% -------------------------------------------------------------- %%%

extract_module_description(ModelModule) ->
    XML = get_xml_version(ModelModule),
    module = XML#xmlElement.name,
    Descriptions = lists:flatten([ Element#xmlElement.content || Element <- XML#xmlElement.content,
								 Element#xmlElement.name == description ]),
    case lists:flatten([ Element#xmlElement.content || Element <- Descriptions,
						       Element#xmlElement.name == fullDescription ]) of
	[] -> % there was no general edoc section for module
	    [];
	[FullDescription] ->
	    FullDescription#xmlText.value
    end.


extract_function_description(ModelModule, Function) ->
    FunctionName = erlang:atom_to_list(Function),
    XML = get_xml_version(ModelModule),
    module = XML#xmlElement.name,
    Functions = lists:flatten([ Element#xmlElement.content || Element <- XML#xmlElement.content,
							      Element#xmlElement.name == functions ]),
    case lists:filter(fun(Element) when is_record(Element, xmlElement) ->
			      [] =/= [ Element#xmlElement.content || Attribute <- Element#xmlElement.attributes,
								     Attribute#xmlAttribute.name == name,
								     Attribute#xmlAttribute.value == FunctionName ]
		      end, Functions) of
	[FunctionDescription] ->
	    function = FunctionDescription#xmlElement.name,
	    Descriptions = lists:flatten([ Element#xmlElement.content || Element <- FunctionDescription#xmlElement.content,
									 Element#xmlElement.name == description ]),
	    case lists:flatten([ Element#xmlElement.content || Element <- Descriptions,
							       Element#xmlElement.name == fullDescription ]) of
		[] -> % there was no edoc description for function
		    [];
		[FullDescription] ->
		    FullDescription#xmlText.value
	    end;
	[] -> % there was no edoc description for function
	    []
    end.

get_xml_version(Module) ->
    FileName = erlang:atom_to_list(Module) ++ ".erl",
    {Module, XML} = edoc_extract:source(FileName, edoc_lib:get_doc_env(FileName), []),
    XML.

% ----- ----- ----- ----- ----- -----  ----- ----- ----- ----- ----- %

extract_property_definition(Exp, Values) when is_record(Exp, exp_iface) ->
    AppDefs = Exp#exp_iface.var_defs,
    PropDefs = [extract_property_definition_aux(AppDef, Values) || AppDef <- AppDefs],
    lists:foldl(fun({Prop, NValues, Alias}, {ListofProp, NValuesAcc, ListofAlias}) ->
			{Prop ++ ListofProp, NValues+NValuesAcc, Alias ++ ListofAlias}
		end, {[], 0, []}, PropDefs).

tolerant_zip([], _) -> [];
tolerant_zip(_, []) -> [];
tolerant_zip([H1|T1], [H2|T2]) -> [{H1, H2}|tolerant_zip(T1, T2)].

extract_property_definition_aux(App, ValuesOrValList) when is_record(App, apply) ->
    {FunBody, _TNValues, PatternList} = extract_property_body(App#apply.arg_list),
    Values = case length(PatternList) of
		 0 -> [];
		 1 -> [ValuesOrValList];
		 _ -> ValuesOrValList
	     end,
    Res = lists:concat([see_logic:pattern_match(Pat, erl_syntax:abstract(Val), [nestcond:make_expansion()])
			|| {Pat, Val} <- tolerant_zip(lists:reverse(PatternList), Values)]),
    DefValues = lists:concat([[{Name, erl_syntax:concrete(Value)}
			       || #apply{name = Name, evaluated = true, value = Value} <- Applies]
			      || #expansion{applys = Applies} <- Res]),
    NValues = length(DefValues),
    ?DEBUG("Replacing ~p (as ~p, with dict ~p) in ~p~n", [Values, NValues, DefValues, FunBody]),
    case replace_values(FunBody, Values, NValues, DefValues) of
	{[],_Aliases} ->
	    FunDef = erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(App#apply.module),
									erl_syntax:operator(App#apply.call)),
					    App#apply.arg_list),
	    %% TODO: replace values in FunDef
	    ?DEBUG("Prop was empty, so petty-printing: ~p~n", [FunDef]),
	    {erl_prettypr:format(FunDef),NValues,[]};
	{{op,_,'not',Property},Aliases} ->
	    ?DEBUG("Replacement got negation of ~p (and ~p)~n", [Property,Aliases]),
	    try erl_prettypr:format(Property) of X -> {X ++ ?ISFALSE,NValues,[]}
	    catch _:_ -> {erl_prettypr:format(FunBody) ++ ?ISFALSE,NValues,Aliases} end;
	{Property,Aliases} ->
	    ?DEBUG("Replacement got ~p (and ~p)~n", [Property,Aliases]),
	    try erl_prettypr:format(Property) of X -> {X,NValues,[]}
	    catch _:_ -> {erl_prettypr:format(FunBody),NValues,Aliases} end
    end.

extract_property_body(Property) ->
    extract_property_body_aux(Property, 0, []).

extract_property_body_aux({call,_,_,CallBody}, N, PatternList) ->
    extract_property_body_aux(CallBody, N, PatternList);
extract_property_body_aux(BodyTree, N, PatternList) when is_list(BodyTree) ->
    case lists:flatten([ Clauses || {'fun',_,{clauses,Clauses}} <- BodyTree]) of
	[]                        -> {[], N, PatternList};
	[{clause,_,[Pattern],_,[Clause]}] -> extract_property_body_aux(Clause, N+1, [Pattern|PatternList])
	%[{clause,_,_,_,[Clause]}] -> extract_property_body_aux(Clause, N+1, PatternList)
    end;
extract_property_body_aux(Body, N, PatternList) ->
    {Body, N, PatternList}.


replace_values([], _, _, _) -> {[], []};
replace_values(Exp, Values, NValues, DefinedValues) ->
    {NewExp,_Values,_N,BindedValues} = transverse_exp(Exp, Values, NValues, DefinedValues),
    BindedAliases = [VarName || {VarName,VarValue} <- BindedValues, VarName =/= VarValue],
    {NewExp,lists:reverse(BindedAliases)}.


transverse_exp({match,X,{var,Y,Name},Exp}, Values, NValues, UsedValues) when is_atom(Name) ->
    {NewExp, LessValues, RestNValues, MoreUsedValues} = transverse_exp(Exp, Values, NValues, [{Name,Name}|UsedValues]),
    {{match,X,{var,Y,Name},NewExp}, LessValues, RestNValues, MoreUsedValues};
transverse_exp({var,N,Name}, Values, NValues, UsedValues) when is_atom(Name) ->
    ?DEBUG("Searching for ~p in ~p (~p)~n", [Name, UsedValues, Values]),
    case lists:keyfind(Name, 1, UsedValues) of
	false ->
	    {Value, MoreValues} = pick(Values, NValues),
	    NewValue = values_as_atoms(Value),
	    {{var,N,NewValue}, MoreValues, NValues-1, [{Name,NewValue}|UsedValues]};
	{Name,UsedValue} ->
	    {{var,N,values_as_atoms(UsedValue)}, Values, NValues, UsedValues}
    end;
transverse_exp(Exp, Values, NValues, UsedValues) when is_tuple(Exp) ->
    ExpList = erlang:tuple_to_list(Exp),
    ?DEBUG("Transversing ~p element by element~n", [ExpList]),
    {NewExpList, LessValues, RestNValues, MoreUsedValues} = transverse_exp(ExpList, Values, NValues, UsedValues),
    {list_to_tuple(NewExpList), LessValues, RestNValues, MoreUsedValues};
transverse_exp(Exp=[{atom,_,throw},{var,_,Name},{var,_,_}], Values, NValues, UsedValues) ->
    {Exp, Values, NValues, [{Name,Name}|UsedValues]};
transverse_exp([{atom,X,throw},{tuple,Y,RestExp},{var,_,_}], Values, NValues, UsedValues) ->
    {NewExp, LessValues, RestNValues, MoreUsedValues} = transverse_exp(RestExp, Values, NValues, UsedValues),
    {[{atom,X,throw},{tuple,Y,NewExp}], LessValues, RestNValues, MoreUsedValues};
transverse_exp(Exp, Values, NValues, UsedValues) when is_list(Exp) ->
    ?DEBUG("Transversing ~p element by element~n", [Exp]),
    lists:foldl(fun(Member, {RExp, Vs, NVs, UVs}) ->
			case transverse_exp(Member, Vs, NVs, UVs) of
			    {{var,N,Value}, MoreValues, RestNValues, MoreUsedValues} ->
				{RExp++[{var,N,Value}], MoreValues, RestNValues, MoreUsedValues};
			    {Other, MoreVs, NVals, MoreUsedVs} ->
				{RExp++[Other], MoreVs, NVals, MoreUsedVs}
			end
		end, {[], Values, NValues, UsedValues}, Exp);
transverse_exp(Exp, Values, NValues, UsedValues) ->
    ?DEBUG("Done transversing ~p element by element~n", [Exp]),
    {Exp, Values, NValues, UsedValues}.


falsify_aux({tree,form_list,Attributes,List}) ->
    {tree,form_list,Attributes,falsify(List)};
falsify_aux(Term = {tree,string,Attributes,Text}) ->
    [First | Words] = string:tokens(Text, " "),
    case string:equal(First, string:strip(?THEN)) of
	true ->
	    [Last, PrevToLast | RestOfWords] = lists:reverse(Words),
	    case string:equal(PrevToLast ++ " " ++ Last, string:strip(?ISFALSE)) of
		true ->
		    {tree,string,Attributes,lists:flatten([[First, 32], lists:reverse(RestOfWords), ?ISTRUE])};
		false ->
		    {tree,string,Attributes,lists:flatten([[First, 32], lists:reverse(RestOfWords), ?ISFALSE])}
	    end;
	false ->
	    Term
    end;
falsify_aux(TermList) when is_list(TermList) ->	
    [falsify(Term) || Term <- TermList];
falsify_aux(Other) ->	
    Other.

pick(V, 0) ->
    pick(V, 1);
pick([Value], 1) ->
    {Value,[]};
pick(Value, 1) ->
    {Value,[]};
pick(Values,_) when is_tuple(Values) ->
    [H|T]=erlang:tuple_to_list(Values),
    {H,erlang:list_to_tuple(T)};
pick(Values,_) when is_list(Values) ->
    [H|T]=Values,
    {H,T}.

values_as_atoms([]) ->
    '[]';
values_as_atoms(Value) when is_atom(Value) ->
    Value;
values_as_atoms(Value) when is_integer(Value) ->
    erlang:list_to_atom(to_string(Value));
values_as_atoms(Value) when is_float(Value) ->
    erlang:list_to_atom(to_string(Value));
values_as_atoms(Value) when is_tuple(Value) ->
    List = lists:foldl(fun(T, AccIn) -> AccIn ++ to_string(T) ++ ","  end, "{", erlang:tuple_to_list(Value)),
    String = string:substr(List, 1, length(List)-1) ++ "}",
    try erlang:list_to_atom(String) of X -> X catch _:_ -> list_to_tuple([values_as_atoms(V) || V <- erlang:tuple_to_list(Value)]) end;
values_as_atoms(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
	true  -> erlang:list_to_atom(Value);
	false ->
	    List = lists:foldl(fun(T, AccIn) -> AccIn ++ to_string(T) ++ ","  end, "[", Value),
	    String = string:substr(List, 1, length(List)-1) ++ "]",
	    try erlang:list_to_atom(String) of X -> X catch _:_ -> [values_as_atoms(V) || V <- Value] end
    end.

to_string(Term) when is_atom(Term) ->
    erlang:atom_to_list(Term);
to_string(Term) when is_integer(Term) ->
    erlang:integer_to_list(Term);
to_string(Term) when is_float(Term) ->
    erlang:float_to_list(Term, [{decimals,?DECIMALS}, compact]);
to_string(Term) when is_list(Term) ->
    case Term of
	[] -> "[]";
	_ -> case io_lib:printable_unicode_list(Term) of
		 true  -> "\"" ++ Term ++ "\"";
		 false -> "[" ++ lists:flatten(intersperse_comma([ to_string(T) || T <- Term])) ++ "]"
	     end
    end;
to_string(Term) when is_tuple(Term) ->
    "{" ++ intersperse_comma(erlang:tuple_to_list(Term)) ++ "}".

intersperse_comma(Term) ->
    List = lists:foldl(fun(T, AccIn) -> AccIn ++ to_string(T) ++ "," end, "", Term),
    string:substr(List, 1, length(List) - 1).
