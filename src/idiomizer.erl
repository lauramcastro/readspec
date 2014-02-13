%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Defines functions to add information about idioms to the structure.
%%% It also defines a framework to tag Erlang elements inside an Erlang
%%% structure so that they can be extracted and modified in place.
%%% Functions that start with "ts" define "templates tags" designed to
%%% work with erl_syntax elements. Functions that start with "t"
%%% define generic "templates tags" that may potentially work with any Erlang
%%% term. Template tags use a common reference that is used to tell
%%% apart Erlang terms that are part of the template tags from the ones
%%% that are not.
%%% For example, we may build a template like this:
%%% <p><code>
%%% Ref = make_ref().
%%% Template = {1, idiomizer:tvar(Ref, 'number_in_the_middle'), 3}.
%%% </code></p>
%%% And then we can match that template with any Erlang term
%%% by using {@link idiomizer:extract_vars/4} as follows:
%%% <p><code>
%%% {match, Dict} = idiomizer:extract_vars(Ref, Template, {1, 2, 3}, dict:new()).
%%% </code></p>
%%% Where <code>dict:to_list(Dict)</code> would be:
%%% <p><code> [{number_in_the_middle,2}] </code></p>
%%% And if we use templates which have the same name for several tags,
%%% {@link idiomizer:extract_vars/4} will check that they all match the
%%% same value.
%%% @end
%%% Created : 5 Feb 2014 by Pablo Lamela
%%%-------------------------------------------------------------------

-module(idiomizer).

-export([idiomize_module_info/1,
	 tsatom_val/2, tsint_val/2, tsvar_name/2, tsvar_name_and_id/3, tsvar_id/2,
	 tvar/2, tobj/2, extract_vars/4, replace_vars/3, tsoneellist_elid/2]).

-include("records.hrl").
-include("template_recs.hrl").

%% General Function

%%% @doc
%%% Generates idiom information for a <code>#module_iface{}</code> record.
%%% It generates both generic information about statem model expansions,
%%% and it also detects common patterns defined in
%%% {@link idiom_definitions:idiom_template_list/1}
%%% @param MIface the <code>#module_iface{}</code> record to idiomize
%%% @return the idiomized <code>#module_iface{}</code> record
-spec idiomize_module_info(MIface :: #module_iface{}) -> #module_iface{}.
idiomize_module_info(MIface) ->
    Ref = make_ref(),
    ArgsIdiomized = idiomize_module_info_args(MIface),
    idiomize_module_info_idioms(
      idiom_definitions:idiom_template_list(Ref),
      ArgsIdiomized, Ref).

% Args idiomize

idiomize_module_info_args(#module_iface{state_fields = {_StateModuleName,
							StateModuleFields},
					call_list = CallList} = MIface) ->
    RecordIdioms = make_record_idiom_list(StateModuleFields),
    MIface#module_iface{call_list = [idiomize_call_info(Call, RecordIdioms)
				     || Call <- CallList]}.

idiomize_call_info(#call_iface{
		      name = FuncName,
		      num_args = ArgNum,
		      pre_exp = PreExps,
		      next_exp = NextExps,
		      post_exp = PostExps} = CallInfo,
		   StateModuleFields) ->
    Idioms = generate_arg_idiom_list({FuncName, ArgNum}, ArgNum)
	++ [res_idiom({FuncName, ArgNum})|StateModuleFields],
    CallInfo#call_iface{pre_exp = [add_idioms_to_exp(PreExp, Idioms) || PreExp <- PreExps],
			next_exp = [add_idioms_to_exp(NextExp, Idioms) || NextExp <- NextExps],
			post_exp = [add_idioms_to_exp(PostExp, Idioms) || PostExp <- PostExps]}.

add_idioms_to_exp(Expansion, []) -> Expansion;
add_idioms_to_exp(Expansion, [{Name, Repr, Subs}|Rest]) ->
    add_idioms_to_exp(nestcond:add_idiom(Name, Repr, Subs, Expansion), Rest).

generate_arg_idiom_list(_, 0) -> [];
generate_arg_idiom_list({Func, ArgNum}, ArgNumLeft) ->
    [begin
	 ArgVarName = model_info:create_arg_name(arg, ArgNumLeft),
	 {ArgVarName, "the " ++ ordinal_for(ArgNumLeft) ++
	      " argument passed to " ++ func_str(Func, ArgNum), []}
     end
     |generate_arg_idiom_list({Func, ArgNum}, ArgNumLeft - 1)].

ordinal_for(Number) ->
    integer_to_list(Number) ++ ordinal_suffix(Number rem 10).
ordinal_suffix(1) -> "st";
ordinal_suffix(2) -> "nd";
ordinal_suffix(3) -> "rd";
ordinal_suffix(_) -> "th".

make_record_idiom_list(RecordFields) ->
    [begin
	 FieldVarName = model_info:create_arg_name(state, FieldName),
	 {FieldVarName, "the field called \"" ++ atom_to_list(FieldName) ++ "\"", []}
     end || {FieldName, _} <- RecordFields].

res_idiom({Func, ArgNum}) ->
    ResVarName = model_info:create_arg_name(result, result),
    {ResVarName, "the result of calling " ++ func_str(Func, ArgNum), []}.

func_str(Func, ArgNum) ->
    atom_to_list(Func) ++ [$/|integer_to_list(ArgNum)].


% Generic idiomize and clean

idiomize_module_info_idioms(Idioms, MIface, Ref) ->
    clean_nestcond:map_model_expansions(
      fun (X) -> idiomize_expiface_idioms({#etd{idioms = Idioms, in_ref = Ref}, X}) end,
      MIface).

idiomize_expiface_idioms({ETD, ExpIface}) ->
    case idiomize_expiface_idioms(ETD#etd.idioms,
				  {ETD, ExpIface}, fail) of
	{fail, _} -> clean_nestcond:clean_expansion(ExpIface);
	{success, {NewETD, NewExpIface}} ->
	    idiomize_expiface_idioms({NewETD, NewExpIface})
    end.

idiomize_expiface_idioms([], ETD_ExpIf, Success) -> {Success, ETD_ExpIf};
idiomize_expiface_idioms([IdiomHead|IdiomTail] = IdiomList, ETD_ExpIf, Success) ->
    case try_to_apply_idiom(IdiomHead, ETD_ExpIf) of
	fail -> idiomize_expiface_idioms(IdiomTail, ETD_ExpIf, Success);
	{success, NewETD_ExpIf} ->
	    idiomize_expiface_idioms(IdiomList, NewETD_ExpIf, success)
    end.

try_to_apply_idiom(#idiom_template{srcs = Srcs, targets = Targets}, ETD_ExpIf) ->
    case try_to_match(Srcs, ETD_ExpIf) of
	fail -> fail;
	{match, {NewETD, ExpWithPH}, Rep} ->
	    {NewSecDict, IdiomsF} = apply_idiom_targets(Targets,
							replace_mapper(Rep#rep.idiom_ref,
								       Rep#rep.idiom_dict),
							Rep#rep.sec_dict,
							ExpWithPH#exp_iface.idioms),
	    {success, {NewETD, replace_vars(Rep#rep.sec_ref, NewSecDict,
							   ExpWithPH#exp_iface{idioms = IdiomsF})}}
    end.

apply_idiom_targets([], _, SecDict, Idioms) -> {SecDict, Idioms};
apply_idiom_targets([#set_idiom_for_apply{apply_name = Name,
					  value = Value}|Rest],
		    ReplacerFunc, SecDict, Idioms) ->
    apply_idiom_targets(Rest, ReplacerFunc, SecDict,
			set_idiom(Name, ReplacerFunc(Value), Idioms));
apply_idiom_targets([#place_holder_action{label = Label,
					  action = replace,
					  value = Value}|Rest],
		    ReplacerFunc, SecDict, Idioms) ->
    apply_idiom_targets(Rest, ReplacerFunc,
			dict:store(Label, ReplacerFunc(Value), SecDict),
			Idioms).

set_idiom(Name, Value, Idioms) ->
    lists:keydelete(Name, #idiom.name, Idioms) ++ [Value].

try_to_match(Srcs, {ETD, ExpIface}) ->
    case clean_nestcond:bt_mapfold_through_exp(fun extract_if_match/2,
					       #rep{
						  rem_srcs = Srcs,
						  idiom_dict = dict:new(),
						  idiom_ref = ETD#etd.in_ref,
						  sec_dict = dict:new(),
						  sec_ref = make_ref(),
						  etd = ETD
						 },
					       ExpIface) of
	{NewExpIface, #rep{rem_srcs = [], etd = NewETD} = Rep} ->
	    {match, {NewETD, NewExpIface}, Rep};
	_ -> fail
    end.

extract_if_match(Elem, #rep{} = Acc) ->
    case any_match(Acc#rep.rem_srcs, [], Elem, Acc) of
	{match, NewRemainingSources, MatchLabel, IdiomDict, NewETD} ->
	    {case NewRemainingSources of
		 [] -> finished;
		 _ -> success
	     end, tvar(Acc#rep.sec_ref, MatchLabel),
	     Acc#rep{
	       rem_srcs = NewRemainingSources,
	       idiom_dict = IdiomDict,
	       sec_dict = case dict:is_key(MatchLabel, Acc#rep.sec_dict) of
			      false -> dict:store(MatchLabel, Elem, Acc#rep.sec_dict);
			      true -> throw({duplicated_label, MatchLabel})
			  end,
	       etd = NewETD
	      }
	    };
	no_match -> {failure, Elem, Acc}
    end.

any_match([], _, _, _) -> no_match;
any_match([#place_holder{label = Label,
			 item = Item,
			 match_just_once = MatchJustOnce} = Ph|Rest],
	  Others, Elem, Rep) ->
    %% For definition debug:
    %% io:format("******************~n~p~n**************~n", [Elem]),
    %% io:format("~p~n", [extract_vars(InRef, Item, Elem, Dict)]),
    case  check_match_just_once(extract_vars(Rep#rep.idiom_ref,
					     Item, Elem,
					     Rep#rep.idiom_dict),
				MatchJustOnce, Rep#rep.etd, Elem, Label) of
	{{match, ResultDict}, NewETD} -> {match, Rest ++ Others, Label, ResultDict,
							 NewETD};
	{{no_match, _}, _} -> any_match(Rest, [Ph|Others], Elem, Rep)
    end.

check_match_just_once({match, RD} = Tuple, true,
		     #etd{matched_applies = Set} = ETD,
		     Elem, Label) ->
    case sets:is_element({Elem, Label}, Set) of
	false -> NewETD = ETD#etd{
			   matched_applies = sets:add_element({Elem, Label}, Set)},
		{Tuple, NewETD};
	true -> {{no_match, RD}, ETD}
    end;
check_match_just_once(Tuple, _, ETD, _, _) ->
    {Tuple, ETD}.


%% Tag extraction

%%% @doc
%%% Matches an Erlang structure (Target) with a template and
%%% stores in a dictionary the subterms that match with the
%%% tags in the template. If several tags have the same
%%% name, it ensures that all of them have the same Erlang
%%% structure associated.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param Template the template that must be used.
%%% @param Target the Erlang structure to match to the template.
%%% @param Dict the dictionary that will save the information extracted. It
%%% may be empty and it may be the one returned by another call to this function,
%%% this way it acts as an accumulator.
%%% @return a tuple with the atom <code>match</code> and the resulting
%%% dictionary if the template matches the target, or else the atom
%%% <code>no_match</code> and a term explaining the reason why it
%%% did not match. If the template produces an exception while matching
%%% this is also considered a <code>no_match</code>.
%%% @see replace_vars/3
-spec extract_vars(reference(), Template :: any(), Target :: any(), Dict :: dict()) ->
			  {'match', dict()} | {'no_match', Err :: any()}.
extract_vars(Ref, Template, Target, Dict) ->
    try extract_vars({Template, Target}, {Ref, Dict}) of
	{_, Result} -> {match, Result}
    catch
	exit:{no_match, Err} -> {no_match, Err}
    end.
extract_vars({#tcontainer{ref = Ref, content = Content},
	      Sth}, {Ref, Result}) ->
    case Content of
	#tvar_rec{} -> tvar_match({Content, Sth}, {Ref, Result});
	#tobj_rec{} -> tobj_match({Content, Sth}, {Ref, Result})
    end;
extract_vars({Same, Same}, {Ref, Result}) -> {Ref, Result};
extract_vars({List1, List2}, {Ref, Result}) when is_list(List1), is_list(List2) ->
    case {length(List1), length(List2)} of
	{Length, Length} -> lists:foldl(fun extract_vars/2, {Ref, Result}, lists:zip(List1, List2));
	_ -> exit({no_match, {different_length, List1, List2}})
    end;
extract_vars({Tuple1, Tuple2}, {Ref, Result}) when is_tuple(Tuple1), is_tuple(Tuple2) ->
    extract_vars({tuple_to_list(Tuple1), tuple_to_list(Tuple2)}, {Ref, Result});
extract_vars({Sth1, Sth2}, {_, _}) -> exit({no_match, {different_term, Sth1, Sth2}}).

tvar_match({#tvar_rec{id = Id}, Sth}, {Ref, Result}) ->
    case dict:find(Id, Result) of
	{ok, Sth} -> {Ref, Result};
	error -> {Ref, dict:store(Id, Sth, Result)};
	{ok, SthElse} -> exit({no_match, {different_var, Id, SthElse, Sth}})
    end.

tobj_match({#tobj_rec{func_list = List}, Sth}, {Ref, Result}) ->
    {_, NewRef, NewResult} = lists:foldl(fun tobj_match_aux/2, {Sth, Ref, Result}, List),
    {NewRef, NewResult}.

tobj_match_aux({identity, Sth1}, {Sth2, Ref, Result}) ->
    {NewRef, NewResult} = extract_vars({Sth1, Sth2}, {Ref, Result}),
    {Sth2, NewRef, NewResult};
tobj_match_aux({{Module, Function}, Sth1}, {RawSth2, Ref, Result}) ->
    Sth2 = try erlang:apply(Module, Function, [RawSth2])
	   catch
	       ET:Err -> exit({no_match, {error_on_apply, {ET, Err}}})
	   end,
    {NewRef, NewResult} = extract_vars({Sth1, Sth2}, {Ref,Result}),
    {RawSth2, NewRef, NewResult}.


%% Tag replacement

% replace_mapper: Produces an anonymous function with one argument X.
% When called, the anonymous function will call replace_vars with as follows:
% replace_vars(Ref, Dict, X, ItSelf) where ItSelf is the anonymous function with one argument
replace_mapper(Ref, Dict) ->
    F = fun (IRef, IDict, Elem, MySelf) ->
		replace_vars(IRef, IDict, Elem, MySelf)
	end,
    G = fun (IG, IF, IRef, IDict) ->
		fun (X) -> IF(IRef, IDict, X, IG(IG, IF, IRef, IDict)) end
	end,
    G(G, F, Ref, Dict).

%%% @doc
%%% Replaces the elements in the dictionary Dict inside the template Template.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param Dict dictionary that stores the substitutions to make in the template.
%%% On each entry of the dictionary, the key represents the name of a tag, and
%%% the value represents the term that must be swapped with the tag. The dict
%%% is of the same kind that the one generated by the function extract_vars.
%%% All the names of the tags inside the template must have their entry on
%%% the dictionary or otherwise the function will throw the exception
%%% <code>unbound_variable</code>.
%%% @param Template the template that must be used. Templates that are used
%%% with replace_vars/3 function cannot have tags of the kind tobj.
%%% @return the element generated from the template
%%% @throws {unbound_variable, term()}
%%% @see extract_vars/4
-spec replace_vars(Ref :: reference(), Dict :: dict(), Template :: any()) -> any().
replace_vars(Ref, Dict, Element) ->
    F = replace_mapper(Ref, Dict),
    F(Element).
replace_vars(Ref, Dict, #tcontainer{ref = Ref, content = Content}, _) ->
    case Content of
	#tvar_rec{} -> tvar_set(Dict, Content);
	#tobj_rec{} -> throw({tobj, not_permitted_here})
    end;
replace_vars(_, _, List, MySelf) when is_list(List) ->
    lists:map(MySelf, List);
replace_vars(_, _, Tuple, MySelf) when is_tuple(Tuple) ->
    list_to_tuple(MySelf(tuple_to_list(Tuple)));
replace_vars(_, _, Elem, _) ->
    Elem.

tvar_set(Dict, #tvar_rec{id = Id}) ->
    case dict:find(Id, Dict) of
	{ok, Sth} -> Sth;
	error -> throw({unbound_variable, Id})
    end.


%% Tagging funcs

%%% @doc
%%% Creates a template that matches an atom in abstract syntax.
%%% It uses {@link tobj/2} underneath and because of that it cannot be
%%% used with {@link replace_vars/3}.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param ValueTemplate template that will be matched to the result of applying
%%% erl_syntax:atom_value/1 to the target Erlang term.
%%% @return the corresponding "template tag"
%%% @see extract_vars/4
-spec tsatom_val(Ref :: reference(), ValueTemplate :: term()) -> TemplateTag :: #tcontainer{}.
tsatom_val(Ref, Value) ->
    tobj(Ref,
	[{{erl_syntax, type}, atom},
         {{erl_syntax, atom_value}, Value}]).


%%% @doc
%%% Creates a template that matches an integer in abstract syntax.
%%% It uses {@link tobj/2} underneath and because of that it cannot be
%%% used with {@link replace_vars/3}.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param ValueTemplate template that will be matched to the result of applying
%%% erl_syntax:integer_value/1 to the target Erlang term.
%%% @return the corresponding "template tag"
%%% @see extract_vars/4
-spec tsint_val(Ref :: reference(), ValueTemplate :: term()) -> TemplateTag :: #tcontainer{}.
tsint_val(Ref, Value) ->
    tobj(Ref,
	 [{{erl_syntax, type}, integer},
	  {{erl_syntax, integer_value}, Value}]).

%%% @doc
%%% Equivalent to {@link tsvar_name_and_id/3} but it does not include a
%%% template for the variable name.
%%% It uses {@link tobj/2} underneath and because of that it cannot be
%%% used with {@link replace_vars/3}.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param ValueTemplate template that will be matched to the target Erlang term.
%%% @return the corresponding "template tag"
%%% @see extract_vars/4
%%% @see tsvar_name_and_id/3
-spec tsvar_id(Ref :: reference(), ValueTemplate :: term()) -> TemplateTag :: #tcontainer{}.
tsvar_id(Ref, Id) ->
    tobj(Ref,
	 [{{erl_syntax, type}, variable},
	  {identity, Id}]).

%%% @doc
%%% Equivalent to {@link tsvar_name_and_id/3} but it does not include a
%%% template for the target Erlang term.
%%% It uses {@link tobj/2} underneath and because of that it cannot be
%%% used with {@link replace_vars/3}.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param NameTemplate template that will be matched to the result of applying
%%% erl_syntax:variable_name/1 to the target Erlang term.
%%% @return the corresponding "template tag"
%%% @see extract_vars/4
%%% @see tsvar_name_and_id/3
-spec tsvar_name(Ref :: reference(), NameTemplate :: term()) -> TemplateTag :: #tcontainer{}.
tsvar_name(Ref, Name) ->
    tobj(Ref,
	 [{{erl_syntax, type}, variable},
          {{erl_syntax, variable_name}, Name}]).


%%% @doc
%%% Creates a template that matches a variable in abstract syntax.
%%% It uses {@link tobj/2} underneath and because of that it cannot be
%%% used with {@link replace_vars/3}.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param NameTemplate template that will be matched to the result of applying
%%% erl_syntax:variable_name/1 to the target Erlang term.
%%% @param ValueTemplate template that will be matched to the target Erlang term.
%%% @return the corresponding "template tag"
%%% @see extract_vars/4
%%% @see tsvar_name/2
%%% @see tsvar_id/2
-spec tsvar_name_and_id(Ref :: reference(), NameTemplate :: term(),
			ValueTemplate :: term()) -> TemplateTag :: #tcontainer{}.
tsvar_name_and_id(Ref, Name, Value) ->
    tobj(Ref,
	 [{{erl_syntax, type}, variable},
          {{erl_syntax, variable_name}, Name},
	  {identity, Value}]).

%%% @doc
%%% Creates a template that matches a list with one element in abstract syntax.
%%% It uses {@link tobj/2} underneath and because of that it cannot be
%%% used with {@link replace_vars/3}.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param Template template that will be matched to the result of applying
%%% erl_syntax:list_head/1 of the target Erlang term.
%%% @return the corresponding "template tag"
%%% @see extract_vars/4
-spec tsoneellist_elid(Ref :: reference(), Template :: term()) -> TemplateTag :: #tcontainer{}.
tsoneellist_elid(Ref, Value) ->
    tobj(Ref,
	 [{{erl_syntax, list_length}, 1},
	  {{erl_syntax, list_head}, Value}]).

tcontainer(Ref, Content) ->
    #tcontainer{ref = Ref, content = Content}.

%%% @doc
%%% Creates a template tag with the name Id that matches any Erlang term.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param Id the name of the tag created.
%%% @return a "template tag" with the name Id that matches any Erlang term.
%%% @see extract_vars/4
%%% @see replace_vars/3
-spec tvar(Ref :: reference(), Id :: term()) -> TemplateTag :: #tcontainer{}.
tvar(Ref, Id) ->
    tcontainer(Ref, #tvar_rec{id = Id}).

%%% @doc
%%% Creates a template tag with no name that allows you to match
%%% the same Erlang term in several different ways. It allows you
%%% to incorporate apply calls in a template and match their results.
%%% Apply calls are assumed to take only one parameter, and the parameter
%%% that is used is the Erlang term matched. If the call produces an
%%% exception, this is considered a <code>no_match</code>.
%%% The special tuple <code>{identity, term()}</code> allows you to match
%%% the whole structure matched by the <code>tobj</code> "template tag"
%%% with another "template tag" or Erlang term, for example: a <code>tvar</code>
%%% tag to save the value if it matches.
%%% <code>tobj</code> cannot be used with {@link replace_vars/3}.
%%% @param Ref the reference used to create the tags in the template, it is used
%%% to reliably tell apart items that are template tags from those that are not,
%%% which allows the use of several kinds of templates in the same structure.
%%% @param FuncList A list with the templates and subtargets to match in the form:
%%% <ul>
%%% <li><code>{{Module, Function}, Template}</code> - Matches the result of
%%% calling the function Module:Function, with the target Erlang term as first
%%% argument, to the template Template</li>
%%% <li><code>{identity, Template}</code> - Matches the target Erlang term
%%% to the template Template</li>
%%% </ul>
%%% @return The template
%%% @see extract_vars/4
-spec tobj(Ref :: reference(),
	   [{identity, Anything :: term()} |
	    {{Module :: atom(), Function :: atom()}, Anything :: term()}])
	  -> TemplateTag :: #tcontainer{}.
tobj(Ref, FuncList) ->
    tcontainer(Ref, #tobj_rec{func_list = FuncList}).

