%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Adds information about idioms to the structure
%%% @end
%%% Created : 5 Feb 2014 by Pablo Lamela

-module(idiomizer).

-export([idiomize_module_info/1,
	 tsatom_val/2, tsint_val/2, tsvar_name/2, tsvar_name_and_id/3, tsvar_id/2,
	 tcontainer/2, tvar/2, tobj/2, extract_vars/4, replace_vars/3,
	 tsoneellist_elid/2]).

-include("records.hrl").
-include("template_recs.hrl").

%% General Function

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

tsatom_val(Ref, Value) ->
    tobj(Ref,
	[{{erl_syntax, type}, atom},
         {{erl_syntax, atom_value}, Value}]).

tsint_val(Ref, Value) ->
    tobj(Ref,
	 [{{erl_syntax, type}, integer},
	  {{erl_syntax, integer_value}, Value}]).

tsvar_id(Ref, Id) ->
    tobj(Ref,
	 [{{erl_syntax, type}, variable},
	  {identity, Id}]).

tsvar_name(Ref, Name) ->
    tobj(Ref,
	 [{{erl_syntax, type}, variable},
          {{erl_syntax, variable_name}, Name}]).


tsvar_name_and_id(Ref, Name, Value) ->
    tobj(Ref,
	 [{{erl_syntax, type}, variable},
          {{erl_syntax, variable_name}, Name},
	  {identity, Value}]).

tsoneellist_elid(Ref, Value) ->
    tobj(Ref,
	 [{{erl_syntax, list_length}, 1},
	  {{erl_syntax, list_head}, Value}]).

tcontainer(Ref, Content) ->
    #tcontainer{ref = Ref, content = Content}.

tvar(Ref, Id) ->
    tcontainer(Ref, #tvar_rec{id = Id}).

tobj(Ref, FuncList) ->
    tcontainer(Ref, #tobj_rec{func_list = FuncList}).

