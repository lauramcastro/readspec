%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Simplified version of description
%%% @end
%%% Created : 30 Jan 2014 by Pablo Lamela

-module(clean_nestcond).

-export([clean_expansions/1, extract_varnames_from_exp/1]).
-include("records.hrl").

clean_expansions(Expansions) ->
    [clean_expansion(Expansion) || Expansion <- Expansions].

clean_expansion(Expansion) ->
    remove_ctxt_labels(
      remove_duplicated_conds(
	remove_unused_applies(
	  intuitive_renaming(Expansion)))).

intuitive_renaming(#expansion{applys = Applies} = Expansion) ->
    rename_all_to_best([Apply#apply.name
			|| Apply <- Applies, Apply#apply.is_call], Expansion).


find_best_renaming(Name, Applies) ->
    PossibleRenamings = find_possible_renamings(Name, Applies),
    case lists:sort(PossibleRenamings) of
	[] -> false;
	[{_, BestRenaming}|_] -> {true, BestRenaming}
    end.

find_possible_renamings(Name, Applies) ->
    [{length(atom_to_list(Apply#apply.name)), Apply#apply.name}
     || Apply <- Applies,
	not Apply#apply.is_call,
	not Apply#apply.is_arg,
	Apply#apply.evaluated,
	erl_syntax:type(Apply#apply.value) =:= variable,
	erl_syntax:variable_name(Apply#apply.value) =:= Name].

rename_all_to_best([], Expansion) -> Expansion;
rename_all_to_best([Name|Rest],
	   #expansion{applys = Applies} = Expansion) ->
    case (find_best_renaming(Name, Applies)) of
	false -> rename_all_to_best(Rest, Expansion);
	{true, BestRename} ->
	    begin
		NewExpansion = nestcond:remove_apply_var(Expansion, BestRename),
		rename_all_to_best(Rest, rename_variable(Name, BestRename, NewExpansion))
	    end
    end.

rename_variable(Name, BestRename, Expansion) ->
    nestcond:recontext(Expansion, renamers(Name, BestRename)).

%% Based on functions from see_logic ***
renamers(Ori, Fin) ->
    RenameFun = rename_fun(Ori, Fin),
    {RenameFun, fun (AST) -> see_logic:do_rename(AST, RenameFun) end}.

rename_fun(Ini, Fin) ->
    fun (NameAtom) ->
	    case (NameAtom =:= Ini) of
		false -> NameAtom;
		true -> Fin
	    end
    end.
%% End of based on functions from see_logic ***

remove_unused_applies(#expansion{applys = Applies} = Expansion) ->
    ImportantVars = extract_varnames_from_exp(Expansion),
    Expansion#expansion{
      applys = [Apply || Apply <- Applies,
	        lists:member(Apply#apply.name, ImportantVars)]
     }.

extract_varnames_from_exp(#expansion{conds = Conds, result = Res} = Expansion) ->
    Required = extract_varnames_from_conds(Conds)
	++ extract_varnames_from_ast(Res),
    extract_varnames_from_exp(Expansion, Required).
extract_varnames_from_exp(#expansion{applys = Applies} = Expansion, Variables) ->
    NewVariables = lists:usort(extract_varnames_from_applys(Applies, Variables) ++ Variables),
    case NewVariables =:= Variables of
	true -> Variables;
	false -> extract_varnames_from_exp(Expansion, NewVariables)
    end.
extract_varnames_from_conds(Conds) when is_list(Conds) ->
    lists:append([extract_varnames_from_cond(Cond) || Cond <- Conds]).
extract_varnames_from_cond(Cond) when is_tuple(Cond) ->
    lists:append([extract_varnames_from_ast(element(ElNum, Cond)) || ElNum <- lists:seq(2, size(Cond))]).
extract_varnames_from_ast(AST) -> extract_varnames_from_ast(AST, erl_syntax:type(AST)).
extract_varnames_from_ast(AST, variable) -> 
    [erl_syntax:variable_name(AST)];
extract_varnames_from_ast(AST, _) -> 
    lists:append([extract_varnames_from_ast(ST, erl_syntax:type(ST))
		  || ST <- lists:append(erl_syntax:subtrees(AST))]).
extract_varnames_from_applys([], Variables) -> Variables;
extract_varnames_from_applys([#apply{
				 name = Name
				} = Apply|Rest], Variables) ->
    case lists:member(Name, Variables) of
	true -> NewVariables = extract_varnames_from_apply(Apply),
		extract_varnames_from_applys(Rest, NewVariables ++ Variables);
	false -> extract_varnames_from_applys(Rest, Variables)
    end.

extract_varnames_from_apply(#apply{is_call = true, arg_list = Args}) ->
    lists:append([extract_varnames_from_ast(Arg) || Arg <- Args]);
extract_varnames_from_apply(#apply{evaluated = true, value = Value}) ->
    extract_varnames_from_ast(Value);
extract_varnames_from_apply(_) -> [].

remove_duplicated_conds(#expansion{conds = Conds} = Expansion) ->
    Expansion#expansion{conds = remove_duplicated_conds_no_sort(Conds)}.

remove_duplicated_conds_no_sort(List) ->
    lists:reverse(remove_duplicated_conds_no_sort(List, [], [])).

remove_duplicated_conds_no_sort([], _, List) -> List;
remove_duplicated_conds_no_sort([Cond|Rest], List, List2) ->
    FlattenedCond = cond_flatten(Cond),
    case lists:member(FlattenedCond, List) of
	true -> remove_duplicated_conds_no_sort(Rest, List, List2);
	false -> remove_duplicated_conds_no_sort(Rest, [FlattenedCond|List], [Cond|List2])
    end.

cond_flatten({Type, Var, AST}) ->
    {Type, erl_syntax:variable_name(Var), lists:flatten(erl_prettypr:format(AST))}.
    
remove_ctxt_labels(#expansion{applys = Applies} = Expansion) ->
    ListOfVars = [Apply#apply.name ||Apply <- Applies],
    ListOfVarsWithCtxt = lists:filter(fun has_ctxt/1, ListOfVars),
    ListOfVarsMinusCtxt = lists:map(fun rmv_ctxt/1, ListOfVarsWithCtxt),
    Combined = lists:zip(ListOfVarsWithCtxt, ListOfVarsMinusCtxt),
    Renames = [{VarWithCtxt, VarWithoutCtxt} || {VarWithCtxt, VarWithoutCtxt} <- Combined,
						not lists:member(VarWithoutCtxt, ListOfVars)],
    rename_all(Renames, Expansion).

has_ctxt(VarName) ->
    lists:member($%, atom_to_list(VarName)).

rmv_ctxt(VarName) ->
    list_to_atom(rmv_ctxt_aux(out, atom_to_list(VarName), [])).
rmv_ctxt_aux(_, [], List) -> lists:reverse(List);
rmv_ctxt_aux(out, [$%|Rest], List) -> rmv_ctxt_aux(in, Rest, List);
rmv_ctxt_aux(out, [Char|Rest], List) -> rmv_ctxt_aux(out, Rest, [Char|List]);
rmv_ctxt_aux(in, [$%|Rest], List) -> rmv_ctxt_aux(out, Rest, List);
rmv_ctxt_aux(in, [_|Rest], List) -> rmv_ctxt_aux(in, Rest, List).

rename_all([{Ori,Dest}|Rest], Expansion) ->
    rename_all(Rest, rename_variable(Ori, Dest, Expansion));
rename_all([], Expansion) -> Expansion.

    
