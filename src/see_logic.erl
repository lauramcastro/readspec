%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Defines functions to extract possible
%%% executions of a function in terms of its arguments
%%% @end
%%% Created : 21 Jan 2014 by Pablo Lamela

-module(see_logic).

-export([test/0, get_funcs/1, get_record_definitions/1, generate_logical_function/3, expand_function/3,
	 renamers/2, test2/0, analyse/3, analyse_dbg/3, do_rename/2, parse_args/1]).

-import(nestcond, [make_expansion/0, set_result/2, add_arg_if_not_bound/2,
		   set_func_patmatcha/2, set_case_patmatcha/2, clear_patmatcha/1,
		   add_cond/2, add_var_value/3, has_value/2, is_bounded/2,
		   variable_type/2, move_result_to_case_patmatcha/1,
		   add_apply/5, touch_index/2, touch_index_n_times/3,
		   get_result/1, get_context/1, set_context/2, new_context/1,
		   recontext/2, add_record_definitions/2, get_record_definition/2,
		   get_var_value/2, remove_apply_vars/2, get_var_values/2,
		   remove_apply_var/2]).
-include("records.hrl").


analyse(File, Function, Args) ->
    nestcond:ppr_expansions(
      clean_nestcond:clean_expansions(
	generate_logical_function({Function, length(Args)},
				  parse_args(Args),
				  File))).

analyse_dbg(File, Function, Args) ->
    nestcond:ppr_expansions(generate_logical_function({Function, length(Args)},
						      parse_args(Args),
						      File)).

parse_args([]) -> [];
parse_args([Arg|Rest]) ->
    [begin
	 case erl_scan:string(Arg ++ ".") of
	     {ok, Tokens, _} ->
		 case erl_parse:parse_exprs(Tokens) of
		     {ok, [Parse]} -> Parse;
		     Result -> throw({problem_parsing, Result})
		 end;
	     Error -> throw({problem_extracting_tokens, Error, Arg})
	 end
     end|parse_args(Rest)].

test() ->
    generate_logical_function({register_post, 3},
			      [erl_syntax:variable('Arg_1'),
			       erl_syntax:variable('Arg_2'),
%			       erl_syntax:list([erl_syntax:variable('Arg_2_1'),
%						erl_syntax:variable('Arg_2_2')]),
			       erl_syntax:variable('Arg_3')],
			      '../registry_eqc.erl').

test2() ->
    generate_logical_function({test, 2},
			      [erl_syntax:variable('Arg_1'),
			       erl_syntax:variable('Arg_2')],
			      '../registry_eqc.erl').

get_funcs(FileName) ->
    {ok, Parse} = epp:parse_file(FileName, [], []),
    [{{erl_syntax:atom_value(erl_syntax:function_name(Function)),
       erl_syntax:function_arity(Function)},
      erl_syntax:function_clauses(Function)} ||
	Function <- Parse, function =:= erl_syntax:type(Function)].

get_record_definitions(FileName) ->
    {ok, Parse} = epp:parse_file(FileName, [], []),
    [{erl_syntax:atom_value(Name),
      [{erl_syntax:atom_value(erl_syntax:record_field_name(Field)),
	case erl_syntax:record_field_value(Field) of
	    none -> erl_syntax:atom(undefined);
	    Other -> Other
	end} || Field <- erl_syntax:tuple_elements(Fields)]}
     || El <- Parse, erl_syntax:type(El) =:= attribute,
	erl_syntax:atom_value(erl_syntax:attribute_name(El)) =:= 'record',
	[Name, Fields] <- [erl_syntax:attribute_arguments(El)]].

generate_logical_function(RootFuncName, Args, FileName) ->
    Expansion = add_record_definitions(make_expansion(), get_record_definitions(FileName)),
    IndexedSyntaxTree = get_funcs(FileName),
    generate_logical_function(RootFuncName, Args, IndexedSyntaxTree, Expansion).
generate_logical_function(RootFuncName, Args, IndexedSyntaxTree, Expansion) ->
    try expand_list_with_res(Args, [Expansion], IndexedSyntaxTree) of
	PlainResult ->
	    begin
		Result = [set_func_patmatcha(Res, EArgs) || {EArgs, Res} <- PlainResult],
		case lists:keyfind(RootFuncName, 1, IndexedSyntaxTree) of
		    false -> {not_expandable, {function_not_found, RootFuncName}};
		    {_, Function} ->
			try expand_function(Function,
					    Result,
					    IndexedSyntaxTree) of
			    FinalResult -> lists:map(fun nestcond:clear_patmatcha/1, FinalResult)
			catch
			    exit:{not_expandable, Reason} -> {not_expandable, Reason}
			end
		end
	    end
    catch
	exit:{not_expandable, Reason} -> {not_expandable, Reason}
    end.

fold_through_args([Arg1|Rest], Expansions, IndexedSyntaxTree) ->
    NewExpansions = expand_function(Arg1, Expansions, IndexedSyntaxTree),
    fold_through_args(Rest, NewExpansions, IndexedSyntaxTree);
fold_through_args([], Expansions, _IndexedSyntaxTree) when is_list(Expansions) ->
    Expansions;
fold_through_args([], Expansion, _) ->
    [Expansion].

expand_function(Func, Expansions, IndexedSyntaxTree)
  when is_list(Expansions) ->
    case (simplify_not_exp(Expansions)) of
	{not_expandable, Reason} -> exit({not_expandable, Reason});
	_ ->
	    lists:append(
	      [expand_function(Func, Exp, IndexedSyntaxTree)
	       || Exp <- Expansions])
    end;
expand_function(List, #expansion{pat_matcha_type = none} = Expansion, IndexedSyntaxTree)
  when is_list(List) ->
    fold_through_args(List, Expansion, IndexedSyntaxTree);
expand_function(List, Expansion, IndexedSyntaxTree)
  when is_list(List) ->
    lists:append([expand_function(Func, Expansion, IndexedSyntaxTree)
		  || Func <- List]);
expand_function(Func, Expansion, IndexedSyntaxTree) ->
    expand_type(erl_syntax:type(Func), Func, Expansion, IndexedSyntaxTree).

expand_type(atom, Func, #expansion{} = Expansion, _IndexedSyntaxTree) ->
    [set_result(Expansion, Func)];
expand_type(char, Func, #expansion{} = Expansion, _IndexedSyntaxTree) ->
    [set_result(Expansion, Func)];
expand_type(float, Func, #expansion{} = Expansion, _IndexedSyntaxTree) ->
    [set_result(Expansion, Func)];
expand_type(integer, Func, #expansion{} = Expansion, _IndexedSyntaxTree) ->
    [set_result(Expansion, Func)];
expand_type(nil, Func, #expansion{} = Expansion, _IndexedSyntaxTree) ->
    [set_result(Expansion, Func)];
expand_type(string, Func, #expansion{} = Expansion, _IndexedSyntaxTree) ->
    [set_result(Expansion, Func)];
expand_type(infix_expr, Eq, #expansion{} = Expansion, IndexedSyntaxTree) ->
    case ({erl_syntax:infix_expr_left(Eq),
	   erl_syntax:atom_value(erl_syntax:infix_expr_operator(Eq)),
	   erl_syntax:infix_expr_right(Eq)}) of
	{L, '=:=', R} -> expand_equality(L, R, Expansion, IndexedSyntaxTree);
	{L, '==', R} -> expand_equality(L, R, Expansion, IndexedSyntaxTree);
	{L, '=/=', R} -> expand_inequality(L, R, Expansion, IndexedSyntaxTree);
	{L, '/=', R} -> expand_inequality(L, R, Expansion, IndexedSyntaxTree);
	{L, 'and', R} -> expand_conjunction(L, R, Expansion, IndexedSyntaxTree);
	{L, 'andalso', R} -> expand_conjunction(L, R, Expansion, IndexedSyntaxTree);
	{L, 'or', R} -> expand_disjunction(L, R, Expansion, IndexedSyntaxTree);
	{L, 'orelse', R} -> expand_disjunction(L, R, Expansion, IndexedSyntaxTree);
	{L, 'xor', R} -> expand_exclusive_disjunction(L, R, Expansion, IndexedSyntaxTree);
	{L, '++', R} -> translate_as_function(lists, append, [L, R], Expansion, IndexedSyntaxTree);
	{L, '--', R} -> translate_as_function(lists, substract, [L, R], Expansion, IndexedSyntaxTree);
	{L, Op, R} -> translate_as_function('?OPERATOR', Op, [L, R], Expansion, IndexedSyntaxTree)
%	{_, Op, _} -> exit({not_expandable, {not_implemented_infix, Op}})
    end;
expand_type(prefix_expr, PE, #expansion{} = Expansion, IndexedSyntaxTree) ->
    case ({erl_syntax:atom_value(erl_syntax:prefix_expr_operator(PE)), erl_syntax:prefix_expr_argument(PE)}) of
	{'not', A} -> expand_negation(A, Expansion, IndexedSyntaxTree);
	{Op, _} -> exit({not_expandable, {not_implemented_prefix, Op}})
    end;
expand_type(variable, Func, #expansion{} = Expansion, _IndexedSyntaxTree) ->
%   io:format("VARIABLE EXPAND: ~p~n", [{erl_syntax:variable_name(Func), Expansion, erl_prettypr:format(Func)}]),
    VarName = erl_syntax:variable_name(Func),
    case variable_type(Expansion, VarName) of
	{valued, V} -> [set_result(Expansion, V)];
	non_valued -> [set_result(Expansion, Func)];
	unknown -> [set_result(add_arg_if_not_bound(Expansion, VarName), Func)]
    end;
expand_type(case_expr, Func, #expansion{} = Expansion, IndexedSyntaxTree) ->
    NewExpansions = expand_function(erl_syntax:case_expr_argument(Func), Expansion, IndexedSyntaxTree),
    PlacedExpansions = move_result_to_case_patmatcha_to_all(NewExpansions),
    expand_function(erl_syntax:case_expr_clauses(Func), PlacedExpansions, IndexedSyntaxTree);
expand_type(application, App, #expansion{} = Expansion, IndexedSyntaxTree) ->
    Qualifier = erl_syntax:application_operator(App),
    {Module, Function} = get_function_details(Qualifier),
    Args = erl_syntax:application_arguments(App),
    Arity = length(Args),
    case try_to_expand(Module, Function, Args, Arity, Expansion, IndexedSyntaxTree) of
	fail ->
	    begin
		try expand_list_with_res(Args, [Expansion], IndexedSyntaxTree) of
		    PlainResult ->  [expand_application_aux(Module, Function, EArgs, Arity, Res)
				     || {EArgs, Res} <- PlainResult]
		catch
		    exit:{not_expandable, _} -> [expand_application_aux(Module, Function, Args, Arity, Expansion)]
		end
	    end;
	Expansions when is_list(Expansions) -> Expansions;
	Expansion -> [Expansion]
    end;
expand_type(clause, Func, #expansion{pat_matcha_type = func,
				     pat_matcha = ArgMatchas} = Expansion, IndexedSyntaxTree) ->
    % Match each arg, sequentially, no need to simplify right hand side since we assume
    % they come simplified when called to function application.
    expand_through_clauses(Func, ArgMatchas, [Expansion], IndexedSyntaxTree);
expand_type(clause, Func, #expansion{pat_matcha_type = 'case',
				     pat_matcha = ArgMatchas} = Expansion, IndexedSyntaxTree) ->
    % Match each arg, sequentially, no need to simplify right hand side since we assume
    % it comes simplified from the case.
    expand_through_clauses(Func, ArgMatchas, [Expansion], IndexedSyntaxTree);
expand_type(list, Func, Expansion, IndexedSyntaxTree) ->
    leave_level_as_is(Func, Expansion, IndexedSyntaxTree);
expand_type(tuple, Func, Expansion, IndexedSyntaxTree) ->
    leave_level_as_is(Func, Expansion, IndexedSyntaxTree);
expand_type(record_field, Func, Expansion, IndexedSyntaxTree) ->
    leave_level_as_is(Func, Expansion, IndexedSyntaxTree);
expand_type(record_access, Func, OldExpansion, IndexedSyntaxTree) ->
    % Translate into pattern matching to a temporary variable
    % Get the temporary variable, set as result
    {[AuxVarName], Expansion} = get_aux_vars(1, OldExpansion),
    Argument = erl_syntax:record_access_argument(Func),
    Type = erl_syntax:record_access_type(Func),
    FieldName = erl_syntax:record_access_field(Func),
    WildcardField = erl_syntax:record_field(FieldName,
					    erl_syntax:variable(AuxVarName)),
    WildcardRecord = erl_syntax:record_expr(Type, [WildcardField]),
    NewExpansions = expand_function(Argument, Expansion, IndexedSyntaxTree),
    lists:append(
      [begin
	   New2Expansions = pattern_match(WildcardRecord, get_result(NewExpansion), [NewExpansion]),
	   [begin
		set_result(remove_apply_var(New2Expansion, AuxVarName),
			   get_var_value(New2Expansion, AuxVarName))
	    end
	    || New2Expansion <- New2Expansions ]
       end
       || NewExpansion <- NewExpansions]);
expand_type(record_expr, Func, Expansion, IndexedSyntaxTree) ->
    Fields = erl_syntax:record_expr_fields(Func),
    Type = erl_syntax:atom_value(erl_syntax:record_expr_type(Func)),
    FieldPairs = extract_fields_as_pair_list(Fields),
    Definitions = get_record_definition(Type, Expansion),
    {FieldNames, _} = lists:unzip(Definitions),
        % Has argument?
    case erl_syntax:record_expr_argument(Func) of
	none -> % If not
	    begin
            % Find missing fields
		Missing = diff_using_first_el(Definitions, FieldPairs),
            % Complete using template
		CompletedRecord = remake_record(Type, Missing ++ FieldPairs),
	    % Eval Rest
		leave_level_as_is(CompletedRecord, Expansion, IndexedSyntaxTree)
	    end;
	NotExpandedArg -> % Else
	    begin
	    % Eval Argument
		NewExpansions = expand_function(NotExpandedArg, Expansion, IndexedSyntaxTree),
		lists:append(
		  [begin
		       Argument = get_result(OneExpansion),
            % Match arg against field
		       ExpansionsWithArgFields = match_fields_record(Type, FieldNames, Argument, Expansion),
			% ExpansionWithArgFields has the structure: [{SubOneExpansion, [{FieldName, FieldValue}]}]
                        % where FieldName is the actual field name and FieldValue is the
                        % expanded value of the field "FieldName" in the record "Argument" of type "Type"
		       lists:append(
			 [begin      
            % Find missing fields
			      ArgPairs = [{FieldName, FieldValue}
					  || {FieldName, FieldValue} <- FieldValues],
			      Missing = diff_using_first_el(ArgPairs, FieldPairs),
            % Compose record
			      RecordWithoutArg = remake_record(Type, Missing ++ FieldPairs),
	    % Eval Rest
			      leave_level_as_is(RecordWithoutArg, SubOneExpansion, IndexedSyntaxTree)
			  end || {SubOneExpansion, FieldValues} <- ExpansionsWithArgFields])
		   end || OneExpansion <- NewExpansions])
	    end
    end;
expand_type(match_expr, Func, Expansion, IndexedSyntaxTree) ->
    Pattern = erl_syntax:match_expr_pattern(Func),
    Body = erl_syntax:match_expr_body(Func),
    NewExpansions = expand_type(erl_syntax:type(Body), Body, Expansion, IndexedSyntaxTree),
    lists:append([begin
		      Result = get_result(NewExpansion),
		      NewNewExpansions = pattern_match(Pattern, Result, [NewExpansion]),
		      set_result_for_all(NewNewExpansions, Result)
		  end
		  || NewExpansion <- NewExpansions]);
expand_type(conjunction, Func, Expansion, IndexedSyntaxTree) ->
    case erl_syntax:conjunction_body(Func) of
	[H|T] -> expand_type(infix_expr, erl_syntax:infix_expr(H, erl_syntax:operator('andalso'),
							       erl_syntax:conjunction(T)),
			     Expansion, IndexedSyntaxTree);
	[] -> [set_result(Expansion, erl_syntax:atom(true))]
    end;
expand_type(disjunction, Func, Expansion, IndexedSyntaxTree) ->
    case erl_syntax:conjunction_body(Func) of
	[H|T] -> expand_type(infix_expr, erl_syntax:infix_expr(H, erl_syntax:operator('orelse'),
							       erl_syntax:disjunction(T)),
			     Expansion, IndexedSyntaxTree);
	[] -> [set_result(Expansion, erl_syntax:atom(false))]
    end;
%expand_type(_, Func, Expansion, IndexedSyntaxTree) ->
%    leave_level_as_is(Func, Expansion, IndexedSyntaxTree); % Already returns a list of expansions
expand_type(Type, _Func, #expansion{} = _Expansion, _IndexedSyntaxTree) ->
    exit({not_expandable, {not_implemented_type, Type}}).

extract_fields_as_pair_list(FieldList) ->
    [{erl_syntax:atom_value(erl_syntax:record_field_name(Field)),
      erl_syntax:record_field_value(Field)} || Field <- FieldList].

diff_using_first_el(TupleList, []) -> TupleList;
diff_using_first_el(TupleList, [TupleHead|TupleTail]) ->
    NewTupleList = lists:keydelete(erlang:element(1, TupleHead), 1, TupleList),
    diff_using_first_el(NewTupleList, TupleTail).

remake_record(Type, ListOfPairs) ->
    erl_syntax:record_expr(erl_syntax:atom(Type),
			   [erl_syntax:record_field(erl_syntax:atom(FieldName), FieldValue)
			    || {FieldName, FieldValue} <- ListOfPairs]).

% match_fields_record returns a structure like: [{SubOneExpansion, [{FieldName, FieldValue}]}]
% where FieldName is the actual field name and FieldValue is the
% expanded value of the field "FieldName" in the record "Argument" of type "Type"
match_fields_record(Type, FieldNames, Argument, OldExpansion) ->
    {AuxVarNames, Expansion} = get_aux_vars(length(FieldNames), OldExpansion),
    FieldAndVarNames = lists:zip(FieldNames, AuxVarNames),
    MatchingRecord = erl_syntax:record_expr(
		       erl_syntax:atom(Type),
		       [erl_syntax:record_field(
			  erl_syntax:atom(FieldName), erl_syntax:variable(VarName))
			|| {FieldName, VarName} <- FieldAndVarNames]
		      ),
    NewExpansions = pattern_match(MatchingRecord, Argument, [Expansion]),
    [begin
	 {remove_apply_vars(NewExpansion, AuxVarNames),
	  lists:zip(FieldNames, get_var_values(NewExpansion, AuxVarNames))}
     end
     || NewExpansion <- NewExpansions].

get_aux_vars(Number, OldExpansion) ->
    {AuxVarNumbers, Expansion} = touch_index_n_times(OldExpansion, aux_vars, Number),
    AuxVarNames = [list_to_atom("__AuxVar-" ++ integer_to_list(AuxVarNumber) ++ "__")
		   || AuxVarNumber <- AuxVarNumbers],
    {AuxVarNames, Expansion}.

leave_level_as_is(Func, Expansion, IndexedSyntaxTree) ->
    ListOfLists = erl_syntax:subtrees(Func),
    {List, Nums} = to_one_list(ListOfLists),
    [set_result(Res, erl_syntax:update_tree(Func, remount_list(Nums, EList)))
     || {EList, Res} <- expand_list_with_res(List, [Expansion], IndexedSyntaxTree)].

to_one_list(ListOfLists) ->
    {lists:append(ListOfLists), lists:map(fun erlang:length/1, ListOfLists)}.

remount_list([], []) -> [];
remount_list([Num|RestN], List) ->
    {FirstPiece, SecondPiece} = lists:split(Num, List),
    [FirstPiece|remount_list(RestN, SecondPiece)].

translate_as_function(ModuleName, FunctionName, Arguments, Expansion, IndexedSyntaxTree) ->
    expand_function(erl_syntax:application(
		      erl_syntax:module_qualifier(
			erl_syntax:atom(ModuleName),
			erl_syntax:atom(FunctionName)
		       ),
		      Arguments),
		    Expansion, IndexedSyntaxTree).

expand_list_with_res(List, Exps, IndexedSyntaxTree) ->
    expand_list_with_res_aux(List, [{[], Exp} || Exp <- Exps], [], IndexedSyntaxTree).
expand_list_with_res_aux([H|_] = List, [{ExpList, Exp}|Rest], DestList, IndexedSyntaxTree) ->
    %% io:format("~p~n", [[List, [{ExpList, Exp}|Rest], DestList]]),
    NewExps = expand_function(H, Exp, IndexedSyntaxTree),
    Res = [{ExpList ++ [get_result(NewExp)], NewExp} || NewExp <- NewExps],
    expand_list_with_res_aux(List, Rest, DestList ++ Res, IndexedSyntaxTree);
expand_list_with_res_aux([_|Tail], [], DestList, IndexedSyntaxTree) ->
    expand_list_with_res_aux(Tail, DestList, [], IndexedSyntaxTree);
expand_list_with_res_aux([], DestList, [], _IndexedSyntaxTree) ->
    DestList.

simplify_not_exp(List) ->
    case lists:dropwhile(fun (Elem) ->
				 (catch element(1, Elem)) =/= not_expandable
			 end,
			 List) of
	[] -> List;
	[NE|_] -> NE
    end.

expand_application_aux(Module, Function, Args, Arity, Expansion) ->
    {CallVarName, NewExpansion} = generate_call_var_name(Expansion, Module, Function, Arity),
    AppExpansion = add_apply(NewExpansion, CallVarName, Module, Function, Args),
    set_result(AppExpansion, erl_syntax:variable(CallVarName)).

expand_quality(L, R, Expansion, IndexedSyntaxTree, Res1, Res2) ->
    LExps = expand_function(L, [Expansion], IndexedSyntaxTree),
    lists:append([expand_quality_phase2(R, LExp, IndexedSyntaxTree, Res2) || LExp <- LExps])
	++ [set_result(Expansion, erl_syntax:atom(Res1))].
expand_quality_phase2(R, LExp, IndexedSyntaxTree, Res2) ->
    LRes = get_result(LExp),
    RExps = expand_function(R, [LExp], IndexedSyntaxTree),
    lists:append([expand_quality_phase3(LRes, RExp, Res2) || RExp <- RExps]).
expand_quality_phase3(LRes, RExp, Res2) ->    
    RRes = get_result(RExp),
    set_result_for_all(pattern_match(LRes, RRes, [RExp]), erl_syntax:atom(Res2)).
expand_equality(L, R, Expansion, IndexedSyntaxTree) ->
    expand_quality(L, R, Expansion, IndexedSyntaxTree, false, true).
expand_inequality(L, R, Expansion, IndexedSyntaxTree) ->
    expand_quality(L, R, Expansion, IndexedSyntaxTree, true, false).
expand_conjunction(L, R, Expansion, IndexedSyntaxTree) ->
    expand_boolean_expression(L, R, Expansion, IndexedSyntaxTree, true, true, true)
	++ [set_result(Expansion, erl_syntax:atom(false))].
expand_disjunction(L, R, Expansion, IndexedSyntaxTree) ->
    expand_boolean_expression(L, R, Expansion, IndexedSyntaxTree, true, true, true)
	++ expand_boolean_expression(L, R, Expansion, IndexedSyntaxTree, false, true, true)
	++ expand_boolean_expression(L, R, Expansion, IndexedSyntaxTree, true, false, true)
	++ [set_result(Expansion, erl_syntax:atom(false))].
expand_exclusive_disjunction(L, R, Expansion, IndexedSyntaxTree) ->
    expand_boolean_expression(L, R, Expansion, IndexedSyntaxTree, false, true, true)
	++ expand_boolean_expression(L, R, Expansion, IndexedSyntaxTree, true, false, true)
	++ [set_result(Expansion, erl_syntax:atom(false))].

expand_boolean_expression(L, R, Expansion, IndexedSyntaxTree, B1, B2, BRes) ->
    LExps = expand_function(L, [Expansion], IndexedSyntaxTree),
    FLExps = lists:append([pattern_match(erl_syntax:atom(B1), get_result(LExp), [LExp]) || LExp <- LExps]),
    RExps = expand_function(R, FLExps, IndexedSyntaxTree),
    FinalExps = lists:append([pattern_match(erl_syntax:atom(B2), get_result(RExp), [RExp]) || RExp <- RExps]),
    set_result_for_all(FinalExps, erl_syntax:atom(BRes)).

expand_negation(A, Expansion, IndexedSyntaxTree) ->
    NExps = expand_function(A, [Expansion], IndexedSyntaxTree),
    TrFa = lists:append([pattern_match(erl_syntax:atom(true), get_result(NExp), [NExp]) || NExp <- NExps]),
    FaTr = lists:append([pattern_match(erl_syntax:atom(false), get_result(NExp), [NExp]) || NExp <- NExps]),
    set_result_for_all(TrFa, erl_syntax:atom(false)) ++ set_result_for_all(FaTr, erl_syntax:atom(true)).

try_to_expand('?MODULE', Function, Args, Arity, Expansion, IndexedSyntaxTree) ->
    OldContext = get_context(Expansion),
    RenamedArgs = args_to_context(Args, OldContext),
    RenamedExpansion = new_context(save_thiscontext(Expansion)),
    case generate_logical_function({Function, Arity}, RenamedArgs, IndexedSyntaxTree, RenamedExpansion) of
	{not_expandable, _} -> fail;
	InExpansions -> [load_thiscontext(set_context(save_thiscontext(InExpansion), OldContext))
			 || InExpansion <- InExpansions]
    end;
try_to_expand(_, _, _, _, _, _) -> fail.



%% fold_through_simplified_matchas(List, Expansions) ->
%%     lists:map(fun (Expansion) ->
%% 		      fold_through_simplified_matcha(List, Expansion)
%% 	      end, Expansions).

fold_through_simplified_matchas([], Expansions) -> Expansions;
fold_through_simplified_matchas([{Pattern, Matcha}|Rest], Expansions) ->
    NewExpansions = pattern_match(Pattern, Matcha, Expansions),
    fold_through_simplified_matchas(Rest, NewExpansions).

expand_through_clauses(Func, ArgMatchas, Expansions, IndexedSyntaxTree) ->
    ArgPatterns = erl_syntax:clause_patterns(Func),
    ArgPairs = lists:zip(ArgPatterns, ArgMatchas),
    PatExpansions = fold_through_simplified_matchas(ArgPairs, Expansions),
    FilteredExpansions = filter_by_guards(PatExpansions, erl_syntax:clause_guard(Func), IndexedSyntaxTree),
    CleanExpansions = [clear_patmatcha(FilteredExpansion) || FilteredExpansion <- FilteredExpansions],
    expand_function(erl_syntax:clause_body(Func), CleanExpansions, IndexedSyntaxTree).

%% TODO: add possibilities
pattern_match(Pattern, Matcha, Expansions) ->
    pattern_match(erl_syntax:type(Pattern), Pattern, erl_syntax:type(Matcha), Matcha, Expansions).
pattern_match(underscore, _, _, _, Expansions) ->
    Expansions;
pattern_match(match_expr, MatchExpr, _, Sth, Expansions) ->
    Pattern = erl_syntax:match_expr_pattern(MatchExpr),
    Body = erl_syntax:match_expr_body(MatchExpr),
    pattern_match(Body, Sth,
		  pattern_match(Pattern, Sth, Expansions));
pattern_match(variable, Variable, _, Sth, Expansions) ->
    match_left_variable_for_all(Variable, Sth, Expansions);
pattern_match(_, Sth, variable, Var, Expansions) ->
    match_right_variable_for_all(Sth, Var, Expansions);
pattern_match(atom, Atom1, atom, Atom2, Expansions) ->
    case ({erl_syntax:atom_value(Atom1), erl_syntax:atom_value(Atom2)}) of
	{Atom, Atom} -> Expansions;
	_ -> []
    end;
pattern_match(atom, _, _, _, _) -> [];
pattern_match(_, _, atom, _, _) -> [];
pattern_match(string, String1, Type2, Sth2, Expansions) ->
    pattern_match(list, convert_to_list(String1), Type2, Sth2, Expansions);
pattern_match(Type1, Sth1, string, String2, Expansions) ->
    pattern_match(Type1, Sth1, list, convert_to_list(String2), Expansions);
pattern_match(nil, _, nil, _, Expansions) -> Expansions;
pattern_match(nil, _, _, _, _) -> [];
pattern_match(_, _, nil, _, _) -> [];
pattern_match(list, List1, list, List2, Expansions) ->
    HeadExpansion = pattern_match(erl_syntax:list_head(List1),
				  erl_syntax:list_head(List2),
				  Expansions),
    pattern_match(erl_syntax:list_tail(List1),
		  erl_syntax:list_tail(List2),
		  HeadExpansion);
pattern_match(list, _, _, _, _) -> [];
pattern_match(_, _, list, _, _) -> [];
pattern_match(tuple, Tuple1, tuple, Tuple2, Expansions) ->
    case {erl_syntax:tuple_size(Tuple1),
	  erl_syntax:tuple_size(Tuple2),
	  erl_syntax:tuple_elements(Tuple1), erl_syntax:tuple_elements(Tuple2)} of
	{Same, Same, [First1|Rest1], [First2|Rest2]} ->
	    begin
		FirstExpansions = pattern_match(First1, First2, Expansions),
		pattern_match(erl_syntax:tuple(Rest1),
			      erl_syntax:tuple(Rest2),
			      FirstExpansions)
	    end;
	{0, 0, [], []} -> Expansions;
	_ -> []
    end;
pattern_match(tuple, _, _, _, _) -> [];
pattern_match(_, _, tuple, _, _) -> [];
pattern_match(record_expr, Record1, record_expr, Record2, Expansion) ->
%%     record_expr_argument
%%     record_expr_fields
%%     record_expr_type
    none = erl_syntax:record_expr_argument(Record1),
    none = erl_syntax:record_expr_argument(Record2),
    Fields2 = erl_syntax:record_expr_fields(Record2),
    case {erl_syntax:atom_value(erl_syntax:record_expr_type(Record1)),
	  erl_syntax:atom_value(erl_syntax:record_expr_type(Record2)),
	  erl_syntax:record_expr_fields(Record1)} of
	{T, T, [FH1|FT1]} ->
	    begin
		FirstExpansions =
		    pattern_match(erl_syntax:record_field_value(FH1),
				  erl_syntax:record_field_value(
					 hd([FH2 || FH2 <- Fields2,
						    erl_syntax:atom_value(erl_syntax:record_field_name(FH1)) =:=
							erl_syntax:atom_value(erl_syntax:record_field_name(FH2))])),
				  Expansion),
		pattern_match(erl_syntax:record_expr(erl_syntax:atom(T), FT1),
			      Record2, FirstExpansions)
	    end;
	{T, T, []} -> Expansion;
	{_, _, _} -> []
    end;
pattern_match(record_expr, _, _, _, _) -> [];
pattern_match(_, _, record_expr, _, _) -> [];
pattern_match(integer, Integer1, integer, Integer2, Expansions) ->
    case ({erl_syntax:integer_value(Integer1), erl_syntax:integer_value(Integer2)}) of
	{Integer1, Integer2} -> Expansions;
	_ -> []
    end;
pattern_match(integer, _, _, _, _) -> [];
pattern_match(_, _, integer, _, _) -> [];
pattern_match(Type1, _Pattern, Type2, _Matcha, _Expansions) ->
    exit({not_expandable, {pattern_matching_not_implemented_for, Type1, Type2}}).
%    add_cond_to_all(Expansions, {matches, erl_prettypr:format(Pattern), erl_prettypr:format(Matcha)}).


match_left_variable(Variable, ASth, OriExpansion) ->
    VarName = erl_syntax:variable_name(Variable),
    {Sth, Expansion} = % Patch to find the value of Sth if it is a variable too
	case erl_syntax:type(ASth) of
	    variable -> case variable_type(OriExpansion, erl_syntax:variable_name(ASth)) of
			    {valued, V} -> {V, OriExpansion};
			    non_valued -> {ASth, OriExpansion};
			    unknown -> {ASth, add_arg_if_not_bound(
						OriExpansion,
						erl_syntax:variable_name(ASth))}
			end;
	    _ -> {ASth, OriExpansion}
	end,
    case variable_type(Expansion, VarName) of
	{valued, Value} -> pattern_match(Value, Sth, [Expansion]);
	non_valued ->
	    case (catch erl_syntax:variable_name(Sth)) of
		VarName -> [Expansion];
		_ ->
		    [add_cond(Expansion, {equals, Variable,
					  Sth})]
	    end;
	unknown -> [add_var_value(Expansion, erl_syntax:variable_name(Variable), Sth)]
    end.
match_right_variable(Sth, Var, Expansion) ->
    VarName = erl_syntax:variable_name(Var),
    case variable_type(Expansion, VarName) of
	{valued, Value} -> pattern_match(Value, Sth, [Expansion]);
	non_valued -> [write_in_terms_of(Sth, Var, Expansion)];
	    %% [add_cond(Expansion, {equals, Var,
	    %% 			  Sth})]; % To do: set in terms of...
	unknown -> NewExpansion = add_arg_if_not_bound(Expansion, VarName),
		   [write_in_terms_of(Sth, Var, NewExpansion)]
    end.

move_result_to_case_patmatcha_to_all(Expansions) ->
    [move_result_to_case_patmatcha(Expansion) || Expansion <- Expansions].
match_left_variable_for_all(Var, Sth, Expansions) ->
    lists:append([match_left_variable(Var, Sth, Expansion) || Expansion <- Expansions]).
match_right_variable_for_all(Sth, Var, Expansions) ->
    lists:append([match_right_variable(Sth, Var, Expansion) || Expansion <- Expansions]).

set_result_for_all(Expansions, Result) ->
    lists:append([[set_result(Expansion, Result)] || Expansion <- Expansions]).

%% add_var_value_to_all(Expansions, VarName, VarValue) ->
%%     [add_var_value(Expansion, VarName, VarValue) || Expansion <- Expansions].

%% add_cond_to_all(Expansions, Cond) ->
%%     [add_cond(Expansion, Cond) || Expansion <- Expansions].

%% TODO: actually filter
filter_by_guards(Expansions, Guards, IndexedSyntaxTree) ->
    case Guards of
	none -> Expansions;
	_ -> NewExpansions = expand_function(Guards, Expansions, IndexedSyntaxTree),
	     lists:append([pattern_match(erl_syntax:atom(true), get_result(NewExpansion),
					 [NewExpansion])
			   || NewExpansion <- NewExpansions])
    end.

convert_to_list(StringAST) ->
    ListPpr = lists:flatten(io_lib:format("~p.", [[atom|erl_syntax:string_value(StringAST)]])),
    {ok, Tokens, _} = erl_scan:string(ListPpr),
    {ok, [ListAST]} = erl_parse:parse_exprs(Tokens),
    erl_syntax:list_tail(ListAST).

write_in_terms_of(Sth, Var, Expansion) ->
    write_in_terms_of(erl_syntax:type(Sth), Sth, Var, Expansion).

write_in_terms_of(atom, Atom, Var, Expansion) -> grant_equal(Atom, Var, Expansion);
write_in_terms_of(integer, Integer, Var, Expansion) -> grant_equal(Integer, Var, Expansion);
write_in_terms_of(string, String, Var, Expansion) -> grant_equal(String, Var, Expansion);
write_in_terms_of(float, Float, Var, Expansion) -> grant_equal(Float, Var, Expansion);
write_in_terms_of(tuple, Tuple, Var, Expansion) ->
    TupleElems = erl_syntax:tuple_elements(Tuple),
    Indexes = lists:seq(1, length(TupleElems)),
    NamesVar = [list_to_atom("tup_elem_" ++ integer_to_list(NumEl)
			     ++ "_of__:" ++ atom_to_list(erl_syntax:variable_name(Var)))
		|| NumEl <- Indexes],
    Applies = [{erlang, element, [erl_syntax:integer(NumEl), Var]} || NumEl <- Indexes],
    ListElemVar = lists:zip3(TupleElems, NamesVar, Applies),
    NewExpansion = add_cond(Expansion, {tuple_size, Var, erl_syntax:integer(length(TupleElems))}),
    fold_write_in_terms_through_args(ListElemVar, NewExpansion);
write_in_terms_of(list, List, Var, Expansion) ->
    ListElems = list_elements(list, List),
    Indexes = lists:seq(1, length(ListElems)),
    NamesVar = [list_to_atom("lis_elem_" ++ integer_to_list(NumEl)
			     ++ "_of__:" ++ atom_to_list(erl_syntax:variable_name(Var)))
		|| NumEl <- Indexes],
    Applies = [{lists, nth, [erl_syntax:integer(NumEl), Var]} || NumEl <- Indexes],
    ListElemVar = lists:zip3(ListElems, NamesVar, Applies),
    NewExpansion = add_cond(Expansion, {list_size, Var, erl_syntax:integer(length(ListElems))}),
    fold_write_in_terms_through_args(ListElemVar, NewExpansion);
write_in_terms_of(record_expr, Record, Var, Expansion) ->
    none = erl_syntax:record_expr_argument(Record),
    RecordType = erl_syntax:record_expr_type(Record),
    RecordFields = erl_syntax:record_expr_fields(Record),
    {RecordFieldNames, RecordFieldValues} =
	lists:unzip([{erl_syntax:atom_value(
			erl_syntax:record_field_name(RecordField)),
		      erl_syntax:record_field_value(RecordField)}
		     || RecordField <- RecordFields]),
    NamesVar = [list_to_atom("rec_elem_" ++ atom_to_list(RecordFieldName)
			     ++ "_of__:" ++ atom_to_list(erl_syntax:variable_name(Var)))
		|| RecordFieldName <- RecordFieldNames],
    Applies = [{record, get_value, [RecordType, erl_syntax:atom(RecordFieldName), Var]}
	       || RecordFieldName <- RecordFieldNames],
    ListElemVar = lists:zip3(RecordFieldValues, NamesVar, Applies),
    NewExpansion = add_cond(Expansion, {record_type, Var, RecordType}),
    fold_write_in_terms_through_args(ListElemVar, NewExpansion);
write_in_terms_of(variable, VarDest, VarOri, Expansion) ->
    VarDestName = erl_syntax:variable_name(VarDest),
    case has_value(Expansion, VarOri) of
	{true, Value} -> add_var_value(Expansion, VarDestName, Value);
	false -> add_var_value(Expansion, VarDestName, VarOri)
    end;
write_in_terms_of(underscore, _, _, Expansion) ->
    Expansion;
write_in_terms_of(Type, _Sth, _Var, _Expansion) ->
    exit({not_expandable, {write_in_terms_not_implemented_for, Type}}).
%    add_cond(Expansion, {write_in_terms_of, Var, Sth}).

fold_write_in_terms_through_args([], Expansion) ->
    Expansion;
fold_write_in_terms_through_args([{Elem, NameVar, {Module, Function, Args}}|Rest], Expansion) ->
    fold_write_in_terms_through_args(
      Rest, write_in_terms_of(
	      Elem, erl_syntax:variable(NameVar), add_apply(Expansion, NameVar, Module, Function, Args))).
    
grant_equal(Sth, Var, Expansion) ->
    add_cond(Expansion, {equals,  Var, Sth}).

list_elements(nil, _) -> [];
list_elements(list, List) ->
    Head = erl_syntax:list_head(List),
    Tail = erl_syntax:list_tail(List),
    [Head|list_elements(erl_syntax:type(Tail), Tail)];
list_elements(Type, _) -> exit({not_expandable, {not_expanded_list, Type}}).

generate_call_var_name(Expansion, Module, Function, Arity) ->
    {Number, NewExpansion} = touch_index(Expansion, {call, {Module, Function, Arity}}),
    {list_to_atom("call_to-" ++ atom_to_list(Module)
		  ++ [$:|atom_to_list(Function)]
		  ++ [$/|integer_to_list(Arity)]
		  ++ [$-|integer_to_list(Number)]
		  ++ "__"), NewExpansion}.

get_function_details(Qualifier) ->
    case (erl_syntax:type(Qualifier)) of
	module_qualifier -> {erl_syntax:atom_value(
			       erl_syntax:module_qualifier_argument(Qualifier)),
			     erl_syntax:atom_value(
			       erl_syntax:module_qualifier_body(Qualifier))};
	atom -> {'?MODULE', erl_syntax:atom_value(Qualifier)}
    end.

args_to_context([], _Context) -> [];
args_to_context([OneArg|Rest], Context) ->
    [rename(OneArg, normal, Context)|
     args_to_context(Rest, Context)].

prefix(normal) ->
    "%CTXT-";
prefix(Context) ->
    "%CTXT-" ++ integer_to_list(Context) ++ "%".

prefix_fin(normal) ->
    "";
prefix_fin(Context) ->
    "%CTXT-" ++ integer_to_list(Context) ++ "%".

rename(Arg, Ori, Fin) ->
    do_rename(Arg, rename_fun(Ori, Fin)).

do_rename(Arg, RenameFun) ->
    do_rename(erl_syntax:type(Arg), Arg, RenameFun).
do_rename(variable, Arg, RenameFun) ->
    erl_syntax:variable(RenameFun(erl_syntax:variable_name(Arg)));
do_rename(_, Arg, RenameFun) ->
    case erl_syntax:is_leaf(Arg) of
	false -> erl_syntax:update_tree(Arg, [[do_rename(erl_syntax:type(SubTree), SubTree, RenameFun)
					      || SubTree <- SubTreeGroup]
					     || SubTreeGroup <- erl_syntax:subtrees(Arg)]);
	true -> Arg
    end.

renamers(Ori, Fin) ->
    RenameFun = rename_fun(Ori, Fin),
    {RenameFun, fun (AST) -> do_rename(AST, RenameFun) end}.

rename_fun(normal, Fin) ->
    Prefix = prefix(normal),
    PrefixFin = prefix_fin(Fin),
    fun (NameAtom) ->
	    Name = atom_to_list(NameAtom),
	    case lists:prefix(Prefix, Name) of
		true -> NameAtom;
		false -> list_to_atom(PrefixFin ++ Name)
	    end
    end;
rename_fun(Ini, Fin) ->
    Prefix = prefix(Ini),
    LengthPrefix = length(Prefix),
    PrefixFin = prefix_fin(Fin),
    fun (NameAtom) ->
	    Name = atom_to_list(NameAtom),
	    case lists:prefix(Prefix, Name) of
		false -> NameAtom;
		true -> list_to_atom(PrefixFin ++ lists:nthtail(LengthPrefix, Name))
	    end
    end.

save_thiscontext(Expansion) ->
    Context = get_context(Expansion),
    recontext(Expansion, renamers(normal, Context)).

load_thiscontext(Expansion) ->
    Context = get_context(Expansion),
    recontext(Expansion, renamers(Context, normal)).
