%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Extracts useful information from statem model
%%% @end
%%% Created : 30 Jan 2014 by Pablo Lamela

-module(model_info).

-export([model_info/1, ppr_callinfos/1, get_state_fields/1, create_arg_name/2]).

-define(ARGS_SUFFIX, "_args").

-include("records.hrl").

model_info(FileName) ->
    Funcs = see_logic:get_funcs(FileName),
    {RecordName, RecordFields} = get_state_fields(FileName),
    WildcardRecord = make_wildcard_record({RecordName, RecordFields}),
    RecordIdiomList = make_record_idiom_list(RecordFields),
    Transitions = get_transitions_and_args(Funcs, FileName, WildcardRecord),
    expand_all_functions(Transitions, WildcardRecord, FileName, RecordIdiomList).

ppr_callinfos([]) -> ok;
ppr_callinfos([#call_info{
		name = Name,
		num_args = ArgNum,
		pre_exp = PreExps,
		next_exp = NextExps,
		post_exp = PostExps}|Rest]) ->
		    io:format("*****************************************************************************~n"),
		    io:format("    CALL: ~s/~B~n", [Name, ArgNum]),
		    io:format("*****************************************************************************~n~n"),
		    io:format("======================================================~n"),
		    io:format("   PRECONDITION~n"),
		    io:format("=====================================================~n~n"),
		    nestcond:ppr_expansions(PreExps),
		    io:format("======================================================~n"),
		    io:format("   NEXT STATE~n"),
		    io:format("=====================================================~n~n"),
		    nestcond:ppr_expansions(NextExps),
		    io:format("======================================================~n"),
		    io:format("   POSTCONDITION~n"),
		    io:format("=====================================================~n~n"),
		    nestcond:ppr_expansions(PostExps),
		    io:format("~n~n"),
		    ppr_callinfos(Rest).

expand_all_functions([], _, _, _) -> [];
expand_all_functions([{Name, ArgNum}|Rest], WildcardRecord, FileName, RecordIdiomList) ->
    ArgList = erl_syntax:list(generate_arg_list(ArgNum)),
    ArgIdiomList = generate_arg_idiom_list({Name, ArgNum}, ArgNum),
    ARIdiomList = [res_idiom({Name, ArgNum})|ArgIdiomList] ++ RecordIdiomList,
    [#call_info{
	name = Name, num_args = ArgNum,
	pre_exp = encapsulated_generate_logical_function(
		    pre,
		    {concat_atoms(Name, '_pre'), 1},
		    [WildcardRecord], FileName, WildcardRecord, ARIdiomList),
	next_exp = encapsulated_generate_logical_function(
		     next,
		     {concat_atoms(Name, '_next'), 3},
		     [WildcardRecord,
		      erl_syntax:variable(create_arg_name(result, result)),
		      ArgList],
		     FileName, WildcardRecord, ARIdiomList),
	post_exp = encapsulated_generate_logical_function(
		     post,
		     {concat_atoms(Name, '_post'), 3},
		     [WildcardRecord,
		      ArgList,
		      erl_syntax:variable(create_arg_name(result, result))],
		     FileName, WildcardRecord, ARIdiomList)
       }|expand_all_functions(Rest, WildcardRecord, FileName, RecordIdiomList)].

concat_atoms(Atom1, Atom2) -> list_to_atom(atom_to_list(Atom1)
					   ++ atom_to_list(Atom2)).

encapsulated_generate_logical_function(FuncType, FuncName, Args, FileName, WildcardRecord, ARIdiomList) ->
    add_arg_idioms(clean_nestcond:clean_expansions(
		     dirty_generate_logical_function(FuncType, FuncName, Args,
						     FileName, WildcardRecord)),
		   ARIdiomList).

add_arg_idioms([], _) -> [];
add_arg_idioms([Expansion|Rest], List) ->
    [add_arg_idioms_to_one(Expansion, List)|add_arg_idioms(Rest, List)].
add_arg_idioms_to_one(Expansion, []) -> Expansion;
add_arg_idioms_to_one(Expansion, [{Name, Repr, Subs}|Rest]) ->
    add_arg_idioms_to_one(nestcond:add_idiom(Name, Repr, Subs, Expansion), Rest).

dirty_generate_logical_function(FuncType, FuncName, Args, FileName, WildcardRecord) ->
    case 
	   see_logic:generate_logical_function(FuncName, Args, FileName) of
	{not_expandable, {function_not_found, _}} ->
	    case FuncType of
		next -> [make_exp_with_result(WildcardRecord, FileName)];
		post -> [make_exp_with_result(erl_syntax:atom(true), FileName)];
		pre -> [make_exp_with_result(erl_syntax:atom(true), FileName)]
	    end;
	[] -> throw({cannot_parse, could_not_expand, {FuncName, length(Args)}});
	A when is_list(A) -> A;
	Err -> throw({cannot_parse, Err, {FuncName, length(Args)}})
    end.

make_exp_with_result(Result, FileName) -> 
    nestcond:set_result(
      hd(see_logic:expand_function(
	   Result,
	   nestcond:add_record_definitions(nestcond:make_expansion(),
					   see_logic:get_record_definitions(FileName)),
	   [])),
      Result).

res_idiom({Func, ArgNum}) ->
    ResVarName = create_arg_name(result, result),
    {ResVarName, "the result of calling " ++ func_str(Func, ArgNum), []}.

generate_arg_list(0) -> [];
generate_arg_list(ArgNum) ->
    [erl_syntax:variable(create_arg_name(arg, ArgNum))|
     generate_arg_list(ArgNum - 1)].

generate_arg_idiom_list(_, 0) -> [];
generate_arg_idiom_list({Func, ArgNum}, ArgNumLeft) ->
    [begin
	 ArgVarName = create_arg_name(arg, ArgNumLeft),
	 {ArgVarName, "the " ++ ordinal_for(ArgNumLeft) ++
	      " argument passed to " ++ func_str(Func, ArgNumLeft), []}
     end
     |generate_arg_idiom_list({Func, ArgNum}, ArgNumLeft - 1)].
    

ordinal_for(Number) ->
    integer_to_list(Number) ++ ordinal_suffix(Number rem 10).
ordinal_suffix(1) -> "st";
ordinal_suffix(2) -> "nd";
ordinal_suffix(3) -> "rd";
ordinal_suffix(_) -> "th".


func_str(Func, ArgNum) ->
    atom_to_list(Func) ++ [$/|integer_to_list(ArgNum)].

make_wildcard_record({RecordName, RecordFields}) ->
    erl_syntax:record_expr(
      erl_syntax:atom(RecordName),
      [erl_syntax:record_field(
	 erl_syntax:atom(FieldName),
	 erl_syntax:variable(create_arg_name(state, FieldName)))
       || {FieldName, _} <- RecordFields]).

make_record_idiom_list(RecordFields) ->
    [begin
	 FieldVarName = create_arg_name(state, FieldName),
	 {FieldVarName, "the field called \"" ++ atom_to_list(FieldName) ++ "\"", []}
     end || {FieldName, _} <- RecordFields].

create_arg_name(result, result) -> '__ResultArgVar__';
create_arg_name(state, Atom) ->
    list_to_atom("__StateArgVar__" ++ atom_to_list(Atom));
create_arg_name(arg, N) when is_integer(N) ->
    list_to_atom("__ArgVar_" ++ integer_to_list(N) ++ "__").

get_state_fields(FileName) ->
    LF = case see_logic:generate_logical_function({initial_state, 0}, [], FileName) of
	     [LFi] -> LFi;
	     [] -> throw({failed_assumption, impossible, initial_state});
	     [_|_] -> throw({failed_assumption, non_deterministic, initial_state});
	     Err -> throw({cannot_parse, initial_state, Err})
	 end,
    Result = nestcond:get_result(LF),
    case erl_syntax:type(Result) of
	record_expr -> ok;
	_ -> throw({failed_assumption, not_a_record, initialstate})
    end,
    RecordType = erl_syntax:atom_value(erl_syntax:record_expr_type(Result)),
    RecordFields =
	[{erl_syntax:atom_value(erl_syntax:record_field_name(Field)),
	  erl_syntax:record_field_value(Field)} || Field <- erl_syntax:record_expr_fields(Result)],
    {RecordType, RecordFields}.

get_num_args(FuncName, FileName, WildcardRecord) ->
    Results = see_logic:generate_logical_function({FuncName, 1}, [WildcardRecord], FileName),
    MappedResults = [begin
			 ResVal = nestcond:get_result(Result),
			 try length(erl_syntax:list_elements(ResVal))
			 catch
			     ErrTyp:Err -> {failed_assumption, not_a_list_of_args, FuncName, {ErrTyp, Err}}
			 end
		     end || Result <- Results],
    ensure_equal_elements(MappedResults, FuncName).

ensure_equal_elements([], FuncName) ->
    throw({failed_assumption, cannot_count_args, FuncName});
ensure_equal_elements([NumArgs], _) -> NumArgs;
ensure_equal_elements([NumArgs,NumArgs|Rest], FuncName) ->
    ensure_equal_elements([NumArgs|Rest], FuncName);
ensure_equal_elements(_, FuncName) ->
    throw({failed_assumption, non_deterministic_arg_count, FuncName}).



get_transitions_and_args(Funcs, FileName, WildcardRecord) ->
    [{remove_args_suffix(FuncName, ?ARGS_SUFFIX), get_num_args(FuncName, FileName, WildcardRecord)}
     || {{FuncName, 1}, _} <- Funcs,
	has_args_suffix(FuncName, ?ARGS_SUFFIX)].

has_args_suffix(FuncName, Suffix) -> lists:suffix(Suffix, atom_to_list(FuncName)).

remove_args_suffix(FuncName, Suffix) ->
    FuncNameLst = atom_to_list(FuncName),
    {TransNameLst, Suffix} = lists:split(length(FuncNameLst) - length(Suffix),
					       FuncNameLst),
    list_to_atom(TransNameLst).
