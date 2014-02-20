%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Extracts useful information from a statem model
%%% @end
%%% Created : 30 Jan 2014 by Pablo Lamela
%%%-------------------------------------------------------------------

-module(model_info).

-export([model_info/1, ppr_callinfos/1, get_state_fields/1, create_arg_name/2]).

-define(ARGS_SUFFIX, "_args").

-include("records.hrl").

-type syntaxTree() :: any(). %%% as used in the syntax tools

-type chars() :: [chars() | char()].
%%% Not flattened string as returned by io_lib:format/2

%%% @doc
%%% Extracts information from a QuickCheck <code>statem</code> model.
%%% @param FileName the name of the file containing the model
%%% @return a list with information for each call
%%% @see ppr_callinfos/1
-spec model_info(FileName :: (atom() | string())) -> [#call_info{}].
model_info(FileName) ->
    Funcs = see_logic:get_funcs(FileName),
    {RecordName, RecordFields} = get_state_fields(FileName),
    WildcardRecord = make_wildcard_record({RecordName, RecordFields}),
    Transitions = get_transitions_and_args(Funcs, FileName, WildcardRecord),
    expand_all_functions(Transitions, WildcardRecord, FileName).

%%% @doc
%%% Pretty prints to a string the information extracted from
%%% a <code>statem</code> model.
%%% @param CallInfoList the information extracted from a
%%% <code>statem</code> model, like the one produced by the function
%%% {@link model_info/1}.
%%% @see model_info/1
-spec ppr_callinfos(CallInfoList :: [#call_info{}]) -> chars().
ppr_callinfos([]) -> [];
ppr_callinfos([#call_info{
		  name = Name,
		  num_args = ArgNum,
		  pre_exp = PreExps,
		  next_exp = NextExps,
		  post_exp = PostExps}|Rest]) ->
    [io_lib:format("*****************************************************************************~n", []),
     io_lib:format("    CALL: ~s/~B~n", [Name, ArgNum]),
     io_lib:format("*****************************************************************************~n~n", []),
     io_lib:format("======================================================~n", []),
     io_lib:format("   PRECONDITION~n", []),
     io_lib:format("=====================================================~n~n", []),
     nestcond:ppr_expansions(PreExps),
     io_lib:format("======================================================~n", []),
     io_lib:format("   NEXT STATE~n", []),
     io_lib:format("=====================================================~n~n", []),
     nestcond:ppr_expansions(NextExps),
     io_lib:format("======================================================~n", []),
     io_lib:format("   POSTCONDITION~n", []),
     io_lib:format("=====================================================~n~n", []),
     nestcond:ppr_expansions(PostExps),
     io_lib:format("~n~n", []),
     ppr_callinfos(Rest)].

expand_all_functions([], _, _) -> [];
expand_all_functions([{Name, ArgNum}|Rest], WildcardRecord, FileName) ->
    ArgList = erl_syntax:list(generate_arg_list(ArgNum)),
    [#call_info{
	name = Name, num_args = ArgNum,
	pre_exp = encapsulated_generate_logical_function(
		    pre,
		    {concat_atoms(Name, '_pre'), 1},
		    [WildcardRecord], FileName, WildcardRecord),
	next_exp = encapsulated_generate_logical_function(
		     next,
		     {concat_atoms(Name, '_next'), 3},
		     [WildcardRecord,
		      erl_syntax:variable(create_arg_name(result, result)),
		      ArgList],
		     FileName, WildcardRecord),
	post_exp = encapsulated_generate_logical_function(
		     post,
		     {concat_atoms(Name, '_post'), 3},
		     [WildcardRecord,
		      ArgList,
		      erl_syntax:variable(create_arg_name(result, result))],
		     FileName, WildcardRecord)
       }|expand_all_functions(Rest, WildcardRecord, FileName)].

concat_atoms(Atom1, Atom2) -> list_to_atom(atom_to_list(Atom1)
					   ++ atom_to_list(Atom2)).

encapsulated_generate_logical_function(FuncType, FuncName, Args, FileName, WildcardRecord) ->
    dirty_generate_logical_function(FuncType, FuncName, Args,
				    FileName, WildcardRecord).

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

generate_arg_list(0) -> [];
generate_arg_list(ArgNum) ->
    [erl_syntax:variable(create_arg_name(arg, ArgNum))|
     generate_arg_list(ArgNum - 1)].


make_wildcard_record({RecordName, RecordFields}) ->
    erl_syntax:record_expr(
      erl_syntax:atom(RecordName),
      [erl_syntax:record_field(
	 erl_syntax:atom(FieldName),
	 erl_syntax:variable(create_arg_name(state, FieldName)))
       || {FieldName, _} <- RecordFields]).

%%% @doc
%%% Returns an atom with the name of the variable that
%%% is used for holding the particular argument or result.
%%% Before symbolically executing a callback in a <code>statem</code>
%%% model, some variables are provided depending on the callback,
%%% the number of arguments of the call and the record holding the state.
%%% The names used for those variables are provided by this function.
%%% @param Type the type of variable whose name we want to obtain.
%%% It may be one of the following:
%%% <ul>
%%% <li><code>arg</code> - the second argument is the position
%%% of the argument variable whose name we want to obtain.</li>
%%% <li><code>result</code> - the second argument must be also the
%%% atom <code>result</code>. Returns the name of the variable representing
%%% the result of executing a call.</li>
%%% <li><code>state</code> - the second argument is the name of
%%% the field variable whose name we want to obtain.</li>
%%% </ul>
%%% @param Element the specific variable of the type we want to obtain
%%% @return an atom with the name of the variable
-spec create_arg_name(Type :: ('arg' | 'result' | 'state'), Element :: (integer() | 'result' | atom())) -> atom().
create_arg_name(result, result) -> '__ResultArgVar__';
create_arg_name(state, Atom) ->
    list_to_atom("__StateArgVar__" ++ atom_to_list(Atom));
create_arg_name(arg, N) when is_integer(N) ->
    list_to_atom("__ArgVar_" ++ integer_to_list(N) ++ "__").

%%% @doc
%%% Uses the function initial_state/0 of a <code>statem</code>
%%% model to extract information about the record that holds
%%% the state in the module, (assuming it is a record, and it
%%% does not change).
%%% @param FileName the name of the file containing the model.
%%% @return a tuple with the type of the record and a list
%%% of tuples containing the name of the fields and their
%%% default values in abstract syntax.
-spec get_state_fields(FileName :: (atom() | string())) -> {RecordType :: atom(),
							    [{FieldName :: atom(),
							      DefaultValue :: syntaxTree()}]}.
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
