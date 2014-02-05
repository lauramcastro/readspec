%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Interface to the extraction tools.
%%% <p>This module collects the most important functions that
%%% can be used to extract information from eqc models using
%%% symbolic execution.</p>
%%% @end
%%% Created :  4 Feb 2014 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(see).

-export([scan_func/3, scan_func_dbg/3, scan_func_str_args/3,
	 scan_func_str_args_dbg/3, scan_model/1,
	 get_arg_var_name/1, get_field_var_name/1,
	 get_result_var_name/0, scan_and_print_model/1,
	 idiomize_model_info/1, print_model_info/1, print_exp_iface/1]).

-include("records.hrl").

-type syntaxTree() :: any(). %%% as used in the syntax tools

%%% @doc
%%% Extracts possibilities from one particular function.
%%% Arguments must represent Erlang expressions, (they may be
%%% complex Erlang expressions as tuples or records, but not patterns),
%%% and they may include unbound variables at any part. Unbound variable
%%% names should not match variables existing in the function
%%% to analyse. The function will try to expand the functions
%%% called within the function to analyse as long as the calls
%%% belong to functions in the module provided. Infinitely
%%% recursive functions may hang the analyser.
%%% @param FileName is an string or atom representing the
%%% relative path to the source where the function is defined
%%% @param FuncName is an atom representing the function to
%%% analyse
%%% @param Args is a list of the ASTs of the arguments that
%%% should be used to symbolically execute the function.
%%% @return a list of #exp_iface records, each representing
%%% a possibility. A tuple with the atom 'not_expandable' as
%%% is first element may be returned if the function cannot
%%% be analysed.
-spec scan_func(atom() | string(), atom(), [syntaxTree()]) -> [#exp_iface{} | {'not_expandable', _}].
scan_func(FileName, FuncName, Args) ->
    declutter_exp_list(scan_func_aux(FileName, FuncName, Args)).

%%% @doc
%%% Equivalent to {@link scan_func/3}, but it does not prune
%%% the result for debugging purposes.
%%% @see scan_func/3
-spec scan_func_dbg(atom() | string(), atom(), [syntaxTree()]) -> [#exp_iface{} | {'not_expandable', _}].
scan_func_dbg(FileName, FuncName, Args) ->
    declutter_exp_list(scan_func_dbg_aux(FileName, FuncName, Args)).

%%% @doc
%%% Calls {@link scan_func/3}, but it takes strings as arguments
%%% and converts them to ASTs before calling it.
%%% @see scan_func/3
-spec scan_func_str_args(atom() | string(), atom(), [string()]) -> [#exp_iface{} | {'not_expandable', _}].
scan_func_str_args(FileName, FuncName, Args) ->
    declutter_exp_list(scan_func_str_args_aux(FileName, FuncName, Args)).

%%% @doc
%%% Calls {@link scan_func_dbg/3}, but it takes strings as arguments
%%% and converts them to ASTs before calling it.
%%% @see scan_func_dbg/3
-spec scan_func_str_args_dbg(atom() | string(), atom(), [string()]) -> [#exp_iface{} | {'not_expandable', _}].
scan_func_str_args_dbg(FileName, FuncName, Args) ->
    declutter_exp_list(scan_func_str_args_dbg_aux(FileName, FuncName, Args)).

%%% @doc
%%% Takes a eqc statem model and extracts possibilities for all
%%% preconditions, postconditions and next state definitions. It
%%% does so by providing as arguments to those calls unbounded
%%% variables defined by the functions {@link get_arg_var_name/1},
%%% {@link get_field_var_name/1}, and {@link get_result_var_name/0}.
%%% Assumes that record information is in the same file,
%%% and that the state is a record.
%%% @param FileName is an string or atom representing the
%%% relative path to the source where the model is defined
%%% @see scan_func/3
%%% @see get_arg_var_name/1
%%% @see get_field_var_name/1
%%% @see get_result_var_name/0
-spec scan_model(atom() | string()) -> #module_iface{call_list::[#call_iface{pre_exp::[#exp_iface{}],next_exp::[#exp_iface{}],post_exp::[#exp_iface{}]}]}.
scan_model(FileName) ->
    declutter_mif(FileName, model_info:model_info(FileName)).

%%% @doc
%%% Returns the name used for the unbound variables passed
%%% as arguments to the functions when using {@link scan_model/1}.
%%% Argument numbers start with 1.
%%% @param ArgNum the number of the argument whose variable name to
%%% obtain
%%% @return an atom with the corresponding variable name
%%% @see scan_model/1
-spec get_arg_var_name(ArgNum::integer()) -> atom().
get_arg_var_name(ArgNum) when is_integer(ArgNum) ->
    model_info:create_arg_name(arg, ArgNum).

%%% @doc
%%% Returns the name used for the unbound variables passed
%%% as fields of the state record to the functions when
%%% using {@link scan_model/1}.
%%% @param FieldName the name of the field whose variable
%%% name to obtain
%%% @return an atom with the corresponding variable name
%%% @see scan_model/1
-spec get_field_var_name(FieldName::atom()) -> atom().
get_field_var_name(FieldName) when is_atom(FieldName) ->
    model_info:create_arg_name(state, FieldName).

%%% @doc
%%% Returns the name used for the unbound variable passed
%%% as result of the call to the functions when
%%% using {@link scan_model/1}.
%%% @return an atom with the corresponding variable name
%%% @see scan_model/1
-spec get_result_var_name() -> atom().
get_result_var_name() ->
    model_info:create_arg_name(result, result).

%%% @doc
%%% Extracts and prints nicely information from a model.
%%% Is a combination of {@link scan_model/1},
%%% {@link idiomize_model_info/1}, and
%%% {@link print_model_info/1}).
%%% @param FileName is an string or atom representing the
%%% relative path to the source where the model is defined.
%%% @see scan_model/1
%%% @see idiomize_model_infol/1
%%% @see print_model_info/1
-spec scan_and_print_model(atom() | string()) -> 'ok'.
scan_and_print_model(FileName) ->
    Model = scan_model(FileName),
    IdiomizedModel = idiomizer:idiomize_module_info(Model),
    print_model_info(IdiomizedModel).

%%% @doc
%%% Adds information about idioms to a #module_iface{} record.
%%% @param ModelInfo record with the information extracted
%%% from a model.
%%% @see scan_model/1
-spec idiomize_model_info(ModelInfo::#module_iface{}) -> #module_iface{}.
idiomize_model_info(ModelInfo) ->
    idiomizer:idiomize_module_info(ModelInfo).

%%% @doc
%%% Prints a #module_iface{} record in a nicer way.
%%% @param ModelInfo record with the information extracted
%%% from a model.
%%% @see scan_model/1
-spec print_model_info(ModelInfo::#module_iface{}) -> 'ok'.
print_model_info(ModelInfo) ->
    model_info:ppr_callinfos(clutter_mif(ModelInfo)).

%%% @doc
%%% Prints an #exp_iface{} record or a list of them in
%%% a nicer way.
%%% @param Exp_iface possibility or list of possibilities
%%% @see scan_func/3
-spec print_exp_iface([#exp_iface{}] | #exp_iface{}) -> 'ok'.
print_exp_iface(#exp_iface{} = ExpIface) ->
    nestcond:ppr_expansions([clutter_exp(ExpIface)]);
print_exp_iface(ExpIfaceList) when is_list(ExpIfaceList) ->
    nestcond:ppr_expansions(clutter_exp_list(ExpIfaceList)).

%%%-------------------------------------------------------------------
%%% Auxiliar functions
%%%-------------------------------------------------------------------

scan_func_aux(FileName, FuncName, Args) ->
    clean_nestcond:clean_expansions(
      scan_func_dbg_aux(FileName, FuncName, Args)).

scan_func_dbg_aux(FileName, FuncName, Args) ->
    see_logic:generate_logical_function({FuncName, length(Args)},
					Args,
					FileName).

scan_func_str_args_aux(FileName, FuncName, Args) ->
    scan_func_aux(FileName, FuncName, see_logic:parse_args(Args)).

scan_func_str_args_dbg_aux(FileName, FuncName, Args) ->
    scan_func_dbg_aux(FileName, FuncName, see_logic:parse_args(Args)).

declutter_exp_list(List) when is_list(List) ->
    lists:map(fun declutter_exp/1, List).

declutter_exp(#expansion{applys = VarDefs,
			 conds = Conditions,
			 result = Result,
			 idioms = Idioms}) ->
    #exp_iface{var_defs = VarDefs, conds = Conditions, result = Result, idioms = Idioms};
declutter_exp(Error) -> Error.

clutter_exp_list(List) when is_list(List) ->
    lists:map(fun clutter_exp/1, List).

clutter_exp(#exp_iface{var_defs = VarDefs, conds = Conditions, result = Result, idioms = Idioms}) ->
    #expansion{applys = VarDefs,
	       conds = Conditions,
	       result = Result,
	       idioms = Idioms}.

declutter_cif(#call_info{name = Name,
			 num_args = NumArgs,
			 pre_exp = PrecondExpansion,
			 next_exp = NextStateExpansion,
			 post_exp = PostcondExpansion}) ->
    #call_iface{name = Name, num_args = NumArgs,
		pre_exp = declutter_exp_list(PrecondExpansion),
		next_exp = declutter_exp_list(NextStateExpansion),
		post_exp = declutter_exp_list(PostcondExpansion)}.

clutter_cif(#call_iface{name = Name,
			 num_args = NumArgs,
			 pre_exp = PrecondExpansion,
			 next_exp = NextStateExpansion,
			 post_exp = PostcondExpansion}) ->
    #call_info{name = Name, num_args = NumArgs,
		pre_exp = clutter_exp_list(PrecondExpansion),
		next_exp = clutter_exp_list(NextStateExpansion),
		post_exp = clutter_exp_list(PostcondExpansion)}.

declutter_mif(FileName, List) when is_list(List) ->		
    #module_iface{
       state_fields = model_info:get_state_fields(FileName),
       call_list = lists:map(fun declutter_cif/1, List)
      }.

clutter_mif(#module_iface{call_list = CallList}) ->
    lists:map(fun clutter_cif/1, CallList).
