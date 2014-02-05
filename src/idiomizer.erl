%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Adds information about idioms to the structure
%%% @end
%%% Created : 5 Feb 2014 by Pablo Lamela

-module(idiomizer).

-export([idiomize_module_info/1]).

-include("records.hrl").

idiomize_module_info(#module_iface{state_fields = {_StateModuleName,
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
	      " argument passed to " ++ func_str(Func, ArgNumLeft), []}
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
