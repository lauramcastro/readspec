%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Defines the representation of
%%% symbolic executions
%%% @end
%%% Created : 21 Jan 2014 by Pablo Lamela

-module(nestcond).

-export([make_expansion/0, set_result/2, add_arg_if_not_bound/2,
	 add_var_value/3, set_func_patmatcha/2, set_case_patmatcha/2,
	 clear_patmatcha/1, add_cond/2, has_value/2, is_bounded/2,
	 variable_type/2, move_result_to_case_patmatcha/1, add_apply/5,
	 touch_index/2, touch_index_n_times/3, get_result/1, get_context/1,
	 set_context/2, new_context/1, recontext/2, ppr_expansions/1,
	 add_record_definitions/2, get_record_definition/2, get_var_value/2,
	 remove_apply_vars/2, get_var_values/2, remove_apply_var/2, add_idiom/4]).

-include("records.hrl").

make_expansion() -> #expansion{}.

set_result(#expansion{} = Expansion, NewResult) ->
    Expansion#expansion{result = NewResult}.
get_result(#expansion{result = Result}) ->
    Result.

add_arg_if_not_bound(#expansion{applys = Applys} = Expansion, ArgName) ->
    case [Apply || Apply <- Applys, Apply#apply.name =:= ArgName] of
	[] -> Expansion#expansion{applys = Applys ++ [mk_arg_apply(ArgName)]};
	_ -> Expansion
    end.

add_var_value(#expansion{applys = Applys} = Expansion, VarName, VarValue) ->
    OtherApplies = [Apply || Apply <- Applys, Apply#apply.name =/= VarName],
    Expansion#expansion{applys = OtherApplies ++ [mk_var_apply(VarName, VarValue)]}.
add_apply(#expansion{applys = Applys} = Expansion, VarName, Module, Function, Args) ->
    OtherApplies = [Apply || Apply <- Applys, Apply#apply.name =/= VarName],
    Expansion#expansion{applys = OtherApplies ++ [mk_func_apply(VarName, Module, Function, Args)]}.

remove_apply_vars(Expansion, []) -> Expansion;
remove_apply_vars(Expansion, [VarName|Tail]) ->
    remove_apply_vars(remove_apply_var(Expansion, VarName), Tail).

remove_apply_var(#expansion{applys = Applys} = Expansion, VarName) ->
    OtherApplies = [Apply || Apply <- Applys,
			     Apply#apply.name =/= VarName],
    Expansion#expansion{applys = OtherApplies}.

mk_arg_apply(ArgName) -> #apply{name = ArgName, is_arg = true}.
mk_var_apply(VarName, VarValue) -> #apply{name = VarName, evaluated = true,
					  value = VarValue}.
mk_func_apply(VarName, Module, Function, Args) ->
    #apply{name = VarName, module = Module, call = Function, arg_list = Args,
	   is_call = true}.
has_value(#expansion{applys = Applys}, VarName) ->
    case [Apply#apply.value || Apply <- Applys, Apply#apply.name =:= VarName,
			       Apply#apply.evaluated =:= true] of
	[Value] -> {true, Value};
	[] -> false;
	_ -> throw(several_values)
    end.
is_bounded(#expansion{applys = Applys}, VarName) ->
    case [Apply#apply.value || Apply <- Applys,
			       Apply#apply.name =:= VarName] of
	[] -> false;
	[_] -> true;
	_ -> throw(several_values)
    end.

variable_type(Expansion, VarName) ->
    case {has_value(Expansion, VarName), is_bounded(Expansion, VarName)} of
	{{true, Value}, _} -> {valued, Value};
	{false, true} -> non_valued;
	{false, false} -> unknown
    end.

get_var_value(Expansion, VarName) ->
    {valued, Value} = variable_type(Expansion, VarName),
    Value.

get_var_values(Expansion, VarNames) ->
    [get_var_value(Expansion, VarName) || VarName <- VarNames].

move_result_to_case_patmatcha(#expansion{result = Result} = Expansion) ->
    set_case_patmatcha(Expansion, [Result]).

set_func_patmatcha(Expansion, Patmatcha) ->
    set_patmatcha_type(Expansion, Patmatcha, func).
set_case_patmatcha(Expansion, Patmatcha) ->
    set_patmatcha_type(Expansion, Patmatcha, 'case').
set_patmatcha_type(#expansion{} = Expansion, Patmatcha, Type) ->
    Expansion#expansion{pat_matcha = Patmatcha, pat_matcha_type = Type}.

clear_patmatcha(#expansion{} = Expansion) ->
    Expansion#expansion{pat_matcha = none, pat_matcha_type = none}.

%% Temporally patched for improved visibility in debugging
add_cond(#expansion{conds = Conds} = Expansion, Cond) ->
    Expansion#expansion{conds = Conds ++ [Cond]}.
%% add_cond(#expansion{conds = Conds} = Expansion, Cond) ->
%%     [Type|ListCond] = tuple_to_list(Cond),
%%     PPCond = [Type|lists:map(fun erl_prettypr:format/1, ListCond)],
%%     Expansion#expansion{conds = Conds ++ [list_to_tuple(PPCond)]}.

touch_index(#expansion{last_given_ids = LastGivenIds} = Expansion, Table) ->
    {Result, NewLastGivenIds} =
	case lists:keyfind(Table, 1, LastGivenIds) of
	    false -> {1, [{Table, 1}|LastGivenIds]};
	    {_, Last} -> {Last + 1, lists:keyreplace(Table, 1,
						     LastGivenIds,
						     {Table, Last + 1})}
	end,
    {Result, Expansion#expansion{last_given_ids = NewLastGivenIds}}.

touch_index_n_times(#expansion{last_given_ids = LastGivenIds} = Expansion, Table, N) ->
    {Result, NewLastGivenIds} =
	case lists:keyfind(Table, 1, LastGivenIds) of
	    false -> {lists:seq(1, N), [{Table, 1}|LastGivenIds]};
	    {_, Last} -> {lists:seq(Last, Last + N - 1), lists:keyreplace(Table, 1,
									  LastGivenIds,
									  {Table, Last + N})}
	end,
    {Result, Expansion#expansion{last_given_ids = NewLastGivenIds}}.
    

get_context(#expansion{ctx_num = Context}) ->    
    Context.

set_context(#expansion{} = Expansion, Context) ->
    Expansion#expansion{ctx_num = Context}.

new_context(Expansion) ->
    NextCtxt = get_highest_given_context(Expansion) + 1,
    NExp = set_highest_given_context(Expansion, NextCtxt),
    set_context(NExp, NextCtxt).

get_highest_given_context(#expansion{highest_given_ctx = Context}) ->    
    Context.

set_highest_given_context(#expansion{} = Expansion, Context) ->
    Expansion#expansion{highest_given_ctx = Context}.

recontext(#expansion{
	     applys = Applies, conds = Conds, result = Result
	    }
	  = Expansion,
	  {RenameAtom, RenameAST}) ->
    Expansion#expansion{
      applys = [recontext_apply(Apply, {RenameAtom, RenameAST})
	        || Apply <- Applies],
      conds = [recontext_conds(Cond, {RenameAtom, RenameAST})
	       || Cond <- Conds],
      result = RenameAST(Result)
     }.

recontext_apply(#apply{name = Name, arg_list = Args,
		       evaluated = IsEval,
		       value = Value}
		= Apply,
		{RenameAtom, RenameAST}) ->
    Apply#apply{
      name = RenameAtom(Name),
      arg_list = [RenameAST(Arg) ||
		     Arg <- Args],
      value = case IsEval of
		  true -> RenameAST(Value);
		  false -> Value
	      end
     }.

add_record_definitions(#expansion{
			  record_definitions = OldRecordDefinitions
			 } = Expansion,
		       RecordDefinitions) ->
    Expansion#expansion{
      record_definitions = OldRecordDefinitions ++ RecordDefinitions
     }.

get_record_definition(RecordType,
		      #expansion{
			 record_definitions = RecordDefinitions
			}) ->
    case lists:keyfind(RecordType, 1, RecordDefinitions) of
	false -> throw({record_definition_not_found, RecordType});
	{RecordType, FieldInfo} -> FieldInfo
    end.

recontext_conds(Cond, {_, RenameAST}) ->
    [Type|ListCond] = tuple_to_list(Cond),
    RNCond = [Type|lists:map(RenameAST, ListCond)],
    list_to_tuple(RNCond).

ppr_expansions(Expansions) ->
    case length(Expansions) of
	Length when Length < 2 -> ok;
	_ -> io:format("================================~n"),
	     io:format("       POSSIBILITIES ...~n"),
	     io:format("================================~n~n")
    end,
    ppr_expansions_aux(Expansions).
ppr_expansions_aux([]) -> ok;
ppr_expansions_aux([Expansion|[]]) ->
    ppr_expansion(Expansion);
ppr_expansions_aux([Expansion|Rest]) ->
    ppr_expansion(Expansion),
    io:format("================================~n"),
    io:format("        OTHERWISE ...~n"),
    io:format("================================~n~n"),
    ppr_expansions_aux(Rest).

ppr_expansion(Expansion) ->
    ppr_expansion_aux(clean_by_idioms(Expansion)).

clean_by_idioms(#expansion{applys = Applies, idioms = Idioms} = Expansion) ->
    IdiomNames = [Idiom#idiom.name || Idiom <- Idioms],
    FilteredApplies = [Apply || Apply <- Applies, not lists:member(Apply#apply.name, IdiomNames)],
    Expansion#expansion{applys = FilteredApplies}.

ppr_expansion_aux(#expansion{applys = Applies,
			     conds = Conds,
			     result = Result,
			     idioms = Idioms} = Expansion) ->
    case Applies of
	[] -> ok;
	_ -> io:format("*** DEFINITIONS ***~n~n"),
	     lists:map(fun (X) -> ppr_apply(X, Idioms) end, Applies)
    end,
    case Conds of
	[] -> ok;
	_ -> io:format("*** REQUIREMENTS ***~n~n"),
	     lists:map(fun (X) -> ppr_cond(X, Idioms) end, Conds)
    end,
    case has_idiom('__res_idiom__', Expansion) of
	false -> io:format("*** RESULT ***~n~n"),
		 ppr_result(Result, Idioms);
	true -> print_idiom_or_varname('__res_idiom__', Expansion)
    end,
    ok.

has_idiom(Idiom, #expansion{idioms = Idioms}) ->
    lists:keymember(Idiom, #idiom.name, Idioms).

print_idiom_or_varname(Idiom, Idioms) ->
    io:format("~s", [get_idiom_or_varname(Idiom, Idioms)]).

get_idiom_or_varname(Idiom, Idioms) ->
    case lists:keyfind(Idiom, #idiom.name, Idioms) of
	false -> io_lib:format("the variable \"~s\"", [atom_to_list(Idiom)]);
	#idiom{repr = Repr,
	       subs = Subs} -> get_idiom(Repr, Subs, Idioms)
    end.

get_idiom(String, [], _) -> io_lib:format("~s", [String]);
get_idiom(String, [{SubName, var_name, VarNameOrIdiom}|Rest], Idioms) ->
    Subs = get_idiom_or_varname(VarNameOrIdiom, Idioms),
    lists:flatten(Subs),
    get_idiom(replace_in_str(String, SubName, Subs), Rest, Idioms).

add_idiom(Name, Repr, Subs, #expansion{idioms = Idioms} = Expansion) ->
    Expansion#expansion{idioms = [#idiom{name = Name,
					 repr = Repr,
					 subs = Subs}
				  |Idioms]};
add_idiom(Name, Repr, Subs, #exp_iface{idioms = Idioms} = Expansion) ->
    Expansion#exp_iface{idioms = [#idiom{name = Name,
					 repr = Repr,
					 subs = Subs}
				  |Idioms]}.

replace_in_str([Head|Rest] = String, Ori, Dest) ->
    case lists:prefix(Ori, String) of
	true -> Dest ++ replace_in_str(lists:nthtail(length(Ori), String), Ori, Dest);
	false -> [Head|replace_in_str(Rest, Ori, Dest)]
    end;
replace_in_str([], _, _) -> [].

ppr_apply(#apply{name = Name, is_call = true,
		 module = Module, call = Call, arg_list = Args}, Idioms) ->
    io:format("[*] We define the variable \"~s\" as the result provided by~n", [atom_to_list(Name)]),
    ModuleDesc = case Module of
		     '?MODULE' -> "this same module";
		     _ -> "the module \"" ++ atom_to_list(Module) ++ "\""
		 end,
    io:format("the function called \"~s\" from ~s, when it is~n", [atom_to_list(Call),
								    ModuleDesc]),
    io:format("executed "),
    case length(Args) of
	0 -> io:format("without arguments.~n");
	1 -> io:format("with the following argument:~n");
	N -> io:format("with the following ~B arguments:~n", [N])
    end,
    [ppr_argument(Idioms, Arg) || Arg <- Args],
    io:format("~n");
ppr_apply(#apply{name = Name, is_arg = true}, _Idioms) ->
    io:format("[*] We define the variable \"~s\" just as the argument provided~n~n", [atom_to_list(Name)]),
    ok;
ppr_apply(#apply{name = Name, evaluated = true, value = Value}, Idioms) ->
    io:format("[*] We define the variable \"~s\" as equal to:~n", [atom_to_list(Name)]),
    io:format("     ~s~n~n", [explain_ast(Value, Idioms, 5)]),
    ok.

ppr_argument(Idioms, Arg) ->
    io:format("    - ~s~n", [explain_ast(Arg, Idioms, 6)]),
    ok.

ppr_cond({equals, Var, Sth}, Idioms) ->
    io:format("[*] ~s must be equal to:~n",
	      [first_cap(get_idiom_or_varname(erl_syntax:variable_name(Var), Idioms))]),
    io:format("     ~s~n~n", [explain_ast(Sth, Idioms, 5)]),
    ok;
ppr_cond({tuple_size, TupleVar, Size}, Idioms) ->
    io:format("[*] ~s must contain a tuple with ~B elements.~n~n",
	      [first_cap(get_idiom_or_varname(erl_syntax:variable_name(TupleVar), Idioms)),
	       erl_syntax:integer_value(Size)]),
    ok;
ppr_cond({list_size, ListVar, Size}, Idioms) ->
    io:format("[*] ~s must contain a list with ~B elements.~n~n",
	      [first_cap(get_idiom_or_varname(erl_syntax:variable_name(ListVar), Idioms)),
	       erl_syntax:integer_value(Size)]),
    ok;
ppr_cond({record_type, RecordVar, Type}, Idioms) ->
    io:format("[*] ~s must contain a record of type \"~s\".~n~n",
	      [first_cap(get_idiom_or_varname(erl_syntax:variable_name(RecordVar), Idioms)),
	       erl_syntax:atom_value(Type)]),
    ok.

first_cap(A) -> first_cap_aux(lists:flatten(A)).
first_cap_aux([]) -> [];
first_cap_aux([H|T]) -> [string:to_upper(H)|T].

ppr_result(Result, Idioms) ->
    io:format("~s~n~n", [explain_ast(Result, Idioms, 0)]),
    ok.

explain_ast(AST, Idioms, Tab) ->
    explain_ast(erl_syntax:type(AST), AST, Idioms, Tab).
explain_ast(nil, _AST, _Idioms, _Tab) ->
    "the empty list";
explain_ast(list, AST, Idioms, Tab) ->
    Pan = pan(Tab + 4),
    ListElements = erl_syntax:list_elements(AST),
    case length(ListElements) of
	0 -> "the empty list";
	1 -> "a list with the following element:";
	_ -> "a list with the following elements:"
    end
	++ [io_lib:format("~n~s- ~s", [Pan, explain_ast(erl_syntax:type(Element), Element, Idioms, Tab + 4)])
     || Element <- ListElements];
explain_ast(tuple, AST, Idioms, Tab) ->
    Pan = pan(Tab + 4),
    TupleElements = erl_syntax:tuple_elements(AST),
    case length(TupleElements) of
	0 -> "the empty tuple";
	1 -> "a tuple with the following element:";
	_ -> "a tuple with the following elements:"
    end
	++ [io_lib:format("~n~s- ~s", [Pan, explain_ast(erl_syntax:type(Element), Element, Idioms, Tab + 4)])
     || Element <- TupleElements];
explain_ast(atom, AST, _Idioms, _Tab) ->
    io_lib:format("the literal '~s'", [erl_prettypr:format(AST)]);
explain_ast(integer, AST, _Idioms, _Tab) ->
    io_lib:format("the number '~B'", [erl_syntax:integer_value(AST)]);
explain_ast(variable, AST, Idioms, _Tab) ->
    io_lib:format("~s", [get_idiom_or_varname(erl_syntax:variable_name(AST), Idioms)]);
explain_ast(record_expr, AST, Idioms, Tab) ->
    Pan = pan(Tab + 4),
    RecordFields = erl_syntax:record_expr_fields(AST),
    RecordType = atom_to_list(erl_syntax:atom_value(erl_syntax:record_expr_type(AST))),
    "a record of type \'" ++ RecordType ++ "\'" ++
	case length(RecordFields) of
	    0 -> "";
	    1 -> " with the field:";
	    _ -> " with the fields:"
	end
	++ [begin
		FieldValue = erl_syntax:record_field_value(Field),
		FieldName = atom_to_list(erl_syntax:atom_value(erl_syntax:record_field_name(Field))),
		io_lib:format("~n~s~s = ~s", [Pan, FieldName,
					      explain_ast(erl_syntax:type(FieldValue),
							  FieldValue, Idioms, Tab + 4)])
		    
	    end
     || Field <- RecordFields];
explain_ast(_Type, AST, _Idioms, _Tab) ->
    throw({thing, _Type}),
    io_lib:format("the erlang term: ~s", [erl_prettypr:format(AST)]).

pan(NumSpaces) ->
    [$\  || _ <- lists:seq(1, NumSpaces)].
