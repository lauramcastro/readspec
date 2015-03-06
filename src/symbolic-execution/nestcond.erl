%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Defines the representation of symbolic executions.
%%% <p>This module provides most of the functions that directly
%%% modify the <code>#expansion{}</code> record. Nestcond comes
%%% from "nested conditions", which was the original
%%% idea for representing symbolic execution results.</p>
%%% <p>Each <code>#expansion{}</code> record saves information
%%% about one possibility of execution and its
%%% basic fields collected in the simplified version
%%% <code>#exp_iface{}</code> are:
%%% <ul>
%%% <li> applys (var_defs) - Defines variables in terms
%%% of existing variables, parameters and function calls.
%%% (Its name comes from the call to erlang:apply)</li>
%%% <li> conds - Defines the conditions that must be
%%% satisfied in order for the result to be produced.
%%% It is written in terms of variables and definitions in
%%% applys.</li>
%%% <li> result - The result to be produced.</li>
%%% <li> idioms - Natural descriptions of the applies and
%%% results, (conds include their own idioms and idiom type).</li>
%%% </ul>
%%% </p>
%%% <p>
%%% Some fields not collected in the simplified version
%%% due to their transient nature:
%%% <ul>
%%% <li> pat_matcha - Store information about a surrounding
%%% pattern matching assignment. It consists in a list which
%%% has an element per argument in function clause pattern
%%% matching and only one element in other cases.</li>
%%% <li> pat_matcha_type - Indicates the type of the current
%%% pattern matching. It may be the atom <code>'case'</code>
%%% in "case" of a 'case' expression, the atom <code>'func'</code>
%%% in case of a function clause, or the atom <code>'none'</code>
%%% if there is not a pattern matching assignment in process.</li>
%%% <li> last_given_ids - Stores auto-numeric indexes associated
%%% to erlang terms (usually atoms). This allows to generate
%%% unique identifiers for variable names, etc...</li>
%%% <li> ctx_num - Number of context is an special separated
%%% auto-numeric index used to tell apart variables from different
%%% functions.</li>
%%% <li> highest_given_ctx - Unlike the auto-numeric indexes in
%%% last_given_ids, the number of context can decrease and thus
%%% it is necessary to store the highest produced context for
%%% each <code>#expansion{}</code> record. This field is used by the function
%%% {@link new_context/1}.</li>
%%% </ul>
%%% </p>
%%% @end
%%% Created : 21 Jan 2014 by Pablo Lamela
%%%-------------------------------------------------------------------

-module(nestcond).

-export([make_expansion/0, set_result/2, add_arg_if_not_bound/2,
	 add_var_value/3, set_func_patmatcha/2, set_case_patmatcha/2,
	 clear_patmatcha/1, add_cond/2, has_value/2, is_bound/2,
	 variable_type/2, move_result_to_case_patmatcha/1, add_apply/5,
	 touch_index/2, touch_index_n_times/3, get_result/1, get_context/1,
	 set_context/2, new_context/1, recontext/2, ppr_expansions/1,
	 add_record_definitions/2, get_record_definition/2, get_var_value/2,
	 remove_apply_vars/2, get_var_values/2, remove_apply_var/2, add_idiom/4,
	 create_idiom/3]).

-include("records.hrl").

-type syntaxTree() :: any(). %%% as used in the syntax tools


-type condition() :: {equals, syntaxTree(), syntaxTree()} |
		     {tuple_size, syntaxTree(), syntaxTree()} |
		     {list_size, syntaxTree(), syntaxTree()} |
		     {record_type, syntaxTree(), syntaxTree()} |
		     {{idiom, string(), [cond_idiom_substitution()]}}.
%%% represents a condition for a possibility

-type apply_idiom_substitution() :: {SubName :: string(), var_name, VarNameOrIdiom :: atom()}.
%%% represents substitutions to be made in the string provided
%%% in the idioms about applies, see {@link add_idiom/4}, they may be one of the following:
%%% <ul><li><code>var_name</code> - The string SubName is replaced by the
%%% description of the variable with name VarNameOrIdiom, or the idiom with that
%%% name if it exists.</li></ul>


-type cond_idiom_substitution() :: {ast_var, VarAst :: syntaxTree()} |
				   {literal, Literal :: any()} |
				   {ast, Ast :: syntaxTree(), Margin :: integer()}.
%%% represents substitutions to be made in the string provided
%%% in the idioms inside conditions, (in the way substitutions are made by io_lib:format).
%%% They may be one of the following:
%%% <ul>
%%% <li><code>ast_var</code> - Replaces with the description of the variable VarAst,
%%% provided that VarAst is a syntax tree of a variable.</li>
%%% <li><code>literal</code> - Replaces with the term as is.</li>
%%% <li><code>ast</code> - Replaces with the description of the Ast, marginated
%%% using the number of spaces specified by Margin.</li>
%%% </ul>

-type record_field_info() :: {RecordFieldName :: atom(),
			      RecordFieldDefaultValue :: syntaxTree()}.
%%% represents information about fields inside a record

-type record_definition() :: {RecordName :: atom(),
			      [record_field_info()]}.
%%% represents the record definitions extracted from a module


%%% @doc
%%% Initialises the <code>#expansion{}</code> record.
%%% @return an empty <code>#expansion{}</code> record
-spec make_expansion() -> #expansion{}.
make_expansion() -> #expansion{}.

%%% @doc
%%% Sets a different result for the <code>#expansion{}</code> record.
%%% @param Expansion the original <code>#expansion{}</code> record
%%% @param NewResult the result to set
%%% @return an <code>#expansion{}</code> record with the specified result
-spec set_result(Expansion :: #expansion{}, NewResult :: syntaxTree()) -> #expansion{}.
set_result(#expansion{} = Expansion, NewResult) ->
    Expansion#expansion{result = NewResult}.

%%% @doc
%%% Extracts the result from the <code>#expansion{}</code> record.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return the result in the <code>#expansion{}</code> record
-spec get_result(Expansion :: #expansion{}) -> syntaxTree().
get_result(#expansion{result = Result}) ->
    Result.

%%% @doc
%%% Adds an apply entry that represents one of the arguments
%%% provided when symbolically executing the function.
%%% If there is already an entry with the provided name it
%%% does nothing.
%%% @param Expansion the <code>#expansion{}</code> record to modify
%%% @param ArgName the name of the argument to add
%%% @return the updated <code>#expansion{}</code> record
-spec add_arg_if_not_bound(Expansion :: #expansion{}, ArgName :: atom()) -> #expansion{}.
add_arg_if_not_bound(#expansion{applys = Applys} = Expansion, ArgName) ->
    case [Apply || Apply <- Applys, Apply#apply.name =:= ArgName] of
	[] -> Expansion#expansion{applys = Applys ++ [mk_arg_apply(ArgName)]};
	_ -> Expansion
    end.
mk_arg_apply(ArgName) -> #apply{name = ArgName, is_arg = true}.

%%% @doc
%%% Adds an apply entry that represents an evaluated (or partially evaluated)
%%% variable with name VarName to the value VarValue. It also ensures
%%% that there is not any other conflicting entry with the same name.
%%% @param Expansion the <code>#expansion{}</code> record to modify
%%% @param VarName the name of the variable to add
%%% @param VarValue the AST representing the value of the variable
%%% @return the updated <code>#expansion{}</code> record
-spec add_var_value(Expansion :: #expansion{}, VarName :: atom(),
		    VarValue :: syntaxTree()) -> #expansion{}.
add_var_value(#expansion{applys = Applys} = Expansion, VarName, VarValue) ->
    OtherApplies = [Apply || Apply <- Applys, Apply#apply.name =/= VarName],
    Expansion#expansion{applys = OtherApplies ++ [mk_var_apply(VarName, VarValue)]}.
mk_var_apply(VarName, VarValue) -> #apply{name = VarName, evaluated = true,
					  value = VarValue}.

%%% @doc
%%% Adds an apply entry that represents a call to a function (not evaluated).
%%% If the module starts with the character ? we assume it is fake module
%%% with an special meaning. For example ?RECORD defines functions that
%%% cannot be written in erlang (for access to record, which are not actual erlang
%%% terms).
%%% @param Expansion the <code>#expansion{}</code> record to modify
%%% @param VarName the name of the variable to add
%%% @param Module the name of the module of the call (unless it starts with ?)
%%% @param Function the name of the function of the call
%%% @param Args a list with the arguments of the call (in abstract syntax)
%%% @return the updated <code>#expansion{}</code> record
-spec add_apply(Expansion :: #expansion{}, VarName :: atom(), Module :: atom(),
		Function :: atom(), Args :: [syntaxTree()]) -> #expansion{}.
add_apply(#expansion{applys = Applys} = Expansion, VarName, Module, Function, Args) ->
    OtherApplies = [Apply || Apply <- Applys, Apply#apply.name =/= VarName],
    Expansion#expansion{applys = OtherApplies ++ [mk_func_apply(VarName, Module, Function, Args)]}.
mk_func_apply(VarName, Module, Function, Args) ->
    #apply{name = VarName, module = Module, call = Function, arg_list = Args,
	   is_call = true}.

%%% @doc
%%% Removes a set of apply vars from an <code>#expansion{}</code> record.
%%% It also works with <code>#exp_iface{}</code> records.
%%% @param Expansion the <code>#expansion{}</code> record to modify
%%% @param VarNames list with the names of the variables to remove
%%% @return the updated <code>#expansion{}</code> record
%%% @see remove_apply_var/2
-spec remove_apply_vars(Expansion :: (#expansion{}|#exp_iface{}), VarNames :: [atom()]) -> #expansion{}|#exp_iface{}.
remove_apply_vars(Expansion, []) -> Expansion;
remove_apply_vars(Expansion, [VarName|Tail]) ->
    remove_apply_vars(remove_apply_var(Expansion, VarName), Tail).

%%% @doc
%%% Removes and apply var from an <code>#expansion{}</code> record.
%%% It is also implemented for <code>#exp_iface{}</code> records.
%%% @param Expansion the <code>#expansion{}</code> record to modify
%%% @param VarName the name of the variable to remove
%%% @return the updated <code>#expansion{}</code> record
%%% @see remove_apply_vars/2
-spec remove_apply_var(Expansion :: (#expansion{}|#exp_iface{}), VarName :: atom()) -> #expansion{}|#exp_iface{}.
remove_apply_var(#expansion{applys = Applys} = Expansion, VarName) ->
    OtherApplies = [Apply || Apply <- Applys,
			     Apply#apply.name =/= VarName],
    Expansion#expansion{applys = OtherApplies};
remove_apply_var(#exp_iface{var_defs = Applys} = Expansion, VarName) ->
    OtherApplies = [Apply || Apply <- Applys,
			     Apply#apply.name =/= VarName],
    Expansion#exp_iface{var_defs = OtherApplies}.

%%% @doc
%%% Checks whether there is an evaluated variable in
%%% the applys of the provided <code>#expansion{}</code> record.
%%% If there is, it returns its value.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param VarName the name of the variable to check
%%% @return a tuple with the atom <code>true</code> and the
%%% value if it exists, or the atom <code>false</code>.
%%% @throws several_values
%%% @see is_bound/2
%%% @see variable_type/2
-spec has_value(Expansion :: #expansion{}, VarName :: atom()) -> {true, syntaxTree()} | false.
has_value(#expansion{applys = Applys}, VarName) ->
    case [Apply#apply.value || Apply <- Applys, Apply#apply.name =:= VarName,
			       Apply#apply.evaluated =:= true] of
	[Value] -> {true, Value};
	[] -> false;
	_ -> throw(several_values)
    end.

%%% @doc
%%% Checks whether there is an apply for the given name.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param VarName the name of the variable/apply to search
%%% @return the atom <code>true</code> if there is such an
%%% apply, or the atom <code>false</code> if there is not.
%%% @throws several_values
%%% @see has_value/2
%%% @see variable_type/2
-spec is_bound(Expansion :: #expansion{}, VarName :: atom()) -> true | false.
is_bound(#expansion{applys = Applys}, VarName) ->
    case [Apply#apply.value || Apply <- Applys,
			       Apply#apply.name =:= VarName] of
	[] -> false;
	[_] -> true;
	_ -> throw(several_values)
    end.

%%% @doc
%%% Checks which type of apply a variable name has.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param VarName the name of the variable/apply to search
%%% @return a tuple with the atom <code>valued</code> and
%%% the value if the variable name corresponds to a valued
%%% apply, the atom <code>non_valued</code> if it corresponds
%%% to other type of apply, and the atom <code>unknown</code>
%%% if the variable does not have an apply.
%%% @throws several_values
%%% @see has_value/2
%%% @see is_bound/2
-spec variable_type(Expansion :: #expansion{}, VarName :: atom()) ->
			   {valued, syntaxTree()} | non_valued | unknown.
variable_type(Expansion, VarName) ->
    case {has_value(Expansion, VarName), is_bound(Expansion, VarName)} of
	{{true, Value}, _} -> {valued, Value};
	{false, true} -> non_valued;
	{false, false} -> unknown
    end.

%%% @doc
%%% Assuming that the variable name provided has a valued apply,
%%% it returns the value of that apply.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param VarName the name of the variable/apply to search
%%% @return the value of the apply
%%% @see get_var_values/2
-spec get_var_value(Expansion :: #expansion{}, VarName :: atom()) -> syntaxTree().
get_var_value(Expansion, VarName) ->
    {valued, Value} = variable_type(Expansion, VarName),
    Value.

%%% @doc
%%% Assuming that the variable names provided have valued applies,
%%% it returns the value of those applies.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param VarNames a list with the names of the variables/applys to search
%%% @return a list with the values of the applies
%%% @see get_var_value/2
-spec get_var_values(Expansion :: #expansion{}, VarNames :: [atom()]) -> [syntaxTree()].
get_var_values(Expansion, VarNames) ->
    [get_var_value(Expansion, VarName) || VarName <- VarNames].

%%% @doc
%%% Moves the result of the <code>#expansion{}</code> record to
%%% the pat_matcha field, sets pat_matcha_type to 'case'
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return the updated <code>#expansion{}</code> record
%%% @see set_case_patmatcha/2
%%% @see get_result/1
%%% @see clear_patmatcha/1
-spec move_result_to_case_patmatcha(Expansion :: #expansion{}) -> #expansion{}.
move_result_to_case_patmatcha(#expansion{result = Result} = Expansion) ->
    set_case_patmatcha(Expansion, [Result]).

%%% @doc
%%% Sets the value for the pat_matcha field to the
%%% one provided. Sets the pat_matcha_type to 'func'
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param PatMatcha the new value for the pat_matcha field
%%% @return the updated <code>#expansion{}</code> record
%%% @see set_case_patmatcha/2
%%% @see clear_patmatcha/1
-spec set_func_patmatcha(Expansion :: #expansion{}, PatMatcha :: [syntaxTree()]) -> #expansion{}.
set_func_patmatcha(Expansion, PatMatcha) ->
    set_patmatcha_type(Expansion, PatMatcha, func).

%%% @doc
%%% Sets the value for the pat_matcha field to the
%%% one provided. Sets the pat_matcha_type to 'case'
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param PatMatcha the new value for the pat_matcha field
%%% @return the updated <code>#expansion{}</code> record
%%% @see set_func_patmatcha/2
%%% @see move_result_to_case_patmatcha/1
%%% @see clear_patmatcha/1
-spec set_case_patmatcha(Expansion :: #expansion{}, PatMatcha :: [syntaxTree()]) -> #expansion{}.
set_case_patmatcha(Expansion, PatMatcha) ->
    set_patmatcha_type(Expansion, PatMatcha, 'case').
set_patmatcha_type(#expansion{} = Expansion, PatMatcha, Type) ->
    Expansion#expansion{pat_matcha = PatMatcha, pat_matcha_type = Type}.

%%% @doc
%%% Clears the fields pat_matcha and pat_matcha_type
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return the updated <code>#expansion{}</code> record
%%% @see set_func_patmatcha/2
%%% @see set_case_patmatcha/2
-spec clear_patmatcha(Expansion :: #expansion{}) -> #expansion{}.
clear_patmatcha(#expansion{} = Expansion) ->
    Expansion#expansion{pat_matcha = none, pat_matcha_type = none}.


%%% @doc
%%% Adds a condition to the end of the list of conditions
%%% of the <code>#expansion{}</code> record
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return the updated <code>#expansion{}</code> record
-spec add_cond(#expansion{}, condition()) -> #expansion{}.
add_cond(#expansion{conds = Conds} = Expansion, Cond) ->
    Expansion#expansion{conds = Conds ++ [Cond]}.

%%% @doc
%%% Increases an index by one and returns its last value (before increasing).
%%% If it does not exist, it is created with one as its last value.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param Table the identifier of the index (usually an atom)
%%% @return A tuple with the last value and the updated <code>#expansion{}</code> record
%%% @see touch_index_n_times/3
-spec touch_index(Expansion :: #expansion{}, Table :: any()) -> {integer(), #expansion{}}.
touch_index(#expansion{last_given_ids = LastGivenIds} = Expansion, Table) ->
    {Result, NewLastGivenIds} =
	case lists:keyfind(Table, 1, LastGivenIds) of
	    false -> {1, [{Table, 1}|LastGivenIds]};
	    {_, Last} -> {Last + 1, lists:keyreplace(Table, 1,
						     LastGivenIds,
						     {Table, Last + 1})}
	end,
    {Result, Expansion#expansion{last_given_ids = NewLastGivenIds}}.

%%% @doc
%%% Returns the indexes that would be generated by successively
%%% calling {@link touch_index/2} N times, but does so in a more efficient way.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param Table the identifier of the index (usually an atom)
%%% @param N the number of times to "touch" the index
%%% @return A tuple with the list of values generated and the updated
%%% <code>#expansion{}</code> record
%%% @see touch_index/2
-spec touch_index_n_times(Expansion :: #expansion{}, Table :: any(), N :: integer()) ->
				 {[integer()], #expansion{}}.
touch_index_n_times(#expansion{last_given_ids = LastGivenIds} = Expansion, Table, N) ->
    {Result, NewLastGivenIds} =
	case lists:keyfind(Table, 1, LastGivenIds) of
	    false -> {lists:seq(1, N), [{Table, 1}|LastGivenIds]};
	    {_, Last} -> {lists:seq(Last, Last + N - 1), lists:keyreplace(Table, 1,
									  LastGivenIds,
									  {Table, Last + N})}
	end,
    {Result, Expansion#expansion{last_given_ids = NewLastGivenIds}}.

%%% @doc
%%% Returns the number of context stored in a given
%%% <code>#expansion{}</code> record.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return The number of context of
%%% @see set_context/2
%%% @see new_context/1
-spec get_context(Expansion :: #expansion{}) -> integer().
get_context(#expansion{ctx_num = Context}) ->    
    Context.

%%% @doc
%%% Sets the context number stored in a given
%%% <code>#expansion{}</code> record.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param Context the number of context to set
%%% @return The updated <code>#expansion{}</code> record
%%% @see get_context/1
%%% @see new_context/1
-spec set_context(Expansion :: #expansion{}, Context :: integer()) -> #expansion{}.
set_context(#expansion{} = Expansion, Context) ->
    Expansion#expansion{ctx_num = Context}.

%%% @doc
%%% Creates a new unique context number and changes
%%% the <code>#expansion{}</code> record to it.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return The updated <code>#expansion{}</code> record
%%% @see get_context/1
%%% @see set_context/2
-spec new_context(Expansion :: #expansion{}) -> #expansion{}.
new_context(Expansion) ->
    NextCtxt = get_highest_given_context(Expansion) + 1,
    NExp = set_highest_given_context(Expansion, NextCtxt),
    set_context(NExp, NextCtxt).

get_highest_given_context(#expansion{highest_given_ctx = Context}) ->    
    Context.

set_highest_given_context(#expansion{} = Expansion, Context) ->
    Expansion#expansion{highest_given_ctx = Context}.

%%% @doc
%%% Maps the provided functions to all the places
%%% where a variable or variable name may exist in the
%%% <code>#expansion{}</code> record provided that no idioms have
%%% been defined yet. It is also defined for the
%%% reduced version (the <code>#exp_iface{}</code> record).
%%% The function takes two functions as argument, <code>RenameAtom</code>
%%% and <code>RenameAST</code>, these two functions are expected to
%%% do a variable name transformation on atoms and
%%% syntax trees respectively.
%%% @param Expansion the <code>#expansion{}</code> or #exp_iface record
%%% @param RenamingFuncs a tuple with two functions:
%%% <ul><li><code>RenameAtom</code> - a function that does the renaming
%%% to atoms representing variable names</li>
%%% <li><code>RenameAST</code> - a function that takes a syntax tree
%%% and renames occurrences of variables</li></ul>
%%% @return the updated <code>#expansion{}</code> or #exp_iface record
%%% after all the renaming
-spec recontext(Expansion :: (#expansion{}|#exp_iface{}),
		RenamingFuncs :: {RenameAtom :: fun((atom()) -> atom()),
				  RenameAST :: fun((syntaxTree()) -> syntaxTree())}) ->
		       #expansion{}|#exp_iface{}.
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
     };
recontext(#exp_iface{
	     var_defs = Applies, conds = Conds, result = Result
	    }
	  = Expansion,
	  {RenameAtom, RenameAST}) ->
    Expansion#exp_iface{
      var_defs = [recontext_apply(Apply, {RenameAtom, RenameAST})
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

recontext_conds(Cond, {_, RenameAST}) ->
    [Type|ListCond] = tuple_to_list(Cond),
    RNCond = [Type|lists:map(RenameAST, ListCond)],
    list_to_tuple(RNCond).

%%% @doc
%%% Adds a list of record definitions to the list
%%% of record definitions inside the <code>#expansion{}</code>
%%% record.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @param RecordDefinitions a list of record definitions
%%% @return the updated <code>#expansion{}</code> record
%%% @see get_record_definition/2
%%% @see see_logic:get_record_definitions/1
-spec add_record_definitions(Expansion :: #expansion{},
			     RecordDefinitions :: [record_definition()]) -> #expansion{}.
add_record_definitions(#expansion{
			  record_definitions = OldRecordDefinitions
			 } = Expansion,
		       RecordDefinitions) ->
    Expansion#expansion{
      record_definitions = OldRecordDefinitions ++ RecordDefinitions
     }.

%%% @doc
%%% Obtains a record definition from the list
%%% of record definitions in the <code>#expansion{}</code> record
%%% @param RecordName the name or type of the record
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return information about the record definition
%%% @see add_record_definitions/2
%%% @see see_logic:get_record_definitions/1
-spec get_record_definition(RecordName :: atom(), Expansion :: #expansion{}) ->
				   [record_field_info()].
get_record_definition(RecordType,
		      #expansion{
			 record_definitions = RecordDefinitions
			}) ->
    case lists:keyfind(RecordType, 1, RecordDefinitions) of
	false -> throw({record_definition_not_found, RecordType});
	{RecordType, FieldInfo} -> FieldInfo
    end.

%%% @doc
%%% Prints to a string the main information of a list of
%%% <code>#expansion{}</code> records using semi-natural language.
%%% It uses idiom information provided in the <code>#expansion{}</code>
%%% record when possible.
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return a string representation of the pretty-printed information
-spec ppr_expansions(Expansions :: [#expansion{}]) -> string().
ppr_expansions(Expansions) ->
    lists:flatten(
      [case length(Expansions) of
	   Length when Length < 2 -> [];
	   _ -> [io_lib:format("================================~n", []),
		 io_lib:format("       POSSIBILITIES ...~n", []),
		 io_lib:format("================================~n~n", [])]
       end,
       ppr_expansions_aux(Expansions)]).
ppr_expansions_aux([]) -> [];
ppr_expansions_aux([Expansion|[]]) ->
    ppr_expansion(Expansion);
ppr_expansions_aux([Expansion|Rest]) ->
    [ppr_expansion(Expansion),
     io_lib:format("================================~n", []),
     io_lib:format("        OTHERWISE ...~n", []),
     io_lib:format("================================~n~n", []),
     ppr_expansions_aux(Rest)].

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
    [case Applies of
	 [] -> [];
	 _ -> [io_lib:format("*** DEFINITIONS ***~n~n", []),
	       lists:map(fun (X) -> ppr_apply(X, Idioms) end, Applies)]
     end,
     case Conds of
	 [] -> [];
	 _ -> [io_lib:format("*** REQUIREMENTS ***~n~n", []),
	       lists:map(fun (X) -> ppr_cond(X, Idioms) end, Conds)]
     end,
     case has_idiom('__res_idiom__', Expansion) of
	 false -> [io_lib:format("*** RESULT ***~n~n", []),
		   ppr_result(Result, Idioms)];
	 true -> print_idiom_or_varname('__res_idiom__', Idioms)
     end].

has_idiom(Idiom, #expansion{idioms = Idioms}) ->
    lists:keymember(Idiom, #idiom.name, Idioms).

print_idiom_or_varname(Idiom, Idioms) ->
    io_lib:format("~s", [get_idiom_or_varname(Idiom, Idioms)]).

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

%%% @doc
%%% Creates a #idiom{} record with the same format
%%% than {@link add_idiom/4}.
%%% @param Name name of the idiom (must be identical
%%% To the name of the call that the idiom is explaining).
%%% The special name <code>__res_idiom__</code> will replace
%%% the description of the result field on the <code>#expansion{}</code>
%%% record description.
%%% @param Repr text describing the apply
%%% @param Subs substitutions that must be done
%%% in Repr before rendering
%%% @see add_idiom/4
-spec create_idiom(Name :: atom(), Repr :: string(),
		   Subs :: [apply_idiom_substitution()]) ->
			  #idiom{}.
create_idiom(Name, Repr, Subs) ->
    #idiom{name = Name,
	   repr = Repr,
	   subs = Subs}.

%%% @doc
%%% Adds an idiom associated to an apply to the list of
%%% idioms in a <code>#expansion{}</code> record. It also works
%%% with #exp_iface records.
%%% @param Name name of the idiom (must be identical
%%% to the name of the call that the idiom is explaining).
%%% The special name <code>__res_idiom__</code> will replace
%%% the description of the result field on the <code>#expansion{}</code>
%%% record description.
%%% @param Repr text describing the apply
%%% @param Subs substitutions that must be done in Repr
%%% before rendering
%%% @param Expansion the <code>#expansion{}</code> record
%%% @return the updated <code>#expansion{}</code> record
%%% @see create_idiom/3
-spec add_idiom(Name :: atom(), Repr :: string(),
		Subs :: [apply_idiom_substitution()],
		Expansion :: #expansion{}) ->
		       #expansion{}.
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
    [io_lib:format("[*] We define the variable \"~s\" as the result provided by~n", [atom_to_list(Name)]),
     begin
	 ModuleDesc = case Module of
			  '?MODULE' -> "this same module";
			  _ -> "the module \"" ++ atom_to_list(Module) ++ "\""
		      end,
	 io_lib:format("the function called \"~s\" from ~s, when it is~n", [atom_to_list(Call),
									    ModuleDesc])
     end,
     io_lib:format("executed ", []),
     case length(Args) of
	 0 -> io_lib:format("without arguments.~n", []);
	 1 -> io_lib:format("with the following argument:~n", []);
	 N -> io_lib:format("with the following ~B arguments:~n", [N])
     end,
     [ppr_argument(Idioms, Arg) || Arg <- Args],
     io_lib:format("~n", [])];
ppr_apply(#apply{name = Name, is_arg = true}, _Idioms) ->
    io_lib:format("[*] We define the variable \"~s\" just as in the arguments provided~n~n", [atom_to_list(Name)]);
ppr_apply(#apply{name = Name, evaluated = true, value = Value}, Idioms) ->
    [io_lib:format("[*] We define the variable \"~s\" as equal to:~n", [atom_to_list(Name)]),
     io_lib:format("     ~s~n~n", [explain_ast(Value, Idioms, 5)])].

ppr_argument(Idioms, Arg) ->
    io_lib:format("    - ~s~n", [explain_ast(Arg, Idioms, 6)]).

ppr_cond({equals, Var, Sth}, Idioms) ->
    [io_lib:format("[*] ~s must be equal to:~n",
		   [first_cap(get_idiom_or_varname(erl_syntax:variable_name(Var), Idioms))]),
     io_lib:format("     ~s~n~n", [explain_ast(Sth, Idioms, 5)])];
ppr_cond({tuple_size, TupleVar, Size}, Idioms) ->
    io_lib:format("[*] ~s must contain a tuple with ~B elements.~n~n",
		  [first_cap(get_idiom_or_varname(erl_syntax:variable_name(TupleVar), Idioms)),
		   erl_syntax:integer_value(Size)]);
ppr_cond({list_size, ListVar, Size}, Idioms) ->
    io_lib:format("[*] ~s must contain a list with ~B elements.~n~n",
		  [first_cap(get_idiom_or_varname(erl_syntax:variable_name(ListVar), Idioms)),
		   erl_syntax:integer_value(Size)]);
ppr_cond({record_type, RecordVar, Type}, Idioms) ->
    io_lib:format("[*] ~s must contain a record of type \"~s\".~n~n",
		  [first_cap(get_idiom_or_varname(erl_syntax:variable_name(RecordVar), Idioms)),
		   erl_syntax:atom_value(Type)]);
ppr_cond({{idiom, Text, Subs}}, Idioms) ->
    ReplacedSubs = [case Sub of
			{ast_var, Ast} -> get_idiom_or_varname(erl_syntax:variable_name(Ast), Idioms);
			{literal, Literal} -> Literal;
			{ast, Ast, N} -> explain_ast(Ast, Idioms, N)
		    end || Sub <- Subs],
    FinalText = io_lib:format(Text, ReplacedSubs),
    io_lib:format("[*] ~s.~n~n", [first_cap(FinalText)]).


first_cap(A) -> first_cap_aux(lists:flatten(A)).
first_cap_aux([]) -> [];
first_cap_aux([H|T]) -> [string:to_upper(H)|T].

ppr_result(Result, Idioms) ->
    io_lib:format("~s~n~n", [explain_ast(Result, Idioms, 0)]).

explain_ast(AST, Idioms, Tab) ->
    explain_ast(erl_syntax:type(AST), AST, Idioms, Tab).
explain_ast(nil, _AST, _Idioms, _Tab) ->
    "the empty list";
explain_ast(list, AST, Idioms, Tab) ->
    Margin = marginate(Tab + 4),
    ListElements = erl_syntax:list_elements(AST),
    case length(ListElements) of
	0 -> "the empty list";
	1 -> "a list with the following element:";
	_ -> "a list with the following elements:"
    end
	++ [io_lib:format("~n~s- ~s", [Margin, explain_ast(erl_syntax:type(Element), Element, Idioms, Tab + 4)])
	    || Element <- ListElements];
explain_ast(tuple, AST, Idioms, Tab) ->
    Margin = marginate(Tab + 4),
    TupleElements = erl_syntax:tuple_elements(AST),
    case length(TupleElements) of
	0 -> "the empty tuple";
	1 -> "a tuple with the following element:";
	_ -> "a tuple with the following elements:"
    end
	++ [io_lib:format("~n~s- ~s", [Margin, explain_ast(erl_syntax:type(Element), Element, Idioms, Tab + 4)])
	    || Element <- TupleElements];
explain_ast(atom, AST, _Idioms, _Tab) ->
    io_lib:format("the literal '~s'", [erl_prettypr:format(AST)]);
explain_ast(integer, AST, _Idioms, _Tab) ->
    io_lib:format("the number '~B'", [erl_syntax:integer_value(AST)]);
explain_ast(string, AST, _Idioms, _Tab) ->
    io_lib:format("the string \"~s\"", [erl_syntax:string_value(AST)]);
explain_ast(variable, AST, Idioms, _Tab) ->
    io_lib:format("~s", [get_idiom_or_varname(erl_syntax:variable_name(AST), Idioms)]);
explain_ast(record_expr, AST, Idioms, Tab) ->
    Pan = marginate(Tab + 4),
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
%    throw({thing, _Type}),
    io_lib:format("the erlang term: ~s", [erl_prettypr:format(AST)]).

marginate(NumSpaces) ->
    [$\  || _ <- lists:seq(1, NumSpaces)].
