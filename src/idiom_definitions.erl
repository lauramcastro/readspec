%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Idiom templates
%%% @end
%%% Created : 6 Feb 2014 by Pablo Lamela

-module(idiom_definitions).

-export([idiom_template_list/1]).
-import(idiomizer, [tsatom_val/2, tsint_val/2, tsvar_name/2, tsvar_id/2,
		    tcontainer/2, tvar/2, tobj/2, tsvar_name_and_id/3, tsoneellist_elid/2]).
-include("records.hrl").
-include("template_recs.hrl").

idiom_template_list(Ref) ->
    [
     key_member_idiom(Ref, false, "must not"),
     key_member_idiom(Ref, true, "must"),
     member_idiom(Ref, false, "must not"),
     member_idiom(Ref, true, "must"),
     keyfind_not_idiom(Ref),
     append_one_el_list_idiom(Ref) 
    ].

key_member_idiom(Ref, Result, String) ->
    #idiom_template{
       name = list_to_atom(atom_to_list('key_member_') ++ atom_to_list(Result) ++ atom_to_list('_idiom')),
       srcs = [#place_holder{label = 'key_member_apply',
			     item = #apply{name = tvar(Ref, 'atom_identifier'),
					   module = lists,
					   call = keymember,
					   arg_list = [tsvar_id(Ref, tvar(Ref, 'key_var')),
						       tsint_val(Ref, tvar(Ref, 'num_col')),
						       tsvar_id(Ref, tvar(Ref, 'dict_var'))],
					   is_call = true,
					   evaluated = false}
			    },
	       #place_holder{label = 'key_member_cond',
			     item = {equals,
				     tsvar_name(Ref, tvar(Ref, 'atom_identifier')),
				     tsatom_val(Ref, Result)}
			    }],
       targets = [#place_holder_action{label = 'key_member_cond',
				       action = replace,
				       value = {{idiom, "the table in ~s " ++ String ++ " contain a row with its~n"
						 ++ "column number ~B equal to ~s",
						 [{ast_var, tvar(Ref, 'dict_var')}, {literal, tvar(Ref, 'num_col')},
						  {ast_var, tvar(Ref, 'key_var')}
						 ]}}}
		 ]}.

member_idiom(Ref, Result, String) ->
    #idiom_template{
       name = list_to_atom(atom_to_list('member_') ++ atom_to_list(Result) ++ atom_to_list('_idiom')),
       srcs = [#place_holder{label = 'member_apply',
			     item = #apply{name = tvar(Ref, 'atom_identifier'),
					   module = lists,
					   call = member,
					   arg_list = [tsvar_id(Ref, tvar(Ref, 'item_var')),
						       tsvar_id(Ref, tvar(Ref, 'collection_var'))],
					   is_call = true,
					   is_arg = false,
					   evaluated = false}
			    },
	       #place_holder{label = 'member_cond',
			     item = {equals,
				     tsvar_name(Ref, tvar(Ref, 'atom_identifier')),
				     tsatom_val(Ref, Result)}
			    }],
       targets = [#place_holder_action{label = 'member_cond',
				       action = replace,
				       value = {{idiom, "the collection in ~s " ++ String ++ "~ncontain ~s",
						 [{ast_var, tvar(Ref, 'collection_var')},
						  {ast_var, tvar(Ref, 'item_var')}
						 ]}}}
		 ]}.

keyfind_not_idiom(Ref) ->
    #idiom_template{
       name = 'keyfind_not',
       srcs = [#place_holder{label = 'key_find_apply',
			     item = #apply{name = tvar(Ref, 'atom_identifier'),
					   module = lists,
					   call = keyfind,
					   arg_list = [tsvar_id(Ref, tvar(Ref, 'key_var')),
						       tsint_val(Ref, tvar(Ref, 'num_col')),
						       tsvar_id(Ref, tvar(Ref, 'dict_var'))],
					   is_call = true,
					   evaluated = false}
			    },
	       #place_holder{label = 'key_find_cond',
			     item = {equals,
				     tsvar_name(Ref, tvar(Ref, 'atom_identifier')),
				     tsatom_val(Ref, false)}
			    }],
       targets = [#place_holder_action{label = 'key_find_cond',
				       action = replace,
				       value = {{idiom, "the table in ~s must not contain a row with its~n"
						 ++ "column number ~B equal to ~s",
						 [{ast_var, tvar(Ref, 'dict_var')}, {literal, tvar(Ref, 'num_col')},
						  {ast_var, tvar(Ref, 'key_var')}
						 ]}}}
		 ]}.

append_one_el_list_idiom(Ref) ->
    #idiom_template{
       name = 'append_one_el_list',
       srcs = [#place_holder{label = 'append_apply',
			     item = #apply{name = tvar(Ref, 'atom_identifier'),
					   module = lists,
					   call = append,
					   arg_list = [tsvar_name(
							 Ref, tvar(Ref, 'collection_varname')), 
						       tsoneellist_elid(
							 Ref, tsvar_name(
								Ref, tvar(Ref, 'element_varname')))],
					   is_call = true,
					   is_arg = false,
					   evaluated = false},
			     match_just_once = true
			    }],
       targets = [
		  #set_idiom_for_apply{apply_name = tvar(Ref, 'atom_identifier'),
		  		       value = nestcond:create_idiom(tvar(Ref, 'atom_identifier'),
		  						     "the result of adding %ELEMENT% to the collection\nin %LIST%",
		  						     [{"%ELEMENT%", var_name, tvar(Ref, 'element_varname')},
		  						      {"%LIST%", var_name, tvar(Ref, 'collection_varname')}]
		  						    )}
		 ]}.
