-record(expansion, {old_applys = [], applys = [], preds = [],
		    conds = [], result = none, pat_matcha = none,
		    ctx_num = 1, highest_given_ctx = 1,
		    pat_matcha_type = none, record_definitions = [],
		    last_given_ids = [], idioms = []}).
-record(apply, {name = none, module = none, call = none, arg_list = [],
		is_call = false, is_arg = false, evaluated = false,
		value = none}).
-record(call_info, {name, num_args, pre_exp, next_exp, post_exp}).
-record(rep, {rem_srcs, idiom_dict, idiom_ref, sec_dict,
	      sec_ref, etd}).
-record(etd, {matched_applies = sets:new(),
	      idioms, in_ref}).

% API records
-record(exp_iface, {var_defs = [], conds = [], result = [], idioms = []}).
-record(call_iface, {name, num_args, pre_exp, next_exp, post_exp}).
-record(module_iface, {state_fields = [], call_list = []}).
-record(idiom, {name, repr = "", subs = []}).
