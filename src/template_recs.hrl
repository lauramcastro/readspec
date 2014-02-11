-record(idiom_template, {name = none, srcs = [], targets = []}).
-record(place_holder, {label = none, item = none,
		       match_just_once = false}).
-record(tcontainer, {ref = none, content = none}).
-record(tvar_rec, {id = none}).
-record(tobj_rec, {func_list = []}).
-record(place_holder_action, {label = none, action = none, value = none}).
-record(set_idiom_for_apply, {apply_name = none, value = none}).
