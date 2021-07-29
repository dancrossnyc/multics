/* ******************************************************
   *                                                    *
   * Copyright, (C) Honeywell Limited, 1983             *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   ****************************************************** */

/* format: style3,^delnl,linecom */
fort_node_templates_:
     proc;

/* Written 28 September 1977 by Richard A. Barnes
	Modified:	25 September 1978 by RAB to help fix 187
	Modified:	2 June 1979 by RAB to add flow_unit.is_active_operator
	Modified:	28 June 1979 by RAB to add flow_unit.dim_or_alias_or_not_set.
	Modified:	2 July 1979 by RAB to fix 218 by moving loop_end_chain stuff
		from loop node to flow_unit node.
	Modified:	14 August 1979 by RAB to replace flow_unit.dim_or_alias_or_not_set
		with flow_unit.always_completely_set.	*/

dcl	me		char (20) int static options (constant) init ("fort_node_templates_");
dcl	code		fixed bin (35);

dcl	1 my_cds_args	aligned like cds_args;

dcl	1 auto_fort_node_templates_
			auto aligned,
	  2 flow_unit_template
			like flow_unit aligned;

dcl	com_err_		entry options (variable);
dcl	create_data_segment_
			entry (ptr, fixed bin (35));

dcl	(addr, null, size, unspec)
			builtin;

%include cds_args;
%include fort_opt_nodes;

	unspec (my_cds_args) = "0"b;

	unspec (auto_fort_node_templates_) = "0"b;

	my_cds_args.have_text = "1"b;
	my_cds_args.num_exclude_names = 0;
	my_cds_args.exclude_array_ptr = null;

	my_cds_args.seg_name = me;
	my_cds_args.sections (1).struct_name = "auto_" || me;
	my_cds_args.sections (1).len = size (auto_fort_node_templates_);
	my_cds_args.sections (1).p = addr (auto_fort_node_templates_);

	flow_unit_template.next,
	     flow_unit_template.back,
	     flow_unit_template.successors,
	     flow_unit_template.predecessors,
	     flow_unit_template.dominator,
	     flow_unit_template.loop,
	     flow_unit_template.next_in_loop,
	     flow_unit_template.loop_end_chain,
	     flow_unit_template.used,
	     flow_unit_template.set,
	     flow_unit_template.busy_on_entry,
	     flow_unit_template.set_multiple,
	     flow_unit_template.busy_on_exit,
	     flow_unit_template.dominated_by,
	     flow_unit_template.is_active_operator,
	     flow_unit_template.always_completely_set = null;

	call create_data_segment_ (addr (my_cds_args), code);

	if code ^= 0
	then call com_err_ (code, me);

     end;
