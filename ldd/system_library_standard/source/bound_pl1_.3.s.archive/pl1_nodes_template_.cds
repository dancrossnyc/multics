/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */


pl1_nodes_template_:
     procedure;

/* Written April 1977 by R.Schoeman to use cds templates for node templates.	*/
/* Modified: 27 March 1980 by PCK to implement by name assignment */


dcl 1 my_cds_args aligned like cds_args;

dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl (addr, binary, length, null, rel, size, unspec) builtin;

dcl  p ptr;
dcl  code fixed bin (35) init (0);

dcl 1 auto_pl1_nodes_template_ aligned,
    2 array_template like array aligned,
    2 block_template like block aligned,
    2 bound_template like bound aligned,
    2 by_name_agg_template like by_name_agg aligned,
    2 default_template like default aligned,
    2 label_template like label aligned,
    2 reference_template like reference aligned,
    2 symbol_template like symbol aligned;


dcl	com_err_ entry options(variable);


%include cds_args;
%include nodes;
%include array;
%include block;
%include by_name_agg;
%include default;
%include label;
%include reference;
%include symbol;


	unspec (my_cds_args) = "0"b;

	unspec (auto_pl1_nodes_template_) = "0"b;



	my_cds_args.have_text = "1"b;
	my_cds_args.num_exclude_names = 0;
	my_cds_args.exclude_array_ptr = null;

	my_cds_args.seg_name = "pl1_nodes_template_";
	my_cds_args.sections(1).struct_name = "auto_pl1_nodes_template_";
	my_cds_args.sections (1).len = size (auto_pl1_nodes_template_) ;
	my_cds_args.sections (1).p = addr (auto_pl1_nodes_template_);




	array_template.node_type = array_node;
	array_template.interleaved = "0"b;

	array_template.number_of_dimensions,
	     array_template.offset_units,
	     array_template.own_number_of_dimensions,
	     array_template.size_units,
	     array_template.element_boundary,
	     array_template.c_element_size,
	     array_template.c_element_size_bits,
	     array_template.c_virtual_origin = 0;

	array_template.element_size,
	     array_template.element_size_bits,
	     array_template.virtual_origin,
	     array_template.bounds,
	     array_template.symtab_virtual_origin,
	     array_template.symtab_element_size,
	     array_template.element_descriptor = null;

	/* BLOCK NODE.  The following fields are initialized in create_block: */
	/*		block.node_type, block.number, block.owner, block.father, */
	/*		block.source_id, block.no_stack */

	block_template.node_type = block_node;
	block_template.brother,
	     block_template.son,
	     block_template.declaration,
	     block_template.end_declaration,
	     block_template.default,
	     block_template.end_default,
	     block_template.context,
	     block_template.prologue,
	     block_template.end_prologue,
	     block_template.return_values,
	     block_template.return_count,
	     block_template.main,
	     block_template.end_main,
	     block_template.free_temps (1),
	     block_template.free_temps (2),
	     block_template.free_temps (3),
	     block_template.entry_list,
	     block_template.temp_list,
	     block_template.o_and_s,
	     block_template.plio_ps,
	     block_template.plio_fa,
	     block_template.plio_ffsb,
	     block_template.plio_ssl,
	     block_template.plio_fab2 = null;

	block_template.level,
	     block_template.symbol_block,
	     block_template.entry_info,
	     block_template.last_auto_loc,
	     block_template.number_of_entries,
	     block_template.enter.start,
	     block_template.enter.end,
	     block_template.leave.start,
	     block_template.leave.end = 0;

	block_template.prefix = "111110000000"b;	/* default conditions */
	string(block_template.why_nonquick) = "0"b;

	block_template.like_attribute,
	     block_template.get_data,
	     block_template.flush_at_call,
	     block_template.processed,
	     block_template.text_displayed = "0"b;

	block_template.prologue_flag = "0"b;
	block_template.pad = "0"b;

	/* BOUND NODE. */

	bound_template.node_type = bound_node;
	bound_template.c_lower,
	     bound_template.c_upper,
	     bound_template.c_multiplier,
	     bound_template.c_desc_multiplier = 0;

	bound_template.next,
	     bound_template.lower,
	     bound_template.upper,
	     bound_template.desc_multiplier,
	     bound_template.symtab_lower,
	     bound_template.symtab_upper,
	     bound_template.symtab_multiplier,
	     bound_template.multiplier = null;

	by_name_agg_template.node_type = by_name_agg_node;
	by_name_agg_template.pad = "0"b;
	by_name_agg_template.ok_to_free = "1"b;
	by_name_agg_template.token,
	     by_name_agg_template.father,
	     by_name_agg_template.left_brother,
	     by_name_agg_template.right_brother,
	     by_name_agg_template.son,
	     by_name_agg_template.next = null;

	default_template.node_type = default_node;
	default_template.next,
	     default_template.predicate,
	     default_template.symbol = null;
	default_template.system,
	     default_template.error,
	     default_template.no_defaults = "0"b;


	label_template.node_type = label_node;
	label_template.next = null;

	label_template.used_as_format,
	     label_template.used_in_goto,
	     label_template.array,
	     label_template.allocated,
	     label_template.symbol_table = "0"b;

	label_template.cross_reference,
	     label_template.statement = null;

	label_template.location,
	     label_template.low_bound,
	     label_template.high_bound = 0;


	reference_template.node_type = reference_node;

	reference_template.subscript_list,
	     reference_template.qualifier,

/* The following reference must be FULLY qualified to avoid an
   apparent ambiguity with reference_template.address.offset.	*/

	     auto_pl1_nodes_template_.reference_template.offset,
	     reference_template.length = null;

	reference_template.ref_count,
	     reference_template.units,
	     reference_template.data_type,
	     reference_template.c_offset,
	     reference_template.c_length = 0;

	reference_template.store_ins = "0"b;

	reference_template.put_data_sw,
	     reference_template.relocation,
	     reference_template.array_ref,
	     reference_template.varying_ref,
	     reference_template.processed,

	     string (reference_template.bits),
	     string (reference_template.more_bits),
	     string (reference_template.info) = "0"b;
	string (reference_template.address) = "0000000000000000000000000001"b; /* no_address */

	reference_template.shared = "1"b;


	symbol_template.node_type = symbol_node;	/* identify this node		*/

	symbol_template.runtime,
	     symbol_template.runtime_offset = (18)"0"b;
	string (symbol_template.source_id) = (27)"0"b;

	symbol_template.cross_references,
	     symbol_template.initial,
	     symbol_template.array,
	     symbol_template.descriptor,
	     symbol_template.equivalence,
	     symbol_template.general,
	     symbol_template.father,
	     symbol_template.brother,
	     symbol_template.son,
	     symbol_template.next,
	     symbol_template.word_size,
	     symbol_template.bit_size,
	     symbol_template.symtab_size,
	     symbol_template.dcl_size = null;

	symbol_template.pix.pic_scale,
	     symbol_template.pix.pic_size,
	     symbol_template.level,
	     symbol_template.location,
	     symbol_template.scale,
	     symbol_template.boundary,
	     symbol_template.size_units,
	     symbol_template.c_word_size,
	     symbol_template.c_bit_size,
	     symbol_template.c_dcl_size = 0;

	symbol_template.reserved,
	     symbol_template.pix.pic_fixed,
	     symbol_template.pix.pic_float,
	     symbol_template.pix.pic_char,
	     symbol_template.allocated = "0"b;

	string (symbol_template.attributes) = "0"b;



	call create_data_segment_ (addr (my_cds_args), code);

	if code ^= 0
	then call com_err_(code, "pl1_nodes_template_");

     end;
