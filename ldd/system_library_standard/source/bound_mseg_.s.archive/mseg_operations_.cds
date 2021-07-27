/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1985 *
   *                                                         *
   *********************************************************** */

/* Definition of the message segment primitive operations (mseg_) */

/* Created:  April 1985 by G. Palter */

/* format: style3,linecom */

mseg_operations_:
     procedure () options (variable);


declare	1 mops		aligned,
	  2 (add_acl_entries_seg, add_message, chname_seg, close_seg, compact_seg, copy_seg_source, copy_seg_target,
	       count_messages, create_seg, delete_acl_entries_seg, delete_message, delete_seg, get_salvaged_flag_seg,
	       get_wakeup_state_seg, initiate_seg, list_acl_seg, list_acl_entries_seg, open_seg, read_message,
	       replace_acl_seg, reset_salvaged_flag_seg, reset_wakeup_state_seg, set_max_length_seg,
	       set_safety_switch_seg, set_wakeup_state_seg, update_message)
			bit (36) aligned,
	  2 names		(26) character (64) varying;


dcl	1 cds_data	aligned like cds_args;

dcl	code		fixed binary (35);
dcl	last_operation_id	fixed binary (9);

dcl	MSEG_OPERATIONS_	character (32) static options (constant) initial ("mseg_operations_");

dcl	error_table_$bigarg fixed binary (35) external;
dcl	error_table_$out_of_bounds
			fixed binary (35) external;

dcl	com_err_		entry () options (variable);
dcl	create_data_segment_
			entry (pointer, fixed binary (35));

dcl	(addr, currentsize, hbound, length, maxlength, null, string)
			builtin;
%page;
/* Define the mseg_operations_$OPERATION and mseg_operations_$names constants */

	last_operation_id = 0;

	call operation (mops.add_acl_entries_seg, "mseg_$add_acl_entries_seg", MSEG_REQUIRED_FOR_FS_INTERFACE,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.add_message, "mseg_$add_message", MSEG_REQUIRED_FOR_MESSAGE,
	     MSEG_BEGIN_FOR_MESSAGE | MSEG_INITIALIZE_HEADER, MSEG_FINISH_FOR_MESSAGE);

	call operation (mops.chname_seg, "mseg_$chname_seg", MSEG_REQUIRE_PATHNAME | MSEG_REQUIRE_MSEG_PTR,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.close_seg, "mseg_$close_seg", MSEG_REQUIRE_MSEG_PTR | MSEG_REQUIRE_MSEG_INDEX,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.compact_seg, "mseg_$compact_seg", MSEG_REQUIRED_FOR_SEGMENT, MSEG_BEGIN_FOR_MESSAGE,
	     MSEG_FINISH_FOR_MESSAGE);		/* compaction requires working with the individual messages */

	call operation (mops.copy_seg_source, "mseg_$copy_seg", MSEG_REQUIRED_FOR_SEGMENT, MSEG_BEGIN_FOR_MESSAGE,
	     MSEG_FINISH_FOR_MESSAGE);		/* copying requires working with the individual messages */
	call operation (mops.copy_seg_target, "mseg_$copy_seg", MSEG_REQUIRED_FOR_SEGMENT, MSEG_LOCK_SEGMENT,
	     MSEG_FINISH_FOR_SEGMENT);

	call operation (mops.count_messages, "mseg_$count_messages", MSEG_REQUIRE_MSEG_PTR | MSEG_REQUIRE_ACCESS_INFO,
	     MSEG_BEGIN_FOR_MESSAGE, MSEG_FINISH_FOR_MESSAGE);
						/* there's no individual message for which we'd need info */

	call operation (mops.create_seg, "mseg_$create_seg", MSEG_REQUIRED_FOR_FS_INTERFACE,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.delete_acl_entries_seg, "mseg_$delete_acl_entries_seg", MSEG_REQUIRED_FOR_FS_INTERFACE,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.delete_message, "mseg_$delete_message", MSEG_REQUIRED_FOR_MESSAGE, MSEG_BEGIN_FOR_MESSAGE,
	     MSEG_FINISH_FOR_MESSAGE);

	call operation (mops.delete_seg, "mseg_$delete_seg", MSEG_REQUIRE_MSEG_PTR,
	     MSEG_LOCK_SEGMENT | MSEG_DONT_CHECK_LOCK_RESULTS, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.get_salvaged_flag_seg, "mseg_$get_salvaged_flag_seg", MSEG_REQUIRED_FOR_SEGMENT,
	     MSEG_BEGIN_FOR_SEGMENT, MSEG_FINISH_FOR_SEGMENT);

	call operation (mops.get_wakeup_state_seg, "mseg_$get_wakeup_state_seg",
	     MSEG_REQUIRED_FOR_SEGMENT | MSEG_REQUIRE_WAKEUP_STATE_VERSION, MSEG_BEGIN_FOR_SEGMENT,
	     MSEG_FINISH_FOR_SEGMENT);

	call operation (mops.initiate_seg, "mseg_$initiate_seg", MSEG_REQUIRED_FOR_FS_INTERFACE,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.list_acl_seg, "mseg_$list_acl_seg", MSEG_REQUIRED_FOR_FS_INTERFACE,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.list_acl_entries_seg, "mseg_$list_acl_entries_seg", MSEG_REQUIRED_FOR_FS_INTERFACE,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.open_seg, "mseg_$open_seg", MSEG_REQUIRE_MSEG_PTR, MSEG_BEGIN_FOR_FS_INTERFACE,
	     MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.read_message, "mseg_$read_message", MSEG_REQUIRED_FOR_MESSAGE, MSEG_BEGIN_FOR_MESSAGE,
	     MSEG_FINISH_FOR_MESSAGE);

	call operation (mops.replace_acl_seg, "mseg_$replace_acl_seg", MSEG_REQUIRED_FOR_FS_INTERFACE,
	     MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.reset_salvaged_flag_seg, "mseg_$reset_salvaged_flag_seg",
	     MSEG_REQUIRED_FOR_SEGMENT | MSEG_REQUIRE_ACCESS_INFO, MSEG_BEGIN_FOR_SEGMENT, MSEG_FINISH_FOR_SEGMENT);

	call operation (mops.reset_wakeup_state_seg, "mseg_$reset_wakeup_state_seg", MSEG_REQUIRED_FOR_SEGMENT,
	     MSEG_BEGIN_FOR_SEGMENT, MSEG_FINISH_FOR_SEGMENT);

	call operation (mops.set_max_length_seg, "mseg_$set_max_length_seg", MSEG_REQUIRED_FOR_SEGMENT,
	     MSEG_BEGIN_FOR_SEGMENT | MSEG_CHECK_COUNT_CONSISTENCY, MSEG_FINISH_FOR_SEGMENT);

	call operation (mops.set_safety_switch_seg, "mseg_$set_safety_switch_seg",
	     MSEG_REQUIRE_PATHNAME | MSEG_REQUIRE_MSEG_PTR, MSEG_BEGIN_FOR_FS_INTERFACE, MSEG_FINISH_FOR_FS_INTERFACE);

	call operation (mops.set_wakeup_state_seg, "mseg_$set_wakeup_state_seg",
	     MSEG_REQUIRED_FOR_SEGMENT | MSEG_REQUIRE_WAKEUP_STATE, MSEG_BEGIN_FOR_SEGMENT, MSEG_FINISH_FOR_SEGMENT);

	call operation (mops.update_message, "mseg_$update_message", MSEG_REQUIRED_FOR_MESSAGE, MSEG_BEGIN_FOR_MESSAGE,
	     MSEG_FINISH_FOR_MESSAGE);


/* Create the data segment */

	cds_data.sections (1).p = addr (mops);
	cds_data.sections (1).len = currentsize (mops);
	cds_data.sections (1).struct_name = "mops";

	cds_data.seg_name = MSEG_OPERATIONS_;
	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null ();
	string (cds_data.switches) = ""b;
	cds_data.have_text = "1"b;

	call create_data_segment_ (addr (cds_data), code);
	if code ^= 0
	then call com_err_ (code, MSEG_OPERATIONS_);

RETURN_FROM_MSEG_OPERATIONS_:
	return;
%page;
/* Define a single operation */

operation:
     procedure (p_operation_value, p_operation_name, p_required_for_operation, p_begin_for_operation,
	p_finish_for_operation);

dcl	p_operation_value	bit (36) aligned parameter;
dcl	p_operation_name	character (*) parameter;
dcl	(p_required_for_operation, p_begin_for_operation, p_finish_for_operation)
			bit (9) aligned parameter;

	if last_operation_id >= hbound (mops.names, 1)
	then do;
		call com_err_ (error_table_$out_of_bounds, MSEG_OPERATIONS_,
		     "^/^5xIncrease the dimension of the mops.names array and recompile.");
		go to RETURN_FROM_MSEG_OPERATIONS_;
	     end;

	last_operation_id = last_operation_id + 1;

	mseg_operation_data_ptr = addr (p_operation_value);
	mseg_operation_data.operation_id = last_operation_id;
	string (mseg_operation_data.required_data) = p_required_for_operation;
	string (mseg_operation_data.begin_flags) = p_begin_for_operation;
	string (mseg_operation_data.finish_flags) = p_finish_for_operation;

	if length (p_operation_name) > maxlength (mops.names (last_operation_id))
	then do;
		call com_err_ (error_table_$bigarg, MSEG_OPERATIONS_,
		     "Operation name ""^a"".^/^5xIncrease the maxlength of mops.names and recompile.",
		     p_operation_name);
		go to RETURN_FROM_MSEG_OPERATIONS_;
	     end;

	mops.names (last_operation_id) = p_operation_name;

	return;

     end operation;

/* format: off */
%page; %include mseg_operation_data;
%page; %include cds_args;
/* format: on */

     end mseg_operations_;
