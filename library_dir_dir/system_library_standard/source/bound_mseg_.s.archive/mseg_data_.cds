/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1985 *
   *                                                         *
   *********************************************************** */

/* mseg_data_.cds -- static and text data for the message segment 
   primitives */

/* format: style3,idind30 */

mseg_data_:
     procedure;

declare	1 mseg_text		aligned,
	  2 template_operation	aligned like mseg_operation,
	  2 max_message_size	fixed bin (35) init (262110),
	  2 block_size		fixed bin (35) init (32),
	  2 admin_ring		fixed binary (3) initial (1);

declare	1 mseg_static		aligned,
	  2 lock_id		bit (36) aligned init (""b),
	  2 process_max_authorization bit (72) aligned init (""b),
	  2 group_id		char (32) unaligned,
	  2 execution_ring		fixed bin (3) aligned;

declare	1 cdsa			aligned like cds_args;

declare	create_data_segment_	entry (pointer, fixed binary (35));
declare	com_err_			entry () options (variable);
declare	code			fixed bin (35);


	mseg_operation_ptr = addr (mseg_text.template_operation);
	unspec (mseg_operation) = ""b;
	mseg_operation.version = MSEG_OPERATION_VERSION_1;
	mseg_operation.operation = ""b;
	mseg_operation.access_operation = 0;
	mseg_operation.caller.validation_level = -1;
	mseg_operation.caller.authorization = ""b;
	mseg_operation.caller.max_authorization = ""b;
	mseg_operation.caller.group_id = "";
	string (mseg_operation.flags) = ""b;
	mseg_operation.dir_name = "";
	mseg_operation.entryname = "";
	mseg_operation.mseg_ptr = null ();
	mseg_operation.md_ptr = null ();
	mseg_operation.access_info.version = ENTRY_ACCESS_INFO_VERSION_1;
	mseg_operation.access_info.type = 0;
	mseg_operation.access_info.dir_name = "";
	mseg_operation.access_info.entryname = "";
	mseg_operation.message_info.version = MSEG_MESSAGE_INFO_V1;
	mseg_operation.wakeup_state.version = MSEG_WAKEUP_STATE_VERSION_1;

	unspec (cdsa) = ""b;
	cdsa.sections (1) /* text */ .p = addr (mseg_text);
	cdsa.sections (1).len = currentsize (mseg_text);
	cdsa.sections (1).struct_name = "mseg_text";

	cdsa.sections (2) /* static */ .p = addr (mseg_static);
	cdsa.sections (2).len = currentsize (mseg_static);
	cdsa.sections (2).struct_name = "mseg_static";

	cdsa.seg_name = "mseg_data_";
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null ();
	cdsa.switches.have_text, cdsa.switches.have_static = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then call com_err_ (code, "mseg_data_");
	return;

%include cds_args;
%include mseg_operation;
%include entry_access_info;
%include mseg_message_info;
%include mseg_wakeup_state;
     end mseg_data_;
