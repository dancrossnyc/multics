/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */
log_data_:
     procedure ();

/* *	LOG_DATA_
   *
   *	Constants concerning new-format logs, and, in particular, the names and
   *	directories for the new-format syserr log.
   *
   *	Modification history:
   *	84-10-30, W. Olin Sibert: Converted from ALM
   */

declare 1 log_data			aligned automatic,
	2 default_log_size		fixed bin		init (51200),
          2 new_message_flag            bit (36) aligned    init ("777111555333"b3),
          2 complete_message_flag       bit (36) aligned    init ("666000444222"b3),
          2 deleted_message_flag        bit (36) aligned    init ("111777333555"b3),
	2 max_text_lth		fixed bin (21)	init (16000),
	2 max_data_size		fixed bin (18)	init (16000),

	2 syserr_log_name		char (32)		init ("syserr_log"),
	2 syserr_log_empty_name	char (32)		init ("syserr_log.empty"),
	2 syserr_log_dir		char (168)	init (">system_library_1"),
	2 syserr_log_history_dir	char (168)	init (">system_control_1>syserr_log"),
	2 syserr_log_partition	char (4)		init ("log"),
	2 syserr_log_daemon		char (32)		init ("SyserrLogger.SysDaemon.z");


declare 1 cds_info aligned like cds_args;
declare	code fixed bin (35);

declare	com_err_ entry options (variable);
declare	create_data_segment_ entry (pointer, fixed bin (35));

/* */

	unspec (cds_info) = ""b;
	cds_info.sections (1).p = addr (log_data);
	cds_info.sections (1).len = size (log_data);
	cds_info.sections (1).struct_name = "log_data";
	cds_info.seg_name = "log_data_";
	cds_info.switches.have_text = "1"b;

	call create_data_segment_ (addr (cds_info), code);

	if (code ^= 0) then
	     call com_err_ (code, cds_info.seg_name, "Cannot create CDS segment.");

	return;

%page; %include cds_args;

	end log_data_;
