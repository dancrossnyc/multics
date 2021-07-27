/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
/* initialize_faults_data.cds -- control bits for initialize_faults */
/* format: style2 */

initialize_faults_data:
     procedure;

	declare 1 if_data		 aligned,
		2 primary_one	 (0:31) bit (1) unaligned,
		2 pad_align1	 bit (0) aligned,
		2 signal_one	 (0:31) bit (1) unaligned,
		2 pad_align2	 bit (0) aligned,
		2 onc_one		 (0:31) bit (1) unaligned,
		2 pad_align3	 bit (0) aligned,
		2 primary_two	 (0:31) bit (1) unaligned,
		2 pad_align4	 bit (0) aligned,
		2 signal_two	 (0:31) bit (1) unaligned,
		2 pad_align5	 bit (0) aligned,
		2 onc_two		 (0:31) bit (1) unaligned;

	declare create_data_segment_	 entry (ptr, fixed bin (35));
	declare com_err_		 entry () options (variable);
	declare code		 fixed bin (35);
	declare PADSTAR		 (1) char (32) init ("pad*") int static options (constant);

%include fault_vector;
%include cds_args;
	declare 1 CDSA		 aligned like cds_args;
	declare (null, size, string, unspec)
				 builtin;


	unspec (if_data) = ""b;

	if_data.primary_one (FAULT_NO_CMD) = "1"b;
	if_data.primary_one (FAULT_NO_TRB) = "1"b;

	if_data.primary_two = if_data.primary_one;

	if_data.primary_two (FAULT_NO_DF0) = "1"b;
	if_data.primary_two (FAULT_NO_F2) = "1"b;

	if_data.signal_one (FAULT_NO_ACV) = "1"b;
	if_data.signal_one (FAULT_NO_STR) = "1"b;
	if_data.signal_one (FAULT_NO_MME) = "1"b;
	if_data.signal_one (FAULT_NO_F1) = "1"b;
	if_data.signal_one (FAULT_NO_DRL) = "1"b;
	if_data.signal_one (FAULT_NO_LUF) = "1"b;
	if_data.signal_one (FAULT_NO_IPR) = "1"b;
	if_data.signal_one (FAULT_NO_OFL) = "1"b;
	if_data.signal_one (FAULT_NO_DIV) = "1"b;
	if_data.signal_one (FAULT_NO_DF0) = "1"b;
	if_data.signal_one (FAULT_NO_DF1) = "1"b;
	if_data.signal_one (FAULT_NO_DF2) = "1"b;
	if_data.signal_one (FAULT_NO_DF3) = "1"b;
	if_data.signal_one (FAULT_NO_MME2) = "1"b;
	if_data.signal_one (FAULT_NO_MME3) = "1"b;
	if_data.signal_one (FAULT_NO_MME4) = "1"b;
	if_data.signal_one (FAULT_NO_F2) = "1"b;
	if_data.signal_one (FAULT_NO_F3) = "1"b;

	if_data.signal_two = if_data.signal_one;

	if_data.signal_two (FAULT_NO_DF0) = "0"b;
	if_data.signal_two (FAULT_NO_DF1) = "0"b;
	if_data.signal_two (FAULT_NO_DF2) = "0"b;
	if_data.signal_two (FAULT_NO_ACV) = "0"b;
	if_data.signal_two (FAULT_NO_F2) = "0"b;

	if_data.onc_one (FAULT_NO_SDF) = "1"b;
	if_data.onc_one (FAULT_NO_SUF) = "1"b;
	if_data.onc_one (FAULT_NO_ONC) = "1"b;


	unspec (CDSA) = ""b;
	CDSA.sections (1).p = addr (if_data);
	CDSA.sections (1).len = size (if_data);
	CDSA.sections (2).p = null;
	CDSA.sections (2).len = 0;
	CDSA.sections (1).struct_name = "if_data";
	CDSA.num_exclude_names = 1;
	CDSA.exclude_array_ptr = addr (PADSTAR);
	CDSA.have_text = "1"b;
	CDSA.seg_name = "initialize_faults_data";

	call create_data_segment_ (addr (CDSA), code);
	if code ^= 0
	then call com_err_ (code, "initialize_faults_data");
	return;

     end initialize_faults_data;
