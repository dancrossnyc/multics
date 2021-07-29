/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */

/* DESCRIPTION:
   bj_suffix_info_ - a constant structure to be used by suffix_bj_$suffix_info
   so that the overhead of assigning 10 or more variables at each invocation
   can be avoided.
*/

/* HISTORY:
Written by M. Pandolf, 11/02/83
Modified:
12/11/84 by M.Sharpe:  to correct format.
*/
/* format: style2,ll79,ind3,^indprocbody,ifthendo,ifthen,^indnoniterdo */
/* format: ^inddcls,dclind5,idind35,linecom */
%page;

bj_suffix_info_:
	procedure ();

/* DECLARATIONS */

/* Automatic */

dcl  (
      code			fixed bin (35),
      1 local_cds_args		aligned like cds_args,
      1 bj_suffix_info		aligned like suffix_info
     )				automatic;

/* Constant */

dcl  (
      EXCLUDE_ARRAY			dim (1) char (32) init ("pad*"),
      MYNAME			char (32) init ("bj_suffix_info_")
     )				internal static options (constant);

/* Entry */

dcl  com_err_			entry() options(variable),
     create_data_segment_		entry (ptr, fixed bin(35));

/* Builtin */

dcl  (addr, hbound, size, unspec)	builtin;

%page;

	bj_suffix_info.version = SUFFIX_INFO_VERSION_1;
	bj_suffix_info.type = "bj";
	bj_suffix_info.type_name = "before journal";
	bj_suffix_info.plural_name = "before journals";
	bj_suffix_info.flags.standard_object = "0"b;
	bj_suffix_info.flags.extended_acl = "0"b;
	bj_suffix_info.flags.has_switches = "0"b;
	bj_suffix_info.flags.mbz1 = ""b;
	bj_suffix_info.modes = "r w";
	bj_suffix_info.max_mode_len = 0;
	bj_suffix_info.num_ring_brackets = 0;
	string (bj_suffix_info.copy_flags) = ""b;
	bj_suffix_info.copy_flags.names = "1"b;
	bj_suffix_info.copy_flags.acl = "1"b;
	bj_suffix_info.copy_flags.max_length = "0"b;
	bj_suffix_info.copy_flags.safety_switch = "0"b;

	bj_suffix_info.info_pathname = "";

/* initialize the cds structure */

	unspec (local_cds_args) = ""b;
	local_cds_args.sections (1).p = addr (bj_suffix_info);
	local_cds_args.sections (1).len = size (bj_suffix_info);
	local_cds_args.sections (1).struct_name = "bj_suffix_info";
	local_cds_args.seg_name = MYNAME;
	local_cds_args.exclude_array_ptr = addr (EXCLUDE_ARRAY);
	local_cds_args.num_exclude_names = hbound (EXCLUDE_ARRAY, 1);
	local_cds_args.switches.have_text = "1"b;

/* call cds to make the segment */

	call create_data_segment_ (addr (local_cds_args), code);
	if code ^= 0
	then call com_err_ (code, MYNAME);

	return;

/* Include Files */

%page;
%include cds_args;
%page;
%include suffix_info;
%page;
%include copy_flags;

end bj_suffix_info_;
