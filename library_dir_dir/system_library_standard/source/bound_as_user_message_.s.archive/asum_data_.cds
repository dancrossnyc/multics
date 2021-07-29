/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1985 *
   *                                                         *
   *********************************************************** */
/* asum_data_ -- static information for the as_user_message facility */
/* format: style2,indcomtxt */

asum_data_:
     procedure;

/* Created 1985-01, BIM */


/* Automatic */

	dcl     1 cdsa		 aligned like cds_args;
	dcl     code		 fixed bin (35);

/* Constants */

	dcl     NAME		 char (32) int static init ("asum_data_") options (constant);
	dcl     EXCLUDE_PAD		 (1) char (32) aligned static options (constant) init ("pad*");


/* Entries */

	dcl     com_err_		 entry options (variable);
	dcl     create_data_segment_	 entry (ptr, fixed bin (35));

	declare sys_info$access_class_ceiling
				 bit (72) aligned ext;

/* The structure */

	dcl     static_ptr		 pointer;
	dcl     1 asum_data_static	 aligned based (static_ptr),
		2 db_dir		 char (168) unaligned,
						/* Where ? */
		2 db_dir_rb	 (2) fixed bin (3),
		2 db_rb		 (3) fixed bin (3), /* What RB's? */
		2 db_multiclass	 bit (1) aligned,	/* Multiclass */
		2 db_cbi		 aligned like create_branch_info,
						/* The whole story */
		2 db_dir_cbi	 aligned like create_branch_info,
		2 system_info_ptr	 pointer init (null ()),
		2 process_info_ptr	 pointer init (null ()),
		2 entry_ring	 fixed bin (3),	/* perprocess, for auditing purposes */
		2 lock_id		 bit (36) aligned,
		2 process_id	 bit (36) aligned,	/* cheapify locking */
		2 db_locked	 bit (1) aligned,	/* for cleanup handlers */
		2 n_acl_entries	 fixed bin,
		2 acl_entries	 (static_n_acl_entries refer (asum_data_static.n_acl_entries)) aligned
				 like general_extended_acl_entry;


	declare static_n_acl_entries	 fixed bin;

%page;
%include acl_structures;
%include access_mode_values;
%include create_branch_info;
%include cds_args;
%page;


	static_n_acl_entries = 1;
	allocate asum_data_static;
	asum_data_static.db_dir = ">system_control_1>user_messages";
	asum_data_static.db_rb (*) = 1;
	asum_data_static.db_dir_rb (*) = 1;
	asum_data_static.db_multiclass = "1"b;
	asum_data_static.acl_entries (1).access_name = "*.*.*";
	asum_data_static.acl_entries (1).mode = RW_ACCESS;
	asum_data_static.acl_entries (1).extended_mode = "11111"b;
						/* just in case */
	asum_data_static.acl_entries (1).status_code = 0;

	begin;
	     declare cbip		      pointer;
	     declare 1 CBI		      aligned like create_branch_info based (cbip);
	     cbip = addr (asum_data_static.db_cbi);

	     unspec (CBI) = ""b;
	     CBI.version = create_branch_version_2;
	     CBI.priv_upgrade_sw = asum_data_static.db_multiclass;
	     CBI.parent_ac_sw = ^CBI.priv_upgrade_sw;
	     CBI.mode = ""b;			/* ACL set seperately */
	     CBI.rings = asum_data_static.db_rb;
	     CBI.userid = "*.*.*";
	     CBI.bitcnt = 0;
	     CBI.quota = 0;
	     if CBI.priv_upgrade_sw
	     then CBI.access_class = sys_info$access_class_ceiling;
	     else ;				/* ignored */
	     CBI.dir_quota = 0;

	     addr (asum_data_static.db_dir_cbi) -> CBI = CBI;
	     cbip = addr (asum_data_static.db_dir_cbi);
	     CBI.dir_sw = "1"b;
	     CBI.parent_ac_sw = "1"b;
	     CBI.priv_upgrade_sw = "0"b;
	     CBI.rings (1) = 1;
	     CBI.rings (2) = 1;
	     CBI.mode = SMA_ACCESS;
	     CBI.userid = "*.*.*";
	end;

/* Now set up call to create data base */

	cdsa.sections (2).p = addr (asum_data_static);
	cdsa.sections (2).len = size (asum_data_static);
	cdsa.sections (2).struct_name = "asum_data_static";

	cdsa.seg_name = NAME;
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (EXCLUDE_PAD);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_static = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	if code ^= 0
	then call com_err_ (code, NAME);
	return;
     end asum_data_;
