/* ***********************************************************
   *                                                         *
   * Copyright, (C) BULL HN Information Systems Inc., 1989   *
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */




/* HISTORY COMMENTS:
  1) change(89-10-02,Farley), approve(89-10-19,MCR8144),
     audit(89-10-20,Parisek), install(89-10-30,MR12.3-1102):
     Corrected the declaration of access_audit_count.  It had two "fixed bin"
     dcls that are now being diagnosed as a severity-3 error with rev 32e of
     the pl1 compiler.
                                                   END HISTORY COMMENTS */


/* format: style3,idind30 */

active_hardcore_data:
     proc;

/* This program creates the active_hardcore_data data base */

/* Last modified (Date and reason):
   2/6/76	by S. Webber Initial coding */
/* 9/76 by S. Barr to add hash table sizes and new alloc sizes */
/* Modified 7/77 by THVV for new system_free_seg stuff */
/* 10/18/77 RE Mullen to set system_free_seg event to nonzero */
/* 10/3/79 by BSG for ahd$n_dir_sizes */
/* 03*12*80 by BIM for the io_daemon tag'ed rule */
/* December 1981 by C. Hornig to remove dir sizes. */
/* BIM 4/82 remove system_free_seg. */
/* June 1982 by J. Bongiovanni to add pdd, sl1 unique id */
/* 830621 BIM for validation_fix_severity, a tuning parameter */
/* 84-11-09 by EJ Sharpe for new access_audit_* meter arrays */
/* 84-11-23 by KP Loepere to move pam flush stuff from active_all_rings_data. */
/* 84-12-27 by KP Loepere for pdir_dir_quota. */
/* 85-01-09 by EJ Sharpe for access_audit_num_meters */
/* 1985-01-27, EJ Sharpe: removed old protection_audit_ meters */
/* 1985-04-08, BIM: audit_ring1_fs_object_ops */

/* Automatic */

dcl	1 cdsa			aligned like cds_args;
dcl	code			fixed bin (35);
dcl	pam_flush_buffer_size	fixed bin;

/* Static */

dcl	active_hardcore_dataname	char (20) aligned static init ("active_hardcore_data") options (constant);
dcl	exclude_pad		(1) char (32) aligned static options (constant) init ("pad*");

/* Builtins */

dcl	(addr, bin, dimension, hbound, mod, rel, size, string, unspec)
				builtin;

/* Entries */

dcl	com_err_			entry options (variable);
dcl	create_data_segment_	entry (ptr, fixed bin (35));
%page;
	pam_flush_buffer_size = dimension (active_hardcore_data$pam_flush_buffer, 1);

	begin;					/* allow automatic size for pam_flush_buffer */

dcl	1 AHD			aligned automatic,
	  2 user_unlock_lock	bit (36),		/* lock set when unlocking invalid user lock */
						/* per-system constants */
	  2 pdir_quota		fixed bin (35),	/* max system quota on pdr */
	  2 pdir_dir_quota		fixed bin (35),
	  2 cold_boot_switch	bit (36),		/* indicates if boot is cold or warm */
	  2 sl1_uid		bit (36),		/* unique ID of current sl1 */
	  2 pdd_uid		bit (36),		/* unique ID of current pdd */
	  2 dir_arearp		fixed bin (35),	/* offset of allocation area */
	  2 metering_interval	fixed bin (35),	/* 30 days in fs time */
	  2 nalloc_sizes		fixed bin (35),	/* no. of allocation sizes in dir */
	  2 alloc_sizes		bit (0) aligned,	/* Some ALM programs reference as an array */
	  2 aclsize		fixed bin (35),	/* Size of an ACL entry */
	  2 ensize		fixed bin (35),	/* Size of an entry name */
	  2 elcsize		fixed bin (35),	/* entry/link common size */
	  2 esize			fixed bin (35),	/* Size of an entry */
	  2 link_max		fixed bin,	/* Largest size of a link */
	  2 ht_sizes		(3) fixed bin,	/* Hash table blocks for sizes 2,  3,  and 4 */
	  2 dir_hdrsize		fixed bin (35),	/* Number of words in directory header */
	  2 num_hash_table_sizes	fixed bin (35),	/* Number of hash table sizes */
	  2 hash_table_sizes	(4) fixed bin (35), /* Hash table sizes */
	  2 pad1			(11) fixed bin (35),/* directory sizes used to be here */
	  2 audit_ring1_fs_object_ops bit (1) aligned,	/* never turn on pds$no_audit_ring1_fs_object_ops */
	  2 link_meters		(72) fixed bin (35),/* meters of linker */
	  2 lock_meters,				/* meters for dir locks */
	    3 lock		bit (36),		/* lock for lock meters */
	    3 total_waits		fixed bin,	/* no. of times waited for lock */
	    3 total_time_waiting	fixed bin (52),	/* time spent waiting for locks */
	    3 max_time_waiting	fixed bin (52),	/* most time waiting for locks */
	    3 l_meters		(0:2),		/* meters for locks for reading writing and modifying */
	      4 total_locks		fixed bin,	/* no. of locks for this purpose */
	      4 total_pf		fixed bin,	/* pfs taken while locked for this purpose */
	      4 total_cpu		fixed bin (71),	/* time spent locked for this purpose */
	      4 max_cpu		fixed bin (71),	/* most time spent locked for this purpose */
	      4 max_pf		fixed bin,	/* pfs taken while most time spent */
	      4 total_dirpf		fixed bin,	/* dir pfs taken while locked */
	      4 max_dirpf		fixed bin,	/* dir pfs taken while most time spent locked */
	      4 padl		fixed bin,
	  2 max_hashes		fixed bin (35),	/* longest hash search */
	  2 hashes		(12) fixed bin (35),/* histogram of length of hash searches */
	  2 total_cpu_ac		fixed bin (71),	/* time of access computations */
	  2 max_cpu_ac		fixed bin (71),	/* time of longest access computation */
	  2 total_ac		fixed bin (35),	/* no. of access computations */
	  2 total_pf_ac		fixed bin (35),	/* pfs while computing access */
	  2 max_pf_ac		fixed bin (35),	/* most pfs during access computation */
	  2 pf_long_ac		fixed bin (35),	/* pfs for longest access computation */
	  2 nacls_long_ac		fixed bin (35),	/* no. ACL entries searched during longest access computation */
	  2 max_acls_ac		fixed bin (35),	/* most ACL entries searched during access computation */
	  2 acls_ac		(0:12) fixed bin (35),
						/* histogram of ACL entries searched during access computations */
	  2 pad2			fixed bin,
/**** meters for access_audit_  */
/****     There are currently 42 buckets.  The number is
		derived from the number of audit flags as follows:
		
		1) There are 6 buckets for each object type audited
		(currently fsobj, fsattr, rcp, admin, special, and
		other).  The 6 buckets are for grant/deny of the read,
		modify, and modify_access operation types.

		2) There is one bucket for each event flag (currently
		priv_op, admin_op, fault, small_channel, and
		moderate_channel).

		3) The last bucket is reserved for events which do not
		fit into any other bucket (mostly for debugging) */
	  2 access_audit_num_meters /* constant 42 */
				fixed bin,
	  2 access_audit_count /* number of invocations */
				(42) fixed bin (35),
	  2 access_audit_check_count /* number of invocations just to check flags */
				(42) fixed bin (35),
	  2 access_audit_cpu_time /* total VCPU time */
				(42) fixed bin (71),
	  2 access_audit_pagefaults /* total pagefaults */
				(42) fixed bin (35),
	  2 pad3			(28) fixed bin,	/* system default search rules data for initiate_search_rules */
	  2 search_rules_lock,			/* Lock on default search rules */
	    3 pid			bit (36),
	    3 event		fixed bin (35),
	    3 notify_sw		bit (1),
	  2 n_sr_tags		fixed bin,	/* Number of search rule tags */
	  2 n_sys_rules		fixed bin,	/* Number of search rules */
	  2 sr_tag		(36),		/* array of rule tags */
	    3 name		char (32),	/* tag name */
	    3 flag		bit (36),		/* tag key */
	  2 search_rule		(50),		/* array of dir names */
	    3 name		char (168),	/* dir name */
	    3 flag		bit (36),		/* which tags want this dir */
	  2 validation_fix_severity	fixed bin,
	  2 pam_flush_level		fixed bin (35),	/* # this bootload */
	  2 pam_flush_buffer	(pam_flush_buffer_size) bit (36);

/* Now begins the initialization */

	     unspec (AHD) = ""b;
	     AHD.validation_fix_severity = -1;		/* no syserr at all */
	     AHD.search_rules_lock.event = 101101101000000000000000000b;
						/* Event is 000555000000 */
	     AHD.search_rule (1).name = "initiated_segments";
	     AHD.search_rule (2).name = "referencing_dir";
	     AHD.search_rule (3).name = "working_dir";
	     AHD.search_rule (4).name = ">system_library_standard";
	     AHD.search_rule (5).name = ">system_library_unbundled";
	     AHD.search_rule (6).name = ">system_library_1";
	     AHD.search_rule (7).name = ">system_library_tools";
	     AHD.search_rule (8).name = ">system_library_auth_maint";
	     AHD.search_rule (1).flag = "101"b;
	     AHD.search_rule (2).flag = "101"b;
	     AHD.search_rule (3).flag = "101"b;
	     AHD.search_rule (4).flag = "111"b;
	     AHD.search_rule (5).flag = "111"b;
	     AHD.search_rule (6).flag = "111"b;
	     AHD.search_rule (7).flag = "111"b;
	     AHD.search_rule (8).flag = "110"b;
	     AHD.sr_tag (1).name = "default";
	     AHD.sr_tag (2).name = "system_libraries";
	     AHD.sr_tag (3).name = "io_daemon";
	     AHD.sr_tag (1).flag = "1"b;		/* 10000000..  */
	     AHD.sr_tag (2).flag = "01"b;		/* 0100000.... */
	     AHD.sr_tag (3).flag = "001"b;		/* 0010000.... */
	     AHD.n_sr_tags = 3;
	     AHD.n_sys_rules = 8;
	     AHD.pdir_quota = 1024;
	     AHD.pdir_dir_quota = 102;
	     AHD.num_hash_table_sizes = 4;
	     AHD.hash_table_sizes (1) = 61;
	     AHD.hash_table_sizes (2) = 251;
	     AHD.hash_table_sizes (3) = 1021;
	     AHD.hash_table_sizes (4) = 2039;
	     AHD.metering_interval = 39550767;		/* 30 days in file system time */
	     AHD.nalloc_sizes = 8;
	     AHD.aclsize = 8;
	     AHD.ensize = 14;
	     AHD.elcsize = 24;
	     AHD.esize = 38;
	     AHD.link_max = 72;
	     AHD.ht_sizes (1) = 130;
	     AHD.ht_sizes (2) = 516;
	     AHD.ht_sizes (3) = 1024;
	     AHD.dir_hdrsize = 64;
	     AHD.dir_arearp = AHD.dir_hdrsize;		/* area follows header */
	     AHD.access_audit_num_meters = hbound (access_audit_count, 1);

/* Now make some checks on alignment of certain variables */

	     call check (addr (AHD.total_cpu), "total_cpu", 2);
	     call check (addr (AHD.max_cpu), "max_cpu", 2);
	     call check (addr (AHD.total_cpu_ac), "total_cpu_ac", 2);
	     call check (addr (AHD.max_cpu_ac), "max_cpu_ac", 2);
	     call check (addr (AHD.pad3), "pad3", 2);

/* Now set up call to create data base */

	     cdsa.sections (1).p = addr (AHD);
	     cdsa.sections (1).len = size (AHD);
	     cdsa.sections (1).struct_name = "AHD";

	     cdsa.seg_name = "active_hardcore_data";
	     cdsa.num_exclude_names = 1;
	     cdsa.exclude_array_ptr = addr (exclude_pad);

	     string (cdsa.switches) = "0"b;
	     cdsa.switches.have_text = "1"b;

	     call create_data_segment_ (addr (cdsa), code);
%page;
check:
     proc (where, message, modulo);

dcl	where			ptr;
dcl	message			char (*);
dcl	modulo			fixed bin;

	if mod (bin (rel (where), 18), modulo) ^= 0
	then call com_err_ (0, active_hardcore_dataname, "The variable ^a is not aligned on a ^d-word boundary.",
		message, modulo);

     end check;
	end;					/* begin block */
%page;
%include cds_args;
%page;
%include pathname_am;
%page;
%include syserr_constants;
     end active_hardcore_data;
