/* ***********************************************************
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
  1) change(85-11-27,Herbst), approve(87-07-20,MCR7697),
     audit(87-07-20,GDixon), install(87-08-04,MR12.1-1055):
     Added system_message_ IPS signal.
  2) change(99-06-23,Haggett):
     Y2K
                                                   END HISTORY COMMENTS */


/* SYS_INFO - This is the Wired All Rings Database. */

/* format: style4,delnl,insnl,tree,ifthenstmt,indnoniterend */

sys_info:
     procedure;

/* This program creates the sys_info data base */

/* Last modified (Date and reason):
   2/6/76	by S. Webber Initial coding
   July 77 by T. Casey to add new IPS signals "susp" and "term"
   Oct 77 by T. Casey to rename them to "sus_" and "trm_" to avoid conflicts with users' signal names.
   29 Jan 79 by D. Spector to add initialization_state variable
   11/28/80, W. Olin Sibert, to add reasonable times
   7/82 BIM merge in bootload multics variables with CAH changes.
   September 1982, J. Bongiovanni, for data_management_ringno
   December 1982, E. N. Kittlitz, for seg_size_256K, default_256K_enable, hfp_exponent_available.
   October 1983 through April 1984 for more bootload Multics variables by Keith Loepere.
   83-12-03 BIM for pgt_ IPS signal.
   11 Jun 1984 by Lee A. Newcomb:  added the new IPS' dm_shutdown_warning_
   and dm_user_shutdown_.
   08/22/84 by R. Michael Tague:  Removed dm_shutdown_warning_ and
   dm_user_shutdown_ IPS signals.  Added system_shutdown_scheduled_
   and dm_shutdown_scheduled_ IPS signals.
   84-11-09 by EJ Sharpe for access_class_floor, and the three audit thresholds
   84-11-20 by EJ Sharpe to change thresholds to system_high as a default and
   added the "security_audit_enable" bit
   84-11-27 by EJ Sharpe to replace "security_audit_enable" with per-threshold
   bits: "audit_covert_channel", "audit_successful_access", "audit_unsuccessful_access".
   84-12-11 by EJ Sharpe to rearrange new flags & thresholds for better alignment
   1985-01-28, BIM: AIM privilege masks.
*/

dcl  1 sys_info aligned automatic,
       2 clock_ bit (3) aligned,			/* word used in reading the clock */
       2 time_zone char (4) aligned,			/* time zone for current site */
       2 time_correction_constant fixed bin,		/* first word of double-word correction factor */
       2 time_delta fixed bin,			/* second word of double-word correction factor */
       2 page_size fixed bin,				/* page size used by the system */
       2 max_seg_size fixed bin (18),			/* maximum segment size (in words) */
       2 bce_max_seg_size fixed bin (18),		/* max length in words of pagable segs in bce (also max file size therein) */
       2 default_stack_length fixed bin (18),		/* default length of stack created by makestack */
       2 default_max_length fixed bin (19),		/* default maximum length of segments */
       2 default_dir_max_length fixed bin (19),		/* ditto for directories */
       2 access_class_ceiling aligned like aim_template,	/* who cares */
       2 access_class_floor aligned like aim_template,	/* these must be double-word aligned so ldaq may be used */
       2 successful_access_threshold aligned like aim_template,
       2 unsuccessful_access_threshold aligned like aim_template,
       2 covert_channel_threshold aligned like aim_template,
       2 audit_successful_access bit (1) aligned,
       2 audit_unsuccessful_access bit (1) aligned,
       2 audit_covert_channel bit (1) aligned,
       2 system_control_dir character (168) varying,
       2 time_of_bootload fixed bin (71),		/* time system was booted */
       2 first_reasonable_time fixed bin (71),		/* Used to check "reasonableness" of times */
       2 last_reasonable_time fixed bin (71),
       2 system_type fixed bin,			/* ADP_SYSTEM or L68_SYSTEM */
       2 initialization_state fixed bin,		/* current collection of initialization being run */
       2 service_system bit (1) aligned,		/* initialization_state = 4 (service) */
       2 collection_1_phase fixed bin,			/* phase of collection 1 initialization, has value SERVICE_INITIALIZATION after bce runs */
       2 maxlinks fixed bin,				/* maximum number of links searched */
       2 data_management_ringno fixed bin,		/* Ring where Data Management runs */
       2 seg_size_256K fixed bin (19),			/* like max_seg_size, but the WHOLE thing (in words) */
       2 default_256K_enable fixed bin,			/* non-zero if default is on */
       2 hfp_exponent_available bit (1) aligned,		/* whether hex-floating point is available on this system. */
       2 all_valid_ips_mask bit (35) aligned,		/* a mask containing a "1" for all valid ips bits; masked against intended ips words */
       2 highest_ips_index bit (0) aligned,
       2 ips_mask_data fixed bin,			/* start of IPS info -- first word is number */
       2 quit_name char (32) aligned,
       2 quit_mask bit (35) aligned,
       2 cput_name char (32) aligned,
       2 cput_mask bit (35) aligned,
       2 alrm_name char (32) aligned,
       2 alrm_mask bit (35) aligned,
       2 neti_name char (32) aligned,
       2 neti_mask bit (35) aligned,
       2 susp_name char (32) aligned,
       2 susp_mask bit (35) aligned,
       2 term_name char (32) aligned,
       2 term_mask bit (35) aligned,
       2 wkp_name char (32) aligned,
       2 wkp_mask bit (35) aligned,
       2 pgt_name char (32) aligned,
       2 pgt_mask bit (36) aligned,
       2 system_shutdown_scheduled_name char (32) aligned,
       2 system_shutdown_scheduled_mask bit (36) aligned,
       2 dm_shutdown_scheduled_name char (32) aligned,
       2 dm_shutdown_scheduled_mask bit (36) aligned,
       2 system_message_name char (32) aligned,
       2 system_message_mask bit (36) aligned,
       2 ipc_privilege bit (36) aligned,
       2 dir_privilege bit (36) aligned,
       2 seg_privilege bit (36) aligned,
       2 soos_privilege bit (36) aligned,
       2 ring1_privilege bit (36) aligned,
       2 rcp_privilege bit (36) aligned,
       2 comm_privilege bit (36) aligned;
%page;
dcl  i fixed bin;
dcl  1 cdsa aligned like cds_args;
dcl  code fixed bin (35);

dcl  double_fixed fixed bin (71) based;

dcl  sys_infoname char (8) static init ("sys_info") options (constant);
dcl  exclude_pad (1) char (32) aligned static options (constant) init ("pad*");

dcl  (addr, bin, mod, null, rel, size, string, unspec) builtin;

dcl  com_err_ entry options (variable);
dcl  convert_date_to_binary_ entry (char (*), fixed bin (71), fixed bin (35));
dcl  create_data_segment_ entry (pointer, fixed bin (35));
%page;
/* Now begins the initialization */

	unspec (sys_info) = ""b;			/* Fill it with zeros */

	sys_info.time_zone = "none";			/* default time zone is invalid */
	addr (sys_info.time_correction_constant) -> double_fixed = 5 * 3600 * 1000000;
						/* NOTE KLUDGE: must be done to preserve time_delta segdef
						   _i_n _t_h_e _m_i_d_d_l_e of the variable time_correction_constant */
	sys_info.page_size = 1024;
	sys_info.max_seg_size = 255 * 1024;
	sys_info.default_max_length = 255 * 1024;
	sys_info.seg_size_256K = 256 * 1024;
	sys_info.default_256K_enable = 0;
	sys_info.default_dir_max_length = 205 * 1024;
	sys_info.default_stack_length = 64 * 1024;
	sys_info.maxlinks = 10;
	sys_info.data_management_ringno = 2;
	sys_info.access_class_ceiling.categories = (18)"1"b || (18)"0"b;
	sys_info.access_class_ceiling.level = 7;
	sys_info.successful_access_threshold = sys_info.access_class_ceiling;
	sys_info.unsuccessful_access_threshold = sys_info.access_class_ceiling;
	sys_info.covert_channel_threshold = sys_info.access_class_ceiling;

	sys_info.system_control_dir = ">system_control_dir";

	sys_info.initialization_state = 0;		/* Begins with bootloading */

	sys_info.system_type = L68_SYSTEM;		/* A sensible default, for now. */

	call set_time ("02/24/73 14:00 est Saturday", sys_info.first_reasonable_time);
						/* The invention of NSS, roughly */
	call set_time ("09/10/2040 0400.", sys_info.last_reasonable_time);


	sys_info.hfp_exponent_available = "0"b;		/* we're all L68s on this bus */
	sys_info.ips_mask_data = 11;			/* number of known IPS events */
	sys_info.quit_name = "quit";
	sys_info.quit_mask = "1000"b;
	sys_info.cput_name = "cput";
	sys_info.cput_mask = "0100"b;
	sys_info.alrm_name = "alrm";
	sys_info.alrm_mask = "0010"b;
	sys_info.neti_name = "neti";
	sys_info.neti_mask = "0001"b;
	sys_info.susp_name = "sus_";
	sys_info.susp_mask = "00001"b;
	sys_info.term_name = "trm_";
	sys_info.term_mask = "000001"b;
	sys_info.wkp_name = "wkp_";
	sys_info.wkp_mask = "0000001"b;
	sys_info.pgt_name = "pgt_";
	sys_info.pgt_mask = "00000001"b;
	sys_info.system_shutdown_scheduled_name = "system_shutdown_scheduled_";
	sys_info.system_shutdown_scheduled_mask = "000000001"b;
	sys_info.dm_shutdown_scheduled_name = "dm_shutdown_scheduled_";
	sys_info.dm_shutdown_scheduled_mask = "0000000001"b;
	sys_info.system_message_name = "system_message_";
	sys_info.system_message_mask = "00000000001"b;
	sys_info.all_valid_ips_mask = "11111111111"b;	/* format: off */
	sys_info.ipc_privilege =     "1"b;
	sys_info.dir_privilege =     "01"b;
	sys_info.seg_privilege =     "001"b;
	sys_info.soos_privilege =    "0001"b;
	sys_info.ring1_privilege =   "00001"b;
	sys_info.rcp_privilege =     "000001"b;
	sys_info.comm_privilege =    "0000001"b;
/* format:  on */

/* Now make some checks on alignment of certain variables */

	call check (addr (sys_info.successful_access_threshold), "successful_access_threshold", 2);
	call check (addr (sys_info.time_correction_constant), "time_correction_constant", 2);
	call check (addr (sys_info.time_of_bootload), "time_of_bootload", 2);
	call check (addr (sys_info.first_reasonable_time), "first_reasonable_time", 2);
	call check (addr (sys_info.last_reasonable_time), "last_reasonable_time", 2);

/* Now set up call to create data base */

	cdsa.sections (1).p = addr (sys_info);
	cdsa.sections (1).len = size (sys_info);
	cdsa.sections (1).struct_name = sys_infoname;

	cdsa.seg_name = sys_infoname;
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (exclude_pad);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	if code ^= 0 then call com_err_ (code, sys_infoname);

GIVE_IT_UP:
	return;
%page;
check:
     proc (where, message, modulo);

dcl  where ptr;
dcl  message char (*);
dcl  modulo fixed bin;

	if mod ((binary (rel (where), 18) - binary (rel (addr (sys_info)), 18)), modulo) ^= 0 then do;
	     call com_err_ (0, sys_infoname, "The variable ^a is not aligned on a ^d-word boundary.", message, modulo);
	     goto GIVE_IT_UP;
	     end;

     end check;



set_time:
     proc (P_string, P_time);

dcl  (
     P_string char (*),
     P_time fixed bin (71)
     ) parameter;

	call convert_date_to_binary_ (P_string, P_time, code);

	if code ^= 0 then do;
	     call com_err_ (code, sys_infoname, "Cannot convert ^a", P_string);
	     goto GIVE_IT_UP;
	     end;

	return;
     end set_time;
%page;
%include cds_args;
%page;
%include aim_template;
%page;
%include system_types;

     end sys_info;
                                    