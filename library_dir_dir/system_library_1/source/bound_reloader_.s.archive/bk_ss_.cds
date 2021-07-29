/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
/* Hierarchy dumper/reloader subsystem static data */

/* Created:  June 1982 by G. Palter from ALM source with modifications for true AIM support in IMFT */
/* Modified: August 1983 by Robert Coren to add minimum_access_class */
/* Modified: November 1983 by Robert Coren to add upgrade_to_user_auth */


/* HISTORY COMMENTS:
  1) change(1987-03-03,GWMay), approve(1987-03-03,MCR7627),
     audit(1987-03-13,Farley), install(1987-03-30,MR12.1-1018):
     added a switch, writing_map, to indicate when the dump map is
     being written.
  2) change(2018-08-21,Swenson), approve(2018-08-21,MCR10048),
     audit(2018-08-22,GDixon), install(2018-08-27,MR12.6g-0015):
     Added support for volume pools to hierarchy backup commands.
                                                   END HISTORY COMMENTS */


/* format: style4,delnl,insnl,ifthenstmt,ifthen */


bk_ss_:
     procedure () options (variable);


dcl  1 bk_static aligned,				/* static data */
       2 allow_dir_overwrite bit (1),			/* ON => allows reloaded segment to replace a directory */
       2 areap pointer,				/* -> directory list */
       2 brief_mapsw bit (1),				/* ON => suppress form-feeds in maps */
       2 caller_handles_conditions bit (1),		/* ON => caller traps errors for us */
       2 control_name character (168) unaligned,		/* dump control segment pathname */
       2 control_ptr pointer,				/* -> control structure for subroutine entries */
       2 cross_retrievesw bit (1),			/* ON => cross retrieveing an entry */
       2 data_iocb pointer,				/* -> I/O switch if preattached by caller */
       2 date fixed binary (52),			/* dump anything modified after this date */
       2 datesw bit (1),				/* ON => dump/reload by date/time modified */
       2 debugsw bit (1),				/* ON => do not use privileged entries */
       2 dir_trim bit (1),				/* ON => delete directories when trimming on reload */
       2 dprint_destination character (24) unaligned,	/* destination for dprinting maps */
       2 dprint_destination_setsw bit (1),		/* ON => destination (above) is set */
       2 dprint_heading character (64) unaligned,		/* heading for dprinting maps */
       2 dprint_heading_setsw bit (1),			/* ON => heading is set */
       2 dprint_queue fixed binary,			/* queue for dprint requests */
       2 dprint_request_type character (24) unaligned,	/* request type for dprint requests */
       2 dprint_request_type_setsw bit (1),		/* ON => request type is set */
       2 dprintsw bit (1),				/* ON => dprint maps */
       2 dtdsw bit (1),				/* ON => dump by date/time dumped */
       2 ename character (32),			/* name of branch to dump */
       2 err_label label variable,			/* error recovery label */
       2 err_onlinesw bit (1),			/* ON => print errors online */
       2 error fixed binary,				/* indicates error recovery method */
       2 holdsw bit (1),				/* ON => do not unmount tape when done */
       2 hp pointer,				/* -> preamble segment */
       2 ignore_dates bit (1),			/* ON => reload without checking dates */
       2 mapsw bit (1),				/* ON => produce a map */
       2 myname character (16) unaligned,		/* name of module invoked (backup_dump, reload, ...) */
       2 namesw bit (1),				/* ON => dump only the named branch */
       2 no_contin bit (1),				/* ON => stop dumping after a catchup dump */
       2 no_output bit (1),				/* ON => do not actually make the tape */
       2 no_primary bit (1),				/* ON => do not use primary pathnames */
       2 no_reload bit (1),				/* ON => do not reload anything into storage system */
       2 no_setlvid bit (1),				/* ON => do not set sons LVID when reloading */
       2 ntapes fixed binary,				/* # of tapes to make (1 or 2) */
       2 onlysw bit (1),				/* ON => do not dump subtree contents or stop on first match
						   on reload */
       2 operator character (32) unaligned,		/* name of operator running dump/reload */
       2 path_index fixed binary,			/* index in backup_control structure being processed */
       2 pathsw bit (1),				/* ON => have starting directory for a dump */
       2 preattached bit (1),				/* ON => use caller's I/O switch for I/O */
       2 pvsw bit (1),				/* ON => reload only for given physical volume */
       2 pvname character (32),			/* name of volume for reload */
       2 qchecksw bit (1),				/* ON => do not suspend quota checking */
       2 quotasw bit (1),				/* ON => restore quota from tape */
       2 restart_dumpsw bit (1),			/* ON => restart dump from given directory */
       2 restart_path character (168),			/* pathname of where to restart dump */
       2 restart_plen fixed binary,			/* length of said pathname */
       2 retrieval_index fixed binary,			/* index of object in bk_retrieve's table */
       2 retrievesw bit (1),				/* ON => retrieval vs. reload */
       2 rlen fixed binary,				/* length of current restart dirname */
       2 rname character (168) unaligned,		/* pathname of retrieval control segment */
       2 rsize fixed binary,				/* size of above pathname */
       2 save_path character (168) unaligned,		/* saved pathname (for subtree dumping) */
       2 save_plen fixed binary,			/* length of saved pathname */
       2 save_time fixed binary (52),			/* starting time of dump/reload */
       2 segptr pointer,				/* -> segment being dumper */
       2 set_dtd bit (1),				/* ON => set date/time dumped */
       2 set_dtd_explicit bit (1),			/* ON => above was set by user/operator */
       2 sp pointer,				/* -> output buffer */
       2 sub_entry bit (1),				/* ON => backup_dump_/backup_load_ */
       2 sub_entry_errfile bit (1),			/* ON => produce error file even when subroutine call */
       2 tapesw bit (1),				/* ON => produce output */
       2 trimsw bit (1),				/* ON => trim contents when reloading */
       2 volume_set_name character (32) unaligned,	/* tape volume name (unimplemented) */
       2 wakeup_interval fixed binary (52),		/* wakeup interval for incremental dumps */
       2 wasnt_known bit (1),				/* ON => must terminate segment being dumped */
       2 enforce_max_access_class bit (1),		/* ON => don't dump anything above give access class */
       2 maximum_access_class bit (72),			/* the maximum access class to enforce on all branches */
       2 enforce_min_access_class bit (1),		/* ON => don't dump anything below give access class */
       2 minimum_access_class bit (72),			/* the minimum access class to enforce on all branches */
       2 dont_dump_upgraded_dirs bit (1),		/* ON => don't dump any directory above given access class */
       2 maximum_dir_access_class bit (72),		/* the access class to enforce on directories */
       2 check_effective_access bit (1),		/* ON => don't dump branches given user can't access */
       2 upgrade_to_user_auth bit (1),			/* ON => set access class of branch being dumped to user's authorization */
       2 user_id character (32) unaligned,		/* the user's Person.Project.tag */
       2 user_authorization bit (72),			/* the user's process authorization */
       2 user_ring fixed binary,			/* the user's ring of execution */
       2 restore_access_class bit (1),			/* ON => restore access class even when debugging */
       2 enforce_minimum_ring bit (1),			/* ON => don't give anything lower ring bracket */
       2 minimum_ring fixed binary,			/* the minimum ring bracket to be used */
       2 translate_access_class bit (1),		/* ON => translate access classes read from tape */
       2 source_attributes_ptr pointer,			/* -> source system's AIM attributes */
       2 target_attributes_ptr pointer,			/* -> target system's AIM attributes */
       2 writing_map bit (1),				/* ON => in backup_map$try_write */
       2 volume_pool_ptr ptr;                               /* -> volume pool, if null, prompt for tapes */

dcl  1 err_label_structure aligned based (addr (bk_static.err_label)),
       2 codeptr pointer,
       2 environmentptr pointer;

dcl  1 cds_control automatic aligned like cds_args;

dcl  code fixed binary (35);

dcl  BK_SS_ character (32) static options (constant) initial ("bk_ss_");

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (pointer, fixed binary (35));

dcl  (addr, currentsize, null, string, unspec) builtin;
%page;
/* Supply initial values for static data */

	unspec (bk_static) = ""b;

	err_label_structure = null ();
	bk_static.areap = null ();
	bk_static.hp = null ();
	bk_static.segptr = null ();
	bk_static.sp = null ();
	bk_static.control_ptr = null ();
	bk_static.data_iocb = null ();
	bk_static.date = 0;
	bk_static.save_time = 0;
	bk_static.wakeup_interval = 0;
	bk_static.cross_retrievesw = "0"b;
	bk_static.allow_dir_overwrite = "0"b;
	bk_static.control_name = "";
	bk_static.datesw = "0"b;
	bk_static.debugsw = "0"b;
	bk_static.dir_trim = "0"b;
	bk_static.dprint_destination = "";
	bk_static.dprint_destination_setsw = "0"b;
	bk_static.dprint_heading = "";
	bk_static.dprint_heading_setsw = "0"b;
	bk_static.dprint_queue = -1;
	bk_static.dprint_request_type = "";
	bk_static.dprint_request_type_setsw = "0"b;
	bk_static.dprintsw = "1"b;
	bk_static.dtdsw = "0"b;
	bk_static.ename = "";
	bk_static.err_onlinesw = "0"b;
	bk_static.error = 0;
	bk_static.holdsw = "0"b;
	bk_static.ignore_dates = "0"b;
	bk_static.mapsw = "1"b;
	bk_static.brief_mapsw = "0"b;
	bk_static.myname = "";
	bk_static.namesw = "0"b;
	bk_static.no_contin = "0"b;
	bk_static.no_output = "0"b;
	bk_static.no_primary = "0"b;
	bk_static.no_reload = "0"b;
	bk_static.no_setlvid = "0"b;
	bk_static.ntapes = 1;
	bk_static.onlysw = "0"b;
	bk_static.operator = "";
	bk_static.path_index = 1;
	bk_static.pathsw = "0"b;
	bk_static.pvsw = "0"b;
	bk_static.pvname = "";
	bk_static.qchecksw = "0"b;
	bk_static.quotasw = "0"b;
	bk_static.rlen = 0;
	bk_static.restart_dumpsw = "0"b;
	bk_static.restart_plen = 0;
	bk_static.restart_path = "";
	bk_static.retrieval_index = 1;
	bk_static.retrievesw = "0"b;
	bk_static.rsize = 0;
	bk_static.rname = "";
	bk_static.save_plen = 0;
	bk_static.save_path = "";
	bk_static.set_dtd = "0"b;
	bk_static.set_dtd_explicit = "0"b;
	bk_static.sub_entry = "0"b;
	bk_static.sub_entry_errfile = "0"b;
	bk_static.caller_handles_conditions = "0"b;
	bk_static.tapesw = "1"b;
	bk_static.trimsw = "0"b;
	bk_static.volume_set_name = "";
	bk_static.wasnt_known = "0"b;
	bk_static.preattached = "0"b;
	bk_static.enforce_max_access_class = "0"b;
	bk_static.maximum_access_class = ""b;
	bk_static.enforce_min_access_class = "0"b;
	bk_static.minimum_access_class = ""b;
	bk_static.dont_dump_upgraded_dirs = "0"b;
	bk_static.maximum_dir_access_class = ""b;
	bk_static.check_effective_access = "0"b;
	bk_static.upgrade_to_user_auth = "0"b;
	bk_static.user_id = "";
	bk_static.user_authorization = ""b;
	bk_static.user_ring = 0;
	bk_static.restore_access_class = "0"b;
	bk_static.enforce_minimum_ring = "0"b;
	bk_static.minimum_ring = 0;
	bk_static.translate_access_class = "0"b;
	bk_static.source_attributes_ptr = null ();
	bk_static.target_attributes_ptr = null ();
	bk_static.volume_pool_ptr = null ();

/* Fill in CDS description and create the data segment */

	cds_control.sections (1).p = null ();
	cds_control.sections (1).len = 0;
	cds_control.sections (1).struct_name = "";

	cds_control.sections (2).p = addr (bk_static);
	cds_control.sections (2).len = currentsize (bk_static);
	cds_control.sections (2).struct_name = "bk_static";

	cds_control.seg_name = BK_SS_;
	cds_control.num_exclude_names = 0;
	cds_control.exclude_array_ptr = null ();

	string (cds_control.switches) = ""b;
	cds_control.have_static = "1"b;

	call create_data_segment_ (addr (cds_control), code);
	if code ^= 0 then call com_err_ (code, BK_SS_);

	return;
%page;
%include cds_args;

     end bk_ss_;
