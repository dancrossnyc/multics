/* ***************************************************************
   *                                                             *
   * Copyright, (C) BULL HN Information Systems Inc., 1992       *
   *                                                             *
   * Copyright, (C) Massachusetts Institute of Technology, 1986  *
   *                                                             *
   * Copyright (c) 1982 by Massachusetts Institute of Technology *
   *                                                             *
   *************************************************************** */




/* HISTORY COMMENTS:
  1) change(86-07-29,Pattin), approve(86-07-29,MCR7354),
     audit(86-08-03,Margolin), install(86-08-16,MR12.0-1128):
     Added meeting_list, log_dir -> log_dest
  2) change(92-09-17,Zimmerman), approve(92-09-17,MCR8258),
     audit(92-09-22,WAAnderson), install(92-09-28,MR12.5-1020):
     Change version number for release 12.5.
  3) change(92-10-07,Vu), approve(92-10-07,MCR8273), audit(92-10-08,Zimmerman),
     install(92-10-19,MR12.5-1027):
     Change version number to 2.16 for release MR12.5
                                                   END HISTORY COMMENTS */


forum_data_:
     procedure ();

/* This procedure creates the segment 'forum_data_' which contains
   static information used by the forum subsystem.

   Initial coding:	800310 by M. Auerbach
   added version_string for ssu_ 08/21/81 Jay Pattin
   rewritten for forum 01/21/82 Jay Pattin
   lobotomized because installers can't answer questions  4/2/82 Jay Pattin
   Added message switches 5/19/82 Jay Pattin
   Added meetings_directory 6/4/83 Jay Pattin
   Added forum_ring 12/30/83 - Jeffrey I. Schiller
   Added log_dir 07/22/84 - Jeffrey I. Schiller */

dcl 1 forum_external_data aligned,
    2 major_version fixed binary,
    2 minor_version fixed binary,
    2 version_string char (8),
    2 print_eligibility_messages bit (1) aligned,
    2 chairman_override bit (1) aligned,
    2 info_directory character (168),
    2 central_directory char (168),
    2 meetings_directory char (32),			/* default for announce_meeting */
    2 forum_ring fixed bin (3),
    2 log_destination char (168);

dcl 1 forum_static_data aligned,
    2 meeting_list pointer;

dcl 1 cds_data aligned like cds_args;			/* arguments to create_data_segment_ subr */

dcl  code fixed binary (35);

dcl  NAME character (32) static options (constant) initial ("forum_data_");

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (pointer, fixed binary (35));
dcl  ioa_ entry () options (variable);
dcl  ioa_$rsnnl entry () options (variable);

dcl (addr, currentsize, null, string) builtin;
%page;
%include cds_args;
%page;
/* Fill in the structure */

	forum_external_data.major_version = 2;		/* canned version for installation */
	forum_external_data.minor_version = 16;
          forum_external_data.forum_ring = 2;		/* Ring of forums */
	forum_external_data.print_eligibility_messages = "0"b;
	forum_external_data.chairman_override = "1"b;
	forum_external_data.central_directory = ">site>forum_dir";
	forum_external_data.info_directory = ">doc>subsystem>forum";
	forum_external_data.meetings_directory = "Meetings_Directory.forum";

	/* Shipped version should have log_dest set to "". If it contains
	   a user-name, that user will be sent error messages from
	   forum_logger_. */

	forum_external_data.log_destination = "";

	forum_static_data.meeting_list = null ();

	call ioa_$rsnnl ("^d.^d", forum_external_data.version_string, (0), forum_external_data.major_version,
	     forum_external_data.minor_version);
	call ioa_ ("Generating forum_data_ version ^a", forum_external_data.version_string);
	if forum_external_data.log_destination ^= "" then
	     call ioa_ ("WARNING: Forum error log will be sent to ^a.",
	          forum_external_data.log_destination);

/* Set up arguments for call to create_data_segment_ */

	cds_data.sections (1).p = addr (forum_external_data);
	cds_data.sections (1).len = currentsize (forum_external_data);
	cds_data.sections (1).struct_name = "forum_external_data";

	cds_data.sections (2).p = addr (forum_static_data);
	cds_data.sections (2).len = currentsize (forum_static_data);
	cds_data.sections (2).struct_name = "forum_static_data";

	cds_data.seg_name = NAME;

	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null ();

	string (cds_data.switches) = "0"b;
	cds_data.switches.have_text = "1"b;
	cds_data.switches.have_static = "1"b;

/* Call create_data_segment_ */

	call create_data_segment_ (addr (cds_data), code);

	if code ^= 0 then
	     call com_err_ (code, NAME);

	return;

     end forum_data_;
