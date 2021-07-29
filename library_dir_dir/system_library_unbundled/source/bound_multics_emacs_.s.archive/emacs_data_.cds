/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1981 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */
/* format: style3 */

/* emacs_data_: Emacs static data. */


/* HISTORY COMMENTS:
  1) change(1981-07-07,Soley), approve(), audit(), install():
     Written.
  2) change(1981-07-22,Soley), approve(), audit(), install():
     add argument flags.
  3) change(1982-04-12,Soley), approve(), audit(), install():
     help punt site-dir.
  4) change(1986-02-24,Margolin), approve(1986-02-24,MCR7325),
     audit(1986-08-12,Harvey), install(1986-08-20,MR12.0-1136):
     Changed version to 12.6.
  5) change(1986-09-20,Margolin), approve(1986-10-10,MCR7553),
     audit(1986-10-17,Lippard), install(1986-11-11,MR12.0-1209):
     Changed version to 12.6e.
  6) change(1987-01-30,Margolin), approve(1987-01-30,MCR7607),
     audit(1987-02-13,RBarstad), install(1987-03-25,MR12.1-1014):
     Changed version to 12.7.
  7) change(1988-01-19,Schroth), approve(1988-02-29,MCR7852),
     audit(1988-06-06,RBarstad), install(1988-08-01,MR12.2-1071):
     Changed version to 12.9.
  8) change(2018-07-30,Swenson), approve(2018-07-30,MCR10051),
     audit(2018-08-01,GDixon), install(2018-08-17,MR12.6g-0014):
     Updated version number to 12.10.
                                                   END HISTORY COMMENTS */


emacs_data_:
     procedure ();

/* Automatic */
declare	code		fixed binary (35);
declare	me		character (32) initial ("emacs_data_");
declare	1 cdsa		aligned like cds_args;

/* Builtin */
declare	(addr, null, size, unspec)
			builtin;

/* Entries */
declare	com_err_		entry options (variable);
declare	create_data_segment_
			entry (pointer, fixed binary (35));

/* Include Files */
%include cds_args;

/* Automatic to become the emacs_data_ variables. */

declare	1 text_data	aligned,
	  2 version	character (10) initial ("12.10"),
	  2 log_dir	character (168) initial ("");
declare	1 static_data	aligned,
	  2 force_tasking	bit (1) aligned initial ("0"b),
	  2 invocation_list pointer initial (null ()),
	  2 status_code	fixed bin (35) initial (0);

/* Fill in CDS data. */
	unspec (cdsa) = ""b;
	cdsa.sections (1).p = addr (text_data);
	cdsa.sections (1).len = size (text_data);
	cdsa.sections (1).struct_name = "text_data";
	cdsa.sections (2).p = addr (static_data);
	cdsa.sections (2).len = size (static_data);
	cdsa.sections (2).struct_name = "static_data";
	cdsa.seg_name = "emacs_data_";
	cdsa.exclude_array_ptr = null ();
	cdsa.switches.have_text = "1"b;
	cdsa.switches.separate_static = "1"b;
	cdsa.switches.have_static = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then do;
		call com_err_ (code, me);
		return;
	     end;

	return;
     end emacs_data_;
