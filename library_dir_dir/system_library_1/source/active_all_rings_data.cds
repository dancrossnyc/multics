/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */


active_all_rings_data: procedure;

/* CDS source for the former active_all_rings_data.alm */
/* This data is only accessable out to ring 5. */
/* coded by Benson I. Margulies April 1981. */
/* the data is in the text section, but it gets patched in initialization */
/* Modified November 1984 by Keith Loepere to move pam flush stuff to
   active_hardcore_data. */
/* format: style2 */

%include cds_args;
	declare 1 cdsa		 aligned like cds_args;

	declare com_err_		 entry () options (variable);
	declare create_data_segment_	 entry (ptr, fixed bin (35));

	declare code		 fixed bin (35);
	declare (addr, currentsize, string, unspec)
				 builtin;

	declare 1 aard		 aligned,		/* automatic structure */
		2 system_id	 character (32),	/* sysid from system tape. */
		2 version_id	 character (32),	/* supervisor version, = MIT system # */
		2 initializer_tty	 character (32),	/* initial console attachment name. */
		2 initializer_dim	 character (32),	/* initial console io module */
		2 hcscnt		 fixed bin (35),	/* # of hardcore segments */
		2 default_max_segno	 fixed bin (35),	/* dseg size by default */
		2 max_segno	 fixed bin (35),	/* max Multics can do */
		2 maxlinks	 fixed bin (35),	/* we will not chase past this many */
		2 max_tree_depth	 fixed bin (35),	/* this many greater thans */
		2 stack_base_segno	 fixed bin (35);	/* stack 0 */

	unspec (aard) = ""b;

	aard.max_tree_depth = 15;			/* that many >'s in a pathname */

/* These next two should be filled in by generate_mst, but
   this puts something recognizable in just in case */

	aard.system_id, aard.version_id = "Unknown";

/* otw_ is a magic thing that means to call ocdcm_ through hphcs_
   to share a console with the hardcore. other things would allow
   the use of, say, lcc channels. */

	aard.initializer_tty = "otw_";

/* ocd_ runs a console. FNP channels cannot be used because 
   they are not loaded in ring 1. */

	aard.initializer_dim = "ocd_";

	aard.default_max_segno = 1023;
	aard.max_segno = 4093;
	aard.maxlinks = 10;
	aard.max_tree_depth = 15;

	cdsa.sections (1).p = addr (aard);
	cdsa.sections (1).len = currentsize (aard);
	cdsa.sections (1).struct_name = "aard";
	cdsa.seg_name = "active_all_rings_data";
	cdsa.num_exclude_names = 0;			/* no pad fields to flush */
	string (cdsa.switches) = ""b;
	cdsa.switches.have_text = "1"b;
	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then call com_err_ (code, "active_all_rings_data");
	return;
     end active_all_rings_data;

