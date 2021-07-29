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

						/* format: off */

/* Static and constant data used by the Multics command processor */

/* Created:  January 1981 by Ellie Donner */
/* Modified: June 1982 by G. Palter to add standard command language definition */
/* Modified: July 1984 by G. Palter to add the permanent scratch segment list */
/* Modified: March 1985 by Keith Loepere so that it can reside within bound_multics_bce_. */

/* format: on,style4,delnl,insnl,ifthenstmt,ifthen */
%page;

cp_data_:
     procedure () options (variable);

dcl  1 cp_constants aligned,				/* constant data: see cp_data_.incl.pl1 for details */
       2 standard_language (0:511) fixed binary (9) unaligned unsigned;

dcl  1 cp_static aligned,				/* static data: see cp_data_.incl.pl1 for details */
       2 command_table_ptr pointer,
       2 under_lss bit (1) aligned,
       2 scratch_lock_id fixed binary (35),
       2 scratch_release_factor fixed binary,
       2 permanent_scratch_segment_list,
         3 n_scratch_segments fixed binary,
         3 scratch_segments (4),
	 4 segment_ptr pointer,
	 4 lock bit (36) aligned,
	 4 usage_count fixed binary;

dcl  1 cds_arguments aligned like cds_args;

dcl  code fixed binary (35);

dcl  CP_DATA_ character (8) static options (constant) initial ("cp_data_");

/* format: off */
dcl (SPACE	initial (" "),
     HT		initial ("	"),		/* horizontal tab */
     VT		initial (""),			/* veritcal tab */
     FF		initial (""),						/* form feed */
     NL		initial ("
"))						/* new line */
	character (1) static options (constant);
/* format: on */

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (pointer, fixed binary (35));

dcl  (addr, currentsize, null, rank, string) builtin;
%page;

/* Define the standard command language */

	cp_constants.standard_language (*) = NORMAL_CHARACTER;
						/* start out by declaring all characters to be normal */

	cp_constants.standard_language (rank (SPACE)) = WHITESPACE;
	cp_constants.standard_language (rank (HT)) = WHITESPACE;
	cp_constants.standard_language (rank (VT)) = WHITESPACE;
	cp_constants.standard_language (rank (FF)) = WHITESPACE;

	cp_constants.standard_language (rank (";")) = COMMAND_SEPARATOR;

	cp_constants.standard_language (rank (NL)) = COMMAND_SEPARATOR_OR_WHITESPACE;

	cp_constants.standard_language (rank ("""")) = QUOTE_CHARACTER;

	cp_constants.standard_language (rank ("(")) = BEGIN_ITERATION_1;
	cp_constants.standard_language (rank (")")) = END_ITERATION_1;

	cp_constants.standard_language (rank ("[")) = BEGIN_ACTIVE_STRING_1;
	cp_constants.standard_language (rank ("]")) = END_ACTIVE_STRING_1;
	cp_constants.standard_language (rank ("|")) = ACTIVE_STRING_MODIFIER;


/* Setup constants and static data related to scratch segment management */

	cp_static.scratch_release_factor = 1000;	/* release "permanement" scratch segments every 1000 uses */

	cp_static.scratch_lock_id = 0;

	cp_static.permanent_scratch_segment_list.n_scratch_segments = 4;
	cp_static.permanent_scratch_segment_list.scratch_segments (*).segment_ptr = null ();
	cp_static.permanent_scratch_segment_list.scratch_segments (*).lock = ""b;
	cp_static.permanent_scratch_segment_list.scratch_segments (*).usage_count = 0;


/* Supply initial values for the remaining static data */

	cp_static.under_lss = "0"b;			/* no restriction on commands that may be executed */
	cp_static.command_table_ptr = null ();


/* Fill in CDS description and create the data segment */

	cds_arguments.sections (1).p = addr (cp_constants);
	cds_arguments.sections (1).len = currentsize (cp_constants);
	cds_arguments.sections (1).struct_name = "cp_constants";

	cds_arguments.sections (2).p = addr (cp_static);
	cds_arguments.sections (2).len = currentsize (cp_static);
	cds_arguments.sections (2).struct_name = "cp_static";

	cds_arguments.seg_name = CP_DATA_;
	cds_arguments.num_exclude_names = 0;
	cds_arguments.exclude_array_ptr = null ();

	string (cds_arguments.switches) = "0"b;
	cds_arguments.switches.have_text = "1"b;
	cds_arguments.switches.have_static = "1"b;

	cds_arguments.switches.separate_static = "1"b;

	call create_data_segment_ (addr (cds_arguments), code);
	if code ^= 0 then call com_err_ (CP_DATA_, code);

	return;
%page;
%include cp_character_types;
%page;
%include cds_args;

     end;
