/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(85-09-24,Elhard), approve(85-09-24,MCR7198),
     audit(86-06-30,Weaver), install(86-07-16,MR12.0-1094):
     Changed version number to 12, added bx_$caller and bx_$temp_bsegp
                                                   END HISTORY COMMENTS */

/* BX_ - Static Segment for the Multics Binder.
   Rewritten in CDS 12/16/76 by Noel I. Morris	*/
/* Modified 8/16/77 by Melanie Weaver to add perprocess_static switch */
/* Modified Oct 78 by David Spector to delete temp pointers and count */
/* Modified Dec 78 by David Spector to make repatch table extensible */
/* Version 10.2: 01/15/81, W. Olin Sibert: -force_order, -force_update, bind command warnings */
/* Modified 5/25/82 by Melanie Weaver to increase addname limit */
/* Modified 06/16/83 by Melanie Weaver to handle list template external variable initialization */
/* Modified 11/03/83 by Melanie Weaver to change version for changes made by JMAthane in 1982 */
/* Modified 11/15/84 by M. Sharpe to change version number */

/* ******************************************************
   *                                                    *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   *                                                    *
   ****************************************************** */


/* format: style3,^indnoniterdo */
bx_:
     proc;

dcl	1 cdsa		like cds_args auto aligned;


dcl	Binder_Version_Name char (167) static options (constant)
			init ("Multics Binder, Version 12 of Tuesday, March 26, 1985");
dcl	Binder_Version_Number
			fixed bin static options (constant) init (12);


dcl	1 bx_text		aligned auto,		/* pure portion of bx_ */
	  2 vers_name,				/* ASCII version name, in ACC form */
	    3 lth		fixed bin (8) unal,
	    3 chr		char (167) unal,
	  2 vers_number	fixed bin,		/* integer part of version number */
	  2 size		fixed bin,		/* size of main data base, for resetting */
	  2 snt_limit	fixed bin,		/* preset limit for segname table */
	  2 oddname_limit	fixed bin,		/* preset limit for oddname table */
	  2 stringmap_limit fixed bin,		/* preset limit for stringmap table */
	  2 addname_limit	fixed bin;		/* preset limit for addname table */


dcl	1 bx_link		aligned auto,		/* internal static portion of bx_ */
	  2 area_begin	bit (0) unal,		/* beginning of main data base */
	  2 ctp		pointer,			/* pointer to component table */
	  2 freep		pointer,			/* pointer to beginning of free area */
	  2 isp		pointer,			/* pointer to first insym table */
	  2 inpp		pointer,			/* pointer to binder's input structure */
	  2 bsegp		pointer,			/* pointer to base of new object segment */
	  2 temp_bsegp	ptr,			/* pointer to temporary bound seg (in [pd]) */
	  2 temp		pointer,			/* pointer to threaded temp segments */
	  2 optp		pointer,			/* pointer to options table */
	  2 odnp		pointer,			/* pointer to oddname table */
	  2 first_rptp	pointer,			/* pointer to first chunk of repatch table */
	  2 last_rptp	pointer,			/* pointer to current chunk of repatch table */
	  2 adnp		pointer,			/* pointer to addname table */
	  2 bindmap_def	pointer,			/* pointer to new object's "bind_map" definition */
	  2 bdefp		pointer,			/* pointer to new object's definition section */
	  2 bstatp	pointer,			/* pointer to new object's static section */
	  2 blnkp		pointer,			/* pointer to new object's linkage section */
	  2 bsymp		pointer,			/* pointer to new object's symbol section */
	  2 sntp		pointer,			/* pointer to segname table */
	  2 tdefp		pointer,			/* pointer to temporary new definition section */
	  2 tintp		pointer,			/* pointer to temporary new internal static */
	  2 tlinkp	pointer,			/* pointer to temporary new linkage section */
	  2 strmp		pointer,			/*  pointer to stringmap table */
	  2 n_firstrefs	fixed bin,		/* ptr to comp tbl for seg with frt */
	  2 bound_segname	char (32) aligned,		/* name of new bound object */
	  2 caller	char (32) aligned,		/* name of calling program */
	  2 fatal_error	fixed bin,		/* 1 -> fatal error was detected */
	  2 bseg_acinfop	pointer,			/* new object's acinfop for "tssi_" */
	  2 bseg_bitcount	fixed bin (24),		/* new object's bitcount */
	  2 o_lng		fixed bin (19),		/* length of new bound object */
	  2 t_lng		fixed bin (18),		/* length of new text section */
	  2 d_lng		fixed bin (18),		/* length of new definition section */
	  2 i_lng		fixed bin,		/* length of new static section */
	  2 l_lng		fixed bin,		/* length of new linkage section */
	  2 s_lng		fixed bin (18),		/* length of new symbol section */
	  2 addname	fixed bin,		/* 1 -> addname option specified */
	  2 debug		fixed bin,		/* 1 -> debug option was specified */
	  2 brief		fixed bin,		/* 1 -> brief option was specified */
	  2 force_order	fixed bin,		/* 1 -> -force_order specified on command line */
	  2 has_sep_stat	fixed bin,		/* 1 -> a comp has nonzero sep static */
	  2 has_comb_stat	fixed bin,		/* 1 -> a comp has nonzero compined static */
	  2 bound_sep_stat	fixed bin,		/* 1 -> bound segment has separate static */
	  2 perprocess_static
			fixed bin,		/* 1 -> bound seg has perprocess static switch on */
	  2 standard	fixed bin,		/* 1 -> bound seg is in standard format */
	  2 bproc		fixed bin,		/* 1 -> at least one component is a procedure */
	  2 textlng	fixed bin (18),		/* length of new pure text portion */
	  2 curdeflng	fixed bin (18),		/* current length of new definition section */
	  2 tintlng	fixed bin,		/* current length of new internal static */
	  2 maxlinklng	fixed bin,		/* maximum size linkage section may attain */
	  2 maxdeflng	fixed bin (18),		/* maximum size definitions section may attain */
	  2 tlinklng	fixed bin,		/* current size of linkage section */
	  2 ncomp		fixed bin,		/* number of component objects to be bound */
	  2 v_lng		fixed bin,		/* length of version name string */
	  2 n_lng		fixed bin,		/* length of bound segment name string */
	  2 nsymdefs	fixed bin,		/* count of non-null symbol definitions */
	  2 nsegdefs	fixed bin;		/* count of non-null segment name definitions */


dcl	code		fixed bin (35),
	create_data_segment_
			entry (ptr, fixed bin (35)),
	com_err_		entry options (variable);

dcl	(addr, length, null, size, string, unspec)
			builtin;

%page;

	unspec (bx_text) = "0"b;			/* Clear data bases. */
	unspec (bx_link) = "0"b;

	vers_name.lth = length (Binder_Version_Name);
	vers_name.chr = Binder_Version_Name;
	vers_number = Binder_Version_Number;
	bx_text.size = size (bx_link);
	snt_limit = 900;
	oddname_limit = 256;
	stringmap_limit = 2048;
	addname_limit = 250;

	ctp, freep, isp, inpp, bsegp, temp_bsegp, optp, odnp, first_rptp, last_rptp, adnp, bindmap_def, bdefp, bstatp,
	     blnkp, bsymp, sntp, tdefp, tintp, tlinkp, strmp, bseg_acinfop = null ();

	n_firstrefs = 0;


	cdsa.sections (1).p = addr (bx_text);
	cdsa.sections (1).len = size (bx_text);
	cdsa.sections (1).struct_name = "bx_text";

	cdsa.sections (2).p = addr (bx_link);
	cdsa.sections (2).len = size (bx_link);
	cdsa.sections (2).struct_name = "bx_link";

	cdsa.seg_name = "bx_";

	cdsa.num_exclude_names = 0;

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;
	cdsa.switches.have_static = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	if code ^= 0
	then call com_err_ (code, "bx_", "");

	return;

%page;
%include cds_args;

     end bx_;
