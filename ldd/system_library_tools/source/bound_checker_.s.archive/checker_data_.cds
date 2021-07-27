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





/* HISTORY COMMENTS:
  1) change(87-01-13,GDixon), approve(87-04-16,MCR7614),
     audit(87-05-21,Farley), install(87-07-15,MR12.1-1040):
     Add support for storing boot program as first segment of MST image stored
     in a file.
                                                   END HISTORY COMMENTS */


/* CHECKER_DATA_ -- static data for the MST checker subsystem */
/* The bulk of the data is actually kept in a temporary area */
/* found by a pointer here. Needless to say, the checker is */
/* nonrecursive. */
/* Modified December 1983 by Keith Loepere for collections that aren't 
   loaded into segments and other subtleties. */
/* Modified January 1985 by Keith Loepere so that collection 3 isn't
   claimed to take up defintions_, etc. */
/* format: style3,idind30 */

checker_data_:
     proc;

dcl	1 cdsa			like cds_args auto aligned;

dcl	1 checker_stat		aligned auto,
	  2 input_iocbp		ptr,		/* checker input switch */
	  2 output_iocbp		ptr,		/* checker output switch */
	  2 temp_ptrs		bit (0),		/* Look at all the following as an array for g_t_segments_ */
	  2 slt_ptr		bit (0),		/* simulated SLT (sslt) */
	  2 sslt_ptr		ptr init (null ()),
	  2 area_ptr		ptr init (null ()), /* area in which most static data is put */
	  2 name_table_ptr		bit (0),		/* simulated name_table */
	  2 sname_table_ptr		ptr init (null ()),
	  2 buffer_ptr		ptr init (null ()), /* Tape reader buffer */
	  2 meter_ptr		ptr init (null ()), /* big structure of checker meters */
	  2 severity		fixed bin,	/* worst error encountered */
	  2 file_attachment		bit (1) init ("0"b);/* MST is a file. */

dcl	1 checker_text		aligned,
	  2 last_wired_collection	fixed bin init (4), /* make_segs_paged runs here, paging defs, etc. */
	  2 last_text_wired_collection
				fixed bin init (2), /* after this, text is not loaded directly into memory */
	  2 last_supervisor_collection
				fixed bin init (5), /* after this load into >sl1 */
	  2 collection_names	(0:10) aligned,
	    3 major		fixed bin init (0, 0, 1, 1, 1, 2, 3, (4) 0),
	    3 minor		fixed bin init (0, 5, 0, 2, 5, 0, 0, (4) 0),
	  2 loaded		(0:10) bit (1) aligned
				init ("1"b, "0"b, "1"b, "0"b, "1"b, "1"b, "0"b, (4) (1)"0"b),
						/* objects in collection are loaded into segments */
	  2 n_temp_ptrs		fixed bin init (4);

dcl	code			fixed bin (35);
dcl	create_data_segment_	entry (ptr, fixed bin (35));
dcl	com_err_			entry options (variable);

dcl	(addr, size, string)	builtin;

%page;

%include cds_args;

	cdsa.sections (1).p = addr (checker_text);
	cdsa.sections (1).len = size (checker_text);
	cdsa.sections (1).struct_name = "checker_text";

	cdsa.sections (2).p = addr (checker_stat);
	cdsa.sections (2).len = size (checker_stat);
	cdsa.sections (2).struct_name = "checker_stat";

	cdsa.seg_name = "checker_data_";

	cdsa.num_exclude_names = 0;

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_static = "1"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	if code ^= 0
	then call com_err_ (code, "checker_data_");

	return;
     end checker_data_;
