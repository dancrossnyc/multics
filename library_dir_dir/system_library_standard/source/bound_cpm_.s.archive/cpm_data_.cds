/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1985 *
   *                                                         *
   *********************************************************** */

/* format: off */

/* Control Point Manager constants and static data */

/* HISTORY COMMENTS:
  1) change(86-08-12,Kissel), approve(86-08-12,MCR7473),
     audit(86-10-20,Fawcett), install(86-11-03,MR12.0-1206):
     Written to support control point management in March 1985 by G. Palter
     based on C. Hornig's task_init_.
                                                   END HISTORY COMMENTS */

/* format: style3,linecom */

cpm_data_:
     procedure () options (variable);

dcl	1 cds_arguments	like cds_args aligned;
dcl	code		fixed binary (35);

dcl	CPM_DATA_		character (32) static options (constant) initial ("cpm_data_");

dcl	com_err_		entry () options (variable);
dcl	create_data_segment_
			entry (pointer, fixed binary (35));

dcl	(addr, currentsize, null, string, unspec)
			builtin;


/* Constants (see cpm_internal_data.incl.pl1 for explanations) */

dcl	1 cpm_constants	aligned,
	  2 subsystem_name	character (32) unaligned;


/* Static data (see cpm_internal_data.incl.pl1 for explanations) */

dcl	1 cpm_static	aligned,
	  2 root_control_point_data
			like control_point_data aligned,
	  2 n_control_points
			fixed binary,
	  2 gc_control_points
			bit (1) aligned,
	  2 saved_cl_intermediary
			entry (bit (36) aligned) variable,
	  2 preferred_control_point
			pointer,
	  2 preferred_control_point_stack
			aligned,
	    3 stack_depth	fixed binary,
	    3 pad		bit (36) aligned,
	    3 cpd_ptr_stack (16) pointer,
	  2 ready_queue	aligned,
	    3 first	pointer,
	    3 last	pointer,
	  2 previous_control_point
			pointer,
	  2 valid_control_points
			aligned,
	    3 map		(0:4095) bit (1) unaligned,
	  2 global_meters	aligned,
	    3 overhead	like control_point_data.meters,
	    3 last_meters	like control_point_data.meters;
%page;
/* Define the constants */

	cpm_constants.subsystem_name = "Control point manager";


/* Initialize the static data as best as possible */

	unspec (cpm_static) = ""b;			/* cpm_initialize_ will actually set this stuff up */
	cpm_static.n_control_points = 1;		/* ... except this value should always be valid */


/* Invoke create_data_segment_ */

	cds_arguments.sections (1).p = addr (cpm_constants);
	cds_arguments.sections (1).len = currentsize (cpm_constants);
	cds_arguments.sections (1).struct_name = "cpm_constants";

	cds_arguments.sections (2).p = addr (cpm_static);
	cds_arguments.sections (2).len = currentsize (cpm_static);
	cds_arguments.sections (2).struct_name = "cpm_static";

	cds_arguments.seg_name = CPM_DATA_;

	cds_arguments.num_exclude_names = 0;
	cds_arguments.exclude_array_ptr = null ();

	string (cds_arguments.switches) = ""b;
	cds_arguments.have_text = "1"b;
	cds_arguments.have_static = "1"b;

	call create_data_segment_ (addr (cds_arguments), code);
	if code ^= 0
	then call com_err_ (code, CPM_DATA_);

	return;

/* format: off */
%page; %include cpm_control_point_data;
%include cpm_ctrl_pt_meters;
%include process_usage;
%page; %include cds_args;
/* format: on */

     end cpm_data_;
