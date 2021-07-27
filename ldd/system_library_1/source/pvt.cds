/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */
/* format: style1 */
pvt: proc ();

/* This creates the PVT data base. No initialization is done here,
   as it is done during system initialization.

   Written February 1982 by J. Bongiovanni */

	dcl     code		 fixed bin (35);
	dcl     1 cdsa		 like cds_args aligned;
	dcl     1 pvt_auto		 aligned like pvt automatic;

	dcl     create_data_segment_	 entry (ptr, fixed bin (35));
	dcl     com_err_		 entry options (variable);


	dcl     MYNAME		 char (3) int static options (constant) init ("pvt");
	dcl     EXCLUDE_ARRAY	 (1) char (32) int static options (constant) init ("pad*");

	dcl     addr		 builtin;
	dcl     dimension		 builtin;
	dcl     size		 builtin;
	dcl     unspec		 builtin;


%page;
	pvtp = addr (pvt_auto);
	unspec (pvt) = ""b;

	unspec (cdsa) = ""b;
	cdsa.have_text = "1"b;
	cdsa.p (1) = pvtp;
	cdsa.len (1) = size (pvt);
	cdsa.struct_name (1) = "pvt";


	cdsa.seg_name = MYNAME;
	cdsa.num_exclude_names = dimension (EXCLUDE_ARRAY, 1);
	cdsa.exclude_array_ptr = addr (EXCLUDE_ARRAY);

	call create_data_segment_ (addr (cdsa), code);

	if code ^= 0 then
	     call com_err_ (code, MYNAME);

	return;

%page;
%include cds_args;
%page;
%include pvt;

     end pvt;
