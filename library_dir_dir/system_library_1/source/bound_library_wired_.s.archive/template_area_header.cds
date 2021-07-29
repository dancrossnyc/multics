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


template_area_header: proc;


/* Automatic */

dcl 1 template_area aligned,
    2 template_area_header aligned like area_header;
dcl 1 cdsa aligned like cds_args;
dcl  code fixed bin (35);

/* Builtin */

dcl (null, unspec, bit, bin, size, addr, string) builtin;

/* Entries */

dcl  create_data_segment_ entry (ptr, fixed bin (35));

/*  */
	unspec (template_area) = "0"b;

	template_area.version = 1;
	template_area.next_virgin = bit (bin (size (template_area), 18), 18);
	template_area.last_size = bit (bin (2, 18), 18);
	template_area.last_block = bit (bin (size (template_area)-2, 18), 18);


/* Now call data base create program */

	cdsa.sections (1).p = addr (template_area);
	cdsa.sections (1).len = size (template_area);
	cdsa.sections (1).struct_name = "template_area";

	cdsa.seg_name = "template_area_header";
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null;;

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	return;

/*  */

%include area_structures;
%include cds_args;
     end;
