/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(86-03-21,Blair), approve(86-03-21,MCR7358),
     audit(86-04-21,RBarstad), install(86-05-28,MR12.0-1062):
     This program creates the xmail data structure for the help search
     directories.
                                                   END HISTORY COMMENTS */

xmail_data_:
	proc;

dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl  1 cdsa	     aligned like cds_args;

dcl  code		     fixed bin (35);

dcl  name		     char (12) aligned static init
                         ("xmail_data_") options (constant),
     exclude_pad         (1) char (32) aligned static options (constant) init
		     ("pad*");

dcl (dim,
     addr,
     size,
     string)	     builtin;

%include xmail_help_data_;

dcl 1 xmail_help_data aligned,
      2 help_dirs,
        3 N fixed bin,
        3 dir_array (1) char(168);


/* Set up help directory search paths */

xmail_help_data.help_dirs.N = dim(xmail_help_data.dir_array, 1);

xmail_help_data.dir_array(1) = ">doc>ss>executive_mail";

/* Now set up call to create data base */

cdsa.sections (1).p = addr (xmail_help_data);
cdsa.sections (1).len = size (xmail_help_data);
cdsa.sections (1).struct_name = "xmail_help_data";
cdsa.seg_name = name;
cdsa.num_exclude_names = 1;
cdsa.exclude_array_ptr = addr (exclude_pad);
string (cdsa.switches) = "0"b;
cdsa.switches.have_text = "1"b;
call create_data_segment_ (addr (cdsa), code);

% include cds_args;

end xmail_data_;
