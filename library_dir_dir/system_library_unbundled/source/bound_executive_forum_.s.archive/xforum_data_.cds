
/* HISTORY COMMENTS:
  1) change(86-01-17,LJAdams), approve(86-02-18,MCR7350),
     audit(86-04-24,Gilcrease), install(86-04-24,MR12.0-1048):
     This program creates the xforum data structure for the help search
     directories.
                                                   END HISTORY COMMENTS */

xforum_data_:
	proc;

dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl  1 cdsa	     aligned like cds_args;

dcl  code		     fixed bin (35);

dcl  name		     char (12) aligned static init
                         ("xforum_data_") options (constant),
     exclude_pad         (1) char (32) aligned static options (constant) init
		     ("pad*");

dcl (dim,
     addr,
     size,
     string)	     builtin;

%include xforum_data_;

dcl 1 xforum_data aligned,
      2 help_dirs,
        3 N fixed bin,
        3 dir_array (2) char(168);


/* Set up help directory search paths */

xforum_data.help_dirs.N = dim(xforum_data.dir_array, 1);

xforum_data.dir_array(1) = ">doc>ss>xforum";
xforum_data.dir_array(2) = ">doc>info";

/* Now set up call to create data base */

cdsa.sections (1).p = addr (xforum_data);
cdsa.sections (1).len = size (xforum_data);
cdsa.sections (1).struct_name = "xforum_data";
cdsa.seg_name = name;
cdsa.num_exclude_names = 1;
cdsa.exclude_array_ptr = addr (exclude_pad);
string (cdsa.switches) = "0"b;
cdsa.switches.have_text = "1"b;
call create_data_segment_ (addr (cdsa), code);

% include cds_args;

end xforum_data_;
