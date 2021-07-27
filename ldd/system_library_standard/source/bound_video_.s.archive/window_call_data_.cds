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


/* CDS program defining the parameters and usage of the various
   functions of window call */
/* Benson I. Margulies, sometime in 1981 */
/* Modified by Chris Jones, 7 December 1981, to handle "undocumented" keys 
   and control args */
/* Modified 29 June 1982 by William York to fix the control args for 
   the clear_region key */
/* Modified 14 June 1983 by Jon A. Rochlis to add "supported_terminal" 
   and "video_invoked" keywords as well as -ttp control arg */
/* Modified 1 October 1983 by JR to add support for partial screen width 
   windows by allowing change_window and create_window to take column and
   n_column arguments */
/* Modified 28 June 1984 by JR to add get_window_width, since I forgot about
   it in October. */
/* Modified 6 September 1984 by C. Marker to add -line_speed which will allow
   a user to specify this speed of his connection. */
/* format: style4,delnl,insnl,indattr,ifthen,dclind9 */

window_call_data_:
     procedure;

/* we dont believe in positional arguments, save the key.
   everything is control arguments, taking various things
   afterwards. */

declare  1 f		  aligned like function based (f_ptr);
declare  f_ptr		  pointer;

declare  1 ca		  aligned like ctl_arg_info based (ca_ptr);
declare  ca_ptr		  pointer;

declare  get_temp_segments_	  entry (character (*), (*) pointer, fixed binary (35));
declare  release_temp_segments_ entry (character (*), (*) pointer, fixed binary (35));

declare  ME		  character (32) init ("window_call_data_") internal static options (constant);
declare  (
         TEXT		  init (1),
         STATIC		  init (2)
         )		  fixed bin internal static options (constant);

declare  sys_info$max_seg_size  fixed bin (19) ext static;

declare  tsp		  (3) pointer;

declare  headerspace	  (sys_info$max_seg_size) bit (36) aligned based (tsp (1));
declare  functionspace	  (sys_info$max_seg_size) bit (36) aligned based (tsp (2));
declare  stringspace	  character (sys_info$max_seg_size * 4) based (tsp (3));

declare  fx		  fixed bin (19);
declare  sx		  fixed bin (21);

declare  h_ptr		  pointer;

/* No refer extents, because we construct with addrel technology,
   and do not touch the functions at all until the very end. */

declare  1 header		  based (h_ptr) aligned,
	 2 n_keys		  fixed bin,
	 2 n_ctl_args	  fixed bin,
	 2 string_length	  fixed bin (21),
	 2 names		  (header.n_keys) aligned,
	   3 long		  character (32) unaligned,
	   3 undocumented_long
			  character (32) unaligned,
	   3 short	  character (8) unaligned,
	   3 undocumented_short
			  character (8) unaligned,
	 2 functions	  (header.n_keys) like function aligned,
	 2 ctl_args	  (header.n_ctl_args) like ctl_arg_info aligned,
	 2 string		  character (header.string_length) unaligned;

declare  code		  fixed bin (35);
declare  com_err_		  entry () options (variable);

declare  1 cdsa		  aligned like cds_args;

declare  create_data_segment_	  entry (pointer, fixed binary (35));

declare  cleanup		  condition;
declare  (addr, currentsize, null, string)
			  builtin;


	tsp (*) = null ();

	on cleanup call clean_it_up;

	call get_temp_segments_ (ME, tsp, (0));		/* if it fails, we fault through null */

	fx = 0;
	sx = 0;

	h_ptr = addr (headerspace (1));
	header.n_keys = 0;

/* hx is not interesting until all the functions are ready */

/* 1 */
	f_ptr = get_function ("clear_window", "clwd", "", "cw", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;		/* accept -io_switch */

/* 2 */
	f_ptr = get_function ("bell", "", "", "", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;

/* 3 */
	f_ptr =
	     get_function ("clear_region", "clrgn", "", "cr",
	     "{-io_switch WINDOW_NAME} -line LINE -height N_LINES -column COLUMN -width N_COLUMNS");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_LINE).allowed = "1"b;
	f.args (C_LINE).required = "1"b;
	f.args (C_N_LINES).allowed = "1"b;		/* No Default, too dangerous */
	f.args (C_N_LINES).required = "1"b;
	f.args (C_COLUMN).allowed = "1"b;
	f.args (C_COLUMN).required = "1"b;
	f.args (C_N_COLUMNS).allowed = "1"b;
	f.args (C_N_COLUMNS).required = "1"b;

/* 4 */

	f_ptr = get_function ("clear_to_end_of_line", "cleol", "", "cteol", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;

/* 5 */
	f_ptr = get_function ("clear_to_end_of_window", "cleowd", "", "cteow", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;

/* 6 */
	f_ptr = get_function ("delete_chars", "dlch", "", "dc", "-count N {-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_COUNT).allowed = "1"b;
	f.args (C_COUNT).required = "1"b;

/* 7 */
	f_ptr = get_function ("get_position", "gpos", "get_cursor_position", "gcp", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.af_allowed = "1"b;

/* 8 */
	f_ptr = get_function ("get_echoed_chars", "gech", "", "gec", "-count N {-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_COUNT).allowed = "1"b;
	f.args (C_COUNT).required = "1"b;
	f.af_allowed = "1"b;

/* 9 */
	f_ptr = get_function ("get_unechoed_chars", "guch", "", "guc", "-count N {-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_COUNT).allowed = "1"b;
	f.args (C_COUNT).required = "1"b;
	f.af_allowed = "1"b;

/* 10 */
	f_ptr = get_function ("insert_text", "itx", "", "it", "-string TEXT {-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_STRING).allowed = "1"b;
	f.args (C_STRING).required = "1"b;

/* 11 */
	f_ptr = get_function ("overwrite_text", "otx", "", "ot", "-string TEXT {-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_STRING).allowed = "1"b;
	f.args (C_STRING).required = "1"b;

/* 12 */
	f_ptr =
	     get_function ("set_position", "spos", "position_cursor", "pc",
	     "-line LINE -column COLUMN {-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_LINE).allowed, f.args (C_LINE).required = "1"b;
	f.args (C_COLUMN).allowed, f.args (C_COLUMN).required = "1"b;

/* 13 */
	f_ptr =
	     get_function ("set_position_rel", "sposrel", "position_cursor_rel", "pcr",
	     "-line LINE_DELTA -column COLUMN_DELTA {-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_LINE).allowed, f.args (C_LINE).required = "1"b;
	f.args (C_COLUMN).allowed, f.args (C_COLUMN).required = "1"b;

/* 14 */
	f_ptr =
	     get_function ("scroll_region", "scrgn", "", "sr",
	     "{-line START -height SIZE} -count SCROLL_DISTANCE {-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_LINE).allowed = "1"b;
	f.args (C_N_LINES).allowed = "1"b;
	f.args (C_COUNT).allowed, f.args (C_COUNT).required = "1"b;

/* 15 */
	f_ptr = get_function ("sync", "", "", "", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;

/* 16 */
	f_ptr =
	     get_function ("write_sync_read", "wsr", "", "", "-count N_TO_READ -string PROMPT {-io_switch WINDOW_NAME}")
	     ;
	f.args (C_SWITCH).allowed = "1"b;
	f.af_allowed = "1"b;
	f.args (C_COUNT).allowed, f.args (C_COUNT).required = "1"b;
	f.args (C_STRING).allowed, f.args (C_STRING).required = "1"b;

/* 17 */
	f_ptr = get_function ("invoke", "", "", "", "{-line_speed LINE_SPEED}");
	f.args (C_LINE_SPEED).allowed = "1"b;

/* 18 */
	f_ptr = get_function ("revoke", "", "", "", "");

/* 19 */
	f_ptr =
	     get_function ("create_window", "crwd", "", "crw",
	     "-io_switch WINDOW_NAME {-line FIRST_LINE} {-column FIRST_COLUMN} {-height N_LINES} {-width N_COLUMNS}");
	f.args (C_SWITCH).allowed, f.args (C_SWITCH).required = "1"b;
	f.args (C_LINE).allowed, f.args (C_N_LINES).allowed = "1"b;
	f.args (C_COLUMN).allowed = "1"b;
	f.args (C_N_COLUMNS).allowed = "1"b;

/* 20 */
	f_ptr = get_function ("delete_window", "dlwd", "destroy_window", "dsw", "-io_switch WINDOW_NAME");
	f.args (C_SWITCH).required, f.args (C_SWITCH).allowed = "1"b;

/* 21 */
	f_ptr = get_function ("change_window", "chgwd", "", "chw", "{-io_switch WINDOW_NAME} {-line N} {-height N} {-column N} {-width N}");
	f.args (C_SWITCH).allowed = "1"b;
	f.args (C_LINE).allowed = "1"b;
	f.args (C_N_LINES).allowed = "1"b;
	f.args (C_COLUMN).allowed = "1"b;
	f.args (C_N_COLUMNS).allowed = "1"b;

/* 22 */

	f_ptr = get_function ("get_first_line", "gfl", "", "", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.af_allowed = "1"b;

/* 23 */

	f_ptr = get_function ("get_window_height", "gwdhgt", "get_n_lines", "gnl", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.af_allowed = "1"b;

/* 24 */
	f_ptr = get_function ("get_window_width", "gwdwid", "get_n_cols", "gnc", "{-io_switch WINDOW_NAME}");
	f.args (C_SWITCH).allowed = "1"b;
	f.af_allowed = "1"b;

/* 25 */

	f_ptr = get_function ("get_terminal_height", "gtmhgt", "", "gtmh", "");
	f.af_allowed = "1"b;

/* 26 */

	f_ptr = get_function ("get_terminal_width", "gtmwid", "", "gtmw", "");
	f.af_allowed = "1"b;

/* 27 */
	f_ptr = get_function ("get_one_unechoed_char", "gouch", "", "gouc", "{-io_switch WINDOW}");
	f.af_allowed = "1"b;
	f.args (C_SWITCH).allowed = "1"b;

/* 28 */  
	f_ptr = get_function ("supported_terminal", "", "supported_ttp", "", "-ttp TERMINAL_TYPE");
	f.af_allowed = "1"b;
	f.args (C_TERMINAL_TYPE).allowed = "1"b;

/* 29 */
	f_ptr = get_function ("video_invoked", "", "", "", "");
	f.af_allowed = "1"b;

/* Now header.n_keys is correct, we can copy the functions */

	begin;
declare  1 farray		  (header.n_keys) aligned like function based (fa_ptr);
declare  fa_ptr		  pointer;

	     fa_ptr = addr (functionspace (1));

	     header.functions = farray;		/* Page faults! get um while their hot! */
	end;

/* Now build the control arguments in the functionspace seg */
/* These calls must be in the order of the C_ constants for
   this to work */

	fx = 0;

	call make_ctl_arg ("line", "", "", "ln", A_NUMBER);
	call make_ctl_arg ("column", "col", "", "cl", A_NUMBER);
	call make_ctl_arg ("count", "ct", "", "", A_NUMBER);
	call make_ctl_arg ("height", "hgt", "n_lines", "nl", A_NUMBER);
	call make_ctl_arg ("io_switch", "is", "", "iosw", A_STRING);
	call make_ctl_arg ("screen", "", "", "", A_STRING);
	call make_ctl_arg ("string", "str", "", "", A_STRING);
	call make_ctl_arg ("width", "wid", "n_columns", "nc", A_NUMBER);
	call make_ctl_arg ("terminal_type", "ttp", "", "", A_STRING);
	call make_ctl_arg ("line_speed", "ls", "", "", A_NUMBER);
	header.n_ctl_args = N_CTL_ARGS;

	begin;

declare  1 ctlargs		  (N_CTL_ARGS) aligned like ctl_arg_info based (cas_ptr);
declare  cas_ptr		  pointer;

	     cas_ptr = addr (functionspace (1));
	     header.ctl_args = ctlargs;
	end;

/* now for usage strings */

	header.string_length = sx;			/* points at last character */

	begin;
declare  stringwewant	  character (header.string_length) defined (stringspace) position (1);

	     header.string = stringwewant;
	end;


	cdsa.sections (TEXT).p = addr (header);
	cdsa.sections (TEXT).len = currentsize (header);
	cdsa.sections (STATIC).p = null ();
	cdsa.sections (STATIC).len = 0;

	cdsa.struct_name = "header";
	cdsa.seg_name = ME;

	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null ();

	string (cdsa.switches) = ""b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	call clean_it_up;

	if code ^= 0 then
	     call com_err_ (code, ME);
	return;


clean_it_up:
     procedure;

	call release_temp_segments_ (ME, tsp, (0));
     end clean_it_up;

get_function:
     procedure (lname, sname, ulname, usname, usage) returns (pointer);

declare  (lname, sname, ulname, usname, usage)
			  character (*);
declare  nf_ptr		  pointer;
declare  1 nf		  aligned like function based (nf_ptr);
declare  usage_x		  fixed bin (21);
declare  (string, length)	  builtin;

	fx = fx + 1;				/* point to first word of new one */
	nf_ptr = addr (functionspace (fx));
	fx = fx + currentsize (nf) - 1;		/* point to last word of new one */

	usage_x = sx + 1;
	begin;
declare  u_in_string	  character (length (usage)) defined (stringspace) position (usage_x);
	     u_in_string = usage;
	     sx = sx + length (usage);
	     nf.usage.index = usage_x;
	     nf.usage.length = length (usage);
	end;
	header.n_keys = header.n_keys + 1;
	header.names (header.n_keys).long = lname;
	header.names (header.n_keys).short = sname;
	header.names (header.n_keys).undocumented_long = ulname;
	header.names (header.n_keys).undocumented_short = usname;

	string (nf.args) = ""b;
	return (nf_ptr);
     end get_function;

make_ctl_arg:
     procedure (lname, sname, ulname, usname, atype);
declare  (lname, sname, ulname, usname)
			  character (*);
declare  atype		  fixed bin;

	fx = fx + 1;
	ca_ptr = addr (functionspace (fx));
	ca.name.long = lname;
	ca.name.short = sname;
	ca.name.undocumented_long = ulname;
	ca.name.undocumented_short = usname;
	ca.argument = atype;
	fx = fx + currentsize (ca) - 1;
     end make_ctl_arg;

%include window_call_info_;
%include cds_args;

     end window_call_data_;

