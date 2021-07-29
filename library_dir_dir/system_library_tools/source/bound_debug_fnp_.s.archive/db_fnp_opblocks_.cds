/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */


/* DB_FNP_OPBLOCKS_ - Procedure to create a table of interpreter opblock names */
/* The table is constructed by scanning the macros.map355 source file */

/* Written March 1977 by Larry Johnson */
/* Modified by R Holmstedt to use the new library >ldd>mcs. */
db_fnp_opblocks_: proc;

/* Automatic */

dcl  segp ptr;					/* Pointer to entire macro segment */
dcl  segl fixed bin;				/* Its length */
dcl  restp ptr;					/* Pointer to unscanned (rest of) segment */
dcl  restl fixed bin;				/* Its length */
dcl  max_cnt fixed bin;				/* Max number of opblocks found */
dcl  i fixed bin;
dcl  opname char (6);
dcl  ptr_array (1) ptr;				/* For get_temp_segments_ */
dcl  linep ptr;					/* Pointer to current line */
dcl  linel fixed bin;				/* Its length */
dcl  code fixed bin (35);
dcl  bit_count fixed bin (24);
dcl  dir char (168);
dcl 1 cds like cds_args automatic;

dcl  line char (linel) based (linep);
dcl  rest char (restl) based (restp);

/* Constants */

dcl  white_space char (2) int static options (constant) init (" 	"); /* Space and tab */
dcl  nl char (1) int static options (constant) init ("
");
dcl  name char (16) int static options (constant) init ("db_fnp_opblocks_");
dcl  exclude_all char (32) int static options (constant) init ("**");
dcl  macro_name char (13) int static options (constant) init ("macros.map355");

/* External stuff */

dcl  com_err_ entry options (variable);
dcl  ioa_ entry options (variable);
dcl  cv_oct_ entry (char (*)) returns (fixed bin (35));
dcl  get_temp_segments_ entry (char (*), dim (*) ptr, fixed bin (35));
dcl  release_temp_segments_ entry (char (*), dim (*) ptr, fixed bin (35));
dcl  hcs_$initiate_count entry (char (*), char (*), char (*), fixed bin (24), fixed bin (2), ptr, fixed bin (35));
dcl  hcs_$terminate_noname entry (ptr, fixed bin (35));
dcl  get_wdir_ entry returns (char (168));
dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl  error_table_$noentry ext fixed bin (35);

dcl  cleanup condition;

dcl (addr, divide, index, max, null, search, size, string, substr, verify) builtin;


/* Initialization */

	ptr_array = null;
	segp = null;
	on cleanup call clean_up;

	call init_macro_seg;			/* This locates the macro segment */

hunt_opstart:					/* Must find the line *++opstart */
	call get_line;
	if linep = null then do;			/* End of file */
	     call com_err_ (0, name, "No *++opstart statement in ^a^[>^]^a", dir, dir ^= ">", macro_name);
	     go to done;
	end;
	if linel < 10 then go to hunt_opstart;
	if substr (line, 1, 10) ^= "*++opstart" then go to hunt_opstart;

/* Found start of opblock section */

	call get_temp_segments_ (name, ptr_array, code);
	if code ^= 0 then do;
	     call com_err_ (code, name, "Unable to get temp segment");
	     go to done;
	end;
	opblock_tablep = ptr_array (1);
	max_cnt = -1;

/* Now scan for maccros */

hunt_macro:
	call get_line;
	if linep = null then go to done_scan;		/* End of segment */
	if substr (line, 1, 1) = "*" then go to hunt_macro; /* Skip comments */
	i = search (line, white_space);		/* Find first white space */
	if (i = 0) | (i = 1) then go to hunt_macro;	/* Ignore lines with no label, or no white space */
	opname = substr (line, 1, i-1);		/* Save label */
	call adv_line (i);				/* Skip over label */
	call skip_white_space;			/* And any trailing white space */
	i = search (line, white_space);		/* Look for end of opcode */
	if i = 0 then i = linel;
	else i = i-1;
	if i ^= 5 then go to hunt_macro;		/* Cant be "macro" */
	if substr (line, 1, 5) ^= "macro" then go to hunt_macro;

/* Found start of a macro */

	call get_line;				/* Read next line */
	if linep = null then go to done_scan;
	call skip_white_space;
	if linel < 3 then go to hunt_macro;		/* Too short to say "oct" */
	if substr (line, 1, 3) ^= "oct" then go to hunt_macro;
	call adv_line (3);
	call skip_white_space;
	if linel < 6 then go to hunt_macro;		/* Too short to say 777*** */
	if substr (line, 1, 3) ^= "777" then go to hunt_macro;
	if verify (substr (line, 4, 3), "01234567") ^= 0 then go to hunt_macro;

/* A real opblock macro was found */

	call store (opname, cv_oct_ (substr (line, 4, 3)));
	go to hunt_macro;				/* Back for next */

/* Come here at end of segment */
done_scan:
	if max_cnt = -1 then do;
	     call com_err_ (0, name, "No opblocks defined in ^a^[>^]^a", dir, dir ^= ">", macro_name);
	     go to done;
	end;

/* Now create the data segment */

	cds.p (1) = opblock_tablep;
	cds.len (1) = size (opblock_table);
	cds.struct_name (1) = "opblock_table";
	cds.p (2) = null;
	cds.len (2) = 0;
	cds.struct_name (2) = "";
	cds.seg_name = name;
	cds.num_exclude_names = 1;
	cds.exclude_array_ptr = addr (exclude_all);
	string (cds.switches) = "0"b;
	cds.have_text = "1"b;
	call create_data_segment_ (addr (cds), code);
	if code ^= 0 then call com_err_ (code, name, "From create_data_segment_");

done:	call clean_up;
	return;

/* Procedure to initiate the macro source segment. It looks first in the working directory,
   then in >ldd>mcs>info */

init_macro_seg: proc;

	     dir = get_wdir_ ();
	     call hcs_$initiate_count (dir, macro_name, "", bit_count, 0, segp, code);
	     if segp = null then do;
		if code ^= error_table_$noentry then do;
macro_seg_err:	     call com_err_ (code, name, "^a^[>^]^a", dir, dir ^= ">", macro_name);
		     go to done;
		end;
		dir = ">ldd>mcs>info";		/* Try library */
		call hcs_$initiate_count (dir, macro_name, "", bit_count, 0, segp, code);
		if segp = null then go to macro_seg_err;
	     end;

	     call ioa_ ("^a: Using ^a^[>^]^a", name, dir, (dir ^= ">"), macro_name);

	     segl = divide (bit_count, 9, 17, 0);
	     restp = segp;
	     restl = segl;
	     return;

	end init_macro_seg;

/* Procedure to isolate the next line in the source */

get_line:	proc;

dcl  i fixed bin;

get_next_line:
	     if restl = 0 then do;			/* End of file */
		linep = null;
		return;
	     end;

	     i = index (rest, nl);
	     if i = 0 then i, linel = restl;		/* No more newlines */
	     else linel = i-1;
	     linep = restp;
	     restp = substraddr (rest, i+1);
	     restl = restl - i;
	     if linel = 0 then go to get_next_line;	/* Ignor empty lines */
	     return;

	end get_line;

/* Procedure called while pasring line to move pointer down the line */

adv_line:	proc (n);

dcl  n fixed bin;					/* How far too move */

	     linep = substraddr (line, n+1);
	     linel = linel - n;
	     return;

	end adv_line;

/* Procedure to skip over any white space */

skip_white_space: proc;

dcl  i fixed bin;

	     i = verify (line, white_space);		/* Count white space charactrs */
	     if i = 0 then i = linel;			/* All white line */
	     else i = i - 1;
	     call adv_line (i);
	     return;

	end skip_white_space;

/* Procedure to store a new entry in the table */

store:	proc (opn, n);

dcl  opn char (6);					/* Name of block */
dcl  n fixed bin;					/* Its number */

dcl  i fixed bin;

	     do i = (max_cnt + 1) to (n - 1);		/* Fill in any skipped entries */
		opblock_table.name (i) = "**??**";
	     end;

	     opblock_table.name (n) = opn;
	     max_cnt = max (max_cnt, n);
	     opblock_table.cnt = max_cnt;
	     return;

	end store;

/* Cleanup handler */

clean_up:	proc;

	     if segp ^= null then call hcs_$terminate_noname (segp, code);
	     if ptr_array (1) ^= null then call release_temp_segments_ (name, ptr_array, code);
	     return;

	end clean_up;

/* Simulate substraddr builtin temporarily */

substraddr: proc (c, n) returns (ptr);

dcl  c char (*);
dcl  n fixed bin;
dcl  ca (n) char (1) unal based (addr (c));

	     return (addr (ca (n)));

	end substraddr;

%include debug_fnp_data;

%include cds_args;

     end db_fnp_opblocks_;
