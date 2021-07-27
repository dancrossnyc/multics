/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */


/* DB_FNP_SYMBOLS_ - Procedure to create a symbol table for debug_fnp */
/* The table is constructed by scanning the macros.map355 source file */

/* Written February 1978 by Larry Johnson to replace the old, canned symbol table */
/* Modified by R Holmstedt 6/81 to use the new library path >ldd>mcs. */
db_fnp_symbols_: proc;

/* Automatic */

dcl  segp ptr;					/* Pointer to entire macro segment */
dcl  segl fixed bin;				/* Its length */
dcl  restp ptr;					/* Pointer to unscanned (rest of) segment */
dcl  restl fixed bin;				/* Its length */
dcl  ptr_array (2) ptr;				/* For get_temp_segments_ */
dcl  linep ptr;					/* Pointer to current line */
dcl  linel fixed bin;				/* Its length */
dcl  code fixed bin (35);
dcl  bit_count fixed bin (24);
dcl  dir char (168);
dcl  lineno fixed bin init (0);			/* Current line number, for errors */
dcl  end_of_line bit (1);
dcl  reloc_type fixed bin;
dcl  wordp ptr;
dcl  wordl fixed bin;
dcl  sym_offset fixed bin;
dcl  label char (6);
dcl  flag_type char (6);
dcl  n_exp_words fixed bin init (0);			/* Number of words used in explanations */
dcl  explain_seg_ptr ptr;

dcl 1 cds like cds_args automatic;

dcl 1 dummy_symbol_table aligned,			/* To define correct entry in object segment */
    2 db_fnp_symbols_ fixed bin;

dcl  line char (linel) based (linep);
dcl  rest char (restl) based (restp);
dcl  word char (wordl) based (wordp);

dcl 1 explain_seg aligned based (explain_seg_ptr),	/* Explanations are accumulated in this seg */
    2 unused bit (36),				/* Dummy word so offset will never be 0 */
    2 data (1) bit (36) aligned;

/* Constants */

dcl  white_space char (2) int static options (constant) init (" 	"); /* Space and tab */
dcl  nl char (1) int static options (constant) init ("
");
dcl  name char (16) int static options (constant) init ("db_fnp_symbols_");
dcl  macro_name char (13) int static options (constant) init ("macros.map355");
dcl  statement_name (9) char (16) int static options (constant) init (
     "symrel", "symlen", "symtype", "symdef", "symflag", "symdel", "synval", "symget", "symgetr");

/* External stuff */

dcl  ioa_ entry options (variable);
dcl  ioa_$general_rs entry (ptr, fixed bin, fixed bin, char (*), fixed bin, bit (1) aligned, bit (1) aligned);
dcl  com_err_ entry options (variable);
dcl  cu_$arg_list_ptr entry (ptr);
dcl  cv_oct_check_ entry (char (*), fixed bin (35)) returns (fixed bin (35));
dcl  cv_dec_check_ entry (char (*), fixed bin (35)) returns (fixed bin (35));
dcl  get_temp_segments_ entry (char (*), dim (*) ptr, fixed bin (35));
dcl  release_temp_segments_ entry (char (*), dim (*) ptr, fixed bin (35));
dcl  hcs_$initiate_count entry (char (*), char (*), char (*), fixed bin (24), fixed bin (2), ptr, fixed bin (35));
dcl  hcs_$terminate_noname entry (ptr, fixed bin (35));
dcl  get_wdir_ entry returns (char (168));
dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl  error_table_$noentry ext fixed bin (35);

dcl  cleanup condition;

dcl (addr, addrel, bin, bit, divide, hbound, index, length, null, rel, search, size, string, substr, unspec, verify) builtin;

/* Initialization */

	ptr_array = null;
	segp = null;
	on cleanup call clean_up;

	call get_temp_segments_ (name, ptr_array, code);
	if code ^= 0 then do;
	     call com_err_ (code, name, "Unable to get temp segment.");
	     go to done;
	end;
	symbol_tablep = ptr_array (1);
	explain_seg_ptr = ptr_array (2);
	exptextp = addr (explain_seg.data);		/* Start data here */

	call init_macro_seg;			/* This locates the macro segment */

	call build_symbol_table;			/* And this does all the work */

	call relocate_explanations;			/* Copy symbols explanations in */


/* Now create the data segment */

	cds.p (1) = symbol_tablep;
	cds.len (1) = size (symbol_table) + n_exp_words;
	dummy_symbol_table.db_fnp_symbols_ = 0;		/* Reference dummy table so compiler doesnt delete it */
	cds.struct_name (1) = "dummy_symbol_table";
	cds.p (2) = null;
	cds.len (2) = 0;
	cds.struct_name (2) = "";
	cds.seg_name = name;
	cds.num_exclude_names = 0;
	cds.exclude_array_ptr = null;
	string (cds.switches) = "0"b;
	cds.have_text = "1"b;
	call create_data_segment_ (addr (cds), code);
	if code ^= 0 then call com_err_ (code, name, "From create_data_segment_");

done:	call clean_up;
	return;

/* Procedure  to scan the macro file and build the symbol table */

build_symbol_table: proc;

dcl  i fixed bin;
dcl  opname char (16);
dcl  r_sw bit (1);					/* Set for symgetr */
dcl  start_cnt fixed bin;
dcl  start_sym char (6);
dcl  end_sym char (6);
dcl  start_lineno fixed bin;
dcl  save_restp ptr;
dcl  save_restl fixed bin;

read_line:
	     call get_line;
	     if linep = null then do;			/* End of segment */
		if symbol_table.cnt = 0 then do;
		     call com_err_ (0, name, "No symbols defined.");
		     go to done;
		end;
		else return;
	     end;
	     if length (line) <= 6 then go to read_line;	/* Very short lines not interesting */
	     if substr (line, 1, 6) ^= "*++sym" then go to read_line; /* Not a line of importance */
	     call adv_line (3);			/* Skip over *++ */
	     i = search (line, white_space);		/* Find end of keyword */
	     if i = 0 then i = length (line);
	     else i = i-1;
	     opname = substr (line, 1, i);
	     call adv_line (i);			/* Skip over keyword */
	     call skip_white_space;			/* And white space after it */
	     do i = 1 to hbound (statement_name, 1);
		if statement_name (i) = opname then go to statement_type (i);
	     end;
	     call com_err_ (0, name, "Invalid *++^a statement on line ^d.", opname, lineno);
	     go to done;

statement_type (1):					/* *++ symrel */
	     if end_of_line then go to no_operand;	/* Need operands */
	     do while (^end_of_line);
		call get_symword;
		call get_word;
		call check_reloc;			/* Be sure this is valid relocation keyword */
		sym.reloc = reloc_type;
	     end;
	     go to read_line;

statement_type (2):					/* *++ symlen */
	     if end_of_line then go to no_operand;
	     do while (^end_of_line);
		call get_symword;			/* Look up symbol */
		call get_word;			/* Get length */
		if word = "" then do;
		     call com_err_ (0, name, "Invalid length for ^a on line ^d.", sym.name, lineno);
		     go to done;
		end;
		sym.len = eval (word);
	     end;
	     go to read_line;

statement_type (3):					/* *++ symtype */
	     if end_of_line then go to no_operand;
	     do while (^end_of_line);
		call get_symword;
		call get_word;
		do i = lbound (long_type_names, 1) to hbound (long_type_names, 1); /* Check for valid type */
		     if word = long_type_names (i) | word = short_type_names (i) then go to got_type;
		end;
		call com_err_ (0, name, "Invalid type for ^a on line ^d.", sym.name, lineno);
		go to done;
got_type:		sym.type = i;
	     end;
	     go to read_line;

statement_type (4):					/* *++symdef - define a new symbol */
	     if end_of_line then go to no_operand;
	     do while (^end_of_line);
		call get_word;
		if word = "" then go to no_operand;
		do i = 1 to symbol_table.cnt;		/* Be sure not duplicate */
		     symp = addr (symbol_table.entry (i));
		     if sym.name = word then do;
			call com_err_ (0, name, "Attempt to multiply define ^a on line ^d.", word, lineno);
			go to done;
		     end;
		end;
		symbol_table.cnt, symbol_table.maxcnt = symbol_table.cnt + 1;
		symp = addr (symbol_table.entry (symbol_table.cnt)); /* Addr of new entry */
		sym.name = word;
		sym.value = 0;
		sym.len = 1;
		sym.reloc = reloc_abs;
		sym.type = type_oct;
		sym.flag_mem = "";
		if ^end_of_line then do;		/* There is value */
		     call get_word;
		     sym.value = eval (word);
		end;
	     end;
	     go to read_line;

statement_type (5):					/* *++symflag */
	     if end_of_line then go to no_operand;
	     do while (^end_of_line);
		call get_symword;
		if end_of_line then sym.flag_mem = "";
		else do;
		     call get_word;
		     sym.flag_mem = word;
		end;
	     end;
	     go to read_line;

statement_type (6):					/* *++symdel - delete a previously defined symbol */
	     if end_of_line then go to no_operand;
	     do while (^end_of_line);
		call get_symword;
		do i = sym_offset + 1 to symbol_table.cnt; /* Shift everything after it down */
		     symbol_table.entry (i-1) = symbol_table.entry (i);
		end;
		unspec (symbol_table.entry (symbol_table.cnt)) = "0"b;
		symbol_table.cnt, symbol_table.maxcnt = symbol_table.cnt - 1;
	     end;
	     go to read_line;

statement_type (7):					/* *++symval - sets the valuue of a symbol */
	     if end_of_line then go to no_operand;
	     do while (^end_of_line);
		call get_symword;
		if end_of_line then sym.value = 0;
		else do;
		     call get_word;
		     sym.value = eval (word);
		end;
	     end;
	     go to read_line;

statement_type (8):					/* *++symget - gets a range of symbols into table */
	     r_sw = "0"b;
	     go to statement_type_8_9;

statement_type (9):					/* *++symgetr - like symget, but adds them in reverse order */
	     r_sw = "1"b;

statement_type_8_9:
	     save_restp = restp;			/* Save these values incase read-ahead needed */
	     save_restl = restl;
	     start_lineno = lineno;
	     start_cnt = symbol_table.cnt;		/* Remember origonal count */
	     if end_of_line then go to no_operand;
	     call get_word;				/* Get starting symbol name */
	     if word = "" | end_of_line then go to no_operand;
	     start_sym = word;
	     call get_word;				/* Ending symbol */
	     if word = "" then go to no_operand;
	     end_sym = word;
	     reloc_type = reloc_abs;			/* Default relocation */
	     flag_type = "";			/* Default flag field */
	     if ^end_of_line then do;			/* There may be relocation field */
		call get_word;
		if word = "" then reloc_type = reloc_abs;
		else call check_reloc;
	     end;
	     if ^end_of_line then do;			/* May be flag field */
		call get_word;
		flag_type = word;
	     end;

scan_start_sym:
	     call get_line;				/* Search for starting symbol */
	     if linep = null then do;
		call com_err_ (0, name, "Cant't find starting symbol ""^a"" requested on line ^d.",
		     start_sym, start_lineno);
		go to done;
	     end;
	     if substr (line, 1, 1) = "*" then go to scan_start_sym;
	     call get_label;
	     if label ^= start_sym then go to scan_start_sym;
	     call make_sym_entry;			/* Make entry for starting symbol */
	     if label = end_sym then go to done_sym_scan;
scan_end_sym:
	     call get_line;
	     if linep = null then do;
		call com_err_ (0, name, "Can't find ending symbol ""^a"" requested on line ^d.",
		     end_sym, start_lineno);
		go to done;
	     end;
	     if substr (line, 1, 1) = "*" then go to scan_end_sym;
	     call get_label;
	     if label = "" then go to scan_end_sym;
	     call make_sym_entry;
	     if label ^= end_sym then go to scan_end_sym;
done_sym_scan:
	     lineno = start_lineno;			/* Back up to *++symget statement */
	     restp = save_restp;
	     restl = save_restl;
	     i = symbol_table.cnt - start_cnt;		/* Number of symbols added */
	     if r_sw then begin;			/* Reverse them if symgetr */

dcl  j fixed bin;
dcl 1 temp_table,
    2 entry (i) unal,
      3 one_symbol like sym unal;

		do j = 1 to i;			/* Copy to temp table */
		     temp_table.entry (j) = symbol_table.entry (start_cnt + j);
		end;
		do j = i to 1 by -1;		/* Copy back */
		     symbol_table.entry (symbol_table.cnt - j + 1) = temp_table.entry (j);
		end;
	     end;
	     go to read_line;

no_operand:    call com_err_ (0, name, "Missing operand for *++^a on line ^d.", opname, lineno);
	     go to done;

	end build_symbol_table;

/* Procedure called after the symbol table is build3t to copy in explanation data and compute offsets to it */

relocate_explanations: proc;

dcl  words (n_exp_words) bit (36) aligned based;
dcl (p, q) ptr;
dcl  i fixed bin;

	     if n_exp_words = 0 then return;
	     p = addrel (symbol_tablep, size (symbol_table)); /* First word available for explanations */
	     q = addr (explain_seg.data);
	     p -> words = q -> words;			/* Copy it */
	     do i = 1 to symbol_table.cnt;		/* Loop to adjust all offsets */
		symp = addr (symbol_table.entry (i));
		if sym.explain ^= "0"b then		/* It has explanation */
		     sym.explain = bit (bin (bin (sym.explain, 17) + size (symbol_table) -1, 18), 18);
	     end;
	     return;

	end relocate_explanations;

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

get_next_line: end_of_line = "0"b;
	     if restl = 0 then do;			/* End of file */
		linep = null;
		return;
	     end;
	     lineno = lineno+1;

	     i = index (rest, nl);
	     if i = 0 then i, linel = restl;		/* No more newlines */
	     else linel = i-1;
	     linep = restp;
	     restp = substraddr (rest, i+1);
	     restl = restl - i;
	     if linel = 0 then go to get_next_line;	/* Ignor empty lines */
	     return;

	end get_line;

/* Procedure called while parsing line to move pointer down the line. */

adv_line:	proc (n);

dcl  n fixed bin;					/* How far too move */

	     linep = substraddr (line, n+1);
	     linel = linel - n;
	     if linel = 0 then end_of_line = "1"b;
	     return;

	end adv_line;

/* Procedure to skip over any white space */

skip_white_space: proc;

dcl  i fixed bin;

	     i = verify (line, white_space);		/* Count white space characters */
	     if i = 0 then i = linel;			/* All white line */
	     else i = i - 1;
	     call adv_line (i);
	     return;

	end skip_white_space;

/* Procedure to get the next word from the operand field. */

get_word:	proc;

dcl  i fixed bin;

	     if end_of_line | (linel = 0) then do;
		end_of_line = "1"b;
		wordp = null;
		wordl = 0;
		return;
	     end;

	     i = search (line, " 	,");		/* Space, tab, and comma */
	     if i = 0 then i = length (line);
	     else i = i - 1;
	     wordp = linep;
	     wordl = i;
	     call adv_line (i);			/* More forward over the word */
	     if linel > 0 then			/* Check for end of line */
		if substr (line, 1, 1) ^= "," then end_of_line = "1"b;
		else call adv_line (1);		/* Skip  ver  comma  */
	     return;

	end get_word;

/* Procedure to get the next word as a symbol */

get_symword: proc;

	     call get_word;
	     do sym_offset = 1 to symbol_table.cnt;
		symp = addr (symbol_table.entry (sym_offset));
		if sym.name = word then return;
	     end;
	     call com_err_ (0, name, "Undefined symbol ^a on line ^d", word, lineno);
	     go to done;

	end get_symword;

/* Check current word for valid relocation type */

check_reloc: proc;

	     if word = "abs" then reloc_type = reloc_abs;
	     else if word = "tib" then reloc_type = reloc_tib;
	     else if word = "sfcm" then reloc_type = reloc_sfcm;
	     else if word = "hwcm" then reloc_type = reloc_hwcm;
	     else if word = "meters" then reloc_type = reloc_meters;
	     else do;
		call com_err_ (0, name, "Invalid relocation type of ^a on line ^d", word, lineno);
		go to done;
	     end;
	     return;

	end check_reloc;

/* Extrace label from assembler statement */

get_label: proc;

dcl  i fixed bin;

	     i = search (line, white_space);
	     if i = 1 then label = "";		/* No label */
	     else do;
		if i = 0 then i = length (line);	/* Label is hole line */
		else i = i-1;
		label = substr (line, 1, i);
		call adv_line (i);
	     end;
	     call skip_white_space;
	     return;

	end get_label;

/* Procedure to make a new symbol table entry by compiling the curent line */

make_sym_entry: proc;

dcl  i fixed bin;
dcl  dec_sw bit (1);

	     i = search (line, white_space);		/* Find end of opcode */
	     if i = 0 then i = length (line);
	     else i = i-1;
	     if substr (line, 1, i) = "set" | substr (line, 1, i) = "equ" then dec_sw = "1"b; /* Valid decimal op */
	     else if substr (line, 1, i) = "bool" then dec_sw = "0"b;
	     else do;
		call com_err_ (0, name, "Unrecognized opcode ""^a"" defining ""^a"" on line ^d.",
		     substr (line, 1, i), label, lineno);
		go to done;
	     end;
	     call adv_line (i);			/* Over opcode */
	     call skip_white_space;
	     symbol_table.cnt, symbol_table.maxcnt = symbol_table.cnt + 1;
	     symp = addr (symbol_table.entry (symbol_table.cnt));
	     sym.name = label;
	     sym.value = 0;
	     sym.len = 1;
	     sym.reloc = reloc_type;
	     if flag_type = "" then sym.type = type_oct;
	     else sym.type = type_bit;
	     sym.flag_mem = flag_type;
	     sym.explain = "0"b;
	     sym.pad = "0"b;
	     i = search (line, white_space);		/* Find end of expression */
	     if i = 0 then i = length (line);
	     else i = i-1;
	     if dec_sw then sym.value = eval (substr (line, 1, i));
	     else sym.value = eval_oct (substr (line, 1, i));

	     call adv_line (i);			/* Over expression */
	     if end_of_line then return;
	     call skip_white_space;			/* Move up to comment */
	     if end_of_line then return;
	     exptext.len = length (line);		/* Rest of line is explanation */
	     exptext.data = line;
	     sym.explain = rel (exptextp);
	     i = size (exptext);
	     n_exp_words = n_exp_words + i;
	     exptextp = addrel (exptextp, i);		/* Loc for next eplanation */

	     return;

	end make_sym_entry;

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

/* Procedure to evaluate expressions in source lines. This code was
   copied origionally from db_fnp_eval_ */

eval:	proc (arg_expr) returns (fixed bin);

/* Parameters */

dcl  arg_expr char (*);				/* The expression to evaluate */

/* Automatic */

dcl  result fixed bin;
dcl  code fixed bin (35);
dcl  ntoken fixed bin;				/* Number of tokens */
dcl  exprp ptr;					/* Pointer to unparsed part of expression */
dcl  exprl fixed bin;				/* Length of unparsed part */
dcl  expr char (exprl) based (exprp);			/* The unparsed part of expression */
dcl  tstart fixed bin;				/* Starting token in sub-expression */
dcl  tend fixed bin;				/* Last token in sub-expression */
dcl  tcur fixed bin;				/* Current token */
dcl  n_mult fixed bin;				/* Count of multiplies and divides */
dcl  n_add fixed bin;				/* Count of adds and subtracts */
dcl  dec_sw bit (1);

dcl 1 token_list aligned,
    2 entry (255) unal,
      3 token like token;


/* Definition of a token */

dcl  tokenp ptr;

dcl 1 token unaligned based (tokenp),
    2 prev fixed bin (8),				/* Backwards pointer */
    2 next fixed bin (8),				/* Forwards pointer */
    2 type fixed bin (8),				/* Kind of token */
    2 sub fixed bin (8),				/* Sub-type, for some tokens */
    2 val fixed bin (35);

/* Values for token.type */

dcl (start_token init (0),				/* Start of expression */
     leftp_token init (1),				/* Left parenthesis */
     rightp_token init (2),				/* Right parenthesis */
     mult_token init (3),				/* Multiply (sub=1), or divide (sub=2) */
     add_token init (4),				/* Add (sub=1), or subtract (sub=2) */
     sym_token init (6),				/* Symbol or constant */
     end_token init (7))				/* End of expression */
     fixed bin int static options (constant);


/* Initialization */

	     dec_sw = "1"b;				/* Called at decimal entryry */
	     go to eval_start;

eval_oct:	     entry (arg_expr) returns (fixed bin);

	     dec_sw = "0"b;

eval_start:

	     exprp = addr (arg_expr);
	     exprl = length (arg_expr);


/* Now evaluate the expression */

	     call parse_expr;

	     call eval_expr;
	     return (result);

/* Procedure to parse the expression */

parse_expr:    proc;

dcl  nparen fixed bin;				/* For paren level counting */
dcl  i fixed bin;

		ntoken = 0;
		call new_token (start_token);		/* First, start of expr token */

		nparen = 0;



		do while (exprl > 0);		/* Loop until end */

		     i = index ("()*/+|-", substr (expr, 1, 1)); /* Check for special character */
		     if i = 0 then do;		/* Must be symbol */
			if sym_or_rightp () then go to bad_char;
			call parse_sym;
			go to next_token;
		     end;
		     else go to parse_op (i);		/* Branch, depending on character */

parse_op (1):					/* Left paren */
		     if sym_or_rightp () then go to bad_char;
		     call new_token (leftp_token);
		     nparen = nparen + 1;
		     call adv (1);
		     go to next_token;

parse_op (2):					/* Right paren */
		     if token.type = start_token | mult_or_add () then go to bad_char;
		     if nparen ^> 0 then do;
			if exprl = length (arg_expr) then go to bad_char;
			else call err ("Too many "")"".");
		     end;
		     call new_token (rightp_token);
		     nparen = nparen - 1;
		     call adv (1);
		     go to next_token;

parse_op (3):					/* "*" - multiply */
		     if ^sym_or_rightp () then go to bad_char;
		     call new_token (mult_token);
		     token.sub = 1;
		     call adv (1);
		     go to next_token;

parse_op (4):					/* Divide */
		     if start_or_leftp () | mult_or_add () then go to bad_char;
		     call new_token (mult_token);
		     token.sub = 2;
		     call adv (1);
		     go to next_token;

parse_op (5):					/* Add */
parse_op (6):					/* Add, alternate form ("|") */
parse_op (7):					/* Subtract */
		     if start_or_leftp () then call new_token (sym_token); /* Unary, treat as 0+ or 0- */
		     else if mult_or_add () then go to bad_char;
		     call new_token (add_token);
		     if substr (expr, 1, 1) = "-" then token.sub = 2;
		     else token.sub = 1;
		     call adv (1);
		     go to next_token;

next_token:
		end;

		if nparen ^= 0 then call err ("Parens do not balance."); /* Must balance in end */

		if mult_or_add () then call err ("Expression ends badly.");

		call new_token (end_token);
		return;

	     end parse_expr;

/* Procedure to parse a constant or a symbol name */

parse_sym:     proc;

dcl  val fixed bin (35);
dcl  bval bit (36) aligned based (addr (val));
dcl (i, j) fixed bin;
dcl  p ptr;

		i = verify (expr, "0123456789");	/* Try constant first */
		if i ^= 1 then do;			/* It is a constant */
		     if i = 0 then i = length (expr);	/* Rest of expr is a constant */
		     else i = i - 1;
		     if dec_sw then do;
			val = cv_dec_check_ (substr (expr, 1, i), code);
			if code ^= 0 then call err ("Invalid decimal integer: ""^a"".", substr (expr, 1, i));
			if val < -262144 | val > 262143 then
			     call err ("Decimal integer not in range -262144 to 262143: ^a", substr (expr, 1, i));
			call adv (i);
		     end;
		     else do;			/* Octal number */
			val = cv_oct_check_ (substr (expr, 1, i), code);
			if code ^= 0 then call err ("Invalid octal integer: ""^a"".", substr (expr, 1, i));
			if substr (bval, 1, 18) ^= "0"b & substr (bval, 1, 18) ^= "777777"b3 then
			     call err ("Octal integer not in range -400000 to 377777: ^a", substr (expr, 1, i));
			call adv (i);
		     end;
		     if val > 0 then if substr (bval, 19, 1) then /* Really negative */
			     substr (bval, 1, 18) = "777777"b3;
		     call new_token (sym_token);	/* Set up token for symbol */
		     token.val = val;
		     return;
		end;

/* Symbol must be a name */

		i = search (expr, "()*/+|-");		/* Look for end */
		if i = 1 then go to bad_char;
		if i = 0 then i = length (expr);
		else i = i - 1;

		do j = 1 to symbol_table.cnt;
		     p = addr (symbol_table.entry (j));
		     if p -> sym.name = substr (expr, 1, i) then go to sym_found;
		end;
		call err ("Invalid symbol: ^a", substr (expr, 1, i));

sym_found:
		call new_token (sym_token);
		token.val = p -> sym.value;
		call adv (i);
		return;

	     end parse_sym;

/* Procedures which to some comon tests on the previous token */

mult_or_add:   proc returns (bit (1));

		return (token.type = mult_token | token.type = add_token);

	     end mult_or_add;

start_or_leftp: proc returns (bit (1));

		return (token.type = start_token | token.type = leftp_token);

	     end start_or_leftp;

sym_or_rightp: proc returns (bit (1));

		return (token.type = sym_token | token.type = rightp_token);

	     end sym_or_rightp;

/* Procedure to create a new token and trhread it in */

new_token:     proc (type);

dcl  type fixed bin;				/* Type of new token */

		if ntoken = hbound (token_list.entry, 1) then call err ("Expression too long.");
		if ntoken > 0 then token.next = ntoken + 1; /* Set pointer in prev token */
		ntoken = ntoken + 1;
		tokenp = addr (token_list.entry (ntoken));
		token.prev = ntoken - 1;
		token.next = 0;
		token.type = type;
		token.sub = 0;
		token.val = 0;
		return;

	     end new_token;

/* Procedure to advance pointer in expression */

adv:	     proc (n);

dcl  n fixed bin;					/* Amount to move */

		exprp = substraddr (expr, n+1);	/* Adjust pointer */
		exprl = exprl - n;			/* Adjust length */
		return;

	     end adv;

/* Procedure to evaluate the expression by scanning the list of tokens */
/* The procedure is to find the inner most expression, evaluate it, and
   continue. At the end, there should only be 3 tokens left: the start, the end,
   and one symbol token containing the final value */

eval_expr:     proc;

		do while (ntoken > 3);
		     call find_sub_expr;		/* Find some inner expression to work on */
		     call eval_sub_expr;		/* And reduce it to a value */
		end;

		tokenp = addr (token_list.entry (1));	/* Pointer to start token */
		tokenp = addr (token_list.entry (token.next)); /* Second token, containing the value */
		result = token.val;			/* Get the answer */
		return;

	     end eval_expr;


/* Procedure to locate an inner expression to evaluate. This will be either
   a part of the expression delimited by parens, or, if no parens left, the
   entire expression. */
/* The following variables are set for future use:
   tstart - the first token in the expression found
   tend - the last
   n_mult - likewise for mult tokens
   n_add - likewise for add tokens */

find_sub_expr: proc;

		n_mult, n_add = 0;
		tstart, tcur = 1;
		tokenp = addr (token_list.entry (tstart));

		do while ((token.type ^= rightp_token) & (token.type ^= end_token));
		     if token.type = leftp_token then do;
			tstart = tcur;		/* Maybe expression will start here */
			n_mult, n_add = 0;		/* Must reset counters for inner level */
		     end;
		     else if token.type = mult_token then n_mult = n_mult + 1;
		     else if token.type = add_token then n_add = n_add + 1;
		     tcur = token.next;		/* On to next one */
		     tokenp = addr (token_list.entry (tcur));
		end;
		tend = tcur;

	     end find_sub_expr;

/* Procedure to evaluate sub-expression once it has been isolated. */
/* The sub-expression is repeatedly scanned for mult tokens, add tokens */

eval_sub_expr: proc;

		do while ((n_mult + n_add) > 0);
		     if n_mult > 0 then call eval_op (mult_token, n_mult);
		     if n_add > 0 then call eval_op (add_token, n_add);
		end;

		call del_token (tstart);		/* Delete parens one expression is evaluated */
		call del_token (tend);
		return;

	     end eval_sub_expr;

/* This procedure scans looking for either mult tokens or add tokens to be evaluated. */

eval_op:	     proc (token_type, token_cnt);

dcl  token_type fixed bin;				/* The kind of token being evaluated, mult or add */
dcl  token_cnt fixed bin;				/* Number still unevaluated in sub-expression */

		tcur = tstart;
		tokenp = addr (token_list.entry (tcur));
		do while (token_cnt > 0);
		     if token.type = token_type then do; /* Got one */
			call compute_op;		/* Go do the arithmetic */
			token_cnt = token_cnt - 1;
		     end;
		     tcur = token.next;
		     tokenp = addr (token_list.entry (tcur));
		end;
		return;				/* Every thing possible is done */

	     end eval_op;

/* Procedure called to evalue a mult or add token. Once the arithmetic is done,
   the value is stored in the first sym token. the operator token and the second
   symbol token are deleted. This procedure is called with tcur as the operator
   token being evaluated */

compute_op:    proc;

dcl (del1, del2) fixed bin;				/* The two tokens to be deleted */
dcl (val1, val2) fixed bin (35);			/* Values of the two symbols */
dcl  p ptr;

		del1 = tcur;			/* The operator token will be deleted */
		del2 = token.next;			/* As well as the second operand */
		p = addr (token_list.entry (token.next)); /* Pointter to second symbol token */
		val2 = p -> token.val;
		p = addr (token_list.entry (token.prev)); /* Pointer to the first symbol */
		val1 = p -> token.val;
		if token.type = add_token then do;	/* Add or subtract */
		     if token.sub = 1 then val1 = val1 + val2;
		     else val1 = val1 - val2;
		end;
		else do;				/* Multiply or divide */
		     if token.sub = 1 then val1 = val1 * val2;
		     else do;
			if val2 = 0 then call err ("Division by zero.");
			else val1 = divide (val1, val2, 35, 0);
		     end;
		end;

		tcur = token.prev;			/* Make first operand the current token */
		tokenp = addr (token_list.entry (tcur));
		token.val = val1;			/* Save answer */
		call del_token (del1);		/* Delete operator */
		call del_token (del2);		/* And the sedond operand */
		return;

	     end compute_op;

/* Procedure to delete a token by untreading it from the list */

del_token:     proc (n);

dcl  n fixed bin;					/* The token to go */
dcl (next, prev) fixed bin;
dcl  p ptr;

		p = addr (token_list.entry (n));
		prev = p -> token.prev;
		next = p -> token.next;
		if (prev = 0) | (next = 0) then return; /* Ndver delete start or end */

		p = addr (token_list.entry (prev));
		p -> token.next = next;
		p = addr (token_list.entry (next));
		p -> token.prev = prev;
		ntoken = ntoken - 1;
		return;

	     end del_token;

/* Error routines */

bad_char:
	     if exprl < length (arg_expr) then call err ("""^a"" after ""^a"" is invalid.",
		substr (expr, 1, 1), substr (arg_expr, 1, length (arg_expr) - exprl));
	     else call err ("""^a"" at beginning is invalid.", substr (expr, 1, 1));


/* General error subroutine */

err:	     proc options (variable);

dcl  s char (256);
dcl  p ptr;

		call cu_$arg_list_ptr (p);
		call ioa_$general_rs (p, 1, 2, s, (0), "1"b, "0"b);
		call com_err_ (0, name, "Invalid expression in line ^d: ""^a"". ^a", lineno, arg_expr, s);
		go to done;

	     end err;

	end eval;

%include debug_fnp_data;

%include cds_args;

     end db_fnp_symbols_;
