/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(85-09-24,Elhard), approve(85-09-24,MCR7198),
     audit(86-06-30,Weaver), install(86-07-16,MR12.0-1094):
     Change error messages to use the caller supplied name instead of
     "parse_bindfile_", changed to produce a reasonable error message when
     parsing a bindfile with no tokens or a zero length bindfile, and to give a
     warning if the obsolete "indirect" keyword is used in a "global" or
     "Global" statement.
                                                   END HISTORY COMMENTS */

/*++ 
INCLUDE NEXT_STMT	\
INCLUDE LEX	\
INCLUDE ERROR	\

BEGIN
stmt	/			/[keyword_idx = 0]          /         \
	/<no-token>		/epilogue		        /RETURN   \
	/ ;			/LEX		        /stmt     \
	/<keyword>		/	 	        /KEYWORD  \
	/<any-token>		/[if keyword_idx = 0
				  then call ERROR (1)]
				NEXT_STMT		        /stmt     \
KEYWORD
	/Addname			/Addname    LEX	        /ADDNAME  \

	/Force_Order : <long_name>	/LEX (2)
				 [arg_ptr = Ptoken;
				  arg_count = 0]	        /ORD_list \
	/Force_Order		/LEX		        /bad_syntax\

	/Global : <globe_arg> ;	/LEX (2)   Global
				 NEXT_STMT	        /stmt     \
	/Global : <globe_arg>	/LEX (2)   ERROR (7)
				 NEXT_STMT	        /stmt     \
	/Global			/LEX		        /bad_syntax\

	/Ignore : <long_name>	/LEX (2)
				 [arg_ptr = Ptoken;
				  arg_count = 0]	        /ORD_list \
	/Ignore			/LEX		        /bad_syntax\
	/No_Table			/LEX		        /noarg_stmt\

	/Objectname : <long_name> ;	/LEX (2)   Objectname
				 NEXT_STMT	        /stmt     \
	/Objectname : <long_name>	/LEX (2)   ERROR(7)
				 NEXT_STMT	        /stmt     \
	/Objectname		/LEX		        /bad_syntax\

	/Order : <long_name>	/LEX (2)
				 [arg_ptr = Ptoken;
				  arg_count = 0]	        /ORD_list \
	/Order			/LEX		        /bad_syntax\

	/Partial_Order : <long_name>	/LEX (2)
				 [arg_ptr = Ptoken;
				  arg_count = 0]	        /ORD_list \
	/Partial_Order		/LEX		        /bad_syntax\
	/Perprocess_Static		/LEX		        /noarg_stmt\

	/delete : <long_name>	/delete   LEX (2)	        /ENTRY_list\
	/delete			/LEX		        /bad_syntax\
	/global : <globe_arg> ;	/LEX (2)   global
				 NEXT_STMT	        /stmt     \
	/global : <globe_arg>	/LEX (2)   ERROR(7)
				 NEXT_STMT	        /stmt     \
	/global			/LEX		        /bad_syntax\
	/indirect : <long_name>	/indirect   LEX (2)	        /ENTRY_list\
	/indirect			/LEX		        /bad_syntax\

	/no_link : <long_name>	/no_link   LEX (2)	        /ENTRY_list\
	/no_link			/LEX		        /bad_syntax\

	/objectname : <long_name> ;	/LEX (2)   objectname
				 NEXT_STMT	        /stmt     \
	/objectname : <long_name>	/LEX (2)   ERROR (7)
				 NEXT_STMT	        /stmt     \
	/objectname		/LEX		        /bad_syntax\
	/retain : <long_name>	/retain   LEX (2)	        /ENTRY_list\
	/retain			/LEX		        /bad_syntax\

	/synonym : <long_name>	/synonym   LEX (2)	        /SEG_list \
	/synonym			/LEX		        /bad_syntax\

	/table			/LEX		        /noarg_stmt\


ADDNAME  	/;			/LEX		        /stmt     \
	/: ;			/LEX (2)		        /stmt     \
	/: <long_name>		/[an.n_an = 0]   LEX        /ADN_list \

bad_syntax 
	/: <any-token>		/LEX ERROR (2) NEXT_STMT    /stmt     \
	/: <no-token>		/ERROR (3) LEX	        /stmt     \
	/<any-token>		/ERROR (4) NEXT_STMT        /stmt     \
	/<no-token>		/ERROR (3)	        /stmt     \

noarg_stmt
	/ ;			/LEX  perform_noarg	        /stmt     \
	/ : ;			/LEX (2)  perform_noarg     /stmt     \
	/ : <any-token>		/ERROR (6) NEXT_STMT        /stmt     \
	/ : <no-token>		/ERROR (3) LEX	        /stmt     \
	/<any-token>		/ERROR (4) NEXT_STMT        /stmt     \
	/<no-token>		/ERROR (3)	        /stmt     \


ADN_list	/<seg_name> ,		/[an.n_an = an.n_an + 1;
				  if an.n_an > an.max_size
				  then call ERROR (13);
				  else an.syn (an.n_an) = token_value]
				 LEX (2)		        
						        /ADN_list \
	/<seg_name> ;		/[an.n_an = an.n_an + 1;
				  if an.n_an > an.max_size
				  then call ERROR (13);
				  else an.syn (an.n_an) = token_value]
				 NEXT_STMT	        /stmt     \
	/<seg_name>		/ERROR (7)   NEXT_STMT      /stmt     \
	/<long_name> ,		/ERROR (8)   LEX(2)	        /ADN_list \
	/<long_name> ;		/ERROR (8)   NEXT_STMT      /stmt     \
	/<any-token>		/ERROR (2)   NEXT_STMT      /stmt     \
	/<no-token>		/ERROR (3)	        /stmt     \


ORD_list	/<long_name> ,		/[if segname_too_long
				  then call ERROR (8);
				  else arg_count = arg_count + 1]
				 LEX (2)		        /ORD_list \
	/<long_name> ;		/[if segname_too_long
				  then call ERROR (8);
				  else arg_count = arg_count + 1]
				 perform_order NEXT_STMT    /stmt     \
	/<long_name> 		/[if segname_too_long
				  then call ERROR (8);
				  else call ERROR (7)]
				  NEXT_STMT	        /stmt     \
	/<any-token>		/ERROR (2) 
				 [if arg_count > 0 then do;
				    ARG_list_error = "1"b;
				    call perform_order ();
				  end]	NEXT_STMT	        /stmt     \
	/<no-token>		/ERROR (3)
				 [if arg_count > 0 then do;
				    ARG_list_error = "1"b;
				    call perform_order ();
				  end]	NEXT_STMT	        /stmt     \

SEG_list	/<seg_name> ,		/FILL_OPTION_ENTRY
				 LEX (2)		        /SEG_list \
	/<seg_name> ;		/FILL_OPTION_ENTRY
				 GET_OPTION_TOTALS
				 NEXT_STMT	        /stmt     \
	/<long_name> ,		/ERROR (8)   LEX (2)        /SEG_list \
	/<long_name> ;		/ERROR (8)   GET_OPTION_TOTALS
				 NEXT_STMT	        /stmt     \
	/<long_name> 		/[if segname_too_long
				  then call ERROR (8);
				  else call ERROR (7)]
				  NEXT_STMT	        /stmt     \
	/<any-token>		/ERROR (2)   NEXT_STMT      /stmt     \
	/<no-token>		/ERROR (3)	        /stmt     \

ENTRY_list
	/<long_name> ,		/[if ^entryname_too_long
				  then call FILL_OPTION_ENTRY]
				 LEX (2)		        /ENTRY_list\
	/<long_name> ;		/[if ^entryname_too_long
				  then call FILL_OPTION_ENTRY]
				 GET_OPTION_TOTALS
				 NEXT_STMT	        /stmt     \
	/<long_name>		/ERROR (5)  NEXT_STMT       /stmt     \
	/<any-token>		/ERROR (2)  NEXT_STMT       /stmt     \
	/<no-token>		/ERROR (3)	        /stmt     \


++*/
/* format: style3,^indnoniterdo */
%page;
/* Parse Bind File - procedure to go through optional bindfile and extract
the bind parameters from it.

Newly coded by Michael J. Spier, November 23, 1971		*/
/* modified 75.06.24 by M. Weaver to remove no_old_alm keyword */
/* modified 77.08.16 by M. Weaver to add Perprocess_Static keyword */
/* Modified 01/14/81 W. Olin Sibert for new format of input structure, and -force_order */

/* Rewritten 9/20/84 by M.Sharpe to use the reduction compiler, and implement
     the Ignore and Partial_Order statements 
   Modified 01/23/83 by M.Sharpe to correct problem with unrecognized 
     "no_link" attribute on "global" and "Global" statements.
*/

np:
parse_bindfile_:
     proc;

/*  Automatic  */

declare	breaks		char (9) var aligned,
	ignored_breaks	char (6) var aligned,
	lex_delims	char (70) var aligned,
	lex_control_chars	char (70) var aligned,
	Linput		fixed bin (21),
	Linput_ignore	fixed bin (21) init (0),
	(Pfirst_stmt_descriptor, Pfirst_token_descriptor, Pinput, Psegment)
			ptr init (null ());

declare	MYNAME		char (32);
declare	arg_count		fixed bin;
declare	code		fixed bin (35);
declare	(
	ORDER		init (1),
	FORCE_ORDER	init (2),
	PARTIAL_ORDER	init (3)
	)		fixed bin;
declare	order_type	fixed bin init (0);
declare	(i, j, k)		fixed bin;
declare	keyword_idx	fixed bin;
declare	Word_extent	fixed bin init (0);
declare	new_nobj		fixed bin;
declare	obj_idx		fixed bin init (0);
declare	opt_counter	fixed bin;
declare	partial_order_number
			fixed bin init (0);
declare	stmt_error	(17) fixed bin init ((17) (0));

declare	opt_code		char (1);

declare	ARG_list_error	bit (1) init ("0"b);
declare	entryname_too_long	bit (1) init ("0"b);
declare	ignore_seen	bit (1) init ("0"b);
declare	MAX_error		bit (1) init ("0"b);
declare	no_parameters	bit (1);
declare	order_error	bit (1) init ("0"b);
declare	Parameters	bit (1);
declare	segname_too_long	bit (1) init ("0"b);


declare	(arg_ptr, p, p1, inpp, optp, copyp, areap)
			ptr init (null);


/*  Based  */

declare	arg		char (arg_ptr -> token.Lvalue) based (arg_ptr -> token.Pvalue);
declare	1 acc_ope		aligned based (p),		/* like op but uses acc format char) */
	  2 symb_len	fixed bin (9) unsigned unaligned,
	  2 symb		char (256) unaligned,
	  2 code		char (1) aligned,
	  2 lng		fixed bin;

declare	Word		(Word_extent) fixed bin based;
declare	reset		bit (2313) based (p);


/*  Static  */

/* format: off */

declare	1 error_control_table
			(25) internal static options (constant),
	  2 severity	fixed bin (17) init ((8) 3, 1, 2, (12) 3, (3) 1),
	  2 Soutput_stmt	bit (1) unaligned
			init ((4) ("1"b), "0"b, "1"b, "1"b, "0"b, (3) ("1"b), (12) ("0"b), (2) ("1"b)),
	  2 message	char (90) varying init (
/*   1  */ "'^a ' is not a legal keyword.",
/*   2  */ "Invalid parameter.  '^a'",
/*   3  */ "Bindfile ends with an incomplete statement.",
/*   4  */ "Keyword not delimited by colon.  '^a'",
/*   5  */ "Entryname is longer than 255 characters  '^a.'",
/*   6  */ "This statement does not accept parameters.",
/*   7  */ "Improper delimiter after '^a'.",
/*   8  */ "Possible segment name is longer than 32 characters  '^a'.",
/*   9  */ "'indirect:' is an obsolete keyword.
It should be replaced by 'retain:'.",
/*  10  */ "Duplicate statement.  Statement is ignored.",
/*  11  */ "This statement must follow an 'objectname:' statement.",
/*  12  */ "'Order', 'Force_Order', and 'Partial_Order' are mutually exclusive.",
/*  13  */ "Too many addnames supplied.  Maximum of 128 addnames is allowed.",
/*  14  */ "Duplicate objectname statement for ^a.",
/*  15  */ "Objectname ^a repeated in this statement.",
/*  16  */ "Objectname ^a not found in any archive.",
/*  17  */ "Zero-length object ^a mentioned in objectname statement.",
/*  18  */ "Zero-length object ^a mentioned in Order/Force_Order/Partial_Order statement.",
/*  19  */ "Objectname ^a mentioned in 'objectname' statement but not in Force_Order statement",
/*  20  */ "Objectname ^a mentioned both in Ignore and Order/Force_Order/Partial_Order statements.",
/*  21  */ "Objectname ^a mentioned both in Ignore and objectname statements.",
/*  22  */ "Objectname ^a not mentioned in Order statement.",
/*  23  */ "Objectname ^a ignored because of zero bitcount.",
/*  24  */ "'indirect' is an obsolete Global option
It should be replaced by 'retain'.",
/*  25  */ "'indirect' is an obsolete global option
It should be replaced by 'retain'."),
	  2 brief_message	char (1) varying init ((25) (" "));
						/* brief messages aren't used */

/* format: on */

declare	ERROR_OUTPUT	char (12) int static init ("error_output");

declare	(
	Addname_idx	init (1),
	FOrder_idx	init (2),
	Global_idx	init (3),
	Ignore_idx	init (4),
	No_Table_idx	init (5),
	Objectname_idx	init (6),
	Order_idx		init (7),
	POrder_idx	init (8),
	PStatic_idx	init (9),
	delete_idx	init (10),
	global_idx	init (11),
	indirect_idx	init (12),
	no_link_idx	init (13),
	objectname_idx	init (14),
	retain_idx	init (15),
	synonym_idx	init (16),
	table_idx		init (17)
	)		fixed bin;

declare	(
	DUP_stmt		init (10),
	NO_obj		init (11),
	EXCLUSIVE_stmt	init (12),
	DUP_objectname	init (14),
	NO_such_seg	init (16),
	SKIP_stmt		init (99)
	)		fixed bin internal static;

/*  External  */

declare	error_table_$zero_length_seg
			external fixed bin (35);

/*  Builtin  */

declare	(addr, addrel, bin, bit, collate, dimension, divide, fixed, max, null, rel, size, substr)
			builtin;


/*  Entry  */

declare	com_err_		entry () options (variable),
	ioa_$ioa_stream	entry () options (variable),
	lex_string_$lex	entry (ptr, fixed bin (21), fixed bin (21), ptr, bit (*), char (*), char (*), char (*),
			char (*), char (*), char (*) var aligned, char (*) var aligned, char (*) var aligned,
			char (*) var aligned, ptr, ptr, fixed bin (35)),
	lex_string_$init_lex_delims
			entry (char (*), char (*), char (*), char (*), char (*), bit (*), char (*) var aligned,
			char (*) var aligned, char (*) var aligned, char (*) var aligned),
	temp_mgr_$reserve	entry (pointer),
	translator_temp_$get_segment
			entry (char (*), ptr, fixed bin (35)),
	translator_temp_$release_all_segments
			entry (ptr, fixed bin (35));


/*  Condition  */

declare	cleanup		condition;
%page;
	on cleanup
	     begin;
		if Psegment ^= null
		then call translator_temp_$release_all_segments (Psegment, code);
		bx_$fatal_error = 1;
	     end;

	if bx_$debug = 1
	then MYNAME = "parse_bindfile_";
	else MYNAME = bx_$caller;
	inpp = bx_$inpp;
	NTOTAL = inp.ntotal;			/* used for allocating copy area */
	NOBJ = inp.nobj;				/* ditto */
	bx_$bound_segname = inp.bound_seg_name;		/* remember in case of abort */
	if inp.bindfilep = null | inp.bindfile_bc = 0
	then do;
	     bx_$optp = null;			/* indicate there are no bind options */
	     bx_$addname = 0;
	     bx_$adnp = null;
	     if ^inp.zeroseg_seen
	     then return;

	     copyp = bx_$freep;			/* allocate area for archive reorder */
	     call temp_mgr_$reserve (addrel (copyp, size (inp)));
						/* allocate with respect to NTOTAL and NOBJ */
	     call CHECK_CONFLICTS;			/* make new structure, removing zero-length segments */
	     return;
	end;


	call translator_temp_$get_segment (MYNAME, Psegment, code);
						/* reduction compiler needs this */
	if code ^= 0
	then do;
	     call com_err_ (code, MYNAME, "Could not get temp segment.  Report to maintainer.");
	     bx_$fatal_error = 1;
	     return;
	end;

	SERROR_CONTROL = "11"b;			/* use long error messages every time */
	call INIT_PARSE;
	if Linput = 0
	then do;
	     call com_err_ (error_table_$zero_length_seg, MYNAME, inp.bindfile_name);
	     goto Return;
	end;

	if bx_$brief = 1
	then MIN_PRINT_SEVERITY = 2;			/* Don't print warnings if -brief was specified.  */

	ignored_breaks = substr (collate, 10, 5) || " ";
	breaks = ignored_breaks || ",:;";
	call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b, breaks, ignored_breaks, lex_delims,
	     lex_control_chars);
	call lex_string_$lex (Pinput, Linput, Linput_ignore, Psegment, "1100"b, """", """", "/*", "*/", ";", breaks,
	     ignored_breaks, lex_delims, lex_control_chars, Pfirst_stmt_descriptor, Pfirst_token_descriptor, code);

	Pthis_token = Pfirst_token_descriptor;

	if code ^= 0
	then do;
	     if Pthis_token = null
	     then do;
		if code = error_table_$zero_length_seg
		then call com_err_ (0, MYNAME, "No tokens found in bindfile. ^a", inp.bindfile_name);
		else call com_err_ (code, MYNAME);
		go to Return;
	     end;					/*	     if code = error_table_$no_stmt_delim | code = error_table_$unbalanced_quotes 
	     error messages which can specify the location of the error are printed during the semantic analysis */
	end;

	call SEMANTIC_ANALYSIS ();
Return:
	if Psegment ^= null
	then call translator_temp_$release_all_segments (Psegment, code);
	return;

%page;
/*****************************************************************************/

/*  These 4 entries are auxiliary scanner/lexical routines */

FUNCTIONS:
     proc returns (bit (1) aligned);			/* this entry returns "1"b if a valid parameter has been specified
   to the Global/global keywords; "0"b otherwise.  */

globe_arg:
     entry returns (bit (1) aligned);

	if token_value = "delete" | token_value = "indirect" | token_value = "no_link" | token_value = "retain"
	then return ("1"b);

	else return ("0"b);


/*  This entry returns "1"b if the current token consists of only alphanumeric
    characters allowed in an entryname.  It also performs asemantic function.
    If the token is longer than 32 characters the segname_too_long flag is
    set; if it is longer than 255 characters the entryname_too_long flag is
    also set.  This reduces the number of reductins necessary for parsing the
    bindfile.  */

long_name:
     entry returns (bit (1) aligned);

	segname_too_long, entryname_too_long = "0"b;


	if token.Lvalue > 32
	then segname_too_long = "1"b;
	if token.Lvalue > 255
	then entryname_too_long = "1"b;

	return ("1"b);


/*  This entry returns "1"b if the current token is an appropriate segment name */

seg_name:
     entry returns (bit (1) aligned);

	segname_too_long = "0"b;

	if token.Lvalue > 32
	then do;
	     segname_too_long = "1"b;
	     call ERROR (8);
	     return ("0"b);
	end;

	return ("1"b);


/*  This entry determines if the current token is a valid keyword.  If so
    keyword_idx is set accordingly; otherwise, keyword_idx is set to 0 and
    "0"b is returned.  Some semantic analysis is also perfomed here.  If a
    token is recognized as a keyword, but one whose presence in the
    bindfile constitues a semantic error, e.g., duplicate master keyword,
    the proper error message is issued here and "0"b is returned.  The
    reduction routine ignores the entire statement if "0"b is returned; it
    issues errors only if keyword_idx is set to 0.   */

keyword:
     entry () returns (bit (1) aligned);

	if token_value = "Addname"
	then keyword_idx = Addname_idx;
	else if token_value = "Force_Order"
	then keyword_idx = FOrder_idx;
	else if token_value = "Global"
	then keyword_idx = Global_idx;
	else if token_value = "Ignore"
	then keyword_idx = Ignore_idx;
	else if token_value = "No_Table"
	then keyword_idx = No_Table_idx;
	else if token_value = "Objectname"
	then keyword_idx = Objectname_idx;
	else if token_value = "Order"
	then keyword_idx = Order_idx;
	else if token_value = "Partial_Order"
	then keyword_idx = POrder_idx;
	else if token_value = "Perprocess_Static"
	then keyword_idx = PStatic_idx;
	else if token_value = "delete"
	then keyword_idx = delete_idx;
	else if token_value = "global"
	then keyword_idx = global_idx;
	else if token_value = "indirect"
	then keyword_idx = indirect_idx;
	else if token_value = "no_link"
	then keyword_idx = no_link_idx;
	else if token_value = "objectname"
	then keyword_idx = objectname_idx;
	else if token_value = "retain"
	then keyword_idx = retain_idx;
	else if token_value = "synonym"
	then keyword_idx = synonym_idx;
	else if token_value = "table"
	then keyword_idx = table_idx;
	else return ("0"b);

	if stmt_error (keyword_idx) = 0
	then return ("1"b);
	else if stmt_error (keyword_idx) = SKIP_stmt
	then return ("0"b);				/* Skip these silently */

	call ERROR (stmt_error (keyword_idx));
	return ("0"b);


     end FUNCTIONS;

%page;
/*****************************************************************************/

/*  The following are the semantic analysis routines.  The entries beginning
    with "Addname" up to "table" correspond to bindfile keywords.  Others are
    auxiliary routines.  */

ACTION:
     proc;


perform_order:
     entry ();

	if keyword_idx = Order_idx
	then call Order ();
	else if keyword_idx = FOrder_idx
	then call Force_Order ();
	else if keyword_idx = POrder_idx
	then call Partial_Order ();
	else if keyword_idx = Ignore_idx
	then call Ignore ();

	else ;					/* should never happen */
	return;


perform_noarg:
     entry ();

	if keyword_idx = No_Table_idx
	then call No_Table ();
	else if keyword_idx = PStatic_idx
	then call Perprocess_Static ();
	else if keyword_idx = table_idx
	then call table ();

	return;


Addname:
     entry ();


	stmt_error (Addname_idx) = DUP_stmt;
	Parameters = "1"b;
	bx_$addname = 1;

	return;


Force_Order:
     entry ();

	stmt_error (FOrder_idx) = DUP_stmt;		/* Don't use this again */
	stmt_error (Order_idx) = EXCLUSIVE_stmt;	/* And this one is mutually exclusive with Force_Order */
	stmt_error (POrder_idx) = EXCLUSIVE_stmt;	/* ... and so's this one */

	order_type = FORCE_ORDER;
	call REORDER_ARCHIVE ();

	return;


Global:
     entry ();

	stmt_error (Global_idx) = DUP_stmt;
	if token_value = "delete"
	then g_delete = "d";			/* This scheme was kept from the original program */
	else if token_value = "retain"
	then g_retain = "r";			/* because it is integral to other parts of the */
	else if token_value = "no_link"
	then g_nolink = "l";			/* binder software */
	else if token_value = "indirect"
	then do;
	     call ERROR (24);
	     g_indirect = "i";
	end;
	else do;
	     call ERROR (2);
	     return;
	end;

	Parameters = "1"b;

	return;


Ignore:
     entry ();

/*  The reduction routine checks the syntax of the "Ignore" statement;
	     if it's OK, it sets arg_ptr to the addr of the 1st parameter and arg_count
	     to the number of parameters.  Because the parameters are separated by commans,
	     every other token is skipped when getting the next parameter */

	stmt_error (Ignore_idx) = DUP_stmt;
	ignore_seen = "1"b;
	p1 = Pthis_token;				/* save it */
	Pthis_token = arg_ptr;			/* allows the correct component names to appear in
						   the error messages in the followin loop */


	do i = 1 to arg_count;			/* Lookup input structure to match parameter */

	     if arg_ptr -> token.Lvalue > 32
	     then goto next_ignore_arg;

	     do j = 1 to bx_$ncomp;
		p = addr (inp.obj (j));
		if obj.filename = arg
		then goto ignore_it;
	     end;

	     call ERROR (NO_such_seg);		/* the segment is not among the components specified by user */
	     goto next_ignore_arg;

ignore_it:
	     if (obj.to_be_ignored & obj.bitcount ^= 0)
	     then do;				/* Duplicated in Ignore statement */
		call ERROR (15);
		goto next_ignore_arg;
	     end;

	     obj.to_be_ignored = "1"b;
next_ignore_arg:
	     Pthis_token, arg_ptr = arg_ptr -> token.Pnext -> token.Pnext;
						/* skip the delimiter between two args */
						/* see opening comments for ignore */
	end;
	Pthis_token = p1;				/* restore to end of Ignore stmt */

	return;


No_Table:
     entry ();

/*  The option table entries are filled by FILL_OPTION_ENTRY by the
	     reduction routine after we return from here.   */

	stmt_error (No_Table_idx) = DUP_stmt;
	option.g_notable = "t";
	no_parameters = "0"b;
	return;


Objectname:
     entry ();

	stmt_error (Objectname_idx) = DUP_stmt;
	if token.Lvalue > 32
	then call ERROR (8);
	else bx_$bound_segname = token_value;

	return;


Order:
     entry ();

	stmt_error (Order_idx) = DUP_stmt;
	stmt_error (FOrder_idx) = EXCLUSIVE_stmt;
	stmt_error (POrder_idx) = EXCLUSIVE_stmt;

	if bx_$force_order ^= 0
	then order_type = FORCE_ORDER;		/* Order becomes Force_Order if -force_order
							   is specified in the command line */
	else order_type = ORDER;
	call REORDER_ARCHIVE ();

	return;


Partial_Order:
     entry ();

	stmt_error (POrder_idx) = DUP_stmt;
	stmt_error (FOrder_idx) = EXCLUSIVE_stmt;
	stmt_error (Order_idx) = EXCLUSIVE_stmt;

	order_type = PARTIAL_ORDER;
	call REORDER_ARCHIVE ();

	return;


Perprocess_Static:
     entry ();

	stmt_error (PStatic_idx) = DUP_stmt;
	bx_$perprocess_static = 1;
	Parameters = "1"b;
	return;


delete:
     entry ();

/*  The option table entries are filled by FILL_OPTION_ENTRY by the
	     reduction routine after we return from here.   */

	stmt_error (delete_idx) = DUP_stmt;
	opt_counter = 0;
	opt_code = "d";
	no_parameters = "0"b;
	return;


global:
     entry ();

	stmt_error (global_idx) = DUP_stmt;
	if token_value = "delete"
	then op.delete = "d";
	else if token_value = "retain"
	then op.retain = "r";
	else if token_value = "no_link"
	then op.no_link = "l";
	else if token_value = "indirect"
	then do;
	     call ERROR (25);
	     op.indirect = "i";
	end;
	else do;
	     call ERROR (2);
	     return;
	end;

	no_parameters = "0"b;

	return;


indirect:
     entry ();

/*  The option table entries are filled by FILL_OPTION_ENTRY by the
	     reduction routine after we return from here.   */

	stmt_error (indirect_idx) = DUP_stmt;
	if bx_$brief = 0
	then call ERROR (9);			/* This is an obsolete keyword; it now functions like "retain" */
	opt_counter = 0;
	opt_code = "i";
	no_parameters = "0"b;
	return;


no_link:
     entry ();

/*  The option table entries are filled by FILL_OPTION_ENTRY by the
	     reduction routine after we return from here.   */

	stmt_error (no_link_idx) = DUP_stmt;
	opt_counter = 0;
	opt_code = "l";
	no_parameters = "0"b;
	return;


objectname:
     entry ();

	stmt_error (objectname_idx) = 0;
	call CLOSE_ENTRY;				/* start a new option table entry, etc. */

	if segname_too_long
	then do;
	     call ERROR (8);
	     goto skip_objname;
	end;
	do obj_idx = 1 to bx_$ncomp;
	     if token_value = inp.obj (obj_idx).filename
	     then go to OPEN_ENTRY;
	end;
	call ERROR (NO_such_seg);

	goto skip_objname;

OPEN_ENTRY:
	if inp.obj (obj_idx).bitcount = 0
	then do;
	     call ERROR (17);			/* zero-length segs can't be specified in any statements */
	     goto skip_objname;
	end;

	if inp.obj (obj_idx).option ^= "0"b
	then do;					/* Another objectname statement for this obj specified earlier */
	     call ERROR (DUP_objectname);
skip_objname:					/* Ignore any attributes pertaining to this rejected */
	     stmt_error (delete_idx),			/*   objectname.  The errors cause the corresponding */
		stmt_error (global_idx),		/*   statements to be ignored (See keyword FUNCTION) */
		stmt_error (indirect_idx),		/*   until the next objectname statement is seen */
		stmt_error (no_link_idx),		/*   when all are reset to 0  */
		stmt_error (retain_idx), stmt_error (synonym_idx), stmt_error (table_idx) = SKIP_stmt;

	     return;
	end;

	op.n_retain, op.n_indirect, op.n_nolink, op.n_options, op.n_synonyms, op.n_delete = 0;
						/* reset structure */
	op.table, op.retain, op.indirect, op.no_link, op.delete = " ";
						/* ... */
	inp.obj (obj_idx).objectname_stmt = "1"b;	/* Now it cannot be Ignore'd or omitted
						   from Order / Force_Order lists */
	no_parameters = "1"b;			/* indicate no params specified yet */

	stmt_error (delete_idx) = 0;			/* reset errors for attribute statements, since any errors */
	stmt_error (global_idx) = 0;			/*   were for previous object and we've got a new one now */
	stmt_error (indirect_idx) = 0;
	stmt_error (no_link_idx) = 0;
	stmt_error (retain_idx) = 0;
	stmt_error (synonym_idx) = 0;
	stmt_error (table_idx) = 0;


/* put objectname into structure, tag it as synonym */

	opt_counter = 0;
	opt_code = "s";				/* pretend it's a synonym */
	call FILL_OPTION_ENTRY;
	op.n_synonyms = 1;

	return;


retain:
     entry ();

/*  The option table entries are filled by FILL_OPTION_ENTRY by the
	     reduction routine after we return from here.   */

	stmt_error (retain_idx) = DUP_stmt;
	opt_counter = 0;
	opt_code = "r";
	no_parameters = "0"b;
	return;

synonym:
     entry ();

/*  The option table entries are filled by FILL_OPTION_ENTRY by the
	     reduction routine after we return from here.   */

	stmt_error (synonym_idx) = DUP_stmt;
	opt_counter = 0;
	opt_code = "s";
	no_parameters = "0"b;
	return;


table:
     entry ();

	stmt_error (table_idx) = DUP_stmt;
	op.table = "t";
	no_parameters = "0"b;
	return;


REORDER_ARCHIVE:
     entry ();

/*  The argument processing is same as Ignore */

	k = 0;
	order_error = ARG_list_error;
	p1 = Pthis_token;				/* save it */
	Pthis_token = arg_ptr;			/*  This will allow the correct component name to be 
						    printed in error messages inside this loop */

	do i = 1 to arg_count;			/* Lookup input structure to match parameter */

	     if arg_ptr -> token.Lvalue > 32
	     then goto next_arg;
	     do j = 1 to bx_$ncomp;
		p = addr (inp.obj (j));
		if obj.filename = arg
		then goto match_found;
	     end;

	     call ERROR (NO_such_seg);
	     order_error = "1"b;
	     goto next_arg;

match_found:
	     if obj.flag = "1"b
	     then do;				/* repeated in order-ing statement */
		call ERROR (15);
		order_error = "1"b;
		goto next_arg;
	     end;

	     if obj.bitcount = 0
	     then do;				/* zero-length obj has no business here */
		call ERROR (18);
		order_error = "1"b;
		goto next_arg;
	     end;

	     obj.flag = "1"b;
	     k = k + 1;				/* increment index */
	     obj.new_order = k;			/* remember the new order index */
next_arg:
	     arg_ptr, Pthis_token = arg_ptr -> token.Pnext -> token.Pnext;
						/* skip the delimiter between two args */
	end;

	Pthis_token = p1;				/* restore to end of the Order stmt */

	if order_type = PARTIAL_ORDER
	then partial_order_number = k;		/* the number of objects so far "ordered".  The rest will be
						   given order numbers, beginning with partial_order_number +1,
						   in the order they appear in the input structure */

	if order_error
	then bx_$fatal_error = 1;
	return;


FILL_OPTION_ENTRY:
     entry ();

	opt_counter = opt_counter + 1;
	op.n_options = op.n_options + 1;
	p = addr (op.opes (op.n_options));
	reset = "0"b;				/* want trailer nulls instead of blanks */
	acc_ope.symb_len = token.Lvalue;
	substr (acc_ope.symb, 1, token.Lvalue) = token_value;
	acc_ope.code = opt_code;
	acc_ope.lng = token.Lvalue + 1;

	return;

GET_OPTION_TOTALS:
     entry ();

/* routine to set the counter for the particular option */

	if keyword_idx = no_link_idx
	then n_nolink = n_nolink + opt_counter;
	else if keyword_idx = indirect_idx
	then n_indirect = n_indirect + opt_counter;
	else if keyword_idx = retain_idx
	then n_retain = n_retain + opt_counter;
	else if keyword_idx = delete_idx
	then n_delete = n_delete + opt_counter;
	else if keyword_idx = synonym_idx
	then n_synonyms = n_synonyms + opt_counter;	/*	 else;		should never happen.  */

	return;



epilogue:
     entry ();

	call CLOSE_ENTRY;
	if order_type > 0 | ignore_seen | zeroseg_seen
	then call CHECK_CONFLICTS;

	if MERROR_SEVERITY > 2
	then do;
	     bx_$fatal_error = 1;
	     return;
	end;

	if Parameters = "0"b
	then do;
	     bx_$adnp = null;			/* ... */
	     bx_$addname = 0;			/* ... */
	     Word_extent = fixed (rel (optp), 18) - fixed (rel (areap), 18) + 128;
	     areap -> Word (*) = 0;			/* reset allocated table area  */
	end;

	call temp_mgr_$reserve (addr (op.opes (opt_counter + 1)));
						/* Mark the extent of the option table */
	return;


CLOSE_ENTRY:
     entry ();

	stmt_error (delete_idx),			/* These statements can't appear again BEFORE */
	     stmt_error (global_idx),			/*    the next objectname stmt */
	     stmt_error (indirect_idx), stmt_error (no_link_idx), stmt_error (retain_idx), stmt_error (synonym_idx),
	     stmt_error (table_idx) = NO_obj;


	if no_parameters
	then /* virgin entry */
	     do;
	     if obj_idx ^= 0
	     then inp.obj (obj_idx).option = "0"b;	/* reset option pointer */
	     return;
	end;

	if obj_idx ^= 0
	then inp.obj (obj_idx).option = bit (bin (fixed (rel (optp), 18) - fixed (rel (areap), 18), 18), 18);
	Parameters = "1"b;
	no_parameters = "1"b;			/* indicate no objectname parameters */
	if op.n_options = 1
	then do;					/* No options; just the objectname used as synonym.  */
	     op.n_options, op.n_synonyms, op.n_options = 0;
	end;
	if obj_idx > 0
	then optp = addr (op.opes (op.n_options + 1));	/* prepare pointer to next option structure */
	obj_idx = 0;

	return;

%page;
/*
 *   It is the responsibility of this routine to examine the inp structure,
 *   verifying that the Ignore, Order, Force_Order and Partial_Order
 *   statements have been used in a consistent fashion.
 *   If no errors are detected, a new copy of the inp structure
 *   is built (in the area reserved and pointed to by copyp) and the
 *   desired objectnames are placed in the new structure, in the
 *   desired order.
 *
 *   It is assumed that the caller has already checked the ignore, order and
 *   zeroseg_seen flags, so this routine is not called when it is not needed.
 */

CHECK_CONFLICTS:
     entry ();

declare	erring_token_value	char (32);


	j = 0;					/*  reset index that is used if no Order-ing statements in bindfile   */

	new_nobj = 0;				/*   reset counter of kept object segments   */

	do i = 1 to bx_$ncomp;			/*   examine every objectname known, looking for errors   */

	     p = addr (inp.obj (i));
	     erring_token_value = obj.filename;

	     if obj.bitcount = 0 /* Warn about zero-lengt segs except where . . . */
		& obj.new_order = 0 /* 1) there's a coflict with order (already handled) */
		& ^obj.objectname_stmt /* 2) there's a conflict with objectname stmt (ditto) */ & bx_$brief = 0
	     then do;				/* 3) they've asked us to keep quiet */
		MERROR_SEVERITY = max (1, MERROR_SEVERITY);
		call ioa_$ioa_stream (ERROR_OUTPUT, "^/WARNING 23^/" || error_control_table.message (23) || "^/",
		     erring_token_value);
	     end;

	     else if obj.to_be_ignored
	     then do;				/* appeared in Ignore stmt */
		if obj.new_order > 0
		then do;				/* ... AND some order stmt */
		     MERROR_SEVERITY = 3;
		     call ioa_$ioa_stream (ERROR_OUTPUT,
			"^/ERROR 20 SEVERITY 3^/" || error_control_table.message (20) || "^/", erring_token_value);
		end;

		if obj.objectname_stmt
		then do;				/* ... AND/OR in an objectname stmt */
		     MERROR_SEVERITY = 3;
		     call ioa_$ioa_stream (ERROR_OUTPUT,
			"^/ERROR 21 SEVERITY 3^/" || error_control_table.message (21) || "^/", erring_token_value);
		end;


	     end;
	     if order_type = FORCE_ORDER & obj.objectname_stmt /* obj was in an objectname stmt... */
		& obj.new_order = 0			/*    but not in the Force_Order stmt */
	     then do;
		MERROR_SEVERITY = 3;
		call ioa_$ioa_stream (ERROR_OUTPUT,
		     "^/ERROR 19 SEVERITY 3^/" || error_control_table.message (19) || "^/", erring_token_value);
	     end;

	     if order_type = ORDER & obj.new_order = 0 /* object not in Order stmt ... */ & ^obj.to_be_ignored
						/*   and had no excuse for not being there */
	     then do;
		MERROR_SEVERITY = 3;
		call ioa_$ioa_stream (ERROR_OUTPUT,
		     "^/ERROR 22 SEVERITY 3^/" || error_control_table.message (22) || "^/", erring_token_value);
	     end;

	     if order_type = PARTIAL_ORDER & ^obj.to_be_ignored & obj.new_order = 0
						/* objects not in Partial_Order stmt ... */
	     then do;				/*   will be assigned orders as we catch them */
		partial_order_number = partial_order_number + 1;
		obj.new_order = partial_order_number;
	     end;

/*
	 *   Assign an order to this module if we had no
	 *   type of Order statement and if this module is
	 *   not to be ignored.
	 */

	     if order_type = 0 & ^obj.to_be_ignored
	     then do;				/*   assign an order index to this not-to-be-ignored object   */
		j = j + 1;
		obj.new_order = j;
	     end;

	     if obj.new_order > 0
	     then new_nobj = new_nobj + 1;
	end;

/*   Now we can fill in the copy of the inp structure and switch pointers, if there were no errors.   */

	if MERROR_SEVERITY < 3
	then do;
	     Word_extent = fixed (fixed (rel (addr (inp.obj (1).filename)), 18) - fixed (rel (inpp), 18), 18);
						/* get size of input structure */
	     copyp -> Word (*) = inpp -> Word (*);	/* copy input structure */

	     do i = 1 to bx_$ncomp;
		p = addr (inp.obj (i).filename);
		if obj.new_order > 0
		then do;
		     p1 = addr (copyp -> inp.obj (obj.new_order).filename);
		     p1 -> obj = p -> obj;
		end;
	     end;

	     bx_$ncomp, copyp -> inp.nobj = new_nobj;
	     inpp, bx_$inpp = copyp;

	end;

	return;

     end ACTION;
%page;

/**************************************************************************************************************************/
INIT_PARSE:
     proc ();


	Pinput = inp.bindfilep;			/* copy pointer to bindfile */

	Linput = divide (inp.bindfile_bc + 8, 9, 17, 0);	/* compute length in characters */
	Parameters = "0"b;
	no_parameters = "1"b;

	copyp = bx_$freep;				/* allocate area for possible archive reorder */
	call temp_mgr_$reserve (addrel (copyp, size (inp)));
						/* allocate with respect to NTOTAL and NOBJ */

	adnp, bx_$adnp = bx_$freep;			/* beginning of addname table */
	call temp_mgr_$reserve (addrel (adnp, (bx_$addname_limit + 2) * 9));
						/* reserve table */
	an.n_an = 0;
	an.max_size = bx_$addname_limit;

	areap, bx_$optp = bx_$freep;			/* beginning of option table */
						/* we shouldn't run off the end of the temp seg by th time the option table 
				   is full.  The temp area for the table is "reserved (actually marked) 
				   before we leave, i.e., when we KNOW how much space is needed. */

/* initialize various variables */

	g_notable, g_retain, g_indirect, g_nolink, g_delete = " ";
						/* reset options structure */
	optp = addr (option.structures);		/* pointer to first structure */

	stmt_error (delete_idx),			/* Can't have any of these stmts until AFTER the next objectname stmt */
	     stmt_error (global_idx), stmt_error (indirect_idx), stmt_error (no_link_idx), stmt_error (retain_idx),
	     stmt_error (synonym_idx), stmt_error (table_idx) = NO_obj;

	if bx_$force_order = 1
	then stmt_error (POrder_idx) = EXCLUSIVE_stmt;	/* if -forcer_order specified on cmmand line, "Order" is "translated" to
					   Force_Order; Partial_Order,however, is not allowed in the bindfile */
	return;

     end INIT_PARSE;

/*	  The binder include files (1:4) and the code generated
		 by the reduction compiler follows.		   */

%page;
%include binder_input;
%page;
%include option;
%page;
%include bndtbl;
%page;
%include bindext;
%page;
