/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/* Name:  library_descriptor_compiler, ldc					*/
	/*									*/
	/*      This command accepts as input a library descriptor source segment, and creates	*/
	/* as output an ALM segment which can be compiled into a binary data base which is a	*/
	/* library descriptor segment.  This data base is used by the library_info, library_map,	*/
	/* and library_print commands (among others), and is part of the Multics Library 	*/
	/* Maintenance System.							*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */


/* HISTORY COMMENTS:
  1) change(74-07-04,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 1.0--
      Created initial version of the program.
  2) change(75-02-28,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 2.0--
      Remove Global keywords; make root keyword more flexible;
      rename it to Root
  3) change(75-12-01,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 2.1--
      a) Change 'Define: default value;' stmt to 'Define: commands;'.
      b) Make commands unsupported unless named explicitly
         in 'Define:commands;' stmt.
      c) Accept the singular keywords, 'library name' and 'search name'.
  4) change(76-05-25,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 3.0--
      Reorganize name structures contained in library descriptor to put the
      names for each entity into a separate, self-defining structure.
  5) change(77-01-17,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 3.1--
      Insure that the ALM labels chosen to link root definitions with root
      names are unique for each root.
  6) change(84-09-08,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 3.2--
      a) accommodate change to lex_string_ handling of quoted strings by adding
         the <null_string_name> relative syntax function to replace an absolute
         syntax function of "".
      b) add support for per-process date/time format in header comment of the
         compiled library descriptor.
  7) change(86-01-14,GDixon), approve(86-02-06,MCR7336),
     audit(86-02-11,Dickson), install(86-02-11,MR12.0-1015):
     Version 3.3--
      corrects a bug which prevents library root names from having
      more than two components.
                                                   END HISTORY COMMENTS */


/*++
BEGIN	/ Descriptor : <name> ;		/ LEX(2) [obj_desc.name = token_value] LEX(2)
					  descriptor_begin				/ descriptor_body \
	/ Descriptor			/ ERROR(1) descriptor_begin NEXT_STMT		/ BEGIN	\
	/ <no-token>			/ ERROR(2)				/ stop	\
	/ <any-token>			/ ERROR(3) descriptor_begin			/	\

descriptor_body
	/ Root :				/ LEX(2) root_begin PUSH(root_body)		/ names	\
	/ Root				/ ERROR(4) NEXT_STMT root_begin		/ root_body \
	/ Define : commands ;		/ [Icommand = 0] NEXT_STMT			/ command_block \
	/ Define : <any-token>		/ ERROR(7) NEXT_STMT			/ descriptor_body \
	/ Define				/ ERROR(1) NEXT_STMT			/ descriptor_body \
	/ End : <descriptor_name> ;		/ NEXT_STMT				/ end	\
	/ End : <name> ;			/ ERROR(6) NEXT_STMT			/ end	\
	/ End				/ ERROR(1) NEXT_STMT			/ end	\
	/ <any-token>			/ ERROR(8) NEXT_STMT			/ descriptor_body \
	/ <no-token>			/ ERROR(9) 				/ stop	\

names	/ ;				/             			LEX	/ STACK_POP \
	/ ( <valid_name>			/ new_element(Pfirst_name_elements) 	LEX	/ name_elements \
	/ ( <null_string_name>		/ new_element(Pfirst_name_elements)	LEX	/ name_elements \
	/   <valid_name>			/ set_name    			LEX	/ names	\
	/   <any-token>			/ ERROR(12)   			LEX	/ names	\
	/   <no-token>			/ ERROR(13)				/ stop	\

name_elements
	/ ) . ( <valid_name>		/ new_element(name_elements.Pnext)      LEX(3)	/ name_elements \
	/ ) . ( <null_string_name>		/ new_element(name_elements.Pnext)      LEX(3)	/ name_elements \
	/ )				/ combine_elements(Pfirst_name_elements,"")	
					  [Pname = addr(obj_root_name)]         LEX	/ names	\
	/ ;				/ combine_elements(Pfirst_name_elements,"") 
					  ERROR(11) [Pname = addr(obj_root_name)] LEX	/ STACK_POP \
	/       <valid_name>		/ set_element      			LEX	/ name_elements \
	/       <null_string_name>		/ set_element      			LEX	/ name_elements \
	/       <any-token>			/ ERROR(12)        			LEX	/ name_elements \
	/       <no-token>			/ ERROR(13)				/ stop	\

root_body	/ path : <absolute_pathname> ;	/ LEX(2) [obj_root.path = token_value] LEX(2)	/ root_body \
	/ path : <any-token> ;		/ ERROR(15) NEXT_STMT			/ root_body \
	/ path				/ ERROR(1) NEXT_STMT			/ root_body \
	/ type : archive ;			/ LEX(4) [obj_root.type = Tarchive]		/ root_body \
	/ type : directory ;		/ LEX(4)					/ root_body \
	/ type : <any-token>		/ LEX(2) ERROR(38) NEXT_STMT			/ root_body \
	/ type				/        ERROR(1)  NEXT_STMT			/ root_body \
	/ search procedure : <entryname> ;	/ LEX(3) [search_proc.ename = ename] 
					  LEX(2)					/ root_body \
	/ search procedure : <any-token> ;	/ ERROR(5) NEXT_STMT			/ root_body \
	/ search				/ ERROR(1) NEXT_STMT			/ root_body \
	/ <any-token>			/ root_end				/ descriptor_body \
	/ <no-token>			/ ERROR(9)				/ stop	\

command_block
	/ command : <command_name_> ;		/ LEX(4) [Scommand = "1"b] command_begin	/ default_value \
	/ unsupported command : <command_name_> ;
					/ LEX(5) [Scommand = "0"b] command_begin	/ command_block \
	/ library 			/ ERROR(35) NEXT_STMT			/ command_block \
	/ search	 			/ ERROR(36) NEXT_STMT			/ command_block \
	/ command : <any-token> ;		/ LEX(2) ERROR(25) NEXT_STMT			/ command_block \
	/ unsupported command : <any-token> ;	/ LEX(3) ERROR(25) NEXT_STMT			/ command_block \
	/ <any-token>			/ 					/ descriptor_body \
	/ <no-token>			/ ERROR(9)				/ stop	\

default_value
	/ library names :			/					/ library_names \
	/ library name :			/					/ library_names \
	/ search names :			/					/ search_names \
	/ search name :			/					/ search_names \
	/ <any-token>			/ command_end				/ command_block \
	/ <no-token>			/					/ command_block \
library_names
	/ 				/ [Pname = addr(obj_dflt_lib_names)]
					  [Pobj_star_code = addr(obj_dflt_lib_codes)]
					  LEX(3) PUSH(default_value)			/ starname \
search_names
	/ 				/ [Pname = addr(obj_dflt_search_names)]
					  [Pobj_star_code = addr(obj_dflt_search_codes)]
					  LEX(3) PUSH(default_value)			/ starname \

starnames	/ ;				/                            LEX		/ STACK_POP \
starname	/ <starname>			/ set_name set_obj_star_code LEX		/ starnames \
	/ ;				/ ERROR(10) 	         NEXT_STMT		/ STACK_POP \
	/ <any-token>			/ ERROR(12) 	         LEX		/ starnames \
	/ <no-token>			/ ERROR(13)				/ STACK_POP \

end	/ <no-token>			/					/ stop	\
	/ <any-token>			/ ERROR(17)				/ stop	\

stop	/ 				/ compile_descriptor			/ RETURN	\
											++*/

library_descriptor_compiler:
ldc:	procedure;

     dcl						/*	automatic variables			*/
	Icommand			fixed bin,	/* index of the command in command default values	*/
						/*    struc.				*/
	Larg			fixed bin,	/* length of an input argument.		*/
	Lin			fixed bin(21),	/* length of input segment (in chars).		*/
	Lout			fixed bin(21),	/* length of output segment (in chars).		*/
	Nargs			fixed bin,	/* number of input arguments.			*/
						/*   to definition of root just being parsed.	*/
	Pacl_out			ptr,		/* ptr to ACL struc for output segment.		*/
	Parg			ptr,		/* ptr to an input argument.			*/
	Pfirst_name_elements	ptr,		/* ptr to the first name elements structure.	*/
	Pin			ptr,		/* ptr to the input segment.			*/
	Pname			ptr,		/* ptr to the current name structure.		*/
						/*    the library names struc, when forming	*/
						/*    full names assoc with current root.	*/
	Pname_elements		ptr,		/* ptr to current name elements structure.	*/
	Pobj_dflt_lib_codes		ptr,		/* ptr to the object default library starcodes.	*/
	Pobj_dflt_lib_names		ptr,		/* ptr to the object default library names struc.	*/
	Pobj_dflt_search_codes	ptr,		/* ptr to the object default search starcodes.	*/
	Pobj_dflt_search_names	ptr,		/* ptr to the object default search names struc.	*/
	Pobj_root			ptr,		/* ptr to the current object root struc.	*/
	Pobj_root_array		ptr,		/* ptr to the object root struc.		*/
	Pobj_root_name		ptr,		/* ptr to the object root names struc.		*/
	Pobj_search_proc		ptr,		/* ptr to the object search procedure struc.	*/
	Pobj_star_code		ptr,		/* ptr to the current object star code struc.	*/
	Pout			ptr,		/* ptr to the output segment.			*/
	Ptemp_seg			ptr,		/* ptr to our temporary segment.		*/
	Scommand			bit(1) aligned,	/* switch: on if command indicated by Icommand is	*/
						/*    "supported" in object command default values*/
	Sreject_root		bit(1) aligned,	/* switch: on if root definition is to be rejected*/
	bc_in			fixed bin(24),	/* length of input segment (in bits).		*/
	cleanup			condition,
	code			fixed bin(35),	/* a status code.				*/
	compilation_date		char(52),		/* date/time output segment was compiled.	*/
	dir_in			char(168),	/* dir part of path name of input segment.	*/
	dir_out			char(168),	/* dir part of path name of output segment.	*/
	ent_in			char(32),		/* ent part of path name of input segment.	*/
	ent_out			char(32),		/* ent part of path name of output segment.	*/
	entry_point		char(70),		/* an ALM format entry point name.		*/
	entry_point_name		char(65) varying,	/* a PL/I format entry point name.		*/
	1 ename			aligned,		/* current entry point name.			*/
	  2 ref			char(32),		/*    reference name			*/
	  2 ent			char(32),		/*        entry name			*/
         (i, j, k)			fixed bin,	/* do-group indices.			*/

	1 obj_command_dflt_values (dimension (command_name,1))
				aligned,		/* object command default values structure.	*/
	  2 S			unaligned,	/*    switches:				*/
	    3 supported		bit(1),		/*       this command is supported.		*/
	  2 lib_names,				/*    library names to be used if none specified.	*/
	    3 Ifirst		fixed bin,
	    3 Ilast		fixed bin,	/*       indices of first/last name in table.	*/
	  2 search_names,				/*    search names to be used if none specified.	*/
	    3 Ifirst		fixed bin,
	    3 Ilast		fixed bin,
	1 obj_desc,				/* object descriptor.			*/
	  2 name			char(32) init ("default_descriptor"),
	path			char(168) aligned,	/* a path name temporary.			*/
	1 search_proc		aligned,		/* struc for the local search procedure.	*/
	  2 ename,
	    3 ref			char(32),		/*    reference name of search proc entry point.	*/
	    3 ent			char(32),		/*        entry name of search proc entry point.	*/
	starcode			fixed bin(35),	/* return code from check_star_name_$entry.	*/
	temp_name			char(32),		/* a name temporary.			*/
	temp_name30		char(30) varying;	/* another name temporary.			*/

     dcl						/*	based variables			*/
	arg			char(Larg) based (Parg),
						/* an input argument.			*/
	1 name			aligned based (Pname),
						/* the name structure.			*/
	  2 M			fixed bin,	/*    maximum number of names struc will hold.	*/
	  2 N			fixed bin,	/*    current number of names in struc.		*/
	  2 ERROR			fixed bin,	/*    error message to print when struc overflows.*/
	  2 V (0 refer (name.N))	char(32) varying aligned,
						/*    array of names.			*/
	1 name_elements		aligned based (Pname_elements),
						/* temp storage for elements of a compound name.	*/
	  2 header,
	    3 Pnext		ptr,		/*   ptr to next name element structure.	*/
	    3 M			fixed bin,	/*   maximum number of names struc will hold.	*/
	    3 N			fixed bin,	/*   current number of names in struc.		*/
	    3 ERROR		fixed bin,	/*   error message to print when struc overflows*/
	  2 V (50 refer (name_elements.N))
				char(32) varying,	/*   array of name elements.			*/
	1 obj_dflt_lib_codes	aligned based (Pobj_dflt_lib_codes),
						/* return codes from check_star_name_$entry for	*/
						/*    names on a 'library names' statement in a	*/
						/*    'Define: commands;' block.		*/
	  2 M			fixed bin,	/*    maximum no of codes structure will hold.	*/
	  2 N			fixed bin,	/*    current no of codes in structure.		*/
	  2 C (100 refer (obj_dflt_lib_codes.M))
				fixed bin,	/*    array of codes.			*/
	1 obj_dflt_lib_names	aligned based (Pobj_dflt_lib_names),
						/* names on a 'library names' statement in a	*/
						/*    'Define: commands;' block.		*/
	  2 M			fixed bin,	/*    maximum no of names structure will hold.	*/
	  2 N			fixed bin,	/*    current no of names in structure.		*/
	  2 ERROR			fixed bin,	/*    error message to print when struc overflows.*/
	  2 V (100 refer (obj_dflt_lib_names.M))
				char(32) varying,	/*    array of names.			*/

	1 obj_dflt_search_codes	aligned based (Pobj_dflt_search_codes),
						/* return codes from check_star_name_$entry for	*/
						/*    names on a 'search names' statement in a	*/
						/*    'Define: commands;' block.		*/
	  2 M			fixed bin,	/*    maximum no of codes structure will hold.	*/
	  2 N			fixed bin,	/*    current no of codes in structure.		*/
	  2 C (100 refer (obj_dflt_search_codes.M))
				fixed bin,	/*    array of codes.			*/
	1 obj_dflt_search_names	aligned based (Pobj_dflt_search_names),
						/* names on a 'search names' statement.		*/
	  2 M			fixed bin,	/*    maximum no of names structure will hold.	*/
	  2 N			fixed bin,	/*    current no of names in structure.		*/
	  2 ERROR			fixed bin,	/*    error message to print when struc overflows.*/
	  2 V (100 refer (obj_dflt_search_names.M))
				char(32) varying,	/*    array of names.			*/
	1 obj_root		aligned based (Pobj_root),
						/* object root definition structure.		*/
	  2 name,					/*    root names:				*/
	    3 Ifirst		fixed bin,	/*       index of first name in list.		*/
	    3 Ilast		fixed bin,	/*       index of last name in object root names.	*/
	    3 label		char(30) varying,	/*       ALM label used to reference root names.	*/
	  2 path			char(168) varying,	/*    path name of defined root.		*/
	  2 search_proc,				/*    entry point of procedure for searching root.*/
	    3 I			fixed bin,	/*       index of search procedure in obj table.	*/
	  2 type			fixed bin,	/*    root type.  2 = directory, 4 = archive	*/
	  2 Pstmt			ptr,		/*    ptr to root statement's descriptor.	*/
	1 obj_root_array		aligned based (Pobj_root_array),
						/* array of object root definition structures.	*/
	  2 M			fixed bin,	/*    maximum number of root definitions which	*/
						/*       the struc will hold.			*/
	  2 N			fixed bin,	/*    the current number of root definitions.	*/
	  2 obj_root (100 refer (obj_root_array.M))	/*    the root definition structures.		*/
				like obj_root,
	1 obj_root_name		aligned based (Pobj_root_name),
						/* full names of all of the roots defined so far.	*/
	  2 M			fixed bin,	/*    maximum number of names struc will hold.	*/
	  2 N			fixed bin,	/*    current number of names in structure.	*/
	  2 ERROR			fixed bin,	/*    error message to print when struc overflows.*/
	  2 V (5000 refer (obj_root_name.M))
				char(32) varying,	/*    array of names.			*/
	1 obj_search_proc		aligned based (Pobj_search_proc),
						/* entry points on the 'search procedure' stmt	*/
						/*    of a root definition.			*/
	  2 M			fixed bin,	/*    maximum no. of entry points struc will hold.*/
	  2 N			fixed bin,	/*    current no of entry points in struc.	*/
	  2 ename (30 refer (obj_search_proc.M)),	/*    array of entry point names.		*/
	    3 ref			char(32),		/*       reference name part of entry point name.	*/
	    3 ent			char(32),		/*       entry part of entry point name.	*/
	1 obj_star_code		aligned based (Pobj_star_code),
	  					/* the star code structure.			*/
	  2 M			fixed bin,	/*    maximum no of codes structure will hold.	*/
	  2 N			fixed bin,	/*    current no of codes in structure.		*/
	  2 C (0 refer (obj_star_code.N))
				fixed bin(35),	/*    array of codes.			*/
	out			char(Lout) based (Pout),
						/* overlay for the _r_e_m_a_i_n_d_e_r of the output segment*/
	stmt_array (stmt.Lvalue)	char(1) based (stmt.Pvalue),
						/* character array overlay for stmt_value.	*/
	stmt_part			char(j) based (stmt.Pvalue);
						/* partial overlay for stmt_value.		*/
 
     dcl (addr, addrel, dimension, divide, index, length, null, search, size, substr, verify)
				builtin;

     dcl						/*	entries				*/
	backup_name_		entry (char(*)) returns (char(32)),
	check_star_name_$entry	entry (char(*), fixed bin(35)),
	check_star_name_$path	entry (char(*), fixed bin(35)),
	clock_			entry returns (fixed bin(71)),
	com_err_			entry options(variable),
	cu_$arg_count		entry returns (fixed bin),
	cu_$arg_ptr		entry (fixed bin, ptr, fixed bin, fixed bin(35)),
	date_time_$format		entry (char(*), fixed bin(71), char(*), char(*)) returns(char(250) var),
	decode_entryname_		entry (char(*), char(32) aligned, char(32) aligned),
	expand_path_		entry (ptr, fixed bin, ptr, ptr, fixed bin(35)),
	get_wdir_			entry returns (char(168) aligned),
	hcs_$truncate_seg		entry (ptr, fixed bin, fixed bin(35)),
	initiate_file_		entry (char(*), char(*), bit(*), ptr, fixed bin(24), fixed bin(35)),
	lex_string_$lex		entry (ptr, fixed bin(21), fixed bin(21), ptr, bit(*) aligned,
				       char(*) aligned, char(*) aligned, char(*) aligned, char(*) aligned,
				       char(*) aligned, char(*) aligned varying, char(*) aligned varying,
				       char(*) aligned varying, char(*) aligned varying,
				       ptr, ptr, fixed bin(35)),
	lex_string_$init_lex_delims	entry (char(*) aligned, char(*) aligned, char(*) aligned, char(*) aligned,
				       char(*) aligned, bit(*) aligned, char(*) aligned varying,
				       char(*) aligned varying, char(*) aligned varying,
				       char(*) aligned varying),
	lex_error_		entry options(variable),
	suffixed_name_$make		entry (char(*), char(*), char(32), fixed bin(35)),
	suffixed_name_$new_suffix	entry (char(*), char(*), char(*), char(32), fixed bin(35)),
	terminate_file_		entry (ptr, fixed bin(24), bit(*), fixed bin(35)),
	translator_temp_$get_segment	entry (char(*), ptr, fixed bin(35)),
	translator_temp_$release_all_segments
				entry (ptr, fixed bin(35)),
	tssi_$clean_up_segment	entry (ptr),
	tssi_$finish_segment	entry (ptr, fixed bin(35), bit(36) aligned, ptr, fixed bin(35)),
	tssi_$get_segment		entry (char(*), char(*), ptr, ptr, fixed bin(35));

     dcl						/*	static variables			*/
	MLout			fixed bin(21) int static init (0),
						/* maximum length of an output segment (in chars).*/
	NL			char(1) int static options(constant) init ("
"),
	NP			char(1) int static options(constant) init (""),
						/* a new-page character.			*/
	breaks			char(7) varying aligned int static options(constant) init (" 	:()
"),							/* SP HT : ( ) NL NP			*/
						/* list of break characters.			*/
         (error_table_$badopt,
	error_table_$fatal_error,
	error_table_$noentry,
	error_table_$no_makeknown,
	error_table_$wrong_no_of_args)
				fixed bin(35) ext static,
	ignored_breaks		char(4) varying aligned int static options(constant) init (" 	
"),							/* SP HT NL NP				*/
						/* list of ignored break characters.		*/
	lex_control_chars		char(128) varying aligned int static,
	lex_delims		char(128) varying aligned int static init (""),
						/* lex_string_ control information.		*/
	proc			char(32) aligned int static options(constant)
				     init ("library_descriptor_compiler"),
	ring_no			pic "9" int static init(8),
						/* current ring number.			*/
	sys_info$max_seg_size	fixed bin(35) ext static;

     dcl	1 error_control_table (38)	aligned int static options(constant),
						/* error message text and specifications.	*/
	  /* 1     2     3     4     5     6     7     8     9    10    11    12    13    14    15	*/
	  /*16    17    18    19    20    21    22    23    24    25    26    27    28    29    30	*/
	  /*31    32    33    34    35    36    37    38    39    40    41    42    43    44    45	*/
	  2 severity		fixed bin(17) unaligned init (
	     2,    3,    1,    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
	     2,    2,    2,    2,    4,    2,    2,    2,    2,    2,    4,    2,    2,    2,    2,
	     2,    3,    4,    4,    2,    2,    3,    2),
	  2 Soutput_stmt		bit(1) unaligned init (
	    "1"b, "0"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "0"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b,
	    "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b,
	    "1"b, "0"b, "1"b, "0"b, "1"b, "1"b, "0"b, "1"b),
	  2 message		char(252) varying init (
	  /*  1 */
"A '^a' statement has an invalid format.  The statement has been
ignored.",
	  /*  2 */
"There are no statements in the library descriptor source segment.",
	  /*  3 */
"The first statement is not a 'Descriptor' statement.  A name of
default_descriptor has been assumed.",
	  /*  4 */
"A '^a' statement has an invalid format.  A ^a statement
without a name list has been assumed.",
	  /*  5 */
"An invalid entry name has been given in a '^a'
statement.  The statement has been ignored.",
	  /*  6 */
"The name used in the 'Descriptor' statement was not used in
the 'End' statement.  The proper name has been assumed.",
	  /*  7 */
"A 'Define' statement contains an invalid keyword.  Only
'Define: commands;' may be given.  The statement has been ignored.",
	  /*  8 */
"An unknown or misplaced statement has been encountered.
It has been ignored.",
	  /*  9 */
"The final 'End' statement is missing from the library descriptor
source.  One has been assumed.",
	  /* 10 */
"A name list ends when a name is expected.",
	  /* 11 */
"A Root name list ends when a right parenthesis ()) is expected.
The list will be processed as if the parenthesis were present.",
	  /* 12 */
"An invalid name, '^a', has been encountered in a name list.
It has been ignored.",
	  /* 13 */
"The library descriptor source ends in the middle of a statement.
Also, it does not end with an 'End' statement.",
	  /* 14 */
"An unexpected string, '^a', was found in a list when a comma (,),
or a semi-colon (;) was expected.  The remainder of the list
has been ignored.",
	  /* 15 */
"An invalid absolute path name was specified in a '^a' statement.
The statement will be ignored.",
	  /* 16 */
"",
	  /* 17 */
"Symbols appear after the 'End' statement.  These symbols
will be ignored.",
	  /* 18 */
"Restriction:  only ^d library names can be specified after
a 'Define: commands;' statement.  Name '^a' has been ignored.",
	  /* 19 */
"Restriction:  only ^d search names can be specified after
a 'Define: commands;' statement.  Search name '^a' has been ignored.",
	  /* 20 */
"Restriction:  the total number of library root names
cannot exceed ^d.  Name '^a' and all root names which
follow are in excess of this number.",
	  /* 21 */
"Restriction:  only ^d elements of a compound root name
may be defined.  Name element '^a' has been ignored.",
	  /* 22 */
"",
	  /* 23 */
"Restriction:  only ^d names can be defined for
the roots of the library.  Name '^a' has been ignored.",
	  /* 24 */
"",
	  /* 25 */
"'^a' is an invalid command name.  This statement has been
ignored.",
	  /* 26 */
"Restriction:  only ^d roots can be defined.  The following
root definition, and any which follow it, are in excess of
this number.",
	  /* 27 */
"No '^a' statement was given in the definition of a root.
The root definition has been ignored.",
 	  /* 28 */
"No '^a' statement was given in the definition of a root.
The definition has been ignored.",
	  /* 29 */
"A full root name is longer than 32 characters.  Full name
'^a' will be ignored.",
	  /* 30 */
"A full root name formed from a library name and a name on
the root statement has already been specified for another
root.  The name '^a' will be ignored.",
	  /* 31 */
"No legal full root names were defined for a root.  The root
definition will be ignored.",
	  /* 32 */
"The library descriptor source does not end with a complete
statement.",
	  /* 33 */
"Restriction: only ^d unique search procedures can be defined.
Search procedure '^a$^a' has been ignored.",
	  /* 34 */
"Restriction:  the library descriptor is too large, causing
the output segment to overflow.",
	  /* 35 */
"A 'library names' statement appears after a 'Define: commands;'
statement, but before a 'command' or 'unsupported command'
statement.  The 'library names' statement has been ignored.",
	  /* 36 */
"A 'search names' statement appears after a 'Define: commands;'
statement, but before a 'command' or 'unsupported command'
statement.  The 'search names' statement has been ignored.",
	  /* 37 */
"No legal root definitions appear in the library descriptor
source segment.",
	  /* 38 */
"An invalid root type '^a' appears in the 'type' statement
of a root definition.  A type of directory has been assumed."),
	  2 brief_message		char(40) varying init (
	  /*  1 */
"Bad '^a' stmt ignored.",
	  /*  2 */
"No source stmts.",
	  /*  3 */
"'Descriptor' stmt missing.",
	  /*  4 */
"Bad '^a' stmt.",
	  /*  5 */
"Bad entry name ignored.",
	  /*  6 */
"Bad name in 'End' stmt.",
	  /*  7 */
"Bad 'Define' keyword ignored.",
	  /*  8 */
"Bad stmt.",
	  /*  9 */
"'End' stmt missing.",
	  /* 10 */
"Name list ends too soon.",
	  /* 11 */
"Missing ) in name list.",
	  /* 12 */
"Bad name, '^a' ignored.",
	  /* 13 */
"Bad end of source.",
	  /* 14 */
"Bad '^a'.  List skipped.",
	  /* 15 */
"Bad absolute path ignored.",
	  /* 16 */
"",
	  /* 17 */
"Symbols after 'End' ignored.",
	  /* 18 - 24 */
(7)(1)">^d names.  '^a' ignored.",
	  /* 25 */
"Bad command name '^a' ignored.",
	  /* 26 */
">^d roots.  Roots ignored.",
	  /* 27 */
"Root '^a' missing.  Root ignored.",
	  /* 28 */
"Root '^a' missing.  Root ignored.",
	  /* 29 */
"Root name '^a' too long.",
	  /* 30 */
"Root name '^a' duplicated.",
	  /* 31 */
"No legal names.  Root ignored.",
	  /* 32 */
"Incomplete statement.",
	  /* 33 */
">^d search procs.  '^a$^a' ignored.",
	  /* 34 */
"Object segment overflow.",
	  /* 35 */
"Missing 'command' stmt.",
	  /* 36 */
"Missing 'command' stmt.",
	  /* 37 */
"No legal root definitions.",
	  /* 38 */
"Bad root type '^a'.  Directory assumed.");

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


	Nargs = cu_$arg_count();			/* complain if <1 or >2 arguments.		*/
	if Nargs < 1 then go to wnoa;
	if Nargs > 2 then go to wnoa;
	call cu_$arg_ptr (1, Parg, Larg, code);		/* get path name of input segment.		*/
	call expand_path_ (Parg, Larg, addr(dir_in), addr(ent_in), code);
	if code ^= 0 then go to bad_path;		/* expand path name to absolute form.		*/
	call suffixed_name_$make (ent_in, "ld", ent_in, code);
	if code ^= 0 then go to bad_input;		/* make sure entry name is properly suffixed.	*/
	call suffixed_name_$new_suffix (ent_in, "ld", "alm", ent_out, code);
	if code ^= 0 then go to bad_output_name;	/* insure name of output segment is suffixed OK.	*/
	dir_out = get_wdir_();			/* put output segment in working directory.	*/

	if Nargs > 1 then do;			/* process any control argument.		*/
	     call cu_$arg_ptr (2, Parg, Larg, code);
	     if arg = "-bf" then
		SERROR_CONTROL = "01"b;
	     else if arg = "-brief" then
		SERROR_CONTROL = "01"b;
	     else if arg = "-lg" then
		SERROR_CONTROL = "10"b;
	     else if arg = "-long" then
		SERROR_CONTROL = "10"b;
	     else
		go to badopt;
	     end;

	Ptemp_seg = null;				/* initialize pointers used by cleanup on unit.	*/
	Pin = null;
	Pout = null;
	on cleanup call cleaner;			/* cleanup temp seg, initiated segments when req'd*/

cleaner:	procedure;				/* This is a cleanup procedure.		*/
	if Ptemp_seg ^= null then
	     call translator_temp_$release_all_segments (Ptemp_seg, 0);
	if Pin ^= null then
	     call terminate_file_ (Pin, 0, TERM_FILE_TERM, 0);
						/* terminate source segment.			*/
	if Pout ^= null then			/* clean up out segment.			*/
	     call tssi_$clean_up_segment (Pacl_out);
	end cleaner;

          call initiate_file_ (dir_in, ent_in, R_ACCESS, Pin, bc_in, code);
	if Pin = null then go to bad_input;		/* initiate source segment.			*/
	Lin = divide (bc_in, 9, 35, 0);		/* convert bit count to char count.		*/
	call translator_temp_$get_segment ((proc), Ptemp_seg, code);
	if code ^= 0 then go to bad_area;

	call tssi_$get_segment (dir_out, ent_out, Pout, Pacl_out, code);
	if code ^= 0 then go to bad_output;		/* get ptr to output segment.			*/
	if MLout = 0 then				/* initialize limit on max. chars in output seg.	*/
	     MLout = sys_info$max_seg_size * 4;

	Pfirst_name_elements = null;
	Pstmt, Pthis_token = null;			/* start out with no input tokens.		*/
	if length(lex_delims) = 0 then		/* initialize static variables used by lex_string_*/
	     call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b, breaks, ignored_breaks,
		lex_delims, lex_control_chars);
	call lex_string_$lex (Pin, Lin, 0, Ptemp_seg, "1000"b, """", """", "/*", "*/", ";",
	     breaks, ignored_breaks, lex_delims, lex_control_chars, null, Pthis_token, code);
	if code ^= 0 then call ERROR(32);		/* parse input into tokens.			*/
	if Pthis_token = null then go to RETURN;	/* a really fatal error occurred in parsing.	*/
	code = 0;					/* clear error code for use below.		*/
	call SEMANTIC_ANALYSIS();			/* This one call does all the work.		*/
RETURN:	if MERROR_SEVERITY > 2 then do;		/* Fatal error?  No output created.		*/
	     Lout = 0;
	     if code = 0 then
		code = error_table_$fatal_error;
	     end;
	if Lout = 0 then				/* error if output segment has zero length.	*/
	     call hcs_$truncate_seg (Pout, 0, 0);	/* even tho char count is zero, truncate to free	*/
						/* records used by output segment.		*/
	call tssi_$finish_segment (Pout, Lout * 9, "1000"b, Pacl_out, 0);
	Pout = null;				/* finish up now.				*/
	call cleaner;				/* clean up areas, initiated source.		*/
	if code ^= 0 then go to error;		/* report any errors to user.			*/
	return;					/* That's All Folks!			*/

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

wnoa:	call com_err_ (error_table_$wrong_no_of_args, proc,
	     "^/Calling sequence:^-library_descriptor_compiler pathname -option-
where pathname is:^-the relative path name of the library descriptor source segment.
        option is:^--long | -lg | -brief | -bf");
	return;

badopt:	call com_err_ (error_table_$badopt, proc, arg);
	return;

bad_path:
	call com_err_ (code, proc, " ^R^a^B", arg);
	return;

bad_input:
	if code = error_table_$no_makeknown then code = error_table_$noentry;
	call com_err_ (code, proc, " ^R^a>^a^B", dir_in, ent_in);
	return;

bad_output_name:
	call suffixed_name_$new_suffix (ent_in, "ld", "", ent_out, 0);
	call com_err_ (code, proc, "^a.alm^/While creating the entry name for the output segment.", ent_out);
	return;

bad_output:
	call com_err_ (code, proc, "^/While creating the output segment (^R^a>^a>B).", dir_out, ent_out);
	call cleaner;
	return;

error:	call com_err_ (code, proc, "^/No output segment will be generated.");
	return;

bad_area:	call com_err_ (code, proc, "^/While creating a temporary segment in the process directory.");
	call cleaner;
	return;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/*	T  O  K  E  N     R  E  Q  U  I  R  E  M  E  N  T     F  U  N  C  T  I  O  N  S	*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


absolute_pathname:	procedure returns (bit(1) aligned);	/* This token requirement function determines	*/
						/* whether the current token is an absolute path.	*/

     dcl (Igreater, Inext_greater, Lentryname)
				fixed bin;

	if token.Lvalue > 0 then
	     if token.Lvalue <= 168 then
		if search (token_value, "<") = 0 then
		     if substr (token_value,1,1) = ">" then do;
			Igreater = 1;
			do while (Igreater < token.Lvalue);
			     Inext_greater = index(substr(token_value,Igreater+1),">");
			     if Inext_greater = 0 then
			          Inext_greater = token.Lvalue - (Igreater - 1);
			     Lentryname = Inext_greater - 1;
			     if Lentryname = 0 then
			          go to reject;
			     if Lentryname > 32 then
			          go to reject;
			     Igreater = Igreater + Inext_greater;
			     end;
			call check_star_name_$path (token_value, code);
			if code = 0 then
			     return ("1"b);
			end;
reject:	return ("0"b);

	end absolute_pathname;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

command_name_:	procedure returns (bit(1) aligned);	/* This token requirement function checks that a	*/
						/* token names one of the library maintenance	*/
						/* commands (eg, those commands which can use the	*/
						/* library descriptor).			*/

     dcl	i 			fixed bin;	/* a do-group index.			*/

	do i = 1 to dimension (command_name,1) while (token_value ^= command_name(i));
	     end;					/* see if token matches a command name.		*/
	if i > dimension (command_name,1) then do;	/* if not, see about command name abbreviation.	*/
	     do i = 1 to dimension (command_abbrev,1) while (token_value ^= command_abbrev(i));
		end;
	     if i > dimension (command_abbrev,1) then
		return ("0"b);			/* no match.  Oh, well.			*/
	     end;
	Icommand = i;				/* save index of command for later use.		*/
	return ("1"b);

	end command_name_;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


descriptor_name:	procedure returns (bit(1) aligned);	/* This token requirement function checks that the*/
						/* library descriptor name given in an 'End'	*/
						/* statement is the same as that given in a	*/
						/* 'Descriptor' statement.			*/
	if token_value = obj_desc.name then
	     return ("1"b);
	else
	     return ("0"b);

	end descriptor_name;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


entryname:	procedure returns (bit(1) aligned);	/* This token requirement function checks that	*/
						/* a reference to a procedure entry point has a	*/
						/* correct format.  Acceptable formats are:	*/
						/*    reference_name$entry_point_name		*/
						/*    reference_name (equivalent to		*/
						/*	reference_name$reference_name)	*/
						/* It decodes the input name into its two parts	*/
						/* and stores these in the ename structure.	*/

     dcl	Idollar			fixed bin;

	if token.Lvalue > 0 then
	     if token.Lvalue <= 65 then do;
		Idollar = index (token_value, "$");
		if Idollar = 0 then
		     Idollar = token.Lvalue;
		else
		     Idollar = Idollar - 1;
		if Idollar <= 32 then
		     if verify (substr(token_value,1,1),"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
		     = 0 then
			if verify (token_value,"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$")
			= 0 then do;
			     call decode_entryname_ (token_value, ename.ref, ename.ent);
			     if ename.ent ^= "" then
				return("1"b);	/* exclude case of reference_name$		*/
			     end;
		end;
	return("0"b);

	end entryname;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


starname:	procedure returns (bit(1) aligned);		/* This token requirement function checks that a	*/
						/* token is a valid storage system entry name	*/
						/* which may be a star name.			*/
 	if token.Lvalue > 0 then
	     if token.Lvalue <= 32 then do;
		call check_star_name_$entry (token_value, starcode);
		if starcode = 0 then
		     return("1"b);
		if starcode = 1 then
		     return("1"b);
		if starcode = 2 then
		     return("1"b);
		end;				/* save starcode for use by set_obj_star_code.	*/
	return("0"b);

	end starname;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


valid_name:	procedure returns (bit(1) aligned);	/* This token requirement function checks that a	*/
						/* library name, library group name, or root	*/
						/* name is valid.				*/

	if token.Lvalue > 0 then
	     if token.Lvalue <= 32 then
		if search (token_value, "(),;<>*?%=") = 0 then
		     return("1"b);
	return("0"b);


null_string_name:
	entry returns(bit(1) aligned);
	
	if token.S.quoted_string &
	     token.Lvalue = 0 then
	          return("1"b);
	return("0"b);

	end valid_name;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/*		  A  C  T  I  O  N        R  O  U  T  I  N  E  S			*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */


combine_elements:	procedure(Pname_elements_, name_sofar);	/* This action routine combines the sets of name	*/
						/*   elements from a compound root name into	*/
						/*   complete root names.			*/

     dcl	Pname_elements_		ptr,		/* ptr to name_elements structure at this level 	*/
						/*   of recursion.				*/
	name_sofar		char(100) varying,	/* part of a complete name constructed so far.	*/
	1 name_elements_		based(Pname_elements_),
	  2 header		like name_elements.header,
	  2 V (50 refer (name_elements_.N))
				char(32) varying,
						/* copy of name_elements structure based upon	*/
						/*   our input argument.			*/
	i			fixed bin,	/* a do-group index.			*/
	l			fixed bin;	/* length of name so far, on entrance.		*/

	l = length(name_sofar);			/* record original length of input for reuse later*/
	do i = 1 to name_elements_.N;			/* index through all names at this recursion level*/
	     if name_elements_.V(i) = "" then;		/*   use name_sofar if our name element is null.	*/
	     else if name_sofar = "" then		/*   use just our element if name_sofar is null.	*/
		name_sofar = name_elements_.V(i);
	     else name_sofar = name_sofar || "." || name_elements_.V(i);

	     if name_elements_.Pnext = null then do;	/* Case 1:  no additional element structures.	*/
add_name:						/* add the name to obj_root_name list.		*/
		if name_sofar = "" then;		/*   do nothing with complete names which are null*/
		else if length(name_sofar) > 32 then	/*   check for complete names which are too long.	*/
		     call lex_error_ (29, SERROR_PRINTED(29), (error_control_table(29).severity),
			MERROR_SEVERITY, obj_root.Pstmt, null, SERROR_CONTROL,
			(error_control_table(29).message), (error_control_table(29).brief_message),
			name_sofar);
		else if obj_root_name.N = obj_root_name.M then
		     call lex_error_ (obj_root_name.ERROR, SERROR_PRINTED(obj_root_name.ERROR),
			(error_control_table(obj_root_name.ERROR).severity), MERROR_SEVERITY,
			obj_root.Pstmt, null, SERROR_CONTROL,
			(error_control_table(obj_root_name.ERROR).message),
			(error_control_table(obj_root_name.ERROR).brief_message),
			obj_root_name.M, name_sofar);	/*   complain if obj_root_name list is full.	*/
		else do;
		     obj_root_name.N = obj_root_name.N + 1;
		     obj_root_name.V(obj_root_name.N) = name_sofar;
		     end;
		end;

	     else if name_elements_.Pnext -> name_elements_.N = 0 then
		go to add_name;			/* Case 2:  there is a next name element struc,	*/
						/*	    but no names in it.		*/
	     else					/* Case 3:  there are more element structures.	*/
		call combine_elements (name_elements_.Pnext, name_sofar);
	     name_sofar = substr(name_sofar,1,l);	/* reset to original value on input to this 	*/
	     end;					/*   level of the subroutine.			*/

	end combine_elements;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */
 
/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

command_begin:	procedure;			/* This action routine performs the prologue	*/
						/* functions necessary for defining command	*/
						/* default values.				*/

	obj_command_dflt_values(Icommand).S.supported = Scommand;
						/* record whether or not command is supported.	*/
	if Scommand then do;			/* if supported, initialize library name and	*/
						/* search name lists.			*/
	     obj_command_dflt_values(Icommand).lib_names.Ifirst = obj_dflt_lib_names.N + 1;
	     obj_command_dflt_values(Icommand).lib_names.Ilast = obj_dflt_lib_names.N;
	     obj_command_dflt_values(Icommand).search_names.Ifirst = obj_dflt_search_names.N + 1;
	     obj_command_dflt_values(Icommand).search_names.Ilast = obj_dflt_search_names.N;
	     end;

	end command_begin;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


command_end:	procedure;			/* This action routine performs epilogue	*/
						/* functions necessary to complete the definition	*/
						/* of command default values.			*/

	if Icommand = 0 then			/* ignore bad definition block.		*/
	     return;
	obj_command_dflt_values(Icommand).lib_names.Ilast = obj_dflt_lib_names.N;
	obj_command_dflt_values(Icommand).search_names.Ilast = obj_dflt_search_names.N;
						/* set upper bounds of name lists.		*/

	end command_end;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

compile_descriptor:	procedure;			/* This action routine converts the information	*/
						/* in the tables filled in by other action rtns	*/
						/* into ALM code which is written into the output	*/
						/* segment.				*/

	if obj_root_array.N = 0 then			/* don't generate output if no roots defined.	*/
	     call ERROR(37);
	if MERROR_SEVERITY > 2 then			/* don't go any further if a fatal error occurred.*/
	     return;
	Lout = MLout;				/* start out with an empty output segment	*/
						/*    (all of the segment remaining).		*/

						/* output header.				*/
	call OUT("
	""*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *""
	""*							*""
	""*  COMPILED OUTPUT FROM SEGMENT  ");
	call OUT(ent_in);
	call OUT("	*""
	""*  Compiled by:  library_descriptor_compiler,			*""
	""*		    Version 3.3 of January 14, 1986 		*""
	""*  Compiled on:  ");
	compilation_date = date_time_$format ("date_time", clock_(), "", "");
	call OUT(compilation_date);
	call OUT("*""
	""*     Refer to:  lib_descriptor_.incl.pl1			*""
	""*		    for a declaration of entries in this database *""
	""*							*""
	""*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *""");

						/* output name and descriptor segdef.		*/
	call OUT("

	name	");
	call OUT(obj_desc.name);
	call OUT("
	segdef	descriptor");

						/* output descriptor structure.		*/
	call OUT("

descriptor:					"" 1 descriptor,
	dec	   2				""   2 version,
	aci	""");
	call OUT(obj_desc.name);
	call OUT("""	""   2 name,
						""   2 command_default_values,
	vfd	18/0,18/command_default_values	""     3 O,");
	call OUT("
						""   2 roots,
	vfd	18/0,18/roots			""     3 O;");

						/* output command_default_values structure.	*/
	call OUT("
						"" ");
	call OUT(NP);
	call OUT("
	even
command_default_values:				"" 1 command_default_values,");
	call OUT ("
	dec	");
	call OUTN(dimension(command_name,1));
	call OUT ("				""   2 N,
						""   2 group (");
	call OUTVN (dimension(command_name,1));
	call OUT (")");
	do i = 1 to dimension(command_name,1);
	     call OUT(",
						""     3 S,/* (");
	     call OUTVN(i);
	     call OUT(": ");
	     call OUTV(command_name(i));
	     call OUT(") */
	oct	");
	     if obj_command_dflt_values(i).S.supported then
		call OUT("000000000000");
	     else
		call OUT("400000000000");
	     call OUT	("			""       4 unsupported,
						""     3 library_names,
	vfd	18/0,18/library_names_");
	     call OUTVN(i);
	     call OUT		("		""       4 O,
						""     3 search_names,
	vfd	18/0,18/search_names_");
	     call OUTVN (i);
	     call OUT		("		""       4 O");
	     end;
	call OUT(";");

						/* output default library names arrays.		*/
	call OUT("
						"" ");
	call OUT(NP);
	do i = 1 to dimension(command_name,1);
	     call OUT ("
	even
library_names_");
	     call OUTVN(i);
	     call OUT
	(":					"" ");
	     call OUTV(command_name(i));
	     call OUT ("
						"" 1 library_names,
	dec	");
	     j = obj_command_dflt_values(i).lib_names.Ilast -
	         obj_command_dflt_values(i).lib_names.Ifirst + 1;
	     call OUTVN (j);
	     call OUT ("				""   2 N,
						""   2 group (");
	     call OUTVN(j);
	     call OUT (")");
	     do j = obj_command_dflt_values(i).lib_names.Ifirst
	         to obj_command_dflt_values(i).lib_names.Ilast by 1;
		call OUT (",
	aci	""");
		temp_name = obj_dflt_lib_names.V(j);
		call OUT(temp_name);
		call OUT			("""	""     3 V,
	dec	");
		call OUTVN (obj_dflt_lib_codes.C(j));
		call OUT
		("				""     3 C");
		end;
	     call OUT (";
");
	     end;

						/* output default search names arrays.		*/
	call OUT("
						"" ");
	call OUT(NP);
	do i = 1 to dimension(command_name,1);
	     call OUT ("
	even
search_names_");
	     call OUTVN(i);
	     call OUT
	(":					"" ");
	     call OUTV(command_name(i));
	     call OUT ("
						"" 1 search_names,
	dec	");
	     j = obj_command_dflt_values(i).search_names.Ilast -
	         obj_command_dflt_values(i).search_names.Ifirst + 1;
	     call OUTVN (j);
	     call OUT ("				""   2 N,
						""   2 group (");
	     call OUTVN(j);
	     call OUT (")");
	     do j = obj_command_dflt_values(i).search_names.Ifirst
	         to obj_command_dflt_values(i).search_names.Ilast by 1;
		call OUT (",
	aci	""");
		temp_name = obj_dflt_search_names.V(j);
		call OUT(temp_name);
		call OUT			("""	""     3 V,
	dec	");
		call OUTVN (obj_dflt_search_codes.C(j));
		call OUT
		("				""     3 C");
		end;
	     call OUT (";
");
	     end;

						/* output root definition structure.		*/
	call OUT("
						"" ");
	call OUT(NP);
	call OUT("
	even
roots:						"" 1 roots,
	dec	");
	call OUTN (obj_root_array.N);
	call OUT ("				""   2 N,
						""   2 root (");
	call OUTVN (obj_root_array.N);
	call OUT(")");
	do i = 1 to obj_root_array.N;
	     call OUT(",
""
""	");
	     Pobj_root = addr(obj_root_array.obj_root(i));
	     Pstmt = obj_root.Pstmt;
	     j = index(stmt_value, NL);
	     do while (j > 0);
		call OUT (stmt_part);
		call OUT ("""	");
		stmt.Pvalue = addr(stmt_array(j+1));
		stmt.Lvalue = stmt.Lvalue - j;
		j = index (stmt_value, NL);
		end;
	     call OUT (stmt_value);
	     call OUT ("
	even					""          (");
	     call OUTVN(i);
	     call OUT(")
						""     3 name,
	vfd	18/0,18/.");
	     temp_name30 = "";			/* find a unique, 30 char root name to use as	*/
						/*   label on list of root names.		*/
	     do j = obj_root.name.Ifirst to obj_root.name.Ilast;
		if length(obj_root_name.V(j)) <= 30 then do;
		     temp_name30 = obj_root_name.V(j);	/*     save 1st possibility, in case none unique.	*/
		     do k = 1 to i-1;
			if obj_root_array.obj_root(k).name.label = obj_root_name.V(j) then go to NAME_DUP;
			end;
		     go to NAME_FOUND;
		     end;
NAME_DUP:		end;
	     if temp_name30 = "" then			/*    no names short enough.			*/
		temp_name30 = obj_root_name.V(obj_root.name.Ifirst);
	     do k = 1 to i-1;			/*    make chosen name unique.		*/
		if obj_root_array.obj_root(k).name.label = temp_name30 then do;
		     temp_name30 = substr(backup_name_ (":." || temp_name30),3);
		     temp_name30 = substr(temp_name30,1,length(temp_name30)+1-verify(reverse(temp_name30)," "));
		     end;
		end;
NAME_FOUND:    obj_root.name.label = temp_name30;
	     call OUTV(temp_name30);
	     call OUT ("
						""       4 O,
	dec	");
	     call OUTN(length(obj_root.path));
	     call OUT("				""     3 path,
	aci	""");
	     path = obj_root.path;			/* assign output path to fixed-length char string.*/
	     do j = 1 to 129 by 32;			/* output path name in 32 char ch.		*/
		call OUT(substr(path,j,32));
		call OUT("""
	aci	""");
		end;
	     call OUT(substr(path,161,8));
	     call OUT("""
	dec	");
	     call OUTN(obj_root.type);
	     call OUT("				""     3 type,
	dec	");
	     j = obj_root.search_proc.I;
	     entry_point = obj_search_proc.ename.ref(j);
	     entry_point_name = substr(entry_point,1,length(entry_point) + 1 - verify(reverse(entry_point)," "));
	     if obj_search_proc.ename.ent(j) ^= "" then do;
		entry_point_name = entry_point_name || "$";
		entry_point = obj_search_proc.ename.ent(j);
		entry_point_name = entry_point_name ||
		     substr(entry_point,1,length(entry_point) + 1 - verify(reverse(entry_point)," "));
		end;
	     call OUTN (length(entry_point_name));
	     call OUT ("				""     3 search_proc_name,
	aci	""");
	     entry_point = entry_point_name;
	     call OUT (substr(entry_point,1,68));
	     call OUT ("""
	even
	itp	bp,search_procs+(");
	     call OUTVN(obj_root.search_proc.I);
	     call OUT("*2)-*
	its	-1,1				""     3 search_proc");
	     end;
	call OUT(";");

						/* output root names arrays.			*/
	call OUT("
						"" ");
	call OUT(NP);
	do i = 1 to obj_root_array.N;
	     call OUT ("
	even
.");
	     Pobj_root = addr (obj_root_array.obj_root(i));
	     call OUTV (obj_root.name.label);
	     call OUT (":
						"" 1 root_names,
	dec	");
	     j = obj_root.name.Ilast - obj_root.name.Ifirst + 1;
	     call OUTVN (j);
	     call OUT ("				""   2 N,
						""   2 root_name (");
	     call OUTVN(j);
	     call OUT (");");
	     do j = obj_root.name.Ifirst to obj_root.name.Ilast by 1;
		call OUT ("
	aci	""");
		temp_name = obj_root_name.V(j);
		call OUT(temp_name);
		call OUT			("""");
		end;
	     end;


						/* output the search procedure transfer vector.	*/
	call OUT("
						"" ");
	call OUT(NP);
	call OUT("
	even
search_procs:					"" search procedure transfer vector
	dec	0
	dec	0");
	do i = 1 to obj_search_proc.N;
	     call OUT("
	getlp					""		/* (");
	     call OUTVN(i);
	     call OUT(") */
	tra	");
	     entry_point = obj_search_proc.ename.ref(i);
	     entry_point_name = "<" ||
		substr (entry_point,1,length(entry_point) + 1 - verify(reverse(entry_point)," ")) || ">";
	     if obj_search_proc.ename.ent(i) = "" then
		entry_point_name = entry_point_name || "|0";
	     else do;
		entry_point = obj_search_proc.ename.ent(i);
		entry_point_name = entry_point_name || "|[" ||
		     substr(entry_point,1,length(entry_point)+1-verify(reverse(entry_point)," ")) || "]";
		end;
	     call OUTV(entry_point_name);
	     end;

	call OUT("

	end
");

	Lout = MLout - Lout;			/* convert Lout to a count of _u_s_e_d chars in output*/

	end compile_descriptor;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


descriptor_begin:	procedure;			/* This procedure performs prologue functions	*/
						/* for the library descriptor compiler.	 	*/

	Pobj_dflt_lib_codes = allocate(Ptemp_seg, size(obj_dflt_lib_codes));
	obj_dflt_lib_codes.M = 100;
	Pobj_dflt_search_codes = allocate(Ptemp_seg, size(obj_dflt_search_codes));
	obj_dflt_search_codes.M = 100;
	Pobj_dflt_lib_names = allocate(Ptemp_seg, size(obj_dflt_lib_names));
	obj_dflt_lib_names.M = 100;
	Pobj_dflt_search_names = allocate(Ptemp_seg, size(obj_dflt_search_names));
	obj_dflt_search_names.M = 100;
	Pobj_root_name = allocate(Ptemp_seg, size(obj_root_name));
	obj_root_name.M = 5000;
	Pobj_root_array = allocate(Ptemp_seg, size(obj_root_array));
	obj_root_array.M = 100;
	Pobj_search_proc = allocate(Ptemp_seg, size(obj_search_proc));
	obj_search_proc.M = 30;

	obj_dflt_lib_codes.N = 0;			/* initialize star code structures.		*/
	obj_dflt_search_codes.N = 0;

	Pname_elements = null;			/* initialize name structures.		*/
	obj_dflt_lib_names.N = 0;
	obj_dflt_lib_names.ERROR = 18;
	obj_dflt_search_names.N = 0;
	obj_dflt_search_names.ERROR = 19;
	obj_root_name.N = 0;

	obj_root_array.N = 0;			/* initialize the array of object roots.	*/
	obj_search_proc.N = 0;			/* initialize the array of search procedures.	*/

	do i = 1 to dimension (obj_command_dflt_values,1);/* initialize command defaults structure.	*/
	     obj_command_dflt_values(i).S.supported = "0"b;
	     obj_command_dflt_values(i).lib_names.Ifirst = 0;
	     obj_command_dflt_values(i).lib_names.Ilast = -1;
	     obj_command_dflt_values(i).search_names.Ifirst = 0;
	     obj_command_dflt_values(i).search_names.Ilast = -1;
	     end;

	end descriptor_begin;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */



new_element:	procedure(Pname_elements_next);	/* This action routine gets space for the next	*/
						/*   set of name elements in a compound name.	*/

     dcl	Pname_elements_next		ptr,		/* Pointer to the next name_elements structure.	*/
	1 name_elements_next	based(Pname_elements_next),
	  2 header		like name_elements.header,
	  2 V (50 refer (name_elements_next.N))
				char(32) varying;

	if Pname_elements_next = null then do;		/* If no next structure has been allocated, do it.*/
	     Pname_elements_next = allocate(Ptemp_seg, size(name_elements_next));
	     name_elements_next.M = 50;
	     name_elements_next.ERROR = 21;
	     name_elements_next.Pnext = null;
	     end;
	name_elements_next.N = 0;			/* there are no names in this new list yet.	*/
	if name_elements_next.Pnext^= null then		/* initialize next next name_elements strucuture	*/
	     name_elements_next.Pnext -> name_elements_next.N = 0;
						/*   if any.				*/
	Pname_elements = addr(name_elements_next);	/* set current name_elements structure to one we	*/
						/*   just allocated.			*/
	Pname = addr(name_elements_next.M);		/* set set_elements structure pointer to next	*/
						/*   name_elements_next structure.			*/
	end new_element;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

root_begin:	procedure;			/* This action routine performs prologue functions*/
						/* necessary prior to defining a new root.	*/

	if obj_root_array.N = obj_root_array.M then do;	/* complain if no more roots can be defined.	*/
	     call lex_error_ (26, SERROR_PRINTED(26), (error_control_table(26).severity), MERROR_SEVERITY,
		obj_root.Pstmt, null, SERROR_CONTROL, (error_control_table(26).message),
		(error_control_table(26).brief_message), obj_root_array.M);
	     go to RETURN;
	     end;
	obj_root_array.N = obj_root_array.N + 1;	/* address the next root.			*/
	Pobj_root = addr(obj_root_array.obj_root(obj_root_array.N));

	obj_root.path = "";				/* initialize the root.			*/
	search_proc.ename.ref = "";
	search_proc.ename.ent = "";
	obj_root.search_proc.I = 0;

	obj_root.type = Tdirectory;			/* assume root is a directory, by default.	*/
	obj_root.Pstmt = token.Pstmt;			/* save pointer to root's statement descriptor.	*/
						/* This will be used in error messages.		*/

	Pname = addr(obj_root_name);
	obj_root.name.Ifirst = obj_root_name.N+1;	/* save index of first name for this root.	*/

	end root_begin;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


root_end:	procedure;				/* This action routine perform epilogue functions	*/
						/* necessary to defining a root.		*/
						/* Steps include:  insuring that definition is	*/
						/* consistent and complete;  applying defaults to	*/
						/* unspecified values;  constructing the root's	*/
						/* list of full names.			*/

	Sreject_root = "0"b;
	if obj_root.path = "" then do;		/* complain if path name unspecified.		*/
	     call lex_error_ (27, SERROR_PRINTED(27), (error_control_table(27).severity), MERROR_SEVERITY,
		obj_root.Pstmt, null, SERROR_CONTROL, (error_control_table(27).message),
		(error_control_table(27).brief_message), "path");
	     Sreject_root = "1"b;
	     end;
	if search_proc.ename.ref = "" then do;	/* complain if search procedure unspecified.	*/
	     call lex_error_ (27, SERROR_PRINTED(27), (error_control_table(27).severity), MERROR_SEVERITY,
		obj_root.Pstmt, null, SERROR_CONTROL, (error_control_table(27).message),
		(error_control_table(27).brief_message), "search procedure");
	     Sreject_root = "1"b;
	     end;

	do i = 1 to obj_search_proc.N;		/* add search procedure to table.		*/
	     if search_proc.ename.ref = obj_search_proc.ename(i).ref then
		if search_proc.ename.ent = obj_search_proc.ename(i).ent then
		     go to already_there;
	     end;
	if i > obj_search_proc.M then do;		/* complain if the table is full.		*/
	     call lex_error_ (33, SERROR_PRINTED(33), (error_control_table(33).severity), MERROR_SEVERITY,
		obj_root.Pstmt, null, SERROR_CONTROL, (error_control_table(33).message),
		(error_control_table(33).brief_message), obj_search_proc.M, search_proc.ename.ref,
		search_proc.ename.ent);
	     go to RETURN;
	     end;
	obj_search_proc.N = i;
	obj_search_proc.ename(i) = search_proc.ename;
already_there:
	obj_root.search_proc.I = i;			/* fill table index into root structure.	*/

	obj_root.name.Ilast = obj_root_name.N;		/* set upper bound on root's name list.		*/
	if obj_root.name.Ifirst > obj_root.name.Ilast then do;
			   			/* complain if no legal names found for root.	*/
	     call lex_error_ (31, SERROR_PRINTED(31), (error_control_table(31).severity), MERROR_SEVERITY,
		obj_root.Pstmt, null, SERROR_CONTROL, (error_control_table(31).message),
		(error_control_table(31).brief_message));
	     Sreject_root = "1"b;
	     end;

	if Sreject_root then do;			/* some error requires that we reject this root.	*/
	     obj_root_name.N = obj_root.name.Ifirst - 1;	/* ignore any root names which were defined.	*/
	     obj_root_array.N = obj_root_array.N - 1;	/* ignore the root.				*/
	     end;

	end root_end;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


set_element:
set_name:	procedure;				/* This action routine adds a name to the current	*/
						/* name list.				*/

	if name.N = name.M then			/* make sure there is room for another name.	*/
	     call lex_error_ (name.ERROR, SERROR_PRINTED(name.ERROR), (error_control_table(name.ERROR).severity),
		MERROR_SEVERITY, addrel(token.Pstmt,0), null, SERROR_CONTROL,
		(error_control_table(name.ERROR).message), (error_control_table(name.ERROR).brief_message),
		name.M, token_value);
	else do;
	     name.N = name.N + 1;
	     name.V(name.N) = token_value;		/* add name to the table.			*/
	     end;

	end set_name;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


set_obj_star_code:	procedure;			/* This action routine adds a starcode to the	*/
						/* current starcode list.			*/

	if obj_star_code.N = obj_star_code.M then;	/* list full; message already printed by set_name.*/
	else do;
	     obj_star_code.N = obj_star_code.N + 1;
	     obj_star_code.C(obj_star_code.N) = starcode;
	     end;

	end set_obj_star_code;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/*		  O  U  T  P  U  T        R  O  U  T  I  N  E  S			*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


OUT:	procedure (value);				/* This procedure outputs a character string into	*/
						/* the output segment.			*/

     dcl	value			char(*);		/* character string to be output.		*/

	if length(value) > Lout then do;		/* if no room for string in output seg, quit.	*/
	     call ERROR(34);
	     go to RETURN;
	     end;
	substr (out, 1, length(value)) = value;
	Pout = addr (substr (out, length(value)+1));
	Lout = Lout - length(value);

	end OUT;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


OUTV:	procedure (value);				/* This procedure outputs a character string into	*/
						/* the output segment.			*/

     dcl	value			char(*) varying;	/* character string to be output.		*/

	if length(value) > Lout then do;		/* if no room for string in output seg, quit.	*/
	     call ERROR(34);
	     go to RETURN;
	     end;
	substr (out, 1, length(value)) = value;
	Pout = addr (substr (out, length(value)+1));
	Lout = Lout - length(value);

	end OUTV;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


OUTN:	procedure (N);				/* This procedure outputs a number into the	*/
						/* output segment.				*/

     dcl	N			fixed bin,	/* Number to be output.			*/
	Nchar			pic "---9";	/* character string representation of the number.	*/

	Nchar = N;
	if length(Nchar) > Lout then do;		/* make sure number will fit in output segment.	*/
	     call ERROR(34);
	     go to RETURN;
	     end;
	substr (out, 1, length(Nchar)) = Nchar;
	Pout = addr (substr (out, length(Nchar)+1));
	Lout = Lout - length(Nchar);

	end OUTN;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

OUTVN:	procedure(N);				/* This procedure outputs a number, stripped of 	*/
						/* any leading spaces, into the output segment.	*/

     dcl	N			fixed bin,	/* number to be output.			*/
	Nchar			pic "---9",	/* character string representation of the number.	*/
	Isignificant		fixed bin;	/* index of first significant character of number.*/

	Nchar = N;				/* convert number to character representation.	*/
	Isignificant = verify (Nchar," ");		/* get index of first significant character.	*/
	call OUT(substr(Nchar,Isignificant));		/* output significant digits of the number.	*/

	end OUTVN;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


%include translator_temp_alloc;

%include access_mode_values;

%include lib_descriptor_;

%include lib_node_;

%include terminate_file;
