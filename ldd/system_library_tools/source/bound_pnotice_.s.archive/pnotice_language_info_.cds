/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(81-05-14,Stansbury), approve(), audit(),
     install(86-08-21,MR12.0-1138):
     Created.
  2) change(82-02-02,Stansbury), approve(), audit(),
     install(86-08-21,MR12.0-1138):
     Modified - Added the 'pmac' suffix for the PL/1 macro expander.
  3) change(82-06-04,Stansbury), approve(), audit(),
     install(86-08-21,MR12.0-1138):
     Modified - Added the 'ld' suffix for the library descriptor language.
  4) change(82-06-17,Stansbury), approve(), audit(),
     install(86-08-21,MR12.0-1138):
     Modified - Added the following suffixes: pascal, cmdb, header, mexp,
     runoff, linus
  5) change(82-09-28,Stansbury), approve(), audit(),
     install(86-08-21,MR12.0-1138):
     Modified - Added the ted and qedx suffixes.
  6) change(85-09-03,LJAdams), approve(85-09-27,MCR7150),
     audit(86-02-07,Wallman), install(86-02-13,MR12.0-1017):
     Added  Type  4  and  Type  5.   Type  4  allows the comment begin
     characters  to  be  "/****^  "  so  the  history comments will be
     indented  properly  by  format  pl1.   Type  5  is for runoff and
     compose files.  Blank lines will not be inserted before and after
     the history comment block as they are interpreted as space blocks
     by compose.
  7) change(85-11-13,LJAdams), approve(85-11-13,MCR7150),
     audit(86-02-07,Wallman), install(86-02-13,MR12.0-1017):
     Changed pascal begin/end delimiters to (* and *) respectively.  Added xdw
     language type suffix.  Changed pmac suffix to type 4.
  8) change(86-01-29,LJAdams), approve(86-01-29,MCR7150),
     audit(86-02-07,Wallman), install(86-02-13,MR12.0-1017):
     Added support for "C" language and micro_assembler.
  9) change(86-07-28,LJAdams), approve(86-08-01,MCR7509),
     audit(86-08-05,Blair), install(86-08-21,MR12.0-1138):
     Added support for lap language.
 10) change(86-09-08,LJAdams), approve(86-09-08,MCR7526),
     audit(86-11-05,GDixon), install(86-11-12,MR12.0-1213):
     Added .cmf, .ttf, .rtmf, .ssl, .teco, and iodt suffixes.
 11) change(86-09-23,LJAdams), approve(86-09-23,MCR7526),
     audit(86-11-05,GDixon), install(86-11-12,MR12.0-1213):
     Added support for bind_fnp suffix.
 12) change(87-03-16,LJAdams), approve(87-04-22,MCR7653),
     audit(87-04-02,Gilcrease), install(87-04-26,MR12.1-1026):
     Added support for C language header files (.H) files. (phx20795)
                                                   END HISTORY COMMENTS */

     
pnotice_language_info_:
	proc;

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/* This program creates the pnotice_language_info_ data structure for language names, and */
	/* for their associated comment delimiters.					*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

	      
%page;
dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl  1 cdsa	     aligned like cds_args;

dcl  code		     fixed bin (35);

dcl  name		     char (22) aligned static init
                         ("pnotice_language_info_") options (constant),
     NL		     char (1) aligned int static options(constant) init ("
"),
     exclude_pad         (1) char (32) aligned static options (constant) init
		     ("pad*");

dcl (dim,
     addr,
     size,
     string)	     builtin;

%include pnotice_language_info_;

dcl 1 lang_info aligned,
      2 languages,
        3 N fixed bin,
        3 lang_array (41) like pnotice_language_info.lang_array;


	lang_info.languages.N = dim(lang_info.languages.lang_array, 1);

	lang_info.lang_type(1) = 4;
	lang_info.lang_name(1) = "pl1";		/* PL/1 */
	lang_info.comment_start(1) = "/****^ ";
	lang_info.comment_end(1) = "*/";
	
	lang_info.lang_type(2) = 2;
	lang_info.lang_name(2) = "alm";		/* ALM */
	lang_info.comment_start(2) = """";
	lang_info.comment_end(2) = NL;

	lang_info.lang_type(3) = 2;
	lang_info.lang_name(3) = "fortran";		/* FORTRAN */
	lang_info.comment_start(3) = "c";
	lang_info.comment_end(3) = NL;
	
	lang_info.lang_type(4) = 2;
	lang_info.lang_name(4) = "cobol";		/* COBOL */
	lang_info.comment_start(4) = "*";
	lang_info.comment_end(4) = NL;
	
	lang_info.lang_type(5) = 2;
	lang_info.lang_name(5) = "lisp";		/* LISP */
	lang_info.comment_start(5) = ";;;";
	lang_info.comment_end(5) = NL;
	
	lang_info.lang_type(6) = 5;
	lang_info.lang_name(6) = "compin";		/* COMPIN */
	lang_info.comment_start(6) = ".*";
	lang_info.comment_end(6) = NL;

	lang_info.lang_type(7) = 1;
	lang_info.lang_name(7) = "cds";		/* CDS */
	lang_info.comment_start(7) = "/*";
	lang_info.comment_end(7) = "*/";

	lang_info.lang_type(8) = 1;
	lang_info.lang_name(8) = "et";		/* ET, ERROR_TABLE */
	lang_info.comment_start(8) = "/*";
	lang_info.comment_end(8) = "*/";

	lang_info.lang_type(9) = 1;
	lang_info.lang_name(9) = "bind";		/* BIND, BINDER */
	lang_info.comment_start(9) = "/*";
	lang_info.comment_end(9) = "*/";

	lang_info.lang_type(10) = 1;
	lang_info.lang_name(10) = "rd";		/* RD, REDUCTION COMPILER */
	lang_info.comment_start(10) = "/*";
	lang_info.comment_end(10) = "*/";

	lang_info.lang_type(11) = 1;
	lang_info.lang_name(11) = "compdv";		/* COMPDV, COMPOSE DEVICE TABLES */
	lang_info.comment_start(11) = "/*";
	lang_info.comment_end(11) = "*/";

	lang_info.lang_type(12) = 1;
	lang_info.lang_name(12) = "macro";		/* MACRO, COMPOSE MACROS */
	lang_info.comment_start(12) = "/*";
	lang_info.comment_end(12) = "*/";

	lang_info.lang_type(13) = 1;
	lang_info.lang_name(13) = "gdt";		/* GDT, GRAPHICS DEVICE TABLES */
	lang_info.comment_start(13) = "/*";
	lang_info.comment_end(13) = "*/";

	lang_info.lang_type(14) = 2;
	lang_info.lang_name(14) = "bcpl";		/* BCPL */
	lang_info.comment_start(14) = "//";
	lang_info.comment_end(14) = NL;

	lang_info.lang_type(15) = 2;
	lang_info.lang_name(15) = "map355";		/* MAP355 */
	lang_info.comment_start(15) = "*";
	lang_info.comment_end(15) = NL;

	lang_info.lang_type(16) = 3;
	lang_info.lang_name(16) = "ec";		/* EC, EXEC_COM */
	lang_info.comment_start(16) = "&";		/* this is modified as needed, by pnotice tools */
	lang_info.comment_end(16) = NL;
	
	lang_info.lang_type(17) = 2;
	lang_info.lang_name(17) = "basic";		/* BASIC */
	lang_info.comment_start(17) = "rem";
	lang_info.comment_end(17) = NL;

	lang_info.lang_type(18) = 3;
	lang_info.lang_name(18) = "absin";		/* ABSIN, ABSENTEE */
	lang_info.comment_start(18) = "&";		/* this is modified as needed by pnotice tools */
	lang_info.comment_end(18) = NL;

	lang_info.lang_type(19) = 4;
	lang_info.lang_name(19) = "pmac";		/* PL/1 MACRO EXPANDER */
	lang_info.comment_start(19) = "/****^ ";
	lang_info.comment_end(19) = "*/";

	lang_info.lang_type(20) = 1;
	lang_info.lang_name(20) = "ld";		/* LIBRARY DESCRIPTOR */
	lang_info.comment_start(20) = "/*";
	lang_info.comment_end(20) = "*/";

	lang_info.lang_type(21) = 1;
	lang_info.lang_name(21) = "pascal";		/* PASCAL */
	lang_info.comment_start(21) = "(*";
	lang_info.comment_end(21) = "*)";

	lang_info.lang_type(22) = 1;
	lang_info.lang_name(22) = "cmdb";		/* MRDS DB SOURCE */
	lang_info.comment_start(22) = "/*";
	lang_info.comment_end(22) = "*/";

	lang_info.lang_type(23) = 1;
	lang_info.lang_name(23) = "header";		/* HARDCORE HEADER */
	lang_info.comment_start(23) = "/*";
	lang_info.comment_end(23) = "*/";

	lang_info.lang_type(24) = 2;
	lang_info.lang_name(24) = "mexp";		/* OLD MACRO EXPANDER */
	lang_info.comment_start(24) = """";
	lang_info.comment_end(24) = "NL";

	lang_info.lang_type(25) = 5;
	lang_info.lang_name(25) = "runoff";		/* RUNOFF SOURCE */
	lang_info.comment_start(25) = ".*";
	lang_info.comment_end(25) = "NL";

	lang_info.lang_type(26) = 1;
	lang_info.lang_name(26) = "linus";		/* LINUS INVOKE MACROS */
	lang_info.comment_start(26) = "/*";
	lang_info.comment_end(26) = "*/";

	lang_info.lang_type(27) = 2;
	lang_info.lang_name(27) = "ted";		/* TED */
	lang_info.comment_start(27) = """";
	lang_info.comment_end(27) = NL;

	lang_info.lang_type(28) = 2;
	lang_info.lang_name(28) = "qedx";		/* QEDX */
	lang_info.comment_start(28) = """";
	lang_info.comment_end(28) = NL;

	lang_info.lang_type(29) = 2;
	lang_info.lang_name(29) = "table";		/* TABLE */
	lang_info.comment_start(29) = """";
	lang_info.comment_end(29) = NL;

	lang_info.lang_type(30) = 4;
	lang_info.lang_name(30) = "xdw";		/* EXPAND DEVICE WRITER*/
	lang_info.comment_start(30) = "/****^ ";
	lang_info.comment_end(30) = "*/";

	lang_info.lang_type(31) = 2;			/* MICRO ASSEMBLER */
	lang_info.lang_name(31) = "asm";
	lang_info.comment_start(31) = ";";
	lang_info.comment_end(31) = NL;
	
	lang_info.lang_type(32) = 1;			/* C */
	lang_info.lang_name(32) = "c";
	lang_info.comment_start(32) = "/*";
	lang_info.comment_end(32) = "*/";

	lang_info.lang_type(33) = 2;
	lang_info.lang_name(33) = "lap";		/* LAP */
	lang_info.comment_start(33) = ";;;";
	lang_info.comment_end(33) = NL;

	lang_info.lang_type(34) = 4;
	lang_info.lang_name(34) = "cmf";		/* CMF */
	lang_info.comment_start(34) = "/****^ ";
	lang_info.comment_end(34) = "*/";

	lang_info.lang_type(35) = 4;
	lang_info.lang_name(35) = "ttf";		/* TTF */
	lang_info.comment_start(35) = "/****^ ";
	lang_info.comment_end(35) = "*/";

	lang_info.lang_type(36) = 4;
	lang_info.lang_name(36) = "rtmf";		/* RTMF */
	lang_info.comment_start(36) = "/****^ ";
	lang_info.comment_end(36) = "*/";

	lang_info.lang_type(37) = 4;
	lang_info.lang_name(37) = "iodt";		/* IODT */
	lang_info.comment_start(37) = "/****^ ";
	lang_info.comment_end(37) = "*/";

	lang_info.lang_type(38) = 2;
	lang_info.lang_name(38) = "ssl";		/* SSL */
	lang_info.comment_start(38) = "*";
	lang_info.comment_end(38) = "NL";

	lang_info.lang_type(39) = 1;
	lang_info.lang_name(39) = "teco";		/* TECO */
	lang_info.comment_start(39) = "!";
	lang_info.comment_end(39) = "!";

	lang_info.lang_type(40) = 1;
	lang_info.lang_name(40) = "bind_fnp";		/* BIND_FNP */
	lang_info.comment_start(40) = "/*";
	lang_info.comment_end(40) = "*/";

	lang_info.lang_type(41) = 1;			/* C Header */
	lang_info.lang_name(41) = "h";
	lang_info.comment_start(41) = "/*";
	lang_info.comment_end(41) = "*/";

/* Now set up call to create data base */

	cdsa.sections (1).p = addr (lang_info);
	cdsa.sections (1).len = size (lang_info);
	cdsa.sections (1).struct_name = "lang_info";
	cdsa.seg_name = name;
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (exclude_pad);
	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;
	call create_data_segment_ (addr (cdsa), code);

	% include cds_args;
	end pnotice_language_info_;
