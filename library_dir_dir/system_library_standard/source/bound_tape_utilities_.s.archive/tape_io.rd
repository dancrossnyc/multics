/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */
/* tape_inout: Created 3/15/76 by J.B. Phillipps */
/* Modified extensively 6/79 by Michael R. Jordan to fix many bugs */
/* Modified extensively 4/82 by J. A. Bush for installation in MR10.0 */
/* format: style4 */

/*++
BEGIN	/<no-token>
		/Error (1)			/RETURN\

newVol	/Volume :
		/build_vcb
		 LEX (2)				/Volume\
	/<any-token>
		/Error (2)			/RETURN\
	/<no-token>
		/				/RETURN\

Volume	/<valid_volidp>
		/[ii = 1]
		 [vcb.volid(ii) = token_value]
		 LEX (1)				/morevols\
	/<any-token>
		/Error (6)
		 NEXT_STMT			/global\
	/<no-token>
		/				/notoken\
nextvol	/<valid_volidp>
		/[ii = ii + 1]
		 [if ii <= hbound (vcb.volid, 1)
		      then vcb.volid(ii) = token_value]
		 [else if ii = hbound (vcb.volid, 1)+1
		      then call Error (25)]
		 LEX (1)				/morevols\
	/<any-token>
		/Error (6)
		 NEXT_STMT			/global\
	/<no-token>
		/				/notoken\

morevols  /"-comment"
		/LEX (1)				/comment\
	/"-com"
		/LEX (1)				/comment\
	/<any-token>
		/				/ck_pun\
	/<no-token>
		/				/notoken\

comment	/<quoted-string>
		/[vcb.comment (ii) = token_value]
		 LEX (1)				/ck_pun\
	/<name>
		/[vcb.comment (ii) = token_value]
		 LEX (1)				/ck_pun\
	/<any-token>
		/[vcb.comment (ii) = token_value]
		 LEX (1)				/ck_pun\
	/<no-token>
		/				/notoken\

ck_pun	/;
		/[vcb.nvols = min (ii, hbound (vcb.volid, 1))]
		 LEX (1)				/global\
	/,
		/LEX (1)				/nextvol\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/global\
	/<no-token>
		/				/notoken\

global	/File :
		/LEX (2)
		 build_fcb (vcb.first_fcb_ptr, current_fcb_ptr)
		 [fcb.file_token_ptr = Ptoken]
		 [if current_default_fcb_ptr ^= null then
		     fcb.default_fcb_ptr = current_default_fcb_ptr]
						/File\
	/End ;
		/end_vcb
		 LEX (2)				/newVol\
	/Volume :
		/Error (4)
		 end_vcb				/newVol\
	/Tape :
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 LEX (2)				/Tape\
	/Density :
		/[if vcb.density ^= 0
		      then call Error (14)]
		 LEX (2)				/Density\

	/<any-token>
		/PUSH (global)
		 [if ^build_default_fcb then do;
		    call build_fcb (vcb.first_default_fcb_ptr, current_default_fcb_ptr);
		    build_default_fcb = "1"b;
		  end]
						/gloop\
	/<no-token>
		/				/notoken\

gloop	/Storage :
		/LEX (2)				/Storage\
          /Expiration :
		/LEX (2)				/Expires\
	/Mode :
		/LEX (2)				/Mode\
	/Format :
		/LEX (2)				/Format\
	/Block :
		/LEX (2)				/Block\
	/Record :
		/LEX (2)				/Record\

	/mode :
		/Error (50)
		 NEXT_STMT			/STACK_POP\
	/storage :
		/Error (50)
		 NEXT_STMT			/STACK_POP\
	/expiration :
		/Error (50)
		 NEXT_STMT			/STACK_POP\
	/number :
		/Error (50)
		 NEXT_STMT			/STACK_POP\
	/replace :
		/Error (50)
		 NEXT_STMT			/STACK_POP\
      	/modify ;
		/Error (50)
		 NEXT_STMT			/STACK_POP\
      	/generate ;
		/Error (50)
		 NEXT_STMT			/STACK_POP\
      	/tape_extend ;
		/Error (50)
		 NEXT_STMT			/STACK_POP\
      	/storage_extend ;
		/Error (50)
		 NEXT_STMT			/STACK_POP\
	/format :
		/Error (50)
		 NEXT_STMT			/STACK_POP\
	/block :
		/Error (50)
		 NEXT_STMT			/STACK_POP\
	/record :
		/Error (50)
		 NEXT_STMT			/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\

Tape	/ANSI ;
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 [else vcb.tape_type = 1]
		 LEX (2)				/global\
          /ansi ;
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 [else vcb.tape_type = 1]
		 LEX (2)				/global\
	/IBMSL ;
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 [else vcb.tape_type = 2]
		 LEX (2)				/global\
          /ibmsl ;
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 [else vcb.tape_type = 2]
		 LEX (2)				/global\
	/IBMNL ;
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 [else vcb.tape_type = 3]
		 LEX (2)				/global\
          /ibmnl ;
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 [else vcb.tape_type = 3]
		 LEX (2)				/global\
	/IBMDOS ;
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 [else vcb.tape_type = 4]
		 LEX (2)				/global\
          /ibmdos ;
		/[if vcb.tape_type ^= 0
		      then call Error (20)]
		 [else vcb.tape_type = 4]
		 LEX (2)				/global\
	/<any-token> ;
		/Error (21)
		 LEX (2)				/global\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/global\
	/<no-token>
		/				/notoken\

Density	/6250 ;
		/[if vcb.density ^= 0
		      then call Error (14)]
		 [else vcb.density = 4]
		 LEX (2)				/global\
	/4 ;
		/[if vcb.density ^= 0
		      then call Error (14)]
		 [else vcb.density = 4]
		 LEX (2)				/global\
	/1600 ;
		/[if vcb.density ^= 0
		      then call Error (14)]
		 [else vcb.density = 3]
		 LEX (2)				/global\
	/3 ;
		/[if vcb.density ^= 0
		      then call Error (14)]
		 [else vcb.density = 3]
		 LEX (2)				/global\
	/800 ;
		/[if vcb.density ^= 0
		      then call Error (14)]
		 [else vcb.density = 2]
		 LEX (2)				/global\
	/2 ;
		/[if vcb.density ^= 0
		      then call Error (14)]
		 [else vcb.density = 2]
		 LEX (2)				/global\
	/<decimal-integer> ;
		/Error (7)
		 LEX (2)				/global\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/global\
	/<no-token>
		/				/notoken\

Storage	/unstructured ;
		/[if fcb.segment.format ^= 0
		      then call Error (33)]
		 [else fcb.segment.format = 1]
		 LEX (2)				/STACK_POP\

	/sequential ;
		/[if fcb.segment.format ^= 0
		      then call Error (33)]
		 [else fcb.segment.format = 2]
		 LEX (2)				/STACK_POP\
	/<any-token> ;
		/Error (12)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

Expires   /<valid_datep> ;
		/[if fcb.tape.expiration ^= "" ""
		      then call Error (34)]
		 [else fcb.tape.expiration = token_value]
		 LEX (2)				/STACK_POP\
          /<any-token> ;
		/Error (22)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

Mode	/ascii ;
		/[if fcb.tape.cmode ^= 0
		      then call Error (44)]
		 [else fcb.tape.cmode = 1]
		 LEX (2)				/STACK_POP\
	/ASCII ;
		/[if fcb.tape.cmode ^= 0
		      then call Error (44)]
		 [else fcb.tape.cmode = 1]
		 LEX (2)				/STACK_POP\
	/ebcdic ;
		/[if fcb.tape.cmode ^= 0
		      then call Error (44)]
		 [else fcb.tape.cmode = 2]
		 LEX (2)				/STACK_POP\
	/EBCDIC ;
		/[if fcb.tape.cmode ^= 0
		      then call Error (44)]
		 [else fcb.tape.cmode = 2]
		 LEX (2)				/STACK_POP\
	/binary ;
		/[if fcb.tape.cmode ^= 0
		      then call Error (44)]
		 [else fcb.tape.cmode = 3]
		 LEX (2)				/STACK_POP\
	/BINARY ;
		/[if fcb.tape.cmode ^= 0
		      then call Error (44)]
		 [else fcb.tape.cmode = 3]
		 LEX (2)				/STACK_POP\
	/<any-token> ;
		/Error (8)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

Format	/U ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 1]
		 LEX (2)				/STACK_POP\
	/u ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 1]
		 LEX (2)				/STACK_POP\
	/F ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 2]
		 LEX (2)				/STACK_POP\
	/f ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 2]
		 LEX (2)				/STACK_POP\
	/FB ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 5]
		 LEX (2)				/STACK_POP\
	/fb ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 5]
		 LEX (2)				/STACK_POP\
	/D ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 3]
		 LEX (2)				/STACK_POP\
	/d ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 3]
		 LEX (2)				/STACK_POP\
	/DB ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 6]
		 LEX (2)				/STACK_POP\
	/db ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 6]
		 LEX (2)				/STACK_POP\
	/S ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 8]
		 LEX (2)				/STACK_POP\
	/s ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 8]
		 LEX (2)				/STACK_POP\
          /SB ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 9]
		 LEX (2)				/STACK_POP\
	/sb ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 9]
		 LEX (2)				/STACK_POP\
          /V ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 4]
		 LEX (2)				/STACK_POP\
	/v ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 4]
		 LEX (2)				/STACK_POP\
	/VB ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 7]
		 LEX (2)				/STACK_POP\
	/vb ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 7]
		 LEX (2)				/STACK_POP\
	/VS ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 10]
		 LEX (2)				/STACK_POP\
	/vs
		;			/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 10]
		 LEX (2)				/STACK_POP\
	/VBS ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 11]
		 LEX (2)				/STACK_POP\
	/vbs ;
		/[if fcb.tape.format ^= 0
		      then call Error (45)]
		 [else fcb.tape.format = 11]
		 LEX (2)				/STACK_POP\
	/<any-token> ;
		/Error (9)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

Block	/<valid_block_sizep> ;
		/[if fcb.tape.blklen ^= 0
		      then call Error (46)]
		 [else fcb.tape.blklen = token.Nvalue]
		 LEX (2)				/STACK_POP\
	/<any-token> ;
		/Error (10)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

Record	/<valid_record_sizep> ;
		/[if fcb.tape.reclen ^= 0
		      then call Error (47)]
		 [else fcb.tape.reclen = token.Nvalue]
		 LEX (2)				/STACK_POP\
	/<any-token> ;
		/Error (11)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

File      /<valid_file_namep> ;
		/[fcb.tape.file_id = token_value]
		 [build_default_fcb = "0"b]
		 LEX (2)				/local\
	/<any-token> ;
		/Error (37)
		 LEX (2)				/local\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/local\
	/<no-token>
		/				/notoken\

local	/<any-token>
		/PUSH (local)			/lloop\
	/<no-token>
		/				/notoken\

lloop	/path :
		/LEX (2)				/path\
	/mode :
		/LEX (2)				/Mode\
	/storage :
		/LEX (2)				/Storage\
	/expiration :
		/LEX (2)				/Expires\
	/number :
		/LEX (2)				/number\
	/replace :
		/LEX (2)				/replace\
	/format :
		/LEX (2)				/Format\
	/block :
		/LEX (2)				/Block\
	/record :
		/LEX (2)				/Record\
      	/modify ;
		/[if fcb.tape.output_mode ^= 0
		      then call Error (38)]
		 [else fcb.tape.output_mode = 2]
		 LEX (2)				/STACK_POP\
      	/generate ;
		/[if fcb.tape.output_mode ^= 0
		      then call Error (38)]
		 [else fcb.tape.output_mode = 3]
		 LEX (2)				/STACK_POP\
      	/tape_extend ;
		/[if fcb.tape.output_mode ^= 0
		      then call Error (38)]
		 [else fcb.tape.output_mode = 1]
		 LEX (2)				/STACK_POP\
      	/storage_extend ;
		/[if fcb.segment.extend ^= 0
		      then call Error (27)]
		 [fcb.segment.extend = 2]
		 LEX (2)				/STACK_POP\
	/File :
		/POP
						/global\
	/End
		/POP				/global\
	/Storage :
		/POP
		 				/global\
	/Expiration :
		/POP
		 				/global\
	/Mode :
		/POP
		 				/global\
          /Format :
		/POP
		 				/global\
	/Block :
		/POP
		 				/global\
          /Record :
		/POP
		 				/global\

	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

path	/<valid_pathnamep> ;
		/[if fcb.segment.ename ^= "" ""
		      then call Error (23)]
		 [fcb.segment.dirname = dirname]
		 [fcb.segment.ename = ename]
		 LEX (2)				/STACK_POP\
	/<any-token> ;
		/Error (13)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

number	/<valid_file_numberp> ;
		/[if fcb.tape.sequence ^= 0
		      then call Error (51)]
		 [else fcb.tape.sequence = token.Nvalue]
		 LEX (2)				/STACK_POP\
	/<any-token> ;
		/Error (48)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\
	/<no-token>
		/				/notoken\

replace	/<valid_file_namep> ;
		/[if fcb.tape.output_mode ^= 0
		      then call Error (38)]
		 [else fcb.tape.output_mode = 4]
		 [fcb.tape.replace_id = token_value]
		 LEX (2)				/STACK_POP\
	/<any-token> ;
		/Error (37)
		 LEX (2)				/STACK_POP\
	/<any-token>
		/Error (5)
		 NEXT_STMT			/STACK_POP\

notoken	/<no-token>
		/Error (3)			/RETURN\
++*/

tape_io: procedure;

/* CONSTANTS */

dcl  ANSI fixed bin internal static options (constant) init (1);
dcl  IBMNL fixed bin internal static options (constant) init (3);
dcl  DEFAULT_DENSITY (4) fixed bin internal static options (constant) init (2, 3, 3, 3);
dcl  SERROR_CONTROL bit (2) internal static options (constant) init ("10"b);
dcl  USAGE_MESSAGE char (115) internal static options (constant) init
	("^a^/Usage:  ^a tcl_path {-control_args}^/where control args are: -check, -ck, -force, -fc, -severityN, -svN, -ring");
dcl  sys_info$max_seg_size fixed bin (35) external static;	/* maximum segment size in words */

dcl  1 EMPTY_FCB aligned static internal options (constant),
       2 file_token_ptr ptr init (null),		/* none */
       2 next_fcb_ptr ptr init (null),			/* none */
       2 prev_fcb_ptr ptr init (null),			/* none */
       2 default_fcb_ptr ptr init (null),		/* none */
       2 segment,
         3 dirname char (168) init ((168)" "),
         3 ename char (32) init ((32)" "),
         3 format fixed bin init (0),			/* not specified */
         3 extend fixed bin init (0),			/* not specified */
         3 truncate_lines fixed bin init (0),		/* not specified */
       2 tape,
         3 cmode fixed bin init (0),			/* not specified */
         3 format fixed bin init (0),			/* not specified */
         3 output_mode fixed bin init (0),		/* not specified */
         3 file_id char (17) init (""),			/* not specified */
         3 replace_id char (17) init (""),		/* not specified */
         3 expiration char (16) init (""),		/* not specified */
         3 sequence fixed bin init (0),			/* not specified */
         3 blklen fixed bin init (0),			/* not specified */
         3 reclen fixed bin (21) init (0);		/* not specified */

dcl  1 EMPTY_VCB aligned static internal options (constant),
       2 volume_token_ptr ptr init (null),		/* none */
       2 next_vcb_ptr ptr init (null),			/* none */
       2 first_fcb_ptr ptr init (null),			/* none */
       2 first_default_fcb_ptr ptr init (null),		/* none */
       2 nvols fixed bin init (0),			/* no volumes */
       2 volid (64) char (32) init ((64) (32)" "),
       2 comment (64) char (64) init ((64) (64)" "),
       2 tape_type fixed bin init (0),			/* not specified */
       2 density fixed bin init (0);			/* not specified */


/* STATIC STORAGE */

dcl  breaks char (128) varying aligned internal static;	/* break characters for lex_string_ */
dcl  ignored_breaks char (128) varying aligned internal static; /* ignored breaks for lex_string_ */
dcl  init_req bit (1) internal static initial ("1"b);	/* initialization switch: 0-not required; 1-required */
dcl  lex_control_chars char (128) varying aligned internal static; /* control characters for lex_string_ */
dcl  lex_delims char (128) varying aligned internal static; /* delimiters for lex_string_ */


/* AUTOMATIC STORAGE */

dcl  1 tid like tape_io_data aligned;
dcl  1 ai like area_info aligned;

dcl  aL fixed bin;
dcl  aP ptr;
dcl  arg_num fixed bin;
dcl  clk_val fixed bin (71);
dcl  bc fixed bin (24);
dcl  code fixed bin (35);
dcl  current_default_fcb_ptr ptr init (null);
dcl  current_fcb_ptr ptr init (null);
dcl  dfcbp ptr;
dcl  dirname char (168);
dcl  ename char (32);
dcl  error_count fixed bin;
dcl  ii fixed bin;
dcl  j fixed bin;
dcl  max_severity_num fixed bin;			/* max severity printed by lex_error_ */
dcl  name char (8);					/* command name */
dcl  nargs fixed bin;
dcl  serror_printed (dimension (error_control_table, 1)) bit (1) unaligned; /* is "1"b if error msg printed prev. */
dcl  temp_ptr ptr;
dcl  writing bit (1);				/* ON => tape_out */
dcl  build_default_fcb bit (1) aligned init ("0"b);

/* BASED STORAGE */

dcl  arg char (aL) based (aP);
dcl  my_area area based (tape_io_data.temp (1));


/* ERROR CODES */

dcl  error_table_$active_function fixed bin (35) ext static;
dcl  error_table_$badopt fixed bin (35) ext static;
dcl  error_table_$noarg fixed bin (35) ext static;
dcl  error_table_$not_act_fnc fixed bin (35) ext static;
dcl  error_table_$translation_failed fixed bin (35) ext static;


/* BUILTIN FUNCTIONS */

dcl  (addr, collate, dimension, divide, hbound, min, mod, null, substr, unspec) builtin;

/* CONDITIONS */

dcl  cleanup condition;


/* EXTERNAL PROCEDURES */

dcl  active_fnc_err_ entry options (variable);
dcl  com_err_ ext entry options (variable);
dcl  convert_date_to_binary_ ext entry (char (*), fixed bin (71), fixed bin (35));
dcl  cu_$af_arg_count ext entry (fixed bin, fixed bin (35));
dcl  cu_$arg_ptr ext entry (fixed bin, ptr, fixed bin, fixed bin (35));
dcl  cv_dec_check_ entry (char (*), fixed bin (35)) returns (fixed bin (35));
dcl  expand_pathname_ entry (char (*), char (*), char (*), fixed bin (35));
dcl  expand_pathname_$add_suffix entry (char (*), char (*), char (*), char (*), fixed bin (35));
dcl  translator_temp_$get_next_segment entry (ptr, ptr, fixed bin (35));
dcl  translator_temp_$get_segment entry (char (*) aligned, ptr, fixed bin (35));
dcl  translator_temp_$release_segment entry (ptr, fixed bin (35));
dcl  define_area_ entry (ptr, fixed bin (35));
dcl  release_area_ entry (ptr);
dcl  hcs_$initiate_count entry (char (*), char (*), char (*), fixed bin (24), fixed bin (2), ptr, fixed bin (35));
dcl  hcs_$terminate_noname ext entry (ptr, fixed bin (35));
dcl  ioa_ ext entry options (variable);
dcl  lex_error_ ext entry options (variable);
dcl  lex_string_$init_lex_delims ext entry (char (*), char (*), char (*), char (*),
	char (*), bit (*), char (*) varying aligned, char (*) varying aligned, char (*) varying aligned,
	char (*) varying aligned);
dcl  lex_string_$lex ext entry (ptr, fixed bin (21), fixed bin (21), ptr, bit (*), char (*), char (*), char (*),
	char (*), char (*), char (*) varying aligned, char (*) varying aligned, char (*) varying aligned,
	char (*) varying aligned, ptr, ptr, fixed bin (35));
dcl  tape_io_interpret_ entry (ptr);

tape_in: tin: entry;

	name = "tape_in";				/* set command name */
	writing = "0"b;				/* not writing tape */
	go to common_code;				/* begin processing */

tape_out: tout: entry;

	name = "tape_out";				/* set command name */
	writing = "1"b;				/* writing tape */

common_code:

	tape_io_data_ptr = addr (tid);
	tape_io_data.temp (*) = null;
	tape_io_data.first_vcb_ptr = null;
	tape_io_data.source.dirname = "";
	tape_io_data.source.ename = "";
	tape_io_data.source.ptr = null;
	tape_io_data.control.ck = "0"b;
	tape_io_data.control.force = "0"b;
	tape_io_data.control.ring = "0"b;
	tape_io_data.control.writing_tape = writing;
	tape_io_data.control.max_severity = 0;

	call cu_$af_arg_count (nargs, code);
	if code = error_table_$not_act_fnc then
	     code = 0;
	else if code = 0 then do;
	     call active_fnc_err_ (error_table_$active_function, name);
	     return;
	end;
	else do;
	     call com_err_ (code, name);
	     return;
	end;
	if nargs < 1 then do;
	     call com_err_ (error_table_$noarg, name, USAGE_MESSAGE,
		"Control file pathname is missing.", name);
	     return;
	end;
	call cu_$arg_ptr (1, aP, aL, (0));
	call expand_pathname_$add_suffix (arg, "tcl",
	     tape_io_data.source.dirname, tape_io_data.source.ename, code);
	if code ^= 0 then do;
	     call com_err_ (code, name, "^a", arg);
	     return;
	end;

	on cleanup call Cleaner;

	call hcs_$initiate_count (tape_io_data.source.dirname,
	     tape_io_data.source.ename, "", bc, 0, tape_io_data.source.ptr,
	     code);
	if tape_io_data.source.ptr = null then do;
	     call com_err_ (code, name, "^a^[>^]^a", tape_io_data.source.dirname,
		(tape_io_data.source.dirname ^= ">"), tape_io_data.source.ename);
	     return;
	end;
	unspec (ai) = "0"b;				/* clear out area info */
	ai.version = area_info_version_1;		/* set up area info block */
	ai.control.extend = "1"b;
	ai.control.zero_on_alloc = "1"b;
	ai.owner = name;
	ai.size = sys_info$max_seg_size;
	ai.version_of_area = area_info_version_1;
	ai.areap = null;
	call define_area_ (addr (ai), code);		/* get an area */
	if code ^= 0 then do;
	     call com_err_ (code, name, "Cannot define an area");
	     go to EXIT;
	end;
	tape_io_data.temp (1) = ai.areap;		/* copy area ptr */
	call translator_temp_$get_segment ((name), tape_io_data.temp (2), code);
	if code = 0 then
	     call translator_temp_$get_next_segment (tape_io_data.temp (2), tape_io_data.temp (3), code);
	if tape_io_data.temp (2) = null | tape_io_data.temp (3) = null then do;
	     call com_err_ (code, name, "Cannot allocate necessary temporary segments.");
	     go to EXIT;
	end;

	do arg_num = 2 repeat arg_num + 1 while (arg_num <= nargs);
	     call cu_$arg_ptr (arg_num, aP, aL, (0));
	     if arg = "-check" | arg = "-ck" then
		tape_io_data.control.ck = "1"b;
	     else if arg = "-severity" | arg = "-sv" then do;
		arg_num = arg_num + 1;
		if arg_num > nargs then do;
		     call com_err_ (error_table_$noarg, name,
			"Severity level missing following ^a.", arg);
		     goto EXIT;
		end;
		call cu_$arg_ptr (arg_num, aP, aL, (0));
		tape_io_data.control.max_severity = cv_dec_check_ (arg, code);
		if code ^= 0 then
		     go to bad_arg;
	     end;
	     else if arg = "-force" | arg = "-fc" then
		tape_io_data.control.force = "1"b;	/* force all file expiration dates */
	     else if arg = "-ring" then
		tape_io_data.control.ring = "1"b;
	     else do;
bad_arg:		call com_err_ (error_table_$badopt, name, "^a", arg);
		goto EXIT;
	     end;
	end;
	error_count = 0;				/* initialize syntatical error counter */
	if init_req then do;			/* initialize static values if necessary */
	     breaks = substr (collate, 1, 33);		/* control characters */
	     breaks = breaks || ":;, ";		/* my definitions */
	     breaks = breaks || substr (collate, 128, 1); /* ....and the null (pad) character */
	     ignored_breaks = substr (collate, 1, 8);	/* control characters.... */
	     ignored_breaks = ignored_breaks || substr (collate, 10, 24); /* ....excluding backspace */
	     ignored_breaks = ignored_breaks || substr (collate, 128, 1); /* and null character */
	     init_req = "0"b;			/* initialization no longer necessary */
	end;

	call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "1"b, /* initialize the lexing routine */
	     breaks, ignored_breaks, lex_delims, lex_control_chars);

/* call to parse character string input into tokens  */
/* chained list of token descriptors generated in temp seg pointed to by temp (2)  */

	call lex_string_$lex (tape_io_data.source.ptr, divide (bc, 9, 21, 0),
	     0, tape_io_data.temp (2), "1"b, """", """", "/*", "*/", ";",
	     breaks, ignored_breaks, lex_delims, lex_control_chars, Pstmt,
	     Pthis_token, code);
	if code ^= 0 then
	     call com_err_ (code, name);
	if Pthis_token = null then do;
	     call com_err_ (error_table_$translation_failed, name, "The source file is uninterpretable.");
	     goto EXIT;
	end;

/* invoke subroutine which translates tokens */

	max_severity_num = 0;			/* initialize */

	call SEMANTIC_ANALYSIS ();
	if max_severity_num > 1 then do;
	     call com_err_ (error_table_$translation_failed, name);
	     goto EXIT;
	end;

/* if user only wants syntax checking then that's all here  */

	if ^tape_io_data.control.ck then
	     call tape_io_interpret_ (tape_io_data_ptr);	/* go do the requested I/O */
	else call ioa_ ("^/^a: Translation finished; Number of errors encountered was ^d", name, error_count);
EXIT:
	call Cleaner;
	return;

Cleaner: procedure;

	if tape_io_data.source.ptr ^= null then
	     call hcs_$terminate_noname (tape_io_data.source.ptr, 0);
	if tape_io_data.temp (1) ^= null then
	     call release_area_ (tape_io_data.temp (1));
	if tape_io_data.temp (2) ^= null then
	     call translator_temp_$release_segment (tape_io_data.temp (2), code);
	if tape_io_data.temp (3) ^= null then
	     call translator_temp_$release_segment (tape_io_data.temp (3), code);

     end Cleaner;


Error: proc (en);					/* subroutine to check severify before printing errors */

dcl  en fixed bin;
dcl  pstmt ptr;

	if Ptoken = null () then pstmt = null ();
	else pstmt = token.Pstmt;
	if error_control_table (en).severity >= tape_io_data.control.max_severity then
	     call lex_error_ (en, serror_printed (en),
		error_control_table (en).severity, max_severity_num, pstmt, null (),
		SERROR_CONTROL, error_control_table (en).message, error_control_table (en).brief_message);

	error_count = error_count + 1;


     end Error;

valid_block_sizep: procedure returns (bit (1) aligned);	/* defines <valid_block_sizep> token */

	token.Nvalue = cv_dec_check_ (token_value, code);
	if code ^= 0 then return ("0"b);
	if token.Nvalue < 18 then return ("0"b);	/* not valid if too small */
	if token.Nvalue > 99996 then return ("0"b);	/* not valid if too large */
	return ("1"b);				/* valid */

     end valid_block_sizep;

valid_file_namep: procedure returns (bit (1) aligned);	/* defines valid_file_namep */

	if token.Lvalue < 1 then return ("0"b);
	if token.Lvalue > 17 then return ("0"b);
	return ("1"b);
     end valid_file_namep;

valid_file_numberp: procedure returns (bit (1) aligned);	/* defines <valid_file_numberp> token */

	if token_value = "*" then do;			/* file number not specified */
	     token.Nvalue = -1;
	     return ("1"b);
	end;
	token.Nvalue = cv_dec_check_ (token_value, code);
	if code ^= 0 then return ("0"b);
	if token.Nvalue > 9999 then return ("0"b);
	if token.Nvalue <= 0 then return ("0"b);
	return ("1"b);				/* valid */
     end valid_file_numberp;

valid_pathnamep: procedure returns (bit (1) aligned);	/* defines <valid_pathnamep> token */

	call expand_pathname_ (token_value, dirname, ename, code);
	return (code = 0);
     end valid_pathnamep;

valid_record_sizep: procedure returns (bit (1) aligned);	/* defines <valid_record_sizep> token */

	token.Nvalue = cv_dec_check_ (token_value, code);
	if code ^= 0 then return ("0"b);
	if token.Nvalue < 1 then return ("0"b);		/* not valid if 0 (or negative) */
	if token.Nvalue > sys_info$max_seg_size * 4 then return ("0"b); /* not valid if > segment size in chars */
	return ("1"b);				/* meets requirements */
     end valid_record_sizep;

valid_volidp: procedure returns (bit (1) aligned);	/* defines <valid_volidp> token */

	if token.Lvalue > 32 then return ("0"b);	/* not <valid_volidp> if greater than 32 characters */
	if token.Lvalue < 1 then return ("0"b);		/* not <valid_volidp> if less than 1 character */
	return ("1"b);				/* no other requirements */
     end valid_volidp;

valid_datep: procedure returns (bit (1) aligned);		/* defines <valid_datep> token */

	call convert_date_to_binary_ (token_value, clk_val, code); /* convert date */
	return (code = 0);
     end valid_datep;

build_vcb: procedure;				/* procedure to build a vcb */

	allocate vcb in (my_area) set (temp_ptr);
	if tid.first_vcb_ptr = null then
	     tid.first_vcb_ptr = temp_ptr;
	else vcb.next_vcb_ptr = temp_ptr;
	vcb_ptr = temp_ptr;
	vcb = EMPTY_VCB;				/* Initialize the vcb */

     end build_vcb;


end_vcb: procedure;					/* procedure to add defaults to VCBs and FCBs */

	if vcb.tape_type = 0 then
	     vcb.tape_type = ANSI;
	if vcb.density = 0 then			/* Must set the default density */
	     vcb.density = DEFAULT_DENSITY (vcb.tape_type);
	if vcb.first_fcb_ptr = null then do;		/* no file-groups in this volume-group */
	     Ptoken = vcb.volume_token_ptr;		/* no source line to be printed */
	     call Error (52);
	     return;
	end;

	do fcb_ptr = vcb.first_fcb_ptr repeat fcb.next_fcb_ptr while (fcb_ptr ^= null ());
	     call Complete_FCB ();			/* add defaults to FCB */
	     call Check_FCB ();			/* and validate FCB */
	end;

     end end_vcb;

build_fcb: procedure (head, tail);			/* procedure to allocate and initialize an FCB */

dcl  (head, tail) ptr;				/* ptr to head & tail of FCB chain */

	allocate fcb in (my_area) set (temp_ptr);
	if head = null then
	     head = temp_ptr;
	else tail -> fcb.next_fcb_ptr = temp_ptr;
	fcb_ptr = temp_ptr;
	fcb = EMPTY_FCB;				/* reset for next <file-group> */
	if tail ^= null then
	     fcb.prev_fcb_ptr = tail;			/* set backward fcb thread */
	tail = temp_ptr;				/* and update fcb tail for next allocation */

     end build_fcb;

Complete_FCB: procedure;				/* procedure to add defaults to FCB */

/* first, add in global default values, if any */

	do dfcbp = fcb.default_fcb_ptr repeat dfcbp -> fcb.prev_fcb_ptr while (dfcbp ^= null);
	     if fcb.tape.blklen = 0 then fcb.tape.blklen = dfcbp -> fcb.tape.blklen;
	     if fcb.tape.reclen = 0 then fcb.tape.reclen = dfcbp -> fcb.tape.reclen;
	     if fcb.tape.format = 0 then fcb.tape.format = dfcbp -> fcb.tape.format;
	     if fcb.tape.cmode = 0 then fcb.tape.cmode = dfcbp -> fcb.tape.cmode;
	     if fcb.tape.expiration = "" then fcb.tape.expiration = dfcbp -> fcb.tape.expiration;
	     if fcb.segment.format = 0 then fcb.segment.format = dfcbp -> fcb.segment.format;
	end;

/* Set the defaults up according to what kind of tape is to be processed */

	if vcb.tape_type = ANSI then do;		/* If ANSI tape, set the ANSI defaults */
	     if fcb.tape.cmode = 0 then		/* if recording mode not specified.. */
		fcb.tape.cmode = 1;			/* set ANSI default to ASCII */
	     if tape_io_data.control.writing_tape then do;/* if tape output */
		if fcb.tape.format = 0 then		/* if tape format not specified.. */
		     fcb.tape.format = 6;		/* set ANSI default to DB */
		if fcb.tape.blklen = 0 then		/* if block length not specified.. */
		     fcb.tape.blklen = 2048;		/* set ANSI default */
		if fcb.tape.reclen = 0 then		/* if Record length not specified.. */
		     fcb.tape.reclen = 2048;		/* set ANSI default  */
	     end;
	end;
	else do;					/* No, its an IBMSL, IBMNL, or IBMDOS tape */
	     if fcb.tape.cmode = 0 then		/* if recording mode not specified.. */
		fcb.tape.cmode = 2;			/* set IBM default to EBCDIC */
	     if tape_io_data.control.writing_tape then do;/* if tape output */
		if fcb.tape.format = 0 then		/* if tape format not specified.. */
		     fcb.tape.format = 7;		/* set IBM default to VB */
		if fcb.tape.blklen = 0 then		/* if block length not specified.. */
		     fcb.tape.blklen = 8192;		/* set IBM default */
		if fcb.tape.reclen = 0 then		/* if Record length not specified.. */
		     fcb.tape.reclen = 8188;		/* set IBM default  */
	     end;
	end;

/* Now do the common defaults */

	if tape_io_data.control.writing_tape then	/* if tape output */
	     if fcb.tape.output_mode = 0 then		/* if no output mode specified... */
		fcb.tape.output_mode = 4;		/* The default is "Create or Replace" */
	if fcb.segment.format = 0 then		/* if no segment format specified... */
	     fcb.segment.format = 1;			/* The default is "Unstructured" */
	if fcb.segment.extend = 0 then		/* if no extend action specified... */
	     fcb.segment.extend = 1;			/* The default is "Truncate" */
	if fcb.segment.truncate_lines = 0 then		/* if no long lines action specified... */
	     fcb.segment.truncate_lines = 1;		/* The default is to "Fold" long lines */

     end Complete_FCB;

Check_FCB: procedure;				/* procedure to validate the FCB for completness */

	Ptoken = null;				/* no source line to be printed */
	if fcb.segment.dirname = "" | fcb.segment.ename = "" then /* no "path" statement */
	     call Error (24);
	if fcb.tape.file_id = "" then			/* No file statement */
	     call Error (18);
	if fcb.tape.sequence = -1 then		/* if number statement specified as "*".. */
	     if fcb.tape.output_mode ^= 4 then		/* mode has to be append */
		call Error (49);
	if vcb.tape_type ^= ANSI then do;		/* tape volume.tape_type not ANSI */
	     if fcb.tape.output_mode = 3 then		/* generate option not supported by IBM */
		call Error (19);
	     if fcb.tape.output_mode > 0 then		/* some output option  specified */
		if fcb.tape.blklen ^= 0 then		/* block size specified */
		     if mod (fcb.tape.blklen, 4) ^= 0 then /* blklen not word multiple */
			call Error (28);
	     if fcb.tape.blklen > 32760 then		/* block size too large for tape_ibm_ */
		call Error (10);
	end;
	if vcb.tape_type = IBMNL then do;		/* unlabeled volume specified */
	     if fcb.tape.file_id ^= "*" then		/* file names not allowed with unlabled volumes */
		call Error (29);
	     if fcb.tape.sequence = 0 then		/* if no file sequence number specified */
		call Error (31);
	     if fcb.tape.replace_id ^= "" then		/* replace statement specified for unlabeled tape */
		call Error (30);
	     if fcb.tape.output_mode = 1 then		/* extend specified for unlabeled tape */
		call Error (32);
	     else if fcb.tape.output_mode = 2 then	/* modify specified for unlabeled tape */
		call Error (55);
	     if fcb.tape.expiration ^= "" then		/* expires specified for unlabeled tape */
		call Error (56);
	     if tape_io_data.control.force then		/* -force option  specified for unlabeled tape */
		call Error (35);
	end;
	else do;					/* ANSI and labeled IBM checks */
	     if fcb.tape.output_mode = 4 then		/* ANSI and IBM checks  -  create */
		if fcb.tape.file_id = "*" then	/* invalid file id for create */
		     call Error (53);
		else ;
	     else if fcb.tape.output_mode = 0 then do;	/* tape input mode */
		if fcb.tape.format = 0 then		/* no format specified on input */
		     if fcb.tape.reclen > 0 | fcb.tape.blklen > 0 then /* reclen or blklen illegal */
			call Error (42);
		if fcb.tape.format > 1 then		/* if format was specified, record and/or block */
		     if fcb.tape.reclen = 0 | fcb.tape.blklen = 0 then /*  length must be specified */
			call Error (42);
	     end;
	     else if fcb.tape.output_mode < 3 then do;	/* output mode is extend or modify */
		if fcb.tape.expiration ^= "" then	/* and expiration specified */
		     if fcb.tape.output_mode = 1 then	/* if extend.. */
			call Error (39);
		     else call Error (40);		/* if modify */
	     end;
	     if fcb.tape.sequence = 0 | fcb.tape.sequence = -1 then /* no explicit sequence  or "*" */
		if fcb.tape.file_id = "*" then	/* and no <fileid> */
		     call Error (41);
	end;

	go to test (fcb.tape.format);			/* now go validate the format */

test (1):						/* U-format   */
	if fcb.tape.reclen ^= 0 then
	     fcb.tape.reclen = 0;			/* reclen must be zero */
	if fcb.tape.blklen = 0 then do;
	     call Error (43);
	     call ioa_ ("Tape file block size is ^d", fcb.tape.blklen);
	end;
	go to out;
test (2):						/* F-format  */
test (5):						/* FB-format */
	if fcb.tape.blklen ^= 0 & fcb.tape.reclen ^= 0 then do; /* non zero block & record length  */
	     if fcb.tape.format = 5 then do;		/* FB check */
		if mod (fcb.tape.blklen, fcb.tape.reclen) ^= 0 then do;
		     call Error (16);
		     call ioa_ ("Tape file record size is ^d", fcb.tape.reclen);
		     call ioa_ ("Tape file block size is ^d", fcb.tape.blklen);
		end;
	     end;
	     else if fcb.tape.blklen ^= fcb.tape.reclen then do; /* F format check */
		call Error (15);
		call ioa_ ("Tape file record size is ^d", fcb.tape.reclen);
		call ioa_ ("Tape file block size is ^d", fcb.tape.blklen);
	     end;
	end;
	go to out;
test (3):						/* D-format */
test (4):						/* V-format */
test (6):						/* DB-format  */
test (7):						/* VB-format */
	if fcb.tape.blklen ^= 0 & fcb.tape.reclen ^= 0 then do; /* d/v format */
	     if vcb.tape_type = ANSI then
		j = 0;				/* don't allow for BDW if ANSI */
	     else j = 4;				/* IBM - allow for 4 byte BDW */
	     if fcb.tape.format > 4 then do;		/* blocked: reclen must be <= blklen */
		if fcb.tape.blklen < fcb.tape.reclen + j then do;
		     call Error (17);
		     call ioa_ ("Tape file record size is ^d", fcb.tape.reclen);
		     call ioa_ ("Tape file block size is ^d", fcb.tape.blklen);
		end;
	     end;
	     else if fcb.tape.blklen ^= fcb.tape.reclen + j then do; /* V-format, D-format */
		call Error (15);
		call ioa_ ("Tape file record size is ^d", fcb.tape.reclen);
		call ioa_ ("Tape file block size is ^d", fcb.tape.blklen);
	     end;
	end;
test (8): test (9):
test (10): test (11):				/* S, SB, VS, VB, VS, VBS formats, all is possible */
test (0):						/* for reading, format code is 0 */
out:
	return;

     end Check_FCB;

%include tape_io_data;

%include area_info;

%include tape_io_errors;
