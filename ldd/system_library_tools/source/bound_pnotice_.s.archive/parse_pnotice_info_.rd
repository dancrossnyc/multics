/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/* parse_pnotice_info_:  A routine to parse >tools>psp_info_ for display_psp and	*/
	/* generate_pnotice commands.							*/
	/*									*/
	/* 0) Created 04/14/81 by R. Holmstedt						*/
	/* 1) Modified 10/15/84 by G. Dixon - use search rules to find psp_info_.		*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

/*  open set of reductions for software ids  */

/*++
BEGIN	/<no-token>		/ERROR (1) /RETURN   \  


in_parm  /Define : <validate_MI> ;      /LEX (2) MI_prod LEX (2)   /in_parm \

        /product : <any-token> ;        /LEX (2) prod_name LEX (2)  /in_parm \

        /titles : <quoted-string> ;     /LEX (2) prod_title LEX (2) /in_parm \

        /STI : <validate_STI> ;         /LEX (2) prod_STI LEX (2)   /in_parm \


        /use : <any-token> ;            /LEX (2) prod_USE LEX (2)   /in_parm \

        /source_C :                     /LEX (2) [obj_sw = "0"b] PUSH(in_parm)      /names   \

        /object_C :                     /LEX (2) [obj_sw = "1"b] PUSH(in_parm)      /names   \

        /x_path : <check_path> ;        /LEX (2) prod_path("xecute") LEX (2)  /in_parm \
        /x_path : <any-token> ;	/LEX (2) ERROR (2) NEXT_STMT/in_parm \

        /source_path : <check_path> ;   /LEX (2) prod_path("source") LEX (2)  /in_parm \
        /source_path : <any-token> ;	/LEX (2) ERROR (2) NEXT_STMT/in_parm \

        /object_path : <check_path> ;	/LEX (2) prod_path("object") LEX (2)  /in_parm \
        /object_path : <any-token> ;	/LEX (2) ERROR (2) NEXT_STMT/in_parm \


        / End;	   	           / LEX (2)	       / finish \
         /<any-token>                    /ERROR(4)  NEXT_STMT      /in_parm \
         /<no-token>                     /ERROR (3)	       /RETURN \


finish
	/ <any-token>		 / ERROR (5)               / RETURN \
	/ <no-token>	           /		       / RETURN \

names    / <any-token>                   /prod_C LEX PUSH(names)   /punct    \
         / ;                             /ERROR(2) LEX             /STACK_POP\
         / ,                             / LEX                     /names    \
         / <any-token>                   /ERROR(2) LEX PUSH(names) /punct    \
         / <no-token>                    /ERROR(3)                 /RETURN   \

punct    /;                               / LEX POP                /STACK_POP\
         /,                               / LEX                    /STACK_POP\
         / <any-token>                    / ERROR(2) NEXT_STMT POP /STACK_POP\
         / <no-token >                    / ERROR(2)               /RETURN   \

++*/



/*   close set of reductions    */




%;

parse_pnotice_info_: procedure (input_ptr, code);

dcl (APstmt, APtoken) ptr init (null ());
dcl  LEGAL char (80)aligned init ("    0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'_-^` ");
dcl (LEXDLM, LEXCTL) char (128) varying internal static;
dcl (BREAKS, IGBREAKS) char (128) varying internal static;
dcl Ccode fixed bin (35);
dcl STI char(12);
dcl bit_ch fixed bin (24);
dcl bc fixed bin (21);
dcl code fixed bin (35);
dcl count_S fixed bin;
dcl count_O fixed bin;
dcl cf_ptr ptr;
dcl dirname char(168);
dcl 01 error_control_table (6) internal static options (constant),
       02 severity fixed bin (17) unaligned init ((6) 2),
       02 Soutput_stmt bit (1) unaligned init ((6) (1) "1"b),
       02 message char (80) varying init (
/* ERROR 1*/ "The psp_info_ segment contains no statments.",
/* ERROR 2*/ "The psp_info_ segment contains an incorrect line.",
/* ERROR 3*/ "The psp_info_ segment does not contain an End statment.",
/* ERROR 4*/ "The line containing the keyword ""^a"" is incorrect.",
/* ERROR 5*/ "Text follows the End statment.",
/* ERROR 6*/ "The STI ""^a"" is incorrect, only uppercase letters or numbers are valid."),
       02 brief_message char (4) varying init ((6) (1) " ");

dcl expand_pathname_ entry (char(*), char(*), char(*), fixed bin(35));
dcl entryname char (32);
dcl  first bit (1) init ("1"b) int static;
dcl hcs_$status_mins entry (ptr, fixed bin(2), fixed bin(24), fixed bin(35));
dcl i fixed bin;
dcl ii fixed bin init (0);
dcl input_ptr ptr;			/* passed in from a call	       */
dcl lex_error_ entry options (variable);
dcl lex_string_$init_lex_delims entry (char(*), char(*), char(*), char(*),
	char(*), bit(*), char(*) var, char(*) var, char(*) var, char(*) var);

dcl lex_string_$lex  entry (ptr, fixed bin(21), fixed bin(21), ptr, bit(*),
	char(*), char(*), char(*), char(*), char(*), char(*) var,
	char(*) var, char(*) var, char(*) var, ptr, ptr, fixed bin(35));
dcl name char(19) init ("");
dcl obj_sw bit (1);
dcl pathname char (168);
dcl proc_ptr  ptr;
dcl 01 product_init aligned int static options (constant),
       02 num(1),
          03 MI char (7) init (""),
          03 prod_name char(20) init (""),
          03 prod_title char (80) init (""),
          03 prod_STI char (12) init (""),
          03 source_C(10) char (24) init ((10) (1) ""),
          03 object_C(10) char (24) init ((10) (1) ""),
	   03 x_path,
	      04 dirname char(168) init (""),
	      04 entryname char(32) init (""),
	   03 source_path,
	      04 dirname char(168) init (""),
	      04 entryname char(32) init (""),
	   03 object_path,
	      04 dirname char(168) init (""),
	      04 entryname char(32) init (""),
          03 prod_use(10) char (7) init ((10) (1)"");
dcl  psp_info_$ fixed bin(35) ext static;
dcl  translator_temp_$get_segment entry (char(*), ptr, fixed bin(35));
dcl  translator_temp_$release_all_segments entry (ptr, fixed bin(35));


dcl  error_table_$translation_failed fixed bin (35) ext static;

dcl (addr, collate, dimension, divide, length, null, substr, verify) builtin;



%include software_pnotice_info_;

	   
/* START			       */
	name = "parse_pnotice_info_";        /* set command name */
	cf_ptr = null;
	proc_ptr = null;

	call translator_temp_$get_segment (name, proc_ptr, code);
				/* area for lex_string_	       */
	if  code ^= 0 then goto fini;
	
	SI_ptr = input_ptr;		/* work area for info structure      */

	product.prod_number = 0;	/* init the structure count	       */
	

	cf_ptr = addr(psp_info_$);
	call hcs_$status_mins (cf_ptr, 0, bit_ch, code);

	bc = divide (bit_ch + 8, 9, 24, 0);
	if first then do;
	     BREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24) || "()*,:;^";
	     IGBREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24);

	     call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b, BREAKS, IGBREAKS, LEXDLM, LEXCTL);
	     first = "0"b;

	end;


	call lex_string_$lex
          (cf_ptr, bc, 0, proc_ptr, "100"b, """", """", "/*", "*/", ";", BREAKS, IGBREAKS, LEXDLM, LEXCTL, APstmt, APtoken, code);
	Pthis_token = APtoken;

	call SEMANTIC_ANALYSIS ();

	if MERROR_SEVERITY > 1 then do;

	     if code = 0 then code =  error_table_$translation_failed;
	     goto fini;
	end;


fini:	if proc_ptr ^= null then
	     call translator_temp_$release_all_segments ( proc_ptr, Ccode);
	 proc_ptr = null;
	 cf_ptr = null;
	 SI_ptr = null;
	return;
	

/*\014 */
MI_prod: proc;


	     product.prod_number = product.prod_number + 1;	/* fill up the structure   */
	     product.num(product.prod_number) = product_init.num(1);
				/* clean it up before using	       */

	     product.num(product.prod_number).MI = token_value;

	     ii = 0;		/* init the Use field count	       */
	     count_O = 0;		/*init the count of object for prod_C*/
	     count_S = 0;		/*init the count of source for prod_C*/

	     return;
	end MI_prod;

prod_name: proc;


	     product.num(product.prod_number).prod_name = token_value;

	     return;
	end prod_name;


check_path: proc returns (bit (1));

dcl  R bit (1);
dcl expand_pathname_ entry (char(*), char(*), char(*), fixed bin(35));
dcl dirname char(168);
dcl entryname char (32);
dcl pathname char (168);
	  
	  pathname = token_value;
	  call expand_pathname_ (pathname, dirname, entryname, code);
	  if code = 0 then R = "1"b;
	  else R = "0"b;
	  return (R);
       end check_path;
       

prod_title: proc;
	     product.num(product.prod_number).prod_title = token_value;

	     return;
	end prod_title;


prod_USE: proc;

				/* set ii to 0 in MI_prod procedure  */
	ii = ii + 1;		/* count the number of MIs  used*/
	product.num(product.prod_number).prod_use(ii) = token_value;

	return;
	end prod_USE;

validate_MI:
	proc returns (bit (1));
	
	dcl alph char(26) int static options (constant) init ("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
	dcl numbers char (10) init static options (constant) init ("1234567890");
	dcl MI_check char (7);

	if length(token_value) ^= 7 then return ("0"b);

	MI_check = token_value;
	if verify (substr(MI_check,1,3), alph) ^= 0 then return ("0"b);

	if verify(substr(MI_check,4,4), numbers) ^= 0 then return ("0"b);
	return ("1"b);
     end validate_MI;
     
	 
validate_STI: proc  returns (bit (1));

	    dcl parse_pnotice_info_$validate_sti entry (char(12)) returns (bit(1));

	    STI = token_value;
	    if parse_pnotice_info_$validate_sti (STI) then return ("1"b);
	    else return ("0"b);
	    

         end validate_STI;

prod_STI:	    proc;
	    
	    product.num(product.prod_number).prod_STI = token_value;
	    return;
	    end prod_STI;
	         

prod_C: proc;
        
        if obj_sw then do;
	   count_O = count_O + 1;
	   product.num(product.prod_number).object_C(count_O) = token_value;
        end;
        else do;
	   count_S = count_S + 1;
	   product.num(product.prod_number).source_C(count_S) = token_value;
        end;
	     return;
	end prod_C;

prod_path: proc (type);

dcl type char (6);
	 
	  
	  pathname = token_value;
	  call expand_pathname_ (pathname, dirname, entryname, code);
	  if code ^= 0 then do;
	       call statement_error (2, token_value, "");
	       return;
	  end;
	  
	  if type = "source" then do;
	     product.num(product.prod_number).source_path.dirname = dirname;
	     product.num(product.prod_number).source_path.entryname = entryname;
	end;
	else if type = "object" then do;
	     product.num(product.prod_number).object_path.dirname = dirname;
	     product.num(product.prod_number).object_path.entryname = entryname;
	end;
	else if type = "xecute" then do;
	     product.num(product.prod_number).x_path.dirname = dirname;
	     product.num(product.prod_number).x_path.entryname = entryname;
	end;
	     return;
	end prod_path;


statement_error: proc (error_num, parm1, parm2);

dcl  error_num fixed bin;
dcl  parm1 char (*);
dcl  parm2 char (*);
dcl (stmt_ptr, token_ptr) ptr init (null);

	     stmt_ptr = token.Pstmt;
	     token_ptr = Pthis_token;

	     call lex_error_ (error_num, SERROR_PRINTED (error_num), (error_control_table.severity (error_num)),
		MERROR_SEVERITY, stmt_ptr, token_ptr, SERROR_CONTROL,
		(error_control_table.message (error_num)), (error_control_table.brief_message (error_num)),
		parm1, parm2);

	     return;

	end statement_error;

validate_sti: entry (entered_value) returns (bit (1));

	dcl R bit (1);
	dcl entered_value char(12);
	dcl valid_numeric char(36) int static options (constant)  init("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");

	    R = "1"b;

	    do i = 1 to 12;
	         if verify (substr(entered_value,i,1),valid_numeric) ^= 0 then R = "0"b;
				/* only uppercase and numbers valid  */
	    end;


	    if verify (substr(entered_value,2,1),"1234") ^= 0 then R = "0"b;
				/* only 4 numbers are valid	       */

	    return (R);		/* good return		       */

