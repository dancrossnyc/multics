/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1985 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(1985-09-03,LJAdams), approve(1985-11-06,MCR7278),
     audit(1986-02-19,Gilcrease), install(1986-02-19,MR12.0-1021):
     Parses and validates the history comments.
  2) change(1986-04-17,LJAdams), approve(1986-05-19,MCR7386),
     audit(1986-05-19,Gilcrease), install(1986-06-05,MR12.0-1071):
     Added error message parameter for validate programs.  Changed so
     that only 1 error message is put out for all programs called.
  3) change(1986-08-28,LJAdams), approve(1986-08-28,MCR7526),
     audit(1986-11-05,GDixon), install(1986-11-12,MR12.0-1213):
     error_msg was not getting initialized  which resulted in garbage being
     displayed.  Set d.Scfix to True when first critical fix number is
     encountered; thereby preventing addition of non-critical fix numbers.
  4) change(1987-03-26,LJAdams), approve(1987-03-26,MCR7653),
     audit(1987-04-22,Gilcrease), install(1987-04-26,MR12.1-1026):
     If comment is greater than max length allowed put char value of
     comment length in src_array_comment.err_msg.
  5) change(1987-03-30,LJAdams), approve(1987-03-30,MCR7653),
     audit(1987-04-22,Gilcrease), install(1987-04-26,MR12.1-1026):
     Put in check for pre-b2 cmts must have null approve, null audit, and null
     install fields present.
  6) change(2016-01-15,Swenson), approve(2016-01-15,MCR10006):
     Fix history_comment to use 4-digit years and be able to handle current
     date/times when there are existing history comments without thinking that
     the new ones are in the past.
                                                   END HISTORY COMMENTS */

hcom_parse_:
  proc (ERROR_RETURN_LABEL, seg, cmt, path, Sprt_path, Pd, src_array_comment, code);

dcl ERROR_RETURN_LABEL         label parameter,
    seg	                     char(*),
    cmt			 char(*),
    code			 fixed bin(35),
    path			 char(*),
    Sprt_path		 bit(1);

/*++
INCLUDE                        ERROR\

BEGIN 

           / <decimal-integer> ) change ( <date> , <change_pers> ) /
                               [src_array_comment.comment_no = token.Nvalue]
			 LEX(4)
			 [src_array_comment.change_dt = date_out] 
			 LEX(2)
			 [src_array_comment.change_person = person]
			 LEX(2)
                               / punct \

           / <decimal-integer> ) change ( <date> , <change_pers> <any-token> /
                               LEX(7)
			 MY_ERROR(19) / RETURN \
           / <decimal-integer> ) change ( <date> , <any-token> /
                               LEX(6)
			 MY_ERROR(3) / RETURN \
           / <decimal-integer> ) change ( <any-token> /
	                     LEX(4)
	                     MY_ERROR(2) / RETURN \
           / <any-token> )     / MY_ERROR(1) / RETURN \
	 / <any-token>	 / MY_ERROR(4) / RETURN \
	 / <no-token>	 / MY_ERROR(5) / RETURN \

punct 	 / :		 / LEX
			 set_text 
			 / RETURN \
           / ,		 / LEX
			 / opt \
           / <any-token>       / MY_ERROR(6) /RETURN \
           / <no-token>        / MY_ERROR(7) /RETURN \

opt        / approve (         / 
			 / opt_arg \
	 / audit (           /
                               / opt_arg \
	 / install (         /
                               / opt_arg \
           / <any-token> (     / MY_ERROR(16) /RETURN\
           / <any-token>       / MY_ERROR(17) /RETURN \
           / <no-token>        / MY_ERROR(18) /RETURN \

opt_arg	 / approve ()        /
                               [src_array_comment.approve_dt = "^"]
			 [src_array_comment.approve_value = ""]
                               [null_approve = True]
			 LEX(3)
			 / punct \
	 \" The reduction above allows comments created prior to existence of
	 \" hcom command to appear to have a nonempty approve field, even
	 \" those no date or approval value is known/specified.  The ^ date
	 \" value makes comments with such a field match the approve comment
	 \" specifier.
	 / approve ( <date> , <apv_id> ) /
			 LEX(2)
			 [src_array_comment.approve_dt = date_out]
			 LEX(2)
			 [src_array_comment.approve_value = ident] 
                               LEX(2)
                               / punct \  
           / audit ()          /
                               [src_array_comment.audit_dt = "^"]
			 [src_array_comment.audit_person = ""]
			 [null_audit = True]
			 LEX(3)
			 / punct \
	 \" Allow pre-hcom comments with unknown audit fields to appear
	 \" to be audited.
           / audit ( <date> , <audit_pers> ) /
			 LEX(2)
			 [src_array_comment.audit_dt = date_out]
			 LEX(2)
			 [src_array_comment.audit_person = person] 
			 LEX(2)
                               / punct \  
           / install ()        /
                               [src_array_comment.install_dt = "^"]
			 [src_array_comment.install_id = ""]
			 [null_install = True]
			 LEX(3)
			 / punct \
	 \" Allow pre-hcom comments with unknown install fields to appear
	 \" to be installed.
           / install ( <date> , <install_id> ) /
			 LEX(2)
			 [src_array_comment.install_dt = date_out]
			 LEX(2)
			 [src_array_comment.install_id = ident] 
			 LEX(2)
                               / punct \  
           / approve ( <date> , <apv_id> <any-token> /
                               LEX(5)
			 MY_ERROR(19) / RETURN \
           / approve ( <date> , <any-token> /
                               LEX(4)
			 MY_ERROR(11) / RETURN \
           / approve ( <any-token> /
                               LEX(2)
                               MY_ERROR(10) / RETURN \
           / approve <any-token> /
                               LEX
                               MY_ERROR(6) / RETURN \
           / audit ( <date> , <audit_pers> <any-token> /
                               LEX(5)
			 MY_ERROR(19) / RETURN \
           / audit ( <date> , <any-token> /
                               LEX(4)
			 MY_ERROR(13) / RETURN \
           / audit ( <any-token> /
                               LEX(2)
			 MY_ERROR(12) / RETURN \
           / audit <any-token> /
                               LEX
                               MY_ERROR(6) / RETURN \
           / install ( <date> , <install_id> <any-token> /
                               LEX(5)
                               MY_ERROR(19) / RETURN \			 
           / install ( <date> , <any-token> /
                               LEX(4)
                               MY_ERROR(15) / RETURN \			 
           / install ( <any-token> /
                               LEX(2)
			 MY_ERROR(14) / RETURN \
           / install <any-token> /
                               LEX
                               MY_ERROR(6) / RETURN \
           / <any-token>	 / MY_ERROR(8) / RETURN \
  	 / <no-token>	 / MY_ERROR(9) / RETURN \
++*/

/* close set of reductions   */

%include hcom_data;

dcl 1 src_array_comment   aligned like src_array.comments;

dcl 1 error_control_table (19) internal static options (constant),
      2 severity               fixed bin (17) unal init ((19) 3),
      2 Soutput_stmt	 bit(1) unal init ((19) (1) "1"b),
      2 message		 char(80) varying init (
      /* ERROR 1*/             "The history comment number (^a) is not a decimal number.",
      /* ERROR 2*/             "The history comment contains an invalid date (^a).",
      /* ERROR 3*/             "The history comment contains an invalid person id (^a).",
      /* ERROR 4*/             "The history comment contains an incorrect line.",
      /* ERROR 5*/             "The history comment is empty.",
      /* ERROR 6*/             "The history comment contains invalid punctuation (^a).",
      /* ERROR 7*/		 "The history comment contains no punctuation",
      /* ERROR 8*/		 "The history comment contains an invalid option (^a).",
      /* ERROR 9*/		 "The history comment ends improperly before the summary.",
      /* ERROR10*/             "The approve date (^a) is invalid.",
      /* ERROR11*/		 "The approve value (^a) is invalid.",
      /* ERROR12*/		 "The audit date (^a) is invalid.",
      /* ERROR13*/		 "The audit person (^a) is invalid.",
      /* ERROR14*/		 "The install date (^a) is invalid.",
      /* ERROR15*/		 "The install id (^a) is invalid.",
      /* ERROR16*/             "The history comment has an invalid option name (^a).",
      /* ERROR17*/             "The history comment is missing left parenthesis.",
      /* ERROR18*/             "The history comment ends with invalid punctuation (^a).",
      /* ERROR19*/             "The history comment is missing right parenthesis"),
      2 brief_message	 char(4) varying init((19) (1) " ");
  
dcl 1 cond_info                aligned like condition_info;

dcl com_err_	           entry() options(variable);

dcl error_table_$bigarg	 fixed bin(35) ext static;
dcl error_table_$improper_data_format
			 fixed bin(35) ext static;

dcl find_condition_info_	 entry (ptr, ptr, fixed bin(35));

dcl hcom_cfix_validate_	 entry (char(*) var, char(*) var, char(*) var, bit(1),
                                             char(*) var, char(*) var, char(100) var);

dcl hcom_site_validate_	 entry (char(*) var, char(*) var, char(*) var, bit(1) aligned,
				     char(*) var, char(*) var, char(100) var);

dcl hcom_default_validate_	 entry (char(*) var, char(*) var, char(*) var, bit(1) aligned,
				     char(*) var, char(*) var, char(100) var);

dcl lex_string_$init_lex_delims
                               entry (char(*), char(*), char(*), char(*), char(*), bit(*),
				     char(*) var, char(*) var, char(*) var, char(*) var);

dcl lex_string_$lex		 entry (ptr, fixed bin(21), fixed bin(21), ptr, bit(*), char(*),
                                             char(*), char(*), char(*), char(*), char(*) var, char(*) var,
			               char(*) var, char(*) var, ptr, ptr, fixed bin(35));

dcl LEXDLM                     char(128) varying internal static,
    LEXCTL		 char(128) varying internal static;

dcl convert_date_to_binary_	 entry (char(*), fixed bin(71), fixed bin(35)),
    date_time_$format	 entry (char(*), fixed bin(71), char(*), char(*)) returns(char(250) var),
    ioa_			 entry() options(variable),
    pathname_$component	 entry (char(*), char(*), char(*)) returns(char(194)),
    translator_temp_$get_segment 
                               entry (char(*), ptr, fixed bin(35)),

    translator_temp_$release_all_segments 
                               entry (ptr, fixed bin(35));

dcl error_table_$translation_failed 
			fixed bin(35) ext static;
dcl (addr,
     addcharno,
     charno,
     char,
     dimension,
     index,
     length,
     maxlength,
     null,
     reverse,
     substr,
     verify)          	 builtin;

dcl  proc_ptr		 ptr,
     Ccode		 fixed bin(35);

dcl  BREAKS                    char(9) varying int static options(constant) init ("	 (),:
");
     /* break characters consist of HT,VT,SP,RP,LP,CM,CLN,NL,NP  */  
dcl IGBREAKS                   char(5) varying int static options(constant) init("	 
");
     /* ignore break characters consist of HT,VT,NP,SP,NL    */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/


dcl APstmt                     ptr,
    APtoken		 ptr;

dcl Lignore		 fixed bin(21);

dcl (cleanup, command_question)  condition;

dcl True			 bit(1) internal static options (constant) init("1"b),
    False			 bit(1) internal static options (constant) init("0"b);

dcl (null_approve,
     null_audit,
     null_install)               bit (1);

dcl valid			 bit(1),
    Serrors		 bit(1);

dcl error_msg                  char(100) varying;

Serrors = False;
error_msg = "";

null_approve, null_audit, null_install = False;

if ^d.ag.ctl.errors then
   MIN_PRINT_SEVERITY = 4;

proc_ptr = null;
on cleanup
  call JANITOR();
  
Lignore = charno(addr(cmt)) - charno(addr(seg));

call translator_temp_$get_segment (CALLER, proc_ptr, code);
if code ^= 0 then 
  call JANITOR;

	/* BREAKS consist of HT VT SP RP LP CM NL NP                */
	/* IGBREAKS to ignore consist of HT VT NP SP NL             */

call lex_string_$init_lex_delims ("","","","","","11"b, BREAKS, IGBREAKS, LEXDLM, LEXCTL);

call lex_string_$lex (addr(seg), length(cmt)+Lignore, Lignore, proc_ptr, "0000"b,"","","","","",BREAKS,
  IGBREAKS, LEXDLM, LEXCTL, APstmt, APtoken, code);

Pthis_token = APtoken;

call SEMANTIC_ANALYSIS ();

if Serrors then 
  if d.ag.ctl.errors then
     call ioa_("^/^a",cmt);

if MERROR_SEVERITY > 0 then
  if code = 0 then
    code = error_table_$translation_failed;
call JANITOR;
return;

JANITOR:
  proc;
  if proc_ptr ^= null then
    call translator_temp_$release_all_segments (proc_ptr, Ccode);
  proc_ptr = null;
  
  if Serrors then
     goto ERROR_RETURN_LABEL;

end JANITOR;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/


/* RELATIVE SYNTAX FUNCTIONS  */

dcl clock_time			fixed bin(71),
      date_out			char(10) aligned;

date:
  proc () returns(bit(1) aligned);

  dcl code			fixed bin(35);

  call convert_date_to_binary_(token_value,clock_time,code);
  if code ^= 0 then
     return(False);

  date_out = date_time_$format("^9999yc-^my-^dm",clock_time,"","");
  return(code=0);
  
end date;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

  dcl person			char(24) var,
      ident			char(24) var;


audit_pers:
  proc() returns(bit(1) aligned);
  
  on command_question
     begin;
        call set_command_question;
        end;

  if d.ag.op.name = REPLACE_FIELD then			/* dont validate fields being replaced		*/
     if d.ag.input.select.aud = OPERANDxxx |
        d.ag.input.select.aud = INPUTxxx then do;
        person = token_value;
        return("1"b);
        end;

  call d.ag.vdt ((CALLER), AUDIT_FIELD_NAME, (token_value), valid, person, "",error_msg);

  return(valid);
  
/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

change_pers:
  entry() returns(bit(1) aligned);
  
  on command_question
     begin;
        call set_command_question;
        end;
  call d.ag.vdt ((CALLER), AUTHOR_FIELD_NAME, (token_value), valid, person, "", error_msg);

  return(valid);

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

apv_id:
  entry() returns(bit(1) aligned);

  if d.ag.op.name = REPLACE_FIELD then		/* dont validate fields being replaced		*/
        if d.ag.input.select.apv = OPERANDxxx |
           d.ag.input.select.apv = INPUTxxx then do;
           ident = token_value;
           return("1"b);
	 end;

  if index(token_value,"fix_") > 0 then do;
     Scfix_found = True;
     d.Scfix_found = True;
     end;

  on command_question
     begin;
     call set_command_question;
     end;
  
  if Scfix_found then
     d.ag.vdt = hcom_cfix_validate_;
  else if d.Ssite then
     d.ag.vdt = hcom_site_validate_;
  else
     d.ag.vdt = hcom_default_validate_;

  call d.ag.vdt ((CALLER), APPROVAL_FIELD_NAME, (token_value), valid, ident, "", error_msg);

  return(valid);
    

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

install_id:
  entry() returns(bit(1) aligned);
  
  if d.ag.op.name = REPLACE_FIELD then			/* dont validate fields being replaced		*/
     if d.ag.input.select.in = OPERANDxxx |
        d.ag.input.select.in = INPUTxxx then do;
        ident = token_value;
        return ("1"b);
        end;

  if Scfix_found then do;
     call hcom_cfix_validate_((CALLER), INSTALL_FIELD_NAME, (token_value), valid, ident, "", error_msg);
     return(valid);
     end;

  on command_question
     begin;
     call set_command_question;
     end;

  call d.ag.vdt ((CALLER), INSTALL_FIELD_NAME, (token_value), valid, ident, "", error_msg);
  
  return(valid);


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/
set_command_question:
  proc;

dcl answer		 char(command_question_info.answer_lth)
			 based(command_question_info.answer_ptr);

  revert command_question;
  cond_info.version = condition_info_version_1;
  call find_condition_info_ (null, addr(cond_info), code);
  cq_info_ptr = cond_info.info_ptr;
  if command_question_info.yes_or_no_sw & 
     command_question_info.max_answer_lth >= length("yes") then do;
     command_question_info.preset_sw = True;
     command_question_info.question_sw = False;
     command_question_info.answer_sw = False;
     command_question_info.answer_lth = length("yes");
     answer = "yes";
     end;
end set_command_question;

RETURN_FALSE:
  
  return(False);

end audit_pers;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/


/* ACTION ROUTINES */

MY_ERROR:
  proc(err_no);

  dcl err_no			fixed bin;

  Serrors = True;

  if ^Sprt_path then do;
     if d.ag.ctl.errors then
        call ioa_("^a",path);
     Sprt_path = True;
     end;
  
  if d.Saf then
     call d.set_return_arg ("false");

  if code = error_table_$bigarg then
     call com_err_ (code, CALLER,
     "^a^/^3xComment ^d is longer than ^d characters.",
     pathname_$component(seg.dir,seg.ent,seg.comp),src_array_comment.comment_no,maxlength(d.ag.input.summary));

  call ERROR(err_no);
  if error_msg ^= "" then
     call ioa_("^3x^a",error_msg);

  return;
  
end MY_ERROR;


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/


set_text:
  proc;
  dcl text                     char(Ltext) based(Ptext),
      Ptext		 ptr,
      Ltext		 fixed bin(21),
      text_arr (Ltext)         char(1) based(Ptext),
      Ptext_line		 ptr,
      Ltext_line		 fixed bin(21),
      text_line		 char(Ltext_line) based(Ptext_line),
      i			 fixed bin(21),
      Iline		 fixed bin(21),
      HT_SP_NL_VT              char(4) int static options(constant) init("	 
"),
      HT_SP_VT                 char(3) int static options(constant) init("	 "),
      SPACES                   char(5) int static options(constant) init("     "),
      NL			 char(1) int static options(constant) init("
");
  
  
  if null_approve & null_audit & ^null_install then do;	/* check to be sure old cmts are properly	*/
						/* formatted.				*/
     src_array_comment.err_msg(1) =  char(error_table_$improper_data_format);
     return;
     end;
            
  Ptext = (addr(cmt));				/* determine if there is any leading wt space	*/
  Ltext = charno(addr(token_value)) - charno(addr(cmt));
  i = verify(reverse(text),HT_SP_VT);
  Ltext = Ltext - i;

  Ptext = addcharno (addr(text_arr(Ltext)), length(NL)+1);	/* charno is offset 0			*/
  
  Ltext = length(cmt) - (charno(Ptext) - charno(addr(cmt)));
  
  if Ltext > maxlength(d.ag.input.summary) then do;
     src_array_comment.err_msg(1) = char(error_table_$bigarg);
     src_array_comment.err_msg(2) =  char(Ltext);
     Ltext = maxlength(d.ag.input.summary);
     end;

  do while(Ltext > 0 & verify(text,HT_SP_NL_VT) ^= 0);
     Iline = index(text,NL);
     if Iline = 0 then do;
        Ptext_line = Ptext;
        Ltext_line = length(text);
        Ltext = 0;
        end;
     else do;
        Ptext_line = Ptext;
        Ltext_line = Iline;
        if Ltext > Iline then
	 Ptext = addr(text_arr(Iline+1));
        Ltext = Ltext - Iline;
        end;

     if verify(text_line,HT_SP_NL_VT) = 0 then		/* nothing but white space			*/
        src_array_comment.summary = src_array_comment.summary || text_line;
     else if substr(text_line,1,5) = SPACES then
						/* hcom_process_seg_ indents every line but the	*/
						/* first five spaces.			*/
        src_array_comment.summary = src_array_comment.summary || substr(text_line,6);
     else	do;					/* if not using hcom to format spacing may differ */
        i = verify(text_line," ");
        src_array_comment.summary = src_array_comment.summary || substr(text_line,i);
        end;
     end;

end set_text;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/
%include condition_info_header;

%include condition_info;

%include command_question_info;


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

%include hcom_field_names;

