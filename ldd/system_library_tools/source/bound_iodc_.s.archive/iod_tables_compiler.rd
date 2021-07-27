/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(88-01-27,Brunelle), approve(), audit(), install():
     Written by J. Stern, January 1975
     Modified by J. C. Whitmore, April 1978 for new RQT substatements
     Modified by J. C. Whitmore, Oct 1978, for version 3 -> adding the
     line_id table
     Modified by J. C. Whitmore, Jan 1980, to remove warning 7 - ctl term
     illegal with line variable construct.
     Modified by R. McDonald May 1980  to add keywords card_charge and
     page_charge. (UNCA)
     Modified: 10 April 1981 by G. Palter to remove warning 4 --
     dprint/dpunch restricts request type names to < 9 characters
     Modified by E. N. Kittlitz June 1981 for UNCA card_charge, page_charge
  2) change(88-01-27,Brunelle), approve(88-10-31,MCR7911),
     audit(88-10-21,Wallman), install(89-10-23,MR12.3-1099):
     Converted to reduction form.  Add new statements.
  3) change(88-10-31,Brunelle), approve(88-10-31,MCR7911),
     audit(88-11-01,Wallman), install(89-10-23,MR12.3-1099):
     Add cross check to verify that any device defined as allowed for a
     request_type has a forms_table entry if the request_type has a
     forms_table entry.
  4) change(89-02-28,Brunelle), approve(89-10-17,MCR8140),
     audit(89-10-23,Beattie), install(89-10-23,MR12.3-1099):
     Correct problem of truncated output file because bitcount not being set
     properly.
                                                   END HISTORY COMMENTS */
%page;

/*++
\" REDUCTIONS FOR iod_tables_compiler

BEGIN	/ <no-token>			/ ERROR (1)				/ RETURN \

\" Scan for main delimiting statements
	/ Time : <decimal-integer> ;		/ LEX (2) store_global_grace_time NEXT_STMT	/ BEGIN \
	/ Time : <any-token>		/ ERROR (3) NEXT_STMT			/ BEGIN \

	/ Max_queues : <decimal-integer> ;	/ LEX (2) store_global_max_queues NEXT_STMT	/ BEGIN \
	/ Max_queues : <any-token>		/ ERROR (5) NEXT_STMT			/ BEGIN \

\" now the main group delimiting statements 
	/ Line : <name_32> ;		/ LEX (2) line_create_table_entry LEX (2)	/ line_stmts \
	/ Line : <any-token>		/ ERROR (3)
					  LEX (2) line_create_table_entry NEXT_STMT	/ line_stmts \

	/ Device : <name_24> ;		/ LEX (2) dev_create_table_entry LEX (2)	/ device_stmts \
	/ Device : <any-token>		/ ERROR (3)
					  LEX (2) dev_create_table_entry NEXT_STMT	/ device_stmts \

	/ Request_type : <name_24> ;		/ LEX (2) rqt_create_table_entry LEX (2)	/ rqt_stmts \
	/ Request_type : <any-token>		/ ERROR (3)
					  LEX (2) rqt_create_table_entry NEXT_STMT	/ rqt_stmts \

	/ Forms_table : <name_32> ;		/ LEX (2) forms_create_group_entry LEX (2)	/ forms_stmts \
	/ Forms_table : <any-token>		/ ERROR (3)
					  LEX (2) forms_create_group_entry NEXT_STMT	/ forms_stmts \

	/ End ;				/ LEX
					  [if token.Pnext ^= null () then
					     call statement_error ()]			/ RETURN \

	/ <any-token> :			/ ERROR (2) NEXT_STMT			/ BEGIN \
	/ <any-token> 			/ ERROR (3) NEXT_STMT			/ BEGIN \
	/ <no-token> 			/ ERROR (1)				/ RETURN \
\" 

\" section to decode Line statements
line_stmts

	/ comment : <quoted-string> ;		/ LEX (2) line_save_comment LEX (2)		/ line_stmts \
	/ comment : <any-token> ;		/ LEX (2) line_save_comment LEX (2)		/ line_stmts \
	/ comment : <any-token> 		/ ERROR (3) NEXT_STMT			/ line_stmts \

	/ channel : <name_comm_line> ;	/ LEX (2) line_save_channel_id LEX (2)		/ line_stmts \
	/ channel : <any-token>		/ ERROR (3) NEXT_STMT			/ line_stmts \

	/ att_desc : <quoted-string> ;	/ LEX (2) line_save_att_desc LEX (2)		/ line_stmts \
	/ att_desc : <any-token> ;		/ LEX (2) line_save_att_desc LEX (2)		/ line_stmts \
	/ att_desc : <any-token> 		/ ERROR (3) NEXT_STMT			/ line_stmts \

	/ device : <name_24> ;		/ LEX (2) line_save_device LEX (2)		/ line_stmts \
	/ device : <any-token>		/ ERROR (3) NEXT_STMT			/ line_stmts \

	/ <any-token>			/ line_validate_entry			/ BEGIN \

\" 

\" section to decode Device statements
device_stmts
	/ comment : <quoted-string> ;		/ LEX (2) dev_save_comment LEX (2)		/ device_stmts \
	/ comment : <any-token> ;		/ LEX (2) dev_save_comment LEX (2)		/ device_stmts \
	/ comment : <any-token> 		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ prph : <name_32> ;		/ LEX (2)
					  dev_save_attach_method (1, ATTACH_TYPE_IOM)
					  LEX (2)					/ device_stmts \
	/ prph : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ line : variable ;			/ LEX (2)
					  dev_save_attach_method (1, ATTACH_TYPE_VARIABLE_LINE)
					  LEX (2)					/ device_stmts \
	/ line : <name_comm_line> ;		/ LEX (2)
					  dev_save_attach_method (1, ATTACH_TYPE_TTY)
					  LEX (2)					/ device_stmts \
	/ line : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ dial_id : <name_32> ;		/ LEX (2)
					  dev_save_attach_method (1, ATTACH_TYPE_DIAL)
					  LEX (2)					/ device_stmts \
	/ dial_id : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ ctl_line : <name_32> ;		/ LEX (2)
					  dev_save_attach_method (2, CTL_ATTACH_TYPE_TTY)
					  LEX (2)					/ device_stmts \
	/ ctl_line : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ ctl_dial_id : <name_32> ;		/ LEX (2)
					  dev_save_attach_method (2, CTL_ATTACH_TYPE_DIAL)
					  LEX (2)					/ device_stmts \
	/ ctl_dial_id : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ ctl_source : <name_32> ;		/ LEX (2)
					  dev_save_attach_method (2, CTL_ATTACH_TYPE_MC)
					  LEX (2)					/ device_stmts \
	/ ctl_source : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ driver_module : <name_path> ;	/ LEX (2) dev_save_driver_module LEX (2)	/ device_stmts \
	/ driver_module : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ head_sheet : <name_path> ;		/ LEX (2) dev_save_head_sheet LEX (2)		/ device_stmts \
	/ head_sheet : <any-token> 		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ tail_sheet : <name_path> ;		/ LEX (2) dev_save_tail_sheet LEX (2)		/ device_stmts \
	/ tail_sheet : <any-token> 		/ ERROR (3) NEXT_STMT			/ device_stmts \
 
	/ paper_type : single ;		/ LEX (2) dev_save_paper_type LEX (2)		/ device_stmts \
	/ paper_type : continuous ;		/ LEX (2) dev_save_paper_type LEX (2)		/ device_stmts \
	/ paper_type : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ forms_validation : <name_path> ;	/ LEX (2) dev_save_forms_validation LEX (2)	/ device_stmts \
	/ forms_validation : <any-token> 	/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ default_form :			/ LEX (2) 				/ dev_process_default_form_string \

	/ font_dir : <name_path> ;		/ LEX (2) dev_save_font_dir LEX (2)		/ device_stmts \
	/ font_dir : <any-token>	 	/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ forms_info : <name_path> ;		/ LEX (2) dev_save_forms_table LEX (2)		/ device_stmts \
	/ forms_info : <any-token>	 	/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ args : <quoted-string> ;		/ LEX (2) dev_save_args LEX (2)		/ device_stmts \
	/ args : <any-token> ;		/ LEX (2) dev_save_args LEX (2)		/ device_stmts \
	/ args : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ minor_device : <name_24> ;		/ LEX (2)
					  dev_create_minor_table_entry NEXT_STMT	/ device_stmts \
	/ minor_device : <any-token>		/ ERROR (3) LEX (2)
					  dev_create_minor_table_entry NEXT_STMT	/ device_stmts \

	/ minor_args : <quoted-string> ;	/ LEX (2) dev_save_minor_args LEX (2)		/ device_stmts \
	/ minor_args : <any-token> ;		/ LEX (2) dev_save_minor_args LEX (2)		/ device_stmts \
	/ minor_args : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ default_type : <name_32_multi> ;	/ LEX (2) dev_save_default_type LEX (2)		/ device_stmts \
	/ default_type : <any-token>		/ ERROR (3) NEXT_STMT			/ device_stmts \

	/ <any-token>			/ dev_validate_entry			/ BEGIN \

\" routine to handle default form string for device
dev_process_default_form_string
	/				/ PUSH (dev_process_default_form_string1)	/ build_temp_token_value \
dev_process_default_form_string1
	/				/ dev_save_default_form			/ device_stmts \

build_temp_token_value
	/				/ [temp_token_value = "";
					   temp_token_value_used = TRUE]		/ \
build_temp_token_value_loop
	/ ;				/ LEX					/ STACK_POP \
	/ <any-token>			/ [temp_token_value = temp_token_value || token_value]
					  LEX					/ build_temp_token_value_loop \
\" 

\" section to decode Request_type statements
rqt_stmts
	/ comment : <quoted-string> ;		/ LEX (2) rqt_save_comment LEX (2)		/ rqt_stmts \
	/ comment : <any-token> ;		/ LEX (2) rqt_save_comment LEX (2)		/ rqt_stmts \
	/ comment : <any-token> 		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ driver_userid : <name_user_id> ;	/ LEX (2) rqt_save_driver_id LEX (2)		/ rqt_stmts \
	/ driver_userid : <any-token>		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ accounting : <name_path> ;		/ LEX (2) rqt_save_accounting LEX (2)		/ rqt_stmts \
	/ accounting : <any-token> 		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ generic_type : <name_24> ;		/ LEX (2) rqt_save_generic_type LEX (2)		/ rqt_stmts \
	/ generic_type : <any-token>		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ rqti_seg : <name_32> ;		/ LEX (2) rqt_save_rqti_seg LEX (2)		/ rqt_stmts \
	/ rqti_seg : <any-token>		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ max_queues : <decimal-integer> ;	/ LEX (2) rqt_save_max_queues LEX (2)		/ rqt_stmts \
	/ max_queues : <any-token>		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ default_queue : <decimal-integer> ;	/ LEX (2) rqt_save_default_queue LEX (2)	/ rqt_stmts \
	/ default_queue : <any-token>		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ line_charge : <any-token>	 	/ LEX (2) rqt_save_charge (1) LEX		/ rqt_save_charges \

	/ page_charge : <any-token> 		/ LEX (2) rqt_save_charge (2) LEX		/ rqt_save_charges \

	/ card_charge : <any-token> 		/ LEX (2) rqt_save_charge (3) LEX		/ rqt_save_charges \

	/ forms_validation : <name_path> ;	/ LEX (2) rqt_save_forms_validation LEX (2)	/ rqt_stmts \
	/ forms_validation : <any-token> 	/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ default_form : 			/ LEX (2)					/ rqt_process_default_form_string \

	/ font_dir : <name_path> ;		/ LEX (2) rqt_save_font_dir LEX (2)		/ rqt_stmts \
	/ font_dir : <any-token>	 	/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ forms_info : <name_path> ;		/ LEX (2) rqt_save_forms_table LEX (2)		/ rqt_stmts \
	/ forms_info : <any-token>	 	/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ device_class : <name_24> ;		/ LEX (2)
					  rqt_create_device_class_entry NEXT_STMT	/ rqt_stmts \
	/ device_class : <any-token>		/ ERROR (3) LEX (2)
					  rqt_create_device_class_entry NEXT_STMT	/ rqt_stmts \
					     
	/ max_access_class : <access_class_ok> ;/ LEX (2) rqt_save_access (1) NEXT_STMT		/ rqt_stmts \
	/ max_access_class : <any-token>	/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ min_access_class : <access_class_ok> ;/ LEX (2) rqt_save_access (2) NEXT_STMT		/ rqt_stmts \
	/ min_access_class : <any-token>	/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ min_banner : <access_class_ok> ;	/ LEX (2) rqt_save_access (3) NEXT_STMT		/ rqt_stmts \
	/ min_banner : <any-token>		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ device : <name_32_multi> ;		/ LEX (2) rqt_save_device LEX (2)		/ rqt_stmts \
	/ device : <any-token>		/ ERROR (3) NEXT_STMT			/ rqt_stmts \

	/ <any-token>			/ rqt_validate_entry			/ BEGIN \

\" section to handle multiple values for line/page/card_charge statements
rqt_save_charges
	/ , 				/ LEX					/ rqt_save_charges \
	/ ; 				/ LEX					/ rqt_stmts \
	/ <any-token>			/ rqt_save_charge_continue LEX		/ rqt_save_charges\
	/ <no-token>			/					/ BEGIN \


\" routine to handle default form string for request type
rqt_process_default_form_string
	/				/ PUSH (rqt_process_default_form_string1)	/ build_temp_token_value \
rqt_process_default_form_string1
	/				/ rqt_save_default_form			/ rqt_stmts \

\" 

\" section to decode Forms_table statements
forms_stmts
	/ name : <name_32>			/ LEX (2) forms_create_element_entry LEX	/ save_forms_name_synonyms \


	/ comment : <quoted-string> ;		/ LEX (2) forms_save_comment LEX (2)		/ forms_stmts \
	/ comment : <any-token> ;		/ LEX (2) forms_save_comment LEX (2)		/ forms_stmts \
	/ comment : <any-token> 		/ ERROR (3) NEXT_STMT			/ forms_stmts \

	/ type : <forms_type> ;		/ LEX (2) forms_save_type LEX (2)		/ forms_stmts \
	/ type : <any-token>		/ ERROR (3) NEXT_STMT			/ forms_stmts \

	/ uses : <name_32>			/ LEX (2) forms_save_uses_name LEX		/ save_forms_uses_names \


	/ string : <quoted-string> ;		/ LEX (2) forms_save_string_token LEX (2)	/ forms_stmts \
	/ string : ;			/ LEX (3) 				/ forms_stmts \
	/ string :			/ LEX (2) 
					  [forms_escape_string_index = 1;
					   forms_escape_string_n (*) = 0]
					  / forms_char_string \

	/ page_height : <forms_size_ok> ;	/ LEX (2) forms_save_size (1) LEX (2)		/ forms_stmts \
	/ page_height : <any-token>		/ ERROR (3) NEXT_STMT			/ forms_stmts \

	/ page_width : <forms_size_ok> ;	/ LEX (2) forms_save_size (2) LEX (2)		/ forms_stmts \
	/ page_width : <any-token>		/ ERROR (3) NEXT_STMT			/ forms_stmts \

	/ char_height : <forms_size_ok> ;	/ LEX (2) forms_save_size (3) LEX (2)		/ forms_stmts \
	/ char_height : <any-token>		/ ERROR (3) NEXT_STMT			/ forms_stmts \

	/ char_width : <forms_size_ok> ;	/ LEX (2) forms_save_size (4) LEX (2)		/ forms_stmts \
	/ char_width : <any-token>		/ ERROR (3) NEXT_STMT			/ forms_stmts \

	/ line_height : <forms_size_ok> ;	/ LEX (2) forms_save_size (5) LEX (2)		/ forms_stmts \
	/ line_height : <any-token>		/ ERROR (3) NEXT_STMT			/ forms_stmts \

	/ <any-token>			/ forms_validate_group			/ BEGIN \

\" section to handle multiple values for uses statement
save_forms_name_synonyms
	/ , 				/ LEX					/ save_forms_name_synonyms \
	/ ; 				/ LEX					/ forms_stmts \
	/ <name_32>			/ forms_add_syn_element_name LEX		/ save_forms_name_synonyms \
	/ <any-token>			/ ERROR (3) forms_add_syn_element_name LEX	/ save_forms_name_synonyms \
	/ <no-token>			/					/ BEGIN \


save_forms_uses_names
	/ , 				/ LEX					/ save_forms_uses_names \
	/ ; 				/ LEX					/ forms_stmts \
	/ <name_32>			/ forms_save_uses_name_continue LEX		/ save_forms_uses_names \
	/ <any-token>			/ ERROR (3) forms_save_uses_name_continue LEX	/ save_forms_uses_names \
	/ <no-token>			/					/ BEGIN \


\" section to handle string statements
forms_char_string
forms_string_item
	/ <quoted-string>			/ append_token_value_string LEX		/ forms_string_item \
	/ <forms_tty_char_ok>		/ insert_single_char ((token.Nvalue)) LEX	/ forms_string_item \

	/ ;				/ forms_save_string LEX			/ forms_stmts \
	/ <any-token>			/ ERROR (3) NEXT_STMT			/ forms_stmts \
	/ <no-token>			/ 					/ RETURN \

   ++*/
%page;

/* format: style4 */
iod_tables_compiler: iodtc: proc;

/* The iod_tables_compiler produces an encoded representation of the various
   IO daemon tables from a source language description.  Source segments are
   assumed to have a name ending with the suffix ".iodt".  An object segment
   will be given the same name as its corresponding source segment with the
   suffix removed.
*/

/* External Entries & Procedures */

dcl  cdt_mgr_$find_cdt_channel entry (ptr, char (32), fixed bin, bit (1) aligned, fixed bin (35));
dcl  clock_ entry () returns (fixed bin (71));
dcl  com_err_ entry () options (variable);
dcl  com_err_$suppress_name entry () options (variable);
dcl  convert_authorization_$from_string entry (bit (72) aligned, char (*), fixed bin (35));
dcl  cu_$arg_count entry (fixed bin, fixed bin (35));
dcl  cu_$arg_ptr entry (fixed bin, ptr, fixed bin (21), fixed bin (35));
dcl  cv_dec_check_ entry (char (*), fixed bin (35)) returns (fixed bin (35));
dcl  cv_float_ entry (char (*), fixed bin (35)) returns (float bin (27));
dcl  cv_oct_check_ entry (char (*), fixed bin (35)) returns (fixed bin (35));
dcl  error_table_$bad_conversion fixed bin (35) ext static;
dcl  error_table_$bad_opt fixed bin (35) ext static;
dcl  error_table_$noarg fixed bin (35) ext static;
dcl  error_table_$too_many_args fixed bin (35) ext static;
dcl  error_table_$zero_length_seg fixed bin (35) ext static;
dcl  expand_pathname_$add_suffix entry (char (*), char (*), char (*), char (*), fixed bin (35));
dcl  get_temp_segments_ entry (char (*), (*) ptr, fixed bin (35));
dcl  get_wdir_ entry () returns (char (168));
dcl  initiate_file_ entry (char (*), char (*), bit (*), ptr, fixed bin (24), fixed bin (35));
dcl  ioa_ entry () options (variable);
dcl  lex_error_ entry () options (variable);
dcl  lex_string_$init_lex_delims entry (char (*), char (*), char (*), char (*), char (*), bit (*), char (*) var,
	char (*) var, char (*) var, char (*) var);
dcl  lex_string_$lex entry (ptr, fixed bin (21), fixed bin (21), ptr, bit (*), char (*), char (*), char (*), char (*),
	char (*), char (*) var, char (*) var, char (*) var, char (*) var, ptr, ptr, fixed bin (35));
dcl  release_temp_segments_ entry (char (*), (*) ptr, fixed bin (35));
dcl  suffixed_name_$new_suffix entry (char (*), char (*), char (*), char (32), fixed bin (35));
dcl  system_info_$resource_price entry (char (*), float bin (27), fixed bin (35));
dcl  terminate_file_ entry (ptr, fixed bin (24), bit (*), fixed bin (35));
dcl  translator_temp_$get_segment entry (char (*) aligned, ptr, fixed bin (35));
dcl  translator_temp_$release_all_segments entry (ptr, fixed bin (35));
dcl  tssi_$clean_up_segment entry (ptr);
dcl  tssi_$finish_segment entry (ptr, fixed bin (24), bit (36) aligned, ptr, fixed bin (35));
dcl  tssi_$get_segment entry (char (*), char (*), ptr, ptr, fixed bin (35));

/* Builtins */

dcl  (addr, addrel, after, before, bin, char, collate, convert, currentsize,
     dimension, divide, fixed, hbound, index, lbound, length, max, mod, null,
     rank, rel, rtrim, substr, translate, unspec, verify) builtin;

dcl  cleanup condition;

/* External Static */

dcl  iod_tables_compiler_severity_ fixed bin (35) ext init (0);
dcl  sc_stat_$sysdir char (168) aligned external;

/* Internal Static */

dcl  ALL_VALID_CHARS_AND_NUMERIC char (62) int static options (constant)
	init ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");

dcl  BREAKS char (35) varying int static;
dcl  CTL_CHARS char (32) varying int static;
dcl  FALSE bit (1) int static options (constant) init ("0"b);
dcl  LEX_CHARS char (6) int static options (constant) init ("""/**/;");
dcl  LEX_COMMENT_CLOSE char (2) defined LEX_CHARS pos (4);
dcl  LEX_COMMENT_OPEN char (2) defined LEX_CHARS pos (2);
dcl  LEX_STATEMENT_DELIMITER char (1) defined LEX_CHARS pos (6);
dcl  LEX_QUOTE_CLOSE char (1) defined LEX_CHARS pos (1);
dcl  LEX_QUOTE_OPEN char (1) defined LEX_CHARS pos (1);
dcl  LEXCTL char (80) varying int static;
dcl  LEXDLM char (80) varying int static;
dcl  TOKEN_BAD bit (1) int static options (constant) init ("0"b);
dcl  TOKEN_GOOD bit (1) int static options (constant) init ("1"b);
dcl  TRUE bit (1) int static options (constant) init ("1"b);
dcl  already_called bit (1) int static init (FALSE);
dcl  max_length_att_desc fixed bin int static options (constant) init (512);
dcl  max_length_att_desc_char char (3) int static options (constant) init ("512");
dcl  max_length_comment fixed bin int static options (constant) init (256);
dcl  max_length_comment_char char (3) int static options (constant) init ("256");
dcl  max_size fixed bin int static options (constant) init (360);
dcl  max_size_char char (3) int static options (constant) init ("360");
dcl  prog_name char (32) int static options (constant) init ("new_iod_tables_compiler");

/* Internal Automatic */

dcl  acl_info_ptr ptr;
dcl  arg char (argl) based (argp);			/* command argument */
dcl  argc fixed bin;				/* input argument count */
dcl  argl fixed bin (21);				/* length of arg */
dcl  argp ptr;					/* ptr to arg */
dcl  argx fixed bin;				/* current input arg being processed */
dcl  authorization_bits bit (72) aligned;		/* converted authorization bits */
dcl  bit_count fixed bin (24);			/* bit count */
dcl  cdtp ptr;					/* Channel Definition Table ptr */
dcl  code fixed bin (35);				/* error code */
dcl  copy_ptr ptr;
dcl  debug bit (1);
dcl  default_device_class_defined bit (1);
dcl  default_minor_device_defined bit (1);
dcl  default_print_defined bit (1);
dcl  default_punch_defined bit (1);
dcl  error_string char (128) varying;
dcl  fb35 fixed bin (35);
dcl  forms_size_info float bin;
dcl  forms_size_type fixed bin;
dcl  forms_element_validated bit (1);
dcl  (i, j, k, l) fixed bin;
dcl  input_dir_name char (168);			/* input directory pathname */
dcl  input_ent_name char (32);			/* input entry name */
dcl  input_ptr ptr;					/* ptr to input segment */
dcl  item_ok bit (1);
dcl  line_charge_keyword_used bit (1);
dcl  minor_name char (32);
dcl  output_dir_name char (168);			/* output directory pathname */
dcl  output_ent_name char (32);			/* output entry name */
dcl  output_ptr ptr;				/* ptr to output segment */
dcl  page_charge_keyword_used bit (1);
dcl  saved_charge_cntr fixed bin;
dcl  saved_charge_type fixed bin;
dcl  stmtp ptr;					/* RDC statement ptr */
dcl  temp_ptrs (10) ptr;				/* scratch segment ptrs */
dcl  text_strings_array (text_strings.length) char (1) unaligned based (text_strings_array_ptr);
dcl  text_strings_array_ptr ptr;
dcl  translator_temp_ptr ptr;

/* temp storage for forms elements & groups used while building the forms info tab */
dcl  forms_elements_ptr ptr;
dcl  forms_elements (iod_forms_info_tab.n_words) fixed bin (35) based (forms_elements_ptr);
dcl  forms_groups_ptr ptr;
dcl  1 forms_groups (iod_forms_info_tab.n_groups) like iod_forms_info_tab.groups based (forms_groups_ptr);

dcl  1 cn aligned,					/* component names of a two-part name  */
       2 first_name char (24),
       2 second_name char (24);

/* the following are for device names and other data which must be saved as we go along */
dcl  misc_data_ptr ptr;
dcl  1 misc_data based (misc_data_ptr),

/* the following are for device names encountered while processing Line
   statements */
       2 line_device_names_count fixed bin,
       2 line_device_names (max_size) char (32),
       2 line_device_index (max_size) fixed bin,

/* the following are for device names (major and/or minor) encountered while
   processing Request_type statements */
       2 q_group_device_names_count fixed bin,
       2 q_group (max_size),
         3 device_name like cn,
         3 is_default_minor bit (1),			/* ON if this is default minor device  */
         3 is_default_device_class bit (1),		/* ON if this is default device class */
         3 device_index fixed bin,			/* device index for this device */
         3 default_type like cn,

/* the following is used while processing Device statements where bit I is ON
   if Device I uses "line: variable;" statement */
       2 var_line_list bit (max_size),

       2 forms_escape_string_index fixed bin,
       2 forms_escape_string_n (1024) fixed bin (8) unaligned,

       2 parms_count fixed bin,
       2 parms_indices_start (64) fixed bin,
       2 parms_indices_length (64) fixed bin,
       2 parms (11) char (32),
       2 temp_token_value_used bit (1),
       2 temp_token_value char (1024) varying,

/* the following two define those request type/device classes and
   major/minor devices which have a forms_table string defined for them */
       2 device_class_forms bit (max_size),
       2 minor_device_forms bit (max_size),

       2 pad fixed bin;
%page;

/* Actual Program Begins Here */

	input_ptr, output_ptr, temp_ptrs (*), translator_temp_ptr, cdtp,
	     acl_info_ptr, copy_ptr = null;

	on cleanup begin;
	     call clean_up;
	     iod_tables_compiler_severity_ = 5;
	end;

	call cu_$arg_count (argc, code);
	if code ^= 0 then do;
	     call com_err_ (code, prog_name);
	     go to severity_5_failure;
	end;

	debug = FALSE;
	SERROR_CONTROL = "00"b;
	TRACING = FALSE;

	if argc < 1 then do;
	     call com_err_$suppress_name (0, prog_name,
		"Usage:  iod_tables_compiler iod_tables_name [-brief | -long]");
	     go to severity_5_failure;
	end;

	input_ent_name = "";			/* no input name yet */

	do argx = 1 to argc;
	     call cu_$arg_ptr (argx, argp, argl, code);
	     if char (arg, 1) ^= "-" then do;
		if input_ent_name ^= "" then do;
		     call com_err_ (error_table_$too_many_args, prog_name,
			"Only one pathname may be given. ^a was the second.", arg);
		     go to severity_5_failure;
		end;

		call expand_pathname_$add_suffix (arg, "iodt", input_dir_name, input_ent_name, code);
		if code ^= 0 then do;
path_error:
		     call com_err_ (code, prog_name, "^a", arg);
		     go to severity_5_failure;
		end;

/* if we get this far, how can we fail? */
		call suffixed_name_$new_suffix (input_ent_name, "iodt", "", output_ent_name, code);
		if code ^= 0 then			/* still, let's have a look */
		     go to path_error;

	     end;					/* Pathname case */
	     else if arg = "-bf" | arg = "-brief" then
		SERROR_CONTROL = "01"b;

	     else if arg = "-lg" | arg = "-long" then
		SERROR_CONTROL = "10"b;

	     else if arg = "-severity" | arg = "-sv" then do;
		if argx >= argc then do;
		     call com_err_ (error_table_$noarg, prog_name, "After ""^a"".", arg);
		     go to severity_5_failure;
		end;
		argx = argx + 1;
		call cu_$arg_ptr (argx, argp, argl, code);
		fb35 = cv_dec_check_ (arg, code);
		if code ^= 0 | fb35 < 0 | fb35 > 5 then do;
		     call com_err_ (error_table_$bad_conversion, prog_name,
			"Severity must be an integer in the range 0 - 5, not ""^a"".", arg);
		     go to severity_5_failure;
		end;
		MIN_PRINT_SEVERITY = fb35;
	     end;

	     else if arg = "-trace_on" | arg = "-tron" then
		TRACING = TRUE;
	     else if arg = "-trace_off" | arg = "-trof" then
		TRACING = FALSE;
	     else if arg = "-debug" | arg = "-db" then
		debug = TRUE;
	     else do;
		code = error_table_$bad_opt;
		call com_err_ (code, prog_name, arg);
		go to severity_5_failure;
	     end;
	end;					/* argument processing loop */

	if input_ent_name = "" then
	     go to path_error;


	call initiate_file_ (input_dir_name, input_ent_name, R_ACCESS, input_ptr, bit_count, code); /* get ptr to input seg */
	if input_ptr = null then do;
	     call com_err_ (code, prog_name, "^a>^a", input_dir_name, input_ent_name);
	     return;
	end;
	if bit_count = 0 then do;
	     call com_err_ (error_table_$zero_length_seg, prog_name, "^a>^a", input_dir_name, input_ent_name);
	     go to finish;
	end;

/* get ptr to channel definition table */
	call initiate_file_ ((sc_stat_$sysdir), "cdt", R_ACCESS, cdtp, (0), code);
	if code ^= 0 then do;
	     call com_err_ (code, prog_name,
		"Accessing cdt.  Channel name checks will not be performed.");
	end;

/* get scratch segments */
	call get_temp_segments_ (prog_name, temp_ptrs, code); /* make temp output seg */
	if code ^= 0 then do;
	     call com_err_ (code, prog_name, "Getting temp segments.");
	     go to finish;
	end;

/* Get pointers to the various tables.  For now let the table sizes be maximum. */

	ithp = temp_ptrs (1);			/* set ptr to header */
	ltp = temp_ptrs (2);			/* get ptr to line id table */
	idtp = temp_ptrs (3);			/* get ptr to iod device table */
	mdtp = temp_ptrs (4);			/* get ptr to minor device table */
	dctp = temp_ptrs (5);			/* get ptr to device class table */
	qgtp = temp_ptrs (6);			/* get ptr to queue group table */
	ifitp = temp_ptrs (7);			/* get ptr to forms info tables */
	forms_groups_ptr = temp_ptrs (8);		/* get ptr to temp storage for forms elements */
	iod_forms_info_tab.n_groups=101;
	forms_elements_ptr = addr (forms_groups.name (101)); /* leave room for 100 Forms_info groups */
	iod_forms_info_tab.n_groups=0;
	text_strings_ptr = temp_ptrs (9);		/* get ptr to text strings area */
	text_strings_array_ptr = addr (text_strings.chars);
	misc_data_ptr = temp_ptrs (10);

/* initialize to common state */

	iod_tables_hdr.max_queues = -1;
	iod_tables_hdr.grace_time = -1;

	line_device_names_count, q_group_device_names_count = 0;
	idtep, mdtep, qgtep, dctep, ltep, fep = null ();
	forms_element_validated,
	     q_group.is_default_minor (*),
	     q_group.is_default_device_class (*),
	     default_minor_device_defined,
	     default_print_defined,
	     default_punch_defined,
	     temp_token_value_used = FALSE;
	device_class_forms, minor_device_forms = "0"b;
	error_string = "";

/* set up for lex_string_ */

	call translator_temp_$get_segment ((prog_name), translator_temp_ptr, code);
	if translator_temp_ptr = null () then do;
	     call com_err_ (code, prog_name, "From translator_temp_$get_segment");
	     go to severity_5_failure;
	end;

	if ^already_called then do;
	     CTL_CHARS = substr (collate (), 1, 8) || substr (collate (), 10, 24);
	     BREAKS = CTL_CHARS || ",:;";
	     call lex_string_$init_lex_delims (LEX_QUOTE_OPEN, LEX_QUOTE_CLOSE,
		LEX_COMMENT_OPEN, LEX_COMMENT_CLOSE, LEX_STATEMENT_DELIMITER,
		"10"b, BREAKS, CTL_CHARS, LEXDLM, LEXCTL);
	     already_called = TRUE;
	end;

	if TRACING | debug then
	     call ioa_ ("calling lex_string_$lex");

	call lex_string_$lex (input_ptr, divide (bit_count, 9, 21, 0), 0,
	     translator_temp_ptr, "1000"b, LEX_QUOTE_OPEN, LEX_QUOTE_CLOSE,
	     LEX_COMMENT_OPEN, LEX_COMMENT_CLOSE, LEX_STATEMENT_DELIMITER,
	     BREAKS, CTL_CHARS, LEXDLM, LEXCTL, stmtp, Pthis_token, code);

	if code ^= 0 then do;
	     call com_err_ (code, prog_name, "Lexing ^a>^a.", input_dir_name, input_ent_name);
	     go to finish;
	end;

/* Go to it! */
	Pstmt = stmtp;
	temp_token_value = stmt_value;

	if TRACING | debug then
	     call ioa_ ("calling SEMANTIC_ANALYSYS");

	call SEMANTIC_ANALYSIS ();

	if TRACING | debug then
	     call ioa_ ("SEMANTIC_ANALYSYS done");

	if MERROR_SEVERITY <= 2 then do;
	     if debug then
		call ioa_ ("Cross checking table.");
	     call cross_check_table;
	end;
	if MERROR_SEVERITY <= 2 then do;
	     if debug then
		call ioa_ ("Building forms table.");
	     if debug then
		call ioa_ ("Building output table.");
	     call build_output_table;
	end;

	if MERROR_SEVERITY > 2 then
	     call com_err_ (0, prog_name, "Translation failed.");
	iod_tables_compiler_severity_ = MERROR_SEVERITY;

	go to finish;

severity_5_failure:
	iod_tables_compiler_severity_ = 5;
finish:	call clean_up;
	return;
%page;

clean_up: proc;

/* cleanup handler -- makes sure anything we initiated gets terminated, anything we allocated gets freed */

	if input_ptr ^= null then
	     call terminate_file_ (input_ptr, (0), TERM_FILE_TERM, (0));
	input_ptr = null;

	if acl_info_ptr ^= null then
	     call tssi_$clean_up_segment (acl_info_ptr);
	acl_info_ptr = null;

	if output_ptr ^= null then
	     call terminate_file_ (output_ptr, (0), TERM_FILE_TRUNC_BC_TERM, (0));
	output_ptr = null;

	if translator_temp_ptr ^= null then
	     call translator_temp_$release_all_segments (translator_temp_ptr, (0));
	translator_temp_ptr = null;

	if temp_ptrs (1) ^= null then
	     call release_temp_segments_ (prog_name, temp_ptrs, code);
	temp_ptrs (*) = null;

	if cdtp ^= null then
	     call terminate_file_ (cdtp, (0), TERM_FILE_TERM, (0));
	cdtp = null;

	return;

     end clean_up;
%page;

/* Since this is a single pass compiler, there is no guarantee that all names
   of entries will be known when they are encountered in the input file.
   Thus we will cross check all of the variables at the end of the input file. */

cross_check_table: proc;

dcl  (full_name, full_name2) char (32);			/* for error display */
dcl  (minor_kw, minor_kw2) char (32);			/* for error display */
dcl  major_name char (32);
dcl  price_name char (32);
dcl  price float bin;
dcl  temp_list bit (360) aligned;			/* a temporary copy of dcte.device_list */

dcl  starting_element_index fixed bin;
dcl  error_statement_no fixed bin;
dcl  error_device_name char (32);
dcl  error_entry_type char (32);
dcl  error_entry_type_id char (32);

/* validate that each 'device' referenced under the Line keywords is
   a) defined as a Device
   b) has a attach type of 'variable'
*/

	line_device_index (*) = 0;			/* mark all as invalid til found */

	do i = 1 to line_device_names_count;		/* all names found */
	     do j = 1 to iod_device_tab.n_devices;	/* all available devices */

/* If name matches and the device was defined with a variable line, save index
   into device table for the entry.

   If the name matches but the device was not defined with a variable line,
   save the index but negate it so we can tell later */

		if iod_device_tab.entries (j).dev_id = line_device_names (i) then do;
		     if iod_device_tab.entries (j).attach_type = ATTACH_TYPE_VARIABLE_LINE then
			line_device_index (i) = j;
		     else line_device_index (i) = -j;
		     go to next_line_dev;
		end;
	     end;
next_line_dev:
	end;

	do i = 1 to line_tab.n_lines;
	     ltep = addr (line_tab.entries (i));
	     temp_list = lte.maj_dev_list;
	     lte.maj_dev_list = ""b;			/* clear so we can put in correct order */
	     j = index (temp_list, TRUE);
	     do while (j ^= 0);
		if line_device_index (j) = 0 then	/* not defined above */
		     call semant_error (12, "device", line_device_names (j), "line", lte.line_id);
		else do;
		     if line_device_index (j) < 0 then
			call semant_error (25, idte.dev_id, lte.line_id);
		     else do;
			substr (lte.maj_dev_list, line_device_index (j), 1) = TRUE; /* a good one */
			substr (var_line_list, line_device_index (j), 1) = FALSE; /* this one was used */
		     end;
		end;
		substr (temp_list, j, 1) = FALSE;
		j = index (temp_list, TRUE);
	     end;
	end;

/* if any unused variable line keywords found, gripe */
	if var_line_list then do;
	     i = index (var_line_list, TRUE);
	     do while (i ^= 0);
		call semant_error (26, iod_device_tab.dev_id (i));
		substr (var_line_list, j, 1) = FALSE;
		i = index (var_line_list, TRUE);
	     end;
	end;

/* Validate the devices specified in the Request_type and device_class entries. */
/* Find the minor device index for each name in the device array. */

	q_group.device_index (*) = 0;			/* mark all as not found until we find them */
	do i = 1 to q_group_device_names_count;		/* loop thru devices */
	     cn = q_group.device_name (i);
	     do j = 1 to iod_device_tab.n_devices;	/* look for major device */
		idtep = addr (iod_device_tab.entries (j));
		if idte.dev_id = cn.first_name then do; /* found major device */
		     if cn.second_name = "" then do;	/* must have a default minor */
			if q_group.is_default_minor (idte.first_minor) then
			     q_group.device_index (i) = idte.first_minor;
			else q_group.device_index (i) = -1; /* error, no default minor */
			go to next_device;
		     end;
		     else do k = idte.first_minor to idte.last_minor;
			mdtep = addr (minor_device_tab.entries (k));
			if mdte.dev_id = cn.second_name then do; /* found minor device */
			     q_group.device_index (i) = k;
			     go to next_device;
			end;
		     end;
		end;
	     end;
next_device:
	end;

/* Transform the device_list of each device class so that each bit
   in the device list corresponds to an entry in the minor device
   table rather than an entry in the device array. */

	do i = 1 to dev_class_tab.n_classes;		/* loop thru device classes */
	     dctep = addr (dev_class_tab.entries (i));
	     if dcte.device_list = ""b then
		go to skip_this_entry;
	     temp_list = dcte.device_list;
	     dcte.device_list = ""b;
	     j = index (temp_list, TRUE);
	     do while (j ^= 0);
		if q_group.device_index (j) <= 0 then do; /* error detected above */
		     if q_group.device_index (j) = -1 then
			error_statement_no = 33;	/* set error number */
		     else error_statement_no = 34;
		     error_device_name = q_group.device_name (j).first_name;
		     if q_group.device_name (j).second_name ^= "" then
			error_device_name = rtrim (error_device_name) || "." || q_group.device_name (j).second_name;
		     error_entry_type_id = before (q_group_tab.name (dcte.qgte_index), " ");
		     if q_group.is_default_device_class (i) then
			error_entry_type = "Request_type";
		     else do;
			error_entry_type = "device_class";
			error_entry_type_id = rtrim (error_entry_type_id) || "." || dcte.id;
		     end;
		     call semant_error (error_statement_no, error_device_name, error_entry_type, error_entry_type_id);
		end;
		else substr (dcte.device_list, q_group.device_index (j), 1) = TRUE;
		substr (temp_list, j, 1) = FALSE;
		j = index (temp_list, TRUE);
	     end;
skip_this_entry:
	end;

/* Validate the default types optionally specified for each minor device. */

	do i = 1 to minor_device_tab.n_minor;		/* loop thru minor devices */
	     if q_group.default_type (i).first_name ^= "" then do; /* default class specified */
		cn = q_group.default_type (i);
		mdtep = addr (minor_device_tab.entries (i));
		do j = 1 to q_group_tab.n_q_groups;	/* look for the default_type */
		     qgtep = addr (q_group_tab.entries (j));
		     if cn.first_name = qgte.name then do;
			if cn.second_name = "" then	/* no device class specified */
			     if q_group.is_default_device_class (qgte.first_dev_class) then /* ok, it's a default class */
				mdte.default_dev_class = qgte.first_dev_class;
			     else do;		/* no default class for this q group */
				error_statement_no = 35; /* set up error number */
				go to bad_def_type;
			     end;
			else do;			/* look for second name of default_type */
			     do k = qgte.first_dev_class to qgte.last_dev_class
				while (dev_class_tab.id (k) ^= cn.second_name);
			     end;
			     if k > qgte.last_dev_class then
				go to def_type_not_found;
			     mdte.default_dev_class = k;
			end;
			dctep = addr (dev_class_tab.entries (mdte.default_dev_class));
			if ^substr (dcte.device_list, i, 1) then do; /* device not specified for default type */
			     error_statement_no = 36;
			     go to bad_def_type;
			end;
			go to next_minor;
		     end;
		end;

def_type_not_found: error_statement_no = 37;
bad_def_type:
		full_name2 = cn.first_name;
		if cn.second_name ^= "" then do;
		     full_name2 = rtrim (full_name2) || "." || cn.second_name;
		     minor_kw2 = "device_class";
		end;
		else minor_kw2 = "Request_type";
		full_name = before (iod_device_tab.dev_id (mdte.major_index), " ");
		if q_group.is_default_minor (i) then
		     minor_kw = "Device";
		else do;
		     full_name = rtrim (full_name) || "." || mdte.dev_id;
		     minor_kw = "minor_device";
		end;
		if error_statement_no ^= 36 then
		     call semant_error (error_statement_no, full_name2, minor_kw, full_name);
		else call semant_error (error_statement_no, full_name2, minor_kw, full_name, full_name, minor_kw2, full_name2);
	     end;
next_minor: end;

/* make sure the optional request type parameters are defined correctly */

	do i = 1 to q_group_tab.n_q_groups;
	     qgtep = addr (q_group_tab.entries (i));

	     if qgte.max_queues = -1 then		/* if not defined, use the Global value */
		qgte.max_queues = iod_tables_hdr.max_queues;

	     if qgte.default_queue = -1 then do;	/* if not defined, use highest up to 3 */
		if qgte.max_queues < 3 then
		     qgte.default_queue = qgte.max_queues;
		else qgte.default_queue = 3;		/* any queue 4 is low priority */
	     end;
	     else if qgte.default_queue > qgte.max_queues then
		call semant_error (38, qgte.name);

	     if qgte.page_charge.queue (1) ^= "" then do; /* was the page_charge keyword given? */
		do j = 1 to hbound (qgte.page_charge.queue, 1); /* look at each price name */
		     price_name = qgte.page_charge.queue (j);
		     if price_name = "UNDEFINED_PRICE" then do; /* it was blank */
			if j > qgte.max_queues then
			     qgte.page_charge.queue (j) = ""; /* not used */
			else do;			/* otherwise we need a real price name */
			     call semant_error (39, qgte.name);
			     go to skip_charge1;
			end;
		     end;
		     else do;
			call system_info_$resource_price (price_name, price, code);
			if code ^= 0 then
			     call semant_error (40, qgte.name, price_name);
		     end;
		end;
skip_charge1:  end;


	     if qgte.line_charge.queue (1) ^= "" then do; /* was the line_charge keyword given? */
		do j = 1 to hbound (qgte.line_charge.queue, 1); /* look at each price name */
		     price_name = qgte.line_charge.queue (j);
		     if price_name = "UNDEFINED_PRICE" then do; /* it was blank */
			if j > qgte.max_queues then
			     qgte.line_charge.queue (j) = ""; /* not used */
			else do;			/* otherwise we need a real price name */
			     call semant_error (39, qgte.name);
			     go to skip_charge;
			end;
		     end;
		     else do;
			call system_info_$resource_price (price_name, price, code);
			if code ^= 0 then
			     call semant_error (40, qgte.name, price_name);
		     end;
		end;
skip_charge:   end;

/* now let's make sure any forms information references are available.
   make sure forms table is defined if they are using forms */
	     if qgte.forms_table.total_chars ^= 0 then do;
		major_name = return_string (qgte.forms_table);
		do j = 1 to iod_forms_info_tab.n_groups
		     while (major_name ^= forms_groups.name (j));
		end;
		if j > iod_forms_info_tab.n_groups then
		     call semant_error (12, "Forms_table", major_name, "Request_type", qgte.name);
		else do;
		     starting_element_index = forms_groups (j).first_element_index;
		     if qgte.default_form.total_chars ^= 0 then do;
			call parse_parms_string (addr (qgte.default_form));
			do k = 1 to parms_count;
			     l = forms_scan_for_element_name (starting_element_index, parms (k));
			     if l = -1 then
				call semant_error (12, "default forms element", parms (k), "Request_type", qgte.name);
			end;
		     end;
		end;

/* make sure any minor devices referred to by this request type also have
   forms tables defined for them */
		do k = qgte.first_dev_class to qgte.last_dev_class;
		     dctep = addr (dev_class_tab.entries (k));
		     do l = 1 to max_size;
			if substr (dcte.device_list, l, 1) then
			     if ^substr (minor_device_forms, l, 1) then do;
				full_name = q_group_tab.entries.name (dcte.qgte_index);
				if q_group_tab.entries.name (dcte.qgte_index) ^= dcte.id then
				     full_name = rtrim (full_name) || "." || dcte.id;
				mdtep = addr (minor_device_tab.entries (l));
				idtep = addr (iod_device_tab.entries (mdte.major_index));
				full_name2 = idte.dev_id;
				if full_name2 ^= mdte.dev_id then
				     full_name2 = rtrim (full_name2) || "." || mdte.dev_id;
				call semant_error (53, full_name, full_name2);
			     end;
		     end;
		end;
	     end;
	end;

/* make sure we've got one of everything we need */

	if iod_tables_hdr.max_queues = -1 then call semant_error (41);

	if iod_tables_hdr.grace_time = -1 then call semant_error (42);

	if iod_device_tab.n_devices = 0 then call semant_error (43);

	if q_group_tab.n_q_groups = 0 then call semant_error (44);

	if ^default_print_defined then call semant_error (45);
	if ^default_punch_defined then call semant_error (46);

     end cross_check_table;
%page;

build_output_table: proc;

/* create permanent output seg and copy in the temp seg */
	output_dir_name = get_wdir_ ();		/* put output seg in working directory */
	call tssi_$get_segment (output_dir_name, output_ent_name, output_ptr, acl_info_ptr, code);
	if code ^= 0 then do;
output_error:  call com_err_ (code, prog_name, "^a>^a", output_dir_name, output_ent_name);
	     go to finish;
	end;

/* copy the tables into the output seg */

	output_ptr -> iod_tables_hdr = iod_tables_hdr;
	ithp = output_ptr;
	iod_tables_hdr.version = "";			/* mark it inconsistent while we're copying */

	copy_ptr = adjust_ptr (addr (iod_tables_hdr.start_of_tables));
	copy_ptr -> line_tab = line_tab;
	ltp = copy_ptr;

	line_tab.n_lines = line_tab.n_lines + 1;
	copy_ptr = adjust_ptr (addr (line_tab.entries (line_tab.n_lines)));
	line_tab.n_lines = line_tab.n_lines - 1;
	copy_ptr -> iod_device_tab = iod_device_tab;
	idtp = copy_ptr;

	iod_device_tab.n_devices = iod_device_tab.n_devices + 1;
	copy_ptr = adjust_ptr (addr (iod_device_tab.entries (iod_device_tab.n_devices)));
	iod_device_tab.n_devices = iod_device_tab.n_devices - 1;
	copy_ptr -> minor_device_tab = minor_device_tab;
	mdtp = copy_ptr;

	minor_device_tab.n_minor = minor_device_tab.n_minor + 1;
	copy_ptr = adjust_ptr (addr (minor_device_tab.entries (minor_device_tab.n_minor)));
	minor_device_tab.n_minor = minor_device_tab.n_minor - 1;
	copy_ptr -> q_group_tab = q_group_tab;
	qgtp = copy_ptr;

	q_group_tab.n_q_groups = q_group_tab.n_q_groups + 1;
	copy_ptr = adjust_ptr (addr (q_group_tab.entries (q_group_tab.n_q_groups)));
	q_group_tab.n_q_groups = q_group_tab.n_q_groups - 1;
	copy_ptr -> dev_class_tab = dev_class_tab;
	dctp = copy_ptr;

	dev_class_tab.n_classes = dev_class_tab.n_classes + 1;
	copy_ptr = adjust_ptr (addr (dev_class_tab.entries (dev_class_tab.n_classes)));
	dev_class_tab.n_classes = dev_class_tab.n_classes - 1;
	iod_forms_info_tab.element_data_block = forms_elements;
	iod_forms_info_tab.groups = forms_groups;
	copy_ptr -> iod_forms_info_tab = iod_forms_info_tab;
	ifitp = copy_ptr;

	iod_forms_info_tab.n_groups = iod_forms_info_tab.n_groups + 1;
	copy_ptr = adjust_ptr (addr (iod_forms_info_tab.groups (iod_forms_info_tab.n_groups)));
	iod_forms_info_tab.n_groups = iod_forms_info_tab.n_groups - 1;
	copy_ptr -> text_strings = text_strings;
	text_strings_ptr = copy_ptr;

/* now fill in the header */

	iod_tables_hdr.line_tab_offset = fixed (rel (ltp), 18);
	iod_tables_hdr.device_tab_offset = fixed (rel (idtp), 18);
	iod_tables_hdr.minor_device_tab_offset = fixed (rel (mdtp), 18);
	iod_tables_hdr.q_group_tab_offset = fixed (rel (qgtp), 18);
	iod_tables_hdr.dev_class_tab_offset = fixed (rel (dctp), 18);
	iod_tables_hdr.forms_info_tab_offset = fixed (rel (ifitp), 18);
	iod_tables_hdr.text_strings_offset = fixed (rel (text_strings_ptr), 18);
	iod_tables_hdr.date_time_compiled = clock_ ();
	iod_tables_hdr.version = IODT_VERSION_5;

	text_strings_array_ptr = addr (text_strings.chars);
	text_strings.length = text_strings.length + 4;
	copy_ptr = addr (text_strings_array (text_strings.length));  /* get ptr 1 char beyond end of data */
	bit_count = 36 * fixed (rel (copy_ptr), 18);	/* compute bit count */
	call tssi_$finish_segment (output_ptr, bit_count, "1000"b, acl_info_ptr, code);
	if code ^= 0 then go to output_error;
	output_ptr, acl_info_ptr = null;

/* internal routine to make sure pointer is on an even word boundary */
adjust_ptr: proc (ptr_to_adjust) returns (ptr);

dcl  ptr_to_adjust ptr;

	     if mod (fixed (rel (ptr_to_adjust), 18), 2) ^= 0 then
		return (addrel (ptr_to_adjust, 1));
	     else return (ptr_to_adjust);

	end adjust_ptr;

     end build_output_table;
%page;

/* Syntax Functions */

/* General syntax functions */

/* similar to the <name> built-in syntax functions except they use different
   length limitations and special char strings */

name_routines: proc;

dcl  name_length fixed bin;
dcl  name_type fixed bin;

name_32: entry returns (bit (1));			/* allow upper/lower case alpha, numeric, - & _ */
	name_length = 32;
	name_type = 1;
	go to name_routines_common;

name_24: entry returns (bit (1));			/* allow upper/lower case alpha, numeric, - & _ */
	name_length = 24;
	name_type = 1;
	go to name_routines_common;

name_path: entry returns (bit (1));			/* allow upper/lower case alpha, numeric & -_$<> */
	name_length = 256;
	name_type = 2;
	go to name_routines_common;

name_comm_line: entry returns (bit (1));		/* allow upper/lower case alpha, numeric & _.* / */
	name_length = 32;
	name_type = 3;
	go to name_routines_common;

name_user_id: entry returns (bit (1));			/* allow upper/lower case alpha, numeric & -_.* */
	name_length = 30;
	name_type = 4;
	go to name_routines_common;

name_32_multi: entry returns (bit (1));			/* allow upper/lower case alpha, numeric & _.- */
	name_length = 32;
	name_type = 5;
	go to name_routines_common;

name_routines_common:
	if token.Lvalue > 0 then do;
	     if token.Lvalue <= name_length then do;

		if name_type = 1 then do;		/* name_32 & name_24 */
		     if verify (token_value, ALL_VALID_CHARS_AND_NUMERIC || "_-") = 0 then
			return (TOKEN_GOOD);
		end;

		else if name_type = 2 then do;	/* name_path */
		     if verify (token_value, ALL_VALID_CHARS_AND_NUMERIC || "-_$<>") = 0 then
			return (TOKEN_GOOD);
		end;

		else if name_type = 3 then do;	/* name_comm_line */
		     if verify (token_value, ALL_VALID_CHARS_AND_NUMERIC || "_.*/") = 0 then
			return (TOKEN_GOOD);
		end;

		else if name_type = 4 then do;	/* name_user_id */
		     if verify (token_value, ALL_VALID_CHARS_AND_NUMERIC || "_.*") = 0 then
			return (TOKEN_GOOD);
		end;

		else if name_type = 5 then do;	/* name_32_multi */
		     if verify (token_value, ALL_VALID_CHARS_AND_NUMERIC || "_.-") = 0 then
			return (TOKEN_GOOD);
		end;
	     end;
	end;
	return (TOKEN_BAD);

     end name_routines;
%page;

/* General action routines */

/* routines to store various strings in the text_strings area */

store_string_functions: proc;

dcl  string_to_store char (*);

dcl  1 target unaligned like text_offset;

dcl  source_string char (source_string_len) based (source_string_ptr);
dcl  source_string_len fixed bin;
dcl  source_string_ptr ptr;

dcl  target_string char (source_string_len) based (target_string_ptr);
dcl  target_string_ptr ptr;
dcl  dupe_string_loc fixed bin;

store_forms_escape_string: entry (target);

/* copy the string in forms_escape_string_n into text_strings and save offset
   and length of the copied string in target structure values */

	source_string_ptr = addr (forms_escape_string_n);
	source_string_len = forms_escape_string_index - 1;
	go to common;


store_temp_token_value_string: entry (target);

/* routine copies the temp_token_value string into text_strings and saves
   offset and length of the copied string in target structure values */

	source_string_ptr = addrel (addr (temp_token_value), 1);
	source_string_len = length (temp_token_value);
	go to common;

store_token_value_string: entry (target);

/* routine copies the token_value string into text_strings and saves offset
   and length of the copied string in target structure values */

	source_string_ptr = token.Pvalue;
	source_string_len = token.Lvalue;
	go to common;

store_direct_string: entry (target, string_to_store);

/* this routine stores the input string it is called with into text_strings
   and saves the offset and length of the string into target structure values */

	source_string_ptr = addr (string_to_store);
	source_string_len = length (rtrim (string_to_store));

common:
	text_strings.length = text_strings.length + 1;
	target_string_ptr = addr (text_strings_array (text_strings.length));
	text_strings.length = text_strings.length - 1;

/* eliminate dupe strings by seeing if this exact string is already in the
   text_strings area.  If not, we will add it.  If it is, we will just
   record the starting location of the string already in the text_strings area
   as the location of this copy */

	dupe_string_loc = index (text_strings.chars, source_string);
	if dupe_string_loc = 0 then do;		/* new string */
	     target_string = source_string;		/* copy string */
	     target.first_char = text_strings.length + 1; /* save where it starts */
	     text_strings.length = text_strings.length + source_string_len; /* bump count of string chars saved */
	end;
	else do;					/* dupe string */
	     target.first_char = dupe_string_loc;	/* save where it starts */
	end;
	target.total_chars = source_string_len;		/* save length of string */

	return;

     end store_string_functions;


/* routine to return a string from the text string area */

return_string: proc (source_offsets) returns (char (*));

dcl  1 source_offsets unaligned like text_offset;

	if source_offsets.total_chars = 0 then
	     return ("");
	else return (substr (text_strings.chars, source_offsets.first_char, source_offsets.total_chars));

     end return_string;


/* parse a comma delimited string into its components */

parse_parms_string: proc (data_offset_ptr);

dcl  data_offset_ptr ptr;				/* ptr to text_offset data */
dcl  1 source_text_offsets unaligned like text_offset based (data_offset_ptr);

	temp_token_value = return_string (source_text_offsets);
	parms_count = 0;
	parms (*) = "";
loop:	parms_count = parms_count + 1;
	parms (parms_count) = before (temp_token_value, ",");
	temp_token_value = after (temp_token_value, ",");
	if temp_token_value ^= "" then
	     go to loop;

     end parse_parms_string;
%page;

/* global action routines */

store_global_grace_time: proc;

	if iod_tables_hdr.grace_time ^= -1 then
	     call statement_error (4, "Time");
	else iod_tables_hdr.grace_time = token.Nvalue * 60000000;

     end store_global_grace_time;


store_global_max_queues: proc;

	if iod_tables_hdr.max_queues ^= -1 then
	     call statement_error (4, "Max_queues");
	else do;
	     if token.Nvalue < 1 | token.Nvalue > 4 then
		call statement_error (6, "Max_queues", "4");
	     else iod_tables_hdr.max_queues = token.Nvalue;
	end;

     end store_global_max_queues;
%page;

/* syntax functions and action routines for Line keyword
   and it's subordinate keywords */

/* Syntax functions */

/* Action Routines */

/* create a new entry in the line table */
line_create_table_entry: proc;

/* first see if line is currently defined in the table */
	do i = 1 to line_tab.n_lines			/* is line currently defined */
	     while (line_tab.line_id (i) ^= token_value);
	end;
	if i <= line_tab.n_lines then			/* in table so complain */
	     call statement_error (10, "Line", token_value);

/* Create entry in table for this line name.
   We will create a new entry for a duplicate name so we can check the rest
   of the substatements */

	line_tab.n_lines = line_tab.n_lines + 1;
	ltep = addr (line_tab.entries (line_tab.n_lines));

/* now clear out the entry */
	unspec (lte) = "0"b;
	lte.line_id = token_value;
	lte.chan_id = "";
	lte.att_desc.first_char,
	     lte.att_desc.total_chars = 0;
	lte.maj_dev_list = ""b;

     end line_create_table_entry;

/* save line attach description */
line_save_att_desc: proc;

	if token.Lvalue > max_length_att_desc then
	     call statement_error (15, "att_desc", max_length_att_desc_char);
	if lte.att_desc.total_chars ^= 0 then
	     call statement_error (13, "att_desc", "Line", token_value);
	call store_token_value_string (lte.att_desc);

     end line_save_att_desc;

/* save line comment */
line_save_comment: proc;

	if token.Lvalue > max_length_comment then
	     call statement_error (15, "comment", max_length_comment_char);
	if lte.comment.total_chars ^= 0 then
	     call statement_error (13, "comment", "Line", token_value);
	call store_token_value_string (lte.comment);

     end line_save_comment;

/* make sure channel ID is valid and save it */
line_save_channel_id: proc;

dcl  channel_id fixed bin;
dcl  fnp_sw bit (1) aligned;

/* validate that this is good channel ID */
	if cdtp ^= null then do;
	     call cdt_mgr_$find_cdt_channel (cdtp, (token_value), channel_id, fnp_sw, code);
	     if code ^= 0 then
		call statement_error (14);
	end;
	if lte.chan_id ^= "" then
	     call statement_error (13, "channel", "Line", (lte.line_id));
	lte.chan_id = token_value;

     end line_save_channel_id;

/* save line device name */
line_save_device: proc;

/* see if this device name is already used in a line statement */
	do i = 1 to line_device_names_count while
	     (line_device_names (i) ^= token_value);
	end;
	if i > line_device_names_count then do;		/* new device name */
	     if i > max_size then do;			/* table too big */
		call statement_error (16, max_size_char, "devices");
	     end;

	     line_device_names_count = i;		/* bump count of remembered devices */
	     line_device_names (i) = token_value;	/* and save the new name */
	end;

/* set the lte entry to point to the device name in the temp list.
   We will reset to to the true device entry later */
	substr (lte.maj_dev_list, i, 1) = TRUE;

     end line_save_device;

/* validate that line table entry is complete */
line_validate_entry: proc;

	if lte.maj_dev_list = FALSE then
	     call semant_error (18, "devices", "Line", lte.line_id);
	if lte.chan_id = "" then
	     call semant_error (19, "channel", "Line", lte.line_id);
	if lte.att_desc.total_chars = 0 then
	     call semant_error (19, "att_desc", "Line", lte.line_id);

     end line_validate_entry;
%page;

/* syntax functions for Device keyword and it's subordinate keywords */


/* the following action entries are for the Device keyword and it's device table */

/* create a new minor device table entry */
dev_create_minor_table_entry: proc;

/* If we defined a default minor device because we already found some minor
   device statements, change its name from the default name to this name.

   If there is no default minor device defined yet, we start add this one to
   the chain of current minor devices */

	if default_minor_device_defined then do;
	     default_minor_device_defined = FALSE;
	     call revert_default_minor_device;
	end;
	else call init_minor_device;

     end dev_create_minor_table_entry;


/* create a new entry in the device table */
dev_create_table_entry: proc;

/* see if this is valid device name and is currently undefined */
	do i = 1 to iod_device_tab.n_devices		/*  is device already defined */
	     while (iod_device_tab.dev_id (i) ^= token_value);
	end;
	if i <= iod_device_tab.n_devices then		/* it is in table so complain */
	     call statement_error (10, "Device", token_value);

/* Create entry in table for this line name.
   We will create a new entry for a duplicate name so we can check the rest
   of the substatements */

	iod_device_tab.n_devices = iod_device_tab.n_devices + 1;
	idtep = addr (iod_device_tab.entries (iod_device_tab.n_devices));

/* now clear out the entry */
	unspec (idte) = "0"b;
	default_minor_device_defined = FALSE;
	idte.dev_id = token_value;
	idte.attach_name,
	     idte.ctl_attach_name = "";
	idte.driver_module.first_char,
	     idte.driver_module.total_chars,
	     idte.args.first_char,
	     idte.args.total_chars,
	     idte.head_sheet.first_char,
	     idte.head_sheet.total_chars,
	     idte.tail_sheet.first_char,
	     idte.tail_sheet.total_chars = 0;
	idte.attach_type,
	     idte.ctl_attach_type,
	     idte.first_minor,
	     idte.last_minor,
	     idte.paper_type = -1;
	mdtep = null;

     end dev_create_table_entry;

dev_save_args: proc;

	if idte.args.total_chars ^= 0 then
	     call statement_error (13, "args", "Device", idte.dev_id);
	call store_token_value_string (idte.args);

     end dev_save_args;

dev_save_attach_method: proc (device_or_terminal, type_attach);

dcl  device_or_terminal fixed bin;			/* 1 = device entry */
						/* 2 = control terminal attachment */
dcl  type_attach fixed bin;
dcl  item_names (2, 4) char (13) int static options (constant) init (
	"prph", "line", "dial_id", "variable line",
	"ctl_line", "ctl_dial_id", "ctl_source", "");

	if device_or_terminal = 1 then do;		/* device */
	     if idte.attach_type ^= -1 then do;		/* already have attachment for this entry */
		if idte.attach_type = type_attach then
		     call statement_error (13, item_names (device_or_terminal, type_attach), "Device", idte.dev_id);
		else call statement_error (22, idte.dev_id);
	     end;
	     else do;
		if type_attach = ATTACH_TYPE_DIAL then
		     call check_dial_id (token_value, "dial_id");
		idte.attach_name = token_value;
		idte.attach_type = type_attach;
		if type_attach = ATTACH_TYPE_VARIABLE_LINE then
		     substr (var_line_list, iod_device_tab.n_devices, 1) = TRUE;
	     end;
	end;
	else do;					/* control terminal attachment */
	     if idte.ctl_attach_type ^= -1 then do;	/* already have attachment for this entry */
		if idte.ctl_attach_type = type_attach then
		     call statement_error (13, item_names (device_or_terminal, type_attach), "Device", idte.dev_id);
		else call statement_error (24, idte.dev_id);
	     end;
	     else do;
		if type_attach = CTL_ATTACH_TYPE_DIAL then
		     call check_dial_id (token_value, "ctl_dial_id");
		idte.ctl_attach_name = token_value;
		idte.ctl_attach_type = type_attach;
		if type_attach = CTL_ATTACH_TYPE_MC then
		     substr (var_line_list, iod_device_tab.n_devices, 1) = TRUE;
	     end;
	end;

/* checks if a dial id is unique */
check_dial_id: proc (value_to_check, keyword_name);

dcl  value_to_check char (*);
dcl  keyword_name char (*);

	     do i = 1 to iod_device_tab.n_devices;
		if iod_device_tab.attach_type (i) = ATTACH_TYPE_DIAL then
		     if iod_device_tab.attach_name (i) = value_to_check then
			go to dial_id_dup;
		if iod_device_tab.ctl_attach_type (i) = CTL_ATTACH_TYPE_DIAL then
		     if iod_device_tab.ctl_attach_name (i) = value_to_check then
			go to dial_id_dup;
	     end;
	     return;

dial_id_dup:
	     call statement_error (23, keyword_name, value_to_check, idte.dev_id);

	end check_dial_id;

     end dev_save_attach_method;

/* save device or minor device comment */
dev_save_comment: proc;

	if token.Lvalue > max_length_comment then
	     call statement_error (15, "comment", max_length_comment_char);

/* if there is no minor device entry, the comment belongs in the device entry */
	if mdtep = null () then do;
	     if idte.comment.total_chars ^= 0 then
		call statement_error (13, "comment", "Device", idte.dev_id);
	     call store_token_value_string (idte.comment);
	end;
	else do;
	     if mdte.comment.total_chars ^= 0 then
		call statement_error (13, "comment", "minor_device", mdte.dev_id);
	     call store_token_value_string (mdte.comment);
	end;

     end dev_save_comment;

dev_save_default_form: proc;

/* if no minor devices defined, then define the default */
	if idte.first_minor = -1 then
	     call init_default_minor_device;

/* validate that this is the first default form entry for this minor device */
	if mdte.default_form.total_chars ^= 0 then
	     call statement_error (49, "default_form", "minor_device", mdte.dev_id, "Device", idte.dev_id);

	if temp_token_value_used then do;
	     temp_token_value_used = FALSE;
	     call store_temp_token_value_string (mdte.default_form);
	end;
	else call store_token_value_string (mdte.default_form);

     end dev_save_default_form;


dev_save_default_type: proc;

/* parse default_type name into it's two possible parts */
	cn.first_name = before (token_value, ".");
	cn.second_name = after (token_value, ".");

/* if there are no minor devices, create the default entry */
	if idte.first_minor = -1 then
	     call init_default_minor_device;

/* otherwise check for duplicate entry for this device */
	else do;
	     if q_group.default_type (minor_device_tab.n_minor).first_name ^= "" then
		call statement_error (49, "default_type", "minor_device", mdte.dev_id,
		     "Device", idte.dev_id);
	end;

/* save the minor device name in temp storage for later use */
	q_group.default_type (minor_device_tab.n_minor).first_name = cn.first_name;
	q_group.default_type (minor_device_tab.n_minor).second_name = cn.second_name;

     end dev_save_default_type;


dev_save_driver_module: proc;

	if idte.driver_module.total_chars ^= 0 then do;
	     call statement_error (13, "driver_module", "Device", idte.dev_id);
	end;
	call store_token_value_string (idte.driver_module);

     end dev_save_driver_module;


dev_save_forms_validation: proc;

	if idte.forms_validation.total_chars ^= 0 then
	     call statement_error (13, "forms_validation", "Device", idte.dev_id);
	call store_token_value_string (idte.forms_validation);
     end dev_save_forms_validation;


dev_save_font_dir: proc;

	if idte.font_dir.total_chars ^= 0 then
	     call statement_error (13, "font_dir", "Device", idte.dev_id);
	call store_token_value_string (idte.font_dir);
     end dev_save_font_dir;


dev_save_forms_table: proc;

	if idte.forms_table.total_chars ^= 0 then
	     call statement_error (13, "forms_table", "Device", idte.dev_id);
	call store_token_value_string (idte.forms_table);
     end dev_save_forms_table;


dev_save_head_sheet: proc;

	if idte.head_sheet.total_chars ^= 0 then
	     call statement_error (13, "head_sheet", "Device", idte.dev_id);
	call store_token_value_string (idte.head_sheet);
     end dev_save_head_sheet;


dev_save_minor_args: proc;

/* if no minor devices defined, then define the default */
	if idte.first_minor = -1 then
	     call init_default_minor_device;

/* validate that this is the first minor args entry for this minor device */
	if mdte.args.total_chars ^= 0 then
	     call statement_error (49, "minor_args", "minor_device", mdte.dev_id, "Device", idte.dev_id);

	call store_token_value_string (mdte.args);

     end dev_save_minor_args;


dev_save_paper_type: proc;

	if idte.paper_type ^= -1 then
	     call statement_error (13, "paper_type", "Device", idte.dev_id);

	if token_value = "continuous" then
	     idte.paper_type = PAPER_TYPE_CONTINUOUS;
	else if token_value = "single" then
	     idte.paper_type = PAPER_TYPE_SINGLE;
     end dev_save_paper_type;


dev_save_tail_sheet: proc;

	if idte.tail_sheet.total_chars ^= 0 then
	     call statement_error (13, "tail_sheet", "Device", idte.dev_id);
	call store_token_value_string (idte.tail_sheet);
     end dev_save_tail_sheet;


/* validate that a device entry is complete */
dev_validate_entry: proc;

/* create a default minor device if none explicitely defined */
	if idte.first_minor = -1 then
	     call init_default_minor_device;

/* make sure required elements are defined */
	if idte.driver_module.total_chars = 0 then
	     call semant_error (47, idte.dev_id);
	if idte.attach_type = 0 then
	     call semant_error (48, idte.dev_id);

/* make sure there is a cross-check between forms_table and default forms */
	do i = idte.first_minor to idte.last_minor;
	     mdtep = addr (minor_device_tab.entries (i));
	     if mdte.default_form.total_chars = 0 then do;/* no default form defined */
		if idte.forms_table.total_chars ^= 0 then do; /* but a forms table defined */
		     if mdte.dev_id = idte.dev_id then
			call semant_error (19, "default_form", "Device", idte.dev_id);
		     else call semant_error (19, "default_form", "minor_device", mdte.dev_id);
		end;
	     end;
	     else do;				/* have default form defined */
		if idte.forms_table.total_chars = 0 then/* but no forms table defined */
		     call semant_error (19, "forms_table", "Device", idte.dev_id);

/* turn on bits for any minor devices which have forms_table entries defined */
		substr (minor_device_forms, i, 1) = TRUE;
	     end;
	end;
     end dev_validate_entry;


init_default_minor_device: proc;

	default_minor_device_defined = TRUE;

init_minor_device: entry;

	minor_device_tab.n_minor = minor_device_tab.n_minor + 1;
	if minor_device_tab.n_minor > max_size then do;
	     call statement_error (16, max_size_char, "minor_device");
	end;
	mdtep = addr (minor_device_tab.entries (minor_device_tab.n_minor));
	unspec (mdte) = "0"b;
	if idte.first_minor = -1 then
	     idte.first_minor = minor_device_tab.n_minor;
	idte.last_minor = minor_device_tab.n_minor;
	mdte.default_dev_class = 0;
	mdte.major_index = iod_device_tab.n_devices;
	mdte.args.first_char,
	     mdte.args.total_chars,
	     mdte.default_form.first_char,
	     mdte.default_form.total_chars = 0;
	q_group.default_type (minor_device_tab.n_minor) = "";
	if default_minor_device_defined then
	     minor_name = idte.dev_id;
	else do;
revert_default_minor_device: entry;
	     minor_name = token_value;
	end;
	mdte.dev_id = substr (minor_name, 1, length (mdte.dev_id));
	q_group.is_default_minor (minor_device_tab.n_minor) = default_minor_device_defined;

     end init_default_minor_device;
%page;

/* syntax and action functions for Request_type keyword
   and it's subordinate keywords */

/* Syntax */

access_class_ok: proc returns (bit (1));

/* validate authorization for validity. leave the validated result in the
   authorization_bits variable for later use */
	call convert_authorization_$from_string (authorization_bits, token_value, code);

	return (code = 0);

     end access_class_ok;


/* Action routines */

/* create a new entry in the request type table */
rqt_create_table_entry: proc;

/* see if this is valid request type name and is currently undefined */
	do i = 1 to q_group_tab.n_q_groups		/* is line in table */
	     while (q_group_tab.name (i) ^= token_value);
	end;
	if i <= q_group_tab.n_q_groups then		/* it is in table so complain */
	     call statement_error (10, "Request_Type", token_value);

/* Create entry in table for this line name.
   We will create a new entry for a duplicate name so we can check the rest
   of the substatements */

	q_group_tab.n_q_groups = q_group_tab.n_q_groups + 1;
	qgtep = addr (q_group_tab.entries (q_group_tab.n_q_groups));

/* now clear out the entry */
	unspec (qgte) = "0"b;
	default_device_class_defined = FALSE;		/* OFF until default class is defined by implication */
	qgte.name = token_value;
	qgte.driver_id,
	     qgte.generic_type,
	     qgte.rqti_seg_name,
	     qgte.line_charge.queue (*),
	     qgte.page_charge.queue (*) = "";
	qgte.accounting.first_char,
	     qgte.accounting.total_chars,
	     qgte.forms_validation.first_char,
	     qgte.forms_validation.total_chars,
	     qgte.default_form.first_char,
	     qgte.default_form.total_chars,
	     qgte.forms_table.first_char,
	     qgte.forms_table.total_chars,
	     qgte.font_dir.first_char,
	     qgte.font_dir.total_chars = 0;
	qgte.default_generic_queue,
	     qgte.default_queue,
	     qgte.max_queues,
	     qgte.first_dev_class,
	     qgte.last_dev_class = -1;
	line_charge_keyword_used, page_charge_keyword_used = FALSE;
	dctep = null;
     end rqt_create_table_entry;

rqt_save_access: proc (type);

dcl  type fixed bin;
dcl  error_string_names (3) char (16) int static options (constant)
	init ("max_access_class", "min_access_class", "min_banner");

/* create default access class if there is no class currently defined */
	if qgte.first_dev_class = -1 then
	     call rqt_create_default_device_class_entry;

/* validate for dupe entry for this storage value */
	if (type = 1 & dcte.max_access ^= (72)"1"b)
	     | (type = 2 & dcte.min_access ^= (72)"1"b)
	     | (type = 3 & dcte.min_banner ^= (72)"1"b) then do;
	     call statement_error (13,
		error_string_names (type), "Request_type", qgte.name);
	end;

/* the authorization_bits variable was set by an immediately preceeding call
   to access_class_ok.  now store it. */

	if type = 1 then				/* max_access_class */
	     dcte.max_access = authorization_bits;
	else if type = 2 then			/* min_access_class */
	     dcte.min_access = authorization_bits;
	else dcte.min_banner = authorization_bits;	/* min_banner */

     end rqt_save_access;

rqt_save_accounting: proc;

	if qgte.accounting.total_chars ^= 0 then
	     call statement_error (13, "accounting", "Request_type", qgte.name);
	call store_token_value_string (qgte.accounting);

     end rqt_save_accounting;


rqt_save_charge: proc (type);

dcl  type fixed bin;

dcl  charge_type (3) char (11) int static options (constant)
	init ("line_charge", "page_charge", "card_charge");

	if type = 1 | type = 3 then do;
	     if line_charge_keyword_used then
		call statement_error (13, charge_type (type), "Request_type", qgte.name);
	     qgte.line_charge.queue (*) = "UNDEFINED_PRICE";
	     line_charge_keyword_used = TRUE;
	end;
	else if type = 2 then do;
	     if qgte.generic_type = "punch" then
		call statement_error (2, charge_type (type));
	     if page_charge_keyword_used then
		call statement_error (13, charge_type (type), "Request_type", qgte.name);
	     qgte.page_charge.queue (*) = "UNDEFINED_PRICE";
	     page_charge_keyword_used = TRUE;
	end;
	saved_charge_type = type;
	saved_charge_cntr = 1;

rqt_save_charge_continue: entry;

	if saved_charge_type = 1 | saved_charge_type = 3 then
	     qgte.line_charge.queue (saved_charge_cntr) = token_value;
	else qgte.page_charge.queue (saved_charge_cntr) = token_value;
	if saved_charge_cntr < 4 then
	     saved_charge_cntr = saved_charge_cntr + 1;

     end rqt_save_charge;


/* save Request_type comment */
rqt_save_comment: proc;

	if token.Lvalue > max_length_comment then
	     call statement_error (15, "comment", max_length_comment_char);

	if qgte.comment.total_chars ^= 0 then
	     call statement_error (13, "comment", "Device", qgte.name);
	call store_token_value_string (qgte.comment);

     end rqt_save_comment;

rqt_save_default_form: proc;

/* look for dupe entry */
	if qgte.default_form.total_chars ^= 0 then
	     call statement_error (13, "default_form", "Request_type", qgte.name);

	if temp_token_value_used then do;
	     temp_token_value_used = FALSE;
	     call store_temp_token_value_string (qgte.default_form);
	end;
	else call store_token_value_string (qgte.default_form);

     end rqt_save_default_form;

rqt_save_default_queue: proc;

	if qgte.default_queue ^= -1 then
	     call statement_error (13, "default_queue", "Request_type", qgte.name);
	if token.Nvalue < 1 | token.Nvalue > 3 then
	     call statement_error (6, "default_queue", "3");
	else qgte.default_queue = token.Nvalue;

     end rqt_save_default_queue;


rqt_save_device: proc;

/* parse device name into it's two possible parts */
	cn.first_name = before (token_value, ".");
	cn.second_name = after (token_value, ".");

/* see device is already defined */
	do i = 1 to q_group_device_names_count while
	     (q_group.device_name (i).first_name ^= cn.first_name
	     | q_group.device_name (i).second_name ^= cn.second_name);
	end;
	if qgte.first_dev_class = -1 then
	     call rqt_create_default_device_class_entry;
	else do;
	     if substr (dcte.device_list, i, 1) then
		call statement_error (17, token_value, "request_type", qgte.name);
	end;

/* save the data */
	substr (dcte.device_list, i, 1) = TRUE;
	if i > q_group_device_names_count then do;
	     q_group_device_names_count = i;
	     q_group.device_name.first_name (i) = cn.first_name;
	     q_group.device_name.second_name (i) = cn.second_name;
	end;

     end rqt_save_device;


rqt_save_driver_id: proc;

dcl  (person, project, instance) char (32) varying;

	if qgte.driver_id ^= "" then do;
	     call statement_error (13, "driver_id", "Request_type", qgte.name);
	end;
	person = before (token_value, ".");
	project = after (token_value, ".");
	if project = "" then
	     call statement_error (20);
	instance = after (project, ".");
	project = before (project, ".");
	if instance ^= "" then
	     call statement_error (21);
	qgte.driver_id = person || "." || project || ".*";

     end rqt_save_driver_id;

rqt_save_forms_validation: proc;

	if qgte.forms_validation.total_chars ^= 0 then
	     call statement_error (13, "forms_validation", "Request_type", qgte.name);
	call store_token_value_string (qgte.forms_validation);
     end rqt_save_forms_validation;


rqt_save_font_dir: proc;

	if qgte.font_dir.total_chars ^= 0 then
	     call statement_error (13, "font_dir", "Device", qgte.name);
	call store_token_value_string (qgte.font_dir);
     end rqt_save_font_dir;


rqt_save_forms_table: proc;

	if qgte.forms_table.total_chars ^= 0 then
	     call statement_error (13, "forms_table", "Device", qgte.name);
	call store_token_value_string (qgte.forms_table);
     end rqt_save_forms_table;


rqt_save_generic_type: proc;

	if qgte.generic_type ^= "" then
	     call statement_error (13, "generic_type", "Request_type", qgte.name);
	qgte.generic_type = token_value;
	if token_value = qgte.name then do;
	     qgte.default_generic_queue = 1;
	     if token_value = "printer" then
		default_print_defined = TRUE;
	     else if token_value = "punch" then
		default_punch_defined = TRUE;
	end;
     end rqt_save_generic_type;

rqt_save_max_queues: proc;

	if qgte.max_queues ^= -1 then
	     call statement_error (13, "max_queues", "Request_type", qgte.name);
	if token.Nvalue < 1 | token.Nvalue > 4 then
	     call statement_error (6, "max_queues", "4");
	else qgte.max_queues = token.Nvalue;

     end rqt_save_max_queues;


rqt_save_rqti_seg: proc;

	if qgte.rqti_seg_name ^= "" then
	     call statement_error (13, "rqti_seg_name", "Request_type", qgte.name);
	qgte.rqti_seg_name = token_value;

     end rqt_save_rqti_seg;


/* validate that a request type entry is complete */
rqt_validate_entry: proc;

	if qgte.first_dev_class = -1 then
	     call rqt_create_default_device_class_entry;
	call rqt_check_device_class;

	if qgte.driver_id = "" then
	     qgte.driver_id = "IO.SysDaemon.*";

	if qgte.accounting.total_chars = 0 then
	     call store_direct_string (qgte.accounting, "system");

	if return_string (qgte.accounting) ^= "system" then do;
	     if qgte.driver_id = "IO.SysDaemon" then
		call semant_error (28, qgte.name);
	end;
	else if after (qgte.driver_id, ".") ^= "SysDaemon.*" then
	     call semant_error (29, qgte.name);

	if qgte.generic_type = "" then
	     call semant_error (19, "generic_type", "Request_type", qgte.name);

	if qgte.forms_table.total_chars = 0 then do;	/* no forms table defined */
	     if qgte.default_form.total_chars ^= 0 then	/* but default form defined */
		call semant_error (19, "forms_table", "Request_type", qgte.name);
	end;
	else do;					/* forms table is defined */
	     if qgte.default_form.total_chars = 0 then	/* but no default form defined */
		call semant_error (19, "default_form", "Request_type", qgte.name);

/* turn on bits to show which device classes have forms table entries */
	     do i = qgte.first_dev_class to qgte.last_dev_class;
		substr (device_class_forms, i, 1) = TRUE;
	     end;
	end;
     end rqt_validate_entry;


rqt_check_device_class: proc;				/* fills in defaults if necessary */

	if dcte.min_access = (72)"1"b then
	     dcte.min_access = ""b;
	if dcte.min_banner = (72)"1"b then
	     dcte.min_banner = dcte.min_access;
	if dcte.max_access = (72)"1"b then
	     dcte.max_access = dcte.min_access;
	if dcte.device_list = ""b then
	     call semant_error (18, "devices", "Request_type", qgte.name);

     end rqt_check_device_class;


rqt_create_default_device_class_entry: proc;

	default_device_class_defined = TRUE;

rqt_create_device_class_entry: entry;

	dev_class_tab.n_classes = dev_class_tab.n_classes + 1;
	if dev_class_tab.n_classes > max_size then do;
	     call statement_error (16, max_size_char, "device_class");
	end;
	dctep = addr (dev_class_tab.entries (dev_class_tab.n_classes));
	unspec (dcte) = "0"b;
	if qgte.first_dev_class = -1 then
	     qgte.first_dev_class = dev_class_tab.n_classes;
	qgte.last_dev_class = dev_class_tab.n_classes;
	dcte.qgte_index = q_group_tab.n_q_groups;
	dcte.max_access,
	     dcte.min_access,
	     dcte.min_banner = (72)"1"b;
	dcte.device_list = ""b;
	if default_device_class_defined then
	     minor_name = qgte.name;
	else do;
revert_default_class: entry;
	     minor_name = token_value;
	end;
	dcte.id = minor_name;
	q_group.is_default_device_class (dev_class_tab.n_classes) = default_device_class_defined;

     end rqt_create_default_device_class_entry;

%page;

/* syntax functions for the Forms_table keyword and it's subordinate keywords */

forms_size_ok: proc returns (bit (1));

dcl  number_float float bin (27);
dcl  type char (12);

	item_ok = TOKEN_GOOD;
	number_float = cv_float_ (token_value, code);	/* try to convert */
	forms_size_type = 1;			/* assume inches */

/* if it didn't like the number to convert, probabily contains alpha chars
   defining the type of number specified.  Reconvert the data up to the bad
   char and test the bad chars for valid conversion types. */
	if code ^= 0 then do;
	     number_float = convert (number_float, substr (token_value, 1, code - 1));
	     type = substr (token_value, code);
	     if type = "in" | type = "i" | type = "inches" then
		forms_size_type = FACTOR_INCHES;
	     else if type = "cm" | type = "c" | type = "centimeters" then
		forms_size_type = FACTOR_CENTIMETERS;
	     else if type = "pt" | type = "p" | type = "points" then
		forms_size_type = FACTOR_POINTS;
	     else if type = "lpi" | type = "LPI" then
		forms_size_type = FACTOR_LPI;
	     else item_ok = TOKEN_BAD;
	end;
	if item_ok then do;
	     if forms_size_type ^= FACTOR_LPI then
		forms_size_info = SIZE_FACTORS (forms_size_type) * number_float;
	     else forms_size_info = SIZE_FACTORS (forms_size_type) / number_float;
	end;
	return (item_ok);

     end forms_size_ok;


forms_type: proc returns (bit (1));

	item_ok = TOKEN_GOOD;
	do i = lbound (FORMS_TYPE_STRINGS, 1) to hbound (FORMS_TYPE_STRINGS, 1)
	     while (token_value ^= FORMS_TYPE_STRINGS (i));
	end;
	if i > hbound (FORMS_TYPE_STRINGS, 1) then do;
	     item_ok = TOKEN_BAD;
	     token.Nvalue = 0;
	end;
	else token.Nvalue = i;
	return (item_ok);
     end forms_type;


forms_tty_char_ok: proc returns (bit (1) aligned);

dcl  ALL_LOWERCASE char (26) defined ALL_VALID_CHARS_AND_NUMERIC pos (27);
dcl  ALL_UPPERCASE char (26) defined ALL_VALID_CHARS_AND_NUMERIC pos (1);
dcl  asc_mnemonic char (3);
dcl  i fixed bin;

dcl  asc_value (0:32) char (3) static options (constant) init
	("nul", "soh", "stx", "etx", "eot", "enq", "ack", "bel",
	"bs ", "tab", "lf ", "vt ", "ff ", "cr ", "so ", "si ",
	"dle", "dc1", "dc2", "dc3", "dc4", "nak", "syn", "etb",
	"can", "em ", "sub", "esc", "fs ", "gs ", "rs ", "us ", "sp ");

	if token.quoted_string & token.Lvalue = 1 then
	     call return_tty_char (bin (unspec (token_value)));

	if token.Lvalue = 1 then do;
	     if index (BREAKS, token_value) = 0 then
		call return_tty_char (bin (unspec (token_value)));
	end;
	if octal_char_ok () then return (TOKEN_GOOD);
	if token.Lvalue = 2 & substr (token_value, 1, 1) = "^" then do;
	     i = index ("@abcdefghijklmnopqrstuvwxyz[\]^_", substr (token_value, 2, 1));
	     if i = 0 then
		i = index ("@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_", substr (token_value, 2, 1));
	     if i = 0 then return (TOKEN_BAD);
	     call return_tty_char (i - 1);
	end;
	if token.Lvalue <= 3 then do;
	     asc_mnemonic = translate (token_value, ALL_LOWERCASE, ALL_UPPERCASE);
	     if asc_mnemonic = "del" then call return_tty_char (127);
	     if asc_mnemonic = "nl " then call return_tty_char (10);
	     if asc_mnemonic = "pad" then call return_tty_char (0);
	     if asc_mnemonic = "ht" then call return_tty_char (9);
	     do i = lbound (asc_value, 1) to hbound (asc_value, 1);
		if asc_value (i) = asc_mnemonic then call return_tty_char (i);
	     end;

	end;
	return (TOKEN_BAD);

return_tty_char: proc (a_value);

dcl  a_value fixed bin;

	     token.Nvalue = a_value;
	     go to nlret;
	end return_tty_char;

nlret:	return (TOKEN_GOOD);

octal_char_ok: proc returns (bit (1) aligned);

	     if token.Lvalue > 3 then
		return (TOKEN_BAD);

	     token.Nvalue = cv_oct_check_ (token_value, code);
	     return (code = 0);
	end octal_char_ok;

     end forms_tty_char_ok;
%page;

/* the following entries are for the Forms_table keyword and it's table */

/* add a synonym name to the current element entry */
forms_add_syn_element_name: proc;

	element_common.n_names = element_common.n_names + 1;
	call store_token_value_string (element_common.names (element_common.n_names));

     end forms_add_syn_element_name;

/* create a new element entry.  since we are creating these entries in a temp
   segment, we know that the entire entry is zeros when we start */
forms_create_element_entry: proc;

dcl  element_offset fixed bin;
dcl  temp_fep ptr;

	call forms_validate_entry;			/* validate last entry */

/* see if this name is already defined within the forms group.  We will save
   the current element ptr (fep), then scan through the current element names
   to see if this one is unique;  if not we will complain.  In any case, we
   will create a new element ptr at the end of the current elements */
	element_offset = forms_groups (iod_forms_info_tab.n_groups).first_element_index;
	i = forms_scan_for_element_name (element_offset, token_value);
	if i ^= -1 then
	     call statement_error (50, token_value, return_string (element_common.names (i)));

/* Create entry in table for this line name.
   We will create a new entry for a duplicate name so we can check the rest
   of the substatements */
	temp_fep = addrel (addr (forms_elements (iod_forms_info_tab.n_words)), 1);
	if fep ^= null then
	     element_common.next_element_index = iod_forms_info_tab.n_words + 1;
	else forms_groups (iod_forms_info_tab.n_groups).first_element_index = iod_forms_info_tab.n_words + 1;
	fep = temp_fep;

/* now clear out the entry */
	unspec (element_common) = "0"b;
	element_common.n_names = 1;
	call store_token_value_string (element_common.names (1));
	element_common.type,
	     element_common.next_element_index = -1;
	element_common.comment.first_char,
	     element_common.comment.total_chars = 0;
	forms_element_validated = FALSE;

     end forms_create_element_entry;

/* create a new entry in the group table */
forms_create_group_entry: proc;

/* validate the last forms group if there is one */
	call forms_validate_group;

/* first see if line is currently defined in the table */
	do i = 1 to iod_forms_info_tab.n_groups		/* is forms group already defined */
	     while (forms_groups (i).name ^= token_value);
	end;
	if i <= iod_forms_info_tab.n_groups then	/* it is in table so complain */
	     call statement_error (10, "Forms_table", token_value);

/* Create entry in table for this line name.
   We will create a new entry for a duplicate name so we can check the rest
   of the substatements */
	iod_forms_info_tab.n_groups = iod_forms_info_tab.n_groups + 1;

/* now initialize the entry */
	forms_groups (i).name = token_value;
	forms_groups (i).comment.first_char,
	     forms_groups (i).comment.total_chars = 0;
	forms_groups (i).first_element_index = -1;
	fep = null;

     end forms_create_group_entry;


forms_save_comment: proc;

/* if there is an element entry open, save the comment there;
   else save in the group entry */
	if fep ^= null () then do;
	     if element_common.comment.first_char ^= 0 then
		call statement_error (13, "comment", "name", return_string (element_common.names (1)));
	     call store_token_value_string (element_common.comment);
	end;
	else do;
	     if forms_groups (iod_forms_info_tab.n_groups).comment.first_char ^= 0 then
		call statement_error (13, "comment", "Forms_info", forms_groups (iod_forms_info_tab.n_groups).name);
	     call store_token_value_string (forms_groups (iod_forms_info_tab.n_groups).comment);
	end;

     end forms_save_comment;


forms_save_size: proc (type);

dcl  type fixed bin;
dcl  type_strings (5) char (11) int static options (constant) init
	("page_height", "page_width", "char_height", "char_width", "line_height");

dcl  got_dupe bit (1);

	got_dupe = FALSE;
	if type = 1 | type = 3 | type = 5 then do;
	     if orientation_element.height ^= 0 then
		got_dupe = TRUE;
	     orientation_element.height = forms_size_info;
	     orientation_element.factors (1) = forms_size_type;
	end;
	if type = 2 | type = 4 then do;
	     if orientation_element.width ^= 0 then
		got_dupe = TRUE;
	     orientation_element.width = forms_size_info;
	     orientation_element.factors (2) = forms_size_type;
	end;

	if got_dupe then
	     call statement_error (13, type_strings, "name", return_string (element_common.names (1)));

     end forms_save_size;

forms_save_string: proc;

dcl  got_dupe bit (1);
dcl  use_token bit (1);

	use_token = "0"b;
	go to forms_save_string_common;

forms_save_string_token: entry;

	use_token = "1"b;

forms_save_string_common:
	got_dupe = "0"b;
	if element_common.type = TYPE_ORIENTATION	/* orientation */
	     | element_common.type = TYPE_FONT_DESC	/* font desc */
	     | element_common.type = TYPE_FONT_SIZE
	     | element_common.type = TYPE_LINE_DESC	/* line desc */
	     | element_common.type = TYPE_HOLES
	then do;
	     if orientation_element.escape_string.total_chars ^= 0 then
		got_dupe = "1"b;
	     if use_token then
		call store_token_value_string (orientation_element.escape_string);
	     else call store_forms_escape_string (orientation_element.escape_string);
	end;

	else if element_common.type = TYPE_SPECIAL then do; /* special */
	     if special_element.special_string.total_chars ^= 0 then
		got_dupe = "1"b;
	     if use_token then
		call store_token_value_string (special_element.special_string);
	     else call store_forms_escape_string (special_element.special_string);
	end;

	else if element_common.type = TYPE_FONT_NAME
	     | element_common.type = TYPE_PREAMBLE
	     | element_common.type = TYPE_POSTAMBLE then do;
	     if font_name_element.escape_string.total_chars ^= 0 then
		got_dupe = "1"b;
	     if use_token then
		call store_token_value_string (font_name_element.escape_string);
	     call store_forms_escape_string (font_name_element.escape_string);
	end;
	else if element_common.type = TYPE_USES then do;
	end;

	if got_dupe then
	     call statement_error (13, "string", "name ", return_string (element_common.names (1)));

     end forms_save_string;


forms_save_type: proc;

	if element_common.type ^= -1 then do;
	     call statement_error (13, "type", "name", return_string (element_common.names (1)));
	     return;
	end;

	element_common.type = token.Nvalue;

     end forms_save_type;


forms_save_uses_name: proc;

	if element_common.type ^= -1 then
	     call statement_error (13, "uses", "name", return_string (element_common.names (1)));
	element_common.type = TYPE_USES;
	uses_element.n_indices = 0;

forms_save_uses_name_continue: entry;

	uses_element.n_indices = uses_element.n_indices + 1;
	call store_token_value_string (uses_element.name (uses_element.n_indices));
	uses_element.index (uses_element.n_indices) = -1;

     end forms_save_uses_name;

/* scan through the given forms table looking for specific element name */
forms_scan_for_element_name: proc (starting_index, seek_name) returns (fixed bin (17));

dcl  starting_index fixed bin;
dcl  seek_name char (*);

dcl  element_index fixed bin;
dcl  element_name char (32);
dcl  i fixed bin;
dcl  my_fep ptr;

	element_index = starting_index;
	do while (element_index ^= -1);
	     my_fep = addr (forms_elements (element_index));
	     do i = 1 to my_fep -> element_common.n_names;
		element_name = return_string (my_fep -> element_common.names (i));
		if element_name = rtrim (seek_name) then
		     go to return_element_index;
	     end;
	     element_index = my_fep -> element_common.next_element_index;
	end;
return_element_index:
	return (element_index);

     end forms_scan_for_element_name;
%page;
/* validate that the current element entry isn't missing any required items */
forms_validate_entry: proc;

dcl  element_words fixed bin;

	if fep = null then return;			/* no current element */
	if forms_element_validated then return;		/* already validated this element */

/* validate the element entry */
	if element_common.type = -1 then do;
	     call semant_error (19, "type", "name", return_string (element_common.names (1)));
	     element_words = currentsize (element_common);
	end;
	else do;
	     if element_common.type = TYPE_USES then do;	/* check out "uses" */
		if uses_element.n_indices = 0 then
		     call semant_error (19, "uses", "name", return_string (element_common.names (1)));
		element_words = currentsize (uses_element);
	     end;
	     else if element_common.type = TYPE_ORIENTATION then do; /* check out "orientation" */
		if orientation_element.height = 0 then
		     call semant_error (19, "page_height", "name", return_string (element_common.names (1)));
		if orientation_element.width = 0 then
		     call semant_error (19, "page_width", "name", return_string (element_common.names (1)));
		if orientation_element.escape_string.total_chars = 0 then
		     call semant_error (19, "string", "name", return_string (element_common.names (1)));
		element_words = currentsize (orientation_element);
	     end;
	     else if element_common.type = TYPE_FONT_DESC then do; /* check out "font_name" */
		if font_element.height = 0 then
		     call semant_error (19, "char_height", "name", return_string (element_common.names (1)));
		if font_element.width = 0 then
		     call semant_error (19, "char_width", "name", return_string (element_common.names (1)));
		if font_element.escape_string.total_chars = 0 then
		     call semant_error (19, "string", "name", return_string (element_common.names (1)));
		element_words = currentsize (font_element);
	     end;
	     else if element_common.type = TYPE_FONT_SIZE then do; /* check out "font_name" */
		if font_size_element.height = 0 then
		     call semant_error (19, "char_height", "name", return_string (element_common.names (1)));
		if font_size_element.width = 0 then
		     call semant_error (19, "char_width", "name", return_string (element_common.names (1)));
		if font_size_element.escape_string.total_chars = 0 then
		     call semant_error (19, "string", "name", return_string (element_common.names (1)));
		element_words = currentsize (font_size_element);
	     end;
	     else if element_common.type = TYPE_LINE_DESC then do; /* check out "line_height" */
		if line_element.height = 0 then
		     call semant_error (19, "line_height", "name", return_string (element_common.names (1)));
		if line_element.escape_string.total_chars = 0 then
		     call semant_error (19, "string", "name", return_string (element_common.names (1)));
		element_words = currentsize (line_element);
	     end;
	     else if element_common.type = TYPE_HOLES then do; /* check out "font_name" */
		if holes_element.height = 0 & holes_element.width = 0 then
		     call semant_error (52, return_string (element_common.names (1)));
		if holes_element.escape_string.total_chars = 0 then
		     call semant_error (19, "string", "name", return_string (element_common.names (1)));
		element_words = currentsize (holes_element);
	     end;
	     else if element_common.type = TYPE_SPECIAL then do; /* check out "special" */
		if special_element.special_string.total_chars = 0 then
		     call semant_error (19, "string", "name", return_string (element_common.names (1)));
		element_words = currentsize (special_element);
	     end;
	     else if element_common.type = TYPE_FONT_NAME then do;
		if font_name_element.escape_string.total_chars = 0 then
		     call semant_error (19, "string", "name", return_string (element_common.names (1)));
		element_words = currentsize (font_name_element);
	     end;
	     else if element_common.type = TYPE_PREAMBLE then do;
		element_words = currentsize (preamble_element);
	     end;
	     else if element_common.type = TYPE_POSTAMBLE then do;
		element_words = currentsize (postamble_element);
	     end;
	end;
	iod_forms_info_tab.n_words = iod_forms_info_tab.n_words + element_words;
	forms_element_validated = TRUE;

     end forms_validate_entry;


/* validate the current group of forms elements */
forms_validate_group: proc;

dcl  (element_index, starting_element_index) fixed bin;
dcl  my_fep ptr;
dcl  i fixed bin;

/* nothing to do if no groups defined yet */
	if iod_forms_info_tab.n_groups = 0 then return;

/* first validate the last element entry if there is one */
	call forms_validate_entry;

/* now validate all the forms elements within this group */
	if forms_groups (iod_forms_info_tab.n_groups).first_element_index = -1 then do;
	     call semant_error (18, "forms elements", "Forms_table", forms_groups (iod_forms_info_tab.n_groups).name);
	end;
	else do;
	     element_index,
		starting_element_index = forms_groups (iod_forms_info_tab.n_groups).first_element_index;
	     do while (element_index ^= -1);
		my_fep = addr (forms_elements (element_index));
		if my_fep -> element_common.type = TYPE_USES then do;
		     do i = 1 to my_fep -> uses_element.n_indices;
			my_fep -> uses_element.index (i) = forms_scan_for_element_name (starting_element_index,
			     return_string (my_fep -> uses_element.name (i)));
			if my_fep -> uses_element.index (i) = -1 then
			     call semant_error (12, "uses element", my_fep -> uses_element.name (i), "name", return_string (my_fep -> element_common.names (1)));
		     end;
		end;
		element_index = my_fep -> element_common.next_element_index;
	     end;
	end;

     end forms_validate_group;


append_token_value_string: proc;

dcl  i fixed bin;
dcl  c char (1);

	do i = 1 to token.Lvalue;
	     c = substr (token_value, i, 1);
	     call insert_single_char (rank (c));
	end;
	return;

     end append_token_value_string;

insert_single_char: proc (n);

dcl  n fixed bin (18);

	forms_escape_string_n (forms_escape_string_index) = n;
	forms_escape_string_index = forms_escape_string_index + 1;
	return;

     end insert_single_char;
%page;

statement_error: proc options (variable);

dcl  arg_count fixed bin;
dcl  arg_lens (7) fixed bin (21);
dcl  arg_ptrs (7) ptr;
dcl  error_num fixed bin based (arg_ptrs (1));
dcl  i fixed bin;
dcl  parm1 char (arg_lens (2)) based (arg_ptrs (2));
dcl  parm2 char (arg_lens (3)) based (arg_ptrs (3));
dcl  parm3 char (arg_lens (4)) based (arg_ptrs (4));
dcl  parm4 char (arg_lens (5)) based (arg_ptrs (5));
dcl  parm5 char (arg_lens (6)) based (arg_ptrs (6));
dcl  parm6 char (arg_lens (7)) based (arg_ptrs (7));
dcl  dummy_string char (1);
dcl  (stmt_ptr, token_ptr) ptr;

	stmt_ptr = token.Pstmt;
	token_ptr = Pthis_token;
	go to error_common;

/* semant_error: entry (error_num, parm1, ..., parm6); */
semant_error: entry options (variable);

	stmt_ptr, token_ptr = null;

error_common: arg_lens (*) = 0;
	arg_ptrs (*) = addr (dummy_string);

	call cu_$arg_count (arg_count, code);
	do i = 1 to arg_count;
	     call cu_$arg_ptr (i, arg_ptrs (i), arg_lens (i), code);
	end;

	if error_control_table (error_num).severity >= MIN_PRINT_SEVERITY
	then call lex_error_ (error_num, SERROR_PRINTED (error_num), (error_control_table.severity (error_num)),
		MERROR_SEVERITY, stmt_ptr, token_ptr, SERROR_CONTROL,
		(error_control_table.message (error_num)), (error_control_table.brief_message (error_num)),
		parm1, parm2, parm3, parm4, parm5, parm6);
	else do;
	     MERROR_SEVERITY = max (MERROR_SEVERITY, error_control_table (error_num).severity);
	     SERROR_PRINTED (error_num) = TRUE;
	end;
	return;

     end statement_error;
%page;

dcl  1 error_control_table (53) aligned int static options (constant),

       2 severity fixed bin (17) unal init (
	  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	  3, 3, 3, 1, 2, 4, 3, 2, 3, 3,
	  3, 3, 3, 3, 3, 3, 3, 1, 1, 3,
	  3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	  3, 3, 3, 3, 1, 1, 3, 3, 3, 3,
	  3, 3, 3
	  ),

       2 Soutput_stmt bit (1) unal init (
	  "0"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b,
	  "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "0"b, "0"b, "1"b,
	  "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "0"b, "0"b, "0"b,
	  "1"b, "1"b, "0"b, "0"b, "0"b, "0"b, "0"b, "0"b, "0"b, "0"b,
	  "0"b, "0"b, "0"b, "0"b, "0"b, "0"b, "1"b, "1"b, "1"b, "1"b,
	  "1"b, "1"b, "0"b
	  ),

       2 message char (120) varying init (
	  "End of input encountered before End statement.", /*  1 */
	  "Keyword ""^a"" unrecognized or out of order.", /*  2 */
	  "Syntax error.",				/*  3 */
	  "Global ""^a"" keyword appears more than once.",/*  4 */
	  "Bad numeric parameter (^a).",		/*  5 */
	  "^a must be between 1 and ^a.",		/*  6 */
	  "Grace time must be positive.",		/*  7 */
	  "The name ""^a"" contains an invalid char.",	/*  8 */
	  "Name ""^a"" is too long.",			/*  9 */
	  "^a ""^a"" has already been defined.",	/* 10 */
	  "^a ""^a"" has not been defined.",		/* 11 */
	  "The ^a ""^a"" specified for ""^a: ^a;"" has not been defined.", /* 12 */
	  "Keyword ""^a"" appears more than once for ""^a: ^a;"".", /* 13 < */
	  "Channel not found in CDT.  Possible error.",	/* 14 */
	  "Data for ""^a"" exceeds ^a chars.",		/* 15 */
	  "Limit of ^a ^a exceeded.  Translation aborted.", /* 16 */
	  "The ^a device has been specified more that once for ^a.", /* 17 */
	  "No ^a specified for ""^a: ^a""",		/* 18 */
	  "No ^a keyword found for ""^a: ^a;"".",	/* 19 */
	  "Specified driver userid doesn't contain project name.", /* 20 */
	  "Specified driver userid contains more than two components.", /* 21 */
	  "Two of the mutually exclusive prph, line and dial_id keywords have been specified for device ""^a"".", /* 22 */
	  "Specified ""^a: ^a;"" for Device ""^a"" matches a previous dial_id or ctl_dial_id.", /* 23 */
	  "Two of the mutually exclusive ctl_line, ctl_dial_id and ctl_source keywords have been specified for device ""^a"".", /* 24 */
	  "Device ""^a"" does not have the ""line: variable;"" statement needed for line ""^a"".", /* 25 */
	  "Device ""^a"" uses a ""line: variable;"" statement, but has no corresponding Line entry.", /* 26 */
	  "The full name of ^a ""^a.^a"" exceeds 32 characters.", /* 27 */
	  "Non-system accounting specified for IO.SysDaemon in Request_type ""^a"".", /* 28 */
	  "System accounting specified for other than *.SysDaemon in Request_type ""^a"".", /* 29 */
	  "",					/* 30 */
	  "One or more device class subkeywords appeared before the first device_class keyword for ""Request_type: ^a;"".", /* 31 */
	  "^a name ""^a"" appears more than once for ^a ""^a"".", /* 32 */
	  "The device ""^a"" specified for ""^a: ^a;"" is missing a minor device component.", /* 33 */
	  "The device ""^a"" specified for ""^a: ^a;"" has not been defined.", /* 34 */
	  "The default_type ""^a"" specified for ""^a: ^a;"" is missing a minor device component.", /* 35 */
	  "The default_type ""^a"" has been specified for ""^a: ^a;"" but device ""^a"" has not been specified for ""^a: ^a;"".", /* 36 */
	  "The default_type ""^a"" specified for ""^a: ^a;"" has not been defined.", /* 37 */
	  "Specified default_queue is greater than max_queues for Request_type ""^a"".", /* 38 */
	  "There must be one price name defined for each possible queue.  Request_type ""^a"".", /* 39 */
	  "Request_type ""^a"".  There is no price defined for ""^a"" in the system price table.", /* 40 */
	  "Max_queues not specified.",		/* 41 */
	  "Time has not been specified.",		/* 42 */
	  "No Device keywords have been specified.",	/* 43 */
	  "No Request_type keywords have been specified.",/* 44 */
	  "No default Request_type has been specified for the ""printer"" generic type.", /* 45 */
	  "No default Request_type has been specified for the ""punch"" generic type.", /* 46 */
	  "No driver_module has been specified for Device ""^a"".", /* 47 */
	  "No prph, line or dial_id has been specified for Device ""^a"".", /* 48 */
	  "Keyword ""^a"" appears more than once for ""^a: ^a;"", ""^a: ^a;"".", /* 49 */
	  "The ""name: ^a;"" statement has already been used in ""Forms_table: ^a;"".", /* 50 */
	  "The ^a statement has too many elements, only ^a allowed.", /* 51 */
	  "Either page_height and/or page_width must be specified for ""name: ^a"".", /* 52 */
	  "Request_type ""^a"" cannot be used by Device ""^a"" since Device entry is missing a forms_table entry." /* 53 */
	  ),

       2 brief_message char (36) varying init (
	  "",					/*  1 */
	  "^a",					/*  2 */
	  "",					/*  3 */
	  "^a",					/*  4 */
	  "^a",					/*  5 */
	  "^a",					/*  6 */
	  "",					/*  7 */
	  "^a",					/*  8 */
	  "^a",					/*  9 */
	  "^a ^a",				/* 10 */
	  "^a ^a",				/* 11 */
	  "^a of ""^a"" for ""^a: ^a"" not defined.",	/* 12 */
	  "Multiple ^a for ""^a: ^a;""",		/* 13 */
	  "",					/* 14 */
	  "^a too long",				/* 15 */
	  "",					/* 16 */
	  "^a ^a",				/* 17 */
	  "^a ""^a: ^a""",				/* 18 */
	  "^a ""^a: ^a;""",				/* 19 */
	  "",					/* 20 */
	  "",					/* 21 */
	  "^a",					/* 22 */
	  """^a: ^a"" ""^a""",			/* 23 */
	  "^a",					/* 24 */
	  """^a"" for Line ""^a"".",			/* 25 */
	  """^a"" for Line ""^a"".",			/* 26 */
	  "^a name ""^a.^a"".",			/* 27 */
	  """^a""",				/* 28 */
	  """^a""",				/* 29 */
	  "",					/* 30 */
	  "^a ""^a""",				/* 31 */
	  "^a ""^a"" ^a ""^a""",			/* 32 */
	  """^a"" ""^a:^a;""",			/* 33 */
	  """^a"" ""^a:^a;""",			/* 34 */
	  """^a"" ""^a:^a;""",			/* 35 */
	  "^a ""^a: ^a;"" ^a ""^a: ^a;""",		/* 36 */
	  "^a ""^a: ^a;""",				/* 37 */
	  """^a""",				/* 38 */
	  """^a""",				/* 39 */
	  """^a"" ""^a""",				/* 40 */
	  "",					/* 41 */
	  "",					/* 42 */
	  "",					/* 43 */
	  "",					/* 44 */
	  "",					/* 45 */
	  "",					/* 46 */
	  """^a""",				/* 47 */
	  """^a""",				/* 48 */
	  """^a"" ""^a: ^a;"" ""^a: ^a;""",		/* 49 */
	  """name: ^a;"" ""Forms_table: ^a;""",		/* 50 */
	  "^a",					/* 51 */
	  "name: ^a",				/* 52 */
	  "Request_type ""^a"", Device ""^a"""		/* 53 */
	  );

%page; %include access_mode_values;
%page; %include device_class;
%page; %include iod_forms_info_tab;
%page; %include iod_constants;
%page; %include iod_device_tab;
%page; %include iod_line_tab;
%page; %include iod_tables_hdr;
%page; %include q_group_tab;
%page; %include terminate_file;
