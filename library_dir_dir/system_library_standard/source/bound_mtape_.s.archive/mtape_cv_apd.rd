/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */

/* DESCRIPTION:

   This command takes an argument processing definition (APD)
   from a source file and translates the information specified
   by that source into a form readily useable by the argument
   processing routine process_arguments_.  The result, called
   a print vector array, is stored into a value segment
   specified by the user.

   The information in the source file must be specified in the
   APD language whose grammar is described elsewhere.

   The syntax of the command is as follows:

        mtape_cv_apd {-source} path {{-target} path} {-control_arg}

   where -control_arg may be -replace or -no_replace and designates
   whether or not to replace the APD if it already exists in the
   target database.

   The source file must have the "mapd" suffix and the target file
   must be a value segment with the "value" suffix.

   The result print_vector_array contains print_vectors of the following 
   composition:

        1) Initial vector:  definition order *
		        command name *
		        default linear form
		        initial implied option
		        explanation
		        validate result
		        validate result explanation

        2) Option vector:  definition order *
		       command name *
		       option *
		       initial argument
		       next implied option
		       excluded option
		       unexcluded option
		       presence required		/~* takes no value *~/

        3) Option name vector:  definition order *
		            command name *
		            option *
		            synonym *
		            negative form		/~* takes no value *~/

        4) Argument vector:  definition order *
		         command name *
		         option *
		         argument *
		         next argument
		         presence required		/~* takes no value *~/
		         validation string
		         default value
		         negative value
		         explanation

		  
      *  Required dimensions

   Note that this program does no validity or consistency checking of the
   info that the user feeds it.  This should be added in the future.

*/

/* HISTORY:

Written by S. Krupp, 03/01/83.
Modified:
07/25/83 by Lindsey Spratt:  Changed to put the definition in  a value segment
	  instead of a vector db.  The default linear form is now stored
	  separately in the value seg, rather than as part of the definition
	  pva.
08/05/83 by S. Krupp:  Changed name from cvapd to mtape_cv_apd.  Changed
            source segment suffix from "cvapd" to "mapd".

08/26/83 by S. Krupp:  Added the "Force_literal" and "Validate_explanation"
	  statements.
*/

%page;
/*++

INCLUDE NEXT_STMT \
INCLUDE ERROR \

BEGIN
initial_definition
          / Program_name :
	     / LEX(2) PUSH(initial_stmt_list)
	     / program_name_stmt                         \
          / <any-token>
               / ERROR(2) NEXT_STMT
	     / initial_stmt_list                         \
          / <no-token>
               / ERROR(3)
	     / error_return                              \

initial_stmt_list
          / Explanation :
	     / LEX(2) PUSH(initial_stmt_list)
               / explanation_stmt                          \
          / Default_linear_form :
               / LEX(2) PUSH(initial_stmt_list)
	     / default_linear_form_stmt                  \
          / Initial_implied_option :
               / LEX(2) PUSH(initial_stmt_list)
	     / initial_implied_option_stmt               \
          / Validate_result :
               / LEX(2) PUSH(initial_stmt_list)
	     / validate_result_stmt                      \
          / Validate_result_explanation :
               / LEX(2) PUSH(initial_stmt_list)
	     / validate_result_explanation_stmt          \
          / <any-token>
               /
	     / end_or_opt                                \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

end_or_opt
          / End ;
               / NEXT_STMT
	     / return                                    \
          / Option :
	     /
	     / option_definition                         \
          / <any-token>
               / ERROR(10) NEXT_STMT
               / initial_stmt_list                         \

option_definition
          / Option :
               / LEX(2) PUSH(per_option_stmt_list)
	     / option_stmt                               \

per_option_stmt_list
          / Option_name :
               / LEX(2) PUSH(per_option_stmt_list)
	     / option_name_stmt                          \
          / First_argument :
               / LEX(2) PUSH(per_option_stmt_list)
	     / first_argument_stmt                       \
          / Antonym :
               / LEX(2) PUSH(per_option_stmt_list)
	     / antonym_stmt                              \
          / Explanation :
               / LEX(2) PUSH(per_option_stmt_list)
	     / explanation_stmt                          \
          / Exclude :
               / LEX(2) PUSH(per_option_stmt_list)
               / exclude_stmt                              \
          / Unexclude :
               / LEX(2) PUSH(per_option_stmt_list)
	     / unexclude_stmt                            \
          / Presence :
               / LEX(2) PUSH(per_option_stmt_list)
	     / presence_stmt                             \
          / Next_implied_option :
               / LEX(2) PUSH(per_option_stmt_list)
	     / next_implied_option_stmt                  \
          / <any-token>
               /
	     / per_opt_end_or_opt_or_arg                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

per_opt_end_or_opt_or_arg
          / End ;
               / [if default_exclude_myself
	        then call add_to_vector(option, EXCLUDED_OPTION)]
	       NEXT_STMT
	     / return                                    \
          / Option :
               / [if default_exclude_myself
	        then call add_to_vector(option, EXCLUDED_OPTION)]
	     / option_definition                         \
          / Argument :
               / [if default_exclude_myself
	        then call add_to_vector(option, EXCLUDED_OPTION)]
	     / argument_definition                       \
          / <any-token>
               / ERROR(11) NEXT_STMT
	     / per_option_stmt_list                      \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

argument_definition
          / Argument :
               / LEX(2) PUSH(per_argument_stmt_list)
	     / argument_stmt                             \

per_argument_stmt_list
          / Validate :
	     / LEX(2) PUSH(per_argument_stmt_list)
	     / validate_stmt                             \
          / Validate_explanation :
               / LEX(2) PUSH(per_argument_stmt_list)
	     / validate_explanation_stmt                 \
          / Default_value :
               / LEX(2) PUSH(per_argument_stmt_list)
	     / default_value_stmt                        \
          / Antonym_value :
               / LEX(2) PUSH(per_argument_stmt_list)
	     / antonym_value_stmt                        \
          / Presence :
               / LEX(2) PUSH(per_argument_stmt_list)
	     / presence_stmt                             \
          / Next_argument :
               / LEX(2) PUSH(per_argument_stmt_list)
               / next_argument_stmt                        \
          / Explanation :
               / LEX(2) PUSH(per_argument_stmt_list)
	     / explanation_stmt                          \
          / Force_literal :
               / LEX(2) PUSH(per_argument_stmt_list)
	     / force_literal_stmt                        \
          / <any-token>
               /
	     / per_arg_end_or_opt_or_arg                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

per_arg_end_or_opt_or_arg
          / End ;
               / NEXT_STMT
	     / return                                    \
          / Option :
               /
	     / option_definition                         \
          / Argument :
               /
	     / argument_definition                       \
          / <any-token>
               / ERROR(13) NEXT_STMT
	     / per_argument_stmt_list                    \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

cum_quoted_comma_list
cum_quoted_comma_list_next
          / ,
	     / ERROR(12) NEXT_STMT
	     / STACK_POP                                 \
          / ;
               / ERROR(12) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \
          / <quoted-string>
               / append_to_str(token_value, expanded_token_len, expanded_token_ptr)
	       LEX(1)
	     / cum_quoted_comma_list_punc                \
          / <any-token>
               / ERROR(12) NEXT_STMT
	     / STACK_POP                                 \
  cum_quoted_comma_list_punc
          / ,
               / LEX(1)
	     / cum_quoted_comma_list_next                \
          / ;
               / [if list_dim = DEFAULT_LINEAR_FORM
	        then call set_name (expanded_token, default_linear_form_length,
		   default_linear_form_ptr);
	        else call add_to_vector(expanded_token, list_dim);
	        call reset_name(expanded_token_len, expanded_token_ptr)]
                 NEXT_STMT
               / STACK_POP                                 \
          / <any-token>
               / ERROR(12) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

comma_list
comma_list_next
          / ,
               / ERROR(12) NEXT_STMT
	     / STACK_POP                                 \
          / ;
               / ERROR(12) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \
          / <any-token>
               / add_to_vector(token_value, list_dim) LEX(1)
	     /                                           \
          / ,
               / LEX(1)
	     / comma_list_next                           \
          / ;
               / NEXT_STMT
	     / STACK_POP                                 \
          / <any-token>
               / ERROR(12) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

return
          / <any-token>
               / ERROR(5)
	     / RETURN                                    \
          / <no-token>
               /
	     / RETURN                                    \

error_return
          / <any-token>
               /
	     / RETURN                                    \
          / <no-token>
               /
	     / RETURN                                    \

stmts
program_name_stmt
          / <any-token> ;
               / [call set_name(token_value, command_name_len, command_name_ptr);
	        call start_vector(INITIAL_VECTOR)] NEXT_STMT
               / STACK_POP                                 \
          / <any-token>
               / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

explanation_stmt
          / <any-token>
               / [list_dim = EXPLANATION]
	     / cum_quoted_comma_list                     \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

default_linear_form_stmt
          / <any-token>
               / [list_dim = DEFAULT_LINEAR_FORM]
	     / cum_quoted_comma_list                     \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

initial_implied_option_stmt
          / <any-token> ;
               / add_to_vector(token_value, INITIAL_IMPLIED_OPTION) NEXT_STMT
	     / STACK_POP                                 \
          / <any-token>
               / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

option_stmt
          / <any-token> ;
               / [call set_name(token_value, option_len, option_ptr);
	        call reset_name(argument_len, argument_ptr);
	        call start_vector(OPTION_VECTOR);
	        default_exclude_myself = "1"b]
	       NEXT_STMT
	     / STACK_POP                                 \
          / <any-token>
               / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

option_name_stmt
          / <any-token>
               / [list_dim = SYNONYM]
	     / comma_list                                \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

first_argument_stmt
          / <any-token> ;
               / add_to_vector(token_value, INITIAL_ARGUMENT) NEXT_STMT
	     / STACK_POP                                 \
          / <any-token>
               / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

antonym_stmt
          / <any-token>
               / [list_dim = NEGATIVE_FORM]
	     / comma_list                                \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

exclude_stmt
          / <any-token>
               / [list_dim = EXCLUDED_OPTION]
	     / comma_list                                \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

unexclude_stmt
          / <any-token>
               / [list_dim = UNEXCLUDED_OPTION]
	     / comma_list                                \
          / <no-token>
               / ERROR(4)
               / error_return                              \

presence_stmt
          / required ;
               / add_to_vector(token_value, PRESENCE_REQUIRED) NEXT_STMT
	     / STACK_POP                                 \
          / literal_required ;
               / add_to_vector(token_value, PRESENCE_REQUIRED) NEXT_STMT
	     / STACK_POP                                 \
          / optional ;
               / NEXT_STMT
	     / STACK_POP                                 \
          / <decimal-integer> ;
               / add_to_vector(token_value, PRESENCE_REQUIRED) NEXT_STMT
	     / STACK_POP                                 \
          / <any-token>
               / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

next_implied_option_stmt
          / <any-token> ;
               / add_to_vector(token_value, NEXT_IMPLIED_OPTION) NEXT_STMT
               / STACK_POP                                 \
          / <any-token>
               / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

argument_stmt
          / <any-token> ;
               / [call set_name(token_value, argument_len, argument_ptr);
	        call start_vector(ARGUMENT_VECTOR)] NEXT_STMT
	     / STACK_POP                                 \
          / <any-token>
               / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

validate_stmt
          / <any-token>
               / [list_dim = VALIDATION_STRING]
	     / cum_quoted_comma_list                     \
          / <no-token>
               / ERROR(4)
	     / error_return                              \


default_value_stmt
          / <any-token>
               / [list_dim = DEFAULT_VALUE]
	     / cum_quoted_comma_list                     \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

antonym_value_stmt
          / <any-token>
               / [list_dim = NEGATIVE_VALUE]
	     / cum_quoted_comma_list                     \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

next_argument_stmt
          / <any-token> ;
               / add_to_vector(token_value, NEXT_ARGUMENT) NEXT_STMT
	     / STACK_POP                                 \
          / <any-token>
               / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

validate_result_stmt
          / <any-token>
               / [list_dim = VALIDATE_RESULT]
	     / cum_quoted_comma_list                     \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

validate_result_explanation_stmt
          / <any-token>
               / [list_dim = VALIDATE_RESULT_EXPLANATION]
	     / cum_quoted_comma_list                     \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

validate_explanation_stmt
          / <any-token>
	     / [list_dim = VALIDATE_EXPLANATION]
	     / cum_quoted_comma_list                     \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

force_literal_stmt
          / ;
	     / add_to_vector("", FORCE_LITERAL) NEXT_STMT
	     / STACK_POP                                 \
          / <any-token>
	     / ERROR(1) NEXT_STMT
	     / STACK_POP                                 \
          / <no-token>
               / ERROR(4)
	     / error_return                              \

++*/
%page;
mtape_cv_apd: proc ();

/* Automatic */

	dcl     answer		 char (3) var;
	dcl     area_ptr		 ptr;
	dcl     arg_len		 fixed bin (21);
	dcl     arg_ptr		 ptr;
	dcl     argument_len	 fixed bin (21);
	dcl     argument_ptr	 ptr;
	dcl     bit_count		 fixed bin (24);
	dcl     code		 fixed bin (35);
	dcl     command_name_len	 fixed bin (21);
	dcl     command_name_ptr	 ptr;
	dcl     created_value_seg	 bit (1) aligned init ("0"b);
	dcl     default_exclude_myself bit (1) aligned;
	dcl     default_linear_form_length fixed bin (21) init (0);
	dcl     default_linear_form_ptr ptr init (null);
	dcl     default_linear_form_value_name char (128) varying init ("");
	dcl     definition_exists	 bit (1) aligned init ("0"b);
	dcl     definition_order	 pic "999";
	dcl     definition_string_length fixed bin (35) init (0);
	dcl     definition_string_ptr	 ptr init (null);
          dcl     dlf_var_length	 fixed bin(21);
          dcl     dlf_var_ptr		 ptr;
	dcl     definition_value_name	 char (128) varying init ("");
	dcl     error_code_array	 (1) fixed bin (35);
	dcl     expanded_token_len	 fixed bin (21);
	dcl     expanded_token_ptr	 ptr;
	dcl     i			 fixed bin;
	dcl     list_dim		 fixed bin;
	dcl     main_pv_num		 fixed bin;
	dcl     main_pv_type	 fixed bin;
	dcl     n_definition_order	 fixed bin;
	dcl     nargs		 fixed bin;
	dcl     option_len		 fixed bin (21);
	dcl     option_ptr		 ptr;
	dcl     Pfirst_stmt_desc	 ptr;
	dcl     Pfirst_token_desc	 ptr;
	dcl     replace		 bit (1) aligned;
	dcl     sdname		 char (168);
	dcl     seg_len		 fixed bin (21);
	dcl     seg_ptr		 ptr;
	dcl     sename		 char (32);
	dcl     source_rpath_len	 fixed bin (21);
	dcl     source_rpath_ptr	 ptr;
	dcl     synonym_len		 fixed bin (21);
	dcl     synonym_ptr		 ptr;
	dcl     target_rpath_len	 fixed bin (21);
	dcl     target_rpath_ptr	 ptr;
	dcl     tdname		 char (168);
	dcl     temp_seg_ptr	 ptr;
	dcl     tename		 char (32);
	dcl     value_defined	 (22) bit (1);	/* *** True means that DIMENSIONS(i) in the current */
						/* print_vector has a value. */
	dcl     value_seg_ptr	 ptr init (null);

	dcl     1 auto_area_info	 like area_info;
	dcl     1 auto_query_info	 like query_info;

/* Based */

	dcl     arg		 char (arg_len) based (arg_ptr);
	dcl     argument		 char (argument_len) based (argument_ptr);
	dcl     based_area		 area based (area_ptr);
	dcl     command_name	 char (command_name_len) based (command_name_ptr);
	dcl     default_linear_form_string char (default_linear_form_length) based (default_linear_form_ptr);
          dcl     dlf_var                char(dlf_var_length) var based(dlf_var_ptr);
	dcl     expanded_token	 char (expanded_token_len) based (expanded_token_ptr);
	dcl     option		 char (option_len) based (option_ptr);
	dcl     source_rpath	 char (source_rpath_len) based (source_rpath_ptr);
	dcl     synonym		 char (synonym_len) based (synonym_ptr);
	dcl     target_rpath	 char (target_rpath_len) based (target_rpath_ptr);

/* Builtin */

	dcl     (addr, after, dimension, divide, empty, length,
	        null, reverse, rtrim, string, unspec) builtin;

/* Entries */

	dcl     command_query_	 entry () options (variable);
	dcl     cu_$arg_count	 entry (fixed bin, fixed bin (35));
	dcl     cu_$arg_ptr		 entry (fixed bin, ptr, fixed bin (21), fixed bin (35));
	dcl     com_err_		 entry () options (variable);
	dcl     define_area_	 entry (ptr, fixed bin (35));
	dcl     expand_pathname_	 entry (char (*), char (*), char (*), fixed bin (35));
	dcl     expand_pathname_$add_suffix entry (char (*), char (*), char (*), char (*), fixed bin (35));
	dcl     get_wdir_		 entry () returns (char (168));
	dcl     hcs_$initiate_count	 entry (char (*), char (*), char (*), fixed bin (24), fixed bin (2), ptr, fixed bin (35));
	dcl     initiate_file_$create	 entry (char (*), char (*), bit (*), ptr, bit (1) aligned, fixed bin (24), fixed bin (35));
	dcl     ioa_		 entry () options (variable);
	dcl     lex_string_$init_lex_delims entry (char (*), char (*), char (*), char (*), char (*), bit (*), char (*) var,
				 char (*) var, char (*) var, char (*) var);
	dcl     lex_string_$lex	 entry (ptr, fixed bin (21), fixed bin (21), ptr, bit (*), char (*), char (*), char (*), char (*),
				 char (*), char (*) var, char (*) var, char (*) var, char (*) var, ptr, ptr, fixed bin (35));
	dcl     suffixed_name_$new_suffix entry (char (*), char (*), char (*), char (32), fixed bin (35));
	dcl     term_$seg_ptr	 entry (ptr, fixed bin (35));
	dcl     terminate_file_	 entry (ptr, fixed bin (24), bit (*), fixed bin (35));
	dcl     translator_temp_$get_next_segment entry (ptr, ptr, fixed bin (35));
	dcl     translator_temp_$get_segment entry (char (*) aligned, ptr, fixed bin (35));
	dcl     translator_temp_$release_all_segments entry (ptr, fixed bin (35));
	dcl     value_$get_data	 entry (ptr, bit (36) aligned, char (*), ptr, ptr, fixed
				 bin (18), fixed bin (35));
	dcl     value_$init_seg	 entry (ptr, fixed bin, ptr, fixed bin (19), fixed bin (35));
	dcl     value_$set_data	 entry (ptr, bit (36) aligned, char (*), ptr, fixed bin (18), ptr, ptr, fixed bin (18),
				 fixed bin (35));
	dcl     value_$delete_data	 entry (ptr, bit (36) aligned, char (*), fixed bin (35));

/* Static */

/* Things marked *** are all related to the dimensions contained in the
     print_vector_array.  If the dimensions change, all of these things should
     be looked at for possible changes.
  */

	dcl     (COMMENT_CLOSE	 char (2) init ("*/"),
	        COMMENT_OPEN	 char (2) init ("/*"),
	        CVAPD_VERSION	 fixed bin init (1),
	        FATAL_ERROR		 fixed bin init (3),
	        FATAL_ERROR_MSG	 char (45) init ("Fatal error has occured.  Translation failed."),
	        FREE_OLD_PV_ARRAY	 bit (1) aligned init ("1"b),
	        IGNORED_INPUT_LEN	 fixed bin (21) init (0),
	        MAX_DIM_NAME_LEN	 fixed bin init (32),    /* *** */
	        MAX_NUM_OF_ARGS	 fixed bin init (5),
	        MIN_NUM_OF_ARGS	 fixed bin init (1),
	        ME		 char (12) init ("mtape_cv_apd"),
	        ME_UPPER		 char (12) init ("MTAPE_CV_APD"),
	        N_INCREMENTAL_PV_SLOTS fixed bin init (10),
	        N_INITIAL_PV_SLOTS	 fixed bin (35) init (10),
	        N_PV_DIMS		 fixed bin init (22), /* *** */
	        NEW_VECTOR		 fixed bin init (-1),
	        NO_COPY		 fixed bin (2) init (1),
	        QUOTE_CLOSE		 char (1) init (""""),
	        QUOTE_OPEN		 char (1) init (""""),
	        SINIT		 bit (2) init ("10"b),
	        SLEX		 bit (4) init ("1000"b),
	        SOURCE_SUFFIX	 char (4) init ("mapd"),
	        STMT_DELIM		 char (1) init (";"),
	        STOP_ON_DUPLICATION	 bit (1) aligned init ("1"b),
	        STOP_ON_ERROR	 bit (1) aligned init ("1"b),
	        USAGE_MSG		 char (66) init ("Usage: mtape_cv_apd {-source} path {{-target} path} {-control_arg}"),
	        VALUE_SUFFIX	 char (5) init ("value")
	        )			 internal static options (constant);

	dcl     (INITIAL_VECTOR	 init (1),
	        OPTION_VECTOR	 init (2),
	        OPTION_NAME_VECTOR	 init (3),
	        ARGUMENT_VECTOR	 init (4),
	        BASIC_VECTOR	 init (5),
	        NEGATIVE_FORM_VECTOR	 init (6)
	        )			 fixed bin internal static options (constant);

	dcl     BYTES_PER_WORD	 init (4) fixed bin internal static options (constant);

	dcl     CAN_HAVE_VALUE	 (22) bit (1) aligned /* *** */
				 init ((11) ("1"b), (1) ("0"b), (9) ("1"b), (1) ("0"b))
				 internal static options (constant);

	dcl     CAN_HAVE_MULTIPLE_DEFINITIONS (22) bit (1) aligned /* *** */
				 init ((9) ("0"b), (3) ("1"b), (5) ("0"b), (3) ("1"b), (2) ("0"b))
				 internal static options (constant);

	dcl     DIMENSIONS		 (22) char (32) var                /* *** */
				 init ("definition order",         /* 1 */
				 "command name",	               /* 2 */
				 "default linear form",            /* 3 */
				 "initial implied option",         /* 4 */
				 "explanation",	               /* 5 */
				 "option",	               /* 6 */
				 "initial argument",               /* 7 */
				 "next implied option",            /* 8 */
				 "presence required",              /* 9 */
				 "excluded option",                /* 10 */
				 "synonym",	               /* 11 */
				 "negative form",	               /* 12 */
				 "argument",	               /* 13 */
				 "next argument",	               /* 14 */
				 "validation string",              /* 15 */
				 "default value",	               /* 16 */
				 "negative value",	               /* 17 */
				 "unexcluded option",              /* 18 */
				 "validate result",                /* 19 */
				 "validate result explanation",    /* 20 */
				 "validate explanation",	     /* 21 */
				 "force literal"		     /* 22 */
				 ) internal static options (constant);

	dcl     (DEFINITION_ORDER	       init (1),	/* *** */
	        COMMAND_NAME	       init (2),
	        DEFAULT_LINEAR_FORM	       init (3),
	        INITIAL_IMPLIED_OPTION       init (4),
	        EXPLANATION		       init (5),
	        OPTION		       init (6),
	        INITIAL_ARGUMENT	       init (7),
	        NEXT_IMPLIED_OPTION	       init (8),
	        PRESENCE_REQUIRED	       init (9),
	        EXCLUDED_OPTION	       init (10),
	        SYNONYM		       init (11),
	        NEGATIVE_FORM	       init (12),
	        ARGUMENT		       init (13),
	        NEXT_ARGUMENT	       init (14),
	        VALIDATION_STRING	       init (15),
	        DEFAULT_VALUE	       init (16),
	        NEGATIVE_VALUE	       init (17),
	        UNEXCLUDED_OPTION	       init (18),
	        VALIDATE_RESULT	       init (19),
	        VALIDATE_RESULT_EXPLANATION  init (20),
	        VALIDATE_EXPLANATION	       init (21),
	        FORCE_LITERAL	       init (22)
	        )			       fixed bin internal static options (constant);

	dcl     PERMANENT_VALUE	 init ("01"b) bit (36) aligned internal static options (constant);
	dcl     PERMANENT_VALUE_SEG_TYPE init (0) fixed bin internal static options (constant);

	dcl     sys_info$max_seg_size	 fixed bin (35) ext static;

	dcl     (error_table_$bad_arg,
	        error_table_$empty_file,
	        error_table_$noarg,
	        error_table_$oldnamerr,
	        error_table_$segknown,
	        error_table_$wrong_no_of_args,
	        error_table_$zero_length_seg
	        )			 fixed bin (35) ext static;

	dcl     (lex_control_chars,
	        lex_delims
	        )			 char (128) var init ("") int static;

	dcl     (cleanup, size)	 condition;

	dcl     BREAK_CHARS		 char (5) var init (":, 	
")
				 internal static options (constant); /* ":", ",", SPACE, TAB, NL */
	dcl     IGNORED_BREAK_CHARS	 char (3) var init (" 	
")
				 internal static options (constant); /* SPACE, TAB, NL */

	dcl     1 error_control_table	 (13) int static options (constant) unaligned,
		2 severity	 fixed bin (17) init ((4) (3), /* 1 - 3 */
				 2, 4, 1, 4, 4,	/* 4 - 8 */
				 (4) (3)),	/* 9 - 13 */
		2 Soutput_stmt	 bit (1) init ((13) ("1"b)),
		2 message		 char (256) var init (
				 "Error in statement.", /* 1 */
				 "Unrecognized statement.  Looking for program_name statement.", /* 2 */
				 "Source file is empty.", /* 3 */
				 "Source file ends unexpectedly.", /* 4 */
				 "Text after end statement.", /* 5 */
				 "Could not append a print vector to the print vector array.", /* 6 */
				 "This value has already been defined.  Ignoring value.", /* 7 */
				 "Could not append a dimension to a print vector.", /* 8 */
				 "Reference made to unknown vector type.", /* 9 */
				 "Unrecognized statement in initial statement list.", /* 10 */
				 "Unrecognized statement in option statement list.", /* 11 */
				 "Bad syntax in list.", /* 12 */
				 "Unrecognized statement in argument statement list."), /* 13 */
		2 brief_message	 char (256) var init ((13) (""));

%page;
/* Include */

%include access_mode_values;
%page;
%include pa_value_names;
%page;
%include vu_print_vector_array;
%page;
%include vu_entry_dcls;
%page;
%include area_info;
%page;
%include terminate_file;
%page;
%include query_info;
%page;
/* Main Procedure */


	call initialize_translator_values ();

	on cleanup call cleanup_trans ();

	call cu_$arg_count (nargs, code);
	if code ^= 0
	then call abort (code, "");

	if nargs = 0
	then call abort (0, USAGE_MSG);

	if nargs < MIN_NUM_OF_ARGS | nargs > MAX_NUM_OF_ARGS
	then call abort (error_table_$wrong_no_of_args, "");

	source_rpath_ptr, target_rpath_ptr = null;
	source_rpath_len, target_rpath_len = 0;
	replace = "0"b;
	i = 1;

	do while (i <= nargs);
	     call get_arg (i, arg_ptr, arg_len);
	     if arg = "-source"
	     then call get_arg (i, source_rpath_ptr, source_rpath_len);
	     else if arg = "-target"
	     then call get_arg (i, target_rpath_ptr, target_rpath_len);
	     else if arg = "-replace" | arg = "-rp"
	     then replace = "1"b;
	     else if arg = "-no_replace" | arg = "-nrp"
	     then replace = "0"b;
	     else if source_rpath_ptr = null
	     then do;
		     source_rpath_ptr = arg_ptr;
		     source_rpath_len = arg_len;
		end;
	     else if target_rpath_ptr = null
	     then do;
		     target_rpath_ptr = arg_ptr;
		     target_rpath_len = arg_len;
		end;
	     else call abort (error_table_$bad_arg, """" || arg || """");
	end;

	if source_rpath_ptr = null
	then call abort (error_table_$noarg, "Source pathname.");

	call expand_pathname_$add_suffix (source_rpath, SOURCE_SUFFIX, sdname, sename, code);
	if code ^= 0
	then call abort (code, """" || source_rpath || """");

	if target_rpath_ptr ^= null
	then do;
		call expand_pathname_$add_suffix (target_rpath, VALUE_SUFFIX, tdname, tename, code);
		if code ^= 0
		then call abort (code, """" || target_rpath || """");
	     end;
	else do;
		tdname = get_wdir_ ();
		call suffixed_name_$new_suffix (sename, SOURCE_SUFFIX, VALUE_SUFFIX, tename, code);
		if code ^= 0 then call abort (code, "Unable to convert the source entry name into a target value segment entry name.");
	     end;

	call hcs_$initiate_count (sdname, sename, "", bit_count, NO_COPY, seg_ptr, code);
	if code = error_table_$segknown
	then code = 0;
	else if code ^= 0
	then call abort (code, rtrim (sdname) || ">" || rtrim (sename));

	seg_len = divide (bit_count + 8, 9, 21, 0);

	call ioa_ ("^a ^3.1f", ME_UPPER, CVAPD_VERSION);

	call translator_temp_$get_segment ((ME), temp_seg_ptr, code);
	if code ^= 0
	then call abort (code, "");

	call translator_temp_$get_next_segment (temp_seg_ptr, area_ptr, code);
	if code ^= 0
	then call abort (code, "");

	call translator_temp_$get_next_segment (area_ptr, definition_string_ptr, code);
	if code ^= 0 then call abort (code, "");
	definition_string_length = sys_info$max_seg_size * 4;

	unspec (auto_area_info) = "0"b;

	auto_area_info.version = 1;
	auto_area_info.extend = "1"b;
	auto_area_info.zero_on_alloc = "1"b;
	auto_area_info.zero_on_free = "0"b;
	auto_area_info.dont_free = "0"b;
	auto_area_info.no_freeing = "0"b;
	auto_area_info.system = "1"b;
	auto_area_info.owner = ME;
	auto_area_info.size = sys_info$max_seg_size;
	auto_area_info.areap = area_ptr;

	call define_area_ (addr (auto_area_info), code);
	if code ^= 0
	then call abort (code, "");

	call vector_util_$init_print_vector_array (area_ptr, N_INITIAL_PV_SLOTS, N_PV_DIMS, MAX_DIM_NAME_LEN, print_vector_array_ptr, code);
	if code ^= 0
	then call abort (code, "");

	print_vector_array.dimension_table.name = DIMENSIONS;

	if lex_delims = ""
	then call lex_string_$init_lex_delims (QUOTE_OPEN, QUOTE_CLOSE,
		COMMENT_OPEN, COMMENT_CLOSE, STMT_DELIM, SINIT,
		BREAK_CHARS, IGNORED_BREAK_CHARS, lex_delims, lex_control_chars);

	call lex_string_$lex (seg_ptr, seg_len, IGNORED_INPUT_LEN,
	     temp_seg_ptr, SLEX, QUOTE_OPEN, QUOTE_CLOSE,
	     COMMENT_OPEN, COMMENT_CLOSE, STMT_DELIM,
	     BREAK_CHARS, IGNORED_BREAK_CHARS, lex_delims, lex_control_chars,
	     Pfirst_stmt_desc, Pfirst_token_desc, code);
	if code = error_table_$zero_length_seg
	then call abort (code, rtrim (sdname) || ">" || rtrim (sename));
	else if code ^= 0
	then do;
		code = 0;
		MERROR_SEVERITY = FATAL_ERROR;
	     end;

	Pthis_token = Pfirst_token_desc;

	call SEMANTIC_ANALYSIS ();

	if MERROR_SEVERITY >= FATAL_ERROR
	then call abort (0, FATAL_ERROR_MSG);

	call initiate_file_$create (tdname, tename, RW_ACCESS, value_seg_ptr, created_value_seg, (0), code);
	if value_seg_ptr = null
	then call abort (code, rtrim (tdname) || ">" || rtrim (tename));

	if created_value_seg
	then do;
		call value_$init_seg (value_seg_ptr, PERMANENT_VALUE_SEG_TYPE, null, 0, code);
		if code ^= 0 then call abort (code, rtrim (tdname) || ">" || rtrim (tename));
	     end;

	definition_value_name = DEFINITION_PREFIX || "." || command_name;
	default_linear_form_value_name = DEFAULT_LINEAR_FORM_PREFIX || "." || command_name;

	if ^replace
	then do;

		call value_$get_data (value_seg_ptr, PERMANENT_VALUE, (definition_value_name), area_ptr, (null ()), (0), code);


		if code = 0 then definition_exists = "1"b;
		else if code = error_table_$oldnamerr then definition_exists = "0"b;
		else call abort (code, "Searching for " || command_name || " definition in value segment.");

		if definition_exists
		then do;
			call command_query_ (addr (auto_query_info), answer, ME,
			     "A definition for ^a exists^/   in the value segment ^a>^a.^/Do you want to replace it?",
			     command_name, tdname, tename);
			if answer = "no" | answer = "n"
			then call abort (0, "Definition not replaced.");
			else do;
				replace = "1"b;
				call com_err_ (0, ME, "Definition will be replaced.");
			     end;
		     end;
	     end;

	if replace
	then do;
		call value_$delete_data (value_seg_ptr, PERMANENT_VALUE, (definition_value_name), code);
		if code = error_table_$empty_file | code = error_table_$oldnamerr
		then call com_err_ ((0), ME, "No old definition of ^a in value segment.  Adding definition.", command_name);
		else if code ^= 0
		then call abort (code, "Cannot replace " || command_name || " definition in value segment.");

		call value_$delete_data (value_seg_ptr, PERMANENT_VALUE, (default_linear_form_value_name), code);
		if code = error_table_$empty_file | code = error_table_$oldnamerr
		then call com_err_ ((0), ME, "No old default linear form of ^a in value segment.  Adding definition.", command_name);
		else if code ^= 0
		then call abort (code, "Cannot replace " || command_name || " definition in value segment.");

	     end;

	call vector_util_$cv_pva_to_string (print_vector_array_ptr, definition_string_ptr, definition_string_length, code);

	if code ^= 0
	then call abort (code, "");

	call value_$set_data (value_seg_ptr, PERMANENT_VALUE, (definition_value_name), definition_string_ptr, divide (definition_string_length, BYTES_PER_WORD, 18, 0), (null ()), (null ()), (0), code);
	if code ^= 0
	then call abort (code, "");

          dlf_var_length = default_linear_form_length;
          allocate dlf_var in(based_area) set(dlf_var_ptr);
          dlf_var = default_linear_form_string;

	call value_$set_data (value_seg_ptr, PERMANENT_VALUE, (default_linear_form_value_name), dlf_var_ptr, divide(dlf_var_length + 3, BYTES_PER_WORD, 17, 0) + 1, (null ()), (null ()), (0), code);
	if code ^= 0
	then call abort (code, "");

RETURN:

	call cleanup_trans ();
	return;

ERROR_RETURN:

	return;

%page;

/* Translation and Utility Routines */

/*
   This subroutine adds the specified string (usually the current token)
   to a print vector in the print vector array we are building.
   The string becomes the value of the dimension specified by dim_num.
   If necessary, this subroutine adds a new print vector
   (in special cases) to accommodate the new dimension value (token).
*/

add_to_vector: proc (str, dim_num);

/* Automatic */

	dcl     vector_num		 fixed bin;

/* Parameter */

	dcl     dim_num		 fixed bin;
	dcl     str		 char (*);

	if MERROR_SEVERITY >= FATAL_ERROR		/* Quit if hopeless. */
	then return;

	if value_defined (dim_num) & ^CAN_HAVE_MULTIPLE_DEFINITIONS (dim_num)
	then do;
		call ERROR (7);
		return;
	     end;

	if (dim_num = EXCLUDED_OPTION | dim_num = UNEXCLUDED_OPTION) & str = option
	then default_exclude_myself = "0"b;		/* Don't need default exclusion anymore. */

	if dim_num = SYNONYM			/* Special, we need a SYNONYM print vector. */
	then do;
		call set_name (str, synonym_len, synonym_ptr);
		call append_vector (OPTION_NAME_VECTOR, vector_num, code);
		if code ^= 0
		then do;
			call ERROR (6);
			return;
		     end;
	     end;
	else if dim_num = NEGATIVE_FORM		/* Special, we need a NEGATIVE_FORM print vector. */
						/* Really a SYNONYM vector plus NEGATIVE_FORM dim. */
	then do;
		call set_name (str, synonym_len, synonym_ptr);
		call append_vector (NEGATIVE_FORM_VECTOR, vector_num, code);
		if code ^= 0
		then do;
			call ERROR (6);
			return;
		     end;
	     end;
	else do;					/* Nonspecial, try adding to current print vector. */
		vector_num = main_pv_num;
		if value_defined (dim_num) & CAN_HAVE_MULTIPLE_DEFINITIONS (dim_num)
		then do;
			call append_vector (main_pv_type, vector_num, code);
			if code ^= 0
			then do;
				call ERROR (6);
				return;
			     end;
		     end;
		if CAN_HAVE_VALUE (dim_num)
		then call add_dim (dim_num, str, vector_num, code);
		else call add_dim (dim_num, "", vector_num, code);
		if code ^= 0
		then do;
			call ERROR (8);
			return;
		     end;
	     end;

	value_defined (dim_num) = "1"b;

     end add_to_vector;

%page;

/*
   Sets up a print vector with all the mandatory information for
   the specified vector type.  Sets the current print vector to this one.
*/

start_vector: proc (vector_type);

/* Parameter */

	dcl     vector_type		 fixed bin;

/* Automatic */

	dcl     vector_num		 fixed bin;

	if MERROR_SEVERITY >= FATAL_ERROR
	then return;

	value_defined (*) = "0"b;

	call append_vector (vector_type, vector_num, code);
	if code ^= 0
	then do;
		call ERROR (6);
		return;
	     end;

	main_pv_num = vector_num;
	main_pv_type = vector_type;

     end start_vector;

%page;

/*
   This routine can be called to initialize different types of print vectors
   given the type wanted.  The input information is vector type.
   The output information is the vector_num of the new vector.
   Before calling this procedure, the caller must
   make sure that all necessary information for that particular
   print vector has been set.  For example, the OPTION print vector
   needs to have then command_name and option variables set.
*/

append_vector: proc (vector_type, vector_num, code);

/* Parameter */

	dcl     code		 fixed bin (35);
	dcl     vector_num		 fixed bin;
	dcl     vector_type		 fixed bin;

	code = 0;
	n_definition_order = n_definition_order + 1;
	call set_definition_order (n_definition_order, definition_order);

	if vector_type = OPTION_VECTOR
	then do;
		call vector_util_$append_general_print_vector
		     (area_ptr, N_INCREMENTAL_PV_SLOTS,
		     FREE_OLD_PV_ARRAY, NEW_VECTOR,
		     DIMENSIONS (DEFINITION_ORDER), definition_order,
		     DIMENSIONS (COMMAND_NAME), command_name,
		     DIMENSIONS (OPTION), option,
		     print_vector_array_ptr, code);
		if code ^= 0
		then return;
	     end;
	else if vector_type = ARGUMENT_VECTOR
	then do;
		call vector_util_$append_general_print_vector
		     (area_ptr, N_INCREMENTAL_PV_SLOTS,
		     FREE_OLD_PV_ARRAY, NEW_VECTOR,
		     DIMENSIONS (DEFINITION_ORDER), definition_order,
		     DIMENSIONS (COMMAND_NAME), command_name,
		     DIMENSIONS (OPTION), option,
		     DIMENSIONS (ARGUMENT), argument,
		     print_vector_array_ptr, code);
		if code ^= 0
		then return;
	     end;
	else if vector_type = INITIAL_VECTOR | vector_type = BASIC_VECTOR
	then do;
		call vector_util_$append_general_print_vector
		     (area_ptr, N_INCREMENTAL_PV_SLOTS,
		     FREE_OLD_PV_ARRAY, NEW_VECTOR,
		     DIMENSIONS (DEFINITION_ORDER), definition_order,
		     DIMENSIONS (COMMAND_NAME), command_name,
		     print_vector_array_ptr, code);
		if code ^= 0
		then return;
	     end;
	else if vector_type = OPTION_NAME_VECTOR
	then do;
		call vector_util_$append_general_print_vector
		     (area_ptr, N_INCREMENTAL_PV_SLOTS,
		     FREE_OLD_PV_ARRAY, NEW_VECTOR,
		     DIMENSIONS (DEFINITION_ORDER), definition_order,
		     DIMENSIONS (COMMAND_NAME), command_name,
		     DIMENSIONS (OPTION), option,
		     DIMENSIONS (SYNONYM), synonym,
		     print_vector_array_ptr, code);
		if code ^= 0
		then return;
	     end;
	else if vector_type = NEGATIVE_FORM_VECTOR
	then do;
		call vector_util_$append_general_print_vector
		     (area_ptr, N_INCREMENTAL_PV_SLOTS,
		     FREE_OLD_PV_ARRAY, NEW_VECTOR,
		     DIMENSIONS (DEFINITION_ORDER), definition_order,
		     DIMENSIONS (COMMAND_NAME), command_name,
		     DIMENSIONS (OPTION), option,
		     DIMENSIONS (SYNONYM), synonym,
		     DIMENSIONS (NEGATIVE_FORM), "",
		     print_vector_array_ptr, code);
		if code ^= 0
		then return;
	     end;
	else do;
		code = error_table_$bad_arg;
		return;
	     end;

	vector_num = print_vector_array.number_of_vectors;

     end append_vector;

%page;

/*
   This routine adds a dimension value to a specified print vector.
   If the print vector is specified by a -1, a new print vector
   is created to hold the dimension value.  The number of the
   new print vector is returned in that case.
*/

add_dim: proc (dim_num, dim_value, vector_num, code);

/* Parameter */

	dcl     code		 fixed bin (35);
	dcl     dim_num		 fixed bin;
	dcl     dim_value		 char (*);
	dcl     vector_num		 fixed bin;

	call vector_util_$append_general_print_vector
	     (area_ptr, N_INCREMENTAL_PV_SLOTS,
	     FREE_OLD_PV_ARRAY, vector_num,
	     DIMENSIONS (dim_num), dim_value,
	     print_vector_array_ptr, code);

     end add_dim;

%page;

/*
   This routine sets the character representation of the definition order.
*/

(size):
set_definition_order: proc (arg_n_definition_order, arg_definition_order);

/* Parameter */

	dcl     arg_n_definition_order fixed bin;
	dcl     arg_definition_order	 pic "999";

	on size begin;
		call com_err_ (0, ME, "The size of your definition is too large."
		     );
		call abort (0, "It has caused the definition_order dimension to overflow.");
	     end;

	arg_definition_order = arg_n_definition_order;

     end set_definition_order;

%page;

/*
   This routine is used to set information we must keep around
   (i.e., the command_name, option, argument variables).
   It sets the specified based variable to be the specified string
   (usually the current token).
*/

set_name: proc (arg_str, arg_name_len, arg_name_ptr);

/* Based */

	dcl     name		 char (arg_name_len) based (arg_name_ptr);

/* Parameter */

	dcl     arg_name_len	 fixed bin (21);
	dcl     arg_name_ptr	 ptr;
	dcl     arg_str		 char (*);

	if arg_name_ptr ^= null
	then free arg_name_ptr -> name;

	arg_name_len = length (arg_str);
	allocate name in (based_area) set (arg_name_ptr);

	name = arg_str;

     end set_name;

%page;

/* This routine "resets" a specified based character string variable by
     freeing it and setting the length to 0 and setting the ptr to null.
  */

reset_name: proc (arg_name_len, arg_name_ptr);

/* Based */

	dcl     name		 char (arg_name_len) based (arg_name_ptr);

/* Parameter */

	dcl     arg_name_len	 fixed bin (21);
	dcl     arg_name_ptr	 ptr;

	if arg_name_ptr ^= null
	then free arg_name_ptr -> name;

	arg_name_len = 0;
	arg_name_ptr = null;

     end reset_name;

%page;

/*
   This routine initializes some global variables used by
   the translator.  Some of these variables must be set immediately
   on invocation not only for the translator can clean up properly,
   but to start proper "running" values.
*/

initialize_translator_values: proc ();

	n_definition_order = 0;

	command_name_ptr, option_ptr, argument_ptr = null;
	command_name_len, option_len, argument_len = 0;

	synonym_ptr = null;
	synonym_len = 0;

	expanded_token_ptr = null;
	expanded_token_len = 0;

	seg_ptr = null;
	temp_seg_ptr, area_ptr = null;

	unspec (auto_query_info) = "0"b;
	auto_query_info.version = query_info_version_5;
	auto_query_info.yes_or_no_sw = "1"b;
	auto_query_info.question_iocbp = null;
	auto_query_info.answer_iocbp = null;
	auto_query_info.explanation_ptr = null;

     end initialize_translator_values;

%page;

/* Gets arg(arg_num) from the command's argument list.
     increments arg_num by 1.  If an error happens getting the arg,
     we call abort directly, we don't return from here.
  */

get_arg: proc (arg_num, arg_ptr, arg_len);

/* Parameter */

	dcl     arg_len		 fixed bin (21);
	dcl     arg_num		 fixed bin;
	dcl     arg_ptr		 ptr;

/* Automatic */

	dcl     code		 fixed bin (35);

	call cu_$arg_ptr (arg_num, arg_ptr, arg_len, code);
	if code ^= 0
	then call abort (code, "");

	arg_num = arg_num + 1;

     end get_arg;

%page;

/*
   This routine cleans up the user environment after an
   invocation of the translator.
*/

cleanup_trans: proc ();

	if seg_ptr ^= null
	then call term_$seg_ptr (seg_ptr, code);

	if temp_seg_ptr ^= null
	then call translator_temp_$release_all_segments (temp_seg_ptr, code);

	if value_seg_ptr ^= null
	then call terminate_file_ (value_seg_ptr, (0), TERM_FILE_TERM, code);

     end cleanup_trans;

%page;

/* This routine prints an error message, cleans up the environment and then
   aborts the invocation of mtape_cv_apd. */


abort: proc (code, msg);

/* Parameter */

	dcl     code		 fixed bin (35);
	dcl     msg		 char (*);

	call com_err_ (code, ME, msg);
	call cleanup_trans ();
	goto ERROR_RETURN;

     end abort;

%page;

/* This routine takes a based character string variable,
     and appends the given string to it (reallocating the based variable
     for more storage if necessary).
  */

append_to_str: proc (str_to_append, str_len, str_ptr);

/* Automatic */

	dcl     temp_str_len	 fixed bin (21);
	dcl     temp_str_ptr	 ptr;

/* Based */

	dcl     str		 char (str_len) based (str_ptr);
	dcl     temp_str		 char (temp_str_len) based (temp_str_ptr);

/* Parameter */

	dcl     str_len		 fixed bin (21);
	dcl     str_ptr		 ptr;
	dcl     str_to_append	 char (*);

	if str_ptr = null
	then do;
		str_len = length (str_to_append);
		allocate str in (based_area) set (str_ptr);
		str = str_to_append;
		return;
	     end;

	temp_str_len = length (str_to_append) + str_len;
	allocate temp_str in (based_area) set (temp_str_ptr);
	temp_str = str || str_to_append;
	free str_ptr -> str;
	str_ptr = temp_str_ptr;
	str_len = temp_str_len;

     end append_to_str;
