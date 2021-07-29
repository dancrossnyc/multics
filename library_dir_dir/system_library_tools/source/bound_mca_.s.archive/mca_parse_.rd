/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-01-14,Fawcett), approve(86-03-19,MCR7374),
     audit(86-05-05,Lippard), install(86-09-16,MR12.0-1159):
     Created to check the MCA command set to the MCA via the tandd gates.
  2) change(86-10-21,Fawcett), approve(86-10-21,PBF7374),
     audit(86-10-23,Farley), install(86-10-30,MR12.0-1203):
     Change the decimal-number reduction to decimal-integer.
                                                   END HISTORY COMMENTS */

/* format: style4 */
/* Created Nov 1984 by R. A. Fawcett */
mca_parse_:
	proc (a_data_ptr,a_data_len,a_mcad_ptr,a_user_level,a_code);
/*++
BEGIN 
main           

\"   Fist word MUST be load, reset, rload, trace, or test;
		/ load		/  LEX (1)				/ load_rest_tar \
		/ reset		/  LEX (1)				/ load_rest_tar \
		/ rload		/  LEX (1)				/ load_rest_tar \
		/ test		/  LEX (1)				/ target_verb   \
		/ read		/  LEX (1)				/ read_verb     \
		/ <no-token>	/ [a_code = error_table_$noarg]		/ RETURN \
		/ <any-token>	/ [a_code = error_table_$bad_arg]		/ AUDIT_RET \

\" Reset targets;

load_rest_tar	/ ipc		/ LEX (1)					/ ft_ipc      \
		/ ipcs		/				/ all_ipcs    \
		/ mca		/ LEX (1)					/ do_mca	    \
		/ <no-token>        / [a_code = error_table_$noarg]		/ RETURN \
		/ <any-token>	/ [a_code = error_table_$bad_arg]		/ AUDIT_RET \

ft_ipc		/ <ipc_num_att>	/ LEX (1)					/ next_ipc \
		/ <decimal-integer>	  / [a_code = error_table_$io_not_assigned]	/ AUDIT_RET \
		/ <any-token>	  / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
		/ <no-token>	  / [a_code = error_table_$noarg]		/ RETURN \
next_ipc		/ <no-token>	  /					/ exit        \
		/ <ipc_num_att>	  / LEX (1)				/ next_ipc    \
		/ <decimal-integer>	    / [a_code = error_table_$io_not_assigned]	/ AUDIT_RET \
		/ <any-token>	    / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
all_ipcs		/ <all_ipc_att>	    / LEX (1)				/ exit  \
		/		 / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
do_mca		/		 / [a_code = error_table_$bad_arg]		/ AUDIT_RET \

read_verb		/ config		 / LEX (1)				/ exit \
		/ <any-token>	   / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
		/ <no-token>	   / [a_code = error_table_$noarg]		/ RETURN \

target_verb	/ ipc		   / LEX (1)				/ ck_ipc_att \
		/ <any-token>	   / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
		/ <no-token>	   / [a_code = error_table_$noarg]		/ RETURN \

ck_ipc_att	/ <ipc_num_att>	   / LEX (1)				/ ck_via \
		/ <decimal-integer>	   / [a_code = error_table_$io_not_assigned]	/ AUDIT_RET \
		/ <any-token>	   / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
		/ <no-token>	   / [a_code = error_table_$noarg]		/ RETURN \

ck_via		/ via		   / LEX (1)				/ ck_imu \
		/ <any-token>	   / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
		/ <no-token>	   / [a_code = error_table_$noarg]		/ RETURN \

ck_imu		/ imu		   / LEX (1)				/ ck_imu_num \
		/ <any-token>	   / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
		/ <no-token>	   / [a_code = error_table_$noarg]		/ RETURN \

ck_imu_num	/ <valid_imu>	   / LEX(1)				/ ck_using \
		/ <any-token>	   / [a_code = code]			/ AUDIT_RET \
		/ <no-token>	   / [a_code = error_table_$noarg]		/ RETURN \

ck_using		/ using		   / LEX (1)				/ ck_technique \
		/ <any-token>	   / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
		/ <no-token>	   / [a_code = error_table_$noarg]		/ RETURN \

\" Parse the <technique>
ck_technique 	/ diag		   / LEX (1)				/ ck_options  \
		/ disp		   / LEX (1)				/ ck_options  \
		/ qry		   / LEX (1)				/ ck_options  \
		/ dpm		   / LEX (1)				/ ck_options  \
		/ mdr		   / LEX (1)				/ ck_options  \
		/ nft		   / LEX (1)				/ ck_options  \
		/ self		   / LEX (1)				/ ck_options  \
		/ <any-token>	   / [a_code = error_table_$bad_arg]		/ AUDIT_RET \
		/ <no-token>	   / [a_code = error_table_$noarg]		/ RETURN \

ck_options	/ <no-token>	   / [a_code = 0]				/ RETURN \
		/ options		   / LEX (1)				/ exit \
		/ <any-token>	   / [a_code = error_table_$bad_arg]		/ AUDIT_RET \

exit		/ <no-token>	   / [a_code = 0]				/ RETURN \
		/ <any-token>	   / [a_code = error_table_$too_many_args]	/ AUDIT_RET \
AUDIT_RET		/		 / audit_err				/ RETURN \
++*/

/* Externial Entries */

dcl  access_audit_r1_$log_general	entry options (variable);
dcl  cv_dec_check_ 	entry (char(*), fixed bin(35)) returns(fixed bin(35));
dcl  get_process_id_ entry() returns(bit(36));
dcl  lex_string_$init_lex_delims entry (char (*), char (*), char (*), char (*), char (*), bit (*),
		     char (*) var, char (*) var, char (*) var, char (*) var);
dcl  lex_string_$lex entry (ptr, fixed bin, fixed bin, ptr, bit(*), char(*),
                        char(*), char(*), char(*), char(*), char(*) var, char(*) var,
		    char(*) var, char(*) var, ptr, ptr, fixed bin(35));
dcl translator_temp_$get_segment entry (char(*) aligned, ptr, fixed bin(35));
dcl translator_temp_$release_all_segments entry (ptr, fixed bin (35));

dcl cleanup condition;

/* Error_table */

dcl error_table_$bad_arg fixed bin(35) ext static;
dcl error_table_$too_many_args fixed bin(35) ext static;
dcl error_table_$noarg fixed bin(35) ext static;
dcl error_table_$not_attached fixed bin(35) ext static;
dcl error_table_$io_not_assigned fixed bin(35) ext static;
dcl error_table_$io_no_path fixed bin(35) ext static;

/* Ext static var */

dcl  access_operations_$invalid_mca bit (36) aligned ext static;

/* Automatic */

dcl (APstmt, APtoken) ptr;
dcl (LEXDLM,LEXCTL) char (32) var aligned;
dcl a_code fixed bin (35);
dcl a_data_ptr ptr;
dcl a_data_len fixed bin;
dcl a_mcad_ptr ptr;
dcl a_user_level fixed bin;
dcl audit_eventflags bit (36);
dcl areap ptr;
dcl code fixed bin (35);
dcl  data char (data_len) based (data_ptr);
dcl  command char (data_len) based (command_ptr);
dcl command_ptr ptr;
dcl data_ptr ptr;
dcl data_len fixed bin;
dcl imu_path_num fixed bin;
dcl ipc_att_num fixed bin init (0);
dcl my_pid bit (36);
dcl user_level fixed bin;

/* Builtins */

dcl (addr, hbound, lbound, null, translate) builtin;

/* Constants */
dcl  lower char (26) static options (constant)
	init ("abcdefghijklmnopqrstuvwxyz");
dcl  UPPER char (26) static options (constant)
	init ("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
dcl TRUE bit (1) init ("1"b) static options (constant);
dcl FALSE bit (1) init ("0"b) static options (constant);
dcl myname char (10) aligned init ("mca_parse_") static options (constant);
%page;

	my_pid = get_process_id_ ();
	mcad_ptr = a_mcad_ptr;
	if my_pid ^= mcad.attach_pid then do;
	   a_code = error_table_$not_attached;
	   return;
	   end;
	data_ptr = a_data_ptr;
	data_len = a_data_len;
          user_level = a_user_level;
	areap = null ();
	command_ptr = null ();

	on cleanup begin;
	    if areap ^= null () then call translator_temp_$release_all_segments (areap, (0));
	end;

	call translator_temp_$get_segment (myname, areap, code);
	if areap = null () then do;
	   a_code = code;
	   return;
	   end;



	command_ptr = allocate (areap,data_len);          

          command = translate (data,lower,UPPER);

	call lex_string_$init_lex_delims
	   ("", "", "", "","", "11"b," ", " ", LEXDLM, LEXCTL);

	call lex_string_$lex (command_ptr, data_len, 0, areap, "0110"b,
	   "", "", "", "","", " ", " ",
	   LEXDLM, LEXCTL, APstmt, APtoken, code);

	if code ^= 0 then do;
	   a_code = code;
	   return;
	   end;
	Pstmt = APstmt;
	Pthis_token = APtoken;

	a_code = 0;
	call SEMANTIC_ANALYSIS ();

error_ret:
          if areap ^= null () then call translator_temp_$release_all_segments (areap, (0));

          return;
%page;

all_ipc_att:
	proc () returns (bit (1));
dcl ret_bit bit (1);
	ret_bit = "1"b;
	do ipc_att_num = lbound(mcad.ipcd_array,1) to hbound(mcad.ipcd_array,1) while (ret_bit = "1"b);
	   if mcad.ipcd_array(ipc_att_num).level_1_state >
	      PH_NOT_PRESENT then ret_bit = ipc_is_att ();
	   end;
	return (ret_bit);
	end all_ipc_att;

ipc_num_att:
          proc () returns (bit (1));
          ipc_att_num = cv_dec_check_ (token_value,code);
	if code ^= 0 then return (FALSE);
          token.Nvalue = ipc_att_num;
	return (ipc_is_att());
end ipc_num_att;	      

ipc_is_att:
	proc () returns (bit (1));
          if mcad.ipcd_array (ipc_att_num).state >= IPC_ATTACHED
             then return (TRUE);
	else
	   return (FALSE);
	end ipc_is_att;
 

valid_imu:
       proc () returns (bit (1));
       imu_path_num =  cv_dec_check_ (token_value,code) + 1;
       if code ^= 0 then do;
	 code = error_table_$bad_arg;
	 return (FALSE);
       end;
       else do;
	if imu_path_num ^= mcad.imu_number then do;
	   a_code = error_table_$io_no_path;
	   return (FALSE);
	   end;
	return (TRUE);
	end;
       end valid_imu;



audit_err: proc;

	audit_eventflags = "0"b;
	addr (audit_eventflags) -> audit_event_flags.special_op = "1"b;
	addr (audit_eventflags) -> audit_event_flags.grant = "0"b;
	call access_audit_r1_$log_general (myname,user_level,
	     audit_eventflags,access_operations_$invalid_mca,"",a_code,
               null (),0,"^a", data);
end audit_err;
%page;
%include translator_temp_alloc;
%page;
%include access_audit_eventflags;
%page;
%include mca_data;
%page;
%include mca_constants;
