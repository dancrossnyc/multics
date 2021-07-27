/* **************************************************************
   *                                                            *
   * Copyright, (C) Honeywell Information Systems Inc., 1982    *
   *                                                            *
   * Copyright, (C) Massachusetts Institute of Technology, 1982 *
   *                                                            *
   * Copyright (c) 1972 by Massachusetts Institute of           *
   * Technology and Honeywell Information Systems, Inc.         *
   *                                                            *
   ************************************************************** */





/* HISTORY COMMENTS:
  1) change(86-05-20,Gilcrease), approve(86-05-22,MCR7369),
     audit(86-06-23,LJAdams), install(86-06-30,MR12.0-1081):
               Allow weekly cutoffs. SCP6250.
                                                   END HISTORY COMMENTS */


/* CV_PMF - Compile a Project Master File (PMF) into a Project Definition Table (PDT).

   See AK-51 for an explanation of the contents of the PMF.
   See AN-66 for an explanation of the format of the PDT.

   THVV April 74
   Modified 750717 by PG to fix bug 001 (pathnames with $ were being diagnosed)
   Modified by THVV and TAC for Priority Scheduler parameters, June 1975
   Modified by REMullen to merge THVV,TAC,PG changes, July 1975
   Modified May 1976 by T. Casey to add per_user cutoff warning thresholds.
   Modified May 1978 by T. Casey to add pdir_quota.
   Modified November 1978 by T. Casey for MR7.0 to add new absentee control parameters.
   Modified October 1979 by T. Casey for MR8.0 for process preservation control parameters.
   Modified December 1981 by E. N. Kittlitz for bugfixes, User_warn controls.
   Modified January 1982 by BIM for author changes.
   Modified July 1982 by E. N. Kittlitz for [severity] and pmf suffix.
   Modified September 1982 by E. N. Kittlitz for default ring.
   Modified November 1982 by J. I. Schiller to correctly parse dollar values.
   Modified 1984-07-05 BIM for authorization ranges.
   Modified 1984-07-25 BIM for checks against live pdt sate.
   Modified 1984-09-14 BIM to check for current version of  live pdt sate
            and NOT use sate authorization as default user authorization.
   Modified 1984-11-20 BIM Fix "Authorization:" statement.
*/

/*++

BEGIN
	/ Projectid : <any-token> ;	/ LEX(2) set_project_name LEX(2)			/ head \
	/			/ ERROR(2)					/ RETURN \

head	/ Grace : <decimal-integer> ;	/ LEX(2) [default.bump_grace = token.Nvalue] LEX(2) 	/ head \
	/ Attributes :		/ LEX(2) PUSH(head) PUSH(assign_default_attributes)	/ attrsub \
	/ Initproc : <path_name> , direct ;
				/ LEX(2) set_ip_ss (default.initial_procedure,default.uflags.ip_given,default.ip_len)
				  LEX(4) [default.dont_call_init_admin = "1"b]		/ head \
	/ Initproc : <path_name> ;	/ LEX(2) set_ip_ss (default.initial_procedure,default.uflags.ip_given,default.ip_len)
				  LEX(2) [default.dont_call_init_admin = "0"b]		/ head \
	/ Initproc : <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ head \
	/ Subsystem : <path_name> ;	/ LEX(2) set_ip_ss (default_subsystem,default.uflags.ss_given, default.ss_len)
				  LEX(2)						/ head \
	/ Subsystem : <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ head \
	/ Homedir : <path_name> ;	/ LEX(2) [call set_string ("Homedir", default.home_dir, token_value)] LEX(2) / head \
	/ Homedir : <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ head \
	/ Outer_module : <path_name> ; / LEX(2) [call set_string ("Outer_module", default.outer_module, token_value)] LEX(2)	/ head \
	/ Outer_module : <any-token> ; / LEX(2) ERROR(22) LEX(2)				/ head \
	/ Lot_size : <decimal-integer> , own ; / LEX(2) [default.lot_size = -token.Nvalue] LEX(4) / head \
	/ Lot_size : <decimal-integer> , stack ; / LEX(2) [default.lot_size = token.Nvalue] LEX(4) / head \
	/ Lot_size : <decimal-integer> ; / LEX(2) [default.lot_size = token.Nvalue] LEX(2)	/ head \
	/ Kst_size : <decimal-integer> ; / LEX(2) [default.kst_size = token.Nvalue] LEX(2)	/ head \
 	/ Cls_size : <decimal-integer> , stack ; / LEX(2) [default.cls_size = -token.Nvalue] LEX(4) / head \
	/ Cls_size : <decimal-integer> , own ; / LEX(2) [default.cls_size = token.Nvalue] LEX(4)	/ head \
	/ Cls_size : <decimal-integer> ; / LEX(2) [default.cls_size = token.Nvalue] LEX(2)	/ head \
	/ Pdir_quota : <decimal-integer> ; / LEX(2) [default.pdir_quota = token.Nvalue] LEX(2)	/ head \
	/ Max_foreground : <decimal-integer> ; / LEX(2) [default.max_foreground = token.Nvalue] LEX(2) / head \
	/ Max_background : <decimal-integer> ; / LEX(2) [default.max_background = token.Nvalue] LEX(2) / head \
	/ Abs_foreground_cpu_limit : <decimal-integer> ;
				/ LEX(2) [default.abs_foreground_cpu_limit = token.Nvalue] LEX(2) / head \
	/ Cutoff :		/ LEX(2) PUSH(head) PUSH(default_cutoff)		/ number \
	/ Limit :			/ LEX(2) PUSH(head) PUSH(default_monthlim)		/ number \
	/ Shift_limit :		/ LEX(2) PUSH(head) [x = 1]				/ default_shiftlims \
	/ User_warn_days : <decimal-integer> ; / LEX(2) [default.user_warn_days = token.Nvalue] LEX(2)	/ head \
	/ User_warn_percent : <decimal-integer> ; / LEX(2) [default.user_warn_pct = token.Nvalue] LEX(2)	/ head \
	/ User_warn_dollars :		/ LEX(2) PUSH(head) PUSH(default_user_warn_doll)		/ number \
	/ Warn_days : <decimal-integer> ; / LEX(2) [default.warn_days = token.Nvalue] LEX(2)	/ head \
	/ Warn_percent : <decimal-integer> ; / LEX(2) [default.warn_pct = token.Nvalue] LEX(2)	/ head \
	/ Warn_dollars :		/ LEX(2) PUSH(head) PUSH(default_warn_doll)		/ number \
	/ Ring : <onetoseven> , <onetoseven> , <onetoseven> ; / LEX (2) [default.low_ring = token.Nvalue] LEX (2)
					[default.high_ring = token.Nvalue] LEX (2)
					[default.default_ring = token.Nvalue] LEX (2) 	/ head \
	/ Ring : <onetoseven> , <onetoseven> ; / LEX(2) [default.low_ring = token.Nvalue] 
					[default.default_ring = token.Nvalue] LEX(2)
					[default.high_ring = token.Nvalue] LEX (2)	/ head \
	/ Ring : <onetoseven> ;	 / LEX(2) [default.low_ring = token.Nvalue]
					[default.default_ring = token.Nvalue]
					[default.high_ring = token.Nvalue] LEX(2)	/ head \
	/ Authorization : <authorization_string> ;
				/ LEX(2) [default.user_authorization = authorization_value] 
				         check_default_authorization
				         LEX(2)
										/ head \
	/ Authorization : <any-token> ;
				/ LEX(2) ERROR(25) LEX(2)				/ head \
	/ Group : <any-token>;	/ LEX(2) [call set_string ("Group", Default_Group, token_value)] LEX(2)/ head \
	/ personid :		/						/ user \
	/ Grace			/						/ error_23_in_head \
	/ Attributes		/						/ error_23_in_head \
	/ Initproc		/						/ error_23_in_head \
	/ Homedir			/						/ error_23_in_head \
	/ Outer_module		/						/ error_23_in_head \
	/ Lot_size		/						/ error_23_in_head \
	/ Kst_size		/						/ error_23_in_head \
	/ Cls_size		/						/ error_23_in_head \
	/ Pdir_quota		/						/ error_23_in_head \
	/ Max_foreground		/						/ error_23_in_head \
	/ Max_background		/						/ error_23_in_head \
	/ Abs_foreground_cpu_limit	/						/ error_23_in_head \
	/ Cutoff			/						/ error_23_in_head \
	/ Limit			/						/ error_23_in_head \
	/ Shift_limit		/						/ error_23_in_head \
	/ User_warn_days		/						/ error_23_in_head \
	/ User_warn_percent		/						/ error_23_in_head \
	/ User_warn_dollars		/						/ error_23_in_head \
	/ Warn_days		/						/ error_23_in_head \
	/ Warn_percent		/						/ error_23_in_head \
	/ Warn_dollars		/						/ error_23_in_head \
	/ Ring			/						/ error_23_in_head \
	/ Audit			/						/ error_23_in_head \
	/ Authorization		/						/ error_23_in_head \
	/ Group			/						/ error_23_in_head \
	/ personid		/						/ error_23_in_head \
	/ Accountid		/ ERROR(29) NEXT_STMT				/ head \
	/ <any-token>		/ ERROR(1) NEXT_STMT				/ head \
	/ <no-token>		/ ERROR(5)			 		/ RETURN \

error_23_in_head
	/			/ ERROR (23) NEXT_STMT				/ head \

user	/ end ;	<no-token>	/ close					/ RETURN \
	/ end ;   <any-token>	/ ERROR (35) close				/ RETURN \
	/ personid : * ;		/ LEX(3) close open_anon LEX				/ user \
	/ personid : <any-token> ;	/ LEX(2) close open LEX(2)				/ user \
	/ password : <any-token> ;	/ LEX(2) [call set_string ("password", user.password, token_value)] LEX(2)/ user \
	/ ring : <onetoseven> , <onetoseven> , <onetoseven> ;	/ LEX (2) [user.low_ring = token.Nvalue] LEX (2)
					[user.high_ring = token.Nvalue] LEX (2)
					[user.default_ring = token.Nvalue] LEX (2) 	/ user \
	/ ring : <onetoseven> , <onetoseven> ; / LEX(2) [user.low_ring = token.Nvalue] 
					[user.default_ring = token.Nvalue] LEX(2)
					[user.high_ring = token.Nvalue] LEX(2)		/ user \
	/ ring : <onetoseven> ;	 / LEX(2) [user.low_ring = token.Nvalue]
					[user.default_ring = token.Nvalue]
					[user.high_ring = token.Nvalue] LEX(2)		/ user \
	/ initproc : <path_name> , direct ;
				/ LEX(2)  set_ip_ss (user.initial_procedure, user.uflags.ip_given, user.ip_len)
				  LEX(4) [user.dont_call_init_admin = "1"b]		/ user \
	/ initproc : <path_name> ;	/ LEX(2)  set_ip_ss (user.initial_procedure, user.uflags.ip_given, user.ip_len)
				  LEX(2) [user.dont_call_init_admin = "0"b]		/ user \
	/ initproc : <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ user \
	/ subsystem : <path_name> ;	/ LEX(2) set_ip_ss (user_subsystem, user.uflags.ss_given, user.ss_len)
				  LEX(2)						/ user \
	/ subsystem: <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ user \
	/ homedir : <path_name> ;	/ LEX(2) [call set_string ("homedir", user.home_dir, token_value)] LEX(2)/ user \
	/ homedir : <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ user \
	/ outer_module : <path_name> ; / LEX(2) [call set_string ("outer_module", user.outer_module, token_value)] LEX(2)/ user \
	/ outer_module : <any-token> ; / LEX(2) ERROR(22) LEX(2)				/ user \
	/ lot_size : <decimal-integer> , own ; / LEX(2) [user.lot_size = -token.Nvalue] LEX(4)	/ user \
	/ lot_size : <decimal-integer> , stack ; / LEX(2) [user.lot_size = token.Nvalue] LEX(4)	/ user \
	/ lot_size : <decimal-integer> ; / LEX(2) [user.lot_size = token.Nvalue] LEX(2)		/ user \
	/ kst_size : <decimal-integer> ; / LEX(2) [user.kst_size = token.Nvalue] LEX(2)		/ user \
	/ cls_size : <decimal-integer> , stack ; / LEX(2) [user.cls_size = -token.Nvalue] LEX(4)	/ user \
	/ cls_size : <decimal-integer> , own ; / LEX(2) [user.cls_size = token.Nvalue] LEX(4)	/ user \
	/ cls_size : <decimal-integer> ; / LEX(2) [user.cls_size = token.Nvalue] LEX(2)		/ user \
	/ pdir_quota : <decimal-integer> ; / LEX(2) [user.pdir_quota = token.Nvalue] LEX(2)	/ user \
	/ max_foreground : <decimal-integer> ; / LEX(2) [user.max_foreground = token.Nvalue] LEX(2)	/ user \
	/ max_background : <decimal-integer> ; / LEX(2) [user.max_background = token.Nvalue] LEX(2)	/ user \
	/ abs_foreground_cpu_limit : <decimal-integer> ;
				/ LEX(2) [user.abs_foreground_cpu_limit = token.Nvalue] LEX(2) / user \
	/ cutoff :		/ LEX(2) PUSH(user) PUSH(cutoff)			/ number \
	/ grace : <decimal-integer> ;	/ LEX(2) [user.bump_grace = token.Nvalue] LEX(2)		/ user \
	/ attributes :		/ LEX(2) PUSH(user) PUSH(assign_user_attributes)		/ attrsub \
	/ limit :			/ LEX(2) PUSH(user) PUSH(monthlim)			/ number \
	/ shift_limit :		/ LEX(2) PUSH(user) [x = 1]				/ shiftlims \
	/ user_warn_days : <decimal-integer> ; / LEX(2) [user.user_warn_days = token.Nvalue] LEX(2)/ user \
	/ user_warn_percent : <decimal-integer> ; / LEX(2) [user.user_warn_pct = token.Nvalue] LEX(2)/ user \
	/ user_warn_dollars :		/ LEX(2) PUSH(user) PUSH(user_warn_doll)	/ number \
	/ warn_days : <decimal-integer> ; / LEX(2) [user.warn_days = token.Nvalue] LEX(2)	/ user \
	/ warn_percent : <decimal-integer> ; / LEX(2) [user.warn_pct = token.Nvalue] LEX(2)	/ user \
	/ warn_dollars :		/ LEX(2) PUSH(user) PUSH(warn_doll)			/ number \
	/ authorization : <authorization_string> ;
				/ LEX(2) [user.user_authorization = authorization_value]
				  check_user_authorization
				  LEX(2)
										/ user \
	/ authorization : <any-token> ; / LEX(2) ERROR(25) LEX(2)				/ user \
	/ group : <any-token> ;	/ LEX(2) [call set_string ("group", user.group, token_value)] LEX(2)	/ user \
	/ Grace : <decimal-integer> ;	/ LEX(2) [default.bump_grace = token.Nvalue] LEX(2) 	/ user \
	/ Attributes :		/ LEX(2) PUSH(user) PUSH(assign_default_attributes)	/ attrsub \
	/ Initproc : <path_name> , direct ;
				/ LEX(2) set_ip_ss (default.initial_procedure,default.uflags.ip_given,default.ip_len)
				  LEX(4) [default.dont_call_init_admin = "1"b]		/ user \
	/ Initproc : <path_name> ;	/ LEX(2) set_ip_ss (default.initial_procedure,default.uflags.ip_given,default.ip_len)
				  LEX(2) [default.dont_call_init_admin = "0"b]		/ user \
	/ Initproc : <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ user \
	/ Subsystem : <path_name> ;	/ LEX(2) 	set_ip_ss (default_subsystem, default.uflags.ss_given, default.ss_len)
				  LEX(2)						/ user \
	/ Subsystem : <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ user \
	/ Homedir : <path_name> ;	/ LEX(2) [call set_string ("Homedir", default.home_dir, token_value)] LEX(2)	/ user \
	/ Homedir : <any-token> ;	/ LEX(2) ERROR(22) LEX(2)				/ user \
	/ Outer_module : <path_name> ; / LEX(2) [call set_string ("Outer_module", default.outer_module, token_value)] LEX(2)	/ user \
	/ Outer_module : <any-token> ; / LEX(2) ERROR(22) LEX(2)				/ user \
	/ Lot_size : <decimal-integer> , own ; / LEX(2) [default.lot_size = -token.Nvalue] LEX(4) / user \
	/ Lot_size : <decimal-integer> , stack ; / LEX(2) [default.lot_size = token.Nvalue] LEX(4) / user \
	/ Lot_size : <decimal-integer> ; / LEX(2) [default.lot_size = token.Nvalue] LEX(2)	/ user \
	/ Kst_size : <decimal-integer> ; / LEX(2) [default.kst_size = token.Nvalue] LEX(2)	/ user \
 	/ Cls_size : <decimal-integer> , stack ; / LEX(2) [default.cls_size = -token.Nvalue] LEX(4) / user \
	/ Cls_size : <decimal-integer> , own ; / LEX(2) [default.cls_size = token.Nvalue] LEX(4)	/ user \
	/ Cls_size : <decimal-integer> ; / LEX(2) [default.cls_size = token.Nvalue] LEX(2)	/ user \
	/ Cutoff :		/ LEX(2) PUSH(user) PUSH(default_cutoff)		/ number \
	/ Limit :			/ LEX(2) PUSH(user) PUSH(default_monthlim)		/ number \
	/ Shift_limit :		/ LEX(2) PUSH(user) [x = 1]				/ default_shiftlims \
	/ User_warn_days : <decimal-integer> ; / LEX(2) [default.user_warn_days = token.Nvalue] LEX(2)/ user \
	/ User_warn_percent : <decimal-integer> ; / LEX(2) [default.user_warn_pct = token.Nvalue] LEX(2)/ user \
	/ User_warn_dollars :		/ LEX(2) PUSH(user) PUSH(default_user_warn_doll)	/ number \
	/ Warn_days : <decimal-integer> ; / LEX(2) [default.warn_days = token.Nvalue] LEX(2)	/ user \
	/ Warn_percent : <decimal-integer> ; / LEX(2) [default.warn_pct = token.Nvalue] LEX(2)	/ user \
	/ Warn_dollars :		/ LEX(2) PUSH(user) PUSH(default_warn_doll)		/ number \
	/ Ring : <onetoseven> , <onetoseven> , <onetoseven> ;	/ LEX (2) [default.low_ring = token.Nvalue] LEX (2)
					[default.high_ring = token.Nvalue] LEX (2)
					[default.default_ring = token.Nvalue] LEX (2)	/ user \
	/ Ring : <onetoseven> , <onetoseven> ;
				/ LEX(2) [default.low_ring = token.Nvalue] 
				         [default.default_ring = token.Nvalue] LEX(2)
				         [default.high_ring = token.Nvalue] LEX(2)	/ user \
	/ Ring : <onetoseven> ;	/ LEX(2) [default.low_ring = token.Nvalue]
				         [default.default_ring = token.Nvalue]
				         [default.high_ring = token.Nvalue] LEX(2)	/ user \
	/ Authorization : <authorization_string> ;
				/ LEX(2) [default.user_authorization = authorization_value] LEX(2)
										/ user \
	/ Authorization : <any-token> ;
				/ LEX(2) ERROR(25) LEX(2)				/ user \
	/ Group : <any-token> ;	/ LEX(2) [call set_string ("Group", Default_Group, token_value)] LEX(2)/ user \
	/ Grace			/						/ error_23_in_user \
	/ Attributes		/						/ error_23_in_user \
	/ Initproc		/						/ error_23_in_user \
	/ Subsystem		/						/ error_23_in_user \
	/ Homedir			/						/ error_23_in_user \
	/ Outer_module		/						/ error_23_in_user \
	/ Lot_size		/						/ error_23_in_user \
	/ Kst_size		/						/ error_23_in_user \
	/ Cls_size		/						/ error_23_in_user \
	/ Pdir_quota		/						/ error_23_in_user \
	/ Max_foreground		/						/ error_23_in_user \
	/ Max_background		/						/ error_23_in_user \
	/ Abs_foreground_cpu_limit	/						/ error_23_in_user \
	/ Cutoff			/						/ error_23_in_user \
	/ Limit			/						/ error_23_in_user \
	/ Shift_limit		/						/ error_23_in_user \
	/ User_warn_days		/						/ error_23_in_user \
	/ User_warn_percent		/						/ error_23_in_user \
	/ User_warn_dollars		/						/ error_23_in_user \
	/ Warn_days		/						/ error_23_in_user \
	/ Warn_percent		/						/ error_23_in_user \
	/ Warn_dollars		/						/ error_23_in_user \
	/ Ring			/						/ error_23_in_user \
	/ Audit			/						/ error_23_in_user \
	/ Authorization		/						/ error_23_in_user \
	/ Group			/						/ error_23_in_user \
	/ end			/						/ error_23_in_user \
	/ personid		/						/ error_23_in_user \
	/ ring			/						/ error_23_in_user \
	/ initproc		/						/ error_23_in_user \
	/ subsystem		/						/ error_23_in_user \
	/ homedir			/						/ error_23_in_user \
	/ outer_module		/						/ error_23_in_user \
	/ lot_size		/						/ error_23_in_user \
	/ kst_size		/						/ error_23_in_user \
	/ cls_size		/						/ error_23_in_user \
	/ pdir_quota		/						/ error_23_in_user \
	/ max_foreground		/						/ error_23_in_user \
	/ max_background		/						/ error_23_in_user \
	/ abs_foreground_cpu_limit	/						/ error_23_in_user \
	/ cutoff			/						/ error_23_in_user \
	/ grace			/						/ error_23_in_user \
	/ attributes		/						/ error_23_in_user \
	/ limit			/						/ error_23_in_user \
	/ shift_limit		/						/ error_23_in_user \
	/ user_warn_days		/						/ error_23_in_user \
	/ user_warn_percent		/						/ error_23_in_user \
	/ user_warn_dollars		/						/ error_23_in_user \
	/ warn_days		/						/ error_23_in_user \
	/ warn_percent		/						/ error_23_in_user \
	/ warn_dollars		/						/ error_23_in_user \
	/ authorization		/						/ error_23_in_user \
	/ group			/						/ error_23_in_user \
	/ Accountid		/ ERROR(29) NEXT_STMT				/ user \
	/ accountid		/ ERROR(29) NEXT_STMT				/ user \
	/ <any-token>		/ ERROR(3) NEXT_STMT				/ user \
	/ <no-token>		/ ERROR(5) close					/ RETURN \

error_23_in_user
	/			/ ERROR (23) NEXT_STMT				/ user \

attrsub	/			/ [sx = ON; string(ats(ON)) = ""b; string(ats(OFF)) = ""b]	/ \
attloop	/ ;			/ LEX						/ STACK_POP \
	/ none			/ LEX						/ gobble_semi \
	/ null			/ LEX						/ gobble_semi \
	/ ^			/ LEX [sx = 1 - sx]					/ \
	/ administrator		/ LEX [ats (sx).administrator = "1"b]			/ atts \
	/ admin			/ LEX [ats (sx).administrator = "1"b]			/ atts \
	/ primary_line		/ LEX [ats (sx).primary_line = "1"b]			/ atts \
	/ nobump			/ LEX [ats (sx).nobump = "1"b]			/ atts \
	/ guaranteed_login		/ LEX [ats (sx).guaranteed_login = "1"b]		/ atts \
	/ guar			/ LEX [ats (sx).guaranteed_login = "1"b]		/ atts \
	/ anonymous		/ LEX [ats (sx).anonymous = "1"b]			/ atts \
	/ anon			/ LEX [ats (sx).anonymous = "1"b]			/ atts \
	/ nopreempt		/ LEX [ats (sx).nopreempt = "1"b]			/ atts \
	/ nolist			/ LEX [ats (sx).nolist = "1"b]			/ atts \
	/ dialok			/ LEX [ats (sx).dialok = "1"b]			/ atts \
	/ dial			/ LEX [ats (sx).dialok = "1"b]			/ atts \
	/ multip			/ LEX [ats (sx).multip = "1"b]			/ atts \
	/ multi_login		/ LEX [ats (sx).multip = "1"b]			/ atts \
	/ preempting		/ LEX [ats (sx).bumping = "1"b]			/ atts \
	/ bumping			/ LEX [ats (sx).bumping = "1"b]			/ atts \
	/ brief			/ LEX [ats (sx).brief = "1"b]				/ atts \
	/ vinitproc		/ LEX [ats (sx).vinitproc = "1"b]			/ atts \
	/ v_process_overseer	/ LEX [ats (sx).vinitproc = "1"b]			/ atts \
	/ vhomedir		/ LEX [ats (sx).vhomedir = "1"b]			/ atts \
	/ v_home_dir		/ LEX [ats (sx).vhomedir = "1"b]			/ atts \
	/ nostartup		/ LEX [ats (sx).nostartup = "1"b]			/ atts \
	/ no_start_up		/ LEX [ats (sx).nostartup = "1"b]			/ atts \
	/ no_secondary		/ LEX [ats (sx).sb_ok = "1"b]				/ atts \
	/ no_sec			/ LEX [ats (sx).sb_ok = "1"b]				/ atts \
	/ no_primary		/ LEX [ats (sx).pm_ok = "1"b]				/ atts \
	/ no_prime		/ LEX [ats (sx).pm_ok = "1"b]				/ atts \
	/ no_edit_only		/ LEX [ats (sx).eo_ok = "1"b]				/ atts \
	/ no_eo			/ LEX [ats (sx).eo_ok = "1"b]				/ atts \
	/ op_login		/ LEX [ats (sx).daemon = "1"b]			/ atts \
	/ daemon			/ LEX [ats (sx).daemon = "1"b]			/ atts \
	/ v_outer_module		/ ERROR(33) LEX					/ atts \
 	/ vdim			/ ERROR(33) LEX					/ atts \
	/ no_warning		/ LEX [ats (sx).no_warning = "1"b]			/ atts \
	/ nowarn			/ LEX [ats (sx).no_warning = "1"b]			/ atts \
	/ igroup			/ LEX [ats (sx).igroup = "1"b]			/ atts \
	/ save_pdir		/ LEX [ats (sx).save_pdir = "1"b]			/ atts \
	/ disconnect_ok		/ LEX [ats (sx).disconnect_ok = "1"b]			/ atts \
	/ save_on_disconnect	/ LEX [ats (sx).save_on_disconnect = "1"b]		/ atts \
	/ save			/ LEX [ats (sx).save_on_disconnect = "1"b]		/ atts \
	/ <any-token>		/ ERROR(4) NEXT_STMT				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \

atts	/ ,			/ LEX [sx = ON]					/ attloop \
gobble_semi
	/ ;			/ LEX						/ STACK_POP \
	/ <any-token>		/ ERROR(4) NEXT_STMT				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \

assign_default_attributes
	/			/ [string (default.at) = string (ats (ON)) & ^string (ats (OFF))]
										/ STACK_POP \

assign_user_attributes
	/			/ [string (user.at) = string (default.at) | string (ats (ON))]
				  [string (user.at) = string (user.at) & ^string (ats (OFF))]
										/ STACK_POP \

number	/			/ [t = 0]						/ \
          / <floating_number>           / cv_float_ (token_value, (0), t) LEX			/ STACK_POP \
	/ <decimal-integer>		/ [t = 1e0*token.Nvalue] LEX				/ STACK_POP \
	/ open			/ [t = BIGFLO] LEX					/ STACK_POP \
	/ <any-token>		/ ERROR(6) NEXT_STMT LEX(-1)				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \

default_monthlim	/ ;		/ LEX [default.dollar_limit = t]			/ STACK_POP \
	/ <any-token>		/ ERROR(7) NEXT_STMT				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \

monthlim	/ ;			/ LEX [user.dollar_limit = t]				/ STACK_POP \
	/ <any-token>		/ ERROR(7) NEXT_STMT				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \

default_cutoff	/ ;		/ LEX [default.absolute_limit = t]			/ STACK_POP \
	/ ,			/ LEX PUSH(get_default_increment) PUSH(check_date) 	/ concatenate_date \
	/			/ ERROR(8) NEXT_STMT				/ STACK_POP \

cutoff	/ ;			/ LEX [user.absolute_limit = t]			/ STACK_POP \
	/ ,			/ LEX PUSH(get_increment) PUSH(check_date) 		/ concatenate_date \
	/			/ ERROR(8) NEXT_STMT				/ STACK_POP \

get_default_increment
	/ ;			/ [default.absolute_limit = t]
				  [default.absolute_cutoff = time] LEX			/ STACK_POP \
	/ , <increment> ;		/ [default.absolute_limit = t]
				  [default.absolute_cutoff = time]
				  [default.absolute_increm = x] LEX(3)			/ STACK_POP \
	/			/ ERROR(8) NEXT_STMT				/ STACK_POP \

get_increment
	/ ;			/ [user.absolute_limit = t]
				  [user.absolute_cutoff = time] LEX			/ STACK_POP \
	/ , <increment> ;		/ [user.absolute_limit = t]
				  [user.absolute_cutoff = time]
				  [user.absolute_increm = x] LEX(3)			/ STACK_POP \
	/			/ ERROR(8) NEXT_STMT				/ STACK_POP \

concatenate_date
	/			/ [date_string = ""]				/ \
date_loop / ;			/						/ STACK_POP \
	/ ,			/						/ STACK_POP \
	/ <any-token>		/ [date_string = date_string || token_value || " "] LEX	/ date_loop \
	/ <no-token>		/ ERROR(8)					/ STACK_POP \

check_date
	/ <okdate>		/ 						/ STACK_POP \
	/			/ ERROR(28)			 		/ STACK_POP \

default_shiftlims	/ ;		/ LEX [default.shift_limit(x) = t]			/ STACK_POP \
	/ ,			/ LEX [default.shift_limit(x) = t] bump_x		/ default_shiftlims \
	/			/ PUSH(default_shiftlims)				/ number \


shiftlims	/ ;			/ LEX [user.shift_limit(x) = t]			/ STACK_POP \
	/ ,			/ LEX [user.shift_limit(x) = t] bump_x			/ shiftlims \
	/			/ PUSH(shiftlims)					/ number \

default_warn_doll	/ ;		/ LEX [default.warn_dollars = t]			/ STACK_POP \
	/ <any-token>		/ ERROR(7) NEXT_STMT				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \

warn_doll	/ ;			/ LEX [user.warn_dollars = t]				/ STACK_POP \
	/ <any-token>		/ ERROR(7) NEXT_STMT				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \

default_user_warn_doll	/ ;	/ LEX [default.user_warn_dollars = t]			/ STACK_POP \
	/ <any-token>		/ ERROR(7) NEXT_STMT				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \

user_warn_doll/ ;			/ LEX [user.user_warn_dollars = t]			/ STACK_POP \
	/ <any-token>		/ ERROR(7) NEXT_STMT				/ STACK_POP \
	/ <no-token>		/ ERROR(5)					/ RETURN \


   ++*/

/* format: style4 */
cv_pmf: procedure;

/* automatic */

dcl  (APstmt, APtoken, areap, pdtep, pdtp, pmfp) ptr;

dcl  1 default aligned like user;
dcl  1 ats (0:1) aligned like user.at;			/* ats (0) - ON bits, ats (1) - OFF bits */

dcl  authorization_value (2) bit (72) aligned;
dcl  access_ceiling bit (72) aligned;
dcl  argc fixed bin;
dcl  argx fixed bin;
dcl  created_table_segment bit (1) aligned;
dcl  date_string char (64) varying;
dcl  dn char (168);
dcl  (supplied_en, pmf_en, pdt_en) char (32);
dcl  (i, n) fixed bin;
dcl  (time, time_now) fixed bin (71);
dcl  bitc fixed bin (24);
dcl  ap ptr;
dcl  al fixed bin (21);
dcl  ec fixed bin (35);
dcl  code fixed bin (35);
dcl  Default_Group char (8) aligned;
dcl  length_of_project_name fixed bin;
dcl  t float bin;
dcl  x fixed bin;
dcl  sx fixed bin;					/* "state index" - whether attribute bit is ON or OFF */
dcl  have_anon bit (1) init ("0"b);
dcl  (default_subsystem, user_subsystem) char (64) aligned; /* put off packing initproc and subsystem together until
						   closing user entry */
dcl  fb35 fixed bin (35);
dcl  (satp, satep) pointer;
dcl  old_pdtp pointer;
dcl  can_check_old_pdt bit (1) aligned init ("0"b);
dcl  project_dir_acc bit (72) aligned;
dcl  project_dir_acc_name char (200);

/* based */

dcl  bchr char (al) unal based (ap);

/* builtin */

declare  (addr, addwordno, bool, character, clock, collate, dimension, divide, index, length, max, null, 
         rtrim, string, substr, verify) builtin;

/* conditions */

declare  cleanup condition;

/* entries */

dcl  aim_check_$greater_or_equal entry (bit (72) aligned, bit (72) aligned) returns (bit (1) aligned);
dcl  aim_check_$greater entry (bit (72) aligned, bit (72) aligned) returns (bit (1) aligned);
dcl  convert_access_class_$to_string_short entry (bit (72) aligned, character (*), fixed binary (35));
dcl  convert_access_class_$to_string_range_short entry ((2) bit (72) aligned, character (*),
	fixed binary (35));
dcl  convert_access_class_$from_string_range entry ((2) bit (72) aligned, char (*), fixed bin (35));
dcl  system_info_$access_ceiling entry (bit (72) aligned);
dcl  cu_$arg_count entry (fixed bin, fixed bin (35));
dcl  cu_$arg_ptr entry (fixed bin, ptr, fixed bin (21), fixed bin (35));
dcl  cv_dec_check_ entry (char (*), fixed bin (35)) returns (fixed bin (35));
dcl  cv_float_ entry (char (*), fixed bin (35), float bin (27));
dcl  get_wdir_ entry () returns (char (168) aligned);
dcl  get_group_id_ entry () returns (char (32) aligned);
dcl  hcs_$status_minf entry (char (*), char (*), fixed bin (1), fixed bin (2), fixed bin (24), fixed bin (35));
dcl  hcs_$get_access_class entry (char (*), char (*), bit (72) aligned, fixed bin (35));
dcl  expand_pathname_ entry (char (*), char (*), char (*), fixed bin (35));
dcl  expand_pathname_$add_suffix entry (char (*), char (*), char (*), char (*), fixed bin (35));
dcl  convert_date_to_binary_ entry (char (*), fixed bin (71), fixed bin (35));
dcl  com_err_ entry options (variable);
dcl  com_err_$suppress_name entry () options (variable);
dcl  lex_error_ entry options (variable);
dcl  lex_string_$init_lex_delims entry (char (*), char (*), char (*), char (*), char (*), bit (*),
	char (*) var, char (*) var, char (*) var, char (*) var);
dcl  lex_string_$lex entry (ptr, fixed bin, fixed bin, ptr, bit (*), char (*), char (*), char (*), char (*), char (*),
	char (*) var, char (*) var, char (*) var, char (*) var, ptr, ptr, fixed bin (35));
dcl  suffixed_name_$new_suffix entry (char (*), char (*), char (*), char (32), fixed bin (35));
dcl  translator_temp_$get_segment entry (char (*), ptr, fixed bin (35));
dcl  translator_temp_$release_all_segments entry (ptr, fixed bin (35));
dcl  hcs_$delentry_seg entry (ptr, fixed bin (35));
dcl  hcs_$make_seg entry (char (*), char (*), char (*), fixed bin (5), ptr, fixed bin (35));
dcl  hcs_$truncate_seg entry (ptr, fixed bin, fixed bin (35));
dcl  initiate_file_ entry (char (*), char (*), bit (*), ptr, fixed bin (24), fixed bin (35));
dcl  terminate_file_ entry (ptr, fixed bin (24), bit (*), fixed bin (35));
dcl  pathname_ entry (char (*), char (*)) returns (char (168));

/* internal static */

dcl  ON fixed bin initial (0) internal static;
dcl  OFF fixed bin initial (1) internal static;
dcl  first bit (1) initial ("1"b) internal static;
dcl  BIGFLO float bin init (1e37) internal static;
dcl  NEVER fixed bin (71) init (4418064000000000) static options (constant);
dcl  LEGAL char (70) aligned init
	("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'_-^`~ ") internal static;
dcl  my_name char (6) initial ("cv_pmf") internal static;
dcl  (LEXDLM, LEXCTL) char (128) var internal static;
dcl  BREAKS char (128) var internal static;
dcl  IGBREAKS char (128) var internal static;
dcl  LIVE_PDT_DIR char (168) int static options (constant) init (">system_control_1>pdt");

/* external static */

dcl  (error_table_$translation_failed,
     error_table_$badopt,
     error_table_$noentry,
     error_table_$too_many_args,
     error_table_$zero_length_seg,
     error_table_$noarg,
     error_table_$bad_conversion) fixed bin (35) external static;

dcl  cv_pmf_severity_ fixed bin (35) external init (0);

%page;
%include access_mode_values;
%page;
%include pdt;
%page;
%include sat;
%page;
%include terminate_file;
%page;
%include user_attributes;
%page;
/* program */

	dn, supplied_en, pmf_en, pdt_en = "";

	pmfp = null;				/* Initialize for cleanup handler */
	pdtp = null;				/* .. */
	areap = null;				/* .. */
	old_pdtp = null ();
	created_table_segment = ""b;

	on cleanup begin;
		call clean_up;
		cv_pmf_severity_ = 5;
	     end;

	call cu_$arg_count (argc, ec);
	if ec ^= 0 then do;
	     call com_err_ (ec, my_name);
	     go to severity_5_failure;
	end;

	if argc = 0 then do;
give_usage:    call com_err_$suppress_name (ec, my_name, "Usage: cv_pmf PMF (-brief|-bf|-long|-lg)");
	     go to severity_5_failure;
	end;

	do argx = 1 to argc;
	     call cu_$arg_ptr (argx, ap, al, ec);
	     if ec ^= 0 then do;
argument_error:
		call com_err_ (ec, my_name, "^a", bchr);
		go to severity_5_failure;
	     end;
	     if character (bchr, 1) ^= "-" then do;
		if supplied_en ^= ""
		then do;
		     call com_err_ (error_table_$too_many_args, my_name, "Only one pathname may be given. ^a was the second.", bchr);
		     go to severity_5_failure;
		end;

		call expand_pathname_ (bchr, dn, supplied_en, ec);
		if ec ^= 0 then do;
path_error:
		     call com_err_ (ec, my_name, "^a", bchr);
		     go to severity_5_failure;
		end;
		call expand_pathname_$add_suffix (bchr, "pmf", dn, pmf_en, ec);
		if ec ^= 0 then go to path_error;

		call suffixed_name_$new_suffix (supplied_en, "pmf", "pdt", pdt_en, ec); /* if we get this far, how can we fail? */
		if ec ^= 0			/* still, let's have a look */
		then go to path_error;

	     end;					/* Pathname case */
	     else if bchr = "-bf" then SERROR_CONTROL = "01"b;
	     else if bchr = "-brief" then SERROR_CONTROL = "01"b;
	     else if bchr = "-long" | bchr = "-lg" then SERROR_CONTROL = "10"b;
	     else if bchr = "-severity" | bchr = "-sv" then do;
		if argx >= argc then do;
		     call com_err_ (error_table_$noarg, my_name, "After ""^a"".", bchr);
		     go to severity_5_failure;
		end;
		argx = argx + 1;
		call cu_$arg_ptr (argx, ap, al, ec);
		fb35 = cv_dec_check_ (bchr, ec);
		if ec ^= 0 | fb35 < 0 | fb35 > 5 then do;
		     call com_err_ (error_table_$bad_conversion, my_name,
			"Severity must be an integer in the range 0 - 5, not ""^a"".", bchr);
		     go to severity_5_failure;
		end;
		MIN_PRINT_SEVERITY = fb35;
	     end;
	     else do;
		ec = error_table_$badopt;
		go to argument_error;
	     end;
	end;					/* argument processing */

	if supplied_en = "" then go to give_usage;

	call system_info_$access_ceiling (access_ceiling);
	time_now = clock ();

	call initiate_file_ (dn, pmf_en, R_ACCESS, pmfp, bitc, ec);
	if ec = error_table_$noentry
	then if pmf_en ^= supplied_en
	     then do;
		call initiate_file_ (dn, supplied_en, R_ACCESS, pmfp, bitc, ec);
		if ec = 0
		then do;
		     call com_err_ (0, my_name, "converting ^a. Please type ""help cv_pmf.changes"".",
			pathname_ (dn, supplied_en));
		     pmf_en = supplied_en;
		end;
	     end;
	if ec ^= 0
	then do;
pmf_error:
	     call com_err_ (ec, my_name, "^a.", pathname_ (dn, pmf_en));
	     go to severity_5_failure;
	end;

	n = divide (bitc + 8, 9, 24, 0);
	if n = 0 then do;
	     ec = error_table_$zero_length_seg;
	     go to pmf_error;
	end;
	dn = get_wdir_ ();
	call hcs_$make_seg (dn, pdt_en, "", 1010b, pdtp, ec);
	created_table_segment = (ec = 0);
	if pdtp = null then do;
pdt_error:
	     call com_err_ (ec, my_name, "^a", pathname_ (dn, pdt_en));
	     go to severity_5_failure;
	end;

	call hcs_$truncate_seg (pdtp, 0, ec);
	if ec ^= 0 then go to pdt_error;

	pdt.author.proc_group_id = get_group_id_ ();	/* Initialize the header of the new pdt */
	pdt.author.table = "PDT";
	pdt.author.w_dir = substr (dn, 1, length (pdt.author.w_dir));
	pdt.author.lock = ""b;
	pdt.author.last_install_time = 0;
	pdt.version = PDT_version;
	pdt.max_size = 1019;

	pdtep = addr (pdt.user (1));			/* Zero the defaults.. */
	default = user;				/* .. the lazy way */
	pdtep = addr (default);			/* Set up pdtep to point at the defaults. */
	default.person_id = "";			/* Initialize defaults. */
	default.password = " ";
	default.initial_procedure = "process_overseer_";
	default.ip_len = 17;			/* number of chars in initproc name */
	default.uflags.ip_given = "1"b;
	default.outer_module = "";			/* dft outer module will be supplied by ans svc */
	default.group = "";
	default.dollar_charge = 0e0;
	default.last_login_unit = " ";

	user.interactive.charge (*) = 0e0b;
	user.absentee.charge (*) = 0e0b;
	user.iod.charge (*) = 0e0b;
	user.devices (*) = 0e0b;

	default.absolute_limit, default.dollar_limit = BIGFLO;
	default.absolute_spent = 0e0;
	default.absolute_cutoff = NEVER;
	default.user_warn_days = 10;
	default.user_warn_pct = 10;
	default.user_warn_dollars = 10e0;
	default.warn_days = 10;
	default.warn_pct = 10;
	default.warn_dollars = 10e0;
	default.bump_grace = 2880;
	default.low_ring, default.default_ring = 4;
	default.high_ring = 5;
	do i = 0 to 7;
	     default.shift_limit (i) = BIGFLO;
	end;
	default.home_dir = "";
	default.user_authorization = ""b;		/* system low by default */
	Default_Group = "";

	call translator_temp_$get_segment (my_name, areap, code);
	if areap = null then do;
	     call com_err_ (code, my_name, "While making a temporary segment in the process directory.");
	     go to severity_5_failure;
	end;

	if first then do;
	     BREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24) || "()*,:;^";
	     IGBREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24);
	     call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b, BREAKS, IGBREAKS, LEXDLM, LEXCTL);
	     first = "0"b;
	end;

	call lex_string_$lex (pmfp, n, 0, areap, "100"b,
	     """", """", "/*", "*/", ";", BREAKS, IGBREAKS,
	     LEXDLM, LEXCTL, APstmt, APtoken, ec);

	Pthis_token = APtoken;
	call SEMANTIC_ANALYSIS ();
	pdt.project_dir = ">user_dir_dir>" || pdt.project_name;

abort:	if MERROR_SEVERITY > 2 then do;
	     call com_err_ (error_table_$translation_failed, my_name, pmf_en);
	     if created_table_segment then bitc = -1;	/* delete it */
	     else bitc = 0;
	end;
	else bitc = (PDT_header_lth + PDT_entry_lth * pdt.current_size) * 36;
	if bitc >= 0 then do;			/* if not deleting it */
	     call terminate_file_ (pdtp, bitc, TERM_FILE_TRUNC_BC_TERM, ec);
	     if ec ^= 0 then do;
		call com_err_ (ec, my_name, "Unable to set bitcount on ^a to ^d.", pathname_ (dn, pdt_en), bitc);
		go to severity_5_failure;
	     end;
	end;

	cv_pmf_severity_ = MERROR_SEVERITY;
	call clean_up;
	return;

severity_5_failure:
	call clean_up;
	cv_pmf_severity_ = 5;
	return;

%page;
clean_up:
     procedure;

	if pmfp ^= null
	then call terminate_file_ (pmfp, (0), TERM_FILE_TERM, (0));

	if old_pdtp ^= null
	then call terminate_file_ (old_pdtp, (0), TERM_FILE_TERM, (0));

	if pdtp ^= null				/* delete or truncate */
	then if created_table_segment
	     then do;
		call hcs_$delentry_seg (pdtp, (0));
		pdtp = null;
	     end;
	     else call terminate_file_ (pdtp, 0, TERM_FILE_TRUNC_BC_TERM, (0));

	if areap ^= null
	then call translator_temp_$release_all_segments (areap, (0));

     end clean_up;

/* SYNTAX FUNCTIONS */

path_name: proc () returns (bit (1) aligned);
dcl  ec fixed bin (35);
dcl  dn char (168);
dcl  en char (32);

	call expand_pathname_ (token_value, dn, en, ec);
	if ec ^= 0 then return ("0"b);
	else return ("1"b);

     end path_name;

onetoseven: proc () returns (bit (1) aligned);
dcl  i fixed bin;

	if token.Lvalue ^= 1 then return ("0"b);
	i = index ("1234567", token_value);
	if i = 0 then return ("0"b);
	token.Nvalue = i;
	return ("1"b);

     end onetoseven;

floating_number: proc () returns (bit (1) aligned);
dcl  ec fixed bin (35);
dcl  foo float bin;

	if verify (token_value, "0123456789.-") ^= 0 then
	     return ("0"b);
	call cv_float_ (token_value, ec, foo);
	if ec ^= 0 then return ("0"b);
	else return ("1"b);

     end floating_number;

okdate: proc () returns (bit (1) aligned);
dcl  ec fixed bin (35);

	if date_string = "now" then do;
	     time = time_now;
	     return ("1"b);
	end;
	if date_string = "open" then go to x1;
	if date_string = "never" then do;
x1:	     time = NEVER;
	     return ("1"b);
	end;
	call convert_date_to_binary_ ((date_string), time, ec);
	if ec ^= 0 then return ("0"b);
	return ("1"b);

     end okdate;

increment: proc () returns (bit (1) aligned);

	if token_value = "never" then x = 0;
	else if token_value = "yearly" then x = 3;
	else if token_value = "daily" then x = 1;
	else if token_value = "monthly" then x = 2;
	else if token_value = "cyear" then x = 4;
	else if token_value = "fyear" then x = 5;
	else if token_value = "weekly" then x = 6;
	else return ("0"b);
	return ("1"b);

     end increment;

authorization_string:
     procedure () returns (bit (1) aligned);

	call convert_access_class_$from_string_range (authorization_value, token_value, code);
	return (code = 0);

     end authorization_string;

check_default_authorization:
     procedure;

declare  temp_string char (200);

	/*** First check complete disjunction */

	if ^can_check_old_pdt then return;
	call convert_access_class_$to_string_range_short (project.project_authorization, temp_string, (0));
	if ^aim_check_$greater_or_equal (default.user_authorization (2), project.project_authorization (1))
	     | ^aim_check_$greater_or_equal (project.project_authorization (2), default.user_authorization (1))
	then do;
	     call error_in_general (38, temp_string, ""); /* completely disjoint */
	     return;
	end;

	if aim_check_$greater (default.user_authorization (2), project.project_authorization (2))
	then call error_in_general (39, temp_string, ""); /* may not be able to log in */
	if aim_check_$greater (project.project_authorization (1), default.user_authorization (1))
	then call error_in_general (40, temp_string, "");
	return;
     end check_default_authorization;

check_user_authorization:
     procedure;

declare  temp_string char (200);

	/*** First check complete disjunction */

	if ^can_check_old_pdt then return;
	call convert_access_class_$to_string_range_short (project.project_authorization, temp_string, (0));

	if ^aim_check_$greater_or_equal (user.user_authorization (2), project.project_authorization (1))
	     | ^aim_check_$greater_or_equal (project.project_authorization (2), user.user_authorization (1))
	then do;
	     call error_in_person (41, temp_string);	/* completely disjoint */
	     return;
	end;

	if aim_check_$greater (user.user_authorization (2), project.project_authorization (2))
	then call error_in_person (42, temp_string);	/* may not be able to log in */
	if aim_check_$greater (project.project_authorization (1), user.user_authorization (1))
	then call error_in_person (43, temp_string);
	return;

     end check_user_authorization;

/* SEMANTIC FUNCTIONS */

set_project_name:
     procedure;

	pdt.project_name = token_value;
	length_of_project_name = length (token_value);

	if verify (token_value, LEGAL) ^= 0
	then call ERROR (21);
	else if index (substr (LEGAL, 1, 36), substr (token_value, 1, 1)) = 0
	then call ERROR (21);

	if length (token_value) > PDT_project_name_length
	then do;
	     call ERROR (24);			/* Project name > 9 characters */
	     length_of_project_name = PDT_project_name_length;
	end;

	call find_old_pdt;
	return;
     end set_project_name;


set_ip_ss: proc (ip_ss, ip_ss_given, ip_ss_len);
dcl  ip_ss char (64) aligned;
dcl  ip_ss_given bit (1) unaligned;
dcl  ip_ss_len fixed bin (17) unaligned;

/* check the 64 character limit */

	if token.Lvalue > 64 then do;
	     call ERROR (31);
	     return;
	end;

/* copy the string and its length, and turn on the switch that says it was given */

	ip_ss = token_value;
	ip_ss_len = token.Lvalue;
	ip_ss_given = "1"b;

	return;

     end set_ip_ss;


set_string: proc (keyword, field, value);
dcl  keyword char (*);
dcl  field char (*) aligned;
dcl  value char (*);

	if length (value) <= length (field) then
	     field = value;
	else if error_control_table (34).severity >= MIN_PRINT_SEVERITY then
	     call lex_error_ (34, SERROR_PRINTED (34), (error_control_table (34).severity),
		MERROR_SEVERITY, null, Pthis_token, SERROR_CONTROL, (error_control_table (34).message),
		(error_control_table (34).brief_message), keyword, length (field));
	else do;
	     MERROR_SEVERITY = max (MERROR_SEVERITY, error_control_table (34).severity);
	     SERROR_PRINTED (34) = "1"b;
	end;
     end;

bump_x: proc;

	if x = 0 then do;
	     call ERROR (7);			/* Error in limit */
	     call NEXT_STMT;			/* Get to next statement. */
	     Ptoken, Pthis_token = Pthis_token -> token.Plast;
						/* Back up so will find semicolon */
	     return;
	end;
	x = x + 1;
	if x = 8 then x = 0;

     end bump_x;

open: proc;

dcl  i fixed bin;
dcl  p ptr;

	if pdt.current_size = pdt.max_size then do;
	     call ERROR (9);
	     go to abort;
	end;

	if token.Lvalue > PDT_person_id_length
	then call ERROR (26);

	if verify (token_value, LEGAL) ^= 0
	then call ERROR (10);
	else if index (substr (LEGAL, 11, 26), substr (token_value, 1, 1)) = 0
	then call ERROR (11);

/* now check to see whether or not personid is unique in table */

	do i = 1 to pdt.current_size;
	     p = addr (pdt.user (i));			/* get pointer to next entry */
	     if token_value = p -> user.person_id
	     then call ERROR (13);
	end;

	pdt.current_size, pdt.n_users = pdt.n_users + 1;
	pdtep = addr (pdt.user (pdt.n_users));
	user = default;
	user_subsystem = default_subsystem;
	user.state = 1;
	user.person_id = token_value;

     end open;

open_anon: proc;

	if pdt.current_size = pdt.max_size then do;
	     call ERROR (9);
	     go to abort;
	end;

	if have_anon
	then call ERROR (12);

	have_anon = "1"b;
	pdt.current_size, pdt.n_users = pdt.n_users + 1;
	pdtep = addr (pdt.user (pdt.n_users));
	user = default;
	user_subsystem = default_subsystem;
	user.state = 1;
	user.person_id = "*";
	user.home_dir = "";				/* Default does not apply */
	user.initial_procedure = "";			/* .. without a warning */

     end open_anon;

close: proc;

dcl  ec fixed bin (35);
dcl  (hdd, hddd) char (168);
dcl  (hdde, hde) char (32);

	if user.ip_len + user.ss_len > 64 then
	     call ERROR (32);
	else substr (user.initial_procedure, user.ip_len + 1, user.ss_len) =
		substr (user_subsystem, 1, user.ss_len);

	if pdt.current_size = 0 then return;

	if user.person_id = "*" then do;
	     if user.home_dir = "" then do;
		call ERROR (14);
		user.home_dir = ">user_dir_dir>" || pdt.project_name;
	     end;
	     if user.password = "" then call ERROR (15);

	     if user.initial_procedure = "" then do;
		call ERROR (16);
		user.initial_procedure = default.initial_procedure;
		user.dont_call_init_admin = default.dont_call_init_admin;
	     end;
	end;
	else do;					/* Normal user. */
	     if user.password ^= "" then do;
		call error_in_person (27, "");
		user.password = "";
	     end;

	     if user.home_dir = ""
	     then user.home_dir = ">user_dir_dir>" ||
		     substr (pdt.project_name, 1, length_of_project_name) ||
		     ">" || user.person_id;
	end;

	if user.low_ring > user.high_ring |
	     user.low_ring > user.default_ring |
	     user.default_ring > user.high_ring |
	     user.low_ring < 1 |
	     user.high_ring > 7
	then call error_in_person (37, "");

	if substr (user.home_dir, 1, 1) ^= ">"
	     & substr (user.home_dir, 1, 5) ^= "[pd]>"
	then call error_in_person (17, (user.home_dir));
	else do;
	     call expand_pathname_ ((user.home_dir), (dn), (supplied_en), ec);
	     if ec ^= 0 then
		call error_in_person (18, (user.home_dir));
	end;

	user.at.eo_ok = ^(user.at.eo_ok);
	user.at.sb_ok = ^(user.at.sb_ok);
	user.at.pm_ok = ^(user.at.pm_ok);

/* if both bits off */
	if bool (user.at.pm_ok, user.at.sb_ok, "1000"b) then
	     call error_in_person (19, "");

	if user.bumping then
	     if ^user.pm_ok then
		call error_in_person (20, "");

	if user.at.igroup then
	     if user.group = "" then
		if Default_Group = "" then do;
		     call error_in_person (30, "");
		     user.at.igroup = "0"b;
		end;
		else user.group = Default_Group;

	if user.group ^= "" then user.at.igroup = "1"b;

	if user.at.save_on_disconnect then		/* as documented in MAM Project, setting default of -save */
	     user.at.disconnect_ok = "1"b;		/* implies giving permission for -save */

/* the following call to adjust_cutoff_ has been commented out.  The reason
   is that if we allow the installation process to adjust_cutoff_, then
   it is possible for a project administrator to change to a finer cutoff gradation,
   and cause the users spending against cutoff to be reset at table installation time.
   e.g. to put user with absolute cutoff of $400 dollars into $20 daily cutoff,
   specify "cutoff: 20,<yesterday>,daily;"
   If the adjust_cutoff_ in cv_pmf_ remains, the users absolute spent (spent against cutoff)
   will not be reset until the next cutoff date occurs.  In above example, if
   user had spent more than $20 when the cutoff was 'never', the user would not
   be able to log in until the day following pdt installation.
   If someone decides it's a bad idea to support this, put the call back in!
   MEANWHILE, let's at least give a warning if the date is in the past... */

/* 	     call adjust_cutoff_ (pdtep, time_now);	/* fixup cutoff dates */

	if user.absolute_increm > 0 then		/* not absolute cutoff */
	     if user.absolute_cutoff < time_now then	/* at a time in the past */
		call error_in_person (36, "");


	call expand_pathname_ ((user.home_dir), hdd, hde, code);
	if code = 0 then do;
	     call hcs_$status_minf (hdd, hde, (1), (0), (0), code);
	     call expand_pathname_ (hdd, hddd, hdde, (0));
	     call hcs_$get_access_class (hddd, hdde, project_dir_acc, code);
	     if code ^= 0 then project_dir_acc = ""b;
	     if code = error_table_$noentry then do;
		if aim_check_$greater (user.user_authorization (1), project_dir_acc)
		then do;
		     project_dir_acc_name = "";
		     call convert_access_class_$to_string_short (project_dir_acc, project_dir_acc_name, (0));
		     call error_in_person (44, project_dir_acc_name);
		end;
	     end;
	end;

	return;
     end close;

error_in_person:
     procedure (bv_errorx, bv_string);

/* parameters */

dcl  (bv_errorx fixed bin,
     bv_first_string char (*),
     bv_string char (*)) parameter;

/* automatic */

dcl  ex fixed bin;
dcl  first_string char (500);

/* program */

	first_string = user.person_id;
	go to common;

error_in_general:
     entry (bv_errorx, bv_first_string, bv_string);

	first_string = bv_first_string;

common:
	ex = bv_errorx;
	if error_control_table (ex).severity >= MIN_PRINT_SEVERITY then
	     call lex_error_ (ex, SERROR_PRINTED (ex), (error_control_table (ex).severity),
		MERROR_SEVERITY, null, Pthis_token, SERROR_CONTROL, (error_control_table (ex).message),
		(error_control_table (ex).brief_message), first_string, bv_string);
	else do;
	     MERROR_SEVERITY = max (MERROR_SEVERITY, error_control_table (ex).severity);
	     SERROR_PRINTED (ex) = "1"b;
	end;

     end error_in_person;

find_old_pdt:
     procedure;

	call initiate_file_ (LIVE_PDT_DIR, rtrim (pdt.project_name) || ".pdt", R_ACCESS, old_pdtp, (0), code);
	if code ^= 0 then return;			/* can_check_old_pdt still = 0 */

	if old_pdtp -> pdt.sat_version ^= SAT_version
	then do;
	     call terminate_file_ (old_pdtp, (0), TERM_FILE_TERM, (0));
	     return; /* can_check_old_pdt still equals 0 */
	end; 
	can_check_old_pdt = "1"b;

	satep = addwordno (addr (old_pdtp -> pdt.satentry), -24); /* magic amount of PDT abstracted from front */
	return;
     end find_old_pdt;


%page;
dcl  1 error_control_table (44) aligned int static options (constant),
       2 severity fixed bin (17) unal init (
	  (8) 3,					/* 1-8 */
	  3,					/* 9 */
	  3,					/* 10 */
	  1,					/* 11 */
	  (2) 3,					/* 12-13 */
	  (3) 1,					/* 14-16 */
	  (2) 3,					/* 17-18 */
	  (2) 1,					/* 19-20 */
	  (6) 3,					/* 21-26 */
	  1,					/* 27 */
	  3,					/* 28 */
	  (2) 1,					/* 29-30 */
	  (2) 3,					/* 31-32 */
	  1,					/* 33 */
	  (2) 3,					/* 34-35 */
	  1,					/* 36 */
	  3,					/* 37 */
	  (7) 1),					/* 38-44 */
       2 Soutput_stmt bit (1) unaligned initial (
	  (4) (1)"1"b,				/* 1-4 */
	  "0"b,					/* 5 */
	  (8) (1)"1"b,				/* 6-13 */
	  (3) (1)"0"b,				/* 14-16 */
	  (10) (1)"1"b,				/* 17-26 */
	  "0"b,					/* 27 */
	  (2) (1)"1"b,				/* 28-29 */
	  "0"b,					/* 30 */
	  "1"b,					/* 31 */
	  (2) (1)"1"b,				/* 32-33 */
	  "1"b,					/* 34 */
	  "0"b,					/* 35 */
	  "0"b,					/* 36 */
	  "0"b,					/* 37 */
	  (6) (1)"1"b,				/* 38-43 */
	  "0"b),					/* 44 */
       2 message char (100) var init
	  ("Unrecognizable statement.",		/* 1 */
	  "PMF does not begin with a valid ""Projectid"" statement.", /* 2 */
	  "Unrecognizable statement.",		/* 3 */
	  "Unknown attribute ""^a"".",		/* 4 */
	  "Premature end of PMF encountered.",		/* 5 */
	  "Invalid item encountered where number expected: ""^a"".", /* 6 */
	  "Invalid item encountered in limit: ""^a"".",	/* 7 */
	  "Invalid format for cutoff specification.",	/* 8 */
	  "Too many users declared in PMF. Maximum is 1019.", /* 9 */
	  "Invalid letter in personid ""^a"".",		/* 10 */
	  "Personid ""^a"" does not begin with a capital letter.", /* 11 */
	  "Only one anonymous user may be specifed in a PMF.", /* 12 */
	  "Personid ""^a"" is specified more than once in the PMF.", /* 13 */
	  "Anonymous user had no ""homedir"" statement; assuming project directory.", /* 14 */
	  "Anonymous user has no password specified.",	/* 15 */
	  "Anonymous user has no initial procedure specified.", /* 16 */
	  "Home directory for user ""^a"" does not begin with "">"" or ""[pd]>"": ^a.", /* 17 */
	  "Home directory for user ""^a"" has illegal syntax: ^a.", /* 18 */
	  "User ""^a"" has both ""no_primary"" and ""no_secondary"" and will be unable to log in.", /* 19 */
	  "User ""^a"" has ""preempting"" and ""no_primary"" attributes.", /* 20 */
	  "Syntax error in project identifier.",	/* 21 */
	  "Invalid specification of path name.",	/* 22 */
	  "Syntax error in ""^a"" statement.",		/* 23 */
	  "Project name ""^a"" is longer than the limit of 9 characters.", /* 24 */
	  "Unable to convert authorization string ""^a"".", /* 25 */
	  "User name ""^a"" is longer than the limit of 22 characters.", /* 26 */
	  "Personid ""^a"" has a password that will be ignored.", /* 27 */
	  "Invalid date/time in cutoff statement.",	/* 28 */
	  "The ^a statement is obsolete and has been ignored.", /* 29 */
	  "Personid ""^a"" has ""igroup"" but no group specified.", /* 30 */
	  "Pathname of initial procedure or subsystem > 64 characters:^/^a", /* 31 */
	  "Sum of initial procedure and subsystem lengths > 64 characters, in preceding user entry (or header)", /* 32 */
	  "The ""^a"" attribute is obsolete and has been ignored.", /* 33 */
	  "The maximum length of the ""^a"" field is ^d.",/* 34 */
	  "Text follows logical end of PMF.",		/* 35 */
	  "Incremental cutoff is in the past. Spending against cutoff will be reset at user's next login.", /* 36 */
	  "Bad ring order. ""ring: low, high {,default};"" must have values 1 <= low <= default <= high <= 7.", /* 37 */
	  "Default authorization range is disjoint from project authorization range ^a.",
	  "Default max authorization is out of project range ^a.",
	  "Default min authorization is out of project range ^a.",
	  "User ^a authorization range is disjoint from project authorization range ^a.",
	  "User ^a max authorization is out of project range ^a.",
	  "User ^a min authorization is out of project range ^a.",
             "User ^a will need an upgraded home dir, which you must create manually."),

       2 brief_message char (20) var init (
	  (3) (1)"",				/* 1-3 */
	  "^a",					/* 4 */
	  "",					/* 5 */
	  (2) (1)"^a",				/* 6-7 */
	  (2) (1)"",				/* 8-9 */
	  (2) (1)"^a",				/* 10-11 */
	  "",					/* 12 */
	  "^a",					/* 13 */
	  (3) (1)"",				/* 14-16 */
	  (2) (1)"^a bad homedir ^a",			/* 17-18 */
	  (2) (1)"^a",				/* 19-20 */
	  (3) (1)"",				/* 21-23 */
	  (4) (1)"^a",				/* 24-27 */
	  "Invalid date/time",			/* 28 */
	  "",					/* 29 */
	  "^a",					/* 30 */
	  "^a",					/* 31 */
	  "",					/* 32 */
	  "^a",					/* 33 */
	  "",					/* 34 */
	  "",					/* 35 */
	  "",					/* 36 */
	  "",					/* 37 */
	  (3) (0)"",				/* 38-43 */
	  "^a",
	  "^a",
	  "^a",
	  "^a");


/* ======================================================== */
