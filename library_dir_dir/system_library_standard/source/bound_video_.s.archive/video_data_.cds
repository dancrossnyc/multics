/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright (c) 1987 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */



/* HISTORY COMMENTS:
  1) change(86-11-26,LJAdams), approve(86-11-26,MCR7485),
     audit(86-12-16,Margolin), install(87-01-06,MR12.0-1255):
     Changed version number for video.
                                                   END HISTORY COMMENTS */


/* terminal control and window control data segment. */

/* June 1981, Benson I. Margulies */
/* Modified 8 October 1983 by Jon A. Rochlis to add version number */
/* Modified 2 June 1984 by JR to add the EXL flags and to remove 
   saved_sus_handler since it is longer needed */
  
/* format: style2 */

video_data_:
     procedure;

%include cds_args;
	declare 1 cdsa		 aligned like cds_args;


	declare create_ips_mask_	 entry (pointer, fixed binary, bit (36) aligned);
	declare create_data_segment_	 entry (pointer, fixed binary (35));
	declare com_err_		 entry () options (variable);

	declare code		 fixed bin (35);
	declare create_ips_mask_err	 condition;

	declare 1 video_text	 aligned,
		2 terminal_switch	 character (32),
		2 shut_mask	 bit (36) aligned,
		2 open_mask	 bit (36) aligned,
		2 only_quit_mask	 bit (36) aligned,
		2 alrm_only_mask	 bit (36) aligned,
		2 as_only_mask	 bit (36) aligned,
		2 error_name	 character (32),
		2 version	           character (12),
                    2 exl_video_system   bit (1)  unaligned,
                    2 mbz                bit (35) unaligned,
		2 pad_end		 bit (0) aligned;

	declare 1 video_static	 aligned,
		2 terminal_iocb	 pointer,
                    2 exl_initialized    bit (1)  unaligned,
                    2 mbz                bit (35) unaligned,
		2 pad_end		 bit (0) aligned;


	declare quit_name		 (4) character (32) aligned static internal
				 init ("quit", "trm_", "sus_", "neti") options (constant);

	declare alrm_only_name	 (1) character (32) aligned static internal init ("alrm") options (constant);

	declare as_only_name	 (3) character (32) aligned static internal init ("trm_", "sus_", "neti")
				 options (constant);

	declare all_name		 (1) character (32) aligned static internal init ("-all") options (constant);

	declare pad_name		 (1) character (32) static internal init ("pad*") options (constant);

	declare ME		 character (32) static internal init ("video_data_") internal static
				 options (constant);

	declare (addr, currentsize, hbound, null, string, unspec)
				 builtin;

	unspec (video_text) = ""b;
	unspec (video_static) = ""b;

	on create_ips_mask_err
	     begin;
		call com_err_ (0, ME, "IPS mask generation failed.");
		go to give_up;
	     end;

	call create_ips_mask_ (addr (quit_name), hbound (quit_name, 1), video_text.only_quit_mask);
	video_text.only_quit_mask = ^video_text.only_quit_mask;
	call create_ips_mask_ (addr (all_name), hbound (all_name, 1), video_text.shut_mask);
	video_text.open_mask = ^video_text.shut_mask;
	call create_ips_mask_ (addr (as_only_name), hbound (as_only_name, 1), video_text.as_only_mask);
	call create_ips_mask_ (addr (alrm_only_name), hbound (alrm_only_name, 1), video_text.alrm_only_mask);
	video_text.error_name = "internal terminal control";

	video_text.terminal_switch = "user_terminal_";
	video_text.version = "MR12";
	video_text.exl_video_system = "0"b; /* video_utils_ will use this when deciding whether or not to call use_exl_video_system */
	video_static.terminal_iocb = null ();
	video_static.exl_initialized = "0"b;

	string (cdsa.switches) = ""b;
	cdsa.switches.separate_static, cdsa.switches.have_static, cdsa.switches.have_text = "1"b;

	cdsa.p (1) = addr (video_text);
	cdsa.len (1) = currentsize (video_text);
	cdsa.struct_name (1) = "video_text";
	cdsa.p (2) = addr (video_static);
	cdsa.len (2) = currentsize (video_static);
	cdsa.struct_name (2) = "video_static";
	cdsa.seg_name = ME;
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (pad_name);

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then call com_err_ (code, ME);
	return;
give_up:
     end video_data_;
