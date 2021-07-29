/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */



/* HISTORY COMMENTS:
  1) change(87-06-10,GDixon), approve(87-07-10,MCR7681),
     audit(87-07-10,Parisek), install(87-08-04,MR12.1-1055):
     Added cpm_ data.
                                                   END HISTORY COMMENTS */


/* format: off */

/* Created:  March 1985 by G. Palter to hold new structure that would not
      fit into the other structure_library_N_ modules */

structure_library_6_:
     procedure ();

/* First come all the structures */

begin; /* to avoid possible conflicts */
   %include cpm_control_point_data;
   %include cpm_ctrl_pt_meters;
   %include process_usage;
      call add ("control_point_data", addr (p -> control_point_data));
   end;

%include mail_format;
   text_length = 0; /* avoid WARNING 307 */
   call add ("mail_format", addr (p -> mail_format));

%include mseg_message;
   call add ("message_block_header", addr (p -> message_block_header));
   call add ("message_descriptor", addr (p -> message_descriptor));
   call add ("first_message_block", addr (p -> first_message_block));
   call add ("other_message_block", addr (p -> other_message_block));

%include mseg_message_info;
   call add ("mseg_message_info", addr (p -> mseg_message_info));

%include mseg_return_args;
   call add ("mseg_return_args", addr (p -> mseg_return_args));

%include mseg_segment;
%include mseg_wakeup_state;
   call add ("mseg_segment", addr (p -> mseg_segment));

%include "_ssu_sci";
   call add ("sci", addr (p -> sci));

begin; /* both tty_ and tc_io_ use the same name for the attach data */
   %include tc_io_attach_data_;
      dcl 1 tc_io_attach_data aligned like attach_data based;
      call add ("tc_io_attach_data", addr (p -> tc_io_attach_data));
   end;

begin; /* both tty_ and tc_io_ use the same name for the attach data */
   %include tty_attach_data_;
      dcl 1 tty_attach_data aligned like attach_data based;
      call add ("tty_attach_data", addr (p -> tty_attach_data));
   end;

/* Followed by the include file containing the code to do the work */

dcl  WHOAMI char (32) internal static options (constant) init ("structure_library_6_");

/* Builtins */

dcl  (currentsize, divide, hbound, pointer) builtin;

%include structure_library_code;

	end structure_library_6_;
