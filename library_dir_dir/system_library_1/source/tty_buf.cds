/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */


tty_buf:	 proc;
	 

/* Program to create ring-0 tty_buf segment

   Written Jan. 81 by J. Bongiovanni								*/
	 

dcl 1 cdsa like cds_args aligned;
dcl code fixed bin (35);
dcl p ptr;
	 

dcl my_name char (7) init ("tty_buf") int static options (constant);
	 

%include cds_args;
%include tty_buf;

dcl com_err_ entry options (variable);
dcl create_data_segment_ entry (ptr, fixed bin(35));
dcl get_temp_segment_ entry (char(*), ptr, fixed bin(35));
dcl release_temp_segment_ entry entry options(variable);

dcl cleanup condition;



     ttybp = null();
     on cleanup call release_temp_segment_ (my_name, ttybp, code);
     
     call get_temp_segment_ (my_name, ttybp, code);
     if code ^= 0 then do;
	call com_err_ (code, my_name, "Getting temp segment");
	return;
     end;
     
     unspec (cdsa) = ""b;
     cdsa.have_text = "1"b;
     cdsa.p (1) = ttybp;
     cdsa.len (1) = size (tty_buf);
     cdsa.struct_name (1), cdsa.seg_name = my_name;
     cdsa.num_exclude_names = 0;
     cdsa.exclude_array_ptr = null();

     call create_data_segment_ (addr (cdsa), code);
     if code ^= 0 then call com_err_ (code, my_name, "Creating data segment");
     
     call release_temp_segment_ (my_name, ttybp, code);
     
end tty_buf;
