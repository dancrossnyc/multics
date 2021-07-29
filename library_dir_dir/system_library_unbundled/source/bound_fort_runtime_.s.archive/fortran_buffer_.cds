/* ******************************************************
   *                                                    *
   * Copyright, (C) Honeywell Limited, 1983             *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   ****************************************************** */

/* format: style3,^delnl,linecom */
fortran_buffer_:
     proc;


/* This cds program initializes and creates the FORTRAN buffer segment	*/
/* used as the file state blocks for fortran io files. R.Schoeman 11/76	*/
/* Modified:
		6 June 1977, D. Levin for new I/O system.
*/

dcl	sys_info$max_seg_size
			external fixed bin (18);
dcl	1 my_buffer	like fortran_buffer_;

dcl	1 my_cds_args	like cds_args;

dcl	create_data_segment_
			entry (ptr, fixed bin (35));
dcl	com_err_		entry options (variable);

dcl	(addr, binary, length, null, rel, size, unspec)
			builtin;

dcl	p		ptr;
dcl	exclude_array	(1) char (32) init ("**");
dcl	code		fixed bin (35) init (0);

%include fortran_buffer;
%include fortran_io_consts;
%include cds_args;


	unspec (my_cds_args) = "0"b;

	unspec (my_buffer) = "0"b;

	my_buffer.table.switch_p = null;		/* aggregate assignment */

	my_buffer.table (5).default_input = "1"b;
	my_buffer.table (41).default_input = "1"b;

	my_buffer.table (6).default_output = "1"b;
	my_buffer.table (42).default_output = "1"b;

	my_buffer.table (6).printer_file = "1"b;
	my_buffer.table (42).printer_file = "1"b;

	my_buffer.maximum_buffer = (sys_info$max_seg_size - (size (fortran_buffer_) - 1));
						/* The * 4 is because its 4 chars per word */

	my_buffer.all_files_closed = "1"b;

	my_buffer.table (0).connected = "1"b;
	my_buffer.table (0).formatted_records = "1"b;
	my_buffer.table (0).direction.in = "1"b;
	my_buffer.table (0).direction.out = "1"b;
	my_buffer.table (0).allow.seq_access = "1"b;
	my_buffer.table (0).carriage_controllable = "1"b;

	my_buffer.table (0).type_of_io = stream_file;
	my_buffer.table (0).open_code = 3;		/* stream_input_output */
	my_buffer.table (0).previous = open_opr;
	my_buffer.table (0).switch_ready = "1"b;




	my_cds_args.have_text = "1"b;
	my_cds_args.seg_name = "fortran_buffer_";
	my_cds_args.struct_name = "my_buffer";
	my_cds_args.sections (1).len = size (fortran_buffer_) - 1;
						/* the -1 is cause last word doesn't really exist but is a kludge */
	my_cds_args.sections (1).p = addr (my_buffer);

	my_cds_args.num_exclude_names = 1;
	my_cds_args.exclude_array_ptr = addr (exclude_array);

	call create_data_segment_ (addr (my_cds_args), code);

	if code ^= 0
	then call com_err_ ("Call to create_data_segment_ to create fortran_buffer_ failed.");

     end;
