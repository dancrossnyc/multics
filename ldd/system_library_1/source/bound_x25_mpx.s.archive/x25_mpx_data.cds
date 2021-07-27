/* ******************************************************
   *                                                    *
   * Copyright, (C) Honeywell Bull Inc., 1987           *
   *                                                    *
   * Copyright (c) 1987 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   ****************************************************** */

/* format: style4,delnl,insnl,ifthenstmt,indnoniterend */
x25_mpx_data:
     procedure;

/* translation tables for X.25 multiplexer */
/* Written by C. Hornig, March 1981 */

dcl  mvt_$make_translation_table entry (char (*), char (*), char (512) aligned);

dcl  code fixed bin (35);
dcl  tt char (512) aligned;
dcl  1 cdsa aligned like cds_args;

dcl  com_err_ entry options (variable);
dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl  1 x25_mpx_data aligned,
       2 eight_bit char (512),
       2 trans_no_parity char (256),
       2 trans_no_parity_lfecho char (256),
       2 trans_parity_lfecho char (256);

/* * * * * * * * * * * * * * * * * * * */

	call mvt_$make_translation_table (substr (collate9 (), 1, 256), substr (collate9 (), 257, 256),
	     x25_mpx_data.eight_bit);

	call mvt_$make_translation_table (substr (collate9 (), 1, 128), substr (collate9 (), 129, 128), tt);
	x25_mpx_data.trans_no_parity = substr (tt, 1, 256);

	call mvt_$make_translation_table (byte (10) || substr (collate9 (), 1, 13) || byte (10)
	     || substr (collate9 (), 15, 114), byte (13) || substr (collate9 (), 129, 128), tt);
	x25_mpx_data.trans_no_parity_lfecho = substr (tt, 1, 256);

	call mvt_$make_translation_table (byte (10), byte (13), tt);
	x25_mpx_data.trans_parity_lfecho = substr (tt, 1, 256);

	unspec (cdsa) = ""b;
	cdsa.sections (1).p = addr (x25_mpx_data);
	cdsa.sections (1).len = size (x25_mpx_data);
	cdsa.sections (1).struct_name = "x25_mpx_data";

	cdsa.seg_name = "x25_mpx_data";
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0 then call com_err_ (code, "x25_mpx_data");
	return;
%page;
%include cds_args;

     end x25_mpx_data;
