/* ******************************************************
   *                                                    *
   * Copyright, (C) Honeywell Limited, 1983             *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   ****************************************************** */

/* format: style3,^delnl,linecom */
fort_instruction_info_:
     proc;

dcl	(i, j)		fixed bin;
dcl	code		fixed bin (35);

dcl	1 cdsa		aligned auto like cds_args;

dcl	1 fort_instruction_info_
			aligned,
	  2 fort_instruction_info_
			(0:1023) aligned,
	    3 alters	unaligned structure,
	      4 a		bit (1),
	      4 q		bit (1),
	      4 indicators	bit (1),
	      4 bases	(6) bit (1),
	      4 index_regs	(0:7) bit (1),
	      4 pad1	bit (1),
	    3 directable	bit (1) unaligned,
	    3 pad2	bit (17) unaligned;

dcl	1 pl1$instruction_info
			(0:1023) aligned ext static,
	  2 alters	bit (18) unal,
	  2 directable	bit (1) unal,
	  2 pad		bit (8) unal,
	  2 num_words	fixed bin (7) unal,
	  2 double_ins	bit (1) unal;

dcl	me		char (22) int static options (constant) init ("fort_instruction_info_");

dcl	create_data_segment_
			entry (ptr, fixed bin (35));

dcl	(addr, divide, mod, null, size, string)
			builtin;

%include cds_args;

/* initialize the table */

	do i = 0 to 1023;
	     j = divide (i, 2, 17, 0) + 512 * mod (i, 2);

	     string (fort_instruction_info_ (i).alters) = pl1$instruction_info (j).alters;
	     fort_instruction_info_.directable (i) = pl1$instruction_info.directable (j);
	     fort_instruction_info_ (i).pad2 = "0"b;
	end;

/* set up for create_data_segment_ */

	cdsa.sections (1).p = addr (fort_instruction_info_);
	cdsa.sections (1).len = size (fort_instruction_info_);
	cdsa.sections (1).struct_name = me;

	cdsa.seg_name = me;
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null;

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	return;
     end;
