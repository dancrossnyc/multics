/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */

/* format: style2 */
(stringrange, stringsize):
pl1_data:
     proc;

/*
	Converted to create_data_segment_  07/07/76 by Bernard Greenberg
	Modified:	27 Dec 1976 by Richard Barnes for after, before, ltrim, rtrim
	Modified:	30 Dec 1976 by RAB for collate9 and high9
	Modified:	17 Feb 1977 by RAB for -check_ansi
	Modified: April 1977 to add codeptr, currentsize, clock, environmentptr,
				maxlength, stackbaseptr, stackframeptr,
				stacq, vclock by RHS
	Modified:	10 May 1977 by RAB to fix 1573 (sum & prod)
	Modified 770519 by PG to add substraddr
	Modified:	12 July 1977 by RAB to fix maxlength & change stacq definition
	Modified: 29 Mar 1978 by PCK to add codeptr, environmentptr, stackbaseptr, and stackframeptr
	Modified 790202 by PG to add byte and rank
	Modified 830101 by BIM add addcharno addbitno addwordno setcharno
		setbitno setwordno, remove substraddr.
*/

	dcl     create_data_segment_	 ext entry (ptr, fixed bin (35) aligned);
	dcl     com_err_		 entry options (variable);
	dcl     1 cdsa		 like cds_args aligned;
	dcl     (get_temp_segments_, release_temp_segments_)
				 entry (char (*), (*) ptr, fixed bin (35));
	dcl     (cleanup, stringrange, stringsize, error)
				 condition;
	dcl     code		 fixed bin (35);
	dcl     myname		 char (10) init ("pl1_data") static internal options (constant);
	dcl     segptrs		 (1) ptr init (null ());


	dcl     (i, j)		 fixed bin (15);

	dcl     p			 ptr;


	dcl     (addr, collate, divide, substr, unspec, null)
				 builtin;

	dcl     ioa_		 entry options (variable);

	dcl     collating_overlay	 (0:511) bit (9) unal based (addr (p -> long_collating_sequence));

	dcl     1 pl1_data		 based (p),
		2 long_collating_sequence
				 char (512) aligned,
		2 builtin_name,
		  3 number_of_names	 fixed bin (15),
		  3 description	 (127 refer (pl1_data.number_of_names)),
		    4 name	 char (14),
		    4 aggregate_result
				 bit (1),
		    4 nonstandard	 bit (1),
		    4 unused	 bit (7),
		    4 opcode	 bit (9),
		    4 reserve_list_number
				 fixed bin (15),
		    4 jump_index	 fixed bin (15),
		    4 check_indicator
				 fixed bin (15),
		    4 number1	 fixed bin (15),
		    4 number2	 fixed bin (15),
		    4 number_of_descriptions
				 fixed bin (15),
		    4 descriptor	 (4),
		      5 check_code	 fixed bin (15),
		      5 type	 bit (36) aligned;

/*	check_indicator		resulting action:

	1			number1 is required number of arguments
	2			number1 is minimum number of arguments
	3			number1 is minimum number of arguments,
				number2 is maximum number of arguments.

	check_code		resulting action:

	1			argument must be of this type
	2			argument should be converted to this type
	3			argument should be converted to float binary
	4			argument should be converted to arithmetic type
	5			argument should be converted to integer type
	6			argument should be real decimal fixed constant
	7			argument should be converted to string type
	8			argument must either be a bit string or real fixed binary
	9			argument must be variable
	10			argument must be computational
	11			argument must be real and converted to float binary
	12			argument must be entry, label, or format value
						*/


%include cds_args;

%include mask;
%include op_codes;
%include system;

/*      */

	on stringsize
	     begin;
		call com_err_ (0, myname, "Stringsize condition raised.");
		signal error;
	     end;
	on cleanup call release_temp_segments_ (myname, segptrs, (0));
	call get_temp_segments_ (myname, segptrs, code);
	if code ^= 0
	then do;
		call com_err_ (code, myname, "Getting temp segments");
		return;
	     end;

	p = segptrs (1);

/*      */

	i = 1;
	p -> builtin_name.description (i).name = "abs";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = abs_fun;
	p -> builtin_name.description (i).jump_index = 13;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 4;

	i = i + 1;
	p -> builtin_name.description (i).name = "acos";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = acos_fun;
	p -> builtin_name.description (i).reserve_list_number = 85;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "add";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = add;
	p -> builtin_name.description (i).jump_index = 14;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 3;
	p -> builtin_name.description (i).number2 = 4;
	p -> builtin_name.description (i).number_of_descriptions = 4;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;
	p -> builtin_name.description (i).descriptor (4).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "addr";
	p -> builtin_name.description (i).opcode = addr_fun;
	p -> builtin_name.description (i).jump_index = 33;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 9;

	i = i + 1;
	p -> builtin_name.description (i).name = "addrel";/* multics function */
	p -> builtin_name.description (i).opcode = addrel_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 41;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = pointer_type;
	p -> builtin_name.description (i).descriptor (2).check_code = 8;
	i = i + 1;
	p -> builtin_name.description (i).name = "addwordno";/* multics function */
	p -> builtin_name.description (i).opcode = addrel_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 41;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = pointer_type;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "allocation";
	p -> builtin_name.description (i).opcode = allocation_fun;
	p -> builtin_name.description (i).jump_index = 36;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "allocn";
	p -> builtin_name.description (i).opcode = allocation_fun;
	p -> builtin_name.description (i).jump_index = 36;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "after";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = index_after_fun;
	p -> builtin_name.description (i).jump_index = 55;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "asin";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = asin_fun;
	p -> builtin_name.description (i).reserve_list_number = 77;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "atan";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = atan_fun;
	p -> builtin_name.description (i).reserve_list_number = 93;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;
	p -> builtin_name.description (i).descriptor (2).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "atand";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = atand_fun;
	p -> builtin_name.description (i).reserve_list_number = 97;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;
	p -> builtin_name.description (i).descriptor (2).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "atanh";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).reserve_list_number = 45;
	p -> builtin_name.description (i).jump_index = 25;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "baseno";/* multics function */
	p -> builtin_name.description (i).opcode = baseno_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 39;
	p -> builtin_name.description (i).check_indicator = 1;
						/* Exact # of args */
	p -> builtin_name.description (i).number1 = 1;	/* One argument */
	p -> builtin_name.description (i).number_of_descriptions = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "segno"; /* multics function */
	p -> builtin_name.description (i).opcode = segno_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 70;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "baseptr";
						/* multics function */
	p -> builtin_name.description (i).opcode = baseptr_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 41;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 8;

	i = i + 1;
	p -> builtin_name.description (i).name = "before";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = index_before_fun;
	p -> builtin_name.description (i).jump_index = 56;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "bin";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 15;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (1).type = binary_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "binary";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 15;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (1).type = binary_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "bit";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 18;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (1).type = bit_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "bool";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = bool_fun;
	p -> builtin_name.description (i).jump_index = 23;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 2;
	p -> builtin_name.description (i).descriptor (1).type = bit_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 2;
	p -> builtin_name.description (i).descriptor (2).type = bit_mask;
	p -> builtin_name.description (i).descriptor (3).check_code = 2;
	p -> builtin_name.description (i).descriptor (3).type = bit_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "byte";	/* multics function */
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = byte_fun;
	p -> builtin_name.description (i).jump_index = 68;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 2;
	p -> builtin_name.description (i).descriptor (1).type = fixed_binary_real_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "ceil";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = ceil_fun;
	p -> builtin_name.description (i).jump_index = 16;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 4;

	i = i + 1;
	p -> builtin_name.description (i).name = "char";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 18;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (1).type = char_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "character";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 18;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (1).type = char_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "clock"; /* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = clock_fun;
	p -> builtin_name.description (i).jump_index = 62;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "codeptr";
						/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = codeptr_fun;
	p -> builtin_name.description (i).jump_index = 63;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 12;

	i = i + 1;
	p -> builtin_name.description (i).name = "collate";
	p -> builtin_name.description (i).jump_index = 1;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "collate9";
						/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 59;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "complex";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = complex_fun;
	p -> builtin_name.description (i).jump_index = 17;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "conjg";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = conjg_fun;
	p -> builtin_name.description (i).jump_index = 50;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = complex_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "convert";
						/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 46;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "copy";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = repeat_fun;
	p -> builtin_name.description (i).jump_index = 9;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 7;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "cos";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = cos_fun;
	p -> builtin_name.description (i).reserve_list_number = 61;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "cosd";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = cosd_fun;
	p -> builtin_name.description (i).reserve_list_number = 65;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "cosh";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).reserve_list_number = 113;
	p -> builtin_name.description (i).jump_index = 25;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "cplx";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = complex_fun;
	p -> builtin_name.description (i).jump_index = 17;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "currentsize";
						/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 64;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "date";
	p -> builtin_name.description (i).reserve_list_number = 186;
	p -> builtin_name.description (i).jump_index = 31;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "dec";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 15;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (1).type = decimal_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "decat";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).reserve_list_number = 190;
	p -> builtin_name.description (i).jump_index = 24;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "decimal";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 15;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (1).type = decimal_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "dim";
	p -> builtin_name.description (i).opcode = sub;
	p -> builtin_name.description (i).reserve_list_number = 3;
						/* third argument in call to bounds_ */
	p -> builtin_name.description (i).jump_index = 26;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "dimension";
	p -> builtin_name.description (i).opcode = sub;
	p -> builtin_name.description (i).reserve_list_number = 3;
						/* third argument in call to bounds_ */
	p -> builtin_name.description (i).jump_index = 26;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "divide";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = div;
	p -> builtin_name.description (i).jump_index = 14;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 3;
	p -> builtin_name.description (i).number2 = 4;
	p -> builtin_name.description (i).number_of_descriptions = 4;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;
	p -> builtin_name.description (i).descriptor (4).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "dot";
	p -> builtin_name.description (i).opcode = add;
	p -> builtin_name.description (i).jump_index = 45;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 3;
	p -> builtin_name.description (i).number2 = 4;
	p -> builtin_name.description (i).number_of_descriptions = 4;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;
	p -> builtin_name.description (i).descriptor (4).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "empty";
	p -> builtin_name.description (i).jump_index = 28;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "environmentptr";
						/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = environmentptr_fun;
	p -> builtin_name.description (i).jump_index = 63;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 12;

	i = i + 1;
	p -> builtin_name.description (i).name = "erf";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).reserve_list_number = 129;
	p -> builtin_name.description (i).jump_index = 25;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "erfc";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).reserve_list_number = 133;
	p -> builtin_name.description (i).jump_index = 25;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "exp";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = exp_fun;
	p -> builtin_name.description (i).reserve_list_number = 29;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "fixed";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 3;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "float";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 2;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "floor";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = floor_fun;
	p -> builtin_name.description (i).jump_index = 16;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 4;

	i = i + 1;
	p -> builtin_name.description (i).name = "hbound";
	p -> builtin_name.description (i).reserve_list_number = 2;
						/* third argument in call to bounds_ */
	p -> builtin_name.description (i).jump_index = 26;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "high";
	p -> builtin_name.description (i).opcode = repeat_fun;
	p -> builtin_name.description (i).jump_index = 5;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "high9"; /* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = repeat_fun;
	p -> builtin_name.description (i).jump_index = 60;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "imag";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = imag_fun;
	p -> builtin_name.description (i).jump_index = 19;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = complex_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "index";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = index_fun;
	p -> builtin_name.description (i).jump_index = 6;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "lbound";
	p -> builtin_name.description (i).reserve_list_number = 1;
						/* third argument in call to bounds_ */
	p -> builtin_name.description (i).jump_index = 26;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "length";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = length_fun;
	p -> builtin_name.description (i).jump_index = 7;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 7;

	i = i + 1;
	p -> builtin_name.description (i).name = "lineno";
	p -> builtin_name.description (i).reserve_list_number = 183;
	p -> builtin_name.description (i).jump_index = 30;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = file_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "log";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = log_fun;
	p -> builtin_name.description (i).reserve_list_number = 33;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "log10";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = log10_fun;
	p -> builtin_name.description (i).reserve_list_number = 41;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "log2";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = log2_fun;
	p -> builtin_name.description (i).reserve_list_number = 37;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "low";
	p -> builtin_name.description (i).opcode = repeat_fun;
	p -> builtin_name.description (i).jump_index = 8;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "ltrim"; /* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = verify_ltrim_fun;
	p -> builtin_name.description (i).jump_index = 57;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 2;
	p -> builtin_name.description (i).descriptor (1).type = char_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 2;
	p -> builtin_name.description (i).descriptor (2).type = char_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "max";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = max_fun;
	p -> builtin_name.description (i).jump_index = 20;
	p -> builtin_name.description (i).check_indicator = 2;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "maxlength";
						/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 65;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 7;

	i = i + 1;
	p -> builtin_name.description (i).name = "min";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = min_fun;
	p -> builtin_name.description (i).jump_index = 20;
	p -> builtin_name.description (i).check_indicator = 2;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "mod";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = pl1_mod_fun;
	p -> builtin_name.description (i).jump_index = 21;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "multiply";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = mult;
	p -> builtin_name.description (i).jump_index = 14;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 3;
	p -> builtin_name.description (i).number2 = 4;
	p -> builtin_name.description (i).number_of_descriptions = 4;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;
	p -> builtin_name.description (i).descriptor (4).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "null";
	p -> builtin_name.description (i).jump_index = 29;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "nullo"; /* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 44;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "offset";
	p -> builtin_name.description (i).opcode = off_fun;
	p -> builtin_name.description (i).jump_index = 35;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = pointer_type;
	p -> builtin_name.description (i).descriptor (2).check_code = 1;
	p -> builtin_name.description (i).descriptor (2).type = area_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "onchar";
	p -> builtin_name.description (i).reserve_list_number = 9;
	p -> builtin_name.description (i).jump_index = 51;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "oncode";
	p -> builtin_name.description (i).reserve_list_number = 10;
	p -> builtin_name.description (i).jump_index = 53;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "onfield";
	p -> builtin_name.description (i).reserve_list_number = 8;
	p -> builtin_name.description (i).jump_index = 42;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "onfile";
	p -> builtin_name.description (i).reserve_list_number = 12;
	p -> builtin_name.description (i).jump_index = 42;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "onkey";
	p -> builtin_name.description (i).reserve_list_number = 13;
	p -> builtin_name.description (i).jump_index = 42;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "onloc";
	p -> builtin_name.description (i).reserve_list_number = 7;
	p -> builtin_name.description (i).jump_index = 42;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "onsource";
	p -> builtin_name.description (i).reserve_list_number = 14;
	p -> builtin_name.description (i).jump_index = 52;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "pageno";
	p -> builtin_name.description (i).reserve_list_number = 184;
	p -> builtin_name.description (i).jump_index = 30;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = file_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "pointer";
	p -> builtin_name.description (i).opcode = ptr_fun;
	p -> builtin_name.description (i).jump_index = 34;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "setwordno"; /* multics function */
	p -> builtin_name.description (i).opcode = ptr_fun;
	p -> builtin_name.description (i).jump_index = 34;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = pointer_type;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "prec";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 4;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "precision";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 4;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "prod";
	p -> builtin_name.description (i).opcode = mult;
	p -> builtin_name.description (i).jump_index = 43;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;

	i = i + 1;
	p -> builtin_name.description (i).name = "ptr";
	p -> builtin_name.description (i).opcode = ptr_fun;
	p -> builtin_name.description (i).jump_index = 34;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;

	i = i + 1;
	p -> builtin_name.description (i).name = "rank";	/* multics function */
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = rank_fun;
	p -> builtin_name.description (i).jump_index = 69;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = char_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "real";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = real_fun;
	p -> builtin_name.description (i).jump_index = 19;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = complex_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "rel";	/* multics function */
	p -> builtin_name.description (i).opcode = rel_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 39;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "wordno";/* multics function */
	p -> builtin_name.description (i).opcode = wordno_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 71;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "charno";
	p -> builtin_name.description (i).opcode = charno_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 71;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "bitno";
	p -> builtin_name.description (i).opcode = bitno_fun;
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 71;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;

	begin;
	     declare NAMES		      (4) char (12) init ("setcharno", "addcharno", "setbitno", "addbitno")
				      int static options (constant);
	     declare OP		      (4) bit (9)
				      init (setcharno_fun, addcharno_fun, setbitno_fun, addbitno_fun);

	     declare nx		      fixed bin;

	     do nx = 1 to 4;
		i = i + 1;
		p -> builtin_name.description (i).name = NAMES (nx);
		p -> builtin_name.description (i).opcode = OP (nx);
		p -> builtin_name.description (i).jump_index = 74;
		p -> builtin_name.description (i).check_indicator = 1;
						/* fixed argument count */
		p -> builtin_name.description (i).number1 = 2;
		p -> builtin_name.description (i).number_of_descriptions = 2;
		p -> builtin_name.description (i).descriptor (1).check_code = 1;
		p -> builtin_name.description (i).descriptor (1).type = pointer_type;
		p -> builtin_name.description (i).descriptor (2).check_code = 5;
	     end;
	end;
	i = i + 1;
	p -> builtin_name.description (i).name = "reverse";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = reverse_fun;
	p -> builtin_name.description (i).jump_index = 27;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 7;

	i = i + 1;
	p -> builtin_name.description (i).name = "round";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = round_fun;
	p -> builtin_name.description (i).jump_index = 22;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 4;
	p -> builtin_name.description (i).descriptor (1).check_code = 4;
	p -> builtin_name.description (i).descriptor (2).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "rtrim"; /* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = verify_rtrim_fun;
	p -> builtin_name.description (i).jump_index = 58;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number2 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 2;
	p -> builtin_name.description (i).descriptor (1).type = char_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 2;
	p -> builtin_name.description (i).descriptor (2).type = char_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "search";/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = search_fun;
	p -> builtin_name.description (i).jump_index = 37;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 2;
	p -> builtin_name.description (i).descriptor (1).type = char_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 2;
	p -> builtin_name.description (i).descriptor (2).type = char_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "sign";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = sign_fun;
	p -> builtin_name.description (i).jump_index = 38;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 4;

	i = i + 1;
	p -> builtin_name.description (i).name = "sin";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = sin_fun;
	p -> builtin_name.description (i).reserve_list_number = 53;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "sind";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = sind_fun;
	p -> builtin_name.description (i).reserve_list_number = 57;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "sinh";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).reserve_list_number = 109;
	p -> builtin_name.description (i).jump_index = 25;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "size";	/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).jump_index = 47;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "sqrt";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = sqrt_fun;
	p -> builtin_name.description (i).reserve_list_number = 25;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "stac";	/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = lock_fun;
	p -> builtin_name.description (i).jump_index = 40;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = pointer_type;
	p -> builtin_name.description (i).descriptor (2).check_code = 1;
	p -> builtin_name.description (i).descriptor (2).type = bit_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "stackbaseptr";
						/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = stackbaseptr_fun;
	p -> builtin_name.description (i).jump_index = 61;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "stackframeptr";
						/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = stackframeptr_fun;
	p -> builtin_name.description (i).jump_index = 61;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "stacq"; /* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = stacq_fun;
	p -> builtin_name.description (i).jump_index = 66;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 1;
	p -> builtin_name.description (i).descriptor (1).type = bit_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 1;
	p -> builtin_name.description (i).descriptor (2).type = bit_mask;
	p -> builtin_name.description (i).descriptor (3).check_code = 1;
	p -> builtin_name.description (i).descriptor (3).type = bit_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "string";
	p -> builtin_name.description (i).jump_index = 10;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "substr";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).jump_index = 11;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 7;
	p -> builtin_name.description (i).descriptor (2).check_code = 5;
	p -> builtin_name.description (i).descriptor (3).check_code = 5;

	i = i + 1;
	p -> builtin_name.description (i).name = "subtract";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = sub;
	p -> builtin_name.description (i).jump_index = 14;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 3;
	p -> builtin_name.description (i).number2 = 4;
	p -> builtin_name.description (i).number_of_descriptions = 4;
	p -> builtin_name.description (i).descriptor (3).check_code = 6;
	p -> builtin_name.description (i).descriptor (4).check_code = 6;

	i = i + 1;
	p -> builtin_name.description (i).name = "sum";
	p -> builtin_name.description (i).opcode = add;
	p -> builtin_name.description (i).jump_index = 43;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 10;

	i = i + 1;
	p -> builtin_name.description (i).name = "tan";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = tan_fun;
	p -> builtin_name.description (i).reserve_list_number = 69;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "tand";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = tand_fun;
	p -> builtin_name.description (i).reserve_list_number = 73;
	p -> builtin_name.description (i).jump_index = 54;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 11;

	i = i + 1;
	p -> builtin_name.description (i).name = "tanh";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).reserve_list_number = 117;
	p -> builtin_name.description (i).jump_index = 25;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 3;

	i = i + 1;
	p -> builtin_name.description (i).name = "time";
	p -> builtin_name.description (i).reserve_list_number = 187;
	p -> builtin_name.description (i).jump_index = 32;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "translate";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = translate_fun;
	p -> builtin_name.description (i).jump_index = 49;
	p -> builtin_name.description (i).check_indicator = 3;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number2 = 3;
	p -> builtin_name.description (i).number_of_descriptions = 3;
	p -> builtin_name.description (i).descriptor (1).check_code = 2;
	p -> builtin_name.description (i).descriptor (1).type = char_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 2;
	p -> builtin_name.description (i).descriptor (2).type = char_mask;
	p -> builtin_name.description (i).descriptor (3).check_code = 2;
	p -> builtin_name.description (i).descriptor (3).type = char_mask;

	i = i + 1;
	p -> builtin_name.description (i).name = "trunc";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = trunc_fun;
	p -> builtin_name.description (i).jump_index = 16;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;
	p -> builtin_name.description (i).number_of_descriptions = 1;
	p -> builtin_name.description (i).descriptor (1).check_code = 4;

	i = i + 1;
	p -> builtin_name.description (i).name = "unspec";
	p -> builtin_name.description (i).jump_index = 12;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "valid";
	p -> builtin_name.description (i).reserve_list_number = 185;
	p -> builtin_name.description (i).jump_index = 48;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 1;

	i = i + 1;
	p -> builtin_name.description (i).name = "vclock";/* multics function */
	p -> builtin_name.description (i).nonstandard = "1"b;
	p -> builtin_name.description (i).opcode = vclock_fun;
	p -> builtin_name.description (i).jump_index = 62;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 0;

	i = i + 1;
	p -> builtin_name.description (i).name = "verify";
	p -> builtin_name.description (i).aggregate_result = "1"b;
	p -> builtin_name.description (i).opcode = verify_fun;
	p -> builtin_name.description (i).jump_index = 37;
	p -> builtin_name.description (i).check_indicator = 1;
	p -> builtin_name.description (i).number1 = 2;
	p -> builtin_name.description (i).number_of_descriptions = 2;
	p -> builtin_name.description (i).descriptor (1).check_code = 2;
	p -> builtin_name.description (i).descriptor (1).type = char_mask;
	p -> builtin_name.description (i).descriptor (2).check_code = 2;
	p -> builtin_name.description (i).descriptor (2).type = char_mask;

	p -> builtin_name.number_of_names = i;

	call ioa_ ("^a: ^d names processed.", myname, i);

/* prepare long_collating_sequence */

	substr (p -> long_collating_sequence, 1, 128) = collate ();
	do j = 128 to 511;
	     collating_overlay (j) = bit (fixed (j, 9), 9);
	end;

/* now call create_data_segment_ */

	unspec (cdsa) = "0"b;
	cdsa.have_text = "1"b;
	cdsa.sections (1).p = p;
	cdsa.sections (1).len = divide (length (unspec (pl1_data)), 36, 17, 0);
	cdsa.sections (1).struct_name = "pl1_data";
	cdsa.seg_name = myname;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then call com_err_ (code, myname, "Creating ^a data segment", myname);

	call release_temp_segments_ (myname, segptrs, (0));
     end pl1_data;
