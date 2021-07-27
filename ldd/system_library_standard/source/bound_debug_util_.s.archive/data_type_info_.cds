/* ******************************************************
   *                                                    *
   * Copyright (c) 1986 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   ****************************************************** */



/* HISTORY COMMENTS:
  1) change(86-09-05,JMAthane), approve(86-09-05,MCR7525),
     audit(86-09-11,Martinson), install(86-11-12,MR12.0-1212):
     Added information concerning new pascal_string_type_dtype.
                                                   END HISTORY COMMENTS */


data_type_info_: proc;

/* Calls create_data_segment_ to create the data segment data_type_info_
   which gives attributes of all data types used in Multics.

   James R. Davis 6 Apr 79
   JRD 16 Oct 79 for new COBOL type 40
   JRD 10 Nov 80 MCR 4503 (I forgot to zero the pad fields)
   MBW 31 July 1981 to add algol68 data types
   JMAthane June 83 to add new PASCAL types and "type" bit in info.
   S. Herbst 01/23/84 Added types 47-50, 81-86, "hex" and "generic" bits.
   JMAthane June 85. Added type 87 (pascal string type dtype)
*/

	dcl     create_data_segment_	 entry (ptr, fixed bin (35));

	dcl     1 dti_struc		 aligned,
		2 version_number	 fixed bin,
		2 info,				/* extra level because of cds restriction! */
		  3 real_info	 (87) like data_type_info_$info,
		2 ninebit_sign_chars char (2),
		2 ninebit_digit_chars char (10),
		2 ninebit_overpunched_sign_chars char (22),
		2 max_decimal_precision fixed bin,
		2 max_float_binary_precision fixed bin,
		2 max_fixed_binary_precision fixed bin;

	dcl     1 cdsa		 aligned automatic internal like cds_args;

	dcl     (addr, hbound, null, size, string, unspec) builtin;
	dcl     code		 fixed bin (35);
	dcl     com_err_		 entry options (variable);
	dcl     exclude		 (1) char (32) static internal options (constant) init ("highest");


	call fillin;
	call init_cds_struc;
	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0 then call com_err_ (code, "make_dti");
	return;

fillin: proc;
	dcl     (Y		 init ("1"b),
	        N			 init ("0"b)) bit (1) aligned internal static options (constant);
	dti_struc.version_number = 1;
	dti_struc.ninebit_sign_chars = "+-";
	dti_struc.ninebit_digit_chars = "0123456789";
	dti_struc.ninebit_overpunched_sign_chars = "{}ABCDEFGHIJKLMNOPQR";

	dti_struc.max_decimal_precision = 59;
	dti_struc.max_float_binary_precision = 63;
	dti_struc.max_fixed_binary_precision = 71;

	dti_struc.info.real_info (*) = "0"b;

	info (1).computational = Y;
	info (1).arithmetic = Y;
	info (1).fixed = Y;
	info (1).complex = N;
	info (1).decimal = N;
	info (1).signed = Y;
	info (1).trailing_sign = N;
	info (1).packed_dec = N;
	info (1).digit_aligned = N;
	info (1).overpunched = N;
	info (1).char_string = N;
	info (1).bit_string = N;
	info (1).varying = N;
	info (1).type = N;
	info (1).hex = N;
	info (1).generic = N;

	info (2).computational = Y;
	info (2).arithmetic = Y;
	info (2).fixed = Y;
	info (2).complex = N;
	info (2).decimal = N;
	info (2).signed = Y;
	info (2).trailing_sign = N;
	info (2).packed_dec = N;
	info (2).digit_aligned = N;
	info (2).overpunched = N;
	info (2).char_string = N;
	info (2).bit_string = N;
	info (2).varying = N;
	info (2).type = N;
	info (2).hex = N;
	info (2).generic = N;

	info (3).computational = Y;
	info (3).arithmetic = Y;
	info (3).fixed = N;
	info (3).complex = N;
	info (3).decimal = N;
	info (3).signed = Y;
	info (3).trailing_sign = N;
	info (3).packed_dec = N;
	info (3).digit_aligned = N;
	info (3).overpunched = N;
	info (3).char_string = N;
	info (3).bit_string = N;
	info (3).varying = N;
	info (3).type = N;
	info (3).hex = N;
	info (3).generic = N;

	info (4).computational = Y;
	info (4).arithmetic = Y;
	info (4).fixed = N;
	info (4).complex = N;
	info (4).decimal = N;
	info (4).signed = Y;
	info (4).trailing_sign = N;
	info (4).packed_dec = N;
	info (4).digit_aligned = N;
	info (4).overpunched = N;
	info (4).char_string = N;
	info (4).bit_string = N;
	info (4).varying = N;
	info (4).type = N;
	info (4).hex = N;
	info (4).generic = N;

	info (5).computational = Y;
	info (5).arithmetic = Y;
	info (5).fixed = Y;
	info (5).complex = Y;
	info (5).decimal = N;
	info (5).signed = Y;
	info (5).trailing_sign = N;
	info (5).packed_dec = N;
	info (5).digit_aligned = N;
	info (5).overpunched = N;
	info (5).char_string = N;
	info (5).bit_string = N;
	info (5).varying = N;
	info (5).type = N;
	info (5).hex = N;
	info (5).generic = N;

	info (6).computational = Y;
	info (6).arithmetic = Y;
	info (6).fixed = Y;
	info (6).complex = Y;
	info (6).decimal = N;
	info (6).signed = Y;
	info (6).trailing_sign = N;
	info (6).packed_dec = N;
	info (6).digit_aligned = N;
	info (6).overpunched = N;
	info (6).char_string = N;
	info (6).bit_string = N;
	info (6).varying = N;
	info (6).type = N;
	info (6).hex = N;
	info (6).generic = N;

	info (7).computational = Y;
	info (7).arithmetic = Y;
	info (7).fixed = N;
	info (7).complex = Y;
	info (7).decimal = N;
	info (7).signed = Y;
	info (7).trailing_sign = N;
	info (7).packed_dec = N;
	info (7).digit_aligned = N;
	info (7).overpunched = N;
	info (7).char_string = N;
	info (7).bit_string = N;
	info (7).varying = N;
	info (7).type = N;
	info (7).hex = N;
	info (7).generic = N;

	info (8).computational = Y;
	info (8).arithmetic = Y;
	info (8).fixed = N;
	info (8).complex = Y;
	info (8).decimal = N;
	info (8).signed = Y;
	info (8).trailing_sign = N;
	info (8).packed_dec = N;
	info (8).digit_aligned = N;
	info (8).overpunched = N;
	info (8).char_string = N;
	info (8).bit_string = N;
	info (8).varying = N;
	info (8).type = N;
	info (8).hex = N;
	info (8).generic = N;

	info (9).computational = Y;
	info (9).arithmetic = Y;
	info (9).fixed = Y;
	info (9).complex = N;
	info (9).decimal = Y;
	info (9).signed = Y;
	info (9).trailing_sign = N;
	info (9).packed_dec = N;
	info (9).digit_aligned = N;
	info (9).overpunched = N;
	info (9).char_string = N;
	info (9).bit_string = N;
	info (9).varying = N;
	info (9).type = N;
	info (9).hex = N;
	info (9).generic = N;

	info (10).computational = Y;
	info (10).arithmetic = Y;
	info (10).fixed = N;
	info (10).complex = N;
	info (10).decimal = Y;
	info (10).signed = Y;
	info (10).trailing_sign = N;
	info (10).packed_dec = N;
	info (10).digit_aligned = N;
	info (10).overpunched = N;
	info (10).char_string = N;
	info (10).bit_string = N;
	info (10).varying = N;
	info (10).type = N;
	info (10).hex = N;
	info (10).generic = N;

	info (11).computational = Y;
	info (11).arithmetic = Y;
	info (11).fixed = Y;
	info (11).complex = Y;
	info (11).decimal = Y;
	info (11).signed = Y;
	info (11).trailing_sign = N;
	info (11).packed_dec = N;
	info (11).digit_aligned = N;
	info (11).overpunched = N;
	info (11).char_string = N;
	info (11).bit_string = N;
	info (11).varying = N;
	info (11).type = N;
	info (11).hex = N;
	info (11).generic = N;

	info (12).computational = Y;
	info (12).arithmetic = Y;
	info (12).fixed = N;
	info (12).complex = Y;
	info (12).decimal = Y;
	info (12).signed = Y;
	info (12).trailing_sign = N;
	info (12).packed_dec = N;
	info (12).digit_aligned = N;
	info (12).overpunched = N;
	info (12).char_string = N;
	info (12).bit_string = N;
	info (12).varying = N;
	info (12).type = N;
	info (12).hex = N;
	info (12).generic = N;

	info (13).computational = N;
	info (13).arithmetic = N;
	info (13).fixed = N;
	info (13).complex = N;
	info (13).decimal = N;
	info (13).signed = N;
	info (13).trailing_sign = N;
	info (13).packed_dec = N;
	info (13).digit_aligned = N;
	info (13).overpunched = N;
	info (13).char_string = N;
	info (13).bit_string = N;
	info (13).varying = N;
	info (13).type = N;
	info (13).hex = N;
	info (13).generic = N;

	info (14).computational = N;
	info (14).arithmetic = N;
	info (14).fixed = N;
	info (14).complex = N;
	info (14).decimal = N;
	info (14).signed = N;
	info (14).trailing_sign = N;
	info (14).packed_dec = N;
	info (14).digit_aligned = N;
	info (14).overpunched = N;
	info (14).char_string = N;
	info (14).bit_string = N;
	info (14).varying = N;
	info (14).type = N;
	info (14).hex = N;
	info (14).generic = N;

	info (15).computational = N;
	info (15).arithmetic = N;
	info (15).fixed = N;
	info (15).complex = N;
	info (15).decimal = N;
	info (15).signed = N;
	info (15).trailing_sign = N;
	info (15).packed_dec = N;
	info (15).digit_aligned = N;
	info (15).overpunched = N;
	info (15).char_string = N;
	info (15).bit_string = N;
	info (15).varying = N;
	info (15).type = N;
	info (15).hex = N;
	info (15).generic = N;

	info (16).computational = N;
	info (16).arithmetic = N;
	info (16).fixed = N;
	info (16).complex = N;
	info (16).decimal = N;
	info (16).signed = N;
	info (16).trailing_sign = N;
	info (16).packed_dec = N;
	info (16).digit_aligned = N;
	info (16).overpunched = N;
	info (16).char_string = N;
	info (16).bit_string = N;
	info (16).varying = N;
	info (16).type = N;
	info (16).hex = N;
	info (16).generic = N;

	info (17).computational = N;
	info (17).arithmetic = N;
	info (17).fixed = N;
	info (17).complex = N;
	info (17).decimal = N;
	info (17).signed = N;
	info (17).trailing_sign = N;
	info (17).packed_dec = N;
	info (17).digit_aligned = N;
	info (17).overpunched = N;
	info (17).char_string = N;
	info (17).bit_string = N;
	info (17).varying = N;
	info (17).type = N;
	info (17).hex = N;
	info (17).generic = N;

	info (18).computational = N;
	info (18).arithmetic = N;
	info (18).fixed = N;
	info (18).complex = N;
	info (18).decimal = N;
	info (18).signed = N;
	info (18).trailing_sign = N;
	info (18).packed_dec = N;
	info (18).digit_aligned = N;
	info (18).overpunched = N;
	info (18).char_string = N;
	info (18).bit_string = N;
	info (18).varying = N;
	info (18).type = N;
	info (18).hex = N;
	info (18).generic = N;

	info (19).computational = Y;
	info (19).arithmetic = N;
	info (19).fixed = N;
	info (19).complex = N;
	info (19).decimal = N;
	info (19).signed = N;
	info (19).trailing_sign = N;
	info (19).packed_dec = N;
	info (19).digit_aligned = N;
	info (19).overpunched = N;
	info (19).char_string = N;
	info (19).bit_string = Y;
	info (19).varying = N;
	info (19).type = N;
	info (19).hex = N;
	info (19).generic = N;

	info (20).computational = Y;
	info (20).arithmetic = N;
	info (20).fixed = N;
	info (20).complex = N;
	info (20).decimal = N;
	info (20).signed = N;
	info (20).trailing_sign = N;
	info (20).packed_dec = N;
	info (20).digit_aligned = N;
	info (20).overpunched = N;
	info (20).char_string = N;
	info (20).bit_string = Y;
	info (20).varying = Y;
	info (20).type = N;
	info (20).hex = N;
	info (20).generic = N;

	info (21).computational = Y;
	info (21).arithmetic = N;
	info (21).fixed = N;
	info (21).complex = N;
	info (21).decimal = N;
	info (21).signed = N;
	info (21).trailing_sign = N;
	info (21).packed_dec = N;
	info (21).digit_aligned = N;
	info (21).overpunched = N;
	info (21).char_string = Y;
	info (21).bit_string = N;
	info (21).varying = N;
	info (21).type = N;
	info (21).hex = N;
	info (21).generic = N;

	info (22).computational = Y;
	info (22).arithmetic = N;
	info (22).fixed = N;
	info (22).complex = N;
	info (22).decimal = N;
	info (22).signed = N;
	info (22).trailing_sign = N;
	info (22).packed_dec = N;
	info (22).digit_aligned = N;
	info (22).overpunched = N;
	info (22).char_string = Y;
	info (22).bit_string = N;
	info (22).varying = Y;
	info (22).type = N;
	info (22).hex = N;
	info (22).generic = N;

	info (23).computational = N;
	info (23).arithmetic = N;
	info (23).fixed = N;
	info (23).complex = N;
	info (23).decimal = N;
	info (23).signed = N;
	info (23).trailing_sign = N;
	info (23).packed_dec = N;
	info (23).digit_aligned = N;
	info (23).overpunched = N;
	info (23).char_string = N;
	info (23).bit_string = N;
	info (23).varying = N;
	info (23).type = N;
	info (23).hex = N;
	info (23).generic = N;

	info (24).computational = N;			/* not used */
	info (24).arithmetic = N;
	info (24).fixed = N;
	info (24).complex = N;
	info (24).decimal = N;
	info (24).signed = N;
	info (24).trailing_sign = N;
	info (24).packed_dec = N;
	info (24).digit_aligned = N;
	info (24).overpunched = N;
	info (24).char_string = N;
	info (24).bit_string = N;
	info (24).varying = N;
	info (24).type = N;
	info (24).hex = N;
	info (24).generic = N;

	info (25).computational = N;			/* not used */
	info (25).arithmetic = N;
	info (25).fixed = N;
	info (25).complex = N;
	info (25).decimal = N;
	info (25).signed = N;
	info (25).trailing_sign = N;
	info (25).packed_dec = N;
	info (25).digit_aligned = N;
	info (25).overpunched = N;
	info (25).char_string = N;
	info (25).bit_string = N;
	info (25).varying = N;
	info (25).type = N;
	info (25).hex = N;
	info (25).generic = N;

	info (26).computational = N;			/* not used */
	info (26).arithmetic = N;
	info (26).fixed = N;
	info (26).complex = N;
	info (26).decimal = N;
	info (26).signed = N;
	info (26).trailing_sign = N;
	info (26).packed_dec = N;
	info (26).digit_aligned = N;
	info (26).overpunched = N;
	info (26).char_string = N;
	info (26).bit_string = N;
	info (26).varying = N;
	info (26).type = N;
	info (26).hex = N;
	info (26).generic = N;

	info (27).computational = N;			/* not used */
	info (27).arithmetic = N;
	info (27).fixed = N;
	info (27).complex = N;
	info (27).decimal = N;
	info (27).signed = N;
	info (27).trailing_sign = N;
	info (27).packed_dec = N;
	info (27).digit_aligned = N;
	info (27).overpunched = N;
	info (27).char_string = N;
	info (27).bit_string = N;
	info (27).varying = N;
	info (27).type = N;
	info (27).hex = N;
	info (27).generic = N;

	info (28).computational = N;			/* not used */
	info (28).arithmetic = N;
	info (28).fixed = N;
	info (28).complex = N;
	info (28).decimal = N;
	info (28).signed = N;
	info (28).trailing_sign = N;
	info (28).packed_dec = N;
	info (28).digit_aligned = N;
	info (28).overpunched = N;
	info (28).char_string = N;
	info (28).bit_string = N;
	info (28).varying = N;
	info (28).type = N;
	info (28).hex = N;
	info (28).generic = N;

	info (29).computational = Y;
	info (29).arithmetic = Y;
	info (29).fixed = Y;
	info (29).complex = N;
	info (29).decimal = Y;
	info (29).signed = Y;
	info (29).trailing_sign = N;
	info (29).packed_dec = N;
	info (29).digit_aligned = N;
	info (29).overpunched = Y;
	info (29).char_string = N;
	info (29).bit_string = N;
	info (29).varying = N;
	info (29).type = N;
	info (29).hex = N;
	info (29).generic = N;

	info (30).computational = Y;
	info (30).arithmetic = Y;
	info (30).fixed = Y;
	info (30).complex = N;
	info (30).decimal = Y;
	info (30).signed = Y;
	info (30).trailing_sign = Y;
	info (30).packed_dec = N;
	info (30).digit_aligned = N;
	info (30).overpunched = Y;
	info (30).char_string = N;
	info (30).bit_string = N;
	info (30).varying = N;
	info (30).type = N;
	info (30).hex = N;
	info (30).generic = N;

	info (31).computational = N;			/* not used */
	info (31).arithmetic = N;
	info (31).fixed = N;
	info (31).complex = N;
	info (31).decimal = N;
	info (31).signed = N;
	info (31).trailing_sign = N;
	info (31).packed_dec = N;
	info (31).digit_aligned = N;
	info (31).overpunched = N;
	info (31).char_string = N;
	info (31).bit_string = N;
	info (31).varying = N;
	info (31).type = N;
	info (31).hex = N;	
	info (31).generic = N;

	info (32).computational = N;			/* not used */
	info (32).arithmetic = N;
	info (32).fixed = N;
	info (32).complex = N;
	info (32).decimal = N;
	info (32).signed = N;
	info (32).trailing_sign = N;
	info (32).packed_dec = N;
	info (32).digit_aligned = N;
	info (32).overpunched = N;
	info (32).char_string = N;
	info (32).bit_string = N;
	info (32).varying = N;
	info (32).type = N;
	info (32).hex = N;
	info (32).generic = N;

	info (33).computational = Y;
	info (33).arithmetic = Y;
	info (33).fixed = Y;
	info (33).complex = N;
	info (33).decimal = N;
	info (33).signed = N;
	info (33).trailing_sign = N;
	info (33).packed_dec = N;
	info (33).digit_aligned = N;
	info (33).overpunched = N;
	info (33).char_string = N;
	info (33).bit_string = N;
	info (33).varying = N;
	info (33).type = N;
	info (33).hex = N;
	info (33).generic = N;

	info (34).computational = Y;
	info (34).arithmetic = Y;
	info (34).fixed = Y;
	info (34).complex = N;
	info (34).decimal = N;
	info (34).signed = N;
	info (34).trailing_sign = N;
	info (34).packed_dec = N;
	info (34).digit_aligned = N;
	info (34).overpunched = N;
	info (34).char_string = N;
	info (34).bit_string = N;
	info (34).varying = N;
	info (34).type = N;
	info (34).hex = N;
	info (34).generic = N;

	info (35).computational = Y;
	info (35).arithmetic = Y;
	info (35).fixed = Y;
	info (35).complex = N;
	info (35).decimal = Y;
	info (35).signed = N;
	info (35).trailing_sign = N;
	info (35).packed_dec = N;
	info (35).digit_aligned = N;
	info (35).overpunched = N;
	info (35).char_string = N;
	info (35).bit_string = N;
	info (35).varying = N;
	info (35).type = N;
	info (35).hex = N;
	info (35).generic = N;

	info (36).computational = Y;
	info (36).arithmetic = Y;
	info (36).fixed = Y;
	info (36).complex = N;
	info (36).decimal = Y;
	info (36).signed = Y;
	info (36).trailing_sign = Y;
	info (36).packed_dec = N;
	info (36).digit_aligned = N;
	info (36).overpunched = N;
	info (36).char_string = N;
	info (36).bit_string = N;
	info (36).varying = N;
	info (36).type = N;
	info (36).hex = N;
	info (36).generic = N;

	info (37).computational = N;			/* not used */
	info (37).arithmetic = N;
	info (37).fixed = N;
	info (37).complex = N;
	info (37).decimal = N;
	info (37).signed = N;
	info (37).trailing_sign = N;
	info (37).packed_dec = N;
	info (37).digit_aligned = N;
	info (37).overpunched = N;
	info (37).char_string = N;
	info (37).bit_string = N;
	info (37).varying = N;
	info (37).type = N;
	info (37).hex = N;
	info (37).generic = N;

	info (38).computational = Y;
	info (38).arithmetic = Y;
	info (38).fixed = Y;
	info (38).complex = N;
	info (38).decimal = Y;
	info (38).signed = N;
	info (38).trailing_sign = N;
	info (38).packed_dec = Y;
	info (38).digit_aligned = Y;
	info (38).overpunched = N;
	info (38).char_string = N;
	info (38).bit_string = N;
	info (38).varying = N;
	info (38).type = N;
	info (38).hex = N;
	info (38).generic = N;

	info (39).computational = Y;
	info (39).arithmetic = Y;
	info (39).fixed = Y;
	info (39).complex = N;
	info (39).decimal = Y;
	info (39).signed = Y;
	info (39).trailing_sign = Y;
	info (39).packed_dec = Y;
	info (39).digit_aligned = N;
	info (39).overpunched = N;
	info (39).char_string = N;
	info (39).bit_string = N;
	info (39).varying = N;
	info (39).type = N;
	info (39).hex = N;
	info (39).generic = N;

	info (40).computational = Y;			/* comp-5 unsigned byte aligned */
	info (40).arithmetic = Y;
	info (40).fixed = Y;
	info (40).complex = N;
	info (40).decimal = Y;
	info (40).signed = N;
	info (40).trailing_sign = N;
	info (40).packed_dec = Y;
	info (40).digit_aligned = N;
	info (40).overpunched = N;
	info (40).char_string = N;
	info (40).bit_string = N;
	info (40).varying = N;
	info (40).type = N;
	info (40).hex = N;
	info (40).generic = N;

	info (41).computational = Y;
	info (41).arithmetic = Y;
	info (41).fixed = Y;
	info (41).complex = N;
	info (41).decimal = Y;
	info (41).signed = Y;
	info (41).trailing_sign = N;
	info (41).packed_dec = Y;
	info (41).digit_aligned = Y;
	info (41).overpunched = N;
	info (41).char_string = N;
	info (41).bit_string = N;
	info (41).varying = N;
	info (41).type = N;
	info (41).hex = N;
	info (41).generic = N;

	info (42).computational = Y;
	info (42).arithmetic = Y;
	info (42).fixed = N;
	info (42).complex = N;
	info (42).decimal = Y;
	info (42).signed = Y;
	info (42).trailing_sign = N;
	info (42).packed_dec = Y;
	info (42).digit_aligned = Y;
	info (42).overpunched = N;
	info (42).char_string = N;
	info (42).bit_string = N;
	info (42).varying = N;
	info (42).type = N;
	info (42).hex = N;
	info (42).generic = N;

	info (43).computational = Y;
	info (43).arithmetic = Y;
	info (43).fixed = Y;
	info (43).complex = N;
	info (43).decimal = Y;
	info (43).signed = Y;
	info (43).trailing_sign = N;
	info (43).packed_dec = Y;
	info (43).digit_aligned = N;
	info (43).overpunched = N;
	info (43).char_string = N;
	info (43).bit_string = N;
	info (43).varying = N;
	info (43).type = N;
	info (43).hex = N;
	info (43).generic = N;

	info (44).computational = Y;
	info (44).arithmetic = Y;
	info (44).fixed = N;
	info (44).complex = N;
	info (44).decimal = Y;
	info (44).signed = Y;
	info (44).trailing_sign = N;
	info (44).packed_dec = Y;
	info (44).digit_aligned = N;
	info (44).overpunched = N;
	info (44).char_string = N;
	info (44).bit_string = N;
	info (44).varying = N;
	info (44).type = N;
	info (44).hex = N;
	info (44).generic = N;

	info (45).computational = Y;
	info (45).arithmetic = Y;
	info (45).fixed = Y;
	info (45).complex = Y;
	info (45).decimal = Y;
	info (45).signed = Y;
	info (45).trailing_sign = N;
	info (45).packed_dec = Y;
	info (45).digit_aligned = N;
	info (45).overpunched = N;
	info (45).char_string = N;
	info (45).bit_string = N;
	info (45).varying = N;
	info (45).type = N;
	info (45).hex = N;
	info (45).generic = N;

	info (46).computational = Y;
	info (46).arithmetic = Y;
	info (46).fixed = N;
	info (46).complex = Y;
	info (46).decimal = Y;
	info (46).signed = Y;
	info (46).trailing_sign = N;
	info (46).packed_dec = Y;
	info (46).digit_aligned = N;
	info (46).overpunched = N;
	info (46).char_string = N;
	info (46).bit_string = N;
	info (46).varying = N;
	info (46).type = N;
	info (46).hex = N;
	info (46).generic = N;

	info (47).computational = Y;			/* real_flt_hex_1_dtype */
	info (47).arithmetic = Y;
	info (47).fixed = N;
	info (47).complex = N;
	info (47).decimal = N;
	info (47).signed = Y;
	info (47).trailing_sign = N;
	info (47).packed_dec = N;
	info (47).digit_aligned = N;
	info (47).overpunched = N;
	info (47).char_string = N;
	info (47).bit_string = N;
	info (47).varying = N;
	info (47).type = N;
	info (47).hex = Y;
	info (47).generic = N;

	info (48).computational = Y;			/* real_flt_hex_2_dtype */
	info (48).arithmetic = Y;
	info (48).fixed = N;
	info (48).complex = N;
	info (48).decimal = N;
	info (48).signed = Y;
	info (48).trailing_sign = N;
	info (48).packed_dec = N;
	info (48).digit_aligned = N;
	info (48).overpunched = N;
	info (48).char_string = N;
	info (48).bit_string = N;
	info (48).varying = N;
	info (48).type = N;
	info (48).hex = Y;
	info (48).generic = N;

	info (49).computational = Y;			/* cplx_flt_hex_1_dtype */
	info (49).arithmetic = Y;
	info (49).fixed = N;
	info (49).complex = Y;
	info (49).decimal = N;
	info (49).signed = Y;
	info (49).trailing_sign = N;
	info (49).packed_dec = N;
	info (49).digit_aligned = N;
	info (49).overpunched = N;
	info (49).char_string = N;
	info (49).bit_string = N;
	info (49).varying = N;
	info (49).type = N;
	info (49).hex = Y;
	info (49).generic = N;

	info (50).computational = Y;			/* cplx_flt_hex_2_dtype */
	info (50).arithmetic = Y;
	info (50).fixed = N;
	info (50).complex = Y;
	info (50).decimal = N;
	info (50).signed = Y;
	info (50).trailing_sign = N;
	info (50).packed_dec = N;
	info (50).digit_aligned = N;
	info (50).overpunched = N;
	info (50).char_string = N;
	info (50).bit_string = N;
	info (50).varying = N;
	info (50).type = N;
	info (50).hex = Y;
	info (50).generic = N;

	info (51).computational = N;			/* not used */
	info (51).arithmetic = N;
	info (51).fixed = N;
	info (51).complex = N;
	info (51).decimal = N;
	info (51).signed = N;
	info (51).trailing_sign = N;
	info (51).packed_dec = N;
	info (51).digit_aligned = N;
	info (51).overpunched = N;
	info (51).char_string = N;
	info (51).bit_string = N;
	info (51).varying = N;
	info (51).type = N;
	info (51).hex = N;
	info (51).generic = N;

	info (52).computational = N;			/* not used */
	info (52).arithmetic = N;
	info (52).fixed = N;
	info (52).complex = N;
	info (52).decimal = N;
	info (52).signed = N;
	info (52).trailing_sign = N;
	info (52).packed_dec = N;
	info (52).digit_aligned = N;
	info (52).overpunched = N;
	info (52).char_string = N;
	info (52).bit_string = N;
	info (52).varying = N;
	info (52).type = N;
	info (52).hex = N;
	info (52).generic = N;

	info (53).computational = N;			/* not used */
	info (53).arithmetic = N;
	info (53).fixed = N;
	info (53).complex = N;
	info (53).decimal = N;
	info (53).signed = N;
	info (53).trailing_sign = N;
	info (53).packed_dec = N;
	info (53).digit_aligned = N;
	info (53).overpunched = N;
	info (53).char_string = N;
	info (53).bit_string = N;
	info (53).varying = N;
	info (53).type = N;
	info (53).hex = N;
	info (53).generic = N;

	info (54).computational = N;			/* not used */
	info (54).arithmetic = N;
	info (54).fixed = N;
	info (54).complex = N;
	info (54).decimal = N;
	info (54).signed = N;
	info (54).trailing_sign = N;
	info (54).packed_dec = N;
	info (54).digit_aligned = N;
	info (54).overpunched = N;
	info (54).char_string = N;
	info (54).bit_string = N;
	info (54).varying = N;
	info (54).type = N;
	info (54).hex = N;
	info (54).generic = N;

	info (55).computational = N;			/* not used */
	info (55).arithmetic = N;
	info (55).fixed = N;
	info (55).complex = N;
	info (55).decimal = N;
	info (55).signed = N;
	info (55).trailing_sign = N;
	info (55).packed_dec = N;
	info (55).digit_aligned = N;
	info (55).overpunched = N;
	info (55).char_string = N;
	info (55).bit_string = N;
	info (55).varying = N;
	info (55).type = N;
	info (55).hex = N;
	info (55).generic = N;

	info (56).computational = N;			/* not used */
	info (56).arithmetic = N;
	info (56).fixed = N;
	info (56).complex = N;
	info (56).decimal = N;
	info (56).signed = N;
	info (56).trailing_sign = N;
	info (56).packed_dec = N;
	info (56).digit_aligned = N;
	info (56).overpunched = N;
	info (56).char_string = N;
	info (56).bit_string = N;
	info (56).varying = N;
	info (56).type = N;
	info (56).hex = N;
	info (56).generic = N;

	info (57).computational = N;			/* not used */
	info (57).arithmetic = N;
	info (57).fixed = N;
	info (57).complex = N;
	info (57).decimal = N;
	info (57).signed = N;
	info (57).trailing_sign = N;
	info (57).packed_dec = N;
	info (57).digit_aligned = N;
	info (57).overpunched = N;
	info (57).char_string = N;
	info (57).bit_string = N;
	info (57).varying = N;
	info (57).type = N;
	info (57).hex = N;
	info (57).generic = N;

	info (58).computational = N;			/* not used */
	info (58).arithmetic = N;
	info (58).fixed = N;
	info (58).complex = N;
	info (58).decimal = N;
	info (58).signed = N;
	info (58).trailing_sign = N;
	info (58).packed_dec = N;
	info (58).digit_aligned = N;
	info (58).overpunched = N;
	info (58).char_string = N;
	info (58).bit_string = N;
	info (58).varying = N;
	info (58).type = N;
	info (58).hex = N;
	info (58).generic = N;

	info (59).computational = N;			/* algol68 straight */
	info (59).arithmetic = N;
	info (59).fixed = N;
	info (59).complex = N;
	info (59).decimal = N;
	info (59).signed = N;
	info (59).trailing_sign = N;
	info (59).packed_dec = N;
	info (59).digit_aligned = N;
	info (59).overpunched = N;
	info (59).char_string = N;
	info (59).bit_string = N;
	info (59).varying = N;
	info (59).type = N;
	info (59).hex = N;
	info (59).generic = N;

	info (60).computational = N;			/* algol68 format */
	info (60).arithmetic = N;
	info (60).fixed = N;
	info (60).complex = N;
	info (60).decimal = N;
	info (60).signed = N;
	info (60).trailing_sign = N;
	info (60).packed_dec = N;
	info (60).digit_aligned = N;
	info (60).overpunched = N;
	info (60).char_string = N;
	info (60).bit_string = N;
	info (60).varying = N;
	info (60).type = N;
	info (60).hex = N;
	info (60).generic = N;

	info (61).computational = N;			/* algol68 array descriptor */
	info (61).arithmetic = N;
	info (61).fixed = N;
	info (61).complex = N;
	info (61).decimal = N;
	info (61).signed = N;
	info (61).trailing_sign = N;
	info (61).packed_dec = N;
	info (61).digit_aligned = N;
	info (61).overpunched = N;
	info (61).char_string = N;
	info (61).bit_string = N;
	info (61).varying = N;
	info (61).type = N;
	info (61).hex = N;
	info (61).generic = N;

	info (62).computational = N;			/* algol68 union */
	info (62).arithmetic = N;
	info (62).fixed = N;
	info (62).complex = N;
	info (62).decimal = N;
	info (62).signed = N;
	info (62).trailing_sign = N;
	info (62).packed_dec = N;
	info (62).digit_aligned = N;
	info (62).overpunched = N;
	info (62).char_string = N;
	info (62).bit_string = N;
	info (62).varying = N;
	info (62).type = N;
	info (62).hex = N;
	info (62).generic = N;

	info (63).computational = Y;			/* picture */
	info (63).arithmetic = N;
	info (63).fixed = N;
	info (63).complex = N;
	info (63).decimal = N;
	info (63).signed = N;
	info (63).trailing_sign = N;
	info (63).packed_dec = N;
	info (63).digit_aligned = N;
	info (63).overpunched = N;
	info (63).char_string = N;
	info (63).bit_string = N;
	info (63).varying = N;
	info (63).type = N;
	info (63).hex = N;
	info (63).generic = N;

	info (64).computational = N;			/* pascal_typed_pointer_type_dtype */
	info (64).arithmetic = N;
	info (64).fixed = N;
	info (64).complex = N;
	info (64).decimal = N;
	info (64).signed = N;
	info (64).trailing_sign = N;
	info (64).packed_dec = N;
	info (64).digit_aligned = N;
	info (64).overpunched = N;
	info (64).char_string = N;
	info (64).bit_string = N;
	info (64).varying = N;
	info (64).type = Y;
	info (64).hex = N;
	info (64).generic = N;

	info (65).computational = N;			/* pascal_char_dtype */
	info (65).arithmetic = N;
	info (65).fixed = N;
	info (65).complex = N;
	info (65).decimal = N;
	info (65).signed = N;
	info (65).trailing_sign = N;
	info (65).packed_dec = N;
	info (65).digit_aligned = N;
	info (65).overpunched = N;
	info (65).char_string = N;
	info (65).bit_string = N;
	info (65).varying = N;
	info (65).type = N;
	info (65).hex = N;
	info (65).generic = N;

	info (66).computational = N;			/*pascal_boolean_dtype*/
	info (66).arithmetic = N;
	info (66).fixed = N;
	info (66).complex = N;
	info (66).decimal = N;
	info (66).signed = N;
	info (66).trailing_sign = N;
	info (66).packed_dec = N;
	info (66).digit_aligned = N;
	info (66).overpunched = N;
	info (66).char_string = N;
	info (66).bit_string = N;
	info (66).varying = N;
	info (66).type = N;
	info (66).hex = N;
	info (66).generic = N;

	info (67).computational = N;			/* pascal_record_file_type_dtype*/
	info (67).arithmetic = N;
	info (67).fixed = N;
	info (67).complex = N;
	info (67).decimal = N;
	info (67).signed = N;
	info (67).trailing_sign = N;
	info (67).packed_dec = N;
	info (67).digit_aligned = N;
	info (67).overpunched = N;
	info (67).char_string = N;
	info (67).bit_string = N;
	info (67).varying = N;
	info (67).type = Y;
	info (67).hex = N;
	info (67).generic = N;

	info (68).computational = N;			/*pascal_record_type_dtype*/
	info (68).arithmetic = N;
	info (68).fixed = N;
	info (68).complex = N;
	info (68).decimal = N;
	info (68).signed = N;
	info (68).trailing_sign = N;
	info (68).packed_dec = N;
	info (68).digit_aligned = N;
	info (68).overpunched = N;
	info (68).char_string = N;
	info (68).bit_string = N;
	info (68).varying = N;
	info (68).type = Y;
	info (68).hex = N;
	info (68).generic = N;

	info (69).computational = N;			/*pascal_set_type_dtype*/
	info (69).arithmetic = N;
	info (69).fixed = N;
	info (69).complex = N;
	info (69).decimal = N;
	info (69).signed = N;
	info (69).trailing_sign = N;
	info (69).packed_dec = N;
	info (69).digit_aligned = N;
	info (69).overpunched = N;
	info (69).char_string = N;
	info (69).bit_string = N;
	info (69).varying = N;
	info (69).type = Y;
	info (69).hex = N;
	info (69).generic = N;

	info (70).computational = N;			/*pascal_enumerated_type_dtype*/
	info (70).arithmetic = N;
	info (70).fixed = N;
	info (70).complex = N;
	info (70).decimal = N;
	info (70).signed = N;
	info (70).trailing_sign = N;
	info (70).packed_dec = N;
	info (70).digit_aligned = N;
	info (70).overpunched = N;
	info (70).char_string = N;
	info (70).bit_string = N;
	info (70).varying = N;
	info (70).type = Y;
	info (70).hex = N;
	info (70).generic = N;

	info (71).computational = N;			/*pascal_enumerated_type_element_dtype*/
	info (71).arithmetic = N;
	info (71).fixed = N;
	info (71).complex = N;
	info (71).decimal = N;
	info (71).signed = N;
	info (71).trailing_sign = N;
	info (71).packed_dec = N;
	info (71).digit_aligned = N;
	info (71).overpunched = N;
	info (71).char_string = N;
	info (71).bit_string = N;
	info (71).varying = N;
	info (71).type = N;
	info (71).hex = N;
	info (71).generic = N;

	info (72).computational = N;			/*pascal_enumerated_type_instance_dtype*/
	info (72).arithmetic = N;
	info (72).fixed = N;
	info (72).complex = N;
	info (72).decimal = N;
	info (72).signed = N;
	info (72).trailing_sign = N;
	info (72).packed_dec = N;
	info (72).digit_aligned = N;
	info (72).overpunched = N;
	info (72).char_string = N;
	info (72).bit_string = N;
	info (72).varying = N;
	info (72).type = N;
	info (72).hex = N;
	info (72).generic = N;

	info (73).computational = N;			/*pascal_user_defined_type_dtype */
	info (73).arithmetic = N;
	info (73).fixed = N;
	info (73).complex = N;
	info (73).decimal = N;
	info (73).signed = N;
	info (73).trailing_sign = N;
	info (73).packed_dec = N;
	info (73).digit_aligned = N;
	info (73).overpunched = N;
	info (73).char_string = N;
	info (73).bit_string = N;
	info (73).varying = N;
	info (73).type = Y;
	info (73).hex = N;
	info (73).generic = N;

	info (74).computational = N;			/* pascal_user_defined_type_instance_dtype */
	info (74).arithmetic = N;
	info (74).fixed = N;
	info (74).complex = N;
	info (74).decimal = N;
	info (74).signed = N;
	info (74).trailing_sign = N;
	info (74).packed_dec = N;
	info (74).digit_aligned = N;
	info (74).overpunched = N;
	info (74).char_string = N;
	info (74).bit_string = N;
	info (74).varying = N;
	info (74).type = N;
	info (74).hex = N;
	info (74).generic = N;

	info (75).computational = N;			/* pascal_text_file_dtype*/
	info (75).arithmetic = N;
	info (75).fixed = N;
	info (75).complex = N;
	info (75).decimal = N;
	info (75).signed = N;
	info (75).trailing_sign = N;
	info (75).packed_dec = N;
	info (75).digit_aligned = N;
	info (75).overpunched = N;
	info (75).char_string = N;
	info (75).bit_string = N;
	info (75).varying = N;
	info (75).type = N;
	info (75).hex = N;
	info (75).generic = N;

	info (76).computational = N;			/* pascal_procedure_type_dtype */
	info (76).arithmetic = N;
	info (76).fixed = N;
	info (76).complex = N;
	info (76).decimal = N;
	info (76).signed = N;
	info (76).trailing_sign = N;
	info (76).packed_dec = N;
	info (76).digit_aligned = N;
	info (76).overpunched = N;
	info (76).char_string = N;
	info (76).bit_string = N;
	info (76).varying = N;
	info (76).type = Y;
	info (76).hex = N;
	info (76).generic = N;

	info (77).computational = N;			/* pascal_var_formal_parm_dtype*/
	info (77).arithmetic = N;
	info (77).fixed = N;
	info (77).complex = N;
	info (77).decimal = N;
	info (77).signed = N;
	info (77).trailing_sign = N;
	info (77).packed_dec = N;
	info (77).digit_aligned = N;
	info (77).overpunched = N;
	info (77).char_string = N;
	info (77).bit_string = N;
	info (77).varying = N;
	info (77).type = N;
	info (77).hex = N;
	info (77).generic  = N;

	info (78).computational = N;			/* pascal_value_formal_parm_dtype*/
	info (78).arithmetic = N;
	info (78).fixed = N;
	info (78).complex = N;
	info (78).decimal = N;
	info (78).signed = N;
	info (78).trailing_sign = N;
	info (78).packed_dec = N;
	info (78).digit_aligned = N;
	info (78).overpunched = N;
	info (78).char_string = N;
	info (78).bit_string = N;
	info (78).varying = N;
	info (78).type = N;
	info (78).hex = N;
	info (78).generic = N;

	info (79).computational = N;			/* pascal_parameter_procedure_dtype*/
	info (79).arithmetic = N;
	info (79).fixed = N;
	info (79).complex = N;
	info (79).decimal = N;
	info (79).signed = N;
	info (79).trailing_sign = N;
	info (79).packed_dec = N;
	info (79).digit_aligned = N;
	info (79).overpunched = N;
	info (79).char_string = N;
	info (79).bit_string = N;
	info (79).varying = N;
	info (79).type = N;
	info (79).hex = N;
	info (79).generic = N;

	info (80).computational = N;			/* pascal_entry_formal_parm_dtype*/
	info (80).arithmetic = N;
	info (80).fixed = N;
	info (80).complex = N;
	info (80).decimal = N;
	info (80).signed = N;
	info (80).trailing_sign = N;
	info (80).packed_dec = N;
	info (80).digit_aligned = N;
	info (80).overpunched = N;
	info (80).char_string = N;
	info (80).bit_string = N;
	info (80).varying = N;
	info (80).type = N;
	info (80).hex = N;
	info (80).generic = N;

	info (81).computational = Y;			/* real_flt_dec_extended_dtype */
	info (81).arithmetic = Y;
	info (81).fixed = N;
	info (81).complex = N;
	info (81).decimal = Y;
	info (81).signed = Y;
	info (81).trailing_sign = N;
	info (81).packed_dec = N;
	info (81).digit_aligned = N;
	info (81).overpunched = N;
	info (81).char_string = N;
	info (81).bit_string = N;
	info (81).varying = N;
	info (81).type = N;
	info (81).hex = N;
	info (81).generic = N;

	info (82).computational = Y;			/* cplx_flt_dec_extended_dtype */
	info (82).arithmetic = Y;
	info (82).fixed = N;
	info (82).complex = Y;
	info (82).decimal = Y;
	info (82).signed = Y;
	info (82).trailing_sign = N;
	info (82).packed_dec = N;
	info (82).digit_aligned = N;
	info (82).overpunched = N;
	info (82).char_string = N;
	info (82).bit_string = N;
	info (82).varying = N;
	info (82).type = N;
	info (82).hex = N;
	info (82).generic = N;

	info (83).computational = Y;			/* real_flt_dec_generic_dtype */
	info (83).arithmetic = Y;
	info (83).fixed = N;
	info (83).complex = N;
	info (83).decimal = Y;
	info (83).signed = Y;
	info (83).trailing_sign = N;
	info (83).packed_dec = N;
	info (83).digit_aligned = N;
	info (83).overpunched = N;
	info (83).char_string = N;
	info (83).bit_string = N;
	info (83).varying = N;
	info (83).type = N;
	info (83).hex = N;
	info (83).generic = Y;

	info (84).computational = Y;			/* cplx_flt_dec_generic_dtype */
	info (84).arithmetic = Y;
	info (84).fixed = N;
	info (84).complex = Y;
	info (84).decimal = Y;
	info (84).signed = N;
	info (84).trailing_sign = N;
	info (84).packed_dec = N;
	info (84).digit_aligned = N;
	info (84).overpunched = N;
	info (84).char_string = N;
	info (84).bit_string = N;
	info (84).varying = N;
	info (84).type = N;
	info (84).hex = N;
	info (84).generic = Y;

	info (85).computational = Y;			/* real_flt_bin_generic_dtype */
	info (85).arithmetic = Y;
	info (85).fixed = N;
	info (85).complex = N;
	info (85).decimal = N;
	info (85).signed = Y;
	info (85).trailing_sign = N;
	info (85).packed_dec = N;
	info (85).digit_aligned = N;
	info (85).overpunched = N;
	info (85).char_string = N;
	info (85).bit_string = N;
	info (85).varying = N;
	info (85).type = N;
	info (85).hex = N;
	info (85).generic = Y;

	info (86).computational = Y;			/* cplx_flt_bin_generic_dtype */
	info (86).arithmetic = Y;
	info (86).fixed = N;
	info (86).complex = Y;
	info (86).decimal = N;
	info (86).signed = Y;
	info (86).trailing_sign = N;
	info (86).packed_dec = N;
	info (86).digit_aligned = N;
	info (86).overpunched = N;
	info (86).char_string = N;
	info (86).bit_string = N;
	info (86).varying = N;
	info (86).type = N;
	info (86).hex = N;
	info (86).generic = Y;

	info (87).computational = N;			/* pascal_string_type_dtype */
	info (87).arithmetic = N;
	info (87).fixed = N;
	info (87).complex = N;
	info (87).decimal = N;
	info (87).signed = N;
	info (87).trailing_sign = N;
	info (87).packed_dec = N;
	info (87).digit_aligned = N;
	info (87).overpunched = N;
	info (87).char_string = N;
	info (87).bit_string = N;
	info (87).varying = N;
	info (87).type = Y;
	info (87).hex = N;
	info (87).generic = N;

     end fillin;


init_cds_struc: proc;


	unspec (cdsa) = "0"b;
	cdsa.seg_name = "data_type_info_";
	cdsa.sections (1).p = addr (dti_struc);
	cdsa.sections (1).len = size (dti_struc);
	cdsa.sections (1).struct_name = "dti_struc";

	cdsa.sections (2).p = null;
	cdsa.sections (2).len = 0;
	cdsa.sections (2).struct_name = "";

	cdsa.num_exclude_names = hbound (exclude, 1);
	cdsa.exclude_array_ptr = addr (exclude);

	string (cdsa.switches) = "0"b;
	cdsa.switches.defs_in_link = "0"b;
	cdsa.switches.separate_static = "0"b;
	cdsa.switches.have_text = "1"b;
	cdsa.switches.have_static = "0"b;

     end;

%include cds_args;
%include data_type_info_;
     end;						/* program */
