/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */

/* format: off */

/* Constant data used by the Multics abbrevation processor */

/* Created:  March 1983 by G. Palter */

/* HISTORY COMMENTS:
  1) change(86-06-20,Gilcrease), approve(86-06-25,MCR7409),
     audit(86-07-29,GWMay), install(86-08-04,MR12.0-1112):
     For hcom...
     Modified: June 1984 by G. Palter to add version string
  2) change(86-06-20,Gilcrease), approve(86-06-25,MCR7409),
     audit(86-07-29,GWMay), install(86-08-04,MR12.0-1112):
               Install version 3.1a abbrev, change version constant.
  3) change(86-07-24,Gilcrease), approve(86-06-25,MCR7409),
     audit(86-07-29,GWMay), install(86-08-04,MR12.0-1112):
               Update version constnat for find_chars_ rather than tct_,
               by Margolin emergency fix.
  4) change(86-10-10,Gilcrease), approve(87-02-27,MCR7626),
     audit(87-03-09,Parisek), install(87-03-20,MR12.1-1005):
               Add version 2 list requests.
                                                   END HISTORY COMMENTS */


/* format: on,style4,delnl,insnl,ifthenstmt,ifthen */


abbrev_data_:
     procedure () options (variable);


dcl  1 abbrev_data aligned,
       2 version character (32) unaligned,
       2 default_breaks_list aligned like DEFAULT_BREAKS_LIST,
       2 default_breaks_tct_table character (512) unaligned;

dcl  abbrev_data_default_breaks_tct_table_as_binary (0:511) fixed binary (9) unaligned unsigned
	based (addr (abbrev_data.default_breaks_tct_table));

dcl  DEFAULT_BREAKS character (21) static options (constant) initial ("	
 ""$'().:;<>[]`{|}");				/* HT NL VT FF SP QUOTE, etc: must be in collating sequence */

dcl  1 DEFAULT_BREAKS_LIST aligned static options (constant),
       2 n_break_sequences fixed binary initial (21),
       2 break_strings_lth fixed binary initial (22),
       2 break_sequences (21),
         3 start fixed binary initial (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22),
         3 lth fixed binary initial (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1),
       2 break_strings character (22) unaligned initial ("	
 ""$'().::;<>[]`{|}");

dcl  1 cds_data aligned like cds_args;

dcl  code fixed binary (35);
dcl  idx fixed binary;

dcl  ABBREV_DATA_ character (32) static options (constant) initial ("abbrev_data_");

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (pointer, fixed binary (35));

dcl  (addr, currentsize, length, low, null, rank, string, substr) builtin;
%page;
/* Fill in the data structure */

	abbrev_data.version = "3.2";

	abbrev_data.default_breaks_list = DEFAULT_BREAKS_LIST;

	abbrev_data.default_breaks_tct_table = low (length (abbrev_data.default_breaks_tct_table));

	do idx = 1 to length (DEFAULT_BREAKS);		/* exactly one sequence starting with each character */
	     abbrev_data_default_breaks_tct_table_as_binary (rank (substr (DEFAULT_BREAKS, idx, 1))) = idx;
	end;


/* Set up arguments for call to create_data_segment_ */

	cds_data.sections (1).p = addr (abbrev_data);
	cds_data.sections (1).len = currentsize (abbrev_data);
	cds_data.sections (1).struct_name = "abbrev_data";

	cds_data.seg_name = ABBREV_DATA_;

	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null ();

	string (cds_data.switches) = ""b;
	cds_data.switches.have_text = "1"b;		/* only constants */


/* Call create_data_segment_ */

	call create_data_segment_ (addr (cds_data), code);

	if code ^= 0 then call com_err_ (code, ABBREV_DATA_);

	return;
%page;
%include cds_args;

     end abbrev_data_;
