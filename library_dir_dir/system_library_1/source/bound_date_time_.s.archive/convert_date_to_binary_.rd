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

/*++
BEGIN
main    	/ <no-token>		/						/ RETURN	\
	\" Begin with parsing of long date
	/ <month_name> <day>	/ SL(1) Amonth_and_LEX Aday_and_LEX SLe			/ year 	\
	/ <day> <month_name>	/ SL(2) Aday_and_LEX Amonth_and_LEX SLe			/ year_	\

	\" Short date formats
	/ <month> /_ <day> /_ <year>
			      	/ SL(3) Amonth_and_LEX LEX Aday_and_LEX LEX Ayear_and_LEX SLe/ main	\
	/ <month> /_ <day>
			       	/ SL(4) Amonth_and_LEX LEX Aday_and_LEX Ayear_default SLe	/ main	\
	/ <year> - <month> - <day>	/ SL(5) Ayear_and_LEX LEX Amonth_and_LEX LEX Aday_and_LEX SLe/ main	\
	/ <fweek> <n>		/ SL(6) LEX Afw LEX SLe				/ main	\

	\" One of the Numeric-Offset family
	/ <N> <andfraction> <offset>	/ SL(7) Afraction_offset LEX(3) SLe			/ main	\

	\" request_id format
	/ <n12> <andfraction6>	/ SL(8) Arequest_id LEX(2) SLe			/ main	\

	\" Time formats
	/ <n> <andfraction>		/ SL(9) Atime LEX Aminute_fraction LEX SLe		/ zone_dif\
	\" <n> . is handled because <andfraction> may be just "."
	/ <hour> : <minute>		/ SL(10) Ahour_and_LEX LEX Aminute_and_LEX SLe		/ second	\
	/ <hour> <meridian>		/ SL(11) Ahour_and_LEX Ameridian LEX
				  Aminute_zero Asecond_zero SLe			/ zone_dif\
	/ <twelve> <half_day>	/ SL(12) LEX Ahalf_day LEX SLe			/ zone_dif\
	/ <half_day>		/ SL(13) Ahalf_day LEX SLe				/ zone_dif\

	\" Day of week.  It is offset if no other date given, or validates a given date.
	/ <day_name> ,		/ SL(14) Uday_of_week LEX(2) SLe			/ main	\
	/ <day_name> <before_on_after>/ SL(15) Uday_of_week LEX Ab_o_a LEX SLe		/ adv_day	\
	/ <day_name>		/ SL(16) Uday_of_week LEX SLe				/ main	\

	\" Numeric offsets
num_off	/ <sign> <N> <offset>	/ SL(17) apply_sign_and_offset LEX(3) SLe		/ main	\
	/ <N> <offset>		/ SL(18) apply_offset LEX(2) SLe			/ main	\
	/ <sign> <N> <andfraction> <offset>
				/ SL(19) Afraction_sign_and_offset LEX(4) SLe		/ main	\
	/ <fraction> <offset>	/ SL(20) Afraction_offset LEX(2) SLe			/ main	\
	/ <sign> <fraction> <offset>
				/ SL(21) Afraction_sign_and_offset LEX(3) SLe		/ main	\


	\" Time zone which qualifies the time given.
	/ <zone>			/ SL(22) Azone LEX SLe				/ main	\
	/ <yesterday>		/ SL(23) Ayesterday LEX SLe				/ main	\
	/ <today>			/ SL(23) Atoday LEX SLe				/ main	\
	/ <tomorrow>		/ SL(23) Atomorrow LEX SLe				/ main	\
	/ <now>			/ SL(23) Anow LEX SLe				/ main	\
	/			/ [last_adverb = VWon]				/	\
	/ <before_after> <or>	/ LEX [err_pt=token.Pvalue;
				  details="""or"" can only be used with <day-name>.";
				  lcode=error_table_$dt_time_conversion_error]		/ RETURN	\
	/ <before_after>		/ SL(24) Aadverb LEX SLe				/ main	\

	\" Error diagnostics and return value.
	/ <on>			/  [err_pt=token.Pvalue;details = """on"" can only be used with <day-name>.";
				    lcode=error_table_$dt_time_conversion_error]		/ RETURN	\
	/ <any-token>		/  [err_pt=token.Pvalue;
				    lcode=error_table_$dt_time_conversion_error]		/ RETURN	\
	/ <no-token>		/						/ RETURN	\

adv_day	/ <or> <before_after>	/ SL(25) LEX Ab_a LEX SLe				/	\
	/			/ [rtime_first = "1"b]				/ main	\

	\" Optional seconds and meridian in HH:MM:SS <MERIDIAN><zone-differential>
second	/ : <second> <andfraction>	/ SL(26) LEX Asecond_and_LEX Asecond_fraction LEX SLe	/ meridian\
	/ : <second>		/ SL(27) LEX Asecond_and_LEX SLe			/ meridian\
	/ <andfraction>		/ SL(28) Aminute_fraction LEX SLe			/ meridian\
	/			/ Asecond_zero					/	\
meridian	/ <meridian>		/ SL(29) Ameridian LEX SLe				/	\

zone_dif	/ <sign> <n4> <offset>	/						/ num_off	\
	/ <sign> <n4> <andfraction>	/						/ num_off	\
	/ <sign> <n4>		/ SL(30) [SIGN = token_value] LEX Azone_dif LEX SLe	/ main	\
	/			/						/ main	\

	\" Look for optional year in long date formats.
year	/ , <year>		/ SL(31) LEX Ayear_and_LEX SLe			/ main	\
year_	/ <n> <andfraction>		/ Ayear_default					/ main	\
	/ <n> :			/ Ayear_default					/ main	\
	/ <n> <meridian>		/ Ayear_default					/ main	\
	/ <twelve> <half_day>	/ Ayear_default					/ main	\
	/ <n> <offset>		/ Ayear_default					/ main	\
	/ <day> <month_name>	/ [err_pt=token.Pvalue;
				   details="<day> <month_name> already given.";
				   lcode=error_table_$dt_time_conversion_error]  		/ RETURN	\
	/ <year>			/ SL(32) Ayear_and_LEX SLe				/ main	\
	/			/ Ayear_default					/ main	\
											++*/

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **/
	/* 								         */
	/* Name: convert_date_to_binary_					         */
	/* 								         */
	/*      This subroutine parses a time string, converting it to a standard Multics        */
	/* clock value.  Refer to the MPM Subroutines for a description of a time string.        */
	/* 								         */
	/* Status								         */
	/* 								         */
	/* 0) Created  March 11, 1971      by  Dan Bricklin			         */
	/* 1) Modified July 30, 1971       by  Dan Bricklin - add offset feature	         */
	/* 2) Modified September 10, 1979  by  Gary Dixon - complete rewrite into reduction      */
	/*    language.							         */
	/* 3) Modified June 1, 1983	     by  J Falksen - add fractional offsets, convert	*/
	/*				to new date/time software.			*/
	/* 4) Modified Jan 20, 1984	     by  J Falksen - added optional "," after day_name,	*/
	/*				zone differential form			*/
	/* 5) Modified Aug 1984	     by jaf - added now/today/tomorrow/yesterday		*/
	/*				this_XXX, and adverbial offsets		*/
	/*								         */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **/

/* HISTORY COMMENTS:
  1) change(86-07-18,GDixon), approve(86-07-25,MCR7495),
     audit(86-07-25,Martinson), install(86-09-16,MR12.0-1120):
     Prior to this change, a time string containing ? caused debugging messages
     to be printed by convert_date_to_binary_.  This change triggers debugging
     from the setting of a new time_defaults_$debug switch, and removes support
     for ? in time strings.
  2) change(86-08-14,GDixon), approve(86-09-04,MCR7532),
     audit(86-09-05,Martinson), install(86-09-16,MR12.0-1159):
     Correct null_pointer fault which occurred when processing the time string
     "this". (phx19450)
  3) change(86-08-14,GDixon), approve(86-09-04,MCR7532),
     audit(86-09-05,Martinson), install(86-09-16,MR12.0-1159):
     Make "<day_name> <on> <date>" and "<date> <day_name>" forms of time
     strings properly verify that the given date falls on the given day_name.
     (phx20492)
  4) change(88-05-26,GDixon), approve(88-05-26,MCR7900),
     audit(88-07-28,Lippard), install(88-08-02,MR12.2-1075):
     Added the $analyze entrypoint, an internal entry used primarily by
     memo_repeat_.pl1 to determine if a memo repeat interval given by the user
     consists only of fixed-length offsets (eg, 1 day 5 hours).  All offsets
     greater than weeks are variable-length.  Even 5 years is variable because
     of leap year calculations.  memo calls the new $analyze entrypoint to
     apply the offset once to the current memo time.  If that produces a time
     which is still in the past and the offsets are all fixed length, memo
     simple adds the fixed-length number of microseconds the offset represents
     iteratively until a time in the future is obtained. If the offsets are
     variable length, memo must call $relative repeated until a time in the
     future is obtained. (phx21094)
  5) change(88-05-29,GDixon), approve(88-06-15,MCR7918),
     audit(88-07-28,Lippard), install(88-08-02,MR12.2-1075):
      A) Correct error preventing an absolute day-of-week specification from
         being enforced.  Fix allows "date 6/1/88 thu" to be reported as an
         error, since it is really a Wednesday. (date_time 32)
  6) change(99-06-23,Haggett):
     Y2K
                                                   END HISTORY COMMENTS */


/* Name: convert_date_to_binary_                                             */
/*                                                                           */
/* ENTRY:  convert_date_to_binary_                                           */
/*                                                                           */
/* The convert_date_to_binary_ subroutine  converts a character rep-         */
/* resentation of a  date and time into a  72-bit clock reading.  It         */
/* accepts  a wide  variety of  date and  time forms,  including the         */
/* output of the date_time_ subroutine.                                      */
/*                                                                           */
/* USAGE:                                                                    */
/*    dcl convert_date_to_binary_ entry (char (*), fixed bin (71),           */
/*       fixed bin (35));                                                    */
/*    call convert_date_to_binary_ (time_string, clock, code);	       */
/*                                                                           */
/* ARGUMENTS:                                                                */
/* time_string (input)					       */
/*    the time string  to be converted.  See  Multics Programmers' Refer-    */
/*    ence Manual for a description of acceptable strings.                   */
/* clock (output)                                                            */
/*    the resulting clock value.  Unchanged if an error occurs.              */
/* code (output)                                                             */
/*    is a standard  status code.  It can have  one of the following         */
/*    values--                                                               */
/*    error_table_$bad_conversion                                            */
/*    error_table_$dt_ambiguous_time                                         */
/*    error_table_$dt_bad_day_of_week				       */
/*    error_table_$dt_bad_fw                                                 */
/*    error_table_$dt_hour_gt_twelve                                         */
/*    error_table_$dt_multiple_date_spec                                     */
/*    error_table_$dt_multiple_diw_spec                                      */
/*    error_table_$dt_multiple_meaning                                       */
/*    error_table_$dt_multiple_time_spec                                     */
/*    error_table_$dt_multiple_zone_spec                                     */
/*    error_table_$dt_time_conversion_error                                  */
/*    error_table_$dt_size_error                                             */
/*    error_table_$too_many_tokens                                           */
/*    error_table_$dt_unknown_word                                           */
/*                                                                           */
/* ENTRY:  convert_date_to_binary_$relative                                  */
/*                                                                           */
/* This entry point is  similar to the convert_date_to_binary_ entry         */
/* point,  except  that  the  clock  reading  returned  is  computed         */
/* relative  to an  input clock time  rather than  the current clock         */
/* time.  Thus the clock reading  returned for the string "March 26"         */
/* is the clock  reading for the first March  26 following the input         */
/* clock time, rather than the clock  reading for the first March 26         */
/* following the current  clock time.  Given a 72-bit  clock time to         */
/* use, this  entry point converts  a character representation  of a         */
/* date and time to the equivalent 72-bit clock reading.                     */
/*                                                                           */
/* USAGE:                                                                    */
/*    dcl convert_date_to_binary_$relative entry (char (*), fixed            */
/*       bin (71), fixed bin (71), fixed bin(35));                           */
/*    call convert_date_to_binary_$relative (time_string, clock,	       */
/*       clock_in, code);                                                    */
/*                                                                           */
/* ARGUMENTS:                                                                */
/* time_string (Input)					       */
/*    is the character representation of the clock reading desired.          */
/* clock (Output)                                                            */
/*    is the computed clock value relative to the clock_in argument.         */
/* clock_in (Input)                                                          */
/*    is the clock time used to compute the clock value.                     */
/* code (output)                                                             */
/*    is a standard status code.                                             */

convert_date_to_binary_: 
	procedure (time_string, clock_out, code);

     dcl						/*		parameters		*/
	time_string		char(*),		/* date string to be converted. (In)		*/
	clock_out			fixed bin(71),	/* binary clock value. (Out)			*/
	relative_to_clock		fixed bin(71),	/* binary clock value which output clock value	*/
						/*    is relative to. (In)			*/
	fixed_length_sw		bit(1) aligned,	/* on if time_string consists only of		*/
						/*    offsets, where all are smaller than months  */
						/*    (Out)				*/
	code			fixed bin(35);	/* a status code. (Out)			*/

     dcl						/*		automatic variables		*/
	Idelim_type		fixed bin,	/* parse type of delimeter found in string.	*/
	Isearch			fixed bin,	/* index of next string token delimiter.	*/
	Iverify			fixed bin,
	Lnumber_in_str		fixed bin,	/* length of next number found in string.	*/
	Lword_in_str		fixed bin,	/* length of next word found in the string.	*/
	Lstr			fixed bin,	/* length of remainder of input string.		*/
	Ntokens			fixed bin,	/* number of tokens in input string.		*/
	Pstr			ptr,		/* ptr to remainder of input string.		*/
	SIGN			char (1),		/* used for zone differential			*/
	Sspace_encountered		bit(1) aligned,	/* on if previous char of input was whitespace	*/
	a_clock			fixed bin(71),	/* number of micro-seconds in absolute time of day*/
	ambig_sw			bit (1),		/* 1- ambiguous token present			*/
	analyze_sw		bit (1),		/* 1- entered at $analyze entrypoint		*/

	clock_now			fixed bin(71),	/* number of micro-seconds in current time of day.*/
	date_given		bit (1),
	details			char(64)var,
	done			bit (1),
	dow_given			bit (1),
	errloc			fixed bin (24),
	err_pt			ptr,
	fld59			float decimal (59),
	fw_sw			bit (1),		/* FW has been given			*/
	h_time_sw			bit (1),		/* 1- a held time exists			*/
	i			fixed bin,
	ii			fixed bin,
	lang_index		fixed bin,
	lang_temp			bit (18),
	lang_used			bit (18),
	last_adverb		fixed bin,
	lcode			fixed bin (35),
	number			fixed bin(35),	/* a temp for numeric token values.		*/
	offset_sign		float dec (2),	/* multiplier which applies sign to offset values.*/
	rtime_ct			fixed bin,	/* # of relative phrases present.		*/
	rtime_ct_h		fixed bin,	/* held rtime_ct				*/
	rtime_first		bit (1),
	rtime_p			ptr,
	silent			bit (1),		/* 0-print error message w/details		*/
	size_flag			bit (1),		/* enable/disble size condition handler		*/
	tcode			fixed bin (35),
	u_day_of_week		fixed bin,	/* an undetermined (absolute or relative) number	*/
						/*    of a day of the week.			*/
	year_needed		bit (1);
	

     dcl
	1 atime			like time_value,	/* absolute time data given			*/
	1 atime_h			like time_value,	/* hold absolute data given when ambig match	*/
	1 atime_init		like time_value,	/* hold initial value			*/
	1 ctime			like time_value,	/* current time data			*/
	1 ttime			like time_value,	/* yesterday/tomorrow data, if needed		*/
	1 tokens (250)		like token,	/* array of tokens.				*/
	1 rtime			aligned like time_offset based (rtime_p),
	1 rtime_array		aligned like time_offset_array based (rtime_p),
	1 rspace_init (8)		aligned,
	  2 rel_ct		fixed bin,
	  2 dw_required		fixed bin,
	  2 data like time_offset_array,
	1 rspace (8)		aligned,		/* rel time given				*/
	  2 rel_ct		fixed bin,
	  2 dw_required		fixed bin,
	  2 data like time_offset_array,
	1 rspace_h (8)		aligned,		/* held rspace during ambig processing		*/
	  2 rel_ct		fixed bin,
	  2 dw_required		fixed bin,
	  2 data like time_offset_array;

     dcl						/*		based variables		*/
	first_char_of_str		char(1) based (Pstr),
						/* first character of remainder of input string.	*/
	number_in_str		char(Lnumber_in_str) based(Pstr),
	str			char(Lstr) based (Pstr),
						/* remainder of input string.			*/
	word_in_str		char(Lword_in_str) based(Pstr);
						/* next word in the string.			*/
	
     dcl
         (addcharno, addr, addrel, charno, char, clock, convert, copy, dim,
	divide, fixed, hbound, index, length, ltrim, mod, null, rtrim,
	search, string, substr, sum, translate, unspec, verify)
				builtin;


     dcl						/*	conditions and entry points.		*/
	(conversion, size)		condition,
	semantic_error		condition, 
	date_time_$from_clock	entry (fixed bin(71), char(*), ptr, fixed bin(35)),
	date_time_$to_clock		entry (ptr, fixed bin(71), fixed bin(35)),
	date_time_$offset_to_clock	entry (ptr, fixed bin(71), char(*), fixed bin(71), fixed bin(35)),
	ioa_$nnl			entry() options(variable);


     dcl						/*		static variables		*/
        (FALSE			init("0"b),
          TRUE			init("1"b)) bit(1) int static options(constant),
	microseconds_per_day	fixed bin (71) int static options (constant) init (864e8),
         (Tunknown		init(0),			/* Table unknown				*/
	TTnumber		init(8),			/* Token Type xxx				*/
	TTbignum		init(9),
	TTfraction	init(10),
	TTandfraction	init(11),
	TTother		init(12),

	VWbefore		init(1),			/* Value for Word xxx			*/
	VWor		init(2),
	VWafter		init(3),
	VWon		init(4),
	VWnoon		init(5),
	VWmidnight	init(6),
	VWnow		init(7),
	VWyesterday	init(8),
	VWtoday		init(9),
	VWtomorrow	init(10),
	VWfw		init(11),
	VWam		init(12),
	VWpm		init(13),

	VOyear		init(1),			/* Value for Offset xxx			*/
	VOmonth		init(2),
/*	VOweek		init(3),				       */
	VOday		init(4),
	VOhour		init(5),
	VOminute		init(6),
	VOsecond		init(7),
/*	VOmicrosecond	init(8)				       */

	need_default	init (-1),
	need_year		init (-2),
	need_this		init (-3),
	need_yesterday	init (-10),
	need_today	init (-11),
	need_tomorrow	init (-12)
	)			fixed bin int static options(constant),
						/* various token types.			*/
	Ttoken (19)		fixed bin int static options(constant) init (
				     0, 0, 0, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3),
				/*  HT SP  _  +  -  0  1  2  3  4  5  6  7  8  9  .  /  :  ,	*/
				/* Type of lex to be performed for a token beginning with one of the	*/
				/*    above characters.					*/
	Type	fixed bin,
	Value	fixed bin(35);
      dcl (
	error_table_$bad_conversion,
	error_table_$dt_ambiguous_time,
	error_table_$dt_bad_day_of_week,
	error_table_$dt_bad_fw,
	error_table_$dt_hour_gt_twelve,
	error_table_$dt_multiple_meaning,
	error_table_$dt_multiple_date_spec,
	error_table_$dt_multiple_diw_spec,
	error_table_$dt_multiple_time_spec,
	error_table_$dt_multiple_zone_spec,
	error_table_$dt_size_error,
	error_table_$dt_time_conversion_error,
	error_table_$dt_unknown_word,
	error_table_$too_many_tokens
	)					 fixed bin(35) ext static;%page;
/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

	analyze_sw = FALSE;
	if constant_sw
	then clock_now = constant_clock;
	else clock_now = clock();			/* initialize clock value to current time.	*/
	goto COMMON;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


relative:	entry	(time_string, clock_out, relative_to_clock, code);

	analyze_sw = FALSE;
	if constant_sw				/* if we're in testing mode, use the forced	*/
	then clock_now = constant_clock;		/* ..value instead of his.			*/
	else clock_now = relative_to_clock;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

analyze:	entry	(time_string, relative_to_clock, clock_out, fixed_length_sw, code);
	
	analyze_sw = TRUE;
	fixed_length_sw = FALSE;
	if constant_sw				/* if we're in testing mode, use the forced	*/
	then clock_now = constant_clock;		/* ..value instead of his.			*/
	else clock_now = relative_to_clock;
	

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

COMMON:	tcode, lcode, errloc = 0;			/* no errors found during semantic analysis.	*/
	details = "";
	Pfirst_token, err_pt = null();
	h_time_sw = ""b;				/* no held time data			*/
	rtime_ct = 0;				/* no relative date/time values encountered yet.	*/
	rtime_first = "1"b;				/* This is set on initially.			*/
						/* before/on/after will do so also.		*/
	lang_used = copy ("1"b, ti_language.number_lang);	/* show ALL languages used.			*/
	ambig_sw = ""b;				/* and no ambiguous tokens			*/
	code = 0;					/* initialize output value.			*/
	ti_token_p = addr (time_info_$tokens);

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **/
	/* 								         */
	/* Parse the input string into tokens structured like those created by lex_string_ and   */
	/* used with reductions (see reduction_compiler).  lex_string_ is not used because we    */
	/* don't need all of its power, and because our parse associates semantic information    */
	/* with the tokens during the parsing.					         */
	/* 								         */
	/* A token is a lexeme of a time string. Examples are 12, /, Monday, Janvier, 1979,      */
	/* etc.  The parser below finds the tokens in the string.  Associated with each token is */
	/* a token type and a numeric value.  The numeric value depends upon the type of token.  */
	/* Numeric character string tokens use the numeric value of the character string         */
	/* (converted according to PL/I conversion rules).  Month names have their month number  */
	/* as a numeric value.  The same is true for day names (1=Mon,...,7=Sun).  Offset words  */
	/* (like weeks, days, etc) have a value from 1 to 7 (1=years,2=months,...7=seconds).  As */
	/* each token is found, it is added to a chain of tokens by the token_ procedure. token_ */
	/* also removes the token from the string, leaving only unprocessed string characters to */
	/* be parsed.							         */
	/* 	Due to the multiple languages, there are tokens which are ambiguous.  This	*/
	/* means that it does not mean the same thing in all languages.  When one of these guys	*/
	/* is encountered, it is flagged as "unknown".  These steps are taken to decide what a	*/
	/* token is to be used as.							*/
	/*   1) All nonambiguous tokens have their language bits ANDed.  If this results in	*/
	/*      all zeroes, there is a mixed language error.				*/
	/*   2) A multiple parse will be undertaken, first with the process default language,	*/
	/*      then the system default, then with any left in the order they appear in		*/
	/*      time_info_.  HOWEVER, the nonambiguous tokens will have restricted the set of	*/
	/*      possible languages.							*/
	/*         The unknown tokens do not have their numeric value set.  During each pass,	*/
	/*      when a semantic function decides a token can be the type it is looking for it	*/
	/*      will plug in the appropriate numeric value while leaving the type unknown.	*/
	/*      The action routine will then be able to pick up the value without extra work.	*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **/

	
	Pstr = addr(time_string);		/* address time string.			*/
	Lstr = length(time_string);
	silent = ^time_defaults_$debug;
	Ntokens = 0;				/* No tokens found yet.			*/
	on conversion				/* handle number conversion errors.		*/
	begin;
	   lcode = error_table_$bad_conversion;
	   goto set_err_loc;
	end;
	size_flag = ""b;
	on size
	begin;
	   if size_flag
	   then do;				/* we know it is all digits, but it might	*/
	      Type = TTbignum;			/* ..not fit in a fixed bin (35).  Thats OK.	*/
	      number = -1;
	      goto lex1a;
	   end;
	   lcode = error_table_$dt_size_error;
	   goto set_err_loc;
	end;

	Sspace_encountered = TRUE;			/* Pretend string begins with whitespace.	*/
	do while (Lstr > 0);			/* parse the string.			*/
/****          Search string begins with HT SP _ + ...			       			*/
	     Isearch = search (str, "	 _+-0123456789./:,");
	     if Isearch = 0 then Isearch = Lstr + 1;	/* find first delimiter character in string.	*/

	     if Isearch > 1 then do;			/* Was a word found in the string preceding the.	*/
						/*   delimiter?				*/
		Sspace_encountered = FALSE;
re_try:
		Lword_in_str = Isearch-1;
		if length(word_in_str) > 32 then goto Eunknown_word;
						/* make sure word isn't too long.		*/
		item_p = find_time_name (word_in_str);
		if (item_p ^= null())
		then do;				/* word was found				*/
		   Type = item.table (1);
		   Value = item.element (1);
		   if (item.count > 1) & (Type ^= This_table)
		   then do;			/* we can't tell yet			*/
		      lang_temp = ""b;
		      do ii = 1 to item.count;
		         lang_temp = lang_temp | item.in_lang (ii);
		      end;
		      lang_used = lang_used & lang_temp;
		      call token_ (length(word_in_str), Tunknown, 0);
		      ambig_sw = "1"b;		/* remember its ambiguous			*/
		   end;
		   else do;			/*   create a token for the word.		*/
		      call token_ (length(word_in_str), Type, Value);
		      lang_used = lang_used & item.in_lang (1);	/* only 1 element */
		   end;
		   token.Psemant = item_p;		/* keep ptr to token info			*/
		end;
		else do;				/* the generated zones end in digits, so before	*/
						/* giving up, lets try gathering some digits	*/
						/* also.					*/
		   Isearch = search (str, "	 _+-./:,");
		   if Isearch = 0 then Isearch = Lstr + 1;
		   if (Isearch-1 > Lword_in_str)	/* If that increased the token size, we'll	*/
						/*   try once again.			*/
		   then goto re_try;
		   goto Eunknown_word;		/* Oops!  You shouldn't say that.		*/
		end;
		if Lstr <= 0 then goto end_scan;	/* Stop scan if word at end of time string.	*/
		end;

	     Idelim_type = Ttoken (index ("	 _+-0123456789./:,", first_char_of_str));    /* HT SP _ ... */
	     goto lex(Idelim_type);			/* we know first char of str is now a delimiter	*/
						/*   because token_ would skip any word which	*/
						/*   might have preceeded the delimiter.	*/
						/*   Continue lexing according to delimiter type.	*/

lex(0):	     Sspace_encountered = TRUE;
	     Iverify = verify (str, "	 _");		/* skip leading white space (HT SP or _)	*/
	     if Iverify = 0 then Iverify = Lstr + 1;
	     Pstr = addcharno (Pstr,(Iverify-1));
	     Lstr = Lstr - (Iverify-1);
	     goto end_scan;

lex(1):	     Iverify = verify(str, "0123456789");	/* find out how long numeric field is.		*/
	     if Iverify = 0 then Lnumber_in_str = Lstr;
	     else Lnumber_in_str = Iverify - 1;
	     Type = TTnumber;
	     size_flag = "1"b;
	     number = convert(number, number_in_str);	/* This conversion is done because some range	*/
	     size_flag = ""b;			/* ..checking is done, so we need the value.	*/
lex1a:
	     call token_ (length(number_in_str), Type, number);
	     if (Lstr = 0)
	     then goto end_scan;
	     if (substr (str, 1, 1) ^= ".")
	     then goto end_scan;
	     Type = TTandfraction;
	     goto lex2a;

lex(2):	     Type = TTfraction;
lex2a:
	     if (Lstr > 1)
	     then do;
	        Iverify = verify(substr (str, 2), "0123456789");	/* find out how long fraction field is.		*/
	        if Iverify = 0 then Lnumber_in_str = Lstr;
	        else Lnumber_in_str = Iverify;
	     end;
	     else Lnumber_in_str = 1;
	     if (Lnumber_in_str > 0)
	     then call token_ (Lnumber_in_str, Type, 0);
	     Sspace_encountered = FALSE;
	     goto end_scan;

lex(3):	     					/* found "+", "-", "/", ":", or ","		*/
	     Sspace_encountered = FALSE;
	     call token_ (1, TTother, 0);		/*    create a token for the break character.	*/

end_scan:	     end;					/* continue lexing with the next field.		*/

	revert conversion, size;			/* get rid of this thing when not needed.	*/
	if Ntokens = 0 then do;
	     clock_out = clock_now;
	     goto exit;
          end;
						/* analyze semantic content of any lexed tokens.	*/
	if (lang_used = ""b)			/* --NO COMMON LANGUAGE--			*/
	then do;
	   lcode = error_table_$dt_ambiguous_time;
	   goto error_exit;
	end;

	unspec (atime_h) = ""b;
	unspec (rspace_h) = ""b;
	unspec (atime_init) = ""b;
	atime_init.version = Vtime_value_3;
	atime_init.yc, atime_init.my, atime_init.dm = need_default;
	atime_init.Hd, atime_init.MH, atime_init.SM, atime_init.US = need_default;
	atime_init.za = "";
	ctime = atime_init;

	unspec (rspace_init) = ""b;
	rtime_p = addr (rspace_init (1).data);
	rtime.version = Vtime_offset_2;
	rtime_array.val (*) = 0;				/* must have decimal zeroes there		*/
	do i = 2 to dim (rspace_init, 1);
	   rspace_init (i) = rspace_init (1);
	end;
	on condition (semantic_error)
	begin;
	   goto parse_fail;
	end;
	done = ""b;
	do lang_index = time_defaults_$language_index,
	   time_info_$default_language_index while (^done),
	   1 to ti_language.number_lang while (^done);
	   if substr (lang_used, lang_index, 1)
	   then do;
	      if ^silent & ambig_sw
	      then call ioa_$nnl ("***Trying ^a.^/", ti_language.name (1, lang_index));
	      substr (lang_used, lang_index, 1) = ""b;	/* show its been used			*/
	      rspace = rspace_init;
	      atime = atime_init;
	      fw_sw = ""b;
	      u_day_of_week = 0;			/* absolute/relativeness undetermined.		*/

						/* analyze semantic content of lexed tokens.	*/
	      Pthis_token = addr(tokens(1));
	      tcode, lcode, errloc = 0;
	      err_pt = null();
	      call SEMANTIC_ANALYSIS();
/****	      We don't know what day_of_week stands for yet, just preserve */
/****	      until later.					       */
	      atime.dw = u_day_of_week;
parse_fail:
	      call error_display;
	      done = ^ambig_sw;
	      if (lcode = 0)
	      then do;
	         if ^ambig_sw
	         then goto parse_success;
/****           When ambiguity exists, we must try all selected languages. 				*/
/****           If there is only 1 match, we assume we are safe.  Otherwise we				*/
/****	      must complain that we can't tell.							*/
	         if ^h_time_sw			/* is this first match?			*/
	         then do
		  h_time_sw = "1"b;			/* ..yes, remember the set of data found.	*/
		  atime_h = atime;
		  rspace_h = rspace;
		  rtime_ct_h = rtime_ct;
	         end;
	         else if (unspec (atime_h) ^= unspec (atime))	/* ..no, did we find a different meaning?	*/
		  | (unspec (rspace_h) ^= unspec (rspace))
	         then do;				/* ..yes					*/
		  lcode = error_table_$dt_multiple_meaning;
		  goto error_exit;			/* Sorry ma'm, no can do.			*/
/**** At this point we could trigger a 2nd pass which will gather info to give a more meaningful message.	*/
	         end;
	      end;
	   end;
	end;
	call error_display;
	if ^h_time_sw				/* if there was no match, we are all done	*/
	then do;
error_exit:
	   call error_display;
	   code = lcode;
exit:
	   return;
	end;

error_display: proc;
	   if (lcode = 0) | (lcode = tcode)
	   then return;
	   if ^silent
	   then do;
	      if (Pfirst_token ^= null())
	      then call ioa_$nnl ("^/");
	      if (lcode ^= 0)
	      then do;
	         if (err_pt ^= null())
	         then errloc = charno (err_pt) - charno (addr (time_string)) + 1;
	         call com_err_ (lcode, "convert_date_to_binary_", "^[
^a^;^s^]^[
String is: ""^va""
 error at: ^vx^^^]", (details ^= ""), details,
		  (errloc > 0), length (time_string), time_string, errloc);
	      end;
	   end;
	   tcode = lcode;
	end error_display;%page;
parse_success:
	if h_time_sw
	then do;
	   atime = atime_h;				/* bring back the remembered success		*/
	   rspace = rspace_h;
	   rtime_ct = rtime_ct_h;
	end;
/****     Now retrieve day_of_week, so that we can determine what it is.     */
	u_day_of_week = atime.dw;
	atime.dw = 0;

	Lstr = 80;				/* keeps debugging code working		*/
	year_needed, date_given, dow_given = ""b;
						/* get current date/time values.		*/

	if analyze_sw				/* For $analyze entrypoint,			*/
	then do;					/*  the time_string is a fixed-length increment   */
	   if unspec(atime) = unspec(atime_init) then	/*  iff no absolute date/time info is given, and  */
	   if u_day_of_week = 0 then			/*  only week, day, hour, minute, second and/or   */
	   if rtime_ct > 0 then do;			/*  microsecond offsets are given.                */
	      do i = rtime_ct to 1 by -1;
	         rtime_p = addr(rspace(i).data);
	         if rtime.flag.yr + rtime.flag.mo > UNUSED
	         then go to NOT_FIXED_LENGTH;
	         end;
	      fixed_length_sw = TRUE;
	      end;
NOT_FIXED_LENGTH:
	   end;

	if (atime.za = "#") then atime.za = "gmt";	/* request_id defaults to GMT			*/
	call date_time_$from_clock (clock_now, atime.za, addr (ctime), code);
	if u_day_of_week ^= 0 then			/* apply defaults to unset date/time values.	*/
	     if atime.my ^= need_default then		/*    day_of_week is absolute if user also gave	*/
		atime.dw = u_day_of_week;		/*       a date specification.		*/
	     else do;				/*    otherwise, it is a day_of_week date,	*/
	          date_given = "1"b;
		call init_rtime;
		rtime.flag.da = USED;
		rtime.val.da = rtime.val.da + mod (u_day_of_week - ctime.dw + 6, 7) + 1;
		end;

	if (atime.yc = need_this) then atime.yc = ctime.yc;
	if (atime.my = need_this) then atime.my = ctime.my;
	if (atime.dm = need_this) then atime.dm = ctime.dm;
	if (atime.Hd = need_this) then atime.Hd = ctime.Hd;
	if (atime.MH = need_this) then atime.MH = ctime.MH;
	if (atime.SM = need_this) then atime.SM = ctime.SM;

	if atime.my = need_yesterday
	then do;
	   ttime = atime_init;
	   call date_time_$from_clock (clock_now - microseconds_per_day, atime.za,
	      addr (ttime), code);
	   atime.yc = ttime.yc;
	   atime.my = ttime.my;
	   atime.dm = ttime.dm;
	end;
	else if atime.my = need_today
	then do;
	   atime.yc = ctime.yc;
	   atime.my = ctime.my;
	   atime.dm = ctime.dm;
	end;
	else if atime.my = need_tomorrow
	then do;
	   ttime = atime_init;
	   call date_time_$from_clock (clock_now + microseconds_per_day, atime.za,
	      addr (ttime), code);
	   atime.yc = ttime.yc;
	   atime.my = ttime.my;
	   atime.dm = ttime.dm;
	end;
	if atime.my = need_default	 		/* supply month-day if needed		*/
	then do;
	   atime.my = ctime.my;
	   atime.dm = ctime.dm;
	end;
	else date_given = "1"b;
	if atime.yc < 0 then do;			/* supply year if needed--		*/
						/*   this_year or a missing year.		*/
	   if atime.yc = need_year			/* missing year-- assume current year for now.	*/
	   then year_needed = "1"b;
	   atime.yc = ctime.yc;
	end;
	if atime.Hd = need_default then do;		/* supply time defaults if needed	*/
	     atime.Hd = ctime.Hd;
	     atime.MH = ctime.MH;
	     atime.SM = ctime.SM;
	     atime.US = ctime.US;
	     end;

/****	  else if (atime.Hd=24)...				       */
/****          In the old days, the "24" times were handled right here.      */
/****          But now date_time_$to_clock handles them instead.	       */
	     
/**** At this point, yc/my/dm/Hd/MH/SM/US have been filled in from "now" if they were not given.	*/
          call date_time_$to_clock (addr (atime), clock_out, code);
	if atime.dw > 0 & code = error_table_$dt_bad_day_of_week
	then do;
	   if clock_out >= clock_now |		/* if day-of-week doesn't match and a   */
	     (^year_needed & date_given)		/* specific date was given, report an   */
	   then go to exit;				/* error now.			*/
	   atime.dw = 0;				/* Otherwise, delay dow check until the */
						/* specific date is known.		*/
	   call date_time_$to_clock (addr (atime), clock_out, code);
	   call init_rtime;				/* setup to do dow checks below.	*/
	   rspace(rtime_ct).dw_required = u_day_of_week;
	   rtime_first = "1"b;
	   atime.dw = 0;
	end;
          if code ^= 0
	then go to exit;
	if (clock_out < clock_now)			/* if value as defaulted is less than	*/
	then do;					/* ..NOW, we have to adjust forward.	*/
	   if year_needed
	   then do;
	      call init_rtime;
	      rtime.flag.yr = USED;
	      rtime.val.yr = rtime.val.yr +1;
	   end;
	   else if ^date_given
	   then clock_out = clock_out + microseconds_per_day;
	end;

	do i = rtime_ct to 1 by -1 while (code = 0);
	   if rspace(i).dw_required > UNUSED then do;
	      call date_time_$from_clock (clock_out, atime.za, addr(atime), code);
	      if atime.dw ^= rspace(i).dw_required
	      then do;
	         code = error_table_$dt_bad_day_of_week;
	         goto exit;
	      end;
	   end;
	   if sum(rspace(i).data.flag(*)) > UNUSED | 
	      rspace(i).data.dw.flag ^= UNUSED
	   then call date_time_$offset_to_clock (addr (rspace (i).data),
	      clock_out, atime.za, clock_out, code);
	end;
	goto exit;%skip(2);
Eunknown_word:
	lcode = error_table_$dt_unknown_word;
	goto set_err_loc;

Etoo_many_adverbs:
	details = "Only " || ltrim (char (hbound (rspace,1)))
	   || " offset groups may be used.";
	lcode = error_table_$too_many_tokens;
	err_pt = token.Pvalue;
	goto error_exit;

Etoo_many_tokens:
	lcode = error_table_$too_many_tokens;
set_err_loc:
	err_pt = Pstr;
	goto error_exit;%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*					       */
	/* The following 2 entries are an aid to debugging.      */
	/* The "set" entry allows a person to fix the reference  */
	/* value to any point they wish.  The "unset" entry      */
	/* returns things to the normal state.  After having     */
	/* called "set", all calls to $convert_date_to_binary_   */
	/* function as if the $relative entry had been called    */
	/* with the saved value.			       */
	/*					       */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

dcl constant_sw	bit (1) int static init (""b);
dcl constant_clock	fixed bin (71) int static;

unset: entry;

          constant_sw = ""b;
	return;

set: entry;
/****     Wrap this code up in a begin block to make 100% sure that nobody outside depends	*/
/****     on anything herein.  Then we can be sure that it may be deleted without effect.		*/
          begin;
	   call cu_$arg_ptr (1, arg_p, arg_l, code);
	   if (code ^= 0)
	   then constant_clock = clock();
	   else do;
	      call convert_date_to_binary_ (arg, a_clock, code);
	      if (code ^= 0)
	      then do;
	         call com_err_ (code, "convert_date_to_binary_$set", "^a", arg);
	         return;
	      end;
	      constant_clock = a_clock;
	   end;
	   constant_sw = "1"b;
	   return;
     dcl
	cu_$arg_ptr		entry (fixed bin, ptr, fixed bin(21), fixed bin(35)),
	arg			char (arg_l) based (arg_p),
	arg_l			fixed bin (21),
	arg_p			ptr,
	code			fixed bin (35);
     end;%page;
dcl Pfirst_token	ptr;
SL: proc (red);
dcl red		fixed bin;	/* reduction to display	       */

        if ^silent
        then do;
	 Pfirst_token = Ptoken;
	 call ioa_$nnl (string (SLtext), red);
        end;
        return;

SLe: entry;

        if ^silent
        then do;
	 do Ptoken = Pfirst_token repeat (token.Pnext) while ((Ptoken ^= null()) & (Ptoken ^= Pthis_token));
	    call ioa_$nnl (" ^a", token_value);
	 end;
	 call ioa_$nnl ("^/");
	 Pfirst_token = null();
	 Ptoken = Pthis_token;
        end;

				/* format: off */
dcl 1 SLtext	unaligned int static options (constant),
    2 x0  char ( 3) init ("^3x"),
    2 x1	char (20) init ("^[<month-name> <day>"),
    2 x2	char (20) init ("^;<day> <month-name>"),
    2 x3	char (26) init ("^;<month> / <day> / <year>"),
    2 x4	char (17) init ("^;<month> / <day>"),
    2 x5	char (26) init ("^;<year> - <month> - <day>"),
    2 x6	char (29) init ("^;<fiscal-indicator> <number>"),
    2 x7	char (34) init ("^;<number> <and-fraction> <offset>"),
    2 x8	char (14) init ("^;<request-id>"),
    2 x9	char (23) init ("^;<HHMM> <and-fraction>"),
    2 x10	char (19) init ("^;<hour> : <minute>"),
    2 x11	char (29) init ("^;<hour> <meridiem-indicator>"),
    2 x12	char (15) init ("^;12 <half-day>"),
    2 x13	char (12) init ("^;<half-day>"),
    2 x14	char (14) init ("^;<day-name> ,"),
    2 x15	char (21) init ("^;<day-name> <adverb>"),
    2 x16	char (12) init ("^;<day-name>"),
    2 x17	char (26) init ("^;<sign> <number> <offset>"),
    2 x18	char (19) init ("^;<number> <offset>"),
    2 x19	char (41) init ("^;<sign> <number> <and-fraction> <offset>"),
    2 x20	char (21) init ("^;<fraction> <offset>"),
    2 x21	char (28) init ("^;<sign> <fraction> <offset>"),
    2 x22	char ( 8) init ("^;<zone>"),
    2 x23	char ( 8) init ("^;<word>"),
    2 x24	char (10) init ("^;<adverb>"),
    2 x25	char (13) init ("^;or <adverb>"),
    2 x26	char (27) init ("^;: <second> <and-fraction>"),
    2 x27	char (12) init ("^;: <second>"),
    2 x28	char (19) init ("^;<minute-fraction>"),
    2 x29	char (22) init ("^;<meridiem-indicator>"),
    2 x30	char (21) init ("^;<zone-differential>"),
    2 x31	char (10) init ("^;, <year>"),
    2 x32	char ( 8) init ("^;<year>"),
    2 x99 char ( 8) init ("^]^25.1t");	/* format: on */

     end SL; %page;
/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/****    A very similar routine exists in get_word_index in date_time_.pl1.				*/

find_time_name: proc (val) returns (ptr);

dcl val		char (*);

dcl (lb, hb)	fixed bin;
dcl symb		char (32) var;
dcl cur_token	fixed bin;
dcl (az		init ("abcdefghijklmnopqrstuvwxyz"),
     AZ		init ("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
		char (26) int static options (constant);

      symb = translate (val, az, AZ);	/* get to normal form		*/
      symb = rtrim (symb);		/* minimize compare time		*/

      lb = 1;
      hb = ti_token.count;
      do while (lb <= hb);
         cur_token = divide (lb + hb, 2, 17, 0);
         if (ti_token.symbol (cur_token) = symb)
         then return (addrel (addr (time_info_$version),
	       ti_token.list_r (cur_token)));
         if (ti_token.symbol (cur_token) < symb)
         then lb = cur_token + 1;
         else hb = cur_token - 1;
      end;
      return (null ());

   end find_time_name; %skip (5);
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/*	SEMANTIC FUNCTIONS  and  ACTION ROUTINES				*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


day_name:	procedure returns (bit(1) aligned);		/* semantic functions which check for token type.	*/

	Twhich1 = Day_table;
	goto word_check;

fraction:	entry returns (bit(1) aligned);

	if token.Itoken_in_stmt = TTfraction then goto true;
	goto false;

andfraction:	entry returns (bit(1) aligned);

	if token.Itoken_in_stmt = TTandfraction then goto true;
	goto false;

andfraction6:	entry returns (bit(1) aligned);

	if token.Itoken_in_stmt = TTandfraction
	then if length (token_value) = 7 then goto true;
	goto false;
fweek: entry returns (bit(1) aligned);

          Twhich1, Twhich2, Twhich3 = VWfw;
	goto word_table_check;


half_day: entry returns (bit(1) aligned);

	Twhich1 = VWnoon;
	Twhich2, Twhich3 = VWmidnight;

word_table_check:
	if (token.Itoken_in_stmt = Word_table)
	then
	   if (token.Nvalue = Twhich1) | (token.Nvalue = Twhich2)
	   | (token.Nvalue = Twhich3)
	   then goto true;
	if (token.Itoken_in_stmt ^= Tunknown) then goto false;

	item_p = token.Psemant;
	do elem = 1 to item.count;
	   if substr (item.in_lang (elem), lang_index, 1)	/* if defined in current language	*/
	   then if (Word_table = item.table (elem))	/* ..and is a type we want		*/
	   then do;				/* ..its a winner			*/
	      if (Twhich1 = item.element (elem))
	      then do;
	         token.Nvalue = Twhich1;
	         goto true;
	      end;
	      if (Twhich2 = item.element (elem))
	      then do;
	         token.Nvalue = Twhich2;
	         goto true;
	      end;
	      if (Twhich3 = item.element (elem))
	      then do;
	         token.Nvalue = Twhich3;
	         goto true;
	      end;
	   end;
	end;
	goto false;

meridian:	entry returns (bit(1) aligned);

	Twhich1 = VWam;
	Twhich2, Twhich3 = VWpm;
	goto word_table_check;

on:	entry returns (bit(1) aligned);

	Twhich1, Twhich2, Twhich3 = VWon;
	goto word_table_check;

before_on_after:	entry returns (bit(1) aligned);

	Twhich1 = VWbefore;
	Twhich2 = VWon;
	Twhich3 = VWafter;
	goto word_table_check;

before_after:	entry returns (bit(1) aligned);

	if (last_adverb = VWon)
	then do;
	   Twhich1 = VWbefore;
	   Twhich2, Twhich3 = VWafter;
	end;
	else Twhich1, Twhich2, Twhich3 = VWon;
	goto word_table_check;

now:	entry returns (bit(1) aligned);
	Twhich1, Twhich2, Twhich3 = VWnow;
	goto word_table_check;

or:	entry returns (bit(1) aligned);
	Twhich1, Twhich2, Twhich3 = VWor;
	goto word_table_check;

today:	entry returns (bit(1) aligned);
	Twhich1, Twhich2, Twhich3 = VWtoday;
	goto word_table_check;

tomorrow:	entry returns (bit(1) aligned);
	Twhich1, Twhich2, Twhich3 = VWtomorrow;
	goto word_table_check;

yesterday:	entry returns (bit(1) aligned);
	Twhich1, Twhich2, Twhich3 = VWyesterday;
	goto word_table_check;

day: entry returns (bit(1) aligned);
	if this_()
	then do;
	   Twhich1 = VOday;
	   goto offset_table_check_next;
	end;
	goto n_;


month: entry returns (bit(1) aligned);
	if this_()
	then do;
	   Twhich1 = VOmonth;
	   goto offset_table_check_next;
	end;
	goto n_;


year: entry returns (bit(1) aligned);
	if this_()
	then do;
	   Twhich1 = VOyear;
	   goto offset_table_check_next;
	end;
	goto n_;


hour: entry returns (bit(1) aligned);
	if this_()
	then do;
	   Twhich1 = VOhour;
	   goto offset_table_check_next;
	end;
	goto n_;


minute: entry returns (bit(1) aligned);
	if this_()
	then do;
	   Twhich1 = VOminute;
	   goto offset_table_check_next;
	end;
	goto n_;


second: entry returns (bit(1) aligned);
	if this_()
	then do;
	   Twhich1 = VOsecond;
	   goto offset_table_check_next;
	end;
	goto n_;


month_name: entry returns (bit(1) aligned);

	if this_()
	then do;
	   Twhich1 = VOmonth;
	   goto offset_table_check_next;
	end;
	Twhich1  = Month_table;
	goto word_check;


offset_table_check_next:
	if token.Pnext = null then goto false;
	Ptoken = token.Pnext;
	if (token.Itoken_in_stmt = Offset_table)
	then
	   if (token.Nvalue = Twhich1)
	   then goto true;
	if (token.Itoken_in_stmt ^= Tunknown) then goto false;

	item_p = token.Psemant;
	do elem = 1 to item.count;
	   if substr (item.in_lang (elem), lang_index, 1)	/* if defined in current language	*/
	   then if (Offset_table = item.table (elem))	/* ..and is a type we want		*/
	   then do;				/* ..its a winner			*/
	      if (Twhich1 = item.element (elem))
	      then do;
	         token.Nvalue = Twhich1;
	         goto true;
	      end;
	   end;
	end;
	goto false;

N:	entry returns (bit(1) aligned);

	if token.Itoken_in_stmt = TTbignum then goto true;

n:
	entry returns (bit(1) aligned);

n_:
	if token.Itoken_in_stmt = TTnumber then goto true;
	goto false;
n4:
	entry returns (bit(1) aligned);

	if token.Itoken_in_stmt = TTnumber then
	     if length(token_value) = 4 then goto true;
	goto false;


n12: entry returns (bit(1) aligned);

	if (token.Itoken_in_stmt = TTnumber) | (token.Itoken_in_stmt = TTbignum)
	     then if length(token_value) = 12 then goto true;
	goto false;


offset:	entry returns (bit(1) aligned);

	Twhich1 = Offset_table;
	goto word_check;


this_:	entry returns (bit(1) aligned);

	Twhich1 = This_table;
	goto word_check;


twelve:	entry returns (bit(1) aligned);

	if token.Itoken_in_stmt = TTnumber then
	     if token.Nvalue = 12 then
		goto true;
	goto false;


sign:	entry returns (bit(1) aligned);

	if token_value = "+" | token_value = "-" then goto true;
	goto false;
	

zone:	entry returns (bit(1) aligned);

	Twhich1 = Zone_table;

/**** GCD: This check won't work if token means 2 different values in 2 different languages,	*/
/****     but both values are in the same table.						*/
/**** JAF: I'm not sure it can ever get here if that is the case.  More thought is needed		*/
/****	on having time_info_.cds preventing this kind of a table being constructed.  Or get	*/
/****	figured out when that could happen and still be parsable and then learn to handle it.	*/

word_check:
	item_p = token.Psemant;
	if (token.Itoken_in_stmt = Twhich1) then goto true;	/* token.Nvalue is already set	*/
	if (token.Itoken_in_stmt ^= Tunknown) then goto false;
	do elem = 1 to item.count;
	   if substr (item.in_lang (elem), lang_index, 1)		/* if defined in current language	*/
	   then if (Twhich1 = item.table (elem))
	   then do;					/* ..and is the type we want		*/
	      token.Nvalue = item.element (elem);		/* its a winner			*/
	      goto true;
	   end;
	end;

false:	return (FALSE);

true:	return (TRUE);

     dcl (Twhich1, Twhich2, Twhich3, elem)			fixed bin;

	end day_name;
/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

Aday_and_LEX:	procedure;			/* action routines which apply input token value	*/
						/*    to absolute/relative date/time, as named in	*/
						/*    entry point.				*/

	if atime.dm ^= need_default then goto Emultiple_date_spec;
	if this_()
	then do;
	   call LEX (1);
	   atime.dm = need_this;
	end;
	else atime.dm = token.Nvalue;
	call LEX (1);
	return;

Ab_o_a:	entry;

	call init_rtime;
	if (rspace.rel_ct (rtime_ct)  ^= 1)
	then do;
	   details = "Cannot mix <day_name> with <offsets>.";
	   lcode = error_table_$dt_time_conversion_error;
	   err_pt = token.Pvalue;
	   signal condition (semantic_error);
	end;
	if (token.Nvalue = VWbefore)			/* <day_name> <before_after>			*/
	then rtime.dw.flag = BEFORE;
	if (token.Nvalue = VWafter)
	then rtime.dw.flag = AFTER;
	rtime.dw.val = u_day_of_week;
	if (token.Nvalue = VWon)			/* <day_name> <on>				*/
	then rspace.dw_required(rtime_ct) = u_day_of_week;
	u_day_of_week = 0;
	last_adverb = token.Nvalue;
	return;

Ab_a:	entry;

	call init_rtime;
	if (rspace.rel_ct (rtime_ct) < 2)   		/* there must be something to apply to		*/
	then do;
	   details = "Nothing for adverb to apply to.";
	   lcode = error_table_$dt_time_conversion_error;
	   err_pt = token.Pvalue;
	   signal condition (semantic_error);
	end;
	begin;					/* <day_name> <before_after> <or> <on>		*/
dcl sign		builtin;
	   if (token.Nvalue = VWon)
	   then rtime.dw.flag = rtime.dw.flag - sign (rtime.dw.flag);
	end;
	if (token.Nvalue = VWbefore)			/* <day_name> <on> <or> <before_after>		*/
	then rtime.dw.flag = ON_OR_BEFORE;
	if (token.Nvalue = VWafter)
	then rtime.dw.flag = ON_OR_AFTER;
	if rtime.dw.flag ^= UNUSED
	then rspace.dw_required(rtime_ct) = UNUSED;
	return;
	
	

Afw:	entry;

	if fw_sw | (atime.yc ^= need_default) |(atime.my ^= need_default)
	then goto Emultiple_date_spec;
	atime.yc, atime.my, atime.dm = 0;
	if (length (token_value) = 6) | (length (token_value) = 5)
	then do;
	   atime.fw = token.Nvalue;
	   fw_sw = "1"b;
	   return;
	end;
	lcode = error_table_$dt_bad_fw;
	err_pt = token.Pvalue;
	signal condition (semantic_error);
	

Ahalf_day:	entry;

	if atime.Hd ^= need_default then goto Emultiple_time_spec;
	if (token.Nvalue = VWnoon)
	then atime.Hd = 12;
	else atime.Hd = 0;
	atime.MH = 0;
	atime.SM = 0;
	atime.US = 0;
	return;


Ahour_and_LEX:	entry;

	if atime.Hd ^= need_default then goto Emultiple_time_spec;
	if this_()
	then do;
	   call LEX (1);
	   atime.Hd = need_this;
	end;
	else atime.Hd = token.Nvalue;
	call LEX (1);
	return;


Ameridian:	entry;

          if atime.Hd > 12 then goto Ehr_gt_12;
	if atime.Hd = 12
	then atime.Hd = 0;
	if (token.Nvalue = VWpm)
	then atime.Hd = atime.Hd + 12;
	return;


Arequest_id: entry;

	if (atime.yc ^= need_default) | (atime.my ^= need_default)
	then goto Emultiple_date_spec;
	if (atime.Hd ^= need_default)
	then goto Emultiple_time_spec;
	atime.yc = CONVERT_TO_4_DIGIT_YEAR (fixed (rqid.yc));
	atime.my = fixed (rqid.my);
	atime.dm = fixed (rqid.dm);
	atime.Hd = fixed (rqid.Hd);
	atime.MH = fixed (rqid.MH);
	atime.SM = fixed (rqid.SM);
	atime.US = fixed (rqid.US);
	fw_sw = "1"b;
	if (atime.za = "") then atime.za = "#";
	return;

dcl 1 rqid	based (token.Pvalue),
      2 (yc, my, dm, Hd, MH, SM) char (2),
      2 fill	char (1),
      2 US	char (6);


Asecond_fraction:	entry;

	atime.US = get_fraction() * 1e6;		/* token value is the <fraction> of the reduction */
						/* which parses  HH:MM:SS.fraction		*/
	return;

Aminute_fraction:	entry;

	if (token_value = ".")
	then atime.SM, atime.US = 0;			/* 	hhmm.				*/
	else do;
	   atime.US = get_fraction() * 6e7;		/* token value is the <fraction> of reduction:	*/
	   atime.SM = divide (atime.US, 1000000, 17, 0);	/* 	hhmm.fraction			*/
	   atime.US = atime.US - (atime.SM * 1000000);
	end;
	return;


Aminute_and_LEX:	entry;

	if this_()
	then do;
	   call LEX (1);
	   atime.MH = need_this;
	end;
	else atime.MH = token.Nvalue;
	call LEX (1);
	return;


Aminute_zero:	entry;

	atime.MH = 0;
	return;


Amonth_and_LEX:	entry;

	if atime.my ^= need_default then goto Emultiple_date_spec;
	if this_()
	then do;
	   call LEX (1);
	   atime.my = need_this;
	end;
	else atime.my = token.Nvalue;
	call LEX (1);
	fw_sw = "1"b;
	return;


Asecond_and_LEX:	entry;

	if this_()
	then do;
	   call LEX (1);
	   atime.SM = need_this;
	end;
	else atime.SM = token.Nvalue;
	call LEX (1);
	atime.US = 0;
	return;


Asecond_zero:	entry;

	atime.SM = 0;
	atime.US = 0;
	return;

Atime:	entry;

	if atime.Hd ^= need_default then goto Emultiple_time_spec;
	atime.Hd = divide(token.Nvalue, 100, 17, 0);
	atime.MH = mod(token.Nvalue, 100);
	return;

Aadverb:	entry;
	if rtime_first
	then do;			/* nothing preceeds the adverb       */
	   details = "Nothing for adverb to apply to.";
	   lcode = error_table_$dt_time_conversion_error;
	   goto set_err_loc;
	end;
	if (rtime.dw.flag ^= UNUSED)	/* day_name cannot be mixed with     */
	then do;			/* ...offsets		       */
	   details = "Cannot mix <day_name> with <offsets>.";
	   lcode = error_table_$dt_time_conversion_error;
	   goto set_err_loc;
	end;
	if (unspec (atime) ^=  unspec (atime_init))	/* all adverbs must preceed any	*/
	then do;					/* ...absolute specs		*/
	   details = "All adverbial offsets must precede absolute data.";
	   lcode = error_table_$dt_time_conversion_error;
	   goto set_err_loc;
	end;
	if (token.Nvalue = VWbefore)
	then do;
	   do i = 1 to 8;
	      rtime_array.val(i) = - rtime_array.val(i);
	   end;
	end;	
	rtime_first = "1"b;
	return;


Anow:	entry;
	if atime.Hd ^= need_default then goto Emultiple_time_spec;
	atime.Hd = ctime.Hd;
	atime.MH = ctime.MH;
	atime.SM = ctime.SM;
	atime.US = ctime.US;
	return;

Atoday:	entry;
	if (atime.dm ^= need_default)
	| (atime.my ^= need_default)
	| (atime.yc ^= need_default) then goto Emultiple_date_spec;
	atime.dm, atime.my, atime.yc = need_today;
	return;

Atomorrow:	entry;
	if (atime.dm ^= need_default)
	| (atime.my ^= need_default)
	| (atime.yc ^= need_default) then goto Emultiple_date_spec;
	atime.dm, atime.my, atime.yc = need_tomorrow;
	return;

Ayesterday:	entry;
	if (atime.dm ^= need_default)
	| (atime.my ^= need_default)
	| (atime.yc ^= need_default) then goto Emultiple_date_spec;
	atime.dm, atime.my, atime.yc = need_yesterday;
	return;


Ayear_and_LEX:	entry;

	if atime.yc ^= need_default then goto Emultiple_date_spec;
	if this_()
	then do;
	   call LEX (1);
	   atime.yc = need_this;
	end;
	else do;
	   atime.yc = token.Nvalue;
	   if (length (token_value) < 3)
	   then do;				/* handle century default */
	        atime.yc = CONVERT_TO_4_DIGIT_YEAR (atime.yc);
	   end;
	end;
	call LEX (1);
	return;


Ayear_default:	entry;

	if atime.yc ^= need_default then goto Emultiple_date_spec;
	atime.yc = need_year;
	return;


Azone:	entry;

	if (atime.za ^= "") & (atime.za ^= "#")
	then goto Emultiple_zone_spec;
	atime.za = token_value;
	atime.zone_index = token.Nvalue;
	return;


Azone_dif:	entry;

	if (atime.za ^= "") & (atime.za ^= "#")
	then goto Emultiple_zone_spec;
	atime.za = SIGN || token_value;
	atime.zone_index = 0;
	return;


Uday_of_week:	entry;

	if u_day_of_week ^= 0 then goto Emultiple_diw_spec;
	u_day_of_week = token.Nvalue;
	return;

apply_sign_and_offset:
	entry;

	if token_value = "+" then offset_sign = 1;	/* apply sign to offset value			*/
	else offset_sign = -1;
	Ptoken = token.Pnext;			/* skip over sign token.			*/
	goto join_offset;


apply_offset:
	entry;

	offset_sign = +1;				/* assume positive sign for offset.		*/
join_offset:
	number = token.Nvalue;			/* get magnitude of offset.			*/
	if (number = 0)
	then return;
	if (number < 0)
	then do;
	   fld59 = get_number();
	   number = 0;
	end;
	else fld59 = 0;
	goto offset_common;

Afraction_sign_and_offset:
	entry;

	if token_value = "+" then offset_sign = 1;	/* apply sign to offset value			*/
	else offset_sign = -1;
	Ptoken = token.Pnext;			/* skip over sign token.			*/
	goto join_fraction;


Afraction_offset:
	entry;

	offset_sign = +1;				/* assume positive sign for offset.		*/
join_fraction:
	if (token.Itoken_in_stmt ^= TTfraction)		/* the <N> part of the form is optional		*/
	then do;
	   number = token.Nvalue;			/* get magnitude of offset.			*/
	   if (number < 0)
	   then do;
	      number = 0;
	      fld59 = get_number();
	   end;
	   else fld59 = 0;
	   Ptoken = token.Pnext;			/* move to fraction.			*/
	end;
	else number, fld59 = 0;
          fld59 = fld59 + get_fraction();
          if (fld59 = 0)
	then do;
	   if (number = 0)
	   then return;				/* forget he even mentioned it		*/
	end;
offset_common:
	Ptoken = token.Pnext;			/* move to offset				*/

	call init_rtime;
	rtime_array.flag (token.Nvalue) = USED;
	rtime_array.val (token.Nvalue)
	   = rtime_array.val (token.Nvalue)
	   + (convert (fld59, number) + fld59) * offset_sign;
	return;

Emultiple_time_spec:
	lcode = error_table_$dt_multiple_time_spec;
	goto set_err_loc;
Emultiple_date_spec:
	lcode = error_table_$dt_multiple_date_spec;
	goto set_err_loc;
Emultiple_zone_spec:
	lcode = error_table_$dt_multiple_zone_spec;
	goto set_err_loc;
Emultiple_diw_spec:
	lcode = error_table_$dt_multiple_diw_spec;
	goto set_err_loc;
Ehr_gt_12:
	lcode = error_table_$dt_hour_gt_twelve;
	goto set_err_loc;
set_err_loc:
	err_pt = token.Pvalue;
	signal condition (semantic_error);

	end Aday_and_LEX;%skip(3);
init_rtime: proc;

      if rtime_first
      then do;
         rtime_first = ""b;
         if (rtime_ct = hbound (rspace, 1))
         then goto Etoo_many_adverbs;
         rtime_ct = rtime_ct + 1;
         rtime_p = addr (rspace (rtime_ct).data);
         rtime_array.version = Vtime_offset_2;
      end;
      rspace.rel_ct (rtime_ct) = rspace.rel_ct (rtime_ct) + 1;

   end init_rtime;%page;
/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */
get_fraction:
get_number: proc returns (float dec (59));

dcl number	char (token.Lvalue) based (token.Pvalue);

      return (convert (fld59, number));

   end get_number;%skip(3);
token_:	procedure (l, type, value);			/* procedure to fill in the next token of the	*/
						/*    token chain.				*/

dcl l		fixed bin,	/* length of the new token.	       */
    type		fixed bin,	/* type of the new token.	       */
				/*    0 = unknown		       */
				/*    1 = day name		       */
				/*      2=language name (unused)     */
				/*    2 = meridian (# borrowed)      */
				/*    3 = month name	       */
				/*    4 = offset		       */
				/*    5 = word		       */
				/*        1 = before	       */
				/*        2 = or		       */
				/*        3 = after		       */
				/*        4 = on		       */
				/*        5 = noon		       */
				/*        6 = midnight	       */
				/*        7 = now		       */
				/*        8 = yesterday	       */
				/*        9 = today		       */
				/*       10 = tomorrow	       */
				/*    6 = zone		       */
				/*    7 = "this"		       */
				/*    8 = number 		       */
				/*    9 = big number	       */
				/*   10 = fraction		       */
				/*   11 = and_fraction	       */
				/*   12 = other		       */
    value		fixed bin(35);	/* numeric value of the token.       */

	if Ntokens = dim(tokens,1) then goto Etoo_many_tokens;
					   	/* if there aren't enough tokens, then there are	*/
						/*    more tokens in input string than can be	*/
	Ntokens = Ntokens + 1;			/*    legal.				*/
	if Ntokens = 1 then do;			/* special case assignment of the first token.	*/
	     Ptoken = addr(tokens(Ntokens));
	     token.Plast = null;
	     end;
	else do;
	     token.Pnext = addr(tokens(Ntokens));
	     token.Pnext -> token.Plast = Ptoken;
	     Ptoken = token.Pnext;
	     end;
	token.Pnext = null;
	token.Pvalue = Pstr;
	token.Lvalue = l;
/****	The comment on token.Itoken_in_stmt is			       */
/****		"position of token within its statement."	       */
/****	But we have no statements, so we are using it for token type       */
	token.Itoken_in_stmt = type;
	token.Nvalue = value;
	token.Pstmt, token.Psemant = null;
	string (token.S) = ""b;
	Pstr = addcharno (Pstr, l);			/* skip to next token of input string.		*/
	Lstr = Lstr - l;
       end token_;%page;
%include time_value;
%include time_offset;
%include time_info_search;
%include time_names;
%include time_defaults_;
dcl com_err_	entry() options(variable);

%skip;
/**** Implement the cutoff for 2 digit year strings.

      00..29  =  2000..2029
      30..99  =  1930..1999
*/
CONVERT_TO_4_DIGIT_YEAR:
	procedure (p_yy)
	returns (fixed binary);

dcl p_yy  fixed binary parameter;

dcl yyyy  fixed binary;

	if p_yy < 30 then
	     yyyy = 2000;
	else yyyy = 1900;
	yyyy = yyyy + p_yy;

	return (yyyy);

end CONVERT_TO_4_DIGIT_YEAR;
                