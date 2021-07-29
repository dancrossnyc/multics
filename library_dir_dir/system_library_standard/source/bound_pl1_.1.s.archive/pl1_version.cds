/* ***********************************************************
   *                                                         *
   * Copyright, (C) BULL HN Information Systems Inc., 1989   *
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */




/* HISTORY COMMENTS:
  1) change(1986-07-15,Ginter), approve(1986-07-15,MCR7287),
     audit(1986-07-16,Mabey), install(1986-07-28,MR12.0-1105):
     Change the version number for the MR12.0 release of the compiler.
  2) change(1988-01-26,RWaters), approve(1988-01-26,MCR7724),
     audit(1988-02-05,Huen), install(1988-02-16,MR12.2-1024):
     Change the version number for the MR12.2 release of the compiler.
  3) change(1988-08-23,RWaters), approve(1988-08-23,MCR7914),
     audit(1988-09-28,Huen), install(1988-10-12,MR12.2-1163):
     Change the version number for the MR12.2 release (Vers. 31a)
  4) change(1989-03-28,Huen), approve(1989-03-28,MCR8077),
     audit(1989-04-26,JRGray), install(1989-06-16,MR12.3-1059):
     Change the version number for the MR12.3 release (Vers. 31b)
  5) change(1989-04-04,Huen), approve(1989-04-04,MCR8092),
     audit(1989-04-26,RWaters), install(1989-06-16,MR12.3-1059):
     Change the version number for the MR12.3 release (Vers. 32a)
  6) change(1989-04-17,JRGray), approve(1989-04-17,MCR8078),
     audit(1989-04-18,Huen), install(1989-06-16,MR12.3-1059):
     Updated version to 32b, part of archive pathname support.
  7) change(1989-04-24,RWaters), approve(1989-04-24,MCR8101),
     audit(1989-04-27,Huen), install(1989-06-16,MR12.3-1059):
     Updated the version number to 32c.
  8) change(1989-07-10,RWaters), approve(1989-07-10,MCR8069),
     audit(1989-09-07,Vu), install(1989-09-19,MR12.3-1068):
     Update Version Number for numerous installed changes.
  9) change(1989-07-28,JRGray), approve(1989-07-28,MCR8123),
     audit(1989-09-12,Vu), install(1989-09-22,MR12.3-1073):
     Updated version to 32e for opt conditional fix  (pl1 2091 fix 2177).
 10) change(1989-10-02,Vu), approve(1989-10-02,MCR8139),
     audit(1989-10-04,Blackmore), install(1989-10-09,MR12.3-1086):
     Updated version to 32f for two named constant changes.
 11) change(1990-05-03,Huen), approve(1990-05-03,MCR8169),
     audit(1990-05-18,Gray), install(1990-05-30,MR12.4-1012):
     Updated version to 33a for pl1 opt concat of a common string exp bug
     (pl1_1885)
 12) change(1990-08-24,Huen), approve(1990-08-24,MCR8187),
     audit(1990-10-03,Zimmerman), install(1990-10-17,MR12.4-1046):
     Updated version to 33b for pl1 padded reference bug (phx13134, pl1_2224)
 13) change(1990-08-30,Huen), approve(1990-08-30,MCR8160),
     audit(1990-10-03,Zimmerman), install(1990-10-17,MR12.4-1046):
     version (33b) is also for fixing PL1 to not complain about constant
     symbols that are actually legal.
 14) change(1990-10-17,Gray), approve(1990-10-17,MCR8160),
     audit(1990-10-19,Schroth), install(1990-10-25,MR12.4-1049):
     Modified to 33c to only validate constants for syms dcled by dcl
     statement.
 15) change(1991-01-09,Blackmore), approve(1991-01-09,MCR8234),
     audit(1991-12-05,Huen), install(1992-04-24,MR12.5-1011):
     Change version to 33d, with constant reference resolution fix.
 16) change(1992-09-17,Zimmerman), approve(1992-09-17,MCR8257),
     audit(1992-09-18,Vu), install(1992-10-06,MR12.5-1023):
     Updated version number to 33e (MR 12.5). Fix source listing
     overflow problem. (PL1 error list entry 2212).
 17) change(2016-12-31,Anthony), approve(2016-12-31,MCR10023),
     audit(2017-01-08,GDixon), install(2017-01-09,MR12.6f-0011):
     Update declaration of decode_clock_value_$date_time to declare time zone
     parameter as char(4) and time_zone automatic variable as char(4).
 18) change(2017-02-09,Swenson), approve(2017-02-09,MCR10029),
     audit(2017-02-11,Davidoff), install(2017-02-11,MR12.6f-0016):
     Fix the previous fix (MCR8169) to string concatenation.  See
     http://multics-trac.swenson.org/ticket/9 for details. Update compiler
     version to 33f.
                                                   END HISTORY COMMENTS */


/* format: style3 */
(stringsize):
pl1_version:
     procedure;

/*     Written: 25 September 1979 by PCK to replace stand alone segment, pl1_version_ */
/*     Modified: 28 January 1988 by RW to fix 1994 and 2186 */

/* external entries */

dcl	create_data_segment_
			entry (ptr, fixed bin (35));
dcl	ioa_		entry options (variable);
dcl	com_err_		entry options (variable);
dcl	decode_clock_value_$date_time
			entry (fixed bin (71), fixed bin, fixed bin, fixed bin, fixed bin, fixed bin, fixed bin,
			fixed bin (71), fixed bin, char (4), fixed bin (35));

/* builtins */

dcl	(addr, clock, ltrim, size, unspec)
			builtin;

/* internal static */

dcl	day_of_week_string	(1:7) character (9) varying int static options (constant)
			init ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday");
dcl	month_string	(1:12) character (9) varying int static options (constant)
			init ("January", "February", "March", "April", "May", "June", "July", "August", "September",
			"October", "November", "December");
dcl	my_name		character (11) int static init ("pl1_version") options (constant);

/* conditions */

dcl	(stringsize, error) condition;

/* automatic */

/* RELEASE = "" for the >experimental_library compiler,
	 = <release_number> for the >system_standard_library compiler */

dcl	RELEASE		character (3) varying init ("33f");
dcl	(clock_reading, microsecond)
			fixed bin (71);
dcl	(month, day_of_month, year, day_of_week, hour, minute, second)
			fixed bin;
dcl	time_zone		character (4) init ("");
dcl	pl1_version_string	character (256) varying;
dcl	code		fixed bin (35);
dcl	1 cdsa		like cds_args aligned;	/* info to be passed to
				  create_data_segment_ */
dcl	1 pl1_version_struc,
	  2 pl1_version	character (256) varying,
	  2 pl1_release	character (3) varying;
dcl	year_pic		picture "9999";
dcl	day_of_month_pic	picture "zz";
dcl	hour_pic		picture "99";
dcl	minute_pic	picture "99";

/* include file */

%include cds_args;

/* on unit */

	on stringsize
	     begin;
		call com_err_ (0, my_name, "Stringsize raised.");
		signal error;
	     end;

/* program */

/* Read system clock and convert to calendar date-time */

	clock_reading = clock ();
	call decode_clock_value_$date_time (clock_reading, month, day_of_month, year, hour, minute, second, microsecond,
	     day_of_week, time_zone, code);

	if code ^= 0
	then do;
		call com_err_ (code, my_name);
		return;
	     end;

	year_pic = year;
	day_of_month_pic = day_of_month;

/* Generate a pl1_version_string appropriate for an EXL or SSS compiler */

	if RELEASE ^= ""
	then pl1_version_string =
		"Multics PL/I Compiler, Release " || RELEASE || ", of " || month_string (month) || " "
		|| ltrim (day_of_month_pic) || ", " || year_pic;
	else do;
		hour_pic = hour;
		minute_pic = minute;
		pl1_version_string =
		     "PL/I Compiler of " || day_of_week_string (day_of_week) || ", "
		     || month_string (month) || " " || ltrim (day_of_month_pic) || ", " || year_pic || " at "
		     || hour_pic || ":" || minute_pic;
	     end;

/* Let user know what version string has been generated */

	call ioa_ ("^a: pl1_version_=""^a"".", my_name, pl1_version_string);

/* Fill in pl1_version_struc with version and release info */

	unspec (pl1_version_struc) = ""b;
	pl1_version_struc.pl1_version = pl1_version_string;
	pl1_version_struc.pl1_release = RELEASE;

/* Fill in cdsa for call to create_data_segment_ */

	unspec (cdsa) = "0"b;
	cdsa.have_text = "1"b;			/* Place pl1_version info in text section */
	cdsa.sections (1).p = addr (pl1_version_struc);
	cdsa.sections (1).len = size (pl1_version_struc);
	cdsa.sections (1).struct_name = "pl1_version_struc";
	cdsa.seg_name = my_name;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then call com_err_ (code, my_name, "Creating ^a data segment.", my_name);

     end pl1_version;
