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
  1) change(87-05-27,Fawcett), approve(87-05-27,MCR7704),
     audit(87-07-08,Farley), install(87-07-17,MR12.1-1043):
     Change the wording for the major status number 19. This is not a real
     major status but rather the entry uesd for I/O system faults. The old
     wording was just system fault.
  2) change(88-02-23,Farley), approve(88-02-23,MCR7793),
     audit(88-02-24,Fawcett), install(88-03-01,MR12.2-1029):
     Removed "rsr" from the "power off" major status. If the path is bad
     attempting to do a RSR will only complicate things..
  3) change(89-06-23,Farley), approve(89-07-26,MCR8122),
     audit(89-09-11,WAAnderson), install(89-09-22,MR12.3-1072):
     Added functionality to seperate some of the FIPS status interpretations,
     while still keeping the space required to a minimum.
  4) change(90-06-27,WAAnderson), approve(90-08-28,MCR8188),
     audit(90-09-21,Schroth), install(90-10-01,MR12.4-1035):
     Added substat 'count field uncorrectable'.
                                                   END HISTORY COMMENTS */


/* DISK_ERROR_DATA - This is the Database for Interpreting Disk Error Status.
	created 5/19/76 by Noel I. Morris	

   Last Modified:

   November 1982, J. Bongiovanni, to fix bug in dev busy, alt channel in control
*/


disk_error_data: proc;

dcl 1 cdsa like cds_args aligned auto;
dcl  xnames (1) char (32) aligned auto init ("*");

dcl 1 data based (tempp (1)) aligned,
    2 maj_array (0: 23) like disk_error_data,
    2 sub_array (nsub) like disk_error_interp;

dcl  nsub fixed bin,				/* number of substatuses */
     charwds fixed bin,				/* number of words of characters allocated */
     tempp (2) ptr,					/* temp segs pointers */
     i fixed bin,					/* iteration variable */
     rcode fixed bin (35),				/* error code */
     nulldescriprel bit (18) aligned,			/* rel ptr to null char string */
     deirel bit (18) aligned,				/* rel pointer to interpretation data */
     dsdrel bit (18) aligned;				/* rel pointer to charactr string */

dcl  get_temp_segments_ entry (char (*), (*) ptr, fixed bin (35)),
     release_temp_segments_ entry (char (*), (*) ptr, fixed bin (35)),
     create_data_segment_ entry (ptr, fixed bin (35)),
     com_err_ entry options (variable);

dcl  copy_chars (charwds) fixed bin (35) based;		/* structure for copying characters */

dcl (NRETRIES init (5),
     NONE init (0),
     ONCE init (1)) fixed bin (5) static options (constant);

dcl (addr, addrel, bin, bit, divide, hbound, index, length, rel, size, translate, string, unspec) builtin;



% include disk_error_interp;



% include cds_args;



	call get_temp_segments_ ("disk_error_data", tempp, rcode);
	if rcode ^= 0 then
	     call com_err_ (rcode, "disk_error_data", "get_temp_segments_");

	dedp = tempp (1);
	dsdp = tempp (2);

	nsub = 1;
	dskerp = addr (data.sub_array (1));
	charwds = 0;

	nulldescriprel = allocate_dsd ("");
	deirel = compute_rel (dedp, dskerp);
	dsdrel = allocate_dsd ("error");
	do i = 0 to hbound (data.maj_array, 1);
	     data.maj_array (i).interp = deirel;
	     data.maj_array (i).namep = dsdrel;
	     data.maj_array (i).finterp = deirel;
	     data.maj_array (i).fnamep = dsdrel;
	end;
	call set_substat ("XXXXXX", NONE, "", "");



	call set_majstat (1, "dev busy");
	call set_fmajstat (1, "dev busy");
	call set_substat ("000000", NRETRIES, "dev positioning",                "reseek");
	call set_substat ("100000", NRETRIES, "alt chan in control",            "bad_dev,reseek,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "reseek,rsr");

	call set_majstat (2, "dev attention");
	call set_fmajstat (2, "dev attention");
	call set_substat ("0000X1",     NONE, "write inhib",                    "bad_dev");
	call set_substat ("00001X", NRETRIES, "seek incomplete",                "bad_dev,reseek,rsr");
	call set_substat ("001000",     NONE, "dev inop",                       "bad_dev,reseek,rsr");
	call set_substat ("010000",     NONE, "dev in standby",                 "bad_dev,rsr");
	call set_substat ("100000",     NONE, "dev offline",                    "bad_dev");
	call set_substat ("XXXXXX",     ONCE, "",                               "reseek,rsr");

	call set_majstat (3, "dev data alert");
	call set_fmajstat (3, "dev data alert");
	call set_substat ("000001", NRETRIES, "xfer timing alert",              "bad_path");
	call set_substat ("000010", NRETRIES, "xmission parity alert",          "bad_path");
	call set_substat ("000100", NRETRIES, "invalid seek addr",              "bad_addr,reseek,rsr");
	call set_substat ("0X1000", NRETRIES, "hdr ver failure",                "bad_addr,reseek,rsr");
	call set_substat ("X1X000", NRETRIES, "check char alert",               "bad_addr,reseek,rsr");
	call set_substat ("1X0000", NRETRIES, "compare alert",                  "bad_addr,reseek,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "reseek,rsr");

	call set_majstat (4, "end of file");
	call set_fmajstat (4, "end of file");
	call set_substat ("000000",     ONCE, "good track",                     "bad_addr,reseek,rsr");
	call set_substat ("0000X1",     ONCE, "last consec block",              "bad_addr,reseek,rsr");
	call set_substat ("00001X",     ONCE, "sect limit exceeded",            "bad_path,reseek,rsr");
	call set_substat ("000100",     ONCE, "defect trk, alt assnd",          "bad_addr,reseek,rsr");
	call set_substat ("001000",     ONCE, "defect trk, no alt",             "bad_addr,reseek,rsr");
	call set_substat ("010000",     ONCE, "alt trk detected",               "bad_addr,reseek,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "reseek,rsr");

	call set_majstat (5, "cmd reject");
	call set_substat ("000XX1",     ONCE, "invalid op code",                "bad_path,rsr");
	call set_substat ("000010",     ONCE, "invalid dev code",               "bad_path,rsr");
	call set_substat ("000100", NRETRIES, "invalid IDCW parity",            "bad_path,rsr");
	call set_substat ("001000",     ONCE, "invalid instruction seq",        "bad_path,reseek,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "reseek,rsr");

	call set_fmajstat (5, "cmd reject");
	call set_substat ("000XX1",     ONCE, "invalid op code",                "bad_path,rsr");
	call set_substat ("000010",     ONCE, "invalid dev code",               "bad_path,rsr");
	call set_substat ("001000",     ONCE, "invalid instruction seq",        "bad_path,reseek,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "reseek,rsr");

	call set_majstat (8, "channel busy");
	call set_fmajstat (8, "channel busy");
	call set_substat ("XXXXXX",     ONCE, "",                               "bad_path,rsr");

	call set_majstat (10, "MPC attention");
	call set_substat ("000001",     NONE, "config switch err",              "bad_path,rsr");
	call set_substat ("000010",     NONE, "multiple devs",                  "bad_path,rsr");
	call set_substat ("000011",     NONE, "illeg dev no",                   "bad_path,rsr");
	call set_substat ("001011",     NONE, "CA err or OPI down",             "bad_path,rsr");
	call set_substat ("001100",     ONCE, "unexpected EN1",                 "bad_dev,reseek,rsr");
	call set_substat ("001101",     ONCE, "CA EN1 err",                     "bad_dev,reseek,rsr");
	call set_substat ("001110",     ONCE, "no EN1",                         "bad_dev,reseek,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "bad_path,rsr");

	call set_fmajstat (10, "IPC-FIPS device attention");
	call set_substat ("000010",     NONE, "multiple devs",                  "bad_path,rsr");
	call set_substat ("000011",     NONE, "illeg dev no",                   "bad_path,rsr");
	call set_substat ("001011",     ONCE, "usage/error stat overflow",      "just_log,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "bad_path,rsr");

	call set_majstat (11, "MPC data alert");
	call set_substat ("000001", NRETRIES, "xmission parity alert",          "bad_path,rsr");
	call set_substat ("000010",     ONCE, "inconsistent command",           "bad_path,rsr");
	call set_substat ("000011",     ONCE, "sum check err",                  "bad_path,rsr");
	call set_substat ("000100",     ONCE, "byte locked out",                "bad_path,rsr");
          call set_substat ("001010", NRETRIES, "count field uncorrectable",      "bad_addr,rsr");
	call set_substat ("001110", NRETRIES, "EDAC parity err",                "bad_addr,rsr");
	call set_substat ("010001", NRETRIES, "sect size err",                  "bad_addr,rsr");
	call set_substat ("010010", NRETRIES, "nonstandard sect size",          "bad_addr,rsr");
	call set_substat ("010011", NRETRIES, "search alert (first)",           "bad_addr,rsr");
	call set_substat ("010100", NRETRIES, "cyclic code err",                "bad_addr,rsr");
	call set_substat ("010101", NRETRIES, "search err (not first)",         "bad_addr,rsr");
	call set_substat ("010110", NRETRIES, "sync byte not HEX 19",           "bad_addr,rsr");
	call set_substat ("010111", NRETRIES, "auto alt trk err",               "bad_addr,rsr");
	call set_substat ("011001", NRETRIES, "EDAC, last sect",                "bad_addr,rsr");
	call set_substat ("011010", NRETRIES, "EDAC, not last sect",            "bad_addr,rsr");
	call set_substat ("011011", NRETRIES, "EDAC, block count limit",        "bad_addr,rsr");
	call set_substat ("011100", NRETRIES, "uncorrectable err",              "bad_addr,rsr");
	call set_substat ("011101", NRETRIES, "EDAC, short block",              "bad_addr,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "bad_path,rsr");

	call set_fmajstat (11, "IPC-FIPS device data alert");
	call set_substat ("010001", NRETRIES, "sect size err",                  "bad_addr,rsr");
	call set_substat ("010010", NRETRIES, "nonstandard sect size",          "bad_addr,rsr");
	call set_substat ("010011", NRETRIES, "search alert (first)",           "bad_addr,rsr");
	call set_substat ("010100", NRETRIES, "cyclic code err",                "bad_addr,rsr");
	call set_substat ("010101", NRETRIES, "search err (not first)",         "bad_addr,rsr");
	call set_substat ("010111", NRETRIES, "auto alt trk err",               "bad_addr,rsr");
	call set_substat ("100001", NRETRIES, "write buffer parity err",        "bad_path,rsr");
	call set_substat ("100010", NRETRIES, "uncorrectable read substatus",   "bad_path,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "bad_path,rsr");

	call set_majstat (13, "MPC cmd reject");
	call set_substat ("000001",     NONE, "illeg procedure",                "bad_path,rsr");
	call set_substat ("000010",     NONE, "illeg log chan",                 "bad_path,rsr");
	call set_substat ("000011",     NONE, "illeg susp log chan",            "bad_path,rsr");
	call set_substat ("000100",     NONE, "continue bit not set",           "bad_path,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "bad_path,rsr");

	call set_fmajstat (13, "IPC-FIPS cmd reject");
	call set_substat ("000001",     NONE, "invalid operation code",         "bad_path,rsr");
	call set_substat ("XXXXXX",     ONCE, "",                               "bad_path,rsr");

	call set_majstat (16, "power off");
	call set_fmajstat (16, "power off");
	call set_substat ("XXXXXX", NRETRIES, "",                               "bad_path");

	call set_majstat (17, "chan stat");
	call set_fmajstat (17, "chan stat");
	call set_substat ("001000",     ONCE, "connect while busy",             "");
	call set_substat ("010000",     ONCE, "illeg chan instruct",            "");
	call set_substat ("011000",     ONCE, "incorrect DCW",                  "");
	call set_substat ("100000",     ONCE, "incomplete instruct seq",        "bad_path");
	call set_substat ("110000", NRETRIES, "PSI parity err",                 "bad_path");
	call set_substat ("111000", NRETRIES, "parity err, I/O bus to chan",    "bad_path");
	call set_substat ("XXXXXX", NRETRIES, "",                               "bad_path,rsr");

	call set_majstat (18, "central stat");
	call set_fmajstat (18, "central stat");
	call set_substat ("111000",     ONCE, "parity err, I/O bus from chan",  "bad_path");
	call set_substat ("XXXXXX", NRETRIES, "",                               "bad_path,rsr");

	call set_majstat (19, "I/O system fault");
	call set_fmajstat (19, "I/O system fault");
	call set_substat ("XXXXXX",     ONCE, "",			  "bad_path");

	call set_majstat (20, "nonzero tally residue");
	call set_fmajstat (20, "nonzero tally residue");
	call set_substat ("XXXXXX",     ONCE, "",			  "bad_dev");

	call set_majstat (21, "Auto retries");
	call set_fmajstat (21, "Auto retries");
	call set_substat ("XXXXXX",     ONCE, "",			  "just_log");

	call set_majstat (22, "EDAC performed");
	call set_fmajstat (22, "EDAC performed");
	call set_substat ("XXXXXX",     ONCE, "",			  "just_log");

	call set_majstat (23, "Data parity");
	call set_fmajstat (23, "Data parity");
	call set_substat ("XXXXXX",     ONCE, "",			  "bad_mem");



	dskerp -> copy_chars = tempp (2) -> copy_chars;	/* copy the characters now. */

	nsub = nsub - 1;				/* back down one */

	do i = 0 to hbound (data.maj_array, 1);
	     call relocate_rel (data.maj_array (i).namep);
	     call relocate_rel (data.maj_array (i).fnamep);
	end;

	do i = 1 to nsub;
	     call relocate_rel (data.sub_array (i).namep);
	end;


	cdsa.sections (1).p = tempp (1);
	cdsa.sections (1).len = size (data) + charwds;
	cdsa.sections (1).struct_name = "data";

	cdsa.seg_name = "disk_error_data";
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (xnames);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), rcode);
	if rcode ^= 0 then
	     call com_err_ (rcode, "disk_error_data", "create_data_segment_");

nl_exit:
	call release_temp_segments_ ("disk_error_data", tempp, rcode);
	if rcode ^= 0 then
	     call com_err_ (rcode, "disk_error_data", "release_temp_segments_");

	return;




set_majstat: proc (mjs, descrip);			/* proc to fill in major status array */

dcl  mjs fixed bin (5),				/* major status */
     descrip char (*) aligned;			/* major status description */


	if mjs > hbound (data.maj_array, 1) then do;
	     call com_err_ (0, "disk_error_data", "The bounds of the major status array have been exceeded.");
	     call com_err_ (0, "disk_error_data", "Current value = ^d, must be at least ^d.",
		hbound (data.maj_array, 1), mjs);
	     go to nl_exit;
	end;
	data.maj_array (mjs).interp = compute_rel (dedp, dskerp);
	data.maj_array (mjs).namep = allocate_dsd (descrip);

	return;


     end set_majstat;

set_fmajstat: proc (mjs, descrip);			/* proc to fill in FIPS major status array items */

dcl  mjs fixed bin (5),				/* major status */
     descrip char (*) aligned;			/* major status description */


	if mjs > hbound (data.maj_array, 1) then do;
	     call com_err_ (0, "disk_error_data", "The bounds of the major status array have been exceeded.");
	     call com_err_ (0, "disk_error_data", "Current value = ^d, must be at least ^d.",
		hbound (data.maj_array, 1), mjs);
	     go to nl_exit;
	end;
	data.maj_array (mjs).finterp = compute_rel (dedp, dskerp);
	data.maj_array (mjs).fnamep = allocate_dsd (descrip);

	return;


     end set_fmajstat;



set_substat: proc (stat, retry, descrip, errs);		/* proc to allocate disk_error_interp structure */

dcl  stat char (6) aligned,
     retry fixed bin (5),
     descrip char (*) aligned,
     errs char (*) aligned;


	unspec (disk_error_interp) = "0"b;

	disk_error_interp.bitson = bit (translate (stat, "0", "X"), 6);
	disk_error_interp.bitmask = bit (translate (stat, "10", "0X"), 6);
	disk_error_interp.max_retries = retry;
	if descrip ^= "" then
	     disk_error_interp.namep = allocate_dsd (descrip);
	else
	     disk_error_interp.namep = nulldescriprel;

	call seterr (errs, "bad_addr", disk_error_interp.bad_addr);
	call seterr (errs, "bad_path", disk_error_interp.bad_path);
	call seterr (errs, "bad_dev", disk_error_interp.bad_dev);
	call seterr (errs, "bad_mem", disk_error_interp.bad_mem);
	call seterr (errs, "just_log", disk_error_interp.just_log);
	call seterr (errs, "reseek", disk_error_interp.reseek);
	call seterr (errs, "rsr", disk_error_interp.rsr);

	nsub = nsub + 1;				/* Count one more. */
	dskerp = addr (data.sub_array (nsub));

	return;


     end set_substat;


seterr: proc (errstring, errname, errbit);		/* procedure to set error bit */

dcl  errstring char (*) aligned,
     errname char (*) aligned,
     errbit bit (1) unal;


	if index (errstring, errname) > 0 then
	     errbit = "1"b;

	return;


     end seterr;



allocate_dsd: proc (descrip) returns (bit (18) aligned);	/* procedure to allocate character string */

dcl  descrip char (*) aligned;

dcl  old_dsdp ptr,
     temp_dsdp ptr,
     chlth fixed bin;

/**** Scan existing strings for a match.  Allocate new string only when no match is found. */

	do temp_dsdp = tempp(2)
	     repeat addrel (temp_dsdp, divide (temp_dsdp -> disk_status_descrip.lth, 4, 17, 0) + 1)
	     while (rel (temp_dsdp) < rel (dsdp));
	     if temp_dsdp -> disk_status_descrip.chr = descrip
		then return (compute_rel (tempp(2), temp_dsdp));
	end;

	disk_status_descrip.lth = length (descrip);
	disk_status_descrip.chr = descrip;

	chlth = divide (length (descrip), 4, 17, 0) + 1;
	charwds = charwds + chlth;
	old_dsdp = dsdp;
	dsdp = addrel (dsdp, chlth);

	return (compute_rel (tempp (2), old_dsdp));


     end allocate_dsd;



compute_rel: proc (basep, strucp) returns (bit (18) aligned);  /* procedure to compute relative offset */

dcl  basep ptr,					/* pointer to database */
     strucp ptr;					/* pointer to structure */


	return (bit (bin (bin (rel (strucp), 18) - bin (rel (basep), 18), 18), 18));


     end compute_rel;



relocate_rel: proc (relp);				/* proc to relocate name pointers */

dcl  relp bit (18) unal;				/* name relative pointer */


	relp = bit (bin (bin (relp, 18) + bin (rel (dskerp), 18), 18), 18);

	return;


     end relocate_rel;



     end disk_error_data;
