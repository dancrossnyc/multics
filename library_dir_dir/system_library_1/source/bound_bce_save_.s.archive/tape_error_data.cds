/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* format: style4,declareind10,dclind10,idind20,indattr,delnl,insnl,ifthenstmt */

tape_error_data:
     proc;

/* HISTORY COMMENTS:
  1) change(86-09-08,Farley), approve(86-07-18,MCR7439),
     audit(86-09-24,Fawcett), install(86-10-20,MR12.0-1189):
     Data Segment used for interpreting tape error status.  Code was copied
     from disk_error_data.cds.
  2) change(87-07-07,Farley), approve(87-07-17,MCR7733),
     audit(87-07-31,Fawcett), install(87-07-31,MR12.1-1051):
     Added "backspace" flag to 13/10 (ID Burst write errors) status.
                                                   END HISTORY COMMENTS */

dcl	1 cdsa		like cds_args aligned auto;
dcl	xnames		(1) char (32) aligned auto init ("*");

dcl	1 data		based (tempp (1)) aligned,
	  2 maj_array	(0:19) like tape_error_data,
	  2 sub_array	(nsub) like tape_error_interp;

dcl	nsub		fixed bin,		/* number of substatuses */
	charwds		fixed bin,		/* number of words of characters allocated */
	tempp		(2) ptr,			/* temp segs pointers */
	i		fixed bin,		/* iteration variable */
	rcode		fixed bin (35),		/* error code */
	nulldescriprel	bit (18) aligned,		/* rel ptr to null char string */
	teirel		bit (18) aligned,		/* rel pointer to interpretation data */
	tsdrel		bit (18) aligned;		/* rel pointer to charactr string */

dcl	get_temp_segments_	entry (char (*), (*) ptr, fixed bin (35)),
	release_temp_segments_
			entry (char (*), (*) ptr, fixed bin (35)),
	create_data_segment_
			entry (ptr, fixed bin (35)),
	com_err_		entry options (variable);

dcl	copy_chars	(charwds) fixed bin (35) based;
						/* structure for copying characters */

dcl	(
	NRETRIES		init (8),
	NONE		init (0),
	ONCE		init (1)
	)		fixed bin (5) static options (constant);

dcl	(addr, addrel, bin, bit, divide, hbound, index, length, null, rel, size, translate, string, unspec)
			builtin;

dcl	cleanup		condition;
%page;
%include tape_error_interp;
%page;
%include cds_args;
%page;
	tempp (*) = null ();

	on cleanup call clean_up;

	call get_temp_segments_ ("tape_error_data", tempp, rcode);
	if rcode ^= 0 then do;
	     call com_err_ (rcode, "tape_error_data", "get_temp_segments_");
	     goto nl_exit;
	end;

	tedp = tempp (1);
	tsdp = tempp (2);

	nsub = 1;
	taperp = addr (data.sub_array (1));
	charwds = 0;

	nulldescriprel = allocate_tsd ("");
	teirel = compute_rel (tedp, taperp);
	tsdrel = allocate_tsd ("error");
	do i = 0 to hbound (data.maj_array, 1);
	     data.maj_array (i).interp = teirel;
	     data.maj_array (i).namep = tsdrel;
	end;
	call set_substat ("XXXXXX", NONE, "", "");
%page;
	call set_majstat (0, "Device Ready");
	call set_substat ("000000", NONE, "Ready", "");
	call set_substat ("XX0XX1", NONE, "Write protected", "");
	call set_substat ("000X1X", NONE, "Positioned at BOT", "");
	call set_substat ("XXX1XX", NONE, "Nine track handler", "");
	call set_substat ("010X0X", NONE, "Two bit fill", "");
	call set_substat ("100X0X", NONE, "Four bit fill", "");
	call set_substat ("110X0X", NONE, "Six bit fill", "");
	call set_substat ("001100", NONE, "ASCII alert", "");

	call set_majstat (1, "Device Busy");
	call set_substat ("000001", NONE, "Tape rewinding", "special");
	call set_substat ("000010", NRETRIES, "Alternate channel in control", "");
	call set_substat ("000100", NONE, "Device loading", "special");
	call set_substat ("100000", NONE, "Device reserved", "bad_dev");

	call set_majstat (2, "Device Attention");
	call set_substat ("00XX01", NONE, "Write protected", "");
	call set_substat ("000010", NONE, "No such tape handler", "bad_dev");
	call set_substat ("0XX10X", NONE, "Handler in standby", "bad_dev");
	call set_substat ("0X1X0X", NONE, "Handler check", "bad_dev,detail");
	call set_substat ("01XX00", NONE, "Blank tape on write", "bad_dev,detail");

	call set_majstat (3, "Device Data Alert");
	call set_substat ("000001", NRETRIES, "Transfer timing alert", "backspace");
	call set_substat ("000010", NONE, "Blank tape on read", "eot");
	call set_substat ("XXXX11", NONE, "Bit detected during erase", "bad_dev,detail");
	call set_substat ("XXX1XX", NRETRIES, "Transmission parity alert", "backspace");
	call set_substat ("XX1XXX", NRETRIES, "Lateral parity alert", "backspace");
	call set_substat ("X1XXXX", NRETRIES, "Longitudinal parity alert", "backspace");
	call set_substat ("1XXXXX", NONE, "End of tape mark detected", "eot");

	call set_majstat (4, "End of File");
	call set_substat ("001111", NONE, "7 track EOF", "eof");
	call set_substat ("010011", NONE, "9 track EOF", "eof");
	call set_substat ("111111", NONE, "Data alert condition", "");

	call set_majstat (5, "Command Reject");
	call set_substat ("000000", NONE, "Invalid density", "bad_density");
	call set_substat ("000XX1", NONE, "Invalid operation code", "");
	call set_substat ("000X1X", NONE, "Invalid device code", "");
	call set_substat ("0001XX", NONE, "Invalid IDCW parity", "");
	call set_substat ("001000", NONE, "Positioned at BOT", "");
	call set_substat ("010000", NONE, "Forward read after write", "");
	call set_substat ("100000", NONE, "Nine track error", "");

	call set_majstat (10, "Controller Attention");
	call set_substat ("000001", NONE, "Configuration switch error", "bad_path");
	call set_substat ("000010", NONE, "Multiple devices", "");
	call set_substat ("000011", NONE, "Illegal device number", "");
	call set_substat ("001000", NONE, "Incompatible mode", "bad_density");
	call set_substat ("001100", NONE, "TCA malfunction (0)", "bad_path,detail");
	call set_substat ("001101", NONE, "TCA malfunction (1)", "bad_path,detail");
	call set_substat ("010000", NONE, "MTH malfunction", "bad_dev,detail");
	call set_substat ("010001", NONE, "Multiple BOT", "bad_dev");

	call set_majstat (11, "Controller Data Alert");
	call set_substat ("000001", NRETRIES, "Transmission parity alert", "bad_path,backspace");
	call set_substat ("000010", NONE, "Inconsistent command", "");
	call set_substat ("000011", NRETRIES, "Sum check error", "bad_path,detail");
	call set_substat ("000100", NRETRIES, "Byte locked out", "bad_path");
	call set_substat ("001000", NRETRIES, "ID Burst write error", "backspace,detail");
	call set_substat ("001001", NRETRIES, "Preamble error", "backspace");
	call set_substat ("001010", NRETRIES, "T&D error", "");
	call set_substat ("010000", NRETRIES, "Multi-track error", "backspace");
	call set_substat ("010001", NRETRIES, "Skew error", "backspace");
	call set_substat ("010010", NRETRIES, "Postamble error", "backspace");
	call set_substat ("010011", NRETRIES, "NRZI CCC error", "backspace");
	call set_substat ("010100", NRETRIES, "Code alert", "backspace");
	call set_substat ("100000", NRETRIES, "Marginal condition", "backspace");

	call set_majstat (13, "Controller Command Reject");
	call set_substat ("000000", NONE, "Indeterminate Density", "bad_density");
	call set_substat ("000001", NONE, "Illegal procedure", "");
	call set_substat ("000010", NONE, "Illegal logical channel", "");
	call set_substat ("000011", NONE, "Illegal suspended logical chnl", "special");
	call set_substat ("000100", NONE, "Continue bit not set", "");

	call set_majstat (POWER_OFF, "power off");
	call set_substat ("XXXXXX", NRETRIES, "", "bad_path");

	call set_majstat (CHAN_STAT, "chan stat");
	call set_substat ("001000", ONCE, "connect while busy", "");
	call set_substat ("010000", ONCE, "illeg chan instruct", "");
	call set_substat ("011000", ONCE, "incorrect DCW", "");
	call set_substat ("100000", ONCE, "incomplete instruct seq", "bad_path");
	call set_substat ("110000", NRETRIES, "PSI parity err", "bad_path");
	call set_substat ("111000", NRETRIES, "parity err, I/O bus to chan", "bad_path");
	call set_substat ("XXXXXX", NRETRIES, "", "bad_path");

	call set_majstat (CENTRAL_STAT, "central stat");
	call set_substat ("001000", ONCE, "LPW tally runout", "bad_path");
	call set_substat ("010000", ONCE, "two TDCW's in a row", "bad_path");
	call set_substat ("011000", ONCE, "boundary error", "bad_path");
	call set_substat ("100000", ONCE, "A.E. change in restricted mode", "bad_path");
	call set_substat ("101000", ONCE, "IDCW in restricted mode", "bad_path");
	call set_substat ("110000", ONCE, "char pos/size discrepancy", "bad_path");
	call set_substat ("111000", ONCE, "parity err, data from chan", "bad_path");
	call set_substat ("XXXXXX", NRETRIES, "", "bad_path");

	call set_majstat (SYS_FAULT, "system fault");
	call set_substat ("XXXXXX", ONCE, "", "bad_path");
%page;
	taperp -> copy_chars = tempp (2) -> copy_chars;	/* copy the characters now. */

	nsub = nsub - 1;				/* back down one */

	do i = 0 to hbound (data.maj_array, 1);
	     call relocate_rel (data.maj_array (i).namep);
	end;

	do i = 1 to nsub;
	     call relocate_rel (data.sub_array (i).namep);
	end;

	cdsa.sections (1).p = tempp (1);
	cdsa.sections (1).len = size (data) + charwds;
	cdsa.sections (1).struct_name = "data";

	cdsa.seg_name = "tape_error_data";
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (xnames);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), rcode);
	if rcode ^= 0 then call com_err_ (rcode, "tape_error_data", "create_data_segment_");

nl_exit:
	call clean_up;
	return;
%page;
clean_up:
     proc;
	if tempp (1) ^= null () then do;
	     call release_temp_segments_ ("tape_error_data", tempp, rcode);
	     if rcode ^= 0 then call com_err_ (rcode, "tape_error_data", "release_temp_segments_");
	end;
	return;
     end clean_up;
%page;
set_majstat:
     proc (mjs, descrip);				/* proc to fill in major status array */

dcl	mjs		fixed bin,		/* major status */
	descrip		char (*) aligned;		/* major status description */

	if mjs > hbound (data.maj_array, 1) then do;
	     call com_err_ (0, "tape_error_data", "The bounds of the major status array have been exceeded.");
	     call com_err_ (0, "tape_error_data", "Current value = ^d, must be at least ^d.",
		hbound (data.maj_array, 1), mjs);
	     go to nl_exit;
	end;
	data.maj_array (mjs).interp = compute_rel (tedp, taperp);
	data.maj_array (mjs).namep = allocate_tsd (descrip);

	return;
     end set_majstat;
%skip (4);
set_substat:
     proc (stat, retry, descrip, errs);			/* proc to allocate tape_error_interp structure */

dcl	stat		char (6) aligned,
	retry		fixed bin (5),
	descrip		char (*) aligned,
	errs		char (*) aligned;

	unspec (tape_error_interp) = "0"b;

	tape_error_interp.bitson = bit (translate (stat, "0", "X"), 6);
	tape_error_interp.bitmask = bit (translate (stat, "10", "0X"), 6);
	tape_error_interp.max_retries = retry;
	if descrip ^= ""
	then tape_error_interp.namep = allocate_tsd (descrip);
	else tape_error_interp.namep = nulldescriprel;

	call seterr (errs, "backspace", tape_error_interp.backspace);
	call seterr (errs, "bad_density", tape_error_interp.bad_density);
	call seterr (errs, "bad_dev", tape_error_interp.bad_dev);
	call seterr (errs, "bad_path", tape_error_interp.bad_path);
	call seterr (errs, "detail", tape_error_interp.get_detail);
	call seterr (errs, "eof", tape_error_interp.end_of_file);
	call seterr (errs, "eot", tape_error_interp.end_of_tape);
	call seterr (errs, "special", tape_error_interp.expect_special);

	nsub = nsub + 1;				/* Count one more. */
	taperp = addr (data.sub_array (nsub));

	return;
     end set_substat;
%page;
seterr:
     proc (errstring, errname, errbit);			/* procedure to set error bit */

dcl	errstring		char (*) aligned,
	errname		char (*) aligned,
	errbit		bit (1) unal;

	if index (errstring, errname) > 0 then errbit = "1"b;

	return;
     end seterr;
%page;
allocate_tsd:
     proc (descrip) returns (bit (18) aligned);		/* procedure to allocate character string */

dcl	descrip		char (*) aligned;

dcl	old_tsdp		ptr,
	chlth		fixed bin;

	tape_status_descrip.lth = length (descrip);
	tape_status_descrip.chr = descrip;

	chlth = divide (length (descrip), 4, 17, 0) + 1;
	charwds = charwds + chlth;
	old_tsdp = tsdp;
	tsdp = addrel (tsdp, chlth);

	return (compute_rel (tempp (2), old_tsdp));
     end allocate_tsd;
%skip (4);
compute_rel:
     proc (basep, strucp) returns (bit (18) aligned);	/* procedure to compute relative offset */

dcl	basep		ptr,			/* pointer to database */
	strucp		ptr;			/* pointer to structure */

	return (bit (bin (bin (rel (strucp), 18) - bin (rel (basep), 18), 18), 18));
     end compute_rel;
%skip (4);
relocate_rel:
     proc (relp);					/* proc to relocate name pointers */

dcl	relp		bit (18) unal;		/* name relative pointer */

	relp = bit (bin (bin (relp, 18) + bin (rel (taperp), 18), 18), 18);

	return;
     end relocate_rel;
     end tape_error_data;
