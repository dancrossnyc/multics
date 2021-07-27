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



/* HISTORY COMMENTS:
  1) change(85-09-09,Farley), approve(85-09-09,MCR6979),
     audit(85-12-02,CLJones), install(86-03-21,MR12.0-1033):
     Support FIPS by
     changing tape 02/04 tp not log detail.
                                                   END HISTORY COMMENTS */

/* IO_LOG_STATUS_INFO - Database for determining what i/O status should be logged */
/* Based, generally, on ELAN PFS */
/* Written December 1979 by Larry Johnson */
/* Modified June 1983 by Rick Kovalcik to add hyperchannel */
/*
   Modifed October 1983 by Paul Farley to add "blank tape on write" to tape
   detail section.
   Modified FEB 1985 by Paul Farley to change tape 02/04 tp not log detail.
*/

io_log_status_info: proc;

/* Automatic */

dcl  cur_major fixed bin;
dcl  code fixed bin (35);
dcl 1 cds like cds_args automatic;

dcl 1 dummy_struct aligned,
    2 io_log_status_info fixed bin init (0);

/* Constants */

dcl  name char (18) int static options (constant) init ("io_log_status_info");

/* External */

dcl  com_err_ entry options (variable);
dcl  get_temp_segment_ entry (char (*), pointer, fixed bin (35));
dcl  release_temp_segment_ entry (char (*), pointer, fixed bin (35));
dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl  cleanup condition;


dcl (addr, bin, bit, currentsize, length, null, string, substr, verify) builtin;


	io_log_infop = null ();
	on cleanup call clean_up;

	call get_temp_segment_ (name, io_log_infop, code);
	if code ^= 0 then do;
	     call com_err_ (code, name, "Unable to get temp segment.");
	     return;
	end;

	call build_data;

/* Now create the data segment */

	cds.p (1) = io_log_infop;
	cds.len (1) = currentsize (io_log_info);
	cds.struct_name (1) = "dummy_struct";
	cds.p (2) = null;
	cds.len (2) = 0;
	cds.struct_name (2) = "";
	cds.seg_name = name;
	cds.num_exclude_names = 0;
	cds.exclude_array_ptr = null ();
	string (cds.switches) = "0"b;
	cds.have_text = "1"b;
	call create_data_segment_ (addr (cds), code);
	if code ^= 0 then call com_err_ (name, code);

done:	call clean_up;
	return;

build_data: proc;

/* Disk */

	     call set_device ("dsk");

	     call set_major (0000b);			/* Ready */
	     call log_detail ("000001");		/* 1 retry */
	     call log_detail ("000010");		/* 2 retries */
	     call log_detail ("000011");		/* 3 retries */
	     call log_detail ("010000");		/* EDAC correction */

	     call set_major (0001b);			/* Channel ready */

	     call set_major (0010b);			/* Attention */
	     call log_detail ("001000");		/* Dev inop */
	     call log_detail ("001101");		/* CA EN1 error */
	     call log_detail ("00001X");		/* Seek incomplete */
	     call log_detail ("001110");		/* CA alert */
	     call log_detail ("010000");		/* Device in standby */

	     call set_major (0011b);			/* Data alert */
	     call log_detail ("000010");		/* Transmission parity */
	     call log_detail ("000100");		/* Invalid seek address */
	     call log_detail ("0X1000");		/* Header verification error */
	     call log_detail ("X1X000");		/* Check character alert */
	     call log_detail ("1X0000");		/* Compare alert */

	     call set_major (0100b);			/* End of file */

	     call set_major (0101b);			/* Command reject */

	     call set_major (1010b);			/* Mpc device attention */
	     call log_detail ("001011");		/* CA unexpected interrupt */

	     call set_major (1011b);			/* Mpc device data alert */
	     call log_detail ("001110");		/* Edac parity */
	     call log_detail ("000010");		/* Inconsistent command */
	     call log_detail ("010011");		/* Search alert */
	     call log_detail ("000011");		/* Sumcheck error */
	     call log_detail ("010100");		/* Cyc code not 1 st */
	     call log_detail ("001001");		/* Error correction rquired */
	     call log_detail ("010110");		/* Sync byte not hex 19 */
	     call log_detail ("001010");		/* Edac error uncorrectable */
	     call log_detail ("010111");		/* Error, alt track */
	     call log_detail ("010001");		/* Sector size error */
	     call log_detail ("011001");		/* Edac correction (last ) */
	     call log_detail ("010010");		/* Non-std secotr size */
	     call log_detail ("011010");		/* Edac correction (last ) */

	     call set_major (1101b);			/* Mpc command reject */

/* Tape */

	     call set_device ("tap");

	     call set_major (0000b);			/* Ready */
	     call log_status ("001100");		/* Code alert */

	     call set_major (0010b);			/* Attention */
	     call log_status ("0XX10X");		/* Handler in standby */
	     call log_detail ("0X1X0X");		/* Dev fault */
	     call log_detail ("01XX00");		/* Blank tape on write */

	     call set_major (0011b);			/* Device data alert */
	     call log_detail ("XXXX11");		/* Bit detected on erase */
	     call log_detail ("XX1XXX");		/* Lateral parity alert */
	     call log_detail ("X1XXXX");		/* Longitudinal parity alert */
	     call log_detail ("001000");		/* Frame drop */

	     call set_major (0100b);			/* End of file */
	     call suppress_log ("001111");		/* Eof - 7trk */
	     call suppress_log ("010011");		/* Eof - 9trk */
	     call log_detail ("111111");		/* Data alert */

	     call set_major (1010b);			/* Mpc device attention */
	     call log_detail ("0011XX");		/* TCA malfunction */
	     call log_detail ("010000");		/* Device malfunction */

	     call set_major (1011b);			/* Mpc datra alert */
	     call log_detail ("001000");		/* Pe burst error */
	     call log_detail ("001001");		/* Preamble error */
	     call log_detail ("010000");		/* Multi track error */
	     call log_detail ("010001");		/* Skew error */
	     call log_detail ("010010");		/* Postamble error */
	     call log_detail ("010011");		/* Nrzi ccc err */
	     call log_detail ("100000");		/* Marginal condition */

/* Printer */

	     call set_device ("prt");

	     call set_major (0010b);			/* Attention */
	     call log_detail ("000000");		/* Power fault */
	     call log_detail ("XXXX1X");		/* Stopped */
	     call log_detail ("XXX1XX");		/* VFU/VFC alert */
	     call log_detail ("XX1XXX");		/* Check alert */

	     call set_major (0011b);			/* Data alert */
	     call log_detail ("000000");		/* Image buffer alert */
	     call log_detail ("0XX01X");		/* Alert before printing start */
	     call log_detail ("XXX10X");		/* Alert after printing started */
	     call log_detail ("XX1XXX");		/* Warning */
	     call log_detail ("X1XXXX");		/* Motion alert */

	     call set_major (0101b);			/* Command reject */
	     call suppress_log ("100000");		/* Top page echo */

/* Reader */

	     call set_device ("rdr");

	     call set_major (0010b);			/* Attention */
	     call log_detail ("000000");		/* Off line */
	     call log_detail ("XX1XXX");		/* Feed alert */
	     call log_detail ("X1XXXX");		/* Jam */
	     call log_detail ("1X0XXX");		/* Read alert */

	     call set_major (0011b);			/* Data alert */
	     call log_detail ("000X10");		/* Validaty alert */
	     call log_detail ("0001X0");		/* Dual read */

/* Punch */

	     call set_device ("pun");
	     call set_major (0010b);			/* Attention */
	     call log_detail ("000000");		/* Off line */
	     call log_detail ("0X1XXX");		/* Feed alert */
	     call log_detail ("X1XXXX");		/* Jam */

	   	call set_device ("hch");		/* hyperchannel */

		call set_major (0001b);		/* Busy */
		call set_major (0011b);		/* Data Alert */
		call set_major (0100b);		/* EOF */
		call set_major (0101b);		/* Interstruction Reject */
		call suppress_log ("100000");		/* 40 - in use ? */
		call set_major (1011b);		/* Adapter Alert */
		call suppress_log ("001000");		/* 10 - timeout */

	     return;

	end build_data;

set_device: proc (arg_device);

dcl  arg_device char (*);

dcl  i fixed bin;

	     if length (arg_device) ^= 3 then do;
		call com_err_ (0, name, "Invalid device name: ^a", arg_device);
		go to done;
	     end;
	     do i = 1 to io_log_info.ndev;
		logp = addr (io_log_info.log_entry (i));
		if log.dev_name = arg_device then do;
		     call com_err_ (0, name, "Duplicated device name: ^a", arg_device);
		     go to done;
		end;
	     end;

	     io_log_info.ndev = io_log_info.ndev + 1;
	     logp = addr (io_log_info.log_entry (io_log_info.ndev));
	     log.dev_name = arg_device;
	     log.status (*, *) = "1"b;		/* Log all status */
	     log.status (0, *) = "0"b;		/* Except ready */
	     log.detail (*, *) = "0"b;

	     return;

	end set_device;


set_major: proc (arg_major);

dcl  arg_major fixed bin;

	     if arg_major < 0 | arg_major > 15 then do;
		call com_err_ (0, name, "Invalid major status level: ^d", arg_major);
		go to done;
	     end;
	     cur_major = arg_major;

	     return;

	end set_major;

suppress_log: proc (arg_status);

dcl  arg_status char (*);

	     call off (arg_status, log.status);

	     return;

	end suppress_log;

log_detail: proc (arg_status);

dcl  arg_status char (*);

	     call on (arg_status, log.detail);
	     call on (arg_status, log.status);

	     return;

	end log_detail;


log_status: proc (arg_status);

dcl  arg_status char (6);

	     call on (arg_status, log.status);

	     return;

	end log_status;

/* Routines that minipulate the tables */

on:	proc (arg_status, arg_table);

dcl  arg_status char (*);
dcl  arg_table (*, *) bit (1) unal;

	     call set (arg_status, arg_table, "1"b);
	     return;

	end on;

off:	proc (arg_status, arg_table);

dcl  arg_status char (*);
dcl  arg_table (*, *) bit (1) unal;

	     call set (arg_status, arg_table, "0"b);
	     return;

	end off;

set:	proc (arg_status, arg_table, arg_state);

dcl  arg_status char (*);
dcl  arg_table (*, *) bit (1) unal;
dcl  arg_state bit (1);

dcl (i, j, k) fixed bin;
dcl  x_count fixed bin;
dcl  basic_mask bit (6);
dcl  work_mask bit (6);
dcl  n_x_states fixed bin;
dcl  x_bits bit (6);

	     if length (arg_status) ^= 6 then do;
bad_status:	call com_err_ (0, name, "Invalid status mask: ^a", arg_status);
		go to done;
	     end;
	     if verify (arg_status, "01X") ^= 0 then go to bad_status;

	     basic_mask = "0"b;
	     x_count = 0;
	     do i = 1 to 6;
		if substr (arg_status, i, 1) = "1" then
		     substr (basic_mask, i, 1) = "1"b;
		else if substr (arg_status, i, 1) = "X" then
		     x_count = x_count + 1;
	     end;

	     if x_count = 0 then do;			/* Easy case */
		arg_table (cur_major, bin (basic_mask)) = arg_state;
		return;
	     end;

	     n_x_states = 2 ** x_count;
	     do i = 1 to n_x_states;
		work_mask = basic_mask;
		x_bits = bit (bin (i - 1, 6), 6);
		k = 7 - x_count;
		do j = 1 to 6;
		     if substr (arg_status, j, 1) = "X" then do;
			substr (work_mask, j, 1) = substr (x_bits, k, 1);
			k = k + 1;
		     end;
		end;
		arg_table (cur_major, bin (work_mask)) = arg_state;
	     end;

	     return;

	end set;

clean_up:	proc;

	     if io_log_infop ^= null () then call release_temp_segment_ (name, io_log_infop, code);
	     return;

	end clean_up;

%include io_log_status_info;

%include cds_args;

     end io_log_status_info;

