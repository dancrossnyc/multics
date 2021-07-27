/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */

cache_threshold_defaults_:
     proc;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
/*									*/
/* cache_threshold_defaults_ - this segment contains the default cache threshold values	*/
/* as defined by HIS (LCPD HW Eng.) as an acceptable error rate for the cache memory in	*/
/* the  L68, DPS68 and DPS8 processors.						*/
/*									*/
/* Created: 2/84 by GA Texada							*/
/*									*/
/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */


/* format: style1,ind2,^inddcls,ifthenstmt,dclind2,declareind2,ifthendo,ifthen*/

dcl code		     fixed bin (35),
  1 cdsa		     aligned like cds_args,
  1 cache_threshold_defaults_ aligned like cache_threshold_data;

dcl (addr, null, size, string, unspec) builtin;

dcl com_err_	     entry () options (variable),
  create_data_segment_   entry (ptr, fixed bin (35));


	unspec (cache_threshold_defaults_) = ""b;	/* start clean				*/
	cache_threshold_defaults_.pri_dir_parity = 4;	/* L68 use only				*/
	cache_threshold_defaults_.port_buffer(*) = 1;	/* HW Eng. said .2, but...			*/
	cache_threshold_defaults_.pri_dir = 2;
	cache_threshold_defaults_.wno_parity_any_port = 1;/* HW Eng. said .4 but...			*/
	cache_threshold_defaults_.dup_dir_parity(*) = 1;
	cache_threshold_defaults_.dup_dir_multimatch = 0; /* These are NOT acceptable			*/

	cdsa.sections (1).p = addr (cache_threshold_defaults_);
	cdsa.sections (1).len = size (cache_threshold_defaults_);
	cdsa.sections (1).struct_name = "cache_threshold_defaults_";
	cdsa.seg_name = "cache_threshold_defaults_";
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null ();
	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;
	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0 then call com_err_ (code, "cache_threshold_defaults_");
	return;

%include cache_threshold_data;

%include cds_args;
     end cache_threshold_defaults_;
