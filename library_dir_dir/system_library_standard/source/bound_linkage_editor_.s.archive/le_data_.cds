/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-08-12,Elhard), approve(86-08-12,MCR7505),
     audit(86-12-10,DGHowe), install(86-12-10,MR12.0-1241):
     Originally written to define constant and static data used by le_.
                                                   END HISTORY COMMENTS */

/* format: style1,insnl,ifthendo,indthenelse,^indnoniterdo,^inditerdo,indcom,^indthenbegin,^indprocbody,ind2,ll78,initcol0,dclind4,idind24,struclvlind1,comcol41 */

le_data_:
  proc;

  /*** ****************************************************************/
  /***							*/
  /***	Name:	le_data_					*/
  /***	Function:	This procedure is used to create the le_data_	*/
  /***		data segment for the linkage editor (le).	*/
  /***							*/
  /*** ****************************************************************/

  /* constants */

  dcl true		bit (1) static options (constant) init ("1"b);
  dcl false		bit (1) static options (constant) init ("0"b);

  /* procedures */

  dcl com_err_		entry () options (variable);
  dcl create_data_segment_	entry (ptr, fixed bin (35));

  /* automatic */

  dcl ec			fixed bin (35) automatic;
  dcl 01 le_cds_args	aligned like cds_args automatic;
  dcl 01 le_static_data	aligned automatic,
       02 caller		char (32) varying,
       02 debug		bit (1),
       02 display_severity	fixed bin,
       02 max_severity	fixed bin,
       02 patch_ptr		ptr,
       02 running		bit (1);
  dcl 01 le_text_data	aligned automatic,
       02 version_number	fixed bin,
       02 version_string	char (64) varying,
       02 version_suffix	char (64) varying;

  /* builtin */

  dcl addr		builtin;
  dcl null		builtin;
  dcl size		builtin;
  dcl string		builtin;

  /* build the cds_args structure */

  le_cds_args.sections (1).p = addr (le_text_data);
  le_cds_args.sections (1).len = size (le_text_data);
  le_cds_args.sections (1).struct_name = "le_text_data";

  le_cds_args.sections (2).p = addr (le_static_data);
  le_cds_args.sections (2).len = size (le_static_data);
  le_cds_args.sections (2).struct_name = "le_static_data";

  le_cds_args.seg_name = "le_data_";
  le_cds_args.num_exclude_names = 0;
  le_cds_args.exclude_array_ptr = null;

  string (le_cds_args.switches) = ""b;

  le_cds_args.switches.have_text = true;
  le_cds_args.switches.have_static = true;

  /* initialize the text and static section data */

  le_text_data.version_number = 1;
  le_text_data.version_string = "le 1.0";
  le_text_data.version_suffix = " Version 1.0 of November 27, 1986";

  le_static_data.caller = "";
  le_static_data.debug = false;
  le_static_data.display_severity = 0;
  le_static_data.max_severity = 0;
  le_static_data.patch_ptr = null;
  le_static_data.running = ""b;

  /* create the segment */

  call create_data_segment_ (addr (le_cds_args), ec);
  if ec ^= 0
    then call com_err_ (ec, "le_data_");

  return;

/****  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/****  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


%include cds_args;

  end le_data_;

