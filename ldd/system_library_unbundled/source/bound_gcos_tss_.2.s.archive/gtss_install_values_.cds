/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
gtss_install_values_:proc;

/* Generate object for "gtss_install_values_" data.

   Author:    Dave Ward 1981
   Modified:  Ron Barstad  83-07-21  Remove dependency on ted_com, add include file
 */
/** Initialize cds_args **/

	cds_args_ptr=addr(space_for_cds_args);
	unspec(space_for_cds_args)="0"b;

/** Reference structure input to cds to assure it is in runtime table. **/

	if addr(gtss_install_values_)=null() then ;

/** No text section **/
	cds_args_ptr -> cds_args.sections (1).p = null ();
	cds_args_ptr -> cds_args.sections (1).len = 0;
	cds_args_ptr -> cds_args.sections (1).struct_name = "NO_TEXT";

/** Static section **/
	cds_args_ptr -> cds_args.sections (2).p = addr (gtss_install_values_);	/* Caller's data. */
	cds_args_ptr -> cds_args.sections (2).len = size (gtss_install_values_);	/* No. words in data structure. */
	cds_args_ptr -> cds_args.sections (2).struct_name = "gtss_install_values_";

	cds_args_ptr -> cds_args.seg_name = "gtss_install_values_";	/* Entryname of object segment. */
	cds_args_ptr -> cds_args.num_exclude_names = 0;		/* All level 2 names are entry points. */
	cds_args_ptr -> cds_args.exclude_array_ptr = null ();
	cds_args_ptr -> cds_args.switches.defs_in_link = "0"b;	/* Definitions contiguous to text section. */
	cds_args_ptr -> cds_args.switches.separate_static = "0"b;	/* Static in linkage section (to bind). */
	cds_args_ptr -> cds_args.switches.have_text = "0"b;	/* No text section. */
	cds_args_ptr -> cds_args.switches.have_static = "1"b;	/* There is a static section. */
	cds_args_ptr -> cds_args.switches.pad = "0"b;		/* Must be zeroes (see create_data_segment_). */

	call create_data_segment_ (cds_args_ptr, code);
	if code ^= 0 
	   then 
	      call com_err_ (code, "cds_gtss_install_values_");
	   else 
	      call com_err_( 0,"gtss_install_values_","Object for gtss_install_values_ created [^i words].",size(gtss_install_values_));

	return;
%page;
/** Data for cds **/
dcl  addr                     builtin;
dcl  cds_args_ptr             ptr init(null());
dcl  code                     fixed bin(35);
dcl  com_err_                 entry options(variable);
dcl  create_data_segment_     entry(ptr,fixed bin(35));
dcl  null                     builtin;
dcl  size                     builtin;
dcl  unspec                   builtin;
dcl  1 space_for_cds_args     aligned like cds_args;
%page;
/** This data structure must exactly match that of gtss_install_values_.incl.pl1 **/

dcl 1 gtss_install_values_ aligned,
      2 fast_msf     char(32)var	init("gtss_fast_library_"),
      2 Lstar_msf    char(32)var	init("gtss_Lstar_"),
      2 starL_msf    char(32)var	init("gtss_starL_"),
      2 memory_limit fixed bin(24)	init(261120),
      2 time_limit   fixed bin(24)	init(1000);

%include gtss_install_values_;
%page;
%include cds_args;
end;
