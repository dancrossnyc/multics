/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
gtss_starCF_:proc;

/* Generate object for "gtss_starCF_" data.

   Author:    Dave Ward 1981
   Modified:  Ron Barstad  83-07-21  Remove dependency on ted_com, add include file
 */
/** Initialize cds_args **/

	cds_args_ptr=addr(space_for_cds_args);
	unspec(space_for_cds_args)="0"b;

/** Reference structure input to cds to assure it is in runtime table. **/

	if addr(gtss_starCF_)=null() then ;

/** No text section **/
	cds_args_ptr -> cds_args.sections (1).p = null ();
	cds_args_ptr -> cds_args.sections (1).len = 0;
	cds_args_ptr -> cds_args.sections (1).struct_name = "NO_TEXT";

/** Static section **/
	cds_args_ptr -> cds_args.sections (2).p = addr (gtss_starCF_);	/* Caller's data. */
	cds_args_ptr -> cds_args.sections (2).len = size (gtss_starCF_);	/* No. words in data structure. */
	cds_args_ptr -> cds_args.sections (2).struct_name = "gtss_starCF_";

	cds_args_ptr -> cds_args.seg_name = "gtss_starCF_";	/* Entryname of object segment. */
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
	      call com_err_ (code, "cds_gtss_starCF_");
	   else 
	      call com_err_( 0,"gtss_starCF_","Object for gtss_starCF_ created [^i words].",size(gtss_starCF_));

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
/** This data structure must exactly match that of gtss_starCF_.incl.pl1 **/

dcl 1 gtss_starCF_ aligned,
      2 FILE like gtss_starCF_$FILE;

%include gtss_starCF_;
%page;
%include cds_args;
end;
