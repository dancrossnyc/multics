/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
gfms_ext:proc;

/* Generate object for "gfms_ext" data.

   Author:    Dave Ward 1981
   Modified:  Ron Barstad  83-07-21  Remove dependency on ted_com, add include file
 */
/** Initialize cds_args **/

	cds_args_ptr=addr(space_for_cds_args);
	unspec(space_for_cds_args)="0"b;

/** Reference structure input to cds to assure it is in runtime table. **/

	if addr(gfms_ext)=null() then ;

/** No text section **/
	cds_args_ptr -> cds_args.sections (1).p = null ();
	cds_args_ptr -> cds_args.sections (1).len = 0;
	cds_args_ptr -> cds_args.sections (1).struct_name = "NO_TEXT";

/** Static section **/
	cds_args_ptr -> cds_args.sections (2).p = addr (gfms_ext);	/* Caller's data. */
	cds_args_ptr -> cds_args.sections (2).len = size (gfms_ext);	/* No. words in data structure. */
	cds_args_ptr -> cds_args.sections (2).struct_name = "gfms_ext";

	cds_args_ptr -> cds_args.seg_name = "gfms_ext";	/* Entryname of object segment. */
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
	      call com_err_ (code, "cds_gfms_ext");
	   else 
	      call com_err_( 0,"gfms_ext","Object for gfms_ext created [^i words].",size(gfms_ext));

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
/** This data structure must exactly match that of gfms_ext.incl.pl1 **/

dcl 1 gfms_ext aligned,

      2 print_routines	aligned like gfms_ext$print_routines,

      2 temp_segs		aligned like gfms_ext$temp_segs,

      2 tape_file		aligned like gfms_ext$tape_file,

      2 options		aligned like gfms_ext$options,

      2 working_dir		char (168) var,	

      2 mapping_rule	char(3),

      2 umc_name		char(12);
%page;
%include gfms_ext;
%page;
%include cds_args;
end;
