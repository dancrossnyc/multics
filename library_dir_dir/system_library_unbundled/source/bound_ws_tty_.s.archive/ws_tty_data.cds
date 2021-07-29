/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-11-17,RBarstad), approve(86-12-11,MCR7585),
     audit(86-12-12,Gilcrease):
     Written. External data seg for ws_tty_.
                                                   END HISTORY COMMENTS */

ws_tty_data:proc;

/* Version 1.0
*/

/** Initialize cds_args **/

	cds_args_ptr=addr(space_for_cds_args);
	unspec(space_for_cds_args)="0"b;

/** Reference structure input to cds to assure it is in runtime table. **/

	if addr(ws_tty_data)=null() then ;

/** No text section **/
	cds_args_ptr -> cds_args.sections (1).p = null ();
	cds_args_ptr -> cds_args.sections (1).len = 0;
	cds_args_ptr -> cds_args.sections (1).struct_name = "NO_TEXT";

/** Static section **/
	cds_args_ptr -> cds_args.sections (2).p = addr (ws_tty_data);	/* Caller's data. */
	cds_args_ptr -> cds_args.sections (2).len = size (ws_tty_data);	/* No. words in data structure. */
	cds_args_ptr -> cds_args.sections (2).struct_name = "ws_tty_data";

	cds_args_ptr -> cds_args.seg_name = "ws_tty_data";	/* Entryname of object segment. */
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
	      call com_err_ (code, "cds_ws_tty_data");
	   else 
	      call com_err_( 0,"ws_tty_data","Object for ws_tty_data created [^i words].",size(ws_tty_data));

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
/** This data structure must exactly match that of ws_tty_data.incl.pl1 **/

dcl 1 ws_tty_data		aligned,
      2 Flags		aligned,
        3 Debug		bit (1) unaligned init ("0"b),
        3 Trace		bit (1) unaligned init ("0"b),
        3 Pad 		bit (34) unaligned init ((34)"0"b);

%page;
%include ws_tty_data;
%page;
%include cds_args;
end;
