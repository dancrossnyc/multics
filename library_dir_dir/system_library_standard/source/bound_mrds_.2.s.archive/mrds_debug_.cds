/* ***********************************************************
   *                                                         *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   *                                                         *
   *********************************************************** */

/* ******************************************************
   *                                                    *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   *                                                    *
   ****************************************************** */



/* HISTORY:

   79-07-01 Jim Gray: Originially written.

   80-11-17 Rickie E. Brinegar: Call to com_err_ with a 0 error code
   changed to a call to ioa_.

*/


mrds_debug_ : procedure () ;

/* DESCRIPTION:

   this is the source to be used by the create_data_segment command
   to create the mrds_debug_ data segment object. to make additions
   or changes to the data segment, change the mrds_debug_structure
   declared in this pl1 program, then invoke cds with the pathname
   of this source.

*/

/* PARAMETERS:

   (input) changes to the mrds_debug_structure

   (output) a new mrds_debug_ data segment, after running
   the command create_data_segment on this source.

*/


/* CHANGE THIS STRUCTURE TO MAKE CHANGES IN MRDS_DEBUG_ */

declare 1 mrds_debug_structure,
        2 switch bit (9) unal dimension (400) init ((400) ((9) "0"b)) ; /* one 9 bit debug switch for each module in MRDS */



/* fill in the input structure to the subroutine
   call to create_data_segment_ */


	data.sections.p (1) = null () ;		/* no text section */
	data.sections.len (1) = 0 ;
	data.sections.struct_name = "NO_TEXT" ;
	data.sections.p (2) = addr (mrds_debug_structure) ; /* static section */
	data.sections.len (2) = size (mrds_debug_structure) ;
	data.sections.struct_name (2) = "mrds_debug_structure" ;
	data.seg_name = "mrds_debug_" ;
	data.num_exclude_names = 0 ;
	data.exclude_array_ptr = null () ;
	data.switches.defs_in_link = OFF ;
	data.switches.separate_static = OFF ;
	data.switches.have_text = OFF ;
	data.switches.have_static = ON ;
	data.switches.pad = OFF ;

/* make the call required by the create_data_segment command */

	call create_data_segment_ (addr (data), error_code) ;
	if error_code = 0 then
	     call ioa_ ("mrds_debug_ data segment created") ;
	else call com_err_ (error_code, "creating data segment for mrds_debug_") ;

	return ;


declare 1 data like cds_args ;			/* local version of create_data_segment_ input structure */
declare  create_data_segment_ entry (ptr, fixed bin (35)) ; /* subroutine for cds */
declare (addr, size) builtin ;


%include cds_args ;
%include mrds_debug_names ;

declare  com_err_ entry options (variable) ;		/* reports errors */
dcl  ioa_ entry options (variable);			/* other messages */
declare  error_code fixed bin (35) ;			/* error status encoding */
declare  ON bit (1) init ("1"b) ;			/* true value */
declare  OFF bit (1) init ("0"b) ;			/* false value */

     end ;
