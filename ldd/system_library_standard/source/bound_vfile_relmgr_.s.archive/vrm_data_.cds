/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */
vrm_data_: proc;

/* NOTES:

   This procedure creates the vrm_data_ database.
*/


/* HISTORY:

   82-08-20 R. Harvey: Initially written by stealing from mrds_data_.cds

   84-05-18 B. G. Moberg : Changed max_vfile_wait_time to 300

*/

%include cds_args;


dcl 1 vrmd aligned,					/* the values to go into vrm_data_ */

     2 oid_slots_per_section  fixed bin (17) init (100),	/* Number of opening id slots  per section */

    2 max_vfile_wait_time fixed bin (35) init (300),	/* max time to wait for file operations for -share option */

     2 max_kattr_length fixed bin (35) init (253),	/* maximum length of key */

     2 typed_vector_array_limit fixed bin (35) init (34359738367), /* Max fixed bin 35 */
   
     2 iocb_list_block_size fixed bin (17) init (100);	/* Number of iocbs allowed in each iocb list block */
dcl 1 cdsa like cds_args;
dcl  code fixed bin (35);

dcl (addr,
     size,
     string,
     null) builtin;

dcl  create_data_segment_ entry (ptr, fixed bin (35));
dcl  com_err_ entry options (variable);

	cdsa.sections.p (1) = addr (vrmd);		/* init. info for cds */
	cdsa.sections.len (1) = size (vrmd);
	cdsa.sections.struct_name (1) = "vrmd";
	cdsa.seg_name = "vrm_data_";
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null;
	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0 then call com_err_ (code, "vrm_data_");
	return;

     end vrm_data_;
