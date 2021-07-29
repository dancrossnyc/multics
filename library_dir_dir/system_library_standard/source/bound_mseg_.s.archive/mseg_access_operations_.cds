/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1985 *
   *                                                         *
   *********************************************************** */

/* format: style3,idind30,linecom */

mseg_access_operations_:
     procedure ();

/****
   this data segment defines a named constant for each message segment
   operation that can be called from the outer rings.  constants are
   used to identify the operation and define the type of access required
   for each operation.

    written January 1985 by M. Pandolf
    modified March 1985 by M. Pandolf to eliminate check_name and add audit_this_operation
                                     to leave caller validation intact when getting UID
   modified April 1985 by M. Pandolf to eliminate restore_caller_validation
   Modified 1985-04-16, BIM: group ms/mbx parallel operations.
             audit get_count.
*/

/**** NOTE: If you modify any of the access_operations_ referenced by this
      program, you must recompile this program with the new version of
      access_operations_ to actually make the changes take effect. */

/**** This segment would work better if it wrote its own program
      which called cds, and compiled and executed it. However,
      there is no time. So the structure below must match the
      suboutine calls below it. */

/**** Note that we assume that some gate entries will have to make 
      two calls to mseg_check_access_ to audit both sides of an
      hybrid operation. That's life in MR11 */

declare	1 mops			aligned,
	  2 data			aligned,
	    3 (d_create_seg, d_delete_seg, d_acl_modify_seg, d_acl_list_seg, d_open_seg, d_close_seg, d_compact_seg,
	         d_get_count_seg, d_read_message, d_read_own_message, d_read_delete_message, d_delete_message,
	         d_update_message, d_add_message, d_accept_wakeups_seg, d_send_normal_wakeup, d_send_urgent_wakeup,
	         d_read_attr_seg, d_modify_attr_seg, d_read_fs_attr_seg, d_modify_fs_attr_seg, d_copy_seg,
	         d_admin_add_message, d_read_delete_own_message, d_reset_salvage_bit_seg)
				aligned like mseg_access_operation,
	  2 (
	  create_seg		init (1),
	  delete_seg		init (2),
	  acl_modify_seg		init (3),
	  acl_list_seg		init (4),
	  open_seg		init (5),
	  close_seg		init (6),
	  compact_seg		init (7),
	  get_count_seg		init (8),
	  read_message		init (9),
	  read_own_message		init (10),
	  read_delete_message	init (11),
	  delete_message		init (12),
	  update_message		init (13),
	  add_message		init (14),
	  accept_wakeups_seg	init (15),
	  send_normal_wakeup	init (16),
	  send_urgent_wakeup	init (17),
	  read_attr_seg		init (18),
	  modify_attr_seg		init (19),
	  read_fs_attr_seg		init (20),
	  modify_fs_attr_seg	init (21),
	  copy_seg		init (22),
	  admin_add_message		init (23),
	  read_delete_own_message	init (24),
	  reset_salvage_bit_seg	init (25)
	  )			fixed bin;

%include mseg_access_operation;

declare	(
	DONT_AUDIT		init ("1"b),
	DONT_AUDIT_SUCCESS		init ("01"b),
	NON_NULL_MODES		init ("001"b),
	NO_MODES			init ("0001"b),
	O_FOR_R			init ("00001"b),
	O_FOR_D			init ("000001"b),
	ADMIN			init ("0000001"b),
	DIR_MODES			init ("00000001"b),
	DIR_MODES_OR_EX_MODES	init ("000000001"b)
	)			bit (36) aligned int static options (constant);


op:
     procedure (op_ptr, a_op, flags, modes, index, dir_modes);

declare	op_ptr			pointer;
declare	a_op			bit (36) aligned;
declare	modes			bit (36) aligned;
declare	flags			bit (36) aligned;
declare	index			fixed bin (9);
declare	dir_modes			bit (3);

declare	1 maop			aligned like mseg_access_operation based (op_ptr);

	unspec (maop) = ""b;
	maop.access_operation = a_op;
	maop.required_modes = modes;
	string (maop.flags) = flags;
	maop.mseg_access_op_index = index;
	maop.required_dir_modes = dir_modes;
	return;
     end op;

	call op (addr (mops.d_create_seg), access_operations_$mseg_create, NO_MODES | DIR_MODES, ""b, MSEG_CREATE_SEG,
	     M_ACCESS | A_ACCESS);
	call op (addr (mops.d_delete_seg), access_operations_$mseg_delete, NO_MODES | DIR_MODES, ""b,
	     MSEG_MODIFY_SEG_ATTR, M_ACCESS);
	call op (addr (mops.d_acl_modify_seg), access_operations_$mseg_access_mod, NO_MODES | DIR_MODES, ""b,
	     MSEG_MODIFY_SEG_ATTR, M_ACCESS);
	call op (addr (mops.d_acl_list_seg), access_operations_$mseg_access_read, NO_MODES | DIR_MODES, ""b,
	     MSEG_READ_SEG_ATTR, S_ACCESS);
	call op (addr (mops.d_open_seg), access_operations_$mseg_open, NON_NULL_MODES, ""b, MSEG_READ_SEG_ATTR, ""b);
	call op (addr (mops.d_close_seg), access_operations_$mseg_close, NO_MODES, ""b, 0, ""b);
	call op (addr (mops.d_compact_seg), access_operations_$mseg_compact, ""b, MSEG_D_ACCESS, MSEG_MODIFY_SEG_ATTR,
	     ""b);
	call op (addr (mops.d_get_count_seg), access_operations_$mseg_get_count, ""b, MSEG_S_ACCESS, MSEG_READ_SEG_ATTR,
	     ""b);
	call op (addr (mops.d_read_message), access_operations_$mseg_read_message, DONT_AUDIT_SUCCESS, MSEG_R_ACCESS,
	     MSEG_READ_MESSAGE, ""b);
	call op (addr (mops.d_read_own_message), access_operations_$mseg_read_message, O_FOR_R | DONT_AUDIT_SUCCESS,
	     MSEG_R_ACCESS, MSEG_READ_MESSAGE, ""b);
	call op (addr (mops.d_read_delete_message), access_operations_$mseg_read_delete_message, DONT_AUDIT_SUCCESS,
	     MSEG_R_ACCESS | MSEG_D_ACCESS, MSEG_MODIFY_MESSAGE, ""b);
	call op (addr (mops.d_delete_message), access_operations_$mseg_delete_message, DONT_AUDIT_SUCCESS | O_FOR_D,
	     MSEG_D_ACCESS, MSEG_MODIFY_MESSAGE, ""b);
	call op (addr (mops.d_update_message), access_operations_$mseg_update_message, DONT_AUDIT_SUCCESS,
	     MSEG_D_ACCESS, MSEG_MODIFY_MESSAGE, ""b);
	call op (addr (mops.d_add_message), access_operations_$mseg_add_message, ""b, MSEG_A_ACCESS, MSEG_ADD_MESSAGE,
	     ""b);
	call op (addr (mops.d_admin_add_message), access_operations_$mseg_add_message, ADMIN, MSEG_A_ACCESS,
	     MSEG_ADD_MESSAGE, ""b);
	call op (addr (mops.d_accept_wakeups_seg), access_operations_$mseg_accept_wakeups, ""b, MSEG_D_ACCESS,
	     MSEG_ACCEPT_WAKEUPS, ""b);
	call op (addr (mops.d_send_normal_wakeup), access_operations_$mseg_wakeup_send, ""b, MSEG_W_ACCESS, 0, ""b);
	call op (addr (mops.d_send_urgent_wakeup), access_operations_$mseg_wakeup_send, ""b, MSEG_U_ACCESS, 0, ""b);
	call op (addr (mops.d_read_attr_seg), access_operations_$mseg_attr_read, ""b, MSEG_S_ACCESS, MSEG_READ_SEG_ATTR,
	     ""b);
	call op (addr (mops.d_modify_attr_seg), access_operations_$mseg_attr_mod, ""b, MSEG_D_ACCESS,
	     MSEG_MODIFY_SEG_ATTR, ""b);
	call op (addr (mops.d_read_fs_attr_seg), access_operations_$mseg_attr_read,
	     NON_NULL_MODES | DIR_MODES_OR_EX_MODES, ""b, MSEG_READ_SEG_ATTR, S_ACCESS);
	call op (addr (mops.d_modify_fs_attr_seg), access_operations_$mseg_attr_mod, NO_MODES | DIR_MODES, ""b,
	     MSEG_MODIFY_SEG_ATTR, M_ACCESS);
	call op (addr (mops.d_copy_seg), access_operations_$mseg_read_message, ""b, MSEG_R_ACCESS, MSEG_COPY_SEG, ""b);
	call op (addr (mops.d_read_delete_own_message), access_operations_$mseg_read_delete_message,
	     O_FOR_R | O_FOR_D | DONT_AUDIT_SUCCESS, MSEG_R_ACCESS | MSEG_D_ACCESS, MSEG_MODIFY_MESSAGE, ""b);

/**** The next one, reset_salv_bit_seg, is strange.  Since the salvage bit
      is set automatically, resetting it is not strictly a write-down.
      We permit it to be reset by any process with D access that is 
      greater_or_equal to the segment parent access class.  Thus
      "READ_SEG_ATTR", since the AIM check is the same for reading the salvage 
      bit.  The covert channel here is covered by explicit code in
      mseg_$reset_salvaged_flag_seg. */

	call op (addr (mops.d_reset_salvage_bit_seg), access_operations_$mseg_attr_mod, ""b, MSEG_D_ACCESS,
	     MSEG_READ_SEG_ATTR, ""b);

dcl	code			fixed bin (35);
dcl	1 local_cds_args		aligned like cds_args;
dcl	exclude_array		(1) char (32) init ("pad*");

dcl	create_data_segment_	entry (ptr, fixed bin (35));
dcl	com_err_			entry () options (variable);

						/** begin generation of data segment */

	unspec (local_cds_args) = ""b;

	local_cds_args.sections (1).p = addr (mops);
	local_cds_args.sections (1).len = size (mops);
	local_cds_args.sections (1).struct_name = "mops";
	local_cds_args.seg_name = "mseg_access_operations_";
	local_cds_args.exclude_array_ptr = addr (exclude_array);
	local_cds_args.num_exclude_names = hbound (exclude_array, 1);
	local_cds_args.switches.have_text = "1"b;

	call create_data_segment_ (addr (local_cds_args), code);
	if code ^= 0
	then call com_err_ (code, "mseg_access_operations_");

	return;


declare	(
	access_operations_$mseg_add_message,
	access_operations_$mseg_create,
	access_operations_$mseg_delete,
	access_operations_$mseg_open,
	access_operations_$mseg_close,
	access_operations_$mseg_attr_read,
	access_operations_$mseg_attr_mod,
	access_operations_$mseg_access_read,
	access_operations_$mseg_access_mod,
	access_operations_$mseg_compact,
	access_operations_$mseg_copy,
	access_operations_$mseg_get_count,
	access_operations_$mseg_read_message,
	access_operations_$mseg_delete_message,
	access_operations_$mseg_read_delete_message,
	access_operations_$mseg_update_message,
	access_operations_$mseg_accept_wakeups,
	access_operations_$mseg_wakeup_send
	)			bit (36) aligned ext static;

%page;
%include cds_args;
%include mseg_access_mode_values;
%include access_mode_values;
     end mseg_access_operations_;
