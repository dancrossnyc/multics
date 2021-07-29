/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */



/* HISTORY COMMENTS:
  1) change(86-10-03,Dupuis), approve(86-10-21,MCR7562), audit(86-10-22,Blair),
     install(86-10-23,MR12.0-1199):
     Added the lock_wait_time variable.
  2) change(88-01-27,Dupuis), approve(88-03-03,MCR7844), audit(88-03-11,Blair),
     install(88-03-15,MR12.2-1036):
     Added the trace_every_n_tuples field.
                                                   END HISTORY COMMENTS */


linus_data_: proc;

/* DESCRIPTION:

   This procedure creates the linus_data_ database.
   


   HISTORY:

   80-02-18   Rickie   E.    Brinegar:   Converted   from  linus_data_.mexp  to
   linus_data_.cds.
   
   80-06-23  Jim  Gray: Modified to make lit_string_size much larger, so that a
   reltion  with  many  large attrs, will not cause "Unable to allocate literal
   string" error messages.
   
   80-06-24 Rickie E.  Brinegar: Modified to reduce Jim's much larger by half.
   
   80-06-24  Jim Gray: Modified to make req_buf_len = 5000, so that linus could
   at least read as much as it can print.
   
   81-06-30 Rickie E.  Brinegar: Changed buf_len to 5000, so that LINUS store,
   modify, write and report have the same buffer size as print.
   
*/

%include cds_args;

dcl 1 ld aligned,
    2 av_id fixed bin (35) init (1),
    2 buff_len fixed bin (35) init (5000),
    2 c_id fixed bin (35) init (2),
    2 chp_id fixed bin (35) init (3),
    2 create_list_id fixed bin (35) init (4),
    2 d_id fixed bin (35) init (5),
    2 dcl_id fixed bin (35) init (6),
    2 dfs_id fixed bin (35) init (7),
    2 ds_id fixed bin (35) init (8),
    2 dltt_id fixed bin (35) init (9),
    2 dtt_id fixed bin (35) init (10),
    2 e_id fixed bin (35) init (11),
    2 eval_expr_id fixed bin (35) init (12),
    2 eval_scal_func_id fixed bin (35) init (13),
    2 eval_set_func_id fixed bin (35) init (14),
    2 h_id fixed bin (35) init (15),
    2 i_id fixed bin (35) init (16),
    2 ldb_id fixed bin (35) init (17),
    2 lila fixed bin (35) init (2),
    2 lila_id fixed bin (35) init (18),
    2 lit_string_size fixed bin (35) init (500000),
    2 lock_wait_time fixed bin (35) init (900),
    2 lrt_id fixed bin (35) init (19),
    2 ls_id fixed bin (35) init (20),
    2 lv_id fixed bin (35) init (21),
    2 m_id fixed bin (35) init (22),
    2 max_expr_items fixed bin (35) init (20),
    2 max_invocs fixed bin (35) init (20),
    2 max_leaf_vals fixed bin (35) init (10),
    2 max_lvars fixed bin (35) init (20),
    2 max_pred_stack_size fixed bin (35) init (20),
    2 max_range_items fixed bin (35) init (20),
    2 max_req_args fixed bin (35) init (100),
    2 max_sclf_items fixed bin (35) init (20),
    2 max_set_stack_size fixed bin (35) init (10),
    2 max_user_items fixed bin (35) init (100),
    2 o_id fixed bin (35) init (23),
    2 p_id fixed bin (35) init (24),
    2 print_col_spaces fixed bin (35) init (2),
    2 report_id fixed bin (35) init (25),
    2 req_buf_len fixed bin (35) init (5000),
    2 req_proc_id fixed bin (35) init (26),
    2 rlb_id fixed bin (35) init (27),
    2 rt_id fixed bin (35) init (28),
    2 s_id fixed bin (35) init (29),
    2 set_id fixed bin (35) init (1),
    2 set_mode_id fixed bin (35) init (30),
    2 sfs_id fixed bin (35) init (31),
    2 srm_id fixed bin (35) init (32),
    2 ss_id fixed bin (35) init (33),
    2 stk_depth fixed bin (35) init (50),
    2 trace_every_n_tuples fixed bin (35) init (1000),
    2 w_id fixed bin (35) init (34),
    2 pr_buff_len fixed bin (35) init (5000);

dcl 1 cdsa like cds_args;

dcl  code fixed bin (35);

dcl (addr,
     null,
     size,
     string) builtin;

dcl  create_data_segment_ entry (ptr, fixed bin (35));
dcl  com_err_ entry options (variable);

	cdsa.sections.p (1) = addr (ld);
	cdsa.sections.len (1) = size (ld);
	cdsa.sections.struct_name (1) = "ld";
	cdsa.seg_name = "linus_data_";
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null;
	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0 then call com_err_ (code, "linus_data_");
	return;

     end linus_data_;
