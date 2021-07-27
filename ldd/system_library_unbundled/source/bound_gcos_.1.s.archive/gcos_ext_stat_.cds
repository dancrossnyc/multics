/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
gcos_ext_stat_:proc;

/* Generate object for "gcos_ext_stat_" data.

   Author:    Dave Ward 1981
   Modified:  Ron Barstad  83-07-21  Remove dependency on ted_com, add include file
 */
/** Initialize cds_args **/

	cds_args_ptr=addr(space_for_cds_args);
	unspec(space_for_cds_args)="0"b;

/** Reference structure input to cds to assure it is in runtime table. **/

	if addr(gcos_ext_stat_)=null() then ;

/** No text section **/
	cds_args_ptr -> cds_args.sections (1).p = null ();
	cds_args_ptr -> cds_args.sections (1).len = 0;
	cds_args_ptr -> cds_args.sections (1).struct_name = "NO_TEXT";

/** Static section **/
	cds_args_ptr -> cds_args.sections (2).p = addr (gcos_ext_stat_);	/* Caller's data. */
	cds_args_ptr -> cds_args.sections (2).len = size (gcos_ext_stat_);	/* No. words in data structure. */
	cds_args_ptr -> cds_args.sections (2).struct_name = "gcos_ext_stat_";

	cds_args_ptr -> cds_args.seg_name = "gcos_ext_stat_";	/* Entryname of object segment. */
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
	      call com_err_ (code, "cds_gcos_ext_stat_");
	   else 
	      call com_err_( 0,"gcos_ext_stat_","Object for gcos_ext_stat_ created [^i words].",size(gcos_ext_stat_));

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
/** This data structure must exactly match that of gcos_ext_stat_.incl.pl1 **/

dcl 1 gcos_ext_stat_ aligned,
      2 abort_reason	char (128) varying,
      2 abort_return	label,
      2 activity_card_num	pic "9999",
      2 activity_name	char(8),
      2 activity_start_time	fixed bin(71),
      2 card_num		pic "9999",
      2 dbs		(36)bit(1),
      2 default_nondollar	char(2),
      2 dir_rings		(3) fixed bin(3),
      2 dpno		char(100) varying,
      2 dpo		char(100) varying,
      2 endfc		char(2),
      2 er		ptr,
      2 etc_filecode	char(2),
      2 gcos_slave_area_seg	ptr,
      2 gf		fixed bin(24),
      2 incode		fixed bin(24),
      2 increment_hold	fixed bin(24),
      2 initial_cpu_time	fixed bin(71),
      2 input_segment_path	char(168) varying,
      2 jcl_warnings	fixed bin(24),
      2 job_cpu_time	fixed bin(71),
      2 job_id		char(18) varying,
      2 job_real_time	fixed bin(71),
      2 last_mme		fixed bin(24),
      2 ldrss		fixed bin(24),
      2 max_activities	fixed bin(24),
      2 max_mem		fixed bin(19),
      2 mme_rtrn		label,
      2 nondollar		char(2),
      2 nongcos		char(2),
      2 normal_return	label,
      2 patchfile_ptr	ptr,
      2 pathname_prefix	char(168)var,
      2 pch		ptr,
      2 pdir		char(168) varying,
      2 prt		ptr,
      2 rs		ptr,
      2 saveseg_ptr		ptr,
      2 save_dir		char(168) varying,
      2 seg_rings		(3) fixed bin(3),
      2 sig_ptr		ptr,
      2 skip_umc		bit(1),
      2 snumb		bit (30) aligned,
      2 sought_label	char(8),
      2 statistics		(3*44) fixed bin(24),
      2 stop_code		fixed bin(24),
      2 storage_limit	fixed bin(19),
      2 sysout_limit	fixed bin(35),
      2 sysout_lines	fixed bin(35),
      2 system_free_pointer	ptr,
      2 tape_buffer_size	fixed bin(35),
      2 temp_dir		char(168) varying ,
      2 temp_seg_ptr	ptr,
      2 termination_code	bit (18),
      2 time_limit		fixed bin(71),
      2 userid		char(12),
      2 validation_level	fixed bin(3),

      2 courtesy_call_control aligned like gcos_ext_stat_$courtesy_call_control,

      2 fct		aligned like gcos_ext_stat_$fct,

      2 save_data		aligned like gcos_ext_stat_$save_data,

      2 mc		like gcos_ext_stat_$mc,

      2 gcos_gtss		like gcos_ext_stat_$gcos_gtss;


%page;
%include gcos_ext_stat_;
%page;
%include cds_args;
end;
