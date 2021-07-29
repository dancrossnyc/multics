/* *************************************************************************
   *                                                                       *
   * Copyright (c) 1980 by Centre Interuniversitaire de Calcul de Grenoble *
   * and Institut National de Recherche en Informatique et Automatique     *
   *                                                                       *
   ************************************************************************* */


/* HISTORY COMMENTS:
  1) change(86-10-09,JMAthane), approve(86-10-09,MCR7521),
     audit(86-10-09,JPFauche), install(86-11-12,MR12.0-1212):
     Pascal Version 8.03 for MR12.0.
  2) change(88-01-12,Martinson), approve(88-01-12,MCR7823),
     audit(88-01-28,Fawcett), install(88-02-02,MR12.2-1020):
     Fix infinite loop problem when pascal_error_ is signalled within a pascal
     program.
                                                   END HISTORY COMMENTS */


pascal_operators_statics_: proc;

/* procedure to generate
   - implicit input and output fsb
   - pascal_error_info structure
   - pascal_modes data */

%include condition_info_header;
%include pascal_fsb;
%include cds_args;

	dcl     i			 fixed bin;
	dcl     code		 fixed bin (35);

	dcl     1 my_args		 like cds_args;
	dcl     com_err_		 entry options (variable);
	dcl     create_data_segment_	 entry (ptr, fixed bin (35));

	dcl     buffer_length	 fixed bin (21) int static options (constant) init (256);

	dcl     (addr, null, size)	 builtin;

	dcl     1 operators_statics,
		2 first_file_ptr	 ptr,
		2 INPUT,
		  3 tff		 like text_fsb_fix,
		  3 input_buffer	 char (buffer_length),
		2 pad_ptr_1	 ptr,		/* dword boundary ! */
		2 OUTPUT,
		  3 tff		 like text_fsb_fix,
		  3 output_buffer	 char (buffer_length),
		2 pad_ptr_2	 ptr,		/* dword boundary ! */
		2 ERROR,
		  3 tff		 like text_fsb_fix,
		  3 error_buffer	 char (buffer_length),
		2 pad_ptr_3	 ptr,		/* dword boundary ! */
		2 ENTREE,
		  3 tff		 like text_fsb_fix,
		  3 entree_buffer	 char (buffer_length),
		2 pad_ptr_4	 ptr,		/* dword boundary ! */
		2 SORTIE,
		  3 tff		 like text_fsb_fix,
		  3 sortie_buffer	 char (buffer_length),
		2 pad_ptr_5	 ptr,		/* dword boundary ! */
		2 ERREUR,
		  3 tff		 like text_fsb_fix,
		  3 erreur_buffer	 char (buffer_length),
		2 error_info like condition_info_header,
		2 fast_mode	 bit (1) aligned,
		2 io_warnings	 bit (1) aligned,
		2 area_warnings	 bit (1) aligned,
		2 area_operators_info,
		  3 nbr_of_areas	 fixed bin (8) unsigned unal,
		  3 aoc		 bit (3) unal,
		  3 term_cond	 bit (7) unal,
		  3 last_structure_offset fixed bin (18) unsigned unal,
		2 areas		 (64),
		  3 unique_id	 fixed bin (71),
		  3 pathname	 char (168) varying,
		  3 area_size	 fixed bin (35),
		  3 area_ptr	 ptr,
		  3 nbr_of_area_segs fixed bin (35),
		  3 new_warning	 fixed bin (35);

/*  */
	first_file_ptr = null;

	fsb_ptr = addr (INPUT);

	allocated_size = 0;
	global_flags = "0"b;
	permanent_file = "1"b;
	standard_file = "1"b;
	SOL_fstatus, SOL_fpos = 0;
	uid = "0"b;
	next_fsb_ptr, system_ptr = null;
	file_name = "input";
	file_is_text = 1;

	fsb_ptr = addr (OUTPUT);

	allocated_size = 0;
	global_flags = "0"b;
	permanent_file = "1"b;
	standard_file = "1"b;
	SOL_fstatus, SOL_fpos = 0;
	uid = "0"b;
	next_fsb_ptr, system_ptr = null;
	file_name = "output";
	file_is_text = 1;

	fsb_ptr = addr (ERROR);

	allocated_size = 0;
	global_flags = "0"b;
	permanent_file = "1"b;
	standard_file = "1"b;
	SOL_fstatus, SOL_fpos = 0;
	uid = "0"b;
	next_fsb_ptr, system_ptr = null;
	file_name = "error";
	file_is_text = 1;

	fsb_ptr = addr (ENTREE);

	allocated_size = 0;
	global_flags = "0"b;
	permanent_file = "1"b;
	standard_file = "1"b;
	SOL_fstatus, SOL_fpos = 0;
	uid = "0"b;
	next_fsb_ptr, system_ptr = null;
	file_name = "entree";
	file_is_text = 1;

	fsb_ptr = addr (SORTIE);

	allocated_size = 0;
	global_flags = "0"b;
	permanent_file = "1"b;
	standard_file = "1"b;
	SOL_fstatus, SOL_fpos = 0;
	uid = "0"b;
	next_fsb_ptr, system_ptr = null;
	file_name = "sortie";
	file_is_text = 1;

	fsb_ptr = addr (ERREUR);

	allocated_size = 0;
	global_flags = "0"b;
	permanent_file = "1"b;
	standard_file = "1"b;
	SOL_fstatus, SOL_fpos = 0;
	uid = "0"b;
	next_fsb_ptr, system_ptr = null;
	file_name = "erreur";
	file_is_text = 1;

	INPUT.buff_len_, OUTPUT.buff_len_, ERROR.buff_len_,
	     ENTREE.buff_len_, SORTIE.buff_len_, ERREUR.buff_len_ = buffer_length;

	error_info.version = 1;
	error_info.length = 69 /* size (error_info) */;
	error_info.action_flags = "0"b;
	error_info.cant_restart = "1"b;
	error_info.default_restart = "0"b;
	error_info.quiet_restart = "0"b;
	error_info.info_string = "";
	error_info.status_code = 0;
	fast_mode = "0"b;
	io_warnings = "1"b;
	area_warnings = "0"b;


	nbr_of_areas = 1;
	aoc = "100"b;
	term_cond = "1000000"b;			/* tze */
	last_structure_offset = 63 * 50;
	do i = 1 to 64;
	     unique_id (i) = -1;
	     pathname (i) = "";
	     area_size (i) = 255;
	     area_ptr (i) = null;
	     nbr_of_area_segs (i), new_warning (i) = 0;
	end;
	unique_id (64) = 0;				/* Standard area */

	my_args.have_text, my_args.separate_static = "0"b;
	my_args.have_static = "1"b;
	my_args.switches.pad = "0"b;
	my_args.num_exclude_names = 0;
	my_args.seg_name = "pascal_operators_statics_";
	my_args.p (2) = addr (operators_statics);
	my_args.len (2) = size (operators_statics);
	my_args.struct_name (2) = "operators_statics";

	call create_data_segment_ (addr (my_args), code);
	if code ^= 0 then call com_err_ (code);

     end;

