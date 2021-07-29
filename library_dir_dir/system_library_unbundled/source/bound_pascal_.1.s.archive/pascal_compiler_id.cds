/* *************************************************************************
   *                                                                       *
   * Copyright, (C) BULL HN Information Systems Inc., 1990                 *
   *                                                                       *
   * Copyright (c) 1980 by Centre Interuniversitaire de Calcul de Grenoble *
   * and Institut National de Recherche en Informatique et Automatique     *
   *                                                                       *
   ************************************************************************* */

/* HISTORY COMMENTS:
  1) change(86-10-09,JMAthane), approve(86-10-09,MCR7521),
     audit(86-10-09,JPFauche), install(86-11-12,MR12.0-1208):
     Pascal Version 8.03 for MR12.0.
  2) change(90-04-30,Zimmerman), approve(90-04-30,MCR8170),
     audit(90-05-03,Huen), install(90-05-30,MR12.4-1013):
     Pascal Version 8.04a for MR12.4. (pas_14,15; MCR8167-pas_10,11,13)
                                                   END HISTORY COMMENTS */

pascal_compiler_id: proc;

%include cds_args;

	dcl     code		 fixed bin (35);

	dcl     1 my_args		 like cds_args;
	dcl     com_err_		 entry options (variable);
	dcl     create_data_segment_	 entry (ptr, fixed bin (35));


	dcl     (null, string)	 builtin;

	dcl     1 compiler_id,
		2 version		 char (32) varying 
				 init ("Pascal 8.04a"),
		2 version_number	 fixed bin init (2),
						/* 0 : until PASCAL 5.02
1 : for PASCAL 6.xx
2 : since PASCAL 7.xx
*/
		2 gen_id		 char (100) var init
				 ("Grenoble University Multics Pascal Compiler, Release 8.04 of April the 30th, 1990");

%page;
	string (my_args.switches) = "0"b;
	my_args.switches.have_static = "1"b;
	my_args.num_exclude_names = 0;
	my_args.seg_name = "pascal_compiler_id";
	my_args.p (2) = addr (compiler_id);
	my_args.len (2) = size (compiler_id);
	my_args.struct_name (2) = "compiler_id";

	call create_data_segment_ (addr (my_args), code);
	if code ^= 0 then call com_err_ (code);

     end;

