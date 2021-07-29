/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */


/* CV_RTMF - Compile a Resource Type Master File into a Resource Type Description Table. */
/* Ripp'd untimely from the womb of cv_pmf on 03/10/78 by C. D. Tavares */
/* Modified 04/12/79 by CDT for precanonicalization of resource names */
/* Modified 8/81 by M.R. Jordan for Time: open; and change error 19 severity */
/* Modified January 1982 BIM for author changes. */
/* Modified July 1982 by E. N. Kittlitz for [severity] and rtmf suffix. */

/*++

BEGIN
 /Device : <name> ;
   /LEX (2) open [auto_rtde.name = token_value] LEX (2)
     / stmt \
 /Volume : <name> ;
   /LEX (2) open [auto_rtde.name = token_value;
   		auto_rtde.is_volume = "1"b] LEX (2)
     / stmt \
 /<any-token>
   /ERROR (19)
     / RETURN \
 /<no-token>
   /ERROR (19)
     / RETURN \

stmt
 /end ;
   /close LEX (2)
     / RETURN \
 /Device : <name> ;
   /LEX (2) close open [auto_rtde.name = token_value] LEX (2)
     / stmt \
 /Volume : <name> ;
   /LEX (2) close open [auto_rtde.name = token_value;
		    auto_rtde.is_volume = "1"b] LEX (2)
     / stmt \
 /Attribute_domain : ;
   / LEX (3) [auto_rtde.n_defined_attributes = 0]
     / stmt \
 /Attribute_domain :
   /LEX (2) [attr_type = Defined] PUSH (stmt)
     / atts \
 /Limit : open ;
   /LEX (4)
     / stmt \
 /Limit : <decimal-integer> ;
   /LEX (2) [auto_rtde.process_limit = token.Nvalue] LEX (2)
     / stmt \
 /Time : <decimal-integer> , <decimal-integer> ;
   /LEX (2) [auto_rtde.default_time = token.Nvalue] LEX (2)
	  [auto_rtde.max_time = token.Nvalue] LEX (2)
     / stmt \
 /Time : <decimal-integer> ;
   /LEX (2) [auto_rtde.default_time = token.Nvalue] LEX (2)
     / stmt \
/Time : open ;
   /LEX (4) [auto_rtde.default_time = FOREVER]
     / stmt \
 /Advance_notice : none ;
   /LEX (4) [auto_rtde.advance_notice_time = -1]
     / stmt \
 /Advance_notice : <decimal-integer> ;
   /LEX (2) [auto_rtde.advance_notice_time = token.Nvalue] LEX (2)
     / stmt \
 /Manual_clear : <yes_no> ;
   /LEX (2) [auto_rtde.manual_clear = (token_value = "yes")] LEX (2)
     / stmt \
 /Implies : <name> ;
   /LEX (2) [auto_rtde.n_mates = 1;
	   auto_rtde.mates (1) = token_value] LEX (2)
     / stmt \
 /Accepts : ;
   /LEX (3) [auto_rtde.n_mates = 0]
     / stmt \
 /Accepts :
   /LEX (2) [matei = 0]
     / mates \
 /Like : <name> ;
   /LEX (2) [auto_rtde.is_synonym = "1"b;
	   auto_rtde.n_mates = 0;
	   auto_rtde.syn_to = token_value;] LEX (2)
     / stmt \
 /Canonicalizer : ;
   /LEX (3)
     / stmt \
 /Canonicalizer : <canon_virtual_entry> ;
   /LEX (2) [auto_rtde.precanon_proc = token_value;] LEX (2)
     / stmt \
 /Canonicalizer : <any-token>
   /LEX (2)  ERROR (4)  NEXT_STMT
     / stmt \
 /charge_type : <name> ;
   /LEX (2) [auto_rtde.registration_defaults.charge_type = find_charge_type ();
	   auto_rtde.registration_defaults.charge_type_given = "1"b] LEX (2)
     / stmt \
 /potential_attributes : ;
   /LEX (3) [auto_rtde.registration_defaults.potential_attributes_given = "1"b]
     / stmt \
 /potential_attributes :
   /LEX (2) [attr_type = Main_potential_defaults] PUSH (stmt)
	  [auto_rtde.registration_defaults.potential_attributes_given = "1"b]
     / atts \
 /attributes : ;
   /LEX (3) [auto_rtde.registration_defaults.attributes_given = "1"b]
     / stmt \
 /attributes :
   /LEX (2) [attr_type = Main_defaults] PUSH (stmt)
	  [auto_rtde.registration_defaults.attributes_given = "1"b]
     / atts \
 /access_range : <authorization_range> ;
   /LEX (2) [auto_rtde.registration_defaults.aim_range (*) = authorization_values (*);
	   auto_rtde.registration_defaults.aim_range_given = "1"b] LEX (2)
     / stmt \
 /access_range : <any-token>
   /LEX (2) ERROR (17) NEXT_STMT
     / stmt \
 /type : <name> ;
   /LEX (2) [subi, auto_rtde.n_subtypes = auto_rtde.n_subtypes + 1;
	   auto_rtde.subtype_name (subi) = token_value;] LEX (2)
     / subtype \
 /<any-token>
   /ERROR (2)  NEXT_STMT
     / stmt \
 /<no-token>
   /ERROR (1)
     / RETURN \

subtype
 /charge_type : <name> ;
   /LEX (2) [auto_rtde.subtype_defaults (subi).charge_type = find_charge_type ();
   	auto_rtde.subtype_defaults (subi).charge_type_given = "1"b] LEX (2)
     / subtype \
 /potential_attributes : ;
   /LEX (3) [auto_rtde.subtype_defaults (subi).potential_attributes_given = "1"b]
     / subtype \
 /potential_attributes :
   /LEX (2) [attr_type = Subtype_potential_defaults] PUSH (subtype)
	[auto_rtde.subtype_defaults (subi).potential_attributes_given = "1"b]
     / atts \
 /attributes : ;
   /LEX (3) [auto_rtde.subtype_defaults (subi).attributes_given = "1"b]
     / subtype \
 /attributes :
   /LEX (2) [attr_type = Subtype_defaults] PUSH (subtype)
	[auto_rtde.subtype_defaults (subi).attributes_given = "1"b]
     / atts \
 /access_range : <authorization_range> ;
   /LEX (2) [auto_rtde.subtype_defaults (subi).aim_range (*) = authorization_values (*);
   	auto_rtde.subtype_defaults (subi).aim_range_given = "1"b] LEX (2)
     / subtype \
 /access_range : <any-token>
   /LEX (2) ERROR (17) NEXT_STMT
     / stmt \
 /<any-token>
   /
     / stmt \
 /<no-token>
   /ERROR (1)
     / RETURN \

atts
 /<any-token> ,
   /add_attribute LEX (2)
     / atts \
 /<any-token> ;
   /add_attribute LEX (2)
     / STACK_POP \
 /<any-token>
   /ERROR (3) NEXT_STMT
     / STACK_POP \
 /<no-token>
   /ERROR (1)
     / RETURN \

mates
 /<name>
   /[matei, auto_rtde.n_mates = matei + 1;
	auto_rtde.mates (matei) = token_value] LEX
     / matepunct \
 /<any-token>
   /ERROR (4)  NEXT_STMT
     / stmt \
 /<no-token>
   /ERROR (1)
     / RETURN \
matepunct
 /,
   /LEX
     / mates \
 /;
   /LEX
     / stmt \
 /<any-token>
   /ERROR (5) NEXT_STMT
     / stmt \
 /<no-token>
   /ERROR (1)
     / RETURN \

   ++*/

/* format: style4 */
cv_rtmf: procedure;

/* automatic */

dcl  (APstmt, APtoken, areap, rtmfp) ptr;

dcl  1 auto_rtde aligned automatic,
       2 fixed_info like rtde.fixed_info aligned,
       2 mates (6) char (32) aligned,
       2 subtypes (32) like rtde.subtypes aligned;

dcl  1 auto_ctt aligned,
       2 n_charge_types fixed bin,
       2 charge_types (100) char (32);

dcl  authorization_values (2) bit (72) aligned,
     access_ceiling bit (72) aligned;

dcl  attr_type fixed bin,
     (matei, subi) fixed bin,
     dn char (168),
     (supplied_en, rtmf_en, rtdt_en) char (32),
     entry_active bit (1) aligned,
     (i, j, k, n) fixed bin,
     time_now fixed bin (71),
     bitc fixed bin (24),
     target_name char (32),
     volume_ind bit (1),
     ap ptr,
     al fixed bin,
     arg based (ap) char (al),
     (ec, code) fixed bin (35);
dcl  created_table_segment bit (1) aligned;
dcl  (SYSTEM_LOW, SYSTEM_HIGH) bit (72) aligned,
     last_block_ptr pointer;

dcl  got_mandatory (100) bit (1) unaligned,
     mytoken char (256),
     temp_ptr pointer,
     loop_ptr pointer,
     found bit (1) aligned;
dcl  argc fixed bin;
dcl  argx fixed bin;
dcl  fb35 fixed bin (35);

dcl  1 auto_token aligned automatic like token;

/* based */

dcl  bchr char (al) unal based (ap);

/* builtin */

declare  (addr, binary, collate, clock, dimension, divide, index, length,
         null, nullo, offset, pointer, rel, reverse, rtrim,
         size, string, substr, unspec, verify) builtin;

/* conditions */

declare  cleanup condition;

/* entries */

dcl  translator_temp_$get_segment entry (char (*) aligned, ptr, fixed bin (35));
dcl  translator_temp_$release_all_segments entry (ptr, fixed bin (35));

dcl  define_area_ ext entry (pointer, fixed bin (35)),
     ioa_$rsnnl ext entry options (variable),
     convert_authorization_$from_string entry (bit (72) aligned, char (*), fixed bin (35)),
     convert_authorization_$from_string_range entry ((2) bit (72) aligned, char (*), fixed bin (35)),
     system_info_$access_ceiling entry (bit (72) aligned),
     cu_$arg_ptr entry (fixed bin, ptr, fixed bin, fixed bin (35)),
     cv_entry_ ext entry (char (*), pointer, fixed bin (35)) returns (entry),
     get_wdir_ entry () returns (char (168) aligned),
     get_group_id_ entry () returns (char (32) aligned),
     expand_pathname_ entry (char (*), char (*), char (*), fixed bin (35)),
     com_err_ entry options (variable),
     lex_string_$init_lex_delims entry (char (*), char (*), char (*), char (*), char (*), bit (*),
	char (*) var, char (*) var, char (*) var, char (*) var),
     lex_string_$lex entry (ptr, fixed bin, fixed bin, ptr, bit (*), char (*), char (*), char (*), char (*), char (*),
	char (*) var, char (*) var, char (*) var, char (*) var, ptr, ptr, fixed bin (35));

dcl  com_err_$suppress_name entry () options (variable);
dcl  cv_dec_check_ entry (char (*), fixed bin (35)) returns (fixed bin (35));
dcl  hcs_$delentry_seg entry (ptr, fixed bin (35));
dcl  hcs_$set_bc_seg entry (ptr, fixed bin (24), fixed bin (35));
dcl  initiate_file_ entry (char (*), char (*), bit (*), ptr, fixed bin (24), fixed bin (35));
dcl  hcs_$make_seg entry (char (*), char (*), char (*), fixed bin (5), ptr, fixed bin (35));
dcl  terminate_file_ entry (ptr, fixed bin (24), bit (*), fixed bin (35));
dcl  hcs_$truncate_seg entry (ptr, fixed bin, fixed bin (35));
dcl  expand_pathname_$add_suffix entry (char (*), char (*), char (*), char (*), fixed bin (35));
dcl  pathname_ entry (char (*), char (*)) returns (char (168));
dcl  suffixed_name_$new_suffix entry (char (*), char (*), char (*), char (32), fixed bin (35));
dcl  cu_$arg_count entry (fixed bin, fixed bin (35));

/* internal static */

dcl  (first bit (1) initial ("1"b),
     (Defined initial (0),
     Main_potential_defaults initial (1),
     Main_defaults initial (2),
     Subtype_potential_defaults initial (3),
     Subtype_defaults initial (4)) fixed bin,
     my_name char (8) aligned initial ("cv_rtmf"),
     (LEXDLM, LEXCTL) char (128) var,
     BREAKS char (128) var,
     IGBREAKS char (128) var) internal static;

dcl  FOREVER fixed bin initial (4880) static options (constant);

/* external static */

dcl  (error_table_$translation_failed,
     error_table_$badopt,
     error_table_$too_many_args,
     error_table_$seg_not_found,
     error_table_$no_ext_sym,
     error_table_$noentry,
     error_table_$noarg,
     error_table_$bad_conversion,
     error_table_$zero_length_seg) fixed bin (35) external static;
dcl  sys_info$max_seg_size fixed bin (18) external static;
dcl  cv_rtmf_severity_ fixed bin (35) external static init (0);

/* include files */

%include rtdt;
%include rcp_mandatories;
%include area_info;
%include access_mode_values;
%include terminate_file;
dcl  1 auto_area_info automatic like area_info aligned;

/* program */

	rtmfp = null;				/* Initialize for cleanup handler */
	rtdtp = null;				/* .. */
	areap = null;				/* .. */
	rtdep = null;
	dn, supplied_en, rtmf_en, rtdt_en = "";
	created_table_segment = ""b;

	on cleanup begin;
		call clean_up;
		cv_rtmf_severity_ = 5;
	     end;

	call cu_$arg_count (argc, ec);
	if ec ^= 0 then do;
	     call com_err_ (ec, my_name);
	     go to severity_5_failure;
	end;

	if argc = 0 then do;
give_usage:    call com_err_$suppress_name (ec, my_name, "Usage: cv_rtmf RTMF (-brief|-bf|-long|-lg)");
	     go to severity_5_failure;
	end;

	do argx = 1 to argc;
	     call cu_$arg_ptr (argx, ap, al, ec);
	     if character (bchr, 1) ^= "-"
	     then do;
		if supplied_en ^= ""
		then do;
		     call com_err_ (error_table_$too_many_args, my_name, "Only one pathname may be given. ^a was the second.", bchr);
		     go to severity_5_failure;
		end;

		call expand_pathname_ (bchr, dn, supplied_en, ec);
		if ec ^= 0 then do;
path_error:
		     call com_err_ (ec, my_name, "^a", bchr);
		     go to severity_5_failure;
		end;
		call expand_pathname_$add_suffix (bchr, "rtmf", dn, rtmf_en, ec);
		if ec ^= 0 then go to path_error;

		call suffixed_name_$new_suffix (supplied_en, "rtmf", "rtdt", rtdt_en, ec); /* if we get this far, how can we fail? */
		if ec ^= 0			/* still, let's have a look */
		then go to path_error;

	     end;					/* Pathname case */
	     else if bchr = "-bf" then SERROR_CONTROL = "01"b;
	     else if bchr = "-brief" then SERROR_CONTROL = "01"b;
	     else if bchr = "-long" | bchr = "-lg" then SERROR_CONTROL = "10"b;
	     else if bchr = "-severity" | bchr = "-sv" then do;
		if argx >= argc then do;
		     call com_err_ (error_table_$noarg, my_name, "After ""^a"".", bchr);
		     go to severity_5_failure;
		end;
		argx = argx + 1;
		call cu_$arg_ptr (argx, ap, al, ec);
		fb35 = cv_dec_check_ (bchr, ec);
		if ec ^= 0 | fb35 < 0 | fb35 > 5 then do;
		     call com_err_ (error_table_$bad_conversion, my_name,
			"Severity must be an integer in the range 0 - 5, not ""^a"".", bchr);
		     go to severity_5_failure;
		end;
		MIN_PRINT_SEVERITY = fb35;
	     end;
	     else do;
		call com_err_ (error_table_$badopt, my_name, "^a", bchr);
		go to severity_5_failure;
	     end;
	end;					/* argument loop */

	if supplied_en = "" then go to give_usage;

	call system_info_$access_ceiling (access_ceiling);
	time_now = clock;
	call initiate_file_ (dn, rtmf_en, R_ACCESS, rtmfp, bitc, ec);
	if ec = error_table_$noentry
	then if rtmf_en ^= supplied_en
	     then do;
		call initiate_file_ (dn, supplied_en, R_ACCESS, rtmfp, bitc, ec);
		if ec = 0
		then do;
		     call com_err_ (0, my_name, "Warning: converting ^a. The segment should be named ^a.",
			pathname_ (dn, supplied_en), rtmf_en);
		     rtmf_en = supplied_en;
		end;
	     end;
	if ec ^= 0
	then do;
rtmf_error:
	     call com_err_ (ec, my_name, "^a.", pathname_ (dn, rtmf_en));
	     go to severity_5_failure;
	end;

	n = divide (bitc + 8, 9, 24, 0);
	if n = 0 then do;
	     ec = error_table_$zero_length_seg;
	     go to rtmf_error;
	end;

	dn = get_wdir_ ();
	call hcs_$make_seg (dn, rtdt_en, "", 1010b, rtdtp, ec);
	created_table_segment = (ec = 0);
	if rtdtp = null then do;
rtdt_error:
	     call com_err_ (ec, my_name, "^a", pathname_ (dn, rtdt_en));
	     go to severity_5_failure;
	end;

	call hcs_$truncate_seg (rtdtp, 0, ec);
	if ec ^= 0 then go to rtdt_error;

	rtdt.author.proc_group_id = get_group_id_ ();	/* Initialize the header of the new rtdt */
	rtdt.author.table = "RTDT";
	rtdt.author.w_dir = dn;
	rtdt.author.lock = ""b;
	rtdt.author.last_install_time = 0;

	rtdt.version = RTDT_version_3;
	rtdt.charge_type_table_ptr, rtdt.first_resource = nullo;

	last_block_ptr = null;

	RTDT_area_len = 0;
	RTDT_area_len = sys_info$max_seg_size - size (rtdt); /* "clever" in the worst sense of the word */

	unspec (auto_area_info) = ""b;
	auto_area_info.version = area_info_version_1;
	auto_area_info.no_freeing, auto_area_info.dont_free = "1"b;
	auto_area_info.owner = my_name;
	auto_area_info.size = RTDT_area_len;
	auto_area_info.areap = addr (rtdt.rtdt_area);

	call define_area_ (addr (auto_area_info), ec);
	if ec ^= 0 then goto rtdt_error;

	auto_ctt.n_charge_types = 0;

	call convert_authorization_$from_string (SYSTEM_LOW, "system_low", code);
	if code ^= 0 then goto badacc;
	call convert_authorization_$from_string (SYSTEM_HIGH, "system_high", code);
	if code ^= 0 then do;
badacc:	     call com_err_ (code, "cv_rtmf", "While converting canned access classes.");
	     go to severity_5_failure;
	end;

	call translator_temp_$get_segment (my_name, areap, ec);
	if ec ^= 0 then do;
	     call com_err_ (ec, my_name, "Getting temporary segment.");
	     go to severity_5_failure;
	end;

	if first then do;
	     BREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24) || "(),.:;";
	     IGBREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24);
	     call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b, BREAKS, IGBREAKS, LEXDLM, LEXCTL);
	     first = "0"b;
	end;

	entry_active = ""b;

	call lex_string_$lex (rtmfp, n, 0, areap, "100"b,
	     """", """", "/*", "*/", ";", BREAKS, IGBREAKS,
	     LEXDLM, LEXCTL, APstmt, APtoken, ec);

	Pthis_token = APtoken;
	call SEMANTIC_ANALYSIS ();

abort:	if MERROR_SEVERITY > 2 then do;
	     call com_err_ (error_table_$translation_failed, my_name, rtmf_en);
	     if created_table_segment then bitc = -1;	/* delete it it we made the branch */
	     bitc = 0;				/* otherwise just zap it */
	end;
	else do;
	     N_CHARGE_TYPES = auto_ctt.n_charge_types;
	     allocate charge_type_table in (rtdt_area);

	     unspec (charge_type_table) = unspec (auto_ctt);
	     rtdt.charge_type_table_ptr = offset (cttp, rtdt.rtdt_area);

/* Now perform a number of "gullibility checks" on each RTDE. */

	     Ptoken = addr (auto_token);		/* hate to diddle with RDC databases, but... */
	     Pthis_token = Ptoken;
	     Pstmt, token.Pstmt = null;
	     token.line_no = 0;
	     token.Pvalue = addr (mytoken);

	     got_mandatory (*) = ""b;

	     do loop_ptr = pointer (rtdt.first_resource, rtdt.rtdt_area)
		repeat (pointer (loop_ptr -> rtde.next_resource, rtdt.rtdt_area))
		while (loop_ptr ^= null);

		rtdep = loop_ptr;
		if rtde.is_synonym then do;
		     mytoken = rtde.name;
		     target_name = rtde.syn_to;
		     volume_ind = rtde.is_volume;
		     found = ""b;

		     do rtdep = pointer (rtdt.first_resource, rtdt.rtdt_area)
			repeat (pointer (rtde.next_resource, rtdt.rtdt_area))
			while (rtdep ^= null & ^found);
			if rtde.name = target_name then do;
			     found = "1"b;
			     if rtde.is_synonym then call ERROR (Syn_chain);
			     if rtde.is_volume ^= volume_ind then
				call ERROR (Device_syn_volume);
			end;
		     end;

		     if ^found then call ERROR (Syn_undefined); /* fell thru loop, lose. */
		end;

		else do;

/* Check to see if this is a mandatory resource name */

		     do i = 1 to dimension (rcp_mandatories.resource_name, 1)
			while (rtde.name ^= rcp_mandatories.resource_name (i));
		     end;

		     if i <= dimension (rcp_mandatories.resource_name, 1) then do;

			got_mandatory (i) = "1"b;

/* Make sure all mandatory attributes for this resource name are defined. */

			do j = 1 to dimension (rcp_mandatories.attributes, 2)
			     while (rcp_mandatories.attributes (i, j) ^= "");

			     do k = 1 to rtde.n_defined_attributes
				while (rtde.attribute_names (k) ^= rcp_mandatories.attributes (i, j));
			     end;

			     if k > rtde.n_defined_attributes then do;
				call ioa_$rsnnl ("""^a"" for ""^a""", mytoken,
				     token.Lvalue, rcp_mandatories.attributes (i, j), rtde.name);
				call ERROR (Mandatory_attr_missing);
			     end;
			end;
		     end;

/* Now make sure the mates for this resource are "consenting". */

		     do i = 1 to rtde.n_mates;

			found = ""b;

			do temp_ptr = pointer (rtdt.first_resource, rtdt.rtdt_area)
			     repeat (pointer (temp_ptr -> rtde.next_resource, rtdt.rtdt_area))
			     while ((temp_ptr ^= null) & ^found);

			     if rtde.mates (i) = temp_ptr -> rtde.name then do;
				do j = 1 to temp_ptr -> rtde.n_mates
				     while (temp_ptr -> rtde.mates (j) ^= rtde.name);
				end;

				if j > temp_ptr -> rtde.n_mates then do;
				     call ioa_$rsnnl ("""^a"" and ""^a""", mytoken,
					token.Lvalue, temp_ptr -> rtde.name, rtde.name);
				     call ERROR (Unrequited_mating);
				end;

				else if rtde.is_volume = temp_ptr -> rtde.is_volume then do;
				     call ioa_$rsnnl ("""^a"" and ""^a""", mytoken,
					token.Lvalue, temp_ptr -> rtde.name, rtde.name);
				     call ERROR (Mates_same_type);
				end;

				else found = "1"b;
			     end;
			end;

			if ^found then do;
			     call ioa_$rsnnl ("""^a"" for ""^a""", mytoken,
				token.Lvalue, rtde.mates (i), rtde.name);
			     call ERROR (No_such_mate);
			end;
		     end;
		end;
	     end;

	     do i = 1 to dimension (rcp_mandatories.resource_name, 1);
		if ^got_mandatory (i) then do;
		     mytoken = rcp_mandatories.resource_name (i);
		     token.Lvalue = length (rtrim (mytoken, " "));
		     call ERROR (Mandatory_resource_missing);
		end;
	     end;

/* Well, it SEEMS kosher enough, close it out. */

	     bitc = (binary (rel (addr (charge_type_table.flagword))) + 1) * 36;
	end;

	if bitc >= 0 then do;
	     call terminate_file_ (rtdtp, bitc, TERM_FILE_TRUNC_BC_TERM, ec);
	     if ec ^= 0 then do;
		call com_err_ (ec, my_name, "Unable to set bitcount on ^a to ^d.", pathname_ (dn, rtdt_en), bitc);
		go to severity_5_failure;
	     end;
	end;

	call clean_up;
	cv_rtmf_severity_ = MERROR_SEVERITY;
	return;

severity_5_failure:
	call clean_up;
	cv_rtmf_severity_ = 5;
	return;
%page;
clean_up:
     procedure;

	if rtmfp ^= null
	then call terminate_file_ (rtmfp, (0), TERM_FILE_TERM, (0));

	if areap ^= null
	then call translator_temp_$release_all_segments (areap, (0));

	if rtdtp ^= null
	then if created_table_segment
	     then do;
		call hcs_$delentry_seg (rtdtp, (0));
		rtdtp = null;
	     end;
	call terminate_file_ (rtdtp, (0), TERM_FILE_TRUNC_BC_TERM, (0));

     end clean_up;

/* SYNTAX FUNCTIONS */

yes_no: proc returns (bit (1) aligned);

	return (token_value = "yes" | token_value = "no");
     end yes_no;


authorization_range:
     procedure () returns (bit (1) aligned);

	call convert_authorization_$from_string_range (authorization_values, token_value, code);
	return (code = 0);

     end authorization_range;


canon_virtual_entry: proc () returns (bit (1) aligned);

dcl  entryvar entry variable;

	entryvar = cv_entry_ (rtrim (token_value), null, code);
	if (code = error_table_$seg_not_found) | (code = error_table_$no_ext_sym) then do; /* not fatal err, but warn him */
	     call ERROR (Canonicalizer_nonexistent);
	     code = 0;
	end;

	return (code = 0);

     end canon_virtual_entry;

/* SEMANTIC FUNCTIONS */

open: proc;

	unspec (auto_rtde) = ""b;
	auto_rtde.name,
	     auto_rtde.precanon_proc,
	     auto_rtde.attribute_names,
	     auto_rtde.mates (*),
	     auto_rtde.subtype_name (*) = "";

	auto_rtde.n_defined_attributes,
	     auto_rtde.process_limit,
	     auto_rtde.n_mates,
	     auto_rtde.advance_notice_time = -1;

	auto_rtde.default_time,
	     auto_rtde.max_time = FOREVER;		/* if that's not enough, hang it up. */

	entry_active = "1"b;
	return;
     end open;


find_charge_type: proc returns (fixed bin);

dcl  i fixed bin;

	do i = 1 to auto_ctt.n_charge_types while (auto_ctt.charge_types (i) ^= token_value);
	end;

	if i > auto_ctt.n_charge_types then do;
	     auto_ctt.n_charge_types = i;
	     auto_ctt.charge_types (i) = token_value;
	end;

	return (i);

     end find_charge_type;

add_attribute: proc;

dcl  implies bit (1) aligned,
     i fixed bin,
     copy_token char (12) aligned;

	if substr (token_value, 1, 1) = "^" then do;
	     call ERROR (Negative_attr);
	     return;
	end;

	if substr (token_value, length (token_value), 1) = "*" then implies = "1"b;
	else implies = ""b;

	if length (token_value) > 12 + (binary (implies)) then call ERROR (Attr_too_long);

	if implies then copy_token = substr (token_value, 1, length (token_value) - 1);
	else copy_token = token_value;

	do i = 1 to auto_rtde.n_defined_attributes while (auto_rtde.attribute_names (i) ^= copy_token);
	end;

	if attr_type ^= Defined then
	     if i > auto_rtde.n_defined_attributes then call ERROR (Undefined_attr);
	     else ;
	else if i <= auto_rtde.n_defined_attributes then do;
	     call ERROR (Multiply_dcl_attr);
	     return;
	end;

	if attr_type = Defined then do;
	     auto_rtde.attribute_names (i) = copy_token;
	     if auto_rtde.is_volume then
		substr (auto_rtde.attributes_to_match, i, 1) = implies;
	     else if implies then call ERROR (Starred_attribute);
	     substr (auto_rtde.attributes_valid, i, 1) = "1"b;
	     auto_rtde.n_defined_attributes = i;
	     return;
	end;

	else if implies then call ERROR (Starred_attribute);

	if attr_type = Main_potential_defaults then
	     substr (auto_rtde.registration_defaults.potential_attributes, i, 1) = "1"b;
	else if attr_type = Main_defaults then
	     substr (auto_rtde.registration_defaults.attributes, i, 1) = "1"b;
	else if attr_type = Subtype_potential_defaults then
	     substr (auto_rtde.subtype_defaults (subi).potential_attributes, i, 1) = "1"b;
	else substr (auto_rtde.subtype_defaults (subi).attributes, i, 1) = "1"b;
	return;

     end add_attribute;

close: proc;

dcl  (j, prefidx) fixed bin;

dcl  save_Lvalue fixed bin (21),
     save_Pvalue pointer,
     fake_token_value char (128);

dcl  1 effective_flags like rtde.registration_defaults.default_flags aligned automatic;
dcl  error_table_$out_of_sequence fixed bin (35) external static,
     sub_err_ ext entry options (variable);


	if ^entry_active then			/* close called before open */
	     call sub_err_ (error_table_$out_of_sequence, "cv_rtmf", "s", null, 0,
		"Attempt to use an undefined RTDE.");
	if auto_rtde.name = "" then call ERROR (No_name);

	save_Lvalue = token.Lvalue;
	save_Pvalue = token.Pvalue;
	token.Pvalue = addr (auto_rtde.name);
	token.Lvalue = length (rtrim (auto_rtde.name, " "));
						/* so error messages make sense */

	if ^auto_rtde.is_synonym then do;		/* don't bother checking syns */
	     if auto_rtde.n_mates = -1 then do;
		call ERROR (No_mates);
		auto_rtde.n_mates = 0;
	     end;
	     if auto_rtde.n_defined_attributes = -1 then call ERROR (No_Domain_stmt);
	     if auto_rtde.n_defined_attributes = 0 then
		auto_rtde.registration_defaults.potential_attributes_given = "1"b;

	     effective_flags = auto_rtde.registration_defaults.default_flags;

	     if auto_rtde.n_subtypes = 0 then call validate_type (auto_rtde.registration_defaults, effective_flags);

	     else do i = 1 to auto_rtde.n_subtypes;

		call ioa_$rsnnl ("""^a"" in ""^a""", fake_token_value, 0,
		     auto_rtde.subtypes (i).subtype_name, auto_rtde.name);
		token.Pvalue = addr (fake_token_value);
		token.Lvalue = length (rtrim (fake_token_value, " "));

		call validate_type (auto_rtde.subtype_defaults (i), effective_flags);
	     end;

/* Now construct the exclusion specs for all attributes of the type "name=". */

	     auto_rtde.n_exclusion_specs = 0;

	     do i = 1 to auto_rtde.n_defined_attributes;

		prefidx = index (auto_rtde.attribute_names (i), "=");

		if prefidx > 0 then do;

		     do j = 1 to auto_rtde.n_exclusion_specs
			while (substr (auto_rtde.exclusion_specs (j), i, 1) = ""b);
		     end;

		     if j > auto_rtde.n_exclusion_specs then do;
			auto_rtde.n_exclusion_specs = auto_rtde.n_exclusion_specs + 1;

			do j = i to auto_rtde.n_defined_attributes;
			     if substr (auto_rtde.attribute_names (i), 1, prefidx)
				= substr (auto_rtde.attribute_names (j), 1, prefidx) then
				substr (auto_rtde.exclusion_specs (auto_rtde.n_exclusion_specs), j, 1) = "1"b;
			end;
		     end;
		end;
	     end;
	end;

	N_MATES = auto_rtde.n_mates;
	N_SUBTYPES = auto_rtde.n_subtypes;

	allocate rtde in (rtdt.rtdt_area);

	unspec (rtde.fixed_info) = unspec (auto_rtde.fixed_info);
	unspec (rtde.mates) = unspec (auto_rtde.mates);
	unspec (rtde.subtypes) = unspec (auto_rtde.subtypes);

	rtde.next_resource = nullo;
	rtde.valid = "1"b;

	if last_block_ptr = null then rtdt.first_resource = offset (rtdep, rtdt.rtdt_area);
	else last_block_ptr -> rtde.next_resource = offset (rtdep, rtdt.rtdt_area);

	last_block_ptr = rtdep;

	token.Pvalue = save_Pvalue;
	token.Lvalue = save_Lvalue;

	rtdep = null;				/* catches bugs */

	return;

validate_type: proc (arg_struc, flag_struc);

dcl  1 arg_struc parameter like rtde.registration_defaults aligned;

dcl  1 flag_struc parameter like rtde.registration_defaults.default_flags aligned;

dcl  1 temp_flags like rtde.registration_defaults.default_flags aligned automatic;

	     temp_flags = effective_flags | arg_struc.default_flags;

	     if ^string (temp_flags) = ""b then return;	/* all are given */

	     if auto_rtde.n_defined_attributes > 0 then do;
		if ^temp_flags.potential_attributes_given then
		     call ERROR (No_potential_attributes);
		if ^temp_flags.attributes_given then
		     call ERROR (No_attributes);
	     end;
	     if ^temp_flags.aim_range_given then do;
		call ERROR (No_aim_range);
		arg_struc.aim_range = SYSTEM_LOW;
	     end;
	     if ^temp_flags.charge_type_given then call ERROR (No_charge_type);

	end validate_type;
     end close;

dcl  (Premature_EOF initial (1),
     Unrecognized initial (2),
     Bad_attribute_syntax initial (3),
     Bad_identifier initial (4),
     Bad_syntax initial (5),
     No_name initial (6),
     No_potential_attributes initial (7),
     No_mates initial (8),
     No_attributes initial (9),
     No_aim_range initial (10),
     Canonicalizer_nonexistent initial (11),
     No_charge_type initial (12),
     Undefined_attr initial (13),
     Multiply_dcl_attr initial (14),
     Negative_attr initial (15),
     Attr_too_long initial (16),
     Bad_access_class initial (17),
     No_Domain_stmt initial (18),
     Bad_first_stmt initial (19),
     Starred_attribute initial (20),
     Mandatory_attr_missing initial (21),
     Mandatory_resource_missing initial (22),
     Unrequited_mating initial (23),
     Mates_same_type initial (24),
     No_such_mate initial (25),
     Syn_undefined initial (26),
     Syn_chain initial (27),
     Device_syn_volume initial (28)) fixed bin static options (constant);

dcl  1 error_control_table (28) aligned internal static options (constant),
       2 severity fixed bin (17) unal init (4, (5) 3, (3) 1, 3, 1, (2) 3, 1, 3, (3) 3, 4, 1, (8) 3),
       2 Soutput_stmt bit (1) unaligned initial
	  ("0"b, (5) (1)"1"b, (4) (1)"0"b, "1"b, "0"b, (5) (1)"1"b, "0"b, (2) (1)"1"b, (8) (1)"0"b),
       2 message char (100) var init
	  ("Premature end of RTMF encountered.",	/* 1 */
	  "Unrecognized keyword or invalid punctuation in recognized statement.", /* 2 */
	  "Improper syntax in attribute string.",	/* 3 */
	  "Improper identifier ""^a"" in statement.",	/* 4 */
	  "Improper syntax in statement.",		/* 5 */
	  "No resource name supplied.",		/* 6 */
	  "No potential attributes supplied for ^a.",	/* 7 */
	  "No Implies or Accepts statement for ^a.",	/* 8 */
	  "No attributes statement supplied for ^a; assuming null list.", /* 9 */
	  "No access range supplied for ^a-- assuming ""system_low : system_low"".", /* 10 */
	  "Specified canonicalization procedure does not seem to exist.", /* 11 */
	  "No charge type supplied for ^a.",		/* 12 */
	  "Default attribute ""^a"" not defined in ""Attributes"" statement.", /* 13 */
	  "Attribute ""^a"" has been multiply declared.", /* 14 */
	  "Negated attributes such as ""^a"" serve no useful purpose.", /* 15 */
	  "Attribute name ""^a"" is too long.",		/* 16 */
	  "Unrecognized access class ""^a"".",		/* 17 */
	  "No Attribute_domain statement for ^a.",	/* 18 */
	  "First statement is not Volume or Device statement.", /* 19 */
	  "Asterisk is only meaningful in volume potential attribute list.", /* 20 */
	  "Mandatory attribute ^a not defined.",	/* 21 */
	  "Mandatory resource ^a not defined.",		/* 22 */
	  "Resources ^a mate in one direction but not in the other.", /* 23 */
	  "Mating resources ^a are both devices or both volumes.", /* 24 */

	  "Undefined mate ^a.",			/* 25 */
	  "Synonym reference for ^a is undefined.",	/* 26 */
	  "Synonym ^a refers to another synonym.",	/* 27 */
	  "Device and volume cannot be synonymous-- ^a"), /* 28 */
       2 brief_message char (30) var init
	  ("Premature EOF.",			/* 1 */
	  "Unrecognizable.",			/* 2 */
	  "Syntax.",				/* 3 */
	  "",					/* 4 */
	  "Syntax.",				/* 5 */
	  "No resource name.",			/* 6 */
	  (12) (1)"^a.",				/* 7-18 */
	  "No Device/Volume stmt.",			/* 19 */
	  "Meaningless asterisk.",			/* 20 */
	  (8) (1)"^a.");				/* 21-28 */

/* ======================================================== */
