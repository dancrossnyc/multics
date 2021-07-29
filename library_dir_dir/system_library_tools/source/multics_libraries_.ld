/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(1986-12-18,Fawcett), approve(1986-12-18,PBF7517),
     audit(1986-12-18,GDixon), install(1987-01-05,MR12.0-1257):
     Remove BOS for MR12.0.  Upgrade to reflect earlier changes to the
     libraries.
  2) change(2021-03-10,GDixon), approve(2021-03-10,MCR10087),
     audit(2021-03-17,Swenson), install(2021-03-17,MR12.6g-0050):
      A) Add Root for >doc>obs directory.
      B) Remove all obsolete_library roots from: online_library online and ""
         library groups.  obsolete_library roots must be selected by
         explicitly giving (obsolete_library obsolete obs) LIBRARY name; or
         (obsolete_library obsolete obs).DIRECTORY where DIRECTORY can be:
                source object execution info
      C) Move obsolete_library entries to very end of the Root list.  When
         selecting from multiple libraries (e.g., -lb *), items in
         obsolete_library are seldom desired, and should be listed as final
         selection.
      D) Add default -library obsolete_library  for library_cleanup command.
      E) Add short name obs for obsolete_library.
                                                   END HISTORY COMMENTS */


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/* Name:  multics_libraries_.ld						*/
	/*									*/
	/*      This segment contains a description of the Multics System Libraries.  The 	*/
	/* description is in the library descriptor language, which can be compiled by the	*/
	/* library_descriptor_compiler (ldc) command into an ALM source segment, which can be	*/
	/* assembled by the alm command into a binary data base object segment.		*/
	/*									*/
	/* Status									*/
	/*									*/
	/* 0) Created:   September, 1974 by G. C. Dixon					*/
	/* 1) Modified:  March, 1975 by G. C. Dixon					*/
	/* 2) Modified:  November, 1975 by G. C. Dixon					*/
	/* 3) Modified:  August, 1979 by G. C. Dixon					*/
	/* 4) Modified:  April, 1981 by P. B. Kelley - Reorganized Hardcore libraries		*/
	/* 5) Modified:  June, 1981 by R. A. Holmstedt - changes the communications  libraries    */
	/* 6) Modified:  Sept, 1981 by R. A. Holmstedt - Change hardcore to an online library.    */
	/* 7) Modified:  March, 1982 by E. N. Kittlitz - eliminate >ldd>include_2.		*/
	/* 8) Modified:  September, 1982 by W. Olin Sibert - eliminate "lists" libraries	*/
	/* 9) Modified:  October, 1983 by Jim Lippard - move obsolete to the end of the list	*/
	/*10) Modified:  Jan, 1985 by R. A. Holmstedt - Removed the network library and moved the */
	/*               lang library into the sss library.				*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

Descriptor:		multics_libraries_;


Define:			commands;
     command:		library_info;
	library name:	online_libraries supervisor_library;
     command:		library_map;
	library names:	online_libraries.source online_libraries.object online_libraries.execution;
	search name:	**;
     command:		library_print;
	library name:	info;
	search name:	**.*.info;
     command:		library_fetch;
	library names:	online_libraries.source
			supervisor_library.source;
     command:		library_cleanup;
	library name:	online_libraries offline_libraries obsolete_library;
	search name:	!??????????????;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


Root:			(standard_library std sss online_libraries online on "").(source s "");
     path:		>ldd>sss>source;
     search procedure:	multics_library_search_$source_dirs;

Root:			(standard_library std sss online_libraries online on "").(object o "");
     path:		>ldd>sss>object;
     search procedure:	multics_library_search_$object_dirs;

Root:			(standard_library std sss online_libraries online on "").(listings);
     path:		>ldd>listings>sss;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(standard_library std sss online_libraries online on ""
			 languages_library lang                                ).(execution x "");
     path:		>sss;
     search procedure:	multics_library_search_$execution_dirs;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


Root:			(unbundled_library unb online_libraries online on "").(source s "");
     path:		>ldd>unb>source;
     search procedure:	multics_library_search_$source_dirs;

Root:			(unbundled_library unb online_libraries online on "").(object o "");
     path:		>ldd>unb>object;
     search procedure:	multics_library_search_$object_dirs;

Root:			(unbundled_library unb online_libraries online on "").(listings);
     path:		>ldd>listings>unbundled_1;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(unbundled_library unb online_libraries online on "").(listings);
     path:		>ldd>listings>unbundled_2;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(unbundled_library unb online_libraries online on "").(execution x "");
     path:		>unb;
     search procedure:	multics_library_search_$execution_dirs;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


Root:			(tools_library tools t online_libraries online on "").(source s "");
     path:		>ldd>tools>source;
     search procedure:	multics_library_search_$source_dirs;

Root:			(tools_library tools t online_libraries online on "").(object o "");
     path:		>ldd>tools>object;
     search procedure:	multics_library_search_$object_dirs;

Root:			(tools_library tools t online_libraries online on "").(listings);
     path:		>ldd>listings>tools;
     search procedure:	multics_library_search_$list_info_dirs;


Root:			(tools_library tools t online_libraries online on "").(execution x "");
     path:		>tools;
     search procedure:	multics_library_search_$execution_dirs;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


Root:			(supervisor_library sup hardcore hard h
			 offline_libraries offline off ""      ).(source s "");
     path:		>ldd>hard>source;
     search procedure:	multics_library_search_$source_dirs;

Root:			(supervisor_library sup hardcore hard h
			 offline_libraries offline off ""      ).(execution ex x e "");
     path:		>ldd>hard>execution;
     search procedure:	multics_library_search_$execution_dirs;

Root:			(supervisor_library sup hardcore hard h
			 offline_libraries offline off ""      ).(object o "");
     path:		>ldd>hard>object;
     search procedure:	multics_library_search_$object_dirs;

Root:			(supervisor_library sup hardcore hard h
			 offline_libraries offline off ""      ).(listings);
     path:		>ldd>listings>hard;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(supervisor_library sup hardcore hard h
			 offline_libraries offline off ""      ).(i "");
     path:		>ldd>hard>info;
     search procedure:	multics_library_search_$list_info_dirs;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


Root:			(mcs communications_library com comm offline_libraries offline off "").(source s "");
     path:		>ldd>mcs>source;
     search procedure:	multics_library_search_$source_dirs;

Root:			(mcs communications_library com comm offline_libraries offline off "").(object o "");
     path:		>ldd>mcs>object;
     search procedure:	multics_library_search_$object_dirs;

Root:			(mcs communications_library com comm offline_libraries offline off "").(i "");
     path:		>ldd>mcs>info;
     search procedure:	multics_library_search_$source_dirs;

Root:			(mcs communications_library com comm offline_libraries offline off "").(listings);
     path:		>ldd>listings>comm;
     search procedure:	multics_library_search_$list_info_dirs;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


Root:			(standard_library std sss
			 languages_library lang
			 tools_library tools t
			 supervisor_library sup hardcore hard h
			 unbundled_library unb
			 obsolete_library obsolete obs
			 offline_libraries offline off
			 online_libraries online on ""              ).(include incl "");
     path:		>ldd>include;
     search procedure:	multics_library_search_$list_info_dirs;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


Root:			(standard_library std sss
			 languages_library lang
			 unbundled_library unb
			 tools_library tools t
			 supervisor_library sup hardcore hard h
			 offline_libraries offline off
			 online_libraries online on "").(info "");
     path:		>doc>info;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(privileged priv 
			 tools_library tools t
			 supervisor_library sup hardcore hard h
			 offline_libraries offline off
			 online_libraries online on "").(info "");
     path:		>doc>privileged;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 accounting "").(info "");
     path:		>doc>subsystem>accounting;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 analyze_multics azm "").(info "");
     path:		>doc>subsystem>analyze_multics;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 offline_libraries offline off 
			 bce "").(info "");
     path:		>doc>subsystem>bce;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 deckfile_manager dfm "").(info "");
     path:		>doc>subsystem>deckfile_manager;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 dfast "").(info "");
     path:		>doc>subsystem>dfast;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 dial_out "").(info "");
     path:		>doc>subsystem>dial_out;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 emacs "").(info "");
     path:		>doc>subsystem>emacs;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 executive_forum xforum "").(info "");
     path:		>doc>subsystem>executive_forum;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 executive_mail xmail "").(info "");
     path:		>doc>subsystem>executive_mail;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 fast "").(info "");
     path:		>doc>subsystem>fast;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 forum "").(info "");
     path:		>doc>subsystem>forum;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 io_daemon "").(info "");
     path:		>doc>subsystem>io_daemon;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 kermit "").(info "");
     path:		>doc>subsystem>kermit;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 linus "").(info "");
     path:		>doc>subsystem>linus;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 mail_system mlsys read_mail rdm "").(info "");
     path:		>doc>subsystem>mail_system>read_mail;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 mail_system mlsys read_mail rdm "").(info "");
     path:		>doc>subsystem>mail_system>read_mail>forward_requests;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 mail_system mlsys send_mail sdm "").(info "");
     path:		>doc>subsystem>mail_system>send_mail;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 mail_system mlsys send_mail sdm "").(info "");
     path:		>doc>subsystem>mail_system>send_mail>original_requests;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 operator initializer "").(info "");
     path:		>doc>subsystem>operator;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 r1_initializer "").(info "");
     path:		>doc>subsystem>r1_initializer;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 report_writer_ rw_ report_writer rw "").(info "");
     path:		>doc>subsystem>report_writer_info_dirs;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 restructure_mrds_db rmdb "").(info "");
     path:		>doc>subsystem>rmdb;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(subsystem_library subsystem ss
			 online_libraries online on 
			 ssu_ ssu "").(info "");
     path:		>doc>subsystem;
     search procedure:	multics_library_search_$list_info_dirs;

Root:			(user_library user auth_maint am
			 installation_library inst install_maint iml
			 online_libraries online on ""              ).(info "");
     path:		>doc>iis;
     search procedure:	multics_library_search_$list_info_dirs;


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

Root:			(obsolete_library obsolete obs).(source s "");
     path:		>ldd>obs>source;
     search procedure:	multics_library_search_$source_dirs;

Root:			(obsolete_library obsolete obs).(object o "");
     path:		>ldd>obs>object;
     search procedure:	multics_library_search_$object_dirs;

Root:			(obsolete_library obsolete obs).(execution x "");
     path:		>obs;
     search procedure:	multics_library_search_$execution_dirs;

Root:			(obsolete_library obsolete obs).(info "");
     path:		>doc>obs;
     search procedure:	multics_library_search_$list_info_dirs;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


End:  multics_libraries_;
