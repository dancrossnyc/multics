
/* HISTORY COMMENTS:
  1) change(2021-02-23,GDixon), approve(2021-02-23,MCR10089),
     audit(2021-03-31,Swenson), install(2021-03-31,MR12.6g-0053):
     Initial version of info_seg_specifications_ data segment.
                                                   END HISTORY COMMENTS */


(stringsize):					/* Condition checks enabled to assist with updates to     */
(subscriptrange):					/*  this data.  If user adds initialization data without  */
info_seg_specifications_:				/*  changing array dimensions, that will be detected by   */
     proc ();					/*  the cds translator.			        */

/* ---------------------------------------------------------------------------
    Info block analysis chart (taken from validate_info_seg in 2020-02-12).
     - Use info block name and leading section title to determine iBlok.kind
       (iBlok_kind_GENERAL_INFO, iBlok_kind_COMMAND, iBlok_kind_ACTIVE_FUNCTION,
       etc.).
     - Apply per-kind guidelines for "usual" or "preferred" section titles, 
       section ordering, etc.

   (START)                                                                                      
      |                                   [S] represents "Scan a section title"                 
   ___v___                                                                                      
  / .gi/  \       GENERAL_INFO
 / .error/ \ Y       +-----------------+                                                        
<  status/  >------->| section         |                                                        
 \ changes /         |{section ...}    |                                                        
  \_______/          +-----------------+                                                        
      |N                                                                     ("Error")          
     [S]                                                                         |              
      |                                                                          |N             
  ____v____                                             _________            ____|____          
 /         \ Y                                         / Entry-  \ Y        /         \         
< untitled  >--[S]----------------------------------->< points in >-------><  :Entry:  >        
 \_________/                                           \_________/          \_________/         
      |N                                                    |N                   |Y             
      |     An info is prescanned enough to determine       |                    |              
      |     it's type, then a real scan begins.             |                    |              
      |                                                     |      SUBROUTINE    v              
  ____v____                                                 |         +---------------------+   
 /         \ Y                                              v         | Function            |   
< Function  >-------------------------------------------------------->| Syntax              |   
 \_________/                           COMMAND                        |{Arguments}         *|   
      |N                                  +---------------------+     |{Access required}   *|   
      |                                   | Syntax as commamd   |     |{Examples}           |   
  ____v____            _________          |                     |     +---------------------+   
 /Syntax as\ Y        /Syntax as\ N       | Function            |                |              
< a command >--[S]--><an act.fun.>------->|{Arguments}         *|           If came from :Entry:
 \_________/          \_________/        #|{Control args}      *|           go look for another 
      |N                   |Y            #|{CA as a command}   *|                               
      |                    |             #|{CA COMMAND...}     *|  COMMAND_AF
      |                    |              |{Access required}   *|     +---------------------+   
      |                    |              |{Examples}           |     | Syntax as command   |   
      |                    |              +---------------------+     | Syntax as act.fun.  |   
      |                    +----------------------------------------->|                     |   
      |                    |Y          ACTIVE_FUNCTION                | Function            |   
  ____v____            ____|____          +---------------------+     |{Arguments}         *|   
 /Syntax as\ Y        /Syntax as\ N       | Syntax as act.fun.  |    #|{Control Args}      *|   
<an act.fun.>--[S]-->< a command >------->|                     |    #|{CA as a command}   *|   
 \_________/          \_________/         | Function            |    #|{CA as an act.func} *|   
      |N                                  |{Arguments}         *|    #|{CA for...}         *|   
      |                                  #|{Control args}      *|     |{Access required}   *|   
      |                                  #|{CA as an act.func} *|     |{Examples}           |   
      |                                  #|{CA for...}         *|     +---------------------+   
      |                                   |{Access required}   *|                               
      |                                   |{Examples}           |                               
      |                                   +---------------------+ (*) These sections can occur  
      |                                                               next in any order:        
      |                                REQUEST                            List of...            
      |                                   +---------------------+         Notes                 
      |                                   | Syntax              |         Notes on...           
  ____v____            _________          |                     |                               
 /         \ Y        /Syntax as\ N       | Function            | (#) Sections can occur as     
<  Syntax   >--[S]--><an act.req.>------->|{Arguments}         *|     a group in any order.     
 \_________/          \_________/        #|{Control args}      *|                               
      |N                   |Y            #|{CA as a request}   *|                               
      |                    |             #|{CA for...}         *|  REQUEST_AR
      |                    |              |{Access required}   *|     +---------------------+   
      |                    |              |{Examples}           |     | Syntax              |   
      |                    |              +---------------------+     | Syntax as act.req.  |   
      |                    +----------------------------------------->|                     |   
      |                    |Y          ACTIVE_REQUEST                 | Function            |   
  ____v____            ____|____          +---------------------+     |{Arguments}         *|   
 /Syntax as\ Y        /         \ N       | Syntax as act.req.  |    #|{Control args}      *|   
<an act.req.>--[S]--><  Syntax   >------->|                     |    #|{CA as a request}   *|   
 \_________/          \_________/         | Function            |    #|{CA as an act.req}  *|   
      |N                                  |{Arguments}         *|    #|{CA for...}         *|   
 ("Not a defined type")                  #|{Control args}      *|     |{Access required}   *|   
      |                                  #|{CA as an act.req}  *|     |{Examples}           |   
      v                                  #|{CA for...}         *|     +---------------------+   
+-----------+                             |{Access required}   *|                               
| sections  |                             |{Examples}           |                               
+-----------+                             +---------------------+                               
   --------------------------------------------------------------------------- */


%page;
  dcl  create_data_segment_ entry (ptr, fixed bin (35));
  dcl  ioa_ entry() options(variable);

  dcl  code fixed bin (35);

  dcl (dimension, addr, size, string)	     builtin;

  dcl (info_seg_specification_error)	     condition;

%include info_seg_dcls_;				/* Structures referenced in like attributes in the        */
						/*  following declarations.			        */
%page;
  dcl (i,						/* Index into info.array 			        */
       i_first_BAD_title,				/* Index of first info.section_titles.array element with  */
						/*   BAD .ilk value.			        */
       k						/* Index info info.section_sequence.array	        */
       ) fixed bin;

  dcl 1 info aligned,				/* The data structure prepared by this .cds file.  Each   */
						/*  level 2 structure defines an entrypoint created in    */
						/*  the object segment output by the cds translator.      */
      2 section_titles,
        3 titleN fixed bin,
        3 array (83) like iSect_title.titles.array,	/* iSect_title structure (and others referenced in array  */
						/*  variables) declared in info_seg_dcls_.incl.pl1        */
      2 block_kinds,
        3 kindN fixed bin,
        3 array (32) like iBlok_kind.kinds.array,
      2 block_kind_names,
        3 nameN fixed bin,
        3 array (10) like iBlok_name.names.array,
      2 file_structures,
        3 patternN fixed bin,
        3 array (15) like iFile_structure.patterns.array,
      2 section_sequence,
        3 kindN fixed bin,
        3 array (15) like iSect_sequence.sequence.array;

     unspec(info) = "0"b;				/* Zero automatic storage containing local copy of data   */

     info.section_titles.titleN     = dimension(info.section_titles.array, 1);
     info.block_kinds.kindN         = dimension(info.block_kinds.array, 1);
     info.block_kind_names.nameN    = dimension(info.block_kind_names.array, 1);
     info.file_structures.patternN  = dimension(info.file_structures.array, 1);
     info.section_sequence.kindN    = dimension(info.section_sequence.array, 1);
						/* Populate the xxxN variables from declared .array       */
						/*  dimensions.				        */
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* EXTERNAL TABLE:      iSect_title		(in info_seg_dcls_.incl.pl1)		        */
	/* INTERNAL STRUCTURE:  info.section_titles					        */
	/*									        */
	/* Each row in iSect_title.array defines characteristics of a section title that may be used in   */
	/* info segments.  Four characteristics are included for each section title:		        */
	/*									        */
	/*  array.cmp	specifies the form of the .title element:			        */
	/*                   EQUALS:  the element gives all characters of a complete section title        */
	/*			exactly as it appears in the info segment.		        */
	/*                   BEGINS:  the element gives leading characters of a section title.  Other     */
	/*			characters that follow provide further information about content    */
	/*			of data within that section of the info segment.		        */
	/*			For example, if .title is "Notes on ", a full title might	        */
	/*			be "Notes on abbrev request lines:"			        */
	/*  array.title     gives all characters of the section title (if .cmp is EQUALS); or leading     */
	/*		characters of a longer section title (if .cmp is BEGINS).		        */
	/*  array.ilk       gives a guideline for use of such section title:			        */
	/*                   PREF:    this section title is preferred for use in info segments.	        */
	/*                   BAD:     this section title should not be used.  The .sectionID specifies    */
	/*			one of the PREF section titles that should be used instead.	        */
	/*  array.sectionID								        */
	/*                  gives a constant that is the row index of one of the array rows.	        */
	/*                   If .ilk is PREF, it is the index of the current row in the array.	        */
	/*                   If .ilk is BAD,  it is the index of another row in the array containing a    */
	/*		                  PREF section title that should be used in place of the      */
	/*			        title in this row.				        */
	/* NOTES:									        */
	/*  1) Order of call statements below gives sequence in which a section title is compared against */
	/*     the constrants of a row in the .array, to see if it matches those constraints.	        */
	/*      - The first matching row defines the preferred .sectionID value for that section title.   */
	/*  2) The row index of each PREF title is its .sectionID value (its iSect_XXX value).	        */
	/*      - PREF titles are listed in alphabetic order.				        */
	/*  3) BAD or obsolete titles are listed following all PREF titles, again in alphabetic order by  */
	/*     title.								        */
	/*  4) The EQUALS, BEGINS, PREF, BAD and iSect_XXX constants are declared in:                     */
	/*     in: info_seg_dcls_.incl.pl1						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */


     i = 0;					  /* Section Titles				         */
     i_first_BAD_title = 0;

/*           cmp      title                                       ilk   sectionID                                      */
     call t( EQUALS, "Access required",                           PREF, iSect_ACCESS_REQUIRED                          );
     call t( EQUALS, "Arguments",			      PREF, iSect_ARGUMENTS			         );
     call t( EQUALS, "Arguments for io_call",		      PREF, iSect_ARGUMENTS_FOR_IO_CALL		         );
     call t( EQUALS, "Arguments for iox_$control",	      PREF, iSect_ARGUMENTS_FOR_IOX_CONTROL	         );
     call t( BEGINS, "Arguments for ",			      PREF, iSect_ARGUMENTS_FOR		         );
     call t( BEGINS, "Arguments (",			      PREF, iSect_ARGUMENTS_SUBSET                         );
     call t( EQUALS, "Control arguments",		      PREF, iSect_CONTROL_ARGUMENTS		         );
     call t( EQUALS, "Control arguments as a command",	      PREF, iSect_CONTROL_ARGUMENTS_AS_A_COMMAND	         );
     call t( EQUALS, "Control arguments as a request",	      PREF, iSect_CONTROL_ARGUMENTS_AS_A_REQUEST	         );
     call t( EQUALS, "Control arguments as an active function",   PREF, iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_FUNCTION  );
     call t( EQUALS, "Control arguments as an active request",    PREF, iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_REQUEST   );
     call t( EQUALS, "Control arguments for attach description",  PREF, iSect_CONTROL_ARGUMENTS_FOR_ATTACH_DESCRIPTION );
     call t( EQUALS, "Control arguments for open description",    PREF, iSect_CONTROL_ARGUMENTS_FOR_OPEN_DESCRIPTION   );
     call t( EQUALS, "Control arguments for close description",   PREF, iSect_CONTROL_ARGUMENTS_FOR_CLOSE_DESCRIPTION  );
     call t( EQUALS, "Control arguments for detach description",  PREF, iSect_CONTROL_ARGUMENTS_FOR_DETACH_DESCRIPTION );
     call t( BEGINS, "Control arguments for ",		      PREF, iSect_CONTROL_ARGUMENTS_FOR		         );
     call t( BEGINS, "Control arguments (",		      PREF, iSect_CONTROL_ARGUMENTS_SUBSET                 );
     call t( EQUALS, "Control order",			      PREF, iSect_CONTROL_ORDER		         );
     call t( BEGINS, "Entry points in ",                          PREF, iSect_ENTRY_POINTS_IN		         );
     call t( EQUALS, "Examples",			      PREF, iSect_EXAMPLES			         );
     call t( EQUALS, "Function",			      PREF, iSect_FUNCTION			         );
     call t( EQUALS, "List of control operations",	      PREF, iSect_LIST_OF_CONTROL_OPERATIONS	         );
     call t( EQUALS, "List of controls",		      PREF, iSect_LIST_OF_CONTROLS		         );
     call t( EQUALS, "List of elements",		      PREF, iSect_LIST_OF_ELEMENTS		         );
     call t( EQUALS, "List of i/o operations",		      PREF, iSect_LIST_OF_IO_OPERATIONS		         );
     call t( EQUALS, "List of mode strings",		      PREF, iSect_LIST_OF_MODE_STRINGS		         );
     call t( EQUALS, "List of opening modes",		      PREF, iSect_LIST_OF_OPENING_MODES		         );
     call t( EQUALS, "List of operations",		      PREF, iSect_LIST_OF_OPERATIONS		         );
     call t( EQUALS, "List of requests",		      PREF, iSect_LIST_OF_REQUESTS		         );
     call t( EQUALS, "List of active requests",                   PREF, iSect_LIST_OF_ACTIVE_REQUESTS                  );
     call t( BEGINS, "List of ",                                  PREF, iSect_LIST_OF			         );
     call t( EQUALS, "Notes on the info_ptr",		      PREF, iSect_NOTES_ON_THE_INFO_PTR		         );
     call t( EQUALS, "Notes",				      PREF, iSect_NOTES			         );
     call t( BEGINS, "Notes on ",                                 PREF, iSect_NOTES_ON			         );
     call t( EQUALS, "Syntax",			      PREF, iSect_SYNTAX			         );
     call t( EQUALS, "Syntax as a command",		      PREF, iSect_SYNTAX_AS_A_COMMAND		         );
     call t( EQUALS, "Syntax as an active function",	      PREF, iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION	         );
     call t( EQUALS, "Syntax as an active request",	      PREF, iSect_SYNTAX_AS_AN_ACTIVE_REQUEST	         );
     call t( EQUALS, "Syntax of attach description",	      PREF, iSect_SYNTAX_OF_ATTACH_DESCRIPTION	         );
     call t( EQUALS, "Syntax of open description",	      PREF, iSect_SYNTAX_OF_OPEN_DESCRIPTION	         );
     call t( EQUALS, "Syntax of close description",	      PREF, iSect_SYNTAX_OF_CLOSE_DESCRIPTION	         );
     call t( EQUALS, "Syntax of detach description",	      PREF, iSect_SYNTAX_OF_DETACH_DESCRIPTION	         );

     call t( EQUALS, "Access requirement",                        BAD,  iSect_ACCESS_REQUIRED		         );
     call t( EQUALS, "Access requirements",		      BAD,  iSect_ACCESS_REQUIRED		         );
     call t( EQUALS, "Active function argument",		      BAD,  iSect_ARGUMENTS			         );
     call t( EQUALS, "Active function arguments",		      BAD,  iSect_ARGUMENTS			         );
     call t( EQUALS, "Active function control argument",	      BAD,  iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_FUNCTION  );
     call t( EQUALS, "Active function control arguments",	      BAD,  iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_FUNCTION  );
     call t( EQUALS, "Active function syntax",		      BAD,  iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION	         );
     call t( EQUALS, "Active function usage",		      BAD,  iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION	         );
     call t( BEGINS, "Argument (",			      BAD,  iSect_ARGUMENTS_SUBSET		         );
     call t( EQUALS, "Argument",			      BAD,  iSect_ARGUMENTS			         );
     call t( EQUALS, "Arguments as active function",	      BAD,  iSect_ARGUMENTS			         );
     call t( EQUALS, "Argument for io_call",		      BAD,  iSect_ARGUMENTS_FOR_IO_CALL		         );
     call t( EQUALS, "Argument for iox_$control",		      BAD,  iSect_ARGUMENTS_FOR_IOX_CONTROL	         );
     call t( EQUALS, "Command argument",		      BAD,  iSect_ARGUMENTS			         );
     call t( EQUALS, "Command arguments",		      BAD,  iSect_ARGUMENTS			         );
     call t( EQUALS, "Command control argument",		      BAD,  iSect_CONTROL_ARGUMENTS_AS_A_COMMAND	         );
     call t( EQUALS, "Command control arguments",		      BAD,  iSect_CONTROL_ARGUMENTS_AS_A_COMMAND	         );
     call t( EQUALS, "Command syntax",			      BAD,  iSect_SYNTAX_AS_A_COMMAND		         );
     call t( EQUALS, "Command usage",			      BAD,  iSect_SYNTAX_AS_A_COMMAND		         );
     call t( EQUALS, "Control argument as command",	      BAD,  iSect_CONTROL_ARGUMENTS_AS_A_COMMAND	         );
     call t( BEGINS, "Control argument (",		      BAD,  iSect_CONTROL_ARGUMENTS_SUBSET	         );
     call t( EQUALS, "Control argument",		      BAD,  iSect_CONTROL_ARGUMENTS		         );
     call t( EQUALS, "Control argument as active function",       BAD,  iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_FUNCTION  );
     call t( BEGINS, "Entry point in ",                           BAD,  iSect_ENTRY_POINTS_IN		         );
     call t( BEGINS, "Entrypoint in ",                            BAD,  iSect_ENTRY_POINTS_IN		         );
     call t( BEGINS, "Entrypoints in ",                           BAD,  iSect_ENTRY_POINTS_IN		         );
     call t( BEGINS, "Example ",                                  BAD,  iSect_EXAMPLES			         );
     call t( EQUALS, "List of Operations",		      BAD,  iSect_LIST_OF_OPERATIONS		         );
     call t( EQUALS, "List of OPERATIONS",		      BAD,  iSect_LIST_OF_OPERATIONS		         );     
     call t( EQUALS, "List of Requests",		      BAD,  iSect_LIST_OF_REQUESTS		         );     
     call t( EQUALS, "List of REQUESTS",		      BAD,  iSect_LIST_OF_REQUESTS		         );     
     call t( EQUALS, "Note",				      BAD,  iSect_NOTES			         );
     call t( BEGINS, "Notes for ",			      BAD,  iSect_NOTES_ON			         );
     call t( EQUALS, "Purpose",			      BAD,  iSect_FUNCTION			         );
     call t( EQUALS, "Syntax and Attach Description",             BAD,  iSect_SYNTAX			         );
     call t( EQUALS, "Syntax as active function",		      BAD,  iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION	         );
     call t( EQUALS, "Syntax as command",		      BAD,  iSect_SYNTAX_AS_A_COMMAND		         );
     call t( EQUALS, "Syntax as request",		      BAD,  iSect_SYNTAX			         );    
     call t( EQUALS, "Syntax as a request",		      BAD,  iSect_SYNTAX			         );    
     call t( BEGINS, "Syntax of ",			      BAD,  iSect_SYNTAX			         );
     call t( EQUALS, "Where",				      BAD,  iSect_ARGUMENTS			         );

     if  i ^= info.section_titles.titleN  then do;
	call ioa_ ("info_seg_specifications_.cds: info.section_titles.titleN = ^d  but should be: ^d",
	     info.section_titles.titleN, i);
	signal condition (info_seg_specification_error);
          end;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* EXTERNAL TABLE:      iBlok_kind   		(in info_seg_dcls_.incl.pl1)		        */
	/* INTERNAL STRUCTURE:  info.block_kinds					        */
	/*									        */
	/* Each row in iBlok_kind.array gives characteristics of a block (complete chunk of data) in an   */
	/* info segment.  Two characteristics are included to identify particular kinds of blocks:        */
	/*									        */
	/*  array.kindID           constant associated with this kind of block.		        */
	/*  array.method_or_title  identifying constraint for this kind of block: either a section title  */
	/*		       appearing at the beginning of the block (immediately after the	        */
	/*		       header); or another constraint by which such blocks are recognized.    */
	/*                         These "method" constraints are:				        */
	/*                          iBlok_method_REFINE_EARLIER_KIND:			        */
	/*                                       kind of block was determined earlier by the	        */
	/*				 info_seg_parse_$file_structure routine.  It may need     */
	/*                                       further adjustment by this method.		        */
	/*                          iBlok_method_PATH_subsystem				        */
	/* 				 first section title is "Syntax" but iFile.directory      */
	/*				 contains either ">subsystem>" or ">ss>" to distinguish   */
	/*				 SUBSYSTEM_REQUEST for those requests installed under     */
	/*				 >doc>subsystem>SUBSYS_DIR>*.info looking like either     */
	/*				 COMMAND block (if date-modified < 1985) or SUBROUTINE    */
	/*				 block (which also has 1st section of "Syntax").	        */
	/*                          iBlok_method_NAME_summary_topic:			        */
	/*                                       section titles and file structure haven't identified a   */
	/*				 block kind, a name on info seg/block is: summary.topic   */
	/*				 distinguish between a SUBSYSTEM_SUMMARY vs another       */
	/*				 SUBSYSTEM_TOPIC kind.			        */
	/*                          iBlok_method_NAMES_MAY_TELL_KIND:			        */
	/*                                       section titles and file structure haven't identified a   */
	/*				 block kind, so names on the info seg/block will	        */
	/*				 distinguish between a GENERAL_INFO block versus a block  */
	/*				 of Unknown kind.				        */
	/*                          iBlok_method_DATE_BEFORE_1985:				        */
	/*                                       "Syntax" section title is ambiguous w/o "Syntax as...".  */
	/*				 It could indicate an older command, or a newer	        */
	/*				 subsystem request.  Use header line date to distinguish  */
	/*				 between these two possibilities.  Dates on/after 1985    */
	/*				 are more likely to be a subsystem request.	        */
	/*  array.second_title     a section section title constraint, or 0 if only one section title is  */
	/*		       needed to recognize this kind of block.  When two section title        */
	/*		       constraints are given, those titles must both appear as 1st and 2nd    */
	/*		       sections of the block; and the two sections may appear in either       */
	/*		       order.						        */
	/*									        */
	/* NOTES:									        */
	/*  1) Order of items in the list below gives sequence in which section titles are tested for     */
	/*     determining their kind of section.					        */
	/*  2) The iBlok_kind_XXX, iSect_XXX and iBlok_method_XXX named constants are declared in:        */
	/*         info_seg_dcls_.incl.pl1						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     i = 0;					/* Block Kinds			                   */
     
/*           kindID                                 method_or_title,                     second_title	         */

     call b( iBlok_kind_SUBROUTINE_INTRO,       iBlok_method_REFINE_EARLIER_KIND,   iSect_FUNCTION,                     0);
     call b( iBlok_kind_SUBROUTINE_BRIEF_INTRO, iBlok_method_REFINE_EARLIER_KIND,   iSect_ENTRY_POINTS_IN,		0);

     call b( iBlok_kind_SUBSYSTEM_SUMMARY,      iBlok_method_NAME_summary_topic,    0,				0);
     call b( iBlok_kind_SUBSYSTEM_TOPIC,        iBlok_method_NAMES_MAY_TELL_KIND,   0,				0);
     call b( iBlok_kind_GENERAL_INFO,	        iBlok_method_NAMES_MAY_TELL_KIND,   0,				0);

     call b( iBlok_kind_SUBROUTINE_ENTRY,       iSect_FUNCTION,                     iSect_SYNTAX,			0);

     call b( iBlok_kind_IO_MODULE,	        iSect_SYNTAX_AS_A_COMMAND,	    iSect_SYNTAX_OF_ATTACH_DESCRIPTION, 0);
     call b( iBlok_kind_IO_OPERATION,	        iSect_SYNTAX_AS_A_COMMAND,	    iSect_SYNTAX_OF_CLOSE_DESCRIPTION,  0);
     call b( iBlok_kind_IO_OPERATION,	        iSect_SYNTAX_AS_A_COMMAND,	    iSect_SYNTAX_OF_DETACH_DESCRIPTION, 0);
     call b( iBlok_kind_IO_OPERATION,	        iSect_SYNTAX_AS_A_COMMAND,	    iSect_SYNTAX_OF_OPEN_DESCRIPTION,   0);
     call b( iBlok_kind_IO_MODULE,	        iSect_SYNTAX_OF_ATTACH_DESCRIPTION, 0,				0);
     call b( iBlok_kind_IO_OPERATION,	        iSect_SYNTAX_OF_CLOSE_DESCRIPTION,  0,				0);
     call b( iBlok_kind_IO_OPERATION,	        iSect_SYNTAX_OF_DETACH_DESCRIPTION, 0,				0);
     call b( iBlok_kind_IO_OPERATION,	        iSect_SYNTAX_OF_OPEN_DESCRIPTION,   0,				0);
     call b( iBlok_kind_IO_CONTROL,	        iSect_CONTROL_ORDER,		    0,				0);

     call b( iBlok_kind_REQUEST_AR,	        iBlok_method_PATH_subsystem,        iSect_SYNTAX,
								    iSect_SYNTAX_AS_AN_ACTIVE_REQUEST    );
     call b( iBlok_kind_REQUEST_AR,	        iBlok_method_PATH_subsystem,        iSect_SYNTAX_AS_A_COMMAND,
								    iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION   );
     call b( iBlok_kind_REQUEST,	        iBlok_method_PATH_subsystem,        iSect_SYNTAX,			0);
     call b( iBlok_kind_REQUEST,	        iBlok_method_PATH_subsystem,        iSect_SYNTAX_AS_A_COMMAND,	0);
     call b( iBlok_kind_ACTIVE_REQUEST,	        iBlok_method_PATH_subsystem,        iSect_SYNTAX_AS_AN_ACTIVE_REQUEST,	0);
     call b( iBlok_kind_ACTIVE_REQUEST,	        iBlok_method_PATH_subsystem,        iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION,	0);

     call b( iBlok_kind_COMMAND_AF,	        iSect_SYNTAX_AS_A_COMMAND,          iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION,	0);
     call b( iBlok_kind_COMMAND,                iSect_SYNTAX_AS_A_COMMAND,          0,				0);
     call b( iBlok_kind_ACTIVE_FUNCTION,        iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION, 0,				0);
     call b( iBlok_kind_COMMAND_AF,	        iSect_SYNTAX,                       iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION,	0);

     call b( iBlok_kind_REQUEST_AR,	        iSect_SYNTAX,                       iSect_SYNTAX_AS_AN_ACTIVE_REQUEST,	0);
     call b( iBlok_kind_COMMAND,	        iBlok_method_DATE_BEFORE_1985,      iSect_SYNTAX,			0);
     call b( iBlok_kind_REQUEST,	        iSect_SYNTAX,                       0,				0);
     call b( iBlok_kind_ACTIVE_REQUEST,	        iSect_SYNTAX_AS_AN_ACTIVE_REQUEST,  0,				0);
     call b( iBlok_kind_SUBSYSTEM_TOPIC,        iSect_NOTES_ON,		    0,				0);

     call b( iBlok_kind_Header_Only,	        iBlok_method_REFINE_EARLIER_KIND,   0,				0);

     call b( iBlok_kind_Unknown,                0,                                  0,				0);

     if  i ^= info.block_kinds.kindN  then do;		/* Ensure array dimension exactly equals number of calls  */
						/* to the b subroutine, each of which fills in a row in   */
						/* the info.block_kinds array.		        */
	call ioa_ ("info_seg_specifications_.cds: info.block_kinds.kindN = ^d  but should be: ^d",
	     info.block_kinds.kindN, i);
	signal condition (info_seg_specification_error);
          end;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* EXTERNAL TABLE:      iBlok_name		(in info_seg_dcls_.incl.pl1)		        */
	/* INTERNAL STRUCTURE:  info.block_kind_names					        */
	/*									        */
	/* Each row in iBlok_name.array gives a suffix that might appear in an info segment name, or in   */
	/* a info block divider name, followed by the block.kind value associated with that name suffix.  */
	/* Element in each row are:							        */
	/*									        */
	/*  name_type  =iBlok_name_ends: name appears as a suffix at end of segment/divider name.	        */
	/*             =iBlok_block_name_ends:  name appears as a suffix at end of block divider name.    */
	/*  name       array of two names: (1) name suffix with .info for checking segment names;	        */
	/*			     (2) name suffix without .info for checking divider names.      */
	/*             name(1) is given as an argument to the n subroutine; it constructs name(2).        */
	/*  kindID     kind of block associated with this name fragment.			        */
	/*									        */
	/* Notes:									        */
	/*  1) All entries with a given kindID must specify the same name_type value.		        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     i = 0;
/*            name_type          name                        kindID					        */

     call n( iBlok_name_ENDS,   "changes.info",		iBlok_kind_GENERAL_INFO );
     call n( iBlok_name_ENDS,   "differences.info",	iBlok_kind_GENERAL_INFO );
     call n( iBlok_name_ENDS,   "diffs.info",		iBlok_kind_GENERAL_INFO );
     call n( iBlok_name_ENDS,   ".errata.info",		iBlok_kind_GENERAL_INFO );
     call n( iBlok_name_ENDS,   ".error.info",		iBlok_kind_GENERAL_INFO );
     call n( iBlok_name_ENDS,   ".errors.info",		iBlok_kind_GENERAL_INFO );
     call n( iBlok_name_ENDS,   ".gi.info",		iBlok_kind_GENERAL_INFO );
     call n( iBlok_name_ENDS,   "new_features.info",	iBlok_kind_GENERAL_INFO );
     call n( iBlok_name_ENDS,   "old_features.info",	iBlok_kind_GENERAL_INFO );

     call n( iBlok_block_name_ENDS,   ".topic.info",	iBlok_kind_SUBSYSTEM_TOPIC );

     if  i ^= info.block_kind_names.nameN  then do;
	call ioa_ ("info_seg_specifications_.cds: info.block_kind_names.nameN = ^d  but should be: ^d",
	     info.block_kind_names.nameN, i);
	signal condition (info_seg_specification_error);
          end;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* EXTERNAL TABLE:      iFile_structure		(in info_seg_dcls_.incl.pl1)		        */
	/* INTERNAL STRUCTURE:  info.file_structures					        */
	/*									        */
	/* Each row in iFile_structure.array gives a minimum block count and pattern of blocks, followed  */
	/* by a string describing that file structure pattern, and a structID value identifying the       */
	/* pattern.								        */
	/*									        */
	/* In patterns below, the following symbols represent block types:			        */
	/*    N   no block divider							        */
	/*    E   an :Entry: block divider						        */
	/*   [I]	an :[Info]: block divider						        */
	/*    I   either :Info: or :[Info]: block dividers				        */
	/*    H   either :hcom: or :Internal: block dividers				        */
	/*									        */
	/* Notes:									        */
	/*  1) Order of array elements controls order in which a file's blocks are compared with the      */
	/*     known (supported) block patterns.					        */
	/*  2) Actual requirements for each array.spec are implemented manually in the info_seg_parse_    */
	/*     match_structure_spec internal procedure.  The spec string given here is only a pictorial   */
	/*     description of the intended pattern configuration.  To change a specification, change      */
	/*     the pictorial array.spec below; then change the implementation in match_structure_spec.    */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

/* --------------------------------------------------------------------------------
   Each specification character string has the form:
      "3+    I    E...  H  "
   in which:
    - a numeric token (e.g., 3+) gives minimum number of blocks in
      this known organizational pattern, with + indicating that 
      more than that minimum block count may be present.

            - a block type token that specifies the first block which must
	    appear in the info seg:
            [I]   an :[Info]: divider
	   I    an :Info: or :[Info]: divider
	   E    an :Entry: divider
	   H    an :hcom: or :Internal: divider (used for history comment blocks)
	   N    an initial block of the info seg that starts with none of
	           four divider starter strings shown above.

                 - a block type token that specifies block dividers appearing in
	         the middle of the info segment.
	        E...   1 or more :Entry: block dividers.
	        E.I.   1 or more :Info: or :[Info]: dividers appearing with :Entry: dividers.
	        I...   1 or more :Info: or :[Info]: block dividers.
	        H.H.   2 or more :hcom: or :Internal: block dividers (for history comment).
	        H      1 :hcom: or :Internal: block divider in middle of info seg.

                       - a block type token that specifies final block divider of info seg.
		    H    1 :hcom: or :Internal: block divider (for history comment).
		    .    1 or more :Entry: or :Info: or :[Info]: dividers appearing at end of info.
   ----------------------------------------------------------------- */

     i = 0;					/* File Block Patterns			                 */
/*       spec                 display_label                                           structID                                 */

call p( "0                 ", "Info segment is empty.",                               iFile_struc_err_EMPTY_INFO               );
call p( "1    N            ", "Info Segment - no block dividers",		      iFile_structure_NO_DIVIDERS              );
call p( "1+  [I]           ", "Multi-block info segment begins with :[Info]: divider",iFile_struc_err_1st_INFO_no_ext_names    );
call p( "2+   .   H.H.   . ", "Info segment has multiple History Comment blocks.",    iFile_struc_err_MULTIPLE_HCOM            );
call p( "2+   .   H      . ", "History Comment not last block of info segment.",      iFile_struc_err_HCOM_NOT_LAST            );
call p( "3+   N   E.I.   . ", "Subroutine info w/ mixed :Entry: and :Info: dividers.",iFile_struc_err_SUBROUTINE_INFO_MIX      );
call p( "2+   N   E...     ", "Subroutine Info",				      iFile_structure_SUBROUTINE               );
call p( "2+   N   I...   . ", "Multi-block info segment lacks 1st :Info: divider.",   iFile_struc_err_MISSING_1st_INFO         );
call p( "1+   E            ", "Subroutine info is missing introduction block.",       iFile_struc_err_MISSING_SUBROUTINE_INTRO );
call p( "3+   I   E...   H ", "Subroutine Info - with History Comment",	      iFile_structure_INFO_SUBROUTINE_HCOM     );
call p( "3+   I   E.I.   . ", "Info segment has mixed :Info: and :Entry: dividers.",  iFile_struc_err_INFO_SUBROUTINE_MIX      );
call p( "2+   I   E...     ", "Subroutine Info - introduction in :Info: block",	      iFile_structure_INFO_SUBROUTINE          );
call p( "2+   I   I...   H ", "Info Segment - with History Comment",		      iFile_structure_INFO_HCOM                );
call p( "1+   I   I...     ", "Info Segment - with block dividers",		      iFile_structure_INFO                     );
call p( "1+                ", "Info segment has an unknown block structure.",         iFile_struc_err_STRUCTURE_UNKNOWN        );

     if  i ^= info.file_structures.patternN  then do;
	call ioa_ ("info_seg_specifications_.cds: info.file_structures.patternN = ^d  but should be: ^d",
	     info.file_structures.patternN, i);
	signal condition (info_seg_specification_error);
          end;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* EXTERNAL TABLE:      iSect_sequence		(in info_seg_dcls_.incl.pl1)		        */
	/* INTERNAL STRUCTURE:  info.section_sequence					        */
	/*									        */
	/* Each row in iSect_sequence.array gives:					        */
	/*  .block_kind  an iBlok_kind_XXX value telling kind of info block described by this row.        */
	/*  .titleN      number of section titles that might appear in this kind of block.	        */
	/*  .title_seq   array holding data for up to 15 section titles.  .titleN gives number filled in. */
	/*               Each title_seq element is a sub-structure containing the following elements:     */
	/*    .position     recommended order in which this section title should appear relative to other */
	/*	          section titles within the block.  Titles sharing same .position value may     */
	/*	          appear in any order (within their .position group of titles).	        */
	/*    .cardinality  an iCard_XXX value indicating how many such section titles may appear in      */
	/*		this kind of info block.					        */
	/*    .type         an iSect_XXX value telling type of section title that might appear in block.  */
	/*									        */
	/* CONSTRAINTS:								        */
	/*   Certain section titles describe the same kind of information about a program when that       */
	/*   program is invoked in differing contexts.					        */
	/*									        */
	/*   For example, "Syntax as a command" and "Syntax as an active function" sections both describe */
	/*   what information the user enters to call a given program as a command, or as a function      */
	/*   within a command line.							        */
	/*									        */
	/*   In many cases, the same program may act as a command and as an active function.  The block   */
	/*   may therefore contain several sections describing syntax for invoking the program, or        */
	/*   arguments when invoking as a command or as an active function, etc.		        */
	/*									        */
	/*   The following constraints limit how these related section titles are listed in the	        */
	/*   iSect_sequence table for a given kind of info block allowing these titles.	 They are         */
	/*   designed to keep these related sections grouped together within the info block.	        */
	/*									        */
	/*  Syntax:         All section titles beginning "Syntax..." must share the same .position value  */
	/*		and must be listed in adjacent calls to the s subroutine in code below.       */
	/*		That will place them in adjacent elements of the title_seq sub-array.	        */
	/*									        */
	/*                  Since 1st or 1st/2nd iSect_XXX values determine iBlok.type (see call b(...)   */
	/*		statements above), be sure correct section title for each block type appears  */
	/*		in the call s(...) statements below: in positions 1 and 2 (for Subroutine     */
	/*                  Entry); or in position 1 for other block kinds having a Syntax section.       */
	/*									        */
	/*  Arguments:      All section titles beginning "Arguments..." must share the same .position     */
	/*		value and must be listed in adjacent calls to s.			        */
	/*  Controls:       All section titles beginning "Control arguments..." must share the same       */
	/*		.position value and must be listed in adjacent calls to s.		        */
	/*									        */
	/*   In addition, titles in two or more of the above groups (Syntax vs. Arguments vs.	        */
	/*   Controls) may not share the same .position value.  Each group must have its own .position    */
	/*   value.								        */
	/*									        */
	/*   The info parse code examines the Syntax section(s) of a given info block looking for         */
	/*   arguments or function return values, or control arguments.  If such references are found,    */
	/*   then verify_info will complain if the block does not contain at least one of given sections  */
	/*   describing Arguments, or describing Controls.  Grouping related sections together within the */
	/*   info block aids the user, and also simplifies this parsing step.			        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

/*               Section Title Sequences							        */

     k = iBlok_kind_COMMAND;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_REQUIRED, iSect_SYNTAX_AS_A_COMMAND                    ); /* Must be 1st title in call b*/
     call s( k,   2,  iCard_1_REQUIRED, iSect_FUNCTION                               );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_ARGUMENTS                              );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_FOR                          );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_SUBSET                       );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_LIST_OF_OPERATIONS		     );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS                      );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS_AS_A_COMMAND         );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_FOR                  );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_SUBSET               ); 
     call s( k,   5,  iCard_1_ALLOWED,  iSect_ACCESS_REQUIRED                        );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF_REQUESTS                       );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF_ACTIVE_REQUESTS                );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF                                );
     call s( k,   5,  iCard_1_ALLOWED,  iSect_NOTES                                  );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_NOTES_ON                               );
     call s( k,   6,  iCard_1_ALLOWED,  iSect_EXAMPLES                               );


     k = iBlok_kind_ACTIVE_FUNCTION;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_REQUIRED, iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION           ); /* Must be 1st title in call b*/
     call s( k,   2,  iCard_1_REQUIRED, iSect_FUNCTION                               );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_ARGUMENTS                              );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_FOR                          );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_SUBSET                       );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_LIST_OF_OPERATIONS		     );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS                      );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_FUNCTION);
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_FOR                  );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_SUBSET               ); 
     call s( k,   5,  iCard_1_ALLOWED,  iSect_ACCESS_REQUIRED                        );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF_REQUESTS                       );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF                                );
     call s( k,   5,  iCard_1_ALLOWED,  iSect_NOTES                                  );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_NOTES_ON                               );
     call s( k,   6,  iCard_1_ALLOWED,  iSect_EXAMPLES                               );


     k = iBlok_kind_COMMAND_AF;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_REQUIRED, iSect_SYNTAX_AS_A_COMMAND                    ); /* Must equal 1st/2nd title   */
     call s( k,   1,  iCard_1_REQUIRED, iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION           ); /*  in call b(...) above.     */
     call s( k,   2,  iCard_1_REQUIRED, iSect_FUNCTION                               );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_ARGUMENTS                              );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_FOR                          );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_SUBSET                       );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_LIST_OF_OPERATIONS		     );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS                      );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS_AS_A_COMMAND         );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_FUNCTION);
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_FOR                  );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_SUBSET               ); 
     call s( k,   5,  iCard_1_ALLOWED,  iSect_ACCESS_REQUIRED                        );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF_REQUESTS                       );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF_ACTIVE_REQUESTS                );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF                                );
     call s( k,   5,  iCard_1_ALLOWED,  iSect_NOTES                                  );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_NOTES_ON                               );
     call s( k,   6,  iCard_1_ALLOWED,  iSect_EXAMPLES                               );


     k = iBlok_kind_GENERAL_INFO;			/* No titles recommended or required for this type.       */
     


     k = iBlok_kind_SUBROUTINE_INTRO;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_ALLOWED,  iSect_FUNCTION                               );
     call s( k,   2,  iCard_1_ALLOWED,  iSect_ACCESS_REQUIRED                        );
     call s( k,   2,  iCard_0_OR_MORE,  iSect_LIST_OF                                );
     call s( k,   2,  iCard_1_ALLOWED,  iSect_NOTES                                  );
     call s( k,   2,  iCard_0_OR_MORE,  iSect_NOTES_ON                               );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_EXAMPLES                               );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_ENTRY_POINTS_IN                        );


     k = iBlok_kind_SUBROUTINE_BRIEF_INTRO;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_REQUIRED,  iSect_ENTRY_POINTS_IN                        );
     

     k = iBlok_kind_SUBROUTINE_ENTRY;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_REQUIRED, iSect_FUNCTION                               ); /* Must equal 1st/2nd title   */
     call s( k,   2,  iCard_1_REQUIRED, iSect_SYNTAX                                 ); /*  in call b(...) above.     */
     call s( k,   3,  iCard_1_REQUIRED, iSect_ARGUMENTS                              );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_ACCESS_REQUIRED                        );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_LIST_OF                                );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_NOTES                                  );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_NOTES_ON                               );


     k = iBlok_kind_REQUEST;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_REQUIRED, iSect_SYNTAX                                 ); /* Must be 1st title in call b*/
     call s( k,   2,  iCard_1_REQUIRED, iSect_FUNCTION                               );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_ARGUMENTS                              );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_FOR                          );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_SUBSET                       );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_LIST_OF_OPERATIONS		     );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS                      );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS_AS_A_REQUEST         );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_FOR                  );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_SUBSET               ); 
     call s( k,   5,  iCard_1_ALLOWED,  iSect_ACCESS_REQUIRED                        );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF                                );
     call s( k,   5,  iCard_1_ALLOWED,  iSect_NOTES                                  );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_NOTES_ON                               );
     call s( k,   6,  iCard_1_ALLOWED,  iSect_EXAMPLES                               );


     k = iBlok_kind_ACTIVE_REQUEST;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_REQUIRED, iSect_SYNTAX_AS_AN_ACTIVE_REQUEST            ); /* Must be 1st title in call b*/
     call s( k,   2,  iCard_1_REQUIRED, iSect_FUNCTION                               );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_ARGUMENTS                              );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_FOR                          );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_SUBSET                       );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_LIST_OF_OPERATIONS		     );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS                      );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_REQUEST );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_FOR                  );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_SUBSET               ); 
     call s( k,   5,  iCard_1_ALLOWED,  iSect_ACCESS_REQUIRED                        );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF                                );
     call s( k,   5,  iCard_1_ALLOWED,  iSect_NOTES                                  );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_NOTES_ON                               );
     call s( k,   6,  iCard_1_ALLOWED,  iSect_EXAMPLES                               );


     k = iBlok_kind_REQUEST_AR;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_REQUIRED, iSect_SYNTAX                                 ); /* Must equal 1st/2nd title   */
     call s( k,   1,  iCard_1_REQUIRED, iSect_SYNTAX_AS_AN_ACTIVE_REQUEST            ); /*  in call b(...) above.     */
     call s( k,   2,  iCard_1_REQUIRED, iSect_FUNCTION                               );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_ARGUMENTS                              );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_FOR                          );
     call s( k,   3,  iCard_0_OR_MORE,  iSect_ARGUMENTS_SUBSET                       );
     call s( k,   3,  iCard_1_ALLOWED,  iSect_LIST_OF_OPERATIONS		     );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS                      );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS_AS_A_REQUEST         );
     call s( k,   4,  iCard_1_ALLOWED,  iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_REQUEST );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_FOR                  );
     call s( k,   4,  iCard_0_OR_MORE,  iSect_CONTROL_ARGUMENTS_SUBSET               ); 
     call s( k,   5,  iCard_1_ALLOWED,  iSect_ACCESS_REQUIRED                        );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_LIST_OF                                );
     call s( k,   5,  iCard_1_ALLOWED,  iSect_NOTES                                  );
     call s( k,   5,  iCard_0_OR_MORE,  iSect_NOTES_ON                               );
     call s( k,   6,  iCard_1_ALLOWED,  iSect_EXAMPLES                               );

     k = iBlok_kind_SUBSYSTEM_SUMMARY;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_ALLOWED,  iSect_LIST_OF_REQUESTS                       );


     k = iBlok_kind_SUBSYSTEM_TOPIC;

/*               pos    cardinality     title							        */
     call s( k,   1,  iCard_1_OR_MORE,  iSect_NOTES_ON                               );


     k = iBlok_kind_IO_MODULE;

/*               pos    cardinality              title						        */
     call s( k,   1,  iCard_1_ALLOWED,	         iSect_SYNTAX_AS_A_COMMAND                      );
     call s( k,   2,  iCard_1_REQUIRED,	         iSect_SYNTAX_OF_ATTACH_DESCRIPTION	      );
     call s( k,   3,  iCard_1_REQUIRED,	         iSect_FUNCTION			      );
     call s( k,   4,  iCard_1_ALLOWED,	         iSect_ARGUMENTS                                );
     call s( k,   4,  iCard_0_OR_MORE,	         iSect_ARGUMENTS_FOR                            );
     call s( k,   4,  iCard_0_OR_MORE,	         iSect_ARGUMENTS_SUBSET                         );
     call s( k,   5,  iCard_1_ALLOWED,	         iSect_CONTROL_ARGUMENTS_FOR_ATTACH_DESCRIPTION );
     call s( k,   5,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS                        );
     call s( k,   5,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_AS_A_COMMAND           );
     call s( k,   5,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_FOR                    );
     call s( k,   5,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_SUBSET                 ); 
     call s( k,   6,  iCard_1_REQUIRED,	         iSect_LIST_OF_OPENING_MODES		      );
     call s( k,   7,  iCard_1_ALLOWED,	         iSect_LIST_OF_OPERATIONS		      );
     call s( k,   8,  iCard_1_REQUIRED,	         iSect_LIST_OF_IO_OPERATIONS		      );
     call s( k,   9,  iCard_1_IN_GROUP_ALLOWED,  iSect_LIST_OF_CONTROLS		      );
     call s( k,   9,  iCard_1_IN_GROUP_ALLOWED,  iSect_LIST_OF_CONTROL_OPERATIONS	      );
     call s( k,  10,  iCard_1_ALLOWED,	         iSect_LIST_OF_MODE_STRINGS		      );
     call s( k,  10,  iCard_0_OR_MORE,	         iSect_LIST_OF                                  );
     call s( k,  11,  iCard_1_ALLOWED,	         iSect_ACCESS_REQUIRED                          );
     call s( k,  12,  iCard_1_ALLOWED,	         iSect_NOTES                                    );
     call s( k,  12,  iCard_0_OR_MORE,	         iSect_NOTES_ON                                 );
     call s( k,  13,  iCard_1_ALLOWED,	         iSect_EXAMPLES                                 );

     k = iBlok_kind_IO_OPERATION;

/*               pos    cardinality              title						        */
     call s( k,   1,  iCard_1_ALLOWED,	         iSect_SYNTAX_AS_A_COMMAND                      );
     call s( k,   2,  iCard_1_IN_GROUP_REQUIRED, iSect_SYNTAX_OF_OPEN_DESCRIPTION	      );
     call s( k,   2,  iCard_1_IN_GROUP_REQUIRED, iSect_SYNTAX_OF_CLOSE_DESCRIPTION	      );
     call s( k,   2,  iCard_1_IN_GROUP_REQUIRED, iSect_SYNTAX_OF_DETACH_DESCRIPTION	      );
     call s( k,   3,  iCard_1_REQUIRED,	         iSect_FUNCTION			      );
     call s( k,   4,  iCard_1_ALLOWED,	         iSect_ARGUMENTS_FOR_IO_CALL                    );
     call s( k,   4,  iCard_0_OR_MORE,	         iSect_ARGUMENTS                                );
     call s( k,   4,  iCard_0_OR_MORE,	         iSect_ARGUMENTS_FOR                            );
     call s( k,   4,  iCard_0_OR_MORE,	         iSect_ARGUMENTS_SUBSET                         );
     call s( k,   5,  iCard_1_REQUIRED,	         iSect_LIST_OF_OPENING_MODES		      );
     call s( k,   6,  iCard_1_IN_GROUP_ALLOWED,  iSect_CONTROL_ARGUMENTS_FOR_OPEN_DESCRIPTION   );
     call s( k,   6,  iCard_1_IN_GROUP_ALLOWED,  iSect_CONTROL_ARGUMENTS_FOR_CLOSE_DESCRIPTION  );
     call s( k,   6,  iCard_1_IN_GROUP_ALLOWED,  iSect_CONTROL_ARGUMENTS_FOR_DETACH_DESCRIPTION );
     call s( k,   6,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_AS_A_COMMAND           );
     call s( k,   6,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_FUNCTION  );
     call s( k,   6,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS                        );
     call s( k,   6,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_FOR                    );
     call s( k,   6,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_SUBSET                 ); 
     call s( k,   7,  iCard_1_ALLOWED,	         iSect_ACCESS_REQUIRED                          );
     call s( k,   7,  iCard_0_OR_MORE,	         iSect_LIST_OF                                  );
     call s( k,   7,  iCard_1_ALLOWED,	         iSect_NOTES                                    );
     call s( k,   7,  iCard_0_OR_MORE,	         iSect_NOTES_ON                                 );
     call s( k,   7,  iCard_1_ALLOWED,	         iSect_EXAMPLES                                 );


     k = iBlok_kind_IO_CONTROL;

/*               pos    cardinality              title						        */
     call s( k,   1,  iCard_1_REQUIRED,	         iSect_CONTROL_ORDER                          );
     call s( k,   2,  iCard_1_REQUIRED,	         iSect_SYNTAX			    );
     call s( k,   3,  iCard_1_REQUIRED,	         iSect_ARGUMENTS_FOR_IOX_CONTROL	    );
     call s( k,   4,  iCard_1_ALLOWED,	         iSect_LIST_OF_ELEMENTS		    );
     call s( k,   4,  iCard_1_ALLOWED,	         iSect_NOTES_ON_THE_INFO_PTR		    );
     call s( k,   5,  iCard_1_ALLOWED,	         iSect_SYNTAX_AS_A_COMMAND		    );
     call s( k,   5,  iCard_1_ALLOWED,	         iSect_SYNTAX_AS_AN_ACTIVE_FUNCTION	    );
     call s( k,   6,  iCard_1_ALLOWED,	         iSect_ARGUMENTS_FOR_IO_CALL                  );
     call s( k,   7,  iCard_1_ALLOWED,	         iSect_CONTROL_ARGUMENTS                      );
     call s( k,   7,  iCard_1_ALLOWED,	         iSect_CONTROL_ARGUMENTS_AS_A_COMMAND         );
     call s( k,   7,  iCard_1_ALLOWED,	         iSect_CONTROL_ARGUMENTS_AS_AN_ACTIVE_FUNCTION);
     call s( k,   7,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_FOR                  );
     call s( k,   7,  iCard_0_OR_MORE,	         iSect_CONTROL_ARGUMENTS_SUBSET               ); 
     call s( k,   8,  iCard_1_ALLOWED,	         iSect_ACCESS_REQUIRED                        );
     call s( k,   8,  iCard_0_OR_MORE,	         iSect_LIST_OF                                );
     call s( k,   8,  iCard_1_ALLOWED,	         iSect_NOTES                                  );
     call s( k,   8,  iCard_0_OR_MORE,	         iSect_NOTES_ON                               );
     call s( k,   8,  iCard_1_ALLOWED,	         iSect_EXAMPLES                               );

%page;
b:   proc (AkindID, Amethod, A2nd_title, A3rd_title);

  dcl  AkindID fixed bin;
  dcl  Amethod fixed bin;
  dcl  A2nd_title fixed bin;
  dcl  A3rd_title fixed bin;

     i = i + 1;
     info.block_kinds.array(i).kindID          = AkindID;
     info.block_kinds.array(i).method_or_title = Amethod;
     info.block_kinds.array(i).second_title    = A2nd_title;
     info.block_kinds.array(i).third_title     = A3rd_title;

     end b;


n:   proc (Aname_type, Afile_name, AkindID);

  dcl  Aname_type fixed bin;
  dcl  Afile_name char(32) var;
  dcl  AkindID fixed bin;

  dcl  nmI fixed bin;

     do nmI = 1 to i;
	if  info.block_kind_names.array(nmI).kindID = AkindID  then 
	if  info.block_kind_names.array(nmI).name_type ^= Aname_type  then do;
	     call ioa_ ("Statement:  call n( ^[iBlok_block_name_ENDS^;iBlok_name_ENDS^], ""^a"", ^d);
               must have a name_type = ^[iBlok_block_name_ENDS^;iBlok_name_ENDS^].",
	          Aname_type, Afile_name, AkindID, info.block_kind_names.array(nmI).name_type);
	     signal condition (info_seg_specification_error);
	     return;
	     end;
	end;

     i = i + 1;
     info.block_kind_names.array(i).name_type     = Aname_type;
     info.block_kind_names.array(i).name(1)       = Afile_name;
     info.block_kind_names.array(i).name(2)       = reverse (after (reverse(Afile_name), reverse("." || info_seg_suffix)));
     info.block_kind_names.array(i).kindID        = AkindID;

     end n;
%page;

p:   proc (Aspec, Adisplay_label, AstructID);

  dcl  Aspec char(18);
  dcl  Adisplay_label char(54) var;
  dcl  AstructID fixed bin;

     i = i + 1;
     info.file_structures.array(i).spec          = Aspec;
     info.file_structures.array(i).display_label = Adisplay_label;
     info.file_structures.array(i).structID      = AstructID;

     end p;


s:   proc (Akind, Agroup_position, Acardinality, AiSect_type);

  dcl  Akind fixed bin;
  dcl  Agroup_position fixed bin;
  dcl  Acardinality fixed bin;
  dcl  AiSect_type fixed bin;

  dcl  i fixed bin;
  dcl  j fixed bin;

     i = Akind;
     if  Akind > hbound(info.section_sequence.array,1)  then do;
	call ioa_ ("Statement:  call s("" ^d, ^d, ^a, ^d (^a)...""
            populates info.section_sequence.array(), 
            but kindID: ^d  > hbound(info.section_sequence.array,1) (= ^d)", 
	       Akind, Agroup_position, iCard_string(Acardinality), AiSect_type, 
	       info.section_titles.array(AiSect_type).title, Akind, hbound(info.section_sequence.array,1) );
	signal condition (info_seg_specification_error);
	end;

     if  AiSect_type >= i_first_BAD_title then do;
	call ioa_ ("Statement:  call s("" ^d, ^d, ^a, ^d (^a)...""
            references section type/title which is not a PREF title.",
	       Akind, Agroup_position, iCard_string(Acardinality), AiSect_type, 
	       info.section_titles.array(AiSect_type).title );
	signal condition (info_seg_specification_error);
          end;

     if  (1 <= Agroup_position) & (Agroup_position <= iSect_sequence_MAX_POSITION)  then;
						/* Title is within bounds on the group_position value     */
     else do;
	call ioa_ ("Statement:  call s("" ^d, ^d, ^a, ^d (^a)...""
            references position (^d) which is ^[less than 1^s^;greater than iSect_sequence_MAX_POSITION (^d)^]",
	     Akind, Agroup_position, iCard_string(Acardinality), AiSect_type, 
	     info.section_titles.array(AiSect_type).title, Agroup_position,
	     (Agroup_position < 1), iSect_sequence_MAX_POSITION);
	signal condition (info_seg_specification_error);
	end;

     info.section_sequence.array(i).block_kind = Akind;
     info.section_sequence.array(i).titleN = info.section_sequence.array(i).titleN + 1;
     j = info.section_sequence.array(i).titleN;

     if  j > hbound(info.section_sequence.array.title_seq,2)  then do;
	call ioa_ ("Statement:  call s("" ^d, ^d, ^a, ^d (^a)...""
            populates info.section_sequence.array().title_seq(j), 
            but j: ^d  > hbound(info.section_sequence.array.title_seq,2) (= ^d)
            Increase size of iSect_sequence_MAX_TITLES.", 
	       Akind, Agroup_position, iCard_string(Acardinality), AiSect_type, 
	       info.section_titles.array(AiSect_type).title, j, hbound(info.section_sequence.array.title_seq,2) );
	signal condition (info_seg_specification_error);
	end;

     info.section_sequence.array(i).title_seq(j).type        = AiSect_type;
     info.section_sequence.array(i).title_seq(j).position    = Agroup_position;
     info.section_sequence.array(i).title_seq(j).cardinality = Acardinality;

     end s;
%page;

t:   proc (Acmp, Atitle, Ailk, AsectID);

  dcl  Acmp fixed bin;
  dcl  Atitle char(iSect_title_MAXLENGTH_KNOWN_TITLE) var;
  dcl  Ailk fixed bin;
  dcl  AsectID fixed bin;

     i = i + 1;
     info.section_titles.array(i).cmp        = Acmp;
     info.section_titles.array(i).title      = Atitle;
     info.section_titles.array(i).ilk        = Ailk;
     info.section_titles.array(i).sectionID  = AsectID;

     if  Ailk = PREF  &  i ^= AsectID  then do;
	call ioa_ ("Statement:  call t(^[EQUALS^;BEGINS^], ""^a"", ^[PREF^;BAD ^], ^d);
	  populates info.section_titles.array(^d), 
	  but has sectionID: ^d", Acmp+1, Atitle, Ailk+1, AsectID, i, AsectID);
	signal condition (info_seg_specification_error);
	end;

     if  Ailk = BAD  then do;
	if  i_first_BAD_title = 0  then 
	     i_first_BAD_title = i;

	if  AsectID >= i_first_BAD_title  then do;
	     call ioa_ ("Statement:  call t(^[EQUALS^;BEGINS^], ""^a"", ^[PREF^;BAD ^], ^d);
	       has sectionID: ^d
                 PREF sectionIDs have values < ^d", Acmp+1, Atitle, Ailk+1, AsectID, AsectID, i_first_BAD_title);
	     signal condition (info_seg_specification_error);
	     end;

	else if  Acmp = EQUALS  &  info.section_titles.array(AsectID).cmp ^= Acmp  then do;
	     call ioa_ ("Statement:  call t(EQUALS, ""^a"", BAD, ^d);
	       has sectionID: ^d
                      with cmp: BEGINS (must be EQUALS)", Acmp+1, Atitle, Ailk+1, AsectID, AsectID);
	     signal condition (info_seg_specification_error);
	     end;
	end;

     end t;

%page;
/* Now setup the call to create the mbuild_info_ data base */

  dcl  1 cdsa aligned like cds_args;
  dcl  DATA_NAME char (25) aligned internal static options (constant) init("info_seg_specifications_"),
       exclude_pad (1) char (32) aligned static options (constant) init("pad*");

     cdsa.sections (1).p = addr (info);
     cdsa.sections (1).len = size (info);
     cdsa.sections (1).struct_name = "info";
     cdsa.seg_name = DATA_NAME;
     cdsa.num_exclude_names = 1;
     cdsa.exclude_array_ptr = addr (exclude_pad);
     string (cdsa.switches) = "0"b;
     cdsa.switches.have_text = "1"b;
     call create_data_segment_ (addr (cdsa), code);
     return;

%page;
% include cds_args;

     end info_seg_specifications_;
