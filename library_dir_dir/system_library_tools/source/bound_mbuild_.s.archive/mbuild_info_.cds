/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 2019 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(2019-08-17,GDixon), approve(2019-10-24,MCR10069),
     audit(2020-01-20,Swenson), install(2020-01-20,MR12.6g-0035):
     Database containing specifics of segments installed in the Multics
     Libraries. For details, see:  MTB-1003  mbuild Subsystem
                                                   END HISTORY COMMENTS */


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* This program populates the mbuild_info_ data structure for seg_types and bld_paradigms; and    */
	/* thread types and summaries.  All of these items tailor operation of the mbuild subsystem.      */
	/*									        */
	/* Use entrypoints in the mbuild_info_find_ subroutine to access elements of this structure.      */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */



(stringsize):					/* Condition checks enabled to assist with updates to     */
(subscriptrange):					/*  this data.  If user adds initialization data without  */
mbuild_info_:					/*  changing array dimensions, that will be detected by   */
	proc;					/*  the cds translator.			        */

	      
%page;
dcl  create_data_segment_ entry (ptr, fixed bin (35));

dcl  1 cdsa	     aligned like cds_args;

dcl  code		     fixed bin (35);

dcl  DATA_NAME	     char (12) aligned static init
                         ("mbuild_info_") options (constant),
     exclude_pad         (1) char (32) aligned static options (constant) init
		     ("pad*");

dcl (dimension, addr, size, string)	     builtin;

%include mbuild_info_;				/* Structures referenced in like attributes in the        */
						/*  following declarations.			        */

dcl 1 info aligned,
      2 seg_types,
        3 segN fixed bin,
        3 seg_info (23) like mbuild_info.seg_type_info,	/* Change dimension as BUILD SEGMENT TYPES are added or   */
						/*  removed.				        */
      2 bld_paradigms,
        3 paradigmN fixed bin,
        3 bld_paradigm_info (11) like mbuild_info.bld_paradigm_info,
						/* Change dimension if BUILD PARADIGMS are added or       */
						/*  removed.				        */
      2 thread_selectors,
        3 selectorN fixed bin,
        3 selector_info (33) like mbuild_info.selector_info,
						/* Change dimension if new Tlist_base substructures       */
						/*  are added to build_data structure, declared in:       */
						/*    mbuild_data.incl.pl1			        */
						/*  or if short names are added/removed for a given       */
						/*  threaded list.				        */
      2 thread_summary,
        3 summaryN fixed bin,
        3 selector_summary (16) char(32) var;		/* Change dimension if new threaded list is added to      */
						/*  the mbuild build_data structure.		        */

	info.seg_types.segN = dimension(info.seg_types.seg_info, 1);
	info.bld_paradigms.paradigmN = dimension(info.bld_paradigms.bld_paradigm_info, 1);
	info.thread_selectors.selectorN = dimension(info.thread_selectors.selector_info, 1);
	info.thread_summary.summaryN = dimension(info.thread_summary.selector_summary, 1);
						/* Populate the xxxN variables from declared array        */
						/*  dimensions.				        */

  dcl i fixed bin;					/* Index into arrays.			        */
  dcl j fixed bin;					/* info.sel_value.				        */
  dcl k fixed bin;					/* info.selector_summary index.		        */
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* BUILD PARADIGMS								        */
	/*									        */
	/* Define name, purpose, example segments that follow/exemplify this paradigm, and brief set of   */
	/* paradigm steps used to build/install segments following this paradigm.		        */
	/*									        */
	/* When adding a new entry to this table, remember to update the dimension of the appropriate     */
	/* array in the structure declaration at top of this file.				        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

	i = PDM_source;
	info.name(i) = "source";
	info.purpose(i) = "Compile source; add/replace/delete source and object in archives.";
	info.examples(i) = "v2pl1.pl1";
	info.steps(i) = "
 I. For replace/add of source:
    1. Compile source.
    2. If source is part of bound_xxx_.**.s.archive:
       a. Update source in its .s.archive.
       b. Update compiled object in corresponding .archive." || "
       c. update_seg add/replace changed source archive in >ldd>LIBRARY>s.
       d. Schedule bind of object archives comprising bound_xxx_.
    3. Else for non-archived source:" || "
       a. update_seg add/replace source in its >ldd>LIBRARY>s dir.
       b. update_seg add/replace derived object in >ldd>LIBRARY>o and >LIBRARY.
 II. For delete of source:
    1. If source is part of bound_xxx_.**.s.archive:" || "
       a. Delete source from bound_xxx_.**.s.archive.
       b. Delete derived object from corresponding object archive.
       c. update_seg replace changed source archive in >ldd>LIBRARY>s." || "
       d. update_seg replace changed object archive in >ldd>LIBRARY>o.
       d. Schedule bind of changed object archive.
    2. Else for non-archived source:
       a. update_seg delete the source in >ldd>LIBRARY>s." || "
       b. update_seg delete derived object in >ldd>LIBRARY>o and >LIBRARY.";


	i = PDM_bindfile;
	info.name(i) = "bindfile";
	info.purpose(i) = "Add/replace/delete .bind file in object archive.";
	info.examples(i) = "bound_pl1_.bind";
	info.steps(i) = "
 1. Replace/add/delete bind file in bound_xxx_.**.archive.
 2. update_seg add/replace changed object archive in >ldd>LIBRARY>o.
 3. Schedule bind of object archive(s) comprising bound_xxx_.";


	i = PDM_source_arch;
	info.name(i) = "source_arch";
	info.purpose(i) = "Install source archive into source directory.";
	info.examples(i) = "bound_pl1_.**.s.archive";
	info.steps(i) = "
 1. update_seg add/replace/delete source archive in >ldd>LIBRARY>s.";


	i = PDM_object_arch;
	info.name(i) = "object_arch";
	info.purpose(i) = "Bind and install object archives into object directory.";
	info.examples(i) = "bound_pl1_.**.archive (except bound_*.**.s.archive)";
	info.steps(i) = "
 I. For replace/add of bound object archives:
    1. Bind all bound_xxx_.**.archive comprising bound_xxx_.
    2. update_seg add/replace changed object archive in >ldd>LIBRARY>o dir.
    3. update_seg add/replace bound_xxx_ in >LIBRARY dir." || "
 II. For delete of some bound object archives comprising bound_xxx_:
    1. Bind remaining bound_xxx_.**.archives.
    2. update_seg delete of targeted object archives from >ldd>LIBRARY>o.
    3. update_seg replace bound_xxx_ in >LIBRARY." || "
 III. For delete of entire bound_xxx_:
    1. update_seg delete of bound_xxx_.**.archive from >ldd>LIBRARY>o.
    2. update_seg delete bound_xxx_ from >LIBRARY.";


	i = PDM_Bound_obj;
	info.name(i) = "Bound_obj";
	info.purpose(i) = "Install executable bound segment into execution dir.";
	info.examples(i) = "bound_pl1_";
	info.steps(i) = "
 1. Bind of comprising archives will add/replace/delete bound object in >LIBRARY.";

%page;
	i = PDM_Unbound_obj;
	info.name(i) = "Unbound_obj";
	info.purpose(i) = "Install executable unbound segment into execution dir.";
	info.examples(i) = "bisync_, installation_gate_, edm";
	info.steps(i) = "
 1. If object is part of an object archive, bound_xxx_.**.archive:
    a. Object would be built as part of compile of its source file.
 2. Else of non-archived object:
    a. update_seg add/replace/delete target in >ldd>LIBRARY>o and >LIBRARY.";


	i = PDM_target_only;
	info.name(i) = "target_only";
	info.purpose(i) = "Install segment into info or include directory.";
	info.examples(i) = "info segs, includes files";
	info.steps(i) = "
 1. update_seg add/replace/delete of target in library dir chosen by target's suffix.";


	i = PDM_listing;
	info.name(i) = "listing";
	info.purpose(i) = "Install .list file in listings directory.";
	info.examples(i) = "pl1.list, hcs_.list";
	info.steps(i) = "
 1. If listings are being retained/installed, update_seg add/replace/delete .list 
    file in appropriate >ldd>listings>LIBRARY directory.";


	i = PDM_source_x_only;
	info.name(i) = "source_x_only";
	info.purpose(i) = "Install source seg or unbound archive into source and execution directories.";
	info.examples(i) = "admin.ec, tss_basic_.archive";
	info.steps(i) = "
 1. update_seg add/replace/delete source file in >ldd>LIBRARY>s and >LIBRARY.";


	i = PDM_object_x_only;
	info.name(i) = "object_x_only";
	info.purpose(i) = "Install segment into object and execution directories.";
	info.examples(i) = "TTF.ttf, pl1.dcl";
	info.steps(i) = "
 1. update_seg add/replace/delete file in >ldd>LIBRARY>o and >LIBRARY.";


	i = PDM_mbuild_support;
	info.name(i) = "mbuild_support";
	info.purpose(i) = "Support file created/used by mbuild; not installed.";
	info.examples(i) = "MCR10056.mb, MCR10056.mb.ec";
	info.steps(i) = "
 1. File residing safely in a build/install preparation directory.
     a. Created and used by mbuild subsystem as build or install progresses
        for all segments in the prep directory." || "
     b. Never removed by a clean request.
     c. Often replaced or extended by mbuild requests.";

%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* BUILD SEGMENT TYPES							        */
	/*									        */
	/* A segment to be installed is compared with:  source_starname			        */
	/* and searching stops with the first matching entry.  Order of entries below matters.	        */
	/*									        */
	/* When adding a new entry to this table, remember to update the dimension of the appropriate     */
	/* array in the structure declaration at top of this file.				        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

	i = 1;
	info.source_starname(i) = "bound_*";		/* Bound Executable */
	info.description(i) = "Executable Bound Segment";
	info.mbuild_type(i) = "Bound_obj";
	info.build_paradigm(i) = PDM_Bound_obj;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "execution";
	info.type_ID(i) = i;
	i = i + 1;

          info.source_starname(i) = "*";		/* Single-component File Name			        */
	info.description(i) = "Standalone Segment";
	info.mbuild_type(i) = "Unbound_obj";
	info.build_paradigm(i) = PDM_Unbound_obj;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "execution";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "bound_*.bind";	/* BIND (bind directive file) */
	info.description(i) = "Bind Directives";
	info.mbuild_type(i) = "bindfile";
	info.build_paradigm(i) = PDM_bindfile;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "object";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "bound_*.**.s.archive"; /* Source ARCHIVE */
	info.description(i) = "Source Archive";
	info.mbuild_type(i) = "source_arch";
	info.build_paradigm(i) = PDM_source_arch;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "source";
	info.type_ID(i) = i;
	i = i + 1;
%page;
	info.source_starname(i) = "bound_*.**.archive";	/* Object ARCHIVE */
	info.description(i) = "Object Archive";
	info.mbuild_type(i) = "object_arch";
	info.build_paradigm(i) = PDM_object_arch;
	info.compiler(i) = "bind";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "object";
	info.type_ID(i) = i;
	i = i + 1;

          info.source_starname(i) = "**.incl.*";		/* INCL */
	info.description(i) = "Include File";
	info.mbuild_type(i) = "Include";
	info.build_paradigm(i) = PDM_target_only;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "include";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.pl1";		/* PL/1 */
	info.description(i) = "PL/I Source File";
	info.mbuild_type(i) = "source";
	info.build_paradigm(i) = PDM_source;
	info.compiler(i) = "pl1";
	info.default_compile_options(i) = "-ot";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "source";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.alm";		/* ALM */
	info.description(i) = "ALM (assembler) Source File";
	info.mbuild_type(i) = "source";
	info.build_paradigm(i) = PDM_source;
	info.compiler(i) = "alm";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "source";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.lisp";		/* LISP */
	info.description(i) = "LISP Source File";
	info.mbuild_type(i) = "source";
	info.build_paradigm(i) = PDM_source;
	info.compiler(i) = "lisp_compile";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "source";
	info.type_ID(i) = i;
	i = i + 1;
%page;	
	info.source_starname(i) = "**.cds";		/* CDS */
	info.description(i) = "Data Segment Source File";
	info.mbuild_type(i) = "source";
	info.build_paradigm(i) = PDM_source;
	info.compiler(i) = "cds";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = ".pl1";
	info.object_suffix(i) = "";
	info.default_library(i) = "source";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.ld";		/* LD (library_descriptor_compiler) */
	info.description(i) = "Library Descriptor Source File";
	info.mbuild_type(i) = "source";
	info.build_paradigm(i) = PDM_source;
	info.compiler(i) = "ldc";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = ".alm";
	info.object_suffix(i) = "";
	info.default_library(i) = "source";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.rd";		/* RD (reductions) */
	info.description(i) = "Reduction Language Source File";
	info.mbuild_type(i) = "source";
	info.build_paradigm(i) = PDM_source;
	info.compiler(i) = "rdc";
	info.default_compile_options(i) = "-ot -trace off";
	info.intermediate_suffix(i) = ".pl1";
	info.object_suffix(i) = "";
	info.default_library(i) = "source";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.pl1.pmac";	/* PL/1 with macros */
	info.description(i) = "PL/I Source File with Macro Definitions";
	info.mbuild_type(i) = "source";
	info.build_paradigm(i) = PDM_source;
	info.compiler(i) = "pmac";
	info.default_compile_options(i) = "-call ""pl1 -ot"" ";
	info.intermediate_suffix(i) = ".pl1";
	info.object_suffix(i) = "";
	info.default_library(i) = "source";
	info.type_ID(i) = i;
	i = i + 1;

          info.source_starname(i) = "**.mb.ec";		/* mbuild Installation Object Creation Exec_com  */
	info.description(i) = "mbuild exec_com ";
	info.mbuild_type(i) = "Build_exec_com";
	info.build_paradigm(i) = PDM_mbuild_support;
	info.compiler(i) = "";			/* This type of file is not compiled during installation. */
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "";			/* Never installed.				        */
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.mb.il";		/* mbuild Log File */
	info.description(i) = "update_seg Installation Log";
	info.mbuild_type(i) = "Build_log";
	info.build_paradigm(i) = PDM_mbuild_support;
	info.compiler(i) = "";			/* This type of file is not compiled during installation. */
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "";			/* Never installed.				        */
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.mb.io";		/* mbuild Log File */
	info.description(i) = "update_seg Installation Object";
	info.mbuild_type(i) = "Build_io";
	info.build_paradigm(i) = PDM_mbuild_support;
	info.compiler(i) = "";			/* This type of file is not compiled during installation. */
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "";			/* Never installed.				        */
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.ec";		/* EC (exec_com) */
	info.description(i) = "Executable Command File";
	info.mbuild_type(i) = "exec_com";
	info.build_paradigm(i) = PDM_object_x_only;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "execution";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.info";		/* INFO */
	info.description(i) = "Info Segment";
	info.mbuild_type(i) = "Info";
	info.build_paradigm(i) = PDM_target_only;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "info";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.mb";		/* mbuild Build Script File */
	info.description(i) = "mbuild Build Script";
	info.mbuild_type(i) = "Build_script";
	info.build_paradigm(i) = PDM_mbuild_support;
	info.compiler(i) = "";			/* This type of file is not compiled during installation. */
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "";			/* Never installed.				        */
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.list";		/* Listing File */
	info.description(i) = "Listing File";
	info.mbuild_type(i) = "listing";
	info.build_paradigm(i) = PDM_listing;
	info.compiler(i) = "";			/* This type of file is not compiled during installation. */
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "listings";		/* Particular library listings.(hard, mcs, sss) chosen at */
	info.type_ID(i) = i;
	i = i + 1;				/*  analysis time.				        */

	info.source_starname(i) = "**.dcl";		/* DCL (Declarations) */
	info.description(i) = "PL/I Declaration Definitions";
	info.mbuild_type(i) = "dcl_file";
	info.build_paradigm(i) = PDM_object_x_only;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "standard.execution";
	info.type_ID(i) = i;
	i = i + 1;
%page;
	info.source_starname(i) = "**.ttf";		/* TTF (Terminal Type File) */
	info.description(i) = "Terminal Type Definitions File";
	info.mbuild_type(i) = "TTF_file";
	info.build_paradigm(i) = PDM_object_x_only;
	info.compiler(i) = "";			/* This type of file is not compiled during installation. */
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "tools.execution";
	info.type_ID(i) = i;
	i = i + 1;

	info.source_starname(i) = "**.archive";		/* Data ARCHIVE */
	info.description(i) = "Data Archive";
	info.mbuild_type(i) = "data_arch";
	info.build_paradigm(i) = PDM_object_x_only;
	info.compiler(i) = "";
	info.default_compile_options(i) = "";
	info.intermediate_suffix(i) = "";
	info.object_suffix(i) = "";
	info.default_library(i) = "execution";
	info.type_ID(i) = i;
	i = i + 1;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Thread Selectors								        */
	/*									        */
	/* In mbuild_data_.incl.pl1, the build_data structure declares the anchor points for several      */
	/* kinds of threaded list.							        */
	/*									        */
	/*  List Anchor ID								        */
	/*    Each anchor is declared like a Tlist_base structure in an element named build_data.XXX_Tb,  */
	/*    where XXX is one of the list anchor IDs shown below.  For example, the threaded list of     */
	/*    COMPILE structures has an anchor declared in:  build_data.COMPILE_Tb		        */
	/*									        */
	/*  Structure								        */
	/*    Identifies the structure declaration for items in the threaded list.		        */
	/*									        */
	/*  Selector ID								        */
	/*    Gives -list OPERAND values for selecting a List Anchor ID to display or act on.	        */
	/*									        */
	/*  Purpose								        */
	/*    Gives a short phrase describing overall purpose of the threaded list.		        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

/* ------
   Threads anchored in build_data structure (see mbuild_data_.incl.pl1):
     
     List Anchor ID     Structure      Selector ID            Purpose
    ----------------   ------------   ----------------       --------------------
     BOUNDOBJ          BOUNDOBJ       BOUNDOBJ, BND          All BOUNDOBJ structures.
     COMPILE           COMPILE        COMPILE, COMP          All COMPILE structures.
     UNBOUNDOBJ        UNBOUNDOBJ     UNBOUNDOBJ, UNBND      All UNBOUNDOBJ structures.
     Seg               Seg            Seg, seg               All Seg structures.

     Unbound_obj       Seg            Unbound_obj, Unb       All Seg(Unbound_obj) structures.
     Bound_obj         Seg            Bound_obj, Bnd         All Seg(Bound_obj) structures.
     bindfile          Seg            bindfile, bind         All Seg(bindfile) structures.
     object_arch       Seg            object_arch, oArch     All Seg(object_arch) structures.
     source_arch       Seg            source_arch, sArch     All Seg(source_arch) structures.
     source            Seg            source, src            All Seg(source) structures, except intermediate sources.
     listing           Seg            listing, list          All Seg(listing) structures.
     mbuild_support    Seg            support, sup           All Seg(Build_xxx) structures.
     target_only       Seg            target_only, target    All Seg(...) structures except those above/below.
     object_x_only     Seg            object_x, o.x	 All Seg(...) structures for object_x paradigm segs.

     scan              Seg	        scan, sc	           Segments found in install dir before building; never cleaned!
     INTERMEDIATE      Seg	        INTERMEDIATE, inter    Some Seg(source) structures that aren't installed.
   ---------------------------------------------------------------------- */
%page;
	i, k = 0;
	j = STRUCT_BOUNDOBJ;    
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "BOUNDOBJ";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "BND";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = STRUCT_COMPILE;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "COMPILE";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "COMP";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = STRUCT_UNBOUNDOBJ;    
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "UNBOUNDOBJ";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "UNBND";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = STRUCT_Seg;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "Seg";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "seg";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);


	j = SEG_INTERMEDIATE;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "INTERMEDIATE";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "inter";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = SEG_scan;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "scan";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "sc";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);


	j = PDM_Unbound_obj;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "Unbound_obj";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "Unb";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_Bound_obj;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "Bound_obj";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "Bnd";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_bindfile;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "bindfile";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "bind";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_object_arch;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "object_arch";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "oArch";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_source_arch;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "source_arch";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "sArch";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_source;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "source";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "src";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_listing;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "listing";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "list";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_mbuild_support;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "mbuild_support";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "support";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "sup";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-2) || ", " || info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_target_only;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "target_only";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "target";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);

	j = PDM_object_x_only;
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "object_x_only";
	i = i + 1;  info.sel_value(i) = j;	info.sel_ID(i) = "o.x";
	k = k + 1;  info.selector_summary(k) = info.sel_ID(i-1) || ", " || info.sel_ID(i);
%page;

/* Now setup the call to create the mbuild_info_ data base */

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
	end mbuild_info_;
