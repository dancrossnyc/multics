
/* HISTORY COMMENTS:
  1) change(2019-08-17,GDixon), approve(2019-10-25,MCR10069),
     audit(2020-01-20,Swenson), install(2020-01-20,MR12.6g-0035):
     Program to parse Build Script Language data found in an mbuild
     Build_script file. For details, see:  MTB-1003  mbuild Subsystem
  2) change(2020-08-29,GDixon), approve(2021-02-22,MCR10086),
     audit(2021-03-17,Swenson), install(2021-03-17,MR12.6g-0051):
      A) Support new code parameter when calling mbuild_data_$scan_Tb_insert(...).
         Detect/report duplicate names used in script file. (Ticket #213)
      B) Add Alines_preceding_script parameter to permit correct line numbers
         in error messages.
      C) Add unknown_entryname error diagnosing attempt to DELETE a Seg
         not found in library.
      D) Fix error in token display by Err messages: bad_path, bad_library.
                                                   END HISTORY COMMENTS */

%page;
/* ------------------------------------------------------------------------------------------

   SUBROUTINE:  mbuild_script_parse_

   This program parses the body of files written in Build Script Language.

   ----------                                                                      ----------

BNF for Build Script Language (used in MCRnnnnn.mb segs):

    -  { <alt-1> | <alt-2> | <alt-3> } identifies alternatives.  Choose only one of them.
       { <alt-1> | <alt-2> | <alt-3> }... says choose one or more of the alternatives. 

    -  [....] says stuff is optional.  [....]... says choose zero or more of this item.

    -  Stuff in /^ .. ^/ are comments added by me in a few cases.  

    -  User commenting may or may not be supported in the actual build script files.  
       Such comments would have to be parsed/stored by read operation to be preserved for
       future save operations; and mbuild would have to learn how to display them.

   ----------                                                                      ----------
%page;
   ----------                                                                      ----------
<script-contents>       ::= [<mb-script-item>]... <installable-item>... 

<installable-item>      ::= {<bound-object-group>|<unbound-object-group>|
                              <info-seg-group>|<include-file-group>|<unanalyzed-seg-group>}


<bound-object-group>    ::= <bound-object-stmt> [<bindfile-stmt>] [<archive-source-group>]... 

 <archive-source-group> ::  {<source-stmt>|<archive-stmt> <source-stmt>...}

<unbound-object-group>  ::= <unbound-object-stmt> <source-stmt> [<naming-group>]...

<include-file-group>    ::= <include-file-stmt> ;

<info-seg-group>        ::= <info-seg-stmt> [<naming-group>]...

<unanalyzed-seg-group>  ::= <unanalyzed-seg-stmt> [<naming-group>]... ;

<bound-object-stmt>     ::= Bound_obj: <bound-object-name> <library> <bound-obj-operation> ;
  <library>             ::=  IN: <library-name>
  <bound-obj-operation> ::=  {ADD|UPDATE|DELETE}

<bindfile-stmt>         ::=  bindfile: <bind-file-name> [<library>] <operation> ;
  <operation>           ::=  {ADD|REPLACE|DELETE}

<archive-stmt>          ::=  source_arch:  <bound-source-archive-name> [<library>] <bound-obj-operation> ;

<source-stmt>           ::=  source:   <source-seg-name> [<library>] <operation> [<compiler-group>]
  <compiler-group>      ::=   <compiler> [<compile-option>]... ; 
  <compiler>            ::=   compiler: <compiler-name>
  <compile-option>      ::=   {<control-arg>|<control-arg> <value-word>}


<unbound-object-stmt>   ::= Unbound_obj: <object-seg-primary-name> <library> <operation> ;

<include-file-stmt>     ::= Include: <include-file-name> [<library>] <operation>;

<info-seg-stmt>         ::= Info: <info-seg-primary-name> <library> <operation> ;

<unanalyzed-seg-stmt>   ::= Seg(<seg-type>): <seg-name> <operation> ;


<naming-group>          ::= <add_name-stmt>|<delete_name-stmt>
 <add_name-stmt>        ::=  add_name: <add-seg-name>... ;
 <delete_name-stmt>     ::=  delete_name: <del-seg-name>... ;


<mb-script-item>        ::= {<mb-script-stmt>|<mb-exec_com_stmt>|<mb-log-stmt>}

 <mb-script-stmt>       ::= Build_script: <mb-seg-name>.mb ;

 <mb-exec_com-stmt>     ::= Build_exec_com: <mb-seg-name>.mb.ec ;

 <mb-log-stmt>          ::= Build_log: <mb-seg-name>.mb.list ;
 

   ----------                                                                      ----------
%page;
   ----------                                                                      ----------
Flattened Description of Build Script Language:

 The script language given as BNF above generates statements such as those shown below.  
 Items in braces are alternatives; only one is present in an actual statement.

 An archive statement is used only for a bound object having more than one source archive
 (i.e., where source archive has a .<digit> in its name; e.g. bound_pl1_.2.s.archive).

 An add_name or delete_name statement not used on a Bound_obj statement; names on a
 bound segment are specified by statements in its bindfile.


Build_script    : <mb-seg-name>.mb ;
Build_exec_com  : <mb-seg-name>.mb.ec ;
Build_log       : <mb-seg-name>.mb.list ;

Bound_obj       : <bound-object-name>       IN : <library-name>   { ADD | UPDATE  | DELETE }  ;
    bindfile    : <bind-file-name>                                { ADD | REPLACE | DELETE }  ;
 source_arch    : <bound-source-archive-name>                     { ADD | UPDATE  | DELETE }  ;
      source    : <source-seg-name>                               { ADD | REPLACE | DELETE }  <compiler-group> ;
      
Unbound_obj     : <object-seg-name>         IN : <library-name>   { ADD | REPLACE | DELETE }  ;
      source    : <source-seg-name>                               { ADD | REPLACE | DELETE }  <compiler-group> ;

Include         : <include-file-name>       IN : <library_name>   { ADD | REPLACE | DELETE }  ;
Info            : <info-seg-primary-name>   IN : <library_name>   { ADD | REPLACE | DELETE }  ;

Seg(<seg-type>) : <seg-name>                IN : <library_name>   { ADD | REPLACE | DELETE }  ;

Either/both of these statements can follow:  Unbound_obj Info Seg
add_name        : <name>... ; 
delete_name     : <name>... ;


%page;
/*++
INCLUDE NEXT_STMT \
INCLUDE LEX \
BEGIN     / Installation_directory : <pathname> ;
                                         / LEX(2)  set_inst_dir                    NEXT_STMT                 / MAJ_STMT  \
          / Installation_directory : <any-token>   \" <pathname> reports this error.
                                         / LEX(2)                                  NEXT_STMT                 / MAJ_STMT  \
          / <any-token>                  /         Err(unexpected_stmt)            NEXT_STMT                 / RETURN    \
          / <no-token>                   /         Err(empty_script)                                         / RETURN    \

MAJ_STMT  / Build_script   : <ename> ;   /         MAJ_stmt  LEX(2) MAJ_set_name   NEXT_STMT                 / MAJ_STMT  \
          / Build_exec_com : <ename> ;   /         MAJ_stmt  LEX(2) MAJ_set_name   NEXT_STMT                 / MAJ_STMT  \
          / Build_log      : <ename> ;   /         MAJ_stmt  LEX(2) MAJ_set_name   NEXT_STMT                 / MAJ_STMT  \
          / Build_io       : <ename> ;   /         MAJ_stmt  LEX(2) MAJ_set_name   NEXT_STMT                 / MAJ_STMT  \
          / Bound_obj      : <ename>     /         MAJ_stmt  LEX(2) MAJ_set_name   LEX  PUSH(BoundSTMT)      / CLAUSE    \
          / Unbound_obj    : <ename>     /         MAJ_stmt  LEX(2) MAJ_set_name   LEX  PUSH(UnbSTMT)        / CLAUSE    \
          / Include        : <ename>     /         MAJ_stmt  LEX(2) MAJ_set_name   LEX  PUSH(MAJ_STMT)       / CLAUSE    \
          / Info           : <ename>     /         MAJ_stmt  LEX(2) MAJ_set_name   LEX  PUSH(SegSTMT)        / CLAUSE    \
          / Seg ( <type> ) : <ename>     / LEX(2)  MAJ_stmt  LEX(3) MAJ_set_name   LEX  PUSH(SegSTMT)        / CLAUSE    \
          / Seg ( <any-token>            / LEX(2)  Err(unsupported_build_type)     NEXT_STMT                 / MAJ_STMT  \
          / <any-token>                  /         Err(invalid_stmt)               NEXT_MAJ_STMT             / MAJ_STMT  \
          / <no-token>                   /                                                                   / RETURN    \

                                         \" Only one bindfile stmt permitted per Bound_obj.
				 \"  Many source_arch and/or source stmt per Bound_obj are OK.
BoundSTMT / bindfile       : <ename>     /         bindf_st  LEX(2) set_seg_name   LEX  PUSH(BoundSTM2)      / CLAUSE    \
          / source_arch    : <ename>     /         sArch_st  LEX(2) set_seg_name   LEX  PUSH(BoundSTMT)      / CLAUSE    \
          / source         : <ename>     /         source_st LEX(2) set_seg_name   LEX  PUSH(BoundSTMT)      / sourceCL  \
endSTMT   / <any-token>                  /                                                                   / MAJ_STMT  \
          / <no-token>                   /                                                                   / RETURN    \

BoundSTM2 / source_arch    : <ename>     /         sArch_st  LEX(2) set_seg_name   LEX  PUSH(BoundSTM2)      / CLAUSE    \
          / source         : <ename>     /         source_st LEX(2) set_seg_name   LEX  PUSH(BoundSTM2)      / sourceCL  \
          /                              /                                                                   / endSTMT   \

UnbSTMT   / add_name       : <ename>     / LEX(2)  begin_add_name                  LEX  PUSH(UnbSTMT)        / moreNames \
          / delete_name    : <ename>     / LEX(2)  begin_delete_name               LEX  PUSH(UnbSTMT)        / moreNames \
          / source         : <ename>     /         source_st LEX(2) set_seg_name   LEX  PUSH(endSTMT)        / sourceCL  \
          /                              /                                                                   / endSTMT   \

SegSTMT   / add_name       : <ename>     / LEX(2)  begin_add_name                  LEX  PUSH(SegSTMT)        / moreNames \
          / delete_name    : <ename>     / LEX(2)  begin_delete_name               LEX  PUSH(SegSTMT)        / moreNames \
          /                              /                                                                   / endSTMT   \


CLAUSE    / IN : <library>               / LEX(2)  set_library                     LEX                       / CLAUSE    \
	/ IN :			 /				   NEXT_STMT                 / STACK_POP \
          / <operation>                  /         set_operation                   LEX                       / CLAUSE    \
endCLAUSE / ;                            /         end_seg                         LEX                       / STACK_POP \
          / <any-token>                  /         Err(bad_clause)                 NEXT_STMT                 / STACK_POP \
          / <no-token>                   /         Err(incomplete_stmt)                                      / RETURN    \
                                      

sourceCL  / IN : <library>               / LEX(2)  set_library                     LEX                       / sourceCL  \
	/ IN : <any-token>		 /                                         LEX(3)		         / sourceCL  \
          / <operation>                  /         set_operation                   LEX                       / sourceCL  \
          / compiler        : <ename>    / LEX(2)  set_compiler                    LEX                       / compOPT   \
          /                              /                                                                   / endCLAUSE \

                                         \" Any token not a statement delimiter is 
                                         \"  appended to compile_options string.
compOPT   / ;                            /         end_seg                         NEXT_STMT                 / STACK_POP \
          / <any-token>                  /         set_compile_option              LEX                       / compOPT   \
          / <no-token>                   /         Err(incomplete_stmt)                                      / RETURN    \


moreNames / <ename>                      /         another_name                    LEX                       / moreNames \
          / ;                            /         end_name                        NEXT_STMT                 / STACK_POP \
          / <any-token>                  /         Err(bad_name)                   LEX                       / moreNames \
          / <no-token>                   /         Err(incomplete_stmt)                                      / RETURN    \
  ++*/

%page;

mbuild_script_parse_: 
     proc (Abuild_dataP, AscriptP, AscriptL, Alines_preceding_script, Adebug, Acode);
     
  dcl  AscriptP ptr;
  dcl  AscriptL fixed bin(21);
  dcl  Alines_preceding_script fixed bin;
  dcl  Adebug fixed bin;
  dcl  Acode fixed bin(35);

  dcl  script char(scriptL) based(AscriptP),                /* Build script                                           */
       script_array (scriptL) char(1) based(AscriptP);

                                                            /* BUILTINS                                               */
  dcl (after, before, collate, index, null, size, search, substr, verify) builtin;

                                                            /* CONSTANTS                                              */
  dcl  ZEROb init(""b) bit(0) aligned int static options(constant);
       
  dcl  mbuild_et_$bad_build_script fixed bin(35) ext static;


  dcl  breakCharsNotTokenized char(5) varying init(" " || substr(collate(),10,4));
                                                            /* SP HT NL VT NP                                         */
  dcl  breakChars char(10) varying init("(),:;");

     breakChars = breakChars || breakCharsNotTokenized;     /* PL/I does not permit this via init statements.         */

  dcl  dStringCharsToIgnore fixed bin(21) int static options(constant) init(0);
  dcl  lexControlChars char(128) varying int static;
  dcl  lexDelims char(128) varying int static;
  dcl  lexInitialized bit(1) aligned int static init("0"b);
  dcl  lexSwitches_Stmts bit(4) int static options(constant) init("1"b);
  dcl (quoteOpenUNUSED, quoteClosedUNUSED, 
       commentOpenUNUSED, commentClosedUNUSED) char(0) int static options(constant) init("");
  dcl  statementDelim char(1) int static options(constant) init(";");
  dcl (statementP, tokenFirstP) ptr;

  dcl  lex_string_$init_lex_delims entry (char(*), char(*), char(*), char(*), char(*), bit(*), char(*) var, char(*) var,
          char(*) var, char(*) var);
  dcl  lex_string_$lex entry (ptr, fixed bin(21), fixed bin(21), ptr, bit(*), char(*), char(*), char(*), char(*),
          char(*), char(*) var, char(*) var, char(*) var, char(*) var, ptr, ptr, fixed bin(35));
  dcl translator_temp_$allocate entry (ptr, fixed bin) returns(ptr);

     if ^lexInitialized then do;
          call lex_string_$init_lex_delims(
               quoteOpenUNUSED, quoteClosedUNUSED, commentOpenUNUSED, commentClosedUNUSED,
               statementDelim, lexSwitches_Stmts, breakChars, breakCharsNotTokenized,
               lexDelims, lexControlChars);
          lexInitialized = T;
          end;


     statementP, tokenFirstP = null();                      /* start out with no input tokens.                        */
     call lex_string_$lex(
          AscriptP, AscriptL, dStringCharsToIgnore, 
          bld.areaP, lexSwitches_Stmts,
          quoteOpenUNUSED, quoteClosedUNUSED, commentOpenUNUSED, commentClosedUNUSED,
          statementDelim, breakChars, breakCharsNotTokenized, lexDelims, lexControlChars,
          statementP, tokenFirstP, Acode);
     if Acode ^= 0 then 
          return;

          /*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
          /*                                                                                                */
          /* For testing purposes, display the tokens found by lex_string_.                                 */
          /*                                                                                                */
          /*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

  dcl (ioa_, ioa_$nnl) entry() options(variable);

     if Adebug >= 2 then do;
          call ioa_("^/Tokens from .build file: ----------------------------------");
          do Ptoken = tokenFirstP repeat token.Pnext while (Ptoken ^= null());
               call displayToken();
               end;
          call ioa_("");
          end;

     tier(*) = "";					/* Declared below, in Semantic Action Routines	        */
     addP, delP, SegP, UNBOUNDOBJp = null();
     depthI = 0;

  dcl  max_error_severity char(1);
     max_error_severity = "";

     Pthis_token = tokenFirstP;                             /* Begin analysis with first token found by lex_string_   */
     TRACING = (Adebug >= 1);				/* Trace matching reductions, only at extreme debug level.*/
     call SEMANTIC_ANALYSIS();                              /* Use reductions to parse the tokens.                    */

     if max_error_severity ^= "" then 
          Acode = mbuild_et_$bad_build_script;
     return;
%page;
/* -------------------- */
mbuild_script_parse_$check_parms:
     entry (Abuild_dataP) returns (bit(1) aligned);

  dcl  PROC char(20) int static options(constant) init("mbuild_script_parse_");

     return (check_parms (mbuild_data_version_3));
/* -------------------- */

%page;
          /*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
          /*                                                                                                */
          /* Syntax Routines                                                                                */
          /*                                                                                                */
          /*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

ename:
     proc() returns (bit(1) aligned);			/* Rules taken from entryname.gi.info for a	        */
						/*   "reasonably valid entryname".		        */

     if token.Lvalue > 0 then
      if token.Lvalue <= 32 then
       if verify(substr(token_value,1,1), "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_") = 0 then
        if verify (token_value, " !&+-./0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz^_~") = 0 then
         if index(token_value,"::") = 0 then
          if index(token_value,"..") = 0 then
	     return("1"b);
     return("0"b);

     end ename;


library:                                                    /* Validate given library name against multics_libraries_ */
     proc() returns (bit(1) aligned);

  dcl  mbuild_library_$library_directories entry (char(*) var, (*) char(200) var, fixed bin, fixed bin(35));

  dcl  dir_paths (20) char(200) var,
       dir_pathsN fixed bin;
  dcl  revised_library char(32) var;

     if index (token_value, "UNKNOWN") > 0 then do;
          if token_value = "UNKNOWN" then return (T);

          revised_library = before(token_value, ".UNKNOWN");
          if revised_library = "UNKNOWN" then return (T);

          revised_library = after(revised_library, "UNKNOWN.");
          if revised_library = "" then return (T);

          call mbuild_library_$library_directories (revised_library, dir_paths, dir_pathsN, code);
          if dir_pathsN = 0 then do;
               call Err_char(bad_library, (token_value));
               return (F);
               end;
          return (T);
          end;
     call mbuild_library_$library_directories ((token_value), dir_paths, dir_pathsN, code);
     if dir_pathsN = 0 then do;
          call Err_char(bad_library, (token_value));
          return (F);
          end;
     return (T);

     end library;


(subscriptrange):					/* Use subscriptrange checking to ensure program logic    */
operation:					/*  won't somehow access tier(4) or higher depthI.        */
     proc() returns (bit(1) aligned);
     
     if  (tier(depthI).type = "source_arch" | tier(depthI).type = "Bound_obj")  then
	return ( token_value = "ADD" | token_value = "UPDATE"  | token_value = "DELETE" );
     else return ( token_value = "ADD" | token_value = "REPLACE" | token_value = "DELETE" );
     end operation;


pathname:
     proc() returns (bit(1) aligned);

  dcl  absolute_pathname_ entry (char(*), char(*), fixed bin(35));
  dcl  hcs_$get_uid_file entry (char(*), char(*), bit(36) aligned, fixed bin(35));
  dcl  full_path char(168);
  dcl (uid_bld_directory, uid_pathname) bit(36) aligned;
     
     call absolute_pathname_ (token_value, full_path, code);
     if code ^= 0 then do;                                  /* Is pathname valid?                                     */
          call Err_char(bad_path, (token_value));
          return (F);
          end;

     if  substr(token_value,1,1) ^= ">"  then do;		/* Is pathname a relative path?  Absolute path required.  */
          call Err(relative_path);
          return (F);
          end;

     call hcs_$get_uid_file (bld.directory, "", uid_bld_directory, code);
     call hcs_$get_uid_file (full_path, "", uid_pathname, code);
     if uid_bld_directory ^= uid_pathname then do;          /* Is pathname equivalent to wdir, used as install dir    */
          call Err(wrong_install_dir);                      /*  by current invocation of mbuild?                      */
          return (F);
          end;
     return (T);

     end pathname;


type:                                                       /* Validate seg build-type name against mbuild_info_      */
     proc() returns (bit(1) aligned);
     
  dcl  mbuild_info_find_$build_type_is_valid entry (char(*)) returns(bit(1) aligned);

     return ( mbuild_info_find_$build_type_is_valid (token_value) );
     end type;

%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*                                                                                                */
	/* ERROR MESSAGE Action Routine and DECLARATIONS                                                  */
	/*                                                                                                */
	/* The Err action routine generates simplified error messages that are more easily declared and   */
	/* used in a reduction language program.                                                          */
	/*                                                                                                */
	/* Each error declaration gives:                                                                  */
	/*   dcl  ERROR_NAME_CONSTANT init(                                                               */
	/*        "<error-tags> : <error-message>")                                                       */
	/*         char(NN) var int static options(constant);                                             */
	/*                                                                                                */
	/* where <error_tags> is an alphanumeric string preceding first colon (:) in the string,          */
	/* consisting of the following components:                                                        */
	/*                                                                                                */
	/*  Error_Number                                                                                  */
	/*     1-3 digit numeric string uniquely identifying the message.                                 */
	/*                                                                                                */
	/*  Severity                                                                                      */
	/*     single character requesting a message prefix suggesting the importance of the message:     */
	/*       I   Information                                                                          */
	/*       W   Warning                                                                              */
	/*       E   ERROR            (default if Severity is omitted from <error-tags>)                  */
	/*       F   FATAL ERROR                                                                          */
	/*                                                                                                */
	/*  Format                                                                                        */
	/*     error message layout selector, chosen with zero or more of the following characters:       */
	/*       T   <error-message> includes ^a which is replaced with token_value for current token     */
	/*           in input line being processed by the reductions.                                     */
	/*           First line of error message ends with " on LINE <input-line-no>", indicating         */
	/*           input line containing that token.                                                    */
	/*       S   Adds SOURCE: <text-of-current-statement> as a final line of the message.             */
	/*           First line of error message ends with " on LINE <input-line-no>", indicating         */
	/*           input line on which that statement begins.                                           */
	/*       +   <error-message> includes ^/ which divides the message into two lines.                */
	/*           Err will split the <error-message> at the ^/ control, so it can append an            */
	/*           <input-line-no> to end of first line.  Text of 2nd line should not depend upon       */
	/*           wording at end of 1st line.                                                          */
	/*       >   call NEXT_MAJ_STMT() after error has been displayed.			        */
	/*                                                                                                */
	/*                                                                                                */
	/* EXAMPLES:                                                                                      */
	/*                                                                                                */
	/*   dcl unexpected_stmt init("01 F S +: Unexpected statement^/^-Expecting a Starter statement.") */
	/*       char(100) var int static options(constant);                                              */
	/*                                                                                                */
	/* when used while processing an input statement on line 12:                                      */
	/*   Table: terminal_XXX  option: 23;                                                             */
	/*                                                                                                */
	/* generates the message:                                                                         */
	/*                                                                                                */
	/*   FATAL ERROR 01:  Unexpected statement on LINE 12                                             */
	/*             Expecting a Starter statement.                                                     */
	/*   SOURCE:  Table: terminal_XXX  option: 23;                                                    */
	/*                                                                                                */
	/* ----------                                                                                     */
	/*                                                                                                */
	/*   dcl empty_script init("02 F    : Build script is empty.");                                   */
	/*                                                                                                */
	/* generates the message:                                                                         */
	/*                                                                                                */
	/*   FATAL ERROR 02:  Build script is empty.                                                      */
	/*                                                                                                */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

%page;
  dcl (
       unexpected_stmt init(
        "01 F S +: Unexpected statement^/^-Expecting optional Description; then Installation_directory statement."),
       empty_script init(
        "02 F    : Build script is empty."),
       unsupported_build_type init(
        "03 F S T: Unsupported build type: ^a "),
       invalid_stmt init(
        "04 E S  : Invalid build script statement"),
       bad_clause init(
        "05 E S T: Unknown clause begins: ^a  for statement"),
       incomplete_stmt init(
        "06 E S  : Build script ends with an incomplete statement"),
       bad_name init(
        "07 E S T: Incorrect segment name format: ^a  in statement"),
       bad_path init(				/*  Err_char()				        */
        "08 E S  : Invalid pathname: ^a  in statement"),
       relative_path init(
        "09 E S  : Absolute path required in statement"),
       wrong_install_dir init (
        "10 F S +: Pathname does not identify working directory^/^-mbuild only builds within the working directory."),
       bad_library init (				/*  Err_char()				        */
        "11 E S  : Invalid library name: ^a  in statement"),
       unsupported_entryname init (
        "12 F ST+: Undefined build type for segment: ^a^/^-Segment must be added to mbuild_info_.cds."),
       entryname_bad_suffix init (			/*  Err_char()				        */
        "13 F ST+: Entryname error: ^a^/^-Unsuitable name for a ^s^a statement."),   
       mbuild_support_bad_prefix init (			/*  Err_char()				        */
        "14 E S T: Entryname error: ^a^/^-It does not use install directory prefix: ^a"),
       duplicate_entryname init (			/*  Err_char()				        */
        "15 F S +: Entryname error: ^a^/^-Same entryname appears in an earlier statement."),
       unknown_entryname init (			/*  Err_char()				        */
        "16 F S +: Unknown entryname error: ^a^/^-Trying to DELETE entryname not found in library.")

         ) char(110) var int static options(constant);


Err: proc (Aerr);                                           /* Action Subroutine: emit a brief explanation of an      */
                                                            /*  error encountered while parsing the entry declaration.*/
  dcl  Aerr char(*) var;                                    /*  The message may (or may not) include the current      */
  dcl  extra_arg char(1) aligned;			/*  token value and statement.                            */

     extra_arg = "";				/* No 2nd arg to ErrXXX routines.		        */
     goto ERR_COMMON;
     
Err_fixed_bin:
     entry (Aerr, Afixed_bin);
     
  dcl  Afixed_bin fixed bin;

     extra_arg = "B";				/* Fixed bin 2nd arg to ErrXXX routines.	        */
     goto ERR_COMMON;
     
Err_char:
     entry (Aerr, Achar);
     
  dcl  Achar char(*) var;				/* Char 2nd arg to ErrXXX routines.		        */

     extra_arg = "C";
     goto ERR_COMMON;


  dcl  ioa_ entry() options(variable);

  dcl (after, before, char, ltrim) builtin;

  dcl  error_data char(16) var,
       error_no char(3) var,                                /* 1-3 digit error number.                                */
       error_severity char(11) var,                         /*  = Information, Warning, ERROR, or FATAL ERROR         */
       error_lineref char(20) var,
       error_2nd_line char(100) var,
       error_string char(200) var,
      (error_token, error_stmt, extra_line, next_stmt) bit(1) aligned;
     
ERR_COMMON:     
     error_data = before(Aerr, ":");
     error_string = after(Aerr, ":");
     error_2nd_line = "";

     error_no = before(error_data, " ");
     error_data = after(error_data, " ");
     
     error_severity = "ERROR";                              /* Set default data values.                               */     
     error_stmt, error_token, extra_line = F; next_stmt = F;

     if  index(error_data, "I") > 0  then error_severity = "Information";
     if  index(error_data, "W") > 0  then error_severity = "Warning";     
     if  index(error_data, "E") > 0  then error_severity = "ERROR";
     if  index(error_data, "F") > 0  then error_severity = "FATAL ERROR";
     
     if  severity_level (substr (error_severity,1,1)) > severity_level (max_error_severity) then
          max_error_severity = substr (error_severity,1,1);

     if  index(error_data, "S") > 0  then error_stmt  = T;
     if  index(error_data, "T") > 0  then error_token = T;     
     if  index(error_data, "+") > 0  then extra_line  = T;     
     if  index(error_data, ">") > 0  then next_stmt   = T;

     Ptoken = Pthis_token;				/* Be sure we access current token & its statement struct */
     Pstmt = token.Pstmt;

     if      error_token then  error_lineref = " on LINE " || ltrim(char(token.line_no + Alines_preceding_script));
     else if error_stmt  then  error_lineref = " on LINE " || ltrim(char(stmt.line_no  + Alines_preceding_script));
     else error_lineref = "";

     if  extra_line  then do;
          error_2nd_line = after(error_string, "^/");
          error_string = before(error_string, "^/");
          end;
     
     error_string = "^/^a ^a: " || error_string;
     if error_lineref ^= "" then 
          error_string = error_string || error_lineref;
     
     if error_token then do;
	if  extra_arg = ""  then
	     call ioa_ (error_string, error_severity, error_no, token_value);
	else if  extra_arg = "B"  then
	     call ioa_ (error_string, error_severity, error_no, token_value, Afixed_bin);
	else if  extra_arg = "C"  then
	     call ioa_ (error_string, error_severity, error_no, token_value, Achar);
	end;
     else do;
	if  extra_arg = ""  then
	     call ioa_ (error_string, error_severity, error_no);
	else if  extra_arg = "B"  then
	     call ioa_ (error_string, error_severity, error_no, Afixed_bin);
	else if  extra_arg = "C"  then
	     call ioa_ (error_string, error_severity, error_no, Achar);
	end;

     if error_2nd_line ^= "" then do;
	if  extra_arg = ""  then
	     call ioa_ (error_2nd_line, token_value);
          else if  extra_arg = "B"  then 
	     call ioa_ (error_2nd_line, token_value, Afixed_bin);
	else if  extra_arg = "C"  then
	     call ioa_ (error_2nd_line, token_value, Achar);
	end;

     if error_stmt then do;
          call ioa_ ("SOURCE:  ^a", stmt_value);
	end;

     if next_stmt then call NEXT_MAJ_STMT();
     return;
     
severity_level:
          proc (Alevel_char) returns (fixed bin);

  dcl  Alevel_char char(1);
  dcl  severity_rank char(5) aligned init(" IWEF");

          return ( index (severity_rank, Alevel_char) );
          end severity_level;

     end Err;
%page;
          /*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
          /*                                                                                                */
          /* Semantic Action Routines                                                                       */
          /*                                                                                                */
          /*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

  dcl  mbuild_data_$get_Seg entry (ptr, char(*) var, char(*) var, char(*) var, char(*) var, char(*) var) returns(ptr);
  dcl  mbuild_data_$get_UNBOUNDOBJ entry (ptr, char(*) var, char(*) var, char(*) var, ptr) returns(ptr);
  dcl  mbuild_data_$scan_Tb_insert entry (ptr, ptr, fixed bin(35));
  dcl  mbuild_info_find_$build_type_is_valid entry (char(*)) returns(bit(1) aligned);
  dcl  mbuild_info_find_$seg_type_for_seg_name entry (char(*) var) returns(ptr);
  dcl  mbuild_info_find_$suffix_for_build_type entry (char(*)) returns(char(12) var);
  dcl  mbuild_library_$replace_library_component entry (char(*) var, char(*) var, char(*) var) returns(char(32) var);

/* ----------------------------------------------------------------------
 *  Major Statement <seg-type>:
 *   - have a level.type beginning with capitalized letter 
 *      ex: Bound_obj, Unbound_obj, Include, Info, Seg(<seg-type>)
 *
 *  minor statement <seg-type>:
 *   - have a level.type beginning with lowercase letter
 *      ex: bindfile, source_arch, source
 *
 *  HIERARCHY USE CASES PERMITTED BY Build Script Language:
                                                                                                                     Tier Depth
    Bound_obj Bound_obj Bound_obj    Bound_obj    Bound_obj  Unbound_obj  Unbound_obj  Include  Info  Seg(<seg-type>)    1
               bindfile  source_arch  source_arch  source                  source                                        2
                                       source                                                                            3
 * ------------------------------------------------------------------------------------------------------------------------- */

  dcl 1 level aligned like Seg.info;			

  dcl 1 tier (3) aligned like Seg.info;                     /* Data holding area until Seg structure allocated.       */
						/*  - Each array element holds data for depth shown in    */
						/*    above diagram.			        */
  dcl  depthI fixed bin init(0);			/* Tier of levels currently empty; no Segs parsed.        */

  dcl (addP, delP) ptr;				/* Pointer to nm_data structure holding names to be       */
						/*  added/deleted from Seg().			        */
%page;

MAJ_stmt: 					/* Starting again with major statements - top level       */
     proc();

     if  ^mbuild_info_find_$build_type_is_valid (token_value)  then do;
	call Err (unsupported_build_type);		/* Unexpected error: mbuild_info_ changed without changes */
	return;					/*  to these reductions.			        */
	end;
     addP, delP, SegP, UNBOUNDOBJp = null();
     tier(*) = "";					/* Initialize entire tier tree.                           */
     depthI = 1;					/* Major statement always uses tier(1) - top of tree      */
     tier(1).type = token_value;
     end MAJ_stmt;
%page;
MAJ_set_name:
     proc ();
     
     tier(1).name = token_value;
     segtP = mbuild_info_find_$seg_type_for_seg_name (tier(1).name);
     if segtP = null() then do;			/* Get seg_type details, based upon given file name.      */
	call Err (unsupported_entryname);
	return;
	end;
     if  segt.mbuild_type ^= tier(1).type  then do;
	call Err_char (entryname_bad_suffix, tier(1).type);
						/*  - report error if file suffix does not match that     */
	return;					/*    expected for the major statement type.	        */
	end;

     if  segt.build_paradigm = PDM_mbuild_support  then do;
  dcl  suffix char(12) var;

	suffix = mbuild_info_find_$suffix_for_build_type ((tier(1).type));
	if  before(tier(1).name, suffix) ^= bld.build_script_prefix  then do;
	     call Err_char (mbuild_support_bad_prefix, bld.build_script_prefix);
					       	/*  - report error if file prefix doesn't match install   */
	     return;				/*    directory entryname.			        */
	     end;

	SegP = mbuild_data_$get_Seg (addr(bld), tier(depthI).type, (token_value), 
			         unknown_library, unknown_operation, no_containing_archive);
	call mbuild_data_$scan_Tb_insert (addr(bld), SegP, code);
	if  code ^= 0  then
	     call Err_char (duplicate_entryname, Seg.name);
	depthI = 0;				/*  - Do equivalent of end_seg, since mbuild_support file */
	end;					/*    has neither operation nor library values.	        */

     end MAJ_set_name;


NEXT_MAJ_STMT:
     proc ();

     call NEXT_STMT();				/* Every Major Statement has a first token identifying    */
     do while (Ptoken ^= null());			/*  its statement type.  Skip forward 1 statement at a    */
	if  is_first_token(token_value)  then return;	/*  time until such token is found.		        */
	call NEXT_STMT();
	end;
     return;					/* Oops, no more Major Statements found in input.	        */

is_first_token:					/* Compare current token with identifier tokens for       */
          proc(tkn) returns(bit(1) aligned);		/*  Major Statement types.			        */
  dcl  tkn char(*);
	
  dcl  MajStmt_token (8) char(16) var int static options(constant) init (
        "Build_script", "Build_exec_com", "Build_log", "Bound_obj", "Unbound_obj", "Include", "Info", "Seg" );
	
  dcl MStokenI fixed bin;
	
	do MStokenI = lbound(MajStmt_token,1) to hbound(MajStmt_token,1);
	     if  tkn = MajStmt_token(MStokenI)  then return (T);
	     end;
	return (F);
	end is_first_token;

     end NEXT_MAJ_STMT;


%page;
(subscriptrange):					/* Use subscriptrange checking to ensure program logic    */
end_seg:						/*  won't somehow access tier(4) or higher depthI.        */
     proc ();
     if  depthI = 0  then return;			/* Do nothing if not parsing a major/minor statement.     */

     if  depthI = 1  then do;				/* Do nothing for relationship structures that don't      */
	if  tier(1).type = "Bound_obj" |		/*  represent a real original-content segment.	        */
	    tier(1).type = "Unbound_obj"  then do;	/*  Their main value is holding top-level library and     */
	     if  tier(1).operation ^= "DELETE"  then	/*  operation values, for use in their child tiers.       */
		return;				/* DELETE of these types is the exception.  Return a      */
	     end;					/*  Seg() structure for any Bound_obj or Unbound_obj that */
	end;					/*  is being DELETEd.			        */

     if depthI > 1 then do;				/* Seg's usually representing original-content segments.  */
						/*  - Copy library from MAJ or source_arch statement      */
	if  tier(depthI).library = ""  then do;	
	     if tier(depthI).type = "bindfile" then
	          tier(depthI).library =
	               mbuild_library_$replace_library_component ("object", before(tier(depthI-1).library,"."), "");
	     else tier(depthI).library =
                         mbuild_library_$replace_library_component ("source", before(tier(depthI-1).library,"."), "");
	     end;
	else if  tier(depthI).library = "source" | tier(depthI).library = "object"  then do;
	     tier(depthI).library =
		mbuild_library_$replace_library_component (tier(depthI).library, before(tier(depthI-1).library,"."), "");
	     end;
	if  tier(depthI).operation = ""  then do;	/*  - Copy operation from MAJ or source_arch statement    */
						/*    if unset in this statement.		        */
	     if  tier(depthI-1).operation = "UPDATE"  then
	          tier(depthI).operation = "REPLACE";	/*  - Map source_arch.operation = "UPDATE" to "REPLACE"   */
	     else tier(depthI).operation = tier(depthI-1).operation;
	     end;
	end;

     if  depthI = 2  &  tier(2).type = "source_arch"  &  tier(2).operation = "UPDATE" then return;
                         /* depthI = 2: a source_arch statement with operation = ADD should cause a Seg structure     */
		     /* to be added to the list.  However, for an "UPDATE" operation, this source_arch statement  */
		     /* is just an informational divider specifying the containing archive for source statements  */
		     /* that follow.  This information will be regenerated by mbuild_library_$get_Seg.	        */

     if  depthI = 3  &  tier(2).operation = "ADD"  then return;
		     /* depthI = 3: only used for source statements below a source_arch statement.  Ignore such   */
		     /* source statements if the source_arch is being ADDed.  For source below a source_arch that */
		     /* is being REPLACEd, the correct archive will exist in Multics Libraries, and be supplied   */
		     /* by the mbuild_library_$get_Seg call.					        */
%page;
						/* For all depthI values that didn't return above:        */
  dcl (arch_name, library_name) char(32) var;
     arch_name = no_containing_archive;			/* By default, we're not dealing with an archive component*/
     library_name = tier(depthI).library;		/* Use library for current statement tier.	        */

     if  tier(depthI).type = "source"  &  tier(depthI).operation = "ADD"  then do;
						/*  - Adding Seg(source)?			        */
	if  depthI = 3  then do;			/*     - At depth 3, use Seg(source_arch) info at depth 2 */
	     library_name = tier(2).library;
	     arch_name = tier(2).name;
	     end;
	else if  depthI = 2  then do;			/*     - At depth 2, use Seg(data_arch) or BOUNDOBJ info  */
	     library_name = tier(1).library || ".source"; /*       available from depth 1.		        */
	     if tier(1).type = "Bound_obj" then
		arch_name = tier(1).name || ".s.archive";
	     end;
	end;

     SegP = mbuild_data_$get_Seg (addr(bld),		/*  - Create a Seg structure.			        */
	tier(depthI).type, tier(depthI).name, library_name, tier(depthI).operation, arch_name);
     if  depthI = 1  &  tier(depthI).operation = "DELETE"	/* If trying to DELETE a Seg not found in library...      */
      &   Seg.operation = "ADD"  then do;		
	call Err_char (unknown_entryname, Seg.name);
	end;

     call mbuild_data_$scan_Tb_insert (addr(bld), SegP, code);
     if  code ^= 0  then				/*  - Insert it into scan_Tb thread		        */
	call Err_char (duplicate_entryname, Seg.name);

     if  depthI = 1  &  (addP ^= null() | delP ^= null())  then do;
	Seg.name_addP = addP;			/*  - Attach add/del names to Seg created by MAJ_stmt.    */
	Seg.name_deleteP = delP;			
	addP, delP = null();
	end;

     if depthI = 2  &  tier(2).type = "source"  &  UNBOUNDOBJp ^= null()  then do;
	UNBOUNDOBJ.sourceP = SegP;			/* Connect Seg(source) with its UNBOUNDOBJ structure, if  */
	Seg.UNBOUNDOBJp = UNBOUNDOBJp;		/*  one was created above to hold add/del names.	        */
	end;
     end end_seg; 


bindf_st: proc ();					/* Process bindfile minor statement.		        */
     depthI = 2;    tier(2) = "";  tier(3) = "";
     tier(2).type = token_value;
     tier(2).library = mbuild_library_$replace_library_component (tier(1).library, "", "object");
     tier(2).operation = tier(1).operation;
     end bindf_st;

sArch_st: proc();					/* Process source_arch minor statement.		        */
     depthI = 2;    tier(2) = "";  tier(3) = "";
     tier(2).type = token_value;
     tier(2).library = mbuild_library_$replace_library_component (tier(1).library, "", "source");
     tier(2).operation = tier(1).operation;
     end sArch_st;

source_st: proc ();					/* Process source minor statement.		        */

     depthI = 2;					/* Many source are immediately after their Bound_obj or   */
						/*  Unbound_obj.				        */
     if  tier(2).type = "source_arch" then		/* However, some follow a source_arch statement before    */
	depthI = 3;				/*  its Bound_obj statement.			        */
     tier(depthI) = "";
     tier(depthI).type = token_value;

     tier(depthI).library = mbuild_library_$replace_library_component (tier(depthI-1).library, "", "UNKNOWN.source");
						/* Adjust library from earlier major or source_arch stmt. */
						/*  This can be overridden by IN clause given for this    */
						/*  source statement.			        */
     if  tier(depthI-1).operation = "UPDATE"  then
	tier(depthI).operation = "REPLACE";		/*  - Map source_arch.operation = "UPDATE" to "REPLACE"   */
     else tier(depthI).operation = tier(depthI-1).operation;/*    "ADD" or "DELETE" gets applied to this source stmt. */
     end source_st;
%page;

  dcl  nameI fixed bin;				/* Index of most recently-found name in current statement.*/

begin_add_name:
     proc ();

  dcl  update_add_ptr bit(1) aligned;
     update_add_ptr = T;
     goto NAME_COMMON;

begin_delete_name:
     entry ();
     
     update_add_ptr = F;
     
NAME_COMMON:
     nm_dataN = countTokens() - 1;			/* Count current and remaining names in current statement */
						/*  (excluding the final statement delimiter).	        */
     nm_dataP = translator_temp_$allocate (bld.areaP, size(nm_data));
     nm_data.N = nm_dataN;
     nameI = 1;
     nm_data.names(nameI) = token_value;

     if update_add_ptr then
	addP = nm_dataP;
     else delP = nm_dataP;
     return;

another_name:
     entry ();

     nameI = nameI + 1;
     nm_data.names(nameI) = token_value;
     return;

end_name:
     entry ();
     
     if  tier(1).type = "Unbound_obj" then do;		/* Normally, ignore Unbound_obj relationship structure.   */
	if  (addP ^= null()  |  delP ^= null() )  then do;
						/*  - But if add/del names on the unbound object, then    */
						/*    generate an UNBOUNDOBJ structure to hold this       */
						/*    name-change information.		        */
	     UNBOUNDOBJp = mbuild_data_$get_UNBOUNDOBJ (addr(bld),
		tier(1).name, tier(1).library, tier(1).operation, null() );
	     UNBOUNDOBJ.name_addP = addP;		/*  - Remember location of UNBOUNDOBJ structure so we     */
	     UNBOUNDOBJ.name_deleteP = delP;		/*    can attach Seg(source) to it; and it to Seg(source) */
	     addP, delP = null();			/*    in a call to end_seg at depthI = 2.	        */
	     end;		
	return;
	end;					/* Other depthI = 1 are real segments represented by      */
     

     end begin_add_name;


(subscriptrange):					/* Use subscriptrange checking to ensure program logic    */
setter:						/*  won't somehow access tier(4) or higher depthI.        */
     proc;

set_compiler: entry();				/* Save compiler name			        */
     tier(depthI).compiler = token_value;
     return;

set_compile_option: entry();				/* Append compile option.			        */
     if tier(depthI).compile_options = "" then
	tier(depthI).compile_options = token_value;
     else tier(depthI).compile_options = tier(depthI).compile_options || " " || token_value;
     return;

set_library: entry();				/* Save IN: <library> value.			        */
     tier(depthI).library = token_value;
     return;

set_operation: entry();				/* Save <operation> value.			        */
     tier(depthI).operation = token_value;
     return;

set_seg_name: entry();				/* Validate/save <ename> against statement type.	        */
     tier(depthI).name = token_value;

     segtP = mbuild_info_find_$seg_type_for_seg_name (tier(depthI).name);
     if segtP = null() then do;			/* Get seg_type details, based upon given file name.      */
	call Err (unsupported_entryname);
	return;
	end;
     if  segt.mbuild_type ^= tier(depthI).type  then do;
	call Err (entryname_bad_suffix);		/*  - report error if file suffix does not match that     */
	return;					/*    expected for the major/minor statement type.        */
	end;

     return;

set_inst_dir: entry();				/* Save install directory.  <pathname> validated it.      */
     bld.directory = token_value;
     return;

     end setter;
%page;
          /*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
          /*                                                                                                */
          /* Utility Subroutines                                                                            */
          /*                                                                                                */
          /*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

countTokens:					/* Count tokens remaining in the current statement.       */
     proc() returns (fixed bin);
     
  dcl  savedTokenP ptr init(Ptoken);			/* Remember current place in input token list.	        */
  dcl  count fixed bin init(1);			/* Use current token to start off our counter.	        */
     
     Ptoken = Pthis_token;
     Pstmt = token.Pstmt;
     do Ptoken = Ptoken  repeat token.Pnext  while (Ptoken ^= null);
	if  Ptoken = stmt.Plast_token  then do;
	     Ptoken = savedTokenP;
	     return (count);
	     end;
	count = count + 1;
	end;
     Ptoken = savedTokenP;
     return (count);

     end countTokens;


displayToken:                                               /* Display a token returned by lex_string_                */
     proc();

     call ioa_$nnl ("^[^a^/^;^a ^]", Ptoken = token.Pstmt->stmt.Plast_token, token_value);

     end displayToken;

%page;
%include ssu_subroutine_dcls_;
%page;
%include mbuild_data_;
%page;
%include mbuild_info_;
%page;
%include mbuild_Tlist_dcls_;
%page;
%include mbuild_request_parms_;
%page;
/* %include lex_descriptors_;  THIS IS INCLUDED BY reductions COMMAND */
%page;
