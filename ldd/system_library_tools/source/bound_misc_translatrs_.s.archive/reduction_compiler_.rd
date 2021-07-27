/* ***************************************************************
   *                                                             *
   * Copyright (c) 1986 by Massachusetts Institute of Technology *
   *                                                             *
   * Copyright (c) 1975 by Massachusetts Institute of Technology *
   *                                                             *
   *************************************************************** */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/* N__a_m_e:  reduction_compiler_							*/
	/*									*/
	/*      This procedure is the subroutine interface for the reduction_compiler.  It	*/
	/* accepts as input a set of reductions, a temporary segment for use in allocations	*/
	/* of a temporary nature, and a pointer to and maximum length of the object segment to	*/
	/* be generated.  It returns the actual length of the compiled object segment.		*/
	/*      The reductions to be compiled have been pre-processed by the lex_string_	*/
	/* subroutine, and are represented by a chain of input tokens.			*/
	/*      This subroutine is, itself, driven by a set of reductions which were compiled	*/
	/* by a bootstrapped version of the reduction_compiler.				*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */


/* HISTORY COMMENTS:
  1) change(74-04-05,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 1.0--
      Created the reduction_compiler (rdc) command.
  2) change(74-05-06,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 1.1--
      Fixed bugs in initial version.
  3) change(74-05-17,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 1.2--
      Changed the following rdc constructs:
       a) STACK-POP  ==>  STACK_POP
       b) (PL/I-stmt)  ==>  [PL/I-stmt]  for semantic statements.
  4) change(75-01-30,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 1.3--
      Make relative syntax functions quick PL/I blocks by converting array of
      entries into relative syntax functions into a label transfer vector into
      calls to the relative syntax functions.
  5) change(75-02-03,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 2.0--
      a) code generated for LEX converted to calls to a subroutine LEX(n).
      b) new DELETE and DELETE_STMT built-in action routines added.
      c) new INCLUDE attribute added to force inclusion of include segments.
      d) code for PUSH DOWN LANGUAGE added but not documented.
      e) allocate statements changed to calls to translator_temp_$allocate.
     
  6) change(75-04-28,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 2.1--
      a) put a space after all tokens in semantic statement brackets ([]),
         except <quoted-string> tokens, and the following paired token
         sequences:
                                   < =
                                   > =
                                   ^ =
                                   ^ >
                                   ^ <
                                   - >
      b) In order to implement this change, four new break characters were
         added: - ^ = ;
      c) Commenting delimiters were added:  \" begins a comment, which ends
         with a newline character.
  7) change(81-02-16,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 2.2--
      a) INCLUDE LEX stmt added, because of
      b) code added to detect only use of LEX (rather than LEX(N)).
         If only LEX used, then the LEX subroutine is NOT included in
         SEMANTIC_ANALYSIS by default.
      c) Many data structures declared options(constant)
      d) VT now acccpted in .rd segments as a whitespace character
      e) <no-token> in a PUSH DOWN LANGUAGE checks to see if token on top of
         push down stack is the final input token (ie, all tokens are on the
         stack or have been deleted.
      f) Add code to support rdc's -trace control argument.
  8) change(83-07-23,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 2.3--
      a) Place sequence numbers in unlabeled reductions appearing in the .pl1
         and .list segments.  The same numbers are placed in the reductions
         printed during tracing.
      b) Changed implementation of -trace to avoid temporary copying of
         reduction source.  Instead, reduction source is extracted from the
         stmt descriptors.
  9) change(84-09-08,GDixon), approve(), audit(),
     install(86-03-17,MR12.0-1032):
     Version 2.4--
      a) Allow ERROR (named_constant) in addition to ERROR (decimal_integer)
      b) Use perprocess date_time format for date put in header comment of
         translator.
 10) change(85-10-21,GDixon), approve(86-02-06,MCR7339),
     audit(86-02-19,Wallman), install(86-02-19,MR12.0-1022):
     Version 2.5--
      Upgrade the severity of several error messages to severity 3, because
      these messages describe conditions which are likely to make PL/I
      compilation fail.  Severity 3 errors prevent the PL/I compiler from
      being invoked. (phx19850)
                                                   END HISTORY COMMENTS */



/*++
MAX_DEPTH 20 \

BEGIN	/ <no-token>			/ 			ERROR(1)		/ stop	\
	/ <any-token>			/ reductions_init				/ attributes \

attributes
	\" 1) parse and process the reduction attributes.  If present, these must precede any
	\"    reduction statements.
	/ BEGIN 				/ 		[Psave = Pthis_token]	/ pass1	 \
	/ MAX_DEPTH <decimal-integer> "\"	/ LEX set_depth     LEX(2)			/ attributes \
	/ PUSH DOWN LANGUAGE "\"		/ LEX(4)            [S_PDL = "1"b]		/ attributes \
	/ INCLUDE DELETE "\"		/ LEX(3)            [Sinclude_DELETE = "1"b]	/ attributes \
	/ INCLUDE DELETE_STMT "\"		/ LEX(3)            [Sinclude_DELETE_STMT = "1"b]	/ attributes \
	/ INCLUDE ERROR "\"			/ LEX(3)            [Sinclude_ERROR  = "1"b]	/ attributes \
	/ INCLUDE NEXT_STMT "\"		/ LEX(3)            [Sinclude_NEXT_STMT = "1"b]	/ attributes \
	/ INCLUDE LEX "\"			/ LEX(3)		[Sinclude_LEX = "1"b]	/ attributes \
	/ INCLUDE				/ 			ERROR(19)	NEXT_STMT	/ attributes \
	/ <no-token>			/ 			ERROR(1)		/ stop	\
	/ <any-token>			/ 			ERROR(2)	NEXT_STMT	/ attributes \

pass1	\" 1) create a symbol table giving name and reduction number for all reduction labels.
	\" 2) count the tokens in the syntax specification field to get an estimate of the amount
	\"    of temporary storage rdc will need to hold the syntax specifications.
set_label	/ /_				/ count_reduction   LEX			/ count	\
	/ <name>				/ set_label         LEX			/ set_label \
	/ "\"				/ 			ERROR(22)	LEX	/ set_label \
	/ <no-token>			/ reductions_begin  [Pthis_token = Psave]	/ pass2	 \
	/ <any-token>			/ 			ERROR(3)	LEX	/ set_label \

count	/ <quoted-string>			/ count_token(1)    LEX(1)			/ count	\
	/ /_ <BS> _			/ count_token(1)    LEX(3)			/ count	\
	/ /_				/ 		NEXT_STMT			/ set_label \
	/ <any-token>			/ count_token(1)    LEX			/ count	\
	/ <no-token>			/ 			ERROR(5)		/ stop	\

pass2	\" Process the reduction statements, as follows:
	\" 1) skip over any labels on the reduction statement.
	\" 2) compile the syntax specifications by storing them in rdc's temporary syntax table.
	\" 3) compile the action specifications by outputting calls to built-in action routines and
	\"    semantic subroutines, and by outputting semantic statements.
	\" 4) compile the next reduction field by outputting code to transfer to the appropriate reduction.
label
skip_label
	/ /_				/ reduction_begin	LEX			/ first_token \
	/ <name>	 			/		LEX			/ skip_label \
	/ "\"				/		LEX			/ skip_label \
	/ <any-token>			/		LEX			/ skip_label \
	/ <no-token>			/ 					/ stop	\

first_token
	/ <PUSH_DOWN_LANGUAGE>		/					/ token1	\
	\" For a non-PUSH DOWN LANGUAGE, <no-token> followed by any syntax specification is in error
	\" because tokens are checked from left to right;  for a PUSH DOWN LANGUAGE, <no-token> has
	\" meaning as the first or last specification in a reduction.  As the first spec, it identifies
	\" the bottom of the push-down stack.  As the last spec, it identifies when the list of input tokens
	\" has run out.
	/				/					/ tokens	\

token1	/ <quoted-string>			/ 					/ tokens	\
	/ <_ no - token >_ <any-token>		/ compile_token(1)  LEX(5)			/ tokens	\

tokens	/ <quoted-string>			/ compile_token(0)	LEX			/ tokens	\
	/ /_ <BS> _			/ compile_token(0)	LEX(3)			/ tokens	\
	/ /_				/ 	    	LEX       action_begin	/ action	\
	/ < <BS> _			/ compile_token(0)	LEX(3)			/ tokens	\
	/ > <BS> _			/ compile_token(0)	LEX(3)			/ tokens	\
	/ [ <BS> _			/ compile_token(0)	LEX(3)			/ tokens	\
	/ ] <BS> _			/ compile_token(0)	LEX(3)			/ tokens	\
	/ ( <BS> _			/ compile_token(0)	LEX(3)			/ tokens	\
	/ ) <BS> _			/ compile_token(0)	LEX(3)			/ tokens	\
	/ <_ no - token >_ /_			/ compile_token(1) 	LEX(6)    action_begin	/ action	\
	/ <_ no - token >_ <any-token>		/		LEX(5)    ERROR(14)		/ error_in_red \
	/ <_ any - token >_			/ compile_token(2)	LEX(5)			/ tokens	\
	/ <_ name >_			/ compile_token(3)	LEX(3)			/ tokens	\
	/ <_ decimal - integer >_		/ compile_token(4)	LEX(5)			/ tokens	\
	/ <_ BS >_				/ compile_token(5)	LEX(3)			/ tokens	\
	/ <_ quoted - string >_		/ compile_token(6)	LEX(5)			/ tokens	\
	/ <_ <name> >_			/ LEX
					  compile_token(7) 	LEX(2)			/ tokens	\
	/ "\"				/ 	  	LEX       ERROR(22)		/ label	\
	/ <any-token>			/ compile_token(0)	LEX			/ tokens	\
	/ <no-token>			/ 		          ERROR(5)		/ stop	\

action	/ /_				/ 		   LEX			/ next_red \
	/ LEX (   <decimal-integer> )		/ set_action_with_args LEX(2) PUSH(last_paren)
					  [Sinclude_LEX = "1"b]			/ args	\
	/ LEX ( - <decimal-integer> )		/ set_action_with_args LEX(2) PUSH(last_paren)
					  [Sinclude_LEX = "1"b]			/ args	\
	/ LEX ( + <decimal-integer> )		/ set_action_with_args LEX(2) PUSH(last_paren)
					  [Sinclude_LEX = "1"b]			/ args	\
	/ LEX (				/ 			ERROR(19)		/ error_in_red \
	/ LEX				/ rtn(1)		   LEX			/ action	\
	/ NEXT_STMT (			/ 			ERROR(19)		/ error_in_red \
	/ NEXT_STMT			/ set_action	   LEX
					  [Sinclude_NEXT_STMT = "1"b]			/ action	\
	/ POP (				/ 			ERROR(19)		/ error_in_red \
	/ POP				/ rtn(2)		   LEX			/ action	\
	/ PUSH ( <name> ) 			/ LEX(2) rtn(3)	   LEX(2)			/ action	\
	/ PUSH				/ 			ERROR(19)		/ error_in_red \
	/ DELETE				/					/ DELETE	\
	\" Remove tests for all of the DELETE cases from main stream of reductions to a subroutine.
	/ DELETE_STMT (			/ 			ERROR(19)		/ error_in_red \
	/ DELETE_STMT			/ set_action 	   LEX
					  [Sinclude_DELETE_STMT = "1"b] 		/ action	\
	/ ERROR ( <decimal-integer> )		/ set_action_with_args LEX(2)
					  [Sinclude_ERROR = "1"b] 	PUSH(last_paren)	/ args	\
	/ ERROR (				/ set_action_with_args LEX(2)
					  [Sinclude_ERROR = "1"b] 	PUSH(last_paren)	/ args	\
	\" The preceding reduction allows the builtin ERROR routine to accept
	\" a named constant instead of a decimal integer.
	/ [				/ output((6)"	" || (4)" ")
					 		   LEX			/ stmt	\
	/ ]				/ 			ERROR(21)	LEX	/ action	\
	/ (				/ 			ERROR(21)	LEX	/ action	\
	/ )				/ 			ERROR(21)	LEX	/ action	\
	/ <quoted-string>			/ 			ERROR(23)		/ error_in_red \
	/ "\"				/ 			ERROR(22)		/ error_in_red \
	/ <any-token> (			/ set_action_with_args LEX(2) PUSH(last_paren)	/ args	\
	/ <any-token>			/ set_action 	   LEX			/ action	\
	/ <no-token>			/ 			ERROR(5)		/ stop	\

error_in_red
	/ 				/ [obj_red.Ilast(Nobj_red) = 0]
					  reduction_end 	   NEXT_STMT		/ label	\

DELETE	/ DELETE (   <decimal-integer> ,   <decimal-integer> )	/			/ DELETE_2 \
	/ DELETE (   <decimal-integer> , - <decimal-integer> )	/			/ DELETE_2 \
	/ DELETE (   <decimal-integer> , + <decimal-integer> )	/			/ DELETE_2 \
	/ DELETE ( - <decimal-integer> ,   <decimal-integer> )	/			/ DELETE_2 \
	/ DELETE ( - <decimal-integer> , - <decimal-integer> )	/			/ DELETE_2 \
	/ DELETE ( - <decimal-integer> , + <decimal-integer> ) 	/			/ DELETE_2 \
	/ DELETE ( + <decimal-integer> ,   <decimal-integer> )	/			/ DELETE_2 \
	/ DELETE ( + <decimal-integer> , - <decimal-integer> )	/			/ DELETE_2 \
	/ DELETE ( + <decimal-integer> , + <decimal-integer> )	/			/ DELETE_2 \
	/ DELETE (   <decimal-integer> )			/			/ DELETE_1 \
	/ DELETE ( - <decimal-integer> )			/			/ DELETE_1 \
	/ DELETE ( + <decimal-integer> )			/			/ DELETE_1 \

	/ DELETE (			/ 			ERROR(19)		/ error_in_red \
	/ DELETE				/ set_action_with_args LEX	output(" 0, 0 )")
					  [Sinclude_DELETE = "1"b]			/ last_paren\
	\" The only way to reach the next reduction is by branch.  All possible cases of DELETE
	\" have been handled above, including illegal ones.

DELETE_1	/				/ set_action_with_args LEX(2)
					  [Sinclude_DELETE = "1"b]			/	\
	/ <any-token> <decimal-integer>	/ output(" ") output(token_value) 	LEX
						    output(token_value) 	LEX(-1)
					  output(",")		PUSH(last_paren)	/ args	\
	/	    <decimal-integer>	/ output(" ") output(token_value)
					  output(",")		PUSH(last_paren)	/ args	\

DELETE_2	/				/ set_action_with_args LEX(2)	PUSH(last_paren)
					  [Sinclude_DELETE = "1"b]			/ args	\

stmt	\" Process the contents of semantic statements.  Special attention is given when generating
	\" PL/I code for the statements to the following cases:
	\" 1) No space is placed between the last token of a semantic statement and its
	\"    ending semi-colon statement delimiter.
	\" 2) No space is placed between an argument in a subprogram call and any comma delimiter
	\"    which may follow it.
	\" 3) No space is placed between a quoted string and any b, b1, b2, b3 or b4
	\"    token which follows it in order to handle bit string constants (eg "101"b)
	\" 4) No space is placed between any of the following pairs of characters which
	\"    have a special meaning in the PL/I language:  ->  >=  <=  ^=  ^>  ^<
	\" 5) No space is placed between any minus sign (-) and the token which follows, in
	\"    order to handle signed numeric constants.
	\" 6) Semantic statements appearing in the same pair of brackets in an action specification
	\"    are placed on different lines in the generated code (as if they had appeared in
	\"    separate brackets).
	/ <quoted-string> b			/					/ bit_constant  \
	/ <quoted-string> b1		/					/ bit_constant  \
	/ <quoted-string> b2		/					/ bit_constant  \
	/ <quoted-string> b3		/					/ bit_constant  \
	/ <quoted-string> b4		/					/ bit_constant  \
	/ <quoted-string>			/ output(" ") output_quote(token_value) LEX	/	\
	/ (				/ output(" ") output("(") PUSH(stmt)    LEX	/ args	\
	/ ]				/				LEX	/ last_paren \
	/ ;				/ output (";" || NL || (6)"	" || (4)" ")
					  				LEX	/ stmt	\
	/ "\"				/ 			ERROR(24)		/ error_in_red \
	/				/ PUSH(stmt) PUSH(stmt1)			/ special_chars \
	\" Always branch to special subroutine to check for paired character sequences.
	\" This subroutine returns to the 1st PUSHed label if a paired character sequence
	\" is found, and to the second PUSHed label if no paired sequence is found.
stmt1	/				/ 				POP	/	\
	/ <any-token>			/ output(" ") output(token_value) 	LEX	/ stmt	\
	/ <no-token>			/ 			ERROR(5)		/ stop	\

bit_constant
	/ 				/ output(" ") output_quote(token_value) LEX
					  	    output(token_value)       LEX	/ stmt	\


args	\" This reduction subroutine processed the arguments in calls to semantic subroutines, and
	\" the parenthesized expression or sub-program arguments in semantic statements.  It handles
	\" the special cases described above under "stmt".  Nested parentheses are handled to a
	\" depth of about 17.  It returns to the last PUSHed reduction label.
	/ <quoted-string> (			/ 					/ quoted_arg \
	/ <quoted-string> "\"		/ 					/ quoted_arg \
	/ <quoted-string> )			/ 					/ quoted_arg \
	/ <quoted-string> <any-token>		/ output(" ") output_quote(token_value) LEX
					  	    output(token_value) 	LEX	/ args	\
quoted_arg
	/ <quoted-string>			/ output(" ") output_quote(token_value) LEX	/	\
	/ (				/ output(" ") output("(") PUSH(args)	LEX	/ args	\
	/ )				/ output(" ") output(")") 		LEX	/ STACK_POP \
	/ ;				/ 			ERROR(24)		/ error_in_red \
	/ "\"				/ 			ERROR(24)		/ error_in_red \
	/				/ PUSH(args) PUSH(args1)			/ special_chars \
	\" Always branch to special subroutine to check for paired character sequences.
	\" This subroutine returns to the 1st PUSHed label if a paired character sequence
	\" is found, and to the second PUSHed label if no paired sequence is found.
args1	/				/ 				POP	/	\
	/ ,				/ 	    output(token_value) 	LEX	/ args	\
	/ <any-token>			/ output(" ") output(token_value) 	LEX	/ args	\
	/ <no-token>			/ 			ERROR(5)		/ stop	\

last_paren/				/ output(";") output(NL)			/ action	\

special_chars
	\" Special reduction subroutine to check for paired character sequences in action specifications.
	\" Calling sequence is:    /	/ PUSH(label1) PUSH(label2)	/ special_chars \
	\"		 label2/  / POP			/	\
	\" This subroutine returns through the first PUSHed reduction label if a paired sequence
	\" is found, and through the second PUSHed label if none if found.
	/ < =				/					/ spec_found \
	/ > =				/					/ spec_found \
	/ ^ =				/					/ spec_found \
	/ ^ >				/					/ spec_found \
	/ ^ <				/					/ spec_found \
	/ - >				/					/ spec_found \
	/ - <any-token>			/					/ spec_found \
	/ + <any-token>			/					/ spec_found\
	/				/					/ STACK_POP \

spec_found
	/				/ output(" ") output(token_value) 	LEX
						    output(token_value)	LEX POP	/ STACK_POP \

next_red	\" The final group of reductions identifies and compiles code for the various next
	\" reduction fields of a reduction statement.
	/ "\"				/ next_reduction	    reduction_end 	LEX	/ label	\
	/ RETURN "\"			/ terminal_reduction    reduction_end 	LEX(2)	/ label	\
	/ STACK "\"			/ stacked_reduction     reduction_end 	LEX(2)	/ label	\
	/ STACK_POP "\"			/ stacked_reduction_pop reduction_end 	LEX(2)	/ label	\
	/ <name> "\"			/ specified_label       reduction_end 	LEX(2)	/ label	\
	/ <name>				/ specified_label       reduction_end 
					  			ERROR(16)	NEXT_STMT	/ label	\
	/ <any-token> "\"			/ next_reduction        reduction_end
					  			ERROR(4)	NEXT_STMT	/ label	\
	/ <any-token>			/ next_reduction        reduction_end 
					  			ERROR(15)	NEXT_STMT	/ label	\
	/ <no-token>			/ 			ERROR(5)		/ stop	\

stop	/ <no-token>			/ reductions_end				/ RETURN	\
	/ <any-token>			/ reductions_end		ERROR(6)		/ RETURN	\
											++*/

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


reduction_compiler_:
	proc    (Psource, Lsource, Psegment, APobj, ALobj, Aname_source, Scontrol, Mseverity, Acode);

     dcl	Psource			ptr,		/* ptr to reduction source segment. (In)	*/
	Lsource			fixed bin(21),	/* length of reduction source segment (in chars).	*/
						/* (Input)				*/
	Psegment			ptr,		/* ptr to a segment in which allocations	*/
						/* may be performed.  The segment must be a temp	*/
						/* segment provided by translator_temp_. (In)	*/
	APobj			ptr,		/* ptr to words of the object segment. (In)	*/
	ALobj			fixed bin(21),	/* maximum number of characters allowed in object	*/
						/* segment. (In)				*/
						/* number of words in constructed object segment.	*/
						/* (Out)					*/
	Aname_source		char(32),		/* entry name of input source segment. (In)	*/
	Scontrol			bit(*),		/* error format control bits. (In)		*/
	Mseverity			fixed bin(35),	/* severity of highest-severity error encountered	*/
						/* during the compilation. (Out)		*/
	Acode			fixed bin(35);	/* error code. (Out)			*/

     dcl						/*	automatic variables			*/
         (Ired_start, Ired_end)	fixed bin(21),	/* index into source of start/end of reductions.	*/
	Llongest_red		fixed bin,	/* length (in chars) of longest reduction.	*/
	Lobj			fixed bin(21),	/* length (in chars) of unused part of object	*/
						/* segment being created.			*/
	Lobj_part			fixed bin(21) init (0),
						/* length of a subset of the object segment.	*/
	Lobj_spaces		fixed bin,	/* number of spaces to be output into object seg.	*/
	Lobj_string		fixed bin,	/* maximum length of the string containing the	*/
						/* stored object token values.		*/
	Lobj_string_part		fixed bin(21),	/* length of a particular token value within the	*/
						/* string of all object token values.		*/
	Ltemp			fixed bin(21),	/* length of temporary character string.	*/
	Ltemp_obj			fixed bin(21),	/* length of temp copy of object segment contents.*/
	Mstack_depth		fixed bin,	/* user-specified maximum depth of the 		*/
						/* next-reduction-label stack.		*/
	Nchar			pic "----9" aligned,/* convert fixed bin integers to 4-char numbers.	*/
	Nobj_red			fixed bin,	/* index of the object reduction being compiled.	*/
	Nobj_token		fixed bin,	/* index of the object token being compiled.	*/
	Nobj_token_fcn		fixed bin,	/* index of the object token function being	*/
						/* compiled.				*/
	Nreductions		fixed bin,	/* number of reductions which can be stored in	*/
						/* object reduction storage structure.		*/
	Ntokens			fixed bin,	/* number of token requirements which can be 	*/
						/* stored in object token storage structure.	*/
	Osc_start			fixed bin(21),	/* char offset of start of source to be output.	*/
	Pobj			ptr,		/* ptr to unused part of object segment.	*/
	Pobj_red			ptr,		/* ptr to temp. storage structure for object	*/
						/* reductions.				*/
	Pobj_spaces		ptr,		/* ptr to adjustable-length string  of spaces.	*/
	Pobj_string		ptr,		/* ptr to temp. storage string for object token	*/
						/* values.				*/
	Pobj_string_part		ptr,		/* ptr to a particular token value within the	*/
						/* string of all object token values.		*/
	Pobj_token		ptr,		/* ptr to temp. storage structure for object	*/
						/* token requirements.			*/
	Pobj_token_quoted		ptr,		/* ptr to temp. storage for bits which are on if	*/
						/* object token was in quotes when input.	*/
	Psave			ptr,		/* ptr used in saving/restoring value of	*/
						/* Pthis_token between pass1 and pass2.		*/
	Ptemp			ptr,		/* ptr to temporary character string.		*/
	Ptemp_obj			ptr,		/* ptr to temp copy of object segment contents.	*/
	S_PDL			bit(1) aligned,	/* on if to be in 'PUSH DOWN LANGUAGE' mode.	*/
	S_TRACE			bit(1) aligned,	/* on if tracing code is to be generated.	*/
	S_TRACE_ON		bit(1) aligned,	/* on if tracing to be on initially.		*/
	Sinclude_DELETE		bit(1) aligned,	/* on if DELETE proc to be included in object seg.*/
	Sinclude_DELETE_STMT	bit(1) aligned,	/* on if DELETE_STMT proc to be included in obj.	*/
	Sinclude_ERROR		bit(1) aligned,	/* on if ERROR proc to be include in object seg.	*/
	Sinclude_NEXT_STMT		bit(1) aligned,	/* on if NEXT_STMT proc to be included.		*/
	Sinclude_LEX		bit(1) aligned,	/* on if LEX proc to be included.		*/
	Sinclude_STACK		bit(1) aligned,	/* on if STACK procs to be included in obj seg.	*/
	Soptimize_possible		bit(1) aligned,	/* on if optimization of object token storage	*/
						/* allocation is possible for the tokens assoc.	*/
						/* with the reduction being compiled.		*/
	code			fixed bin(35),	/* a status code.				*/
	date			char(53),		/* a date/time string.			*/
	form			fixed bin,	/* form of an object token.			*/
	i			fixed bin,	/* an integer temporary.			*/
	j			fixed bin,	/* an integer temporary.			*/
	name_source		char(32),		/* name of source segment, without its suffix.	*/
	1 obj_label		aligned,		/* temp storage for labels on object reductions.	*/
	  2 N			fixed bin,	/* number of labels currently defined.		*/
	  2 set (1000),				/* space for up to 1000 labels.		*/
	    3 name		char(32) aligned,	/* name of label.				*/
	    3 reduction_no		fixed bin,	/* number of reduction labelled by this label.	*/
	1 obj_token_fcn		aligned,		/* temp storage for relative token requirement	*/
						/* functions.				*/
	  2 N			fixed bin,	/* number of object token requirements defined.	*/
	  2 name (100)		char(32) varying,	/* name of token requirement.			*/
	type			fixed bin;	/* type of an object token.			*/


     dcl						/*	builtin functions			*/
         (addcharno, addr, addrel, bit, char, charno, dimension, divide,
	fixed, index, length, log, ltrim, max, min, null, rtrim,
	size, string, substr, verify)
				builtin;

     dcl						/*	entries				*/
	clock_			entry returns (fixed bin(71)),
	date_time_$format		entry (char(*), fixed bin(71), char(*), char(*)) returns(char(250) var),
	lex_error_		entry options (variable),
	lex_string_$lex		entry (ptr, fixed bin(21), fixed bin(21), ptr, bit(*) aligned,
				       char(*) aligned, char(*) aligned, char(*) aligned, char(*) aligned,
				       char(*) aligned, char(*) varying aligned, char(*) varying aligned,
				       char(*) varying aligned, char(*) varying aligned,
				       ptr, ptr, fixed bin(35)),
	lex_string_$init_lex_delims	entry (char(*) aligned, char(*) aligned, char(*) aligned, char(*) aligned,
				       char(*) aligned, bit(*) aligned, char(*) varying aligned, 
				       char(*) varying aligned, char(*) varying aligned,
				       char(*) varying aligned),
	suffixed_name_$new_suffix	entry (char(*), char(*), char(*), char(32), fixed bin(35)),
	translator_temp_$allocate	entry (ptr, fixed bin) returns (ptr);

     dcl	NL			char(1) defined (NP) position (2);

     dcl						/*	based variables			*/
	obj			char(Lobj) aligned based (Pobj),
						/* object segment being created.		*/
	1 obj_red			aligned based (Pobj_red),
						/* temp storage for object reductions, prior	*/
						/* to outputting them into the object segment.	*/
	  2 N			fixed bin,	/* number of reductions currently defined.	*/
	  2 M			fixed bin,	/* maximum number which may be defined.		*/
	  2 token_reqd (Nreductions refer (obj_red.M)),
	    3 Ifirst		fixed bin(17) unal,	/* index of 1st and last token requirements	*/
	    3 Ilast		fixed bin(17) unal,	/* associated with this reduction.		*/

	obj_spaces		char(Lobj_spaces) based (Pobj_spaces),
						/* overlay for a number of spaces used to 	*/
						/* right-adjust an output line.		*/
	obj_string		char(Lobj_string) varying aligned based (Pobj_string),
						/* temp storage for object token values.	*/
	obj_string_part		char(Lobj_string_part) based (Pobj_string_part),
						/* overlay for a particular token value within	*/
						/* the string of all object token values.	*/
	obj_token_quoted (Ntokens)	bit(1) unaligned based (Pobj_token_quoted),
						/* on if object token was enclosed in quotes.	*/
	1 obj_token		aligned based (Pobj_token),
						/* temp storage for object token requirements,	*/
						/* prior to outputting them into the object seg.	*/
	  2 N			fixed bin,	/* number of tokens currently defined.		*/
	  2 M			fixed bin,	/* maximum number which may be defined.		*/
	  2 token (Ntokens refer (obj_token.M)),
	    3 form		fixed bin(17) unal,	/* form of the object token:			*/
						/*  -1 = relative token requirement function;	*/
						/*       type = index of the particular token	*/
						/*	      function in the token_fcn array.	*/
						/*   0 = built-in token requirement function;	*/
						/*       type = as defined below.		*/
						/*  >0 = absolute token requirement:		*/
						/*       form = index(token_strings,token_req);	*/
						/*       type = length(token_req);		*/
	    3 type		fixed bin(17) unal,	/* type of the built-in token requirement	*/
						/* function:				*/
						/*   1 = compile test to see if input token 	*/
						/*       chain is exhausted (<no-token>).	*/
						/*   2 = compile test for any token value	*/
						/*       (<any-token>).			*/
						/*   3 = compile test for a PL/I identifier	*/
						/*       (<name>) of 32 or fewer characters.	*/
						/*   4 = compile test for token which is a	*/
						/*       <decimal-integer>.			*/
						/*   5 = compile test for token which is a single	*/
						/*       backspace character (<BS>).		*/
						/*   6 = compile test for a token which is a	*/
						/*       <quoted-string>.			*/
	source			char(Lsource) based(Psource),
						/* overlay for reduction source segment.	*/
	temp			char(Ltemp) based (Ptemp),
						/* overlay for part of object segment contents	*/
						/* just generated.				*/
	temp_obj			char(Ltemp_obj) based (Ptemp_obj);
						/* temporary copy of object segment contents.	*/

     dcl						/*	static variables			*/
	HT			char(1) int static options(constant) init("	"),
	HT_SP			char(2) int static options(constant) init("	 "),
	Mreductions		fixed bin int static options(constant) init (9999),
	MMstack_depth		fixed bin int static options(constant) init (9999),
	Mtokens			fixed bin int static options(constant) init (9999),
	NP			char(2) int static options(constant) init ("
"),						/* <NP><NL>				*/
	Sinitialization_reqd	bit(1) aligned int static init ("1"b),
	breaks			char(19) varying aligned int static options(constant) init (" 	
/\<>[]()-^=;,"),					/* BS SP HT NL / \ < > [ ] ( ) - ^ = ; , VT NP	*/
	1 error_control_table (26)	aligned internal static options(constant),
						/* reduction compiler error message text and 	*/
						/* action specifications.			*/
	  /* 1     2     3     4     5     6     7     8     9    10    11    12    13    14    15	*/
	  /*16    17    18    19    20    21    22    23    24    25    26    27    28    29    30	*/
	  2 severity		fixed bin(17) unaligned init (
	     3,    2,    2,    3,    3,    2,    4,    4,    4,    4,    3,    4,    3,    2,    3,
	     3,    4,    2,    3,    4,    3,    3,    3,    3,    3,    3),
						/* severity of each error.			*/
	  2 Soutput_stmt		bit(1) unaligned init (
	    "0"b, "1"b, "1"b, "1"b, "0"b, "0"b, "1"b, "1"b, "1"b, "1"b, "1"b, "0"b, "1"b, "1"b, "1"b,
	    "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "1"b, "0"b, "0"b),
						/* on if "current" statement should be output	*/
						/* with the error message.			*/
	  2 message		char(252) varying init (
	  /*  1 */
"The reduction source segment does not contain any valid reductions.",
	  /*  2 */
"The statement is not a valid attribute declaration or
reduction.  (Remember, the label of the first reduction must
be 'BEGIN'.)",
	  /*  3 */
"Label '^a' is invalid.  The label has been ignored.",
	  /*  4 */
"Label '^a' in the next-reduction field of the
reduction statement is invalid.  The label has been ignored.",
	  /*  5 */
"Unexpected end encountered.  The reduction source segment ends
with an incomplete reduction.",
	  /*  6 */
"Unexpected statement encountered when the end of the reduction
source segment was expected.",
	  /*  7 */
"Compiler restriction:  the reduction source segment contains
more than ^d labels.  Label '^a'
and all labels which follow it have been ignored.",
	  /*  8 */
"Compiler restriction:  the reduction source segment contains
more than ^d reductions.  The reduction on line ^d,
and those which follow it, could not be compiled.",
	  /*  9 */
"Compiler restriction:  the reduction source segment contains
more than ^d tokens.  Token '^a'
could not be compiled.",
	  /* 10 */
"Compiler restriction:  the reduction source segment contains
too many different tokens.  Because more than ^d token value
characters have been defined, token '^a'
could not be compiled.",
	  /* 11 */
"Label '^a' is undefined.  The reference to this label
could not be resolved.",
	  /* 12 */
"The reduction source segment is too large to compile, causing
the object segment to overflow.",
	  /* 13 */
"Label '^a' has been multiply-defined.",
	  /* 14 */
"Token requirement '^a' appears in a reduction
after a <no-token> token requirement.  This combination of
requirements could never be satisfied.  Therefore, the reduction
will be ignored.",
	  /* 15 */
"Label '^a' in the next-reduction field
of a reduction is invalid.  In addition, the next-reduction field
contains more than one label.  This is not allowed.",
	  /* 16 */
"The next-reduction field of a reduction contains more than one label.
This is not allowed.",
	  /* 17 */
"Compiler restriction:  the reduction source segment contains
more than ^d token requirement functions.
Function '<^a>' could not be compiled.",
	  /* 18 */
"Compiler restriction:  the number specified in a 'MAX_DEPTH'
attribute declaration is out of bounds.  The allowable range is:
^2-0 < MAX_DEPTH < ^d
A maximum depth of ^d will be assumed.",
	  /* 19 */
"The '^a' built-in action routine has been used improperly
in a reduction.",
	  /* 20 */
"In attempting to compile the reduction on line ^d,
the estimated number of reductions (^d) was exceeded.
The reduction on line ^d, and those which follow it,
could not be compiled.",
	  /* 21 */
"Unexpected '^a' in the action field of the reduction statement.",
	  /* 22 */
"One or more fields are missing from a reduction.  All of the
reduction fields (label, syntax, action, & next-label field)
must be supplied.",
	  /* 23 */
"A quoted string appears as the name of a semantic subroutine
in the action field.  This is not permitted.  The reduction
has been ignored.",
	  /* 24 */
"A right parenthesis ()) is missing from the action field of
a reduction.",
	  /* 25 */
"The reduction segment ends with an incomplete reduction.",
            /* 26 */
"The reduction delimiters in the reduction segment were not
found or were positioned improperly."),
						/* text of the error message.			*/
	  2 brief_message		char(64) varying init (
	  /*  1 */
"No reductions.",
	  /*  2 */
"Invalid statement.",
	  /*  3 */
"Invalid label '^a' ignored.",
	  /*  4 */
"Invalid label '^a' ignored.",
	  /*  5 */
"Reductions are incomplete.",
	  /*  6 */
"Unexpected statement after end of reductions.",
	  /*  7 */
"Restriction: >^d labels.  '^a' ignored.",
	  /*  8 */
"Restriction: >^d reductions.  Line ^d ignored.",
	  /*  9 */
"Restriction: >^d tokens.  '^a' ignored.",
	  /* 10 */
"Restriction: >^d token characters.  '^a' ignored.",
	  /* 11 */
"Label '^a' undefined.",
	  /* 12 */
"Object segment overflow.",
	  /* 13 */
"Label '^a' multiply-defined.",
	  /* 14 */
"'^a' appears after <no-token>.",
	  /* 15 */
"Label '^a' invalid & >1 next-reduction labels.",
	  /* 16 */
">1 label in next-reduction field.",
	  /* 17 */
"Restriction: >^d token requirement functions.  '<^a>' ignored.",
	  /* 18 */
"Restriction:  0 < MAX_DEPTH < ^d.  ^d assumed.",
	  /* 19 */
"'^a' built-in used improperly.",
	  /* 20 */
"#_reductions > ^s^d estimate.",
	  /* 21 */
"Unexpected '^a' ignored.",
	  /* 22 */
"Incomplete reduction.",
	  /* 23 */
"Quoted subroutine name.",
	  /* 24 */
"')' missing from action field.",
	  /* 25 */
"Reductions incomplete.",
	  /* 26 */
"Bad reduction delimiters."),
	ignored_breaks		char(5) varying aligned int static options(constant) init (" 	
"),					/* SP HT NL VT NP					*/
         (error_table_$fatal_error,
	error_table_$improper_data_format)
				fixed bin(35) ext static,
	lex_control_chars		char(128) varying aligned int static,
	lex_delims		char(128) varying aligned int static,
	nl			char(1) aligned int static options(constant) init ("
"),
	spaces			char(120) aligned int static options(constant) init ((120)" ");

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


	Acode = 0;				/* initialize error code.			*/
	SERROR_CONTROL = Scontrol;
	S_TRACE = substr(bit(Scontrol,36),3,1);
	if S_TRACE then S_TRACE_ON = substr(bit(Scontrol,36),4,1);
	else S_TRACE_ON = "0"b;
	TRACING = S_TRACE;
	Pobj_spaces = addr(spaces);
	Pstmt, Pthis_token = null;			/* start out with no input tokens.		*/
	Ired_start = index(source,"/*++");		/* find reductions in reduction source segment.	*/
	Ired_end = index(source,"++*/");
	if (Ired_start = 0) | (Ired_end = 0) | (Ired_start+4 >= Ired_end-1) then do;
	     call ERROR(26);
	     Acode = error_table_$improper_data_format;
	     go to RETURN;
	     end;
	Ired_start = Ired_start + 4;			/* skip over delimiters.			*/
	Ired_end = Ired_end - 1;
	if Sinitialization_reqd then do;		/* initialize static variables.		*/
	   call lex_string_$init_lex_delims ("""", """", "\""", nl, "\", "10"b,
		breaks, ignored_breaks, lex_delims, lex_control_chars);
	     Sinitialization_reqd = "0"b;
	     end;
	call lex_string_$lex (Psource, Ired_end-Ired_start+1, Ired_start-1, Psegment, "1"b,
	   """", """", "\""", nl, "\", breaks, ignored_breaks,
	     lex_delims, lex_control_chars, null, Ptoken, code);
	if code ^= 0 then				/* lex source segment into tokens.		*/
	     call ERROR(25);
	if Ptoken = null then do;
	     Acode = code;
	     go to RETURN;
	     end;
	Pthis_token = Ptoken;
	call SEMANTIC_ANALYSIS;			/* perform semantic analysis of tokens.		*/
RETURN:	Mseverity = MERROR_SEVERITY;
	if Mseverity > 2 then do;			/* Fatal error?  Return nothing.		*/
	     ALobj = 0;
	     if Acode = 0 then
		Acode = error_table_$fatal_error;
	     end;
	return;					/* All done!				*/

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/* RELATIVE SYNTAX FUNCTIONS							*/
	/*									*/
	/*      The relative syntax functions below are invoked to compare the input tokens	*/
	/* with specifications built into the function.					*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */


PUSH_DOWN_LANGUAGE:	procedure returns (bit(1) aligned);	/* returns "1"b if a 'PUSH DOWN LANGUAGE' is being*/
						/* compiled.				*/

	return (S_PDL);

	end PUSH_DOWN_LANGUAGE;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/* ACTION ROUTINES:								*/
	/*									*/
	/*      The action routines below are invoked at various stages of the compilation	*/
	/* process to impart semantic meaning to the series of tokens which have passed the	*/
	/* syntactic analysis tests of the input reductions.				*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */


action_begin:	procedure;			/* invoked when beginning to compile the actions	*/
						/* associated with a particular reduction.	*/

	call output ("
RD_ACTION(");					/* output label array constant identifying rtn.	*/
	call output_number (Nobj_red);
	call output ("):					/* /					*/
");
	end action_begin;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


compile_token:	procedure (type);			/* invoked to compile a syntax specification for	*/
						/* the reduction being parsed.		*/

     dcl	type			fixed bin;	/* type of specification to be compiled. (In)	*/
						/*   0 = compile absolute token requirement whose	*/
						/*       value is the character string value of	*/
						/*       the "current" token.			*/
						/*   1 = compile test to see if input token 	*/
						/*       chain is exhausted (<no-token>).	*/
						/*   2 = compile test for any token value	*/
						/*       (<any-token>).			*/
						/*   3 = compile test for a PL/I identifier	*/
						/*       (<name>) of 32 or fewer characters.	*/
						/*   4 = compile test for token which is a	*/
						/*       <decimal-integer>.			*/
						/*   5 = compile test for token which is a single	*/
						/*       backspace character (<BS>).		*/
						/*   6 = compile test for token which is a	*/
						/*       <quoted-string>.			*/
						/*   7 = compile relative token requirement fcn.	*/

	Nobj_token = obj_token.N + 1;			/* increment count of object tokens.		*/
	if Nobj_token > obj_token.M then do;		/* make sure we don't overflow obj token table.	*/
	     call lex_error_ (9, SERROR_PRINTED(9), (error_control_table(9).severity), MERROR_SEVERITY,
			  addrel(token.Pstmt,0), Ptoken, SERROR_CONTROL, (error_control_table(9).message),
			  (error_control_table(9).brief_message), obj_token.M, token_value);
	     go to RETURN;
	     end;
	obj_token.N = Nobj_token;			/* append token to obj token array, and to list 	*/
	obj_red.Ilast (Nobj_red) 			/* of object tokens related to reduction being	*/
	     = obj_red.Ilast (Nobj_red) + 1;		/* parsed.				*/
	go to comp (type);				/* compile the appropriate type of token.	*/

comp(0):	i = index (obj_string, token_value);		/* see if current token exists in string	*/
						/* of previously-defined token values.		*/
	if i > 0 then do;				/* if so, use previously-defined string.	*/
	     obj_token.form (Nobj_token) = i;
	     obj_token.type (Nobj_token) = token.Lvalue;
	     obj_token_quoted (Nobj_token) = token.S.quoted_string;
	     end;
	else do;					/* if not found, add it to obj token string	*/
						/* values.				*/
	     Soptimize_possible = "0"b;		/* optimization of obj token storage requirements	*/
						/* no longer possible for this reduction.	*/
	     if token.Lvalue + length (obj_string) > Lobj_string then do;
		call lex_error_ (10, SERROR_PRINTED(10), (error_control_table(10).severity), MERROR_SEVERITY,
			       addrel(token.Pstmt,0), Ptoken, SERROR_CONTROL, (error_control_table(10).message),
			       (error_control_table(10).brief_message), Lobj_string, token_value);
		go to RETURN;			/* complain if token too big for object string.	*/
		end;
	     else do;
		obj_token.form (Nobj_token) = length (obj_string) + 1;
		obj_token.type (Nobj_token) = token.Lvalue;
		obj_token_quoted (Nobj_token) = token.S.quoted_string;
		obj_string = obj_string || token_value;
		end;
	     end;
	return;
comp(1):
comp(2):
comp(3):
comp(4):
comp(5):
comp(6):	obj_token.form (Nobj_token) = 0;		/* indicate built-in nature of object token.	*/
	obj_token.type (Nobj_token) = type;		/* set appropriate object token type.		*/
	return;

comp(7):	obj_token.form (Nobj_token) = -1;
	do Nobj_token_fcn = 1 to obj_token_fcn.N while (obj_token_fcn.name(Nobj_token_fcn) ^= token_value);
	     end;					/* see if it was previously defined.		*/
	if Nobj_token_fcn <= obj_token_fcn.N then do;	/* yes, it was.				*/
	     obj_token.type (Nobj_token) = Nobj_token_fcn;
	     return;
	     end;

	if Nobj_token_fcn > dimension (obj_token_fcn.name, 1) then do;
	     call lex_error_ (17, SERROR_PRINTED(17), (error_control_table(17).severity), MERROR_SEVERITY,
			  addrel(token.Pstmt,0), Ptoken, SERROR_CONTROL, (error_control_table(17).message),
			  (error_control_table(17).brief_message), dimension(obj_token_fcn.name,1), token_value);
						/* complain if no more room to define functions.	*/
	     go to RETURN;
	     end;
	obj_token.type(Nobj_token) = Nobj_token_fcn;
	obj_token_fcn.N = Nobj_token_fcn;
	obj_token_fcn.name(Nobj_token_fcn) = token_value;

	end compile_token;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


count_reduction:	proc;				/* invoked during pass 1 to count the number of	*/
						/* reductions and tokens being compiled.	*/

	Nreductions = Nreductions + 1;		/* count reduction being parsed.		*/
	if Nreductions > Mreductions then do;		/* check for too many reductions.		*/
	     Nreductions = Mreductions;
	     Ptoken = Pthis_token;
	     Pstmt = token.Pstmt;
	     call lex_error_ (8, SERROR_PRINTED(8), (error_control_table(8).severity), MERROR_SEVERITY,
			  addrel(token.Pstmt,0), Ptoken, SERROR_CONTROL, (error_control_table(8).message),
			  (error_control_table(8).brief_message), Mreductions, fixed (stmt.line_no,35));
	     go to RETURN;
	     end;
	Pstmt = token.Pstmt;
	Llongest_red = min (254, max(Llongest_red, length(stmt_value)));
	return;

count_token:	entry (N);

     dcl	N			fixed bin;	/* number of tokens to be counted. (In)		*/

	Ntokens = min (Mtokens, Ntokens + 1);		/* By counting every token requirement of each	*/
						/* reduction, we get an upper limit on the number	*/
						/* of object tokens.			*/
	do i = 1 to N;				/* in each reduction, count length of every token	*/
	     Lobj_string = min (Mtokens, Lobj_string + token.Lvalue);
	     Ptoken = token.Pnext;			/* to get upper limit on length of string in which*/
	     end;					/* tokens will be stored by compiler.	*/

	end count_reduction;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


label_value:	procedure (label_sought)		/* invoked to obtain the reduction number which	*/
		returns	(fixed bin(17));		/* is the value of a given reduction label.	*/

     dcl	label_sought		char(*),		/* name of label whose value is sought. (In)	*/
	i			fixed bin;	/* do group index.				*/

	do i = 1 to obj_label.N while (obj_label.name(i) ^= label_sought);
	     end;					/* search for the sought label in list of defined	*/
						/* labels.				*/
	if i > obj_label.N then do;			/* if label not found in list, complain.	*/
	     call ERROR(11);
	     return(1);				/* return value for first reduction.		*/
	     end;
	else					/* if label found, return its value.		*/
	     return (obj_label.reduction_no(i));

	end label_value;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


next_reduction:	procedure;			/* invoked to compile the next-reduction field of	*/
						/* a reduction where no label is specified.  This	*/
						/* means "proceed with the next reduction".	*/

	call output ("	go to RD_NEXT_REDUCTION;			/* /	\				*/
");

	end next_reduction;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/


number:	procedure (no, statement) returns (char(*));	/* Procedure to put a sequence number at the 	*/
						/* beginning of each reduction (in label field).	*/
     dcl	no			fixed bin,
	statement			char(*);

     dcl	Isearch			fixed bin,
	number			char(4) varying;

	number = ltrim(char(no));
	if substr(ltrim(statement, HT_SP), 1, 1) = "/" then do;
						/* Don't put in a sequence number if a label	*/
						/* is already present.			*/
	     if substr(statement,1,1) = HT then
		return (number || statement);
	     if substr(statement,1,length(number)) = "" then
		return (number || substr(statement, length(number)+1));
	     if substr(statement,1,1) = "/" then
		return(statement);
	     end;

	Isearch = verify(statement, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789");
	Isearch = (Isearch-1) + verify(substr(statement,Isearch), HT_SP);
	if substr(statement,Isearch,1) = NL then do;	/* Look for label on line by itself, with next	*/
						/*   line starting with whitespace.		*/
	     Isearch = Isearch + 1;
	     if substr(statement,Isearch,1) = HT then	/*   line begins with HT.			*/
		return (substr(statement,1,Isearch-1) || number ||
		     substr(statement,Isearch));
	     if substr(statement,Isearch,length(number)) = "" then
		return (substr(statement,1,Isearch-1) || number ||
		     substr(statement,Isearch+length(number)));
	     end;
	return (statement);

	end number;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


output:	procedure	(chars);				/* invoked to write a character string into the	*/
						/* object segment.				*/

     dcl	chars			char(*);		/* the character string to be written. (In)	*/

	if length (chars) > Lobj then do;		/* make sure character string will fit.		*/
	     call ERROR(12);
	     go to RETURN;				/* give up completely.  This error is very fatal.	*/
	     end;
	substr (obj, 1, length(chars)) = chars;
	Pobj = addr (substr (obj, length(chars)+1));
	Lobj = Lobj - length(chars);
	Lobj_part = Lobj_part + length(chars);

	end output;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */



output_quote:	procedure (chars);			/* invoked to write a character string into the	*/
						/* object segment, handling the doubling of quotes*/
						/* if necessary.				*/

     dcl	chars			char(*);		/* the character string (possibly containing	*/
						/* quotes which must be doubled) to be output.	*/
     dcl	Iquote			fixed bin(21),	/* index into part of character string.		*/
	Lpart			fixed bin(21),	/* length of part of character string.		*/
	Ppart			ptr,		/* ptr to part of character string.		*/
	up_to_quote		char(Iquote) based (Ppart),
						/* part of part up to the next quote.		*/
	part			char(Lpart) based (Ppart);
						/* part of character string.			*/

	call output ("""");
	Ppart = addr(chars);
	Lpart = length(chars);
	Iquote = index(part, """");
	do while (Iquote > 0);
	     call output (up_to_quote);
	     call output ("""");
	     Ppart = addr(substr(part,Iquote+1));
	     Lpart = Lpart - Iquote;
	     Iquote = index(part, """");
	     end;
	if Lpart > 0 then call output (part);
	call output ("""");

	end output_quote;


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


output_number:	procedure (number);			/* invoked to convert a number to a character	*/
						/* string, strip off leading blanks, and output	*/
						/* the result.				*/

     dcl	number			fixed bin,	/* number to be output. (In)			*/
	ltrim			builtin;

	Nchar = number;				/* convert number to a character string.	*/
	call output (ltrim(Nchar));

	end output_number;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


output_source:
     	procedure (Ostart, Iend);

     dcl	Ostart			fixed bin(21),
	Iend			fixed bin(21);

     dcl	Lsource_part		fixed bin(21),
	Psource_part		ptr,
	source_part		char(Lsource_part) based(Psource_part);

	Psource_part = addcharno(addr(source), Ostart);
	Lsource_part = Iend - Ostart;
	call output (source_part);

	end output_source;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/


output_var:	procedure (chars);			/* invoked to write a varying character string	*/
						/* into the output segment.			*/

     dcl	chars			char(*) varying aligned;
						/* the character string to be written. (In)	*/

	if length (chars) > Lobj then do;		/* make sure character string will fit.		*/
	     call ERROR(12);
	     go to RETURN;
	     end;
	else do;
	     substr (obj, 1, length(chars)) = chars;
	     Pobj = addr (substr (obj, length(chars)+1));
	     Lobj = Lobj - length(chars);
	     Lobj_part = Lobj_part + length(chars);
	     end;

	end output_var;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


reduction_begin:	procedure;			/* invoked to begin parsing a reduction.	*/

	Nobj_red = obj_red.N + 1;			/* address the next object reduction.		*/
	if Nobj_red > obj_red.M then do;		/* if there is none, complain.		*/
	     Pstmt = token.Pstmt;
	     call lex_error_ (20, SERROR_PRINTED(20), (error_control_table(20).severity), MERROR_SEVERITY,
			  Pstmt, Ptoken, SERROR_CONTROL, (error_control_table(20).message),
			  (error_control_table(20).brief_message), fixed(stmt.line_no,35), obj_red.M,
			  fixed(stmt.line_no,35));
	     go to RETURN;
	     end;

	obj_red.Ifirst (Nobj_red) = obj_token.N + 1;	/* initiate indices of first/last token req'mts	*/
	obj_red.Ilast (Nobj_red) = obj_token.N;		/* to reflect no token requirements (so far).	*/
	Soptimize_possible = "1"b;			/* indicate optimization of token requirements is	*/
						/* possible (so far).			*/
	end reduction_begin;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 	*/


reduction_end:	procedure;			/* invoke to end parsing of a reduction.	*/

	obj_red.N = Nobj_red;			/* Formally add the completed reduction to the	*/
						/* object reduction array.			*/
	if Soptimize_possible then;			/* All end work involves optimization of token	*/
	else					/* requirement storage.  If optimization not	*/
	     return;				/* possible, quit while we're ahead.		*/
	if obj_red.Ifirst (Nobj_red) > obj_red.Ilast (Nobj_red) then
	     return;				/* same if no token requirements associated with	*/
						/* the reduction.				*/
	form = obj_token.form (obj_red.Ifirst(Nobj_red));	/* for efficiency, save value of first token 	*/
	type = obj_token.type (obj_red.Ifirst(Nobj_red));	/* requirement associated with reduction.	*/
	do i = 1 to obj_red.Ifirst(Nobj_red) - 1;	/* search through previously-defined token	*/
	     if obj_token.form(i) = form then		/* requirements for a series which match those	*/
	     if obj_token.type(i) = type then do;	/* associated with reduction.			*/
		do j = 1 to obj_red.Ilast(Nobj_red) - obj_red.Ifirst(Nobj_red);
		     if obj_token.form(i+j) = obj_token.form(obj_red.Ifirst(Nobj_red)+j) then
			if obj_token.type(i+j) = obj_token.type(obj_red.Ifirst(Nobj_red)+j) then;
			else
			     go to no_match;
		     else
			go to no_match;
		     end;
		j = j - 1;			/* make j = do-group end limit above.		*/
		obj_token.N = max(obj_red.Ifirst(Nobj_red)-1, i+j);
		obj_red.Ifirst (Nobj_red) = i;	/* if search succeeds, use previously-defined	*/
		obj_red.Ilast (Nobj_red) = i + j;	/* tokens in this reduction.			*/
		return;
no_match:		end;
	     end;

	end reduction_end;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


reductions_begin:	procedure;			/* invoked before parsing the first reduction to	*/
						/* temporary storage for object reductions, object*/
						/* tokens, and object token strings.		*/
						/* Also initialize object segment and maximum	*/
						/* severity value.				*/
	Pobj_red = translator_temp_$allocate (Psegment, size(obj_red));
	obj_red.M = Nreductions;
	Pobj_token = translator_temp_$allocate (Psegment, size(obj_token));
	obj_token.M = Ntokens;
	Pobj_token_quoted = translator_temp_$allocate (Psegment, size(obj_token_quoted));
	Pobj_string = translator_temp_$allocate (Psegment, size(obj_string));
	if S_TRACE then
	     Llongest_red = min(254, Llongest_red + log(Nreductions));
	obj_red.N = 0;
	obj_token.N = 0;
	string (obj_token_quoted) = "0"b;
	obj_token_fcn.N = 0;
	Pobj = APobj;
	Lobj = ALobj;

	end reductions_begin;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


reductions_end:	procedure;			/* invoked after all reductions have been parsed,	*/
						/* and after action routine calls have been	*/
						/* output.				*/

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */
	/*									*/
	/* 1) Store maximum severity as an output value.  If it is greater than 2, return with	*/
	/*    an empty object segment as output.					*/
	/* 2) Otherwise:								*/
	/*    a) copy the action routine calls which have already been output into temporary	*/
	/*       storage.								*/
	/*    b) re-initialize the output object segment to zero length.			*/
	/*    c) output declarations for the object reduction and token structures.		*/
	/*    d) re-output the copied action routine calls.				*/
	/*    e) output an end statement.						*/
	/*									*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * */

	if MERROR_SEVERITY > 2 then
	     go to RETURN;
	if obj_red.N = 0 then do;			/* if there is no output, give up.		*/
	     call ERROR(1);
	     go to RETURN;
	     end;
	Ptemp = APobj;				/* copy contents of object segment output so far	*/
	Ltemp = ALobj - Lobj;			/* to temp storage so we can reuse		*/
	Ltemp_obj = Ltemp;				/* space at head of object segment.		*/
	Ptemp_obj = translator_temp_$allocate (Psegment, size(temp_obj));
	temp_obj = temp;
	Pobj = APobj;
	Lobj = ALobj;

	call output(NP);				/* output start of SEMANTIC_ANALYSIS subroutine.	*/
	if S_TRACE_ON then
	     call output ("
     dcl	TRACING			bit(1) aligned int static init(""1""b);
");
	else call output ("
     dcl	TRACING			bit(1) aligned int static init(""0""b);
");
	call output ("

%include rdc_start_;
");

	call output (NP);				/* output the reduction label stack.		*/
	if S_PDL then
	     call output ("     dcl	DIRECTION			fixed bin init(-1);	/* direction in which tokens compared.		*/
");
	else call output ("     dcl	DIRECTION			fixed bin init(+1);	/* direction in which tokens compared.		*/
");
	if Sinclude_STACK then do;
	     call output ("     dcl	STACK (");
	     call output_number (Mstack_depth);
	     call output (")   		fixed bin,	/* reduction label stack.			*/
	STACK_DEPTH		fixed bin init (0);	/* index into STACK.			*/
");
	     end;

						/* output declaration for object reductions.	*/
	call output ("

     dcl	1 REDUCTION (");
	call output_number (obj_red.N);
	call output (")		unaligned based (addr (REDUCTIONS)),
						/* object reductions.			*/
	  2 TOKEN_REQD,
");
	call output ("	    3 IFIRST		fixed bin(17),	/* index of first required token.		*/
	    3 ILAST		fixed bin(17),	/* index of last required token.		*/

	REDUCTIONS  (");
	call output_number (obj_red.N + obj_red.N);
	call output (")		fixed bin(17) unaligned internal static options(constant) initial (
");

	do i = 1 to obj_red.N;
	     call output ("	     ");
	     if S_PDL then Nchar = obj_red.Ilast (i);
		    else Nchar = obj_red.Ifirst(i);
	     call output ((Nchar));
	     call output (", ");
	     if S_PDL then Nchar = obj_red.Ifirst(i);
		    else Nchar = obj_red.Ilast (i);
	     call output ((Nchar));
	     if i = obj_red.N then
		call output (");	/* ");
	     else
		call output (",	/* ");
	     Nchar = i;
	     call output ((Nchar));
	     call output ("/  ");
	     Lobj_part = 41;
	     do j = obj_red.Ifirst(i) to obj_red.Ilast(i);
		if obj_token.form(j) > 0 then do;
		     Pobj_string_part = addr(substr(obj_string, obj_token.form(j)));
		     Lobj_string_part = obj_token.type(j);
		     if obj_token_quoted(j) then
			call output_quote (obj_string_part);
		     else call output (obj_string_part);
		     call output (" ");
		     end;
		else if obj_token.form(j) = 0 then do;
		     go to comment (obj_token.type(j));

comment(1):	     call output ("<no-token> ");
		     go to end_comment;
comment(2):	     call output ("<any-token> ");
		     go to end_comment;
comment(3):	     call output ("<name> ");
		     go to end_comment;
comment(4):	     call output ("<decimal-integer> ");
		     go to end_comment;
comment(5):	     call output ("<BS> ");
		     go to end_comment;
comment(6):	     call output ("<quoted-string> ");
end_comment:	     end;
		else do;
		     call output ("<");
		     call output_var (obj_token_fcn.name(obj_token.type(j)));
		     call output ("> ");
		     end;
		end;
	     Lobj_spaces = max(0, 110-Lobj_part);
	     call output (obj_spaces);
	     call output ("*/
");
	     end;

	call output (NP);				/* output declaration for object tokens.	*/
	call output ("     dcl	1 TOKEN_REQUIREMENT (");
	call output_number (obj_token.N);
	call output (")	unaligned based (addr (TOKEN_REQUIREMENTS)),
						/* object token requirements.			*/
	  2 FORM			fixed bin(17),	/* form of the token requirement:		*/");
	call output ("
						/*  -1 = relative token requirement function;	*/
						/*       TYPE = index of the particular token	*/
						/*	      function in the token_fcn array.	*/
						/*   0 = built-in token requirement function;	*/");
	call output ("
						/*       TYPE = as defined below.		*/
						/*  >0 = absolute token requirement:		*/
						/*       FORM = index(TOKEN_STRINGS,TOKEN_REQD);	*/
						/*       TYPE = length(TOKEN_REQD);		*/");
	call output ("
	  2 TYPE			fixed bin(17) unal,	/* type of the built-in token requirement	*/
						/* function:				*/
						/*   1 = compile test to see if input token 	*/");
	call output ("
						/*       chain is exhausted (<no-token>).	*/
						/*   2 = compile test for any token value	*/
						/*       (<any-token>).			*/");
	call output ("
						/*   3 = compile test for a PL/I identifier	*/
						/*       (<name>) of 32 or fewer characters.	*/
						/*   4 = compile test for token which is a	*/
						/*       <decimal-integer>.			*/");
	call output ("
						/*   5 = compile test for token which is a single	*/
						/*       backspace character (<BS>).		*/
						/*   6 = compile test for a token which is a	*/
						/*       <quoted-string>.			*/");
	call output ("

	TOKEN_REQUIREMENTS  (");
	call output_number (obj_token.N + obj_token.N);
	call output (")	fixed bin(17) unaligned internal static options(constant) initial (");
	do i = 1 to obj_token.N;
	     call output ("
	     ");
	     do i = i to min (obj_token.N, i + 6);
		Nchar = obj_token.form(i);
		call output ((Nchar));
		call output (",");
		Nchar = obj_token.type(i);
		call output ((Nchar));
		if i = obj_token.N then
		     call output (");
");
		else
		     call output (",   ");
		end;
	     i = i - 1;
	     end;

	i = length (obj_string);			/* output declaration for object token values.	*/
	call output ("

     dcl	TOKEN_STRINGS		char(");
	call output_number (i);
	call output (") aligned based (addr (TOKEN_STRING_ARRAYS)),
						/* object token values.			*/
");
	i = divide (length(obj_string),100,17,0) + 1;	/* compute number of 100-char substrings.	*/
	call output ("	TOKEN_STRING_ARRAYS (");
	call output_number (i);
	call output (")	char(100) aligned internal static options(constant) initial (
");
	Lobj_string_part = 100;
	do i = 0 to i-2;
	     call output ("	     """);
	     Pobj_string_part = addr (substr (obj_string, i*100+1));
	     call output (obj_string_part);
	     call output (""",
");
	     end;
	call output ("	     """);
	Pobj_string_part = addr (substr (obj_string, i*100+1));
	Lobj_string_part = length(obj_string) - i*100;
	call output (obj_string_part);
	call output (""");
");

	call output (NP);				/* output include statement for end semant.	*/
	call output ("	%include rdc_end_;
");
	if obj_token_fcn.N > 0 then do;		/* output relative syntax function calls, if any. */
	     call output ("
	     else do;				/* relative syntax function.			*/
		go to RD_TOKEN_FCN(TOKEN_REQD.TYPE);
");
	     do i = 1 to obj_token_fcn.N;
		call output ("
RD_TOKEN_FCN(");	call output_number(i);
		call output("):	STOKEN_FCN = ");
		call output_var (obj_token_fcn.name(i));
		call output ("();
		go to RD_TEST_RESULT;");
		end;
	     call output ("

RD_TEST_RESULT:	if STOKEN_FCN then go to RD_MATCH;
		else go to RD_NEXT_REDUCTION;
		end;
");
	     end;

	if S_PDL then
	     call output ("
RD_MATCH:	     Ptoken = token.Plast;
RD_MATCH_NO_TOKEN:
	     end;
	Ptoken = Pthis_token;
");
	else call output ("
RD_MATCH:      Ptoken = token.Pnext;
RD_MATCH_NO_TOKEN:
	     end;
	Ptoken = Pthis_token;
");
	if S_TRACE then do;
	     call output ("
	if TRACING then do;
	     call PRINT_REDUCTION(NRED);
	     call PRINT_TOKENS (DIRECTION, (RED.TOKEN_REQD.IFIRST), (RED.TOKEN_REQD.ILAST));
	     end;
");
	     end;
	call output ("	go to RD_ACTION(NRED);
");

	if Sinclude_STACK then do;			/* include the label stack functions.		*/
	     call output (NP);
	     call output ("	%include rdc_stack_fcns_;
");
	     end;

	call output (NP);				/* output action routine calls saved previously.	*/
	call output (temp_obj);
	if S_TRACE then do;
	     call output (NP);
	     call output ("%include rdc_tracing_fcns_;
");
	     end;
	call output ("

	end SEMANTIC_ANALYSIS;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */
");

	call output (NP);				/* define the PUSH DOWN LANGUAGE switch and	*/
						/*   include the LEX subroutine in object seg.	*/
	if S_PDL then
	     call output ("     dcl	SPDL			bit(1) aligned init (""1""b);
						/* on: This compiler parses a PUSH DOWN LANGUAGE.	*/

");	else
	     call output ("     dcl	SPDL			bit(1) aligned init (""0""b);
						/* off: This compiler parses a non-PUSH DOWN	*/
						/*      LANGUAGE.				*/
");
	if Sinclude_LEX then
	     call output ("	%include rdc_lex_;
");
	if Sinclude_DELETE then do;			/* include the DELETE subroutine.		*/
	     call output (NP);
	     call output ("	%include rdc_delete_;
");	     end;
	if Sinclude_DELETE_STMT then do;		/* include the DELETE_STMT subroutine.		*/
	     call output (NP);
	     call output ("	%include rdc_delete_stmt_;
");	     end;
	if Sinclude_ERROR then do;			/* output include statement for ERROR message proc*/
	     call output (NP);
	     call output ("	%include rdc_error_;
");
	     end;

	if Sinclude_NEXT_STMT then do;		/* output NEXT_STMT proc.			*/
	     call output (NP);
	     call output ("	%include rdc_next_stmt_;
");
	     end;

	if S_TRACE then do;
	     call output (NP);
	     call output ("     dcl	RED_TEXT (");
	     call output_number (Nreductions);
	     call output (")		char (");
	     call output_number (Llongest_red);
	     Llongest_red = Llongest_red - log(Nreductions);
						/* leave room for reduction number.		*/
	     call output (") varying int static options(constant) init (
");
	     Pstmt = Psave -> token.Pstmt;
	     call output_quote (number(1, substr(stmt_value,1,min(length(stmt_value), Llongest_red))));
	     do i = 2 to Nreductions;
		call output (",
");
		Pstmt = stmt.Pnext;
		call output_quote (number(i, substr(stmt_value,1,min(length(stmt_value), Llongest_red))));
		end;
	     call output (");
");
	     end;

	Ptemp = APobj;				/* again, copy what we've generated in obj seg	*/
	Ltemp = ALobj - Lobj;			/* so we can reuse beginning.			*/
	Ltemp_obj = Ltemp;
	Ptemp_obj = translator_temp_$allocate (Psegment, size(temp_obj));
	temp_obj = temp;

	Pobj = APobj;				/* output segment header for object segment.	*/
	Lobj = ALobj;
	call output ("

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */
	/*							*/
	/* COMPILED OUTPUT OF SEGMENT  ");
	call output (Aname_source);
	call output ("	*/
	/* Compiled by:  reduction_compiler, Version 2.5 of Oct 21, 1985      */
	/* Compiled on:  ");
	date = date_time_$format ("date_time", clock_(), "", "");
	call output (date);
	call output ("*/
	/*							*/
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */
");
	call output(NP);

	Pstmt = Psave -> token.Pstmt;			/* copy source into object segment.		*/
	Osc_start = 0;				/*   each reduction is written separately so it	*/
	do i = 1 to Nreductions;			/*   can be numbered for ease of debugging.	*/
	     call output_source (Osc_start, charno(addr(stmt_value)));
	     call output (number(i, stmt_value));
	     Osc_start = charno(addr(stmt_value)) + length(stmt_value);
	     Pstmt = stmt.Pnext;
	     end;
	call output_source (Osc_start, length(source));

	call output(temp_obj);			/* output object previously generated & saved.	*/
						/* output final end statement for translator.	*/
	call suffixed_name_$new_suffix (Aname_source, "rd", "", name_source, code);
	call output ("
	end ");
	call output (rtrim(name_source));
	call output (";
");
	ALobj = ALobj - Lobj;			/* adjust length of object seg returned to caller.*/

	end reductions_end;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


reductions_init:	procedure;			/* invoked before pass1 of parse to		*/
						/* initialize maximum stack depth, reduction and	*/
						/* token counters, & maximum object string length.*/
						/* Set switch to suppress inclusion of ERROR proc */
						/* unless is it actually referenced.  Do same for	*/
						/* reduction STACK fcns, NEXT_STMT proc, and	*/
						/* DELETE procs. Default to ^'PUSH DOWN LANGUAGE'.*/
	Mstack_depth = 10;				/* maximum stack depth is 10, by default.	*/
	Nreductions = 0;
	Ntokens = 0;
	Llongest_red = 0;
	Lobj_string = 0;
	obj_label.N = 0;
	S_PDL = "0"b;
	Sinclude_DELETE = "0"b;
	Sinclude_DELETE_STMT = "0"b;
	Sinclude_ERROR = "0"b;
	Sinclude_NEXT_STMT = "0"b;
	Sinclude_LEX = "0"b;
	Sinclude_STACK = "0"b;

	end reductions_init;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


rtn:	procedure (type);				/* invoked to compile one of the pre-defined	*/
						/* (pre-, mid-, or post-) actions.		*/

     dcl	type			fixed bin;	/* type of action to be compiled.		*/
						/*   1 = LEX				*/
						/*   2 = POP				*/
						/*   3 = PUSH(<label>)			*/

	go to RTN(type);

RTN(1):	if S_PDL then do;
	     call output ("	call					     LEX(1);
");
	     Sinclude_LEX = "1"b;
	     end;
	else
	     call output ("	Ptoken, Pthis_token = Pthis_token -> token.Pnext;	/*   LEX					*/
");	return;

RTN(2):	call output ("	STACK_DEPTH = max(STACK_DEPTH-1,0);		/*   POP					*/
");	Sinclude_STACK = "1"b;
	return;

RTN(3):	call output ("	call PUSH(");
	call output_number (label_value(token_value));
	call output (");				/*   PUSH(");
	Lobj_part = 70;
	call output (token_value);
	call output (")");
	Lobj_spaces = 110 - Lobj_part;
	call output (obj_spaces);
	call output ("*/
");	Sinclude_STACK = "1"b;
	return;

	end rtn;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


set_action:	procedure;			/* invoked to compile a call to an action routine.*/

	call output ("	call					     ");
	call output (token_value);
	call output ("();
");

	end set_action;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


set_action_with_args:	procedure;		/* invoked to compile a call to an action routine	*/
						/* that requires input arguments.		*/

	call output ("	call 					     ");
	call output (token_value);
	call output (" (");

	end set_action_with_args;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


set_depth:	procedure;			/* invoked to set the maximum allowable depth of	*/
						/* the reduction stack.			*/

	Mstack_depth = token.Nvalue;
	if Mstack_depth <= 0 | Mstack_depth > MMstack_depth then do;
	     call lex_error_ (18, SERROR_PRINTED(18), (error_control_table(18).severity), MERROR_SEVERITY,
			  addrel(token.Pstmt,0), Ptoken, SERROR_CONTROL, (error_control_table(18).message),
			  (error_control_table(18).brief_message), MMstack_depth, MMstack_depth);
	     Mstack_depth = MMstack_depth;
	     return;
	     end;

	end set_depth;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


set_label:	procedure;			/* invoked to store an association between a	*/
						/* the label of a reduction and the number of the	*/
						/* reduction into the object label store.	*/

     dcl	N			fixed bin(17);	/* number of labels stored in object label array.	*/

	do N = 1 to obj_label.N while (obj_label.name(N) ^= token_value);
	     end;					/* see if label is already defined.		*/
	if N <= obj_label.N then do;			/* it is! Complain.				*/
	     call ERROR(13);
	     return;
	     end;
	if N > dimension (obj_label.set, 1) then do;
	     call lex_error_ (7, SERROR_PRINTED(7), (error_control_table(7).severity), MERROR_SEVERITY,
			  addrel(token.Pstmt,0), Ptoken, SERROR_CONTROL, (error_control_table(7).message),
			  (error_control_table(7).brief_message), dimension(obj_label.set,1), token_value);
	     go to RETURN;
	     end;
	obj_label.N = N;
	obj_label.name(N) = token_value;
	obj_label.reduction_no(N) = Nreductions + 1;

	end set_label;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


specified_label:	procedure;			/* invoked to compile the next-reduction field of	*/
						/* a reduction where a label was specified.  This	*/
						/* means "proceed with the reduction whose label	*/
						/* was specified".				*/

	call output ("	NRED = ");
	call output_number (label_value(token_value));
	call output (";
	go to RD_TEST_REDUCTION;			/* / ");
	Lobj_part = 0;
	call output (token_value);
	call output (" \");
	Lobj_spaces = max (0, 34 - Lobj_part);
	call output (obj_spaces);
	call output ("		*/
");

	end specified_label;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


stacked_reduction:	procedure;			/* invoked to compile the next-reduction field of	*/
						/* a reduction where STACK is specified.  This	*/
						/* means "proced with the reduction whose label	*/
						/* is on the top of the reduction stack".	*/

	call output ("	go to RD_STACK;				/* / STACK	\			*/
");
	Sinclude_STACK = "1"b;

	end stacked_reduction;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


stacked_reduction_pop:	procedure;		/* invoked to compile the next-reduction field of	*/
						/* a reduction where STACK_POP is specified. 	*/
						/* This means "proceed with the reduction whose	*/
						/* label is on the top of the reduction stack,	*/
						/* and pop the stack".			*/

	call output ("	go to RD_STACK_POP;				/* / STACK_POP	\			*/
");
	Sinclude_STACK = "1"b;

	end stacked_reduction_pop;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


terminal_reduction:	procedure;			/* invoked to compile the next-reduction field of	*/
						/* a reduction where RETURN is specified.  This 	*/
						/* means "compilation is complete; return to the	*/
						/* caller of the compiler".			*/

	call output ("	return;					/* / RETURN	\			*/
");
	end terminal_reduction;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


