/* HISTORY COMMENTS:
  1) change(2016-05-30,GDixon), approve(2016-10-13,MCR10014),
     audit(2016-10-13,Swenson), install(2016-10-13,MR12.6f-0002):
     Initial implementation of call_scalar_dcl_.rd (based upon call_entry_info_.rd).
                                                   END HISTORY COMMENTS */



%page;
/*++

BEGIN	/ declare				/ LEX(1)					         / varName	 \
	/ dcl    				/ LEX(1)					         / varName	 \
	/ <any-token>			/ Err(no_declare_key)			         / RETURN	 \
	/ <no-token>			/ Err(empty_string) [code=call_et_$bad_declaration]        / RETURN    \

varName	/ <varName>			/ set_varName LEX(1)			         / attrBegin \
	/ <any-token>			/ Err(bad_varname)				         / RETURN    \
	/ <no-token>			/ Err(incomplete_dcl)			         / RETURN    \
	
					  \" SUBROUTINE: attrBegin, a reduction subroutine that 
					  \"  parses attributes of a variable, forming a single
					  \"  data <descriptor>.  Structure and array declarations
					  \"  are parsed, but diagnosed as errors.  This program
					  \"  is designed to produce a scalar variable descriptor.
					  \" Inputs:
					  \"  Pthis_token -> first attribute of the declaration.
attrBegin /				/        begin_descriptor  no_structure_yet                /           \
					  \" Begin parsing a <descriptor> by initializing 
					  \"  the t.ATTRIBUTE structure, and starting token of the
					  \"  <descriptor>.

					  \" Structure level number and/or array bounds without
					  \"  <dim-key> must appear at the beginning of a 
					  \"  <descriptor>.  Other attributes may follow in 
					  \"  almost any order.
parmBegin	/ <level_starts_struct>		/ start_structure          LEX(1) [t.structure=T]          / bndParen  \
	/ <level_for_item_in_struct>		/ continue_structure       LEX(1)		         / bndParen  \
	/ <any-token>			/ end_structure				         /	 \
					  \" Any non-<level> token at start of parameter means the
					  \"  prior structure parameter ends (has no more items 
					  \"  within the structure).
bndParen  / (        			/        [t.dimensioned=T] PUSH(attrs)		         / bounds	 \

					  \" Look for delimiters that can end a <descriptor>.
attrs	/ , <level_for_item_in_struct>          /                          LEX(1)                          / parmBegin \
					  \" Each item in a structure requires a pass through
					  \" the parm subroutine to get <descriptor> for that
					  \" structure item.  Items are treated as components
					  \" of the outer-level structure <descriptor>.
          / ,				/ Err(multi_returns_attrs)              	         / RETURN    \
					  \" EXIT SUBROUTINE WITH ERROR: multiple <parameter-set>s
					  \" A between-parameter comma (,) is not permitted inside
					  \"  a scalar declaration.

	/ ;				/ end_descriptor           LEX(1)                          / RETURN    \
					  \" RETURN FROM SUBROUTINE: parm (no errors found)
 
					  \" Look for <attribute>s that form the <attribute-set>
					  \"  of a <descriptor>.
	/ dimension                             / LEX(1) [t.dimensioned=T] PUSH(attrs)		         / bounds	 \
	/ dim                                   / LEX(1) [t.dimensioned=T] PUSH(attrs)		         / bounds	 \
					  \" If specified via <dim-key>, bounds can appear anywhere
					  \" in the <attribute-set> of a <descriptor>.

	/ fixed                                 / LEX(1) [t.fixed=T]       PUSH(attrs)		         / prec	 \
	/ float                                 / LEX(1) [t.float=T]       PUSH(attrs)		         / prec	 \

	/ binary                                / LEX(1) [t.binary=T]      PUSH(attrs)		         / prec	 \
	/ bin                                   / LEX(1) [t.binary=T]      PUSH(attrs)		         / prec	 \
	/ decimal                               / LEX(1) [t.decimal=T]     PUSH(attrs)		         / prec	 \
	/ dec                                   / LEX(1) [t.decimal=T]     PUSH(attrs)		         / prec	 \

	/ real                                  / LEX(1) [t.real=T]	       PUSH(attrs)		         / prec	 \
	/ complex                               / LEX(1) [t.complex=T]     PUSH(attrs)		         / prec	 \
	/ cplx                                  / LEX(1) [t.complex=T]     PUSH(attrs)		         / prec	 \

          / precision                             / LEX(1)                   PUSH(attrs)		         / prec	 \
	/ prec                                  / LEX(1)		       PUSH(attrs)		         / prec	 \

          / varying				/ LEX(1) [t.varying=T]			         / attrs     \
	/ var    				/ LEX(1) [t.varying=T]			         / attrs     \
	/ nonvarying			/ LEX(1) [t.varying=F]			         / attrs     \
	/ nonvar				/ LEX(1) [t.varying=F]			         / attrs     \
	/ bit 				/ LEX(1) [t.bit=T]         PUSH(attrs)		         / size	 \
	/ char 				/ LEX(1) [t.char=T]	       PUSH(attrs)		         / size	 \
	/ character 			/ LEX(1) [t.char=T]	       PUSH(attrs)		         / size	 \
          / pic				/ Err(no_pictures)	       LEX(1)		         / RETURN	 \
          / picture				/ Err(no_pictures)	       LEX(1)		         / RETURN	 \

	/ area 				/ LEX(1) [t.area=T]	       PUSH(attrs)		         / size	 \

	/ pointer 			/ LEX(1) [t.ptr=T]				         / attrs	 \
	/ ptr				/ LEX(1) [t.ptr=T]				         / attrs	 \
	/ offset				/ LEX(1) [t.offset=T]			         / attrs	 \
	/ label				/ LEX(1) [t.label=T]			         / attrs	 \

	/ entry (				/ LEX(1) [t.entry=T] PUSH(attrs)		         / skipCl	 \
					  \" We don't parse recursive entry dcls.  Use the skipC
					  \" reduction subroutine to skip over all tokens until
					  \" a matching right paren is found.  Upon return from
					  \" skipCl, continue parsing other attributes.
	/ entry 				/ LEX(1) [t.entry=T]			         / attrs     \	

	/ file 				/ LEX(1) [t.file=T] 			         / attrs	 \	

	/ aligned				/ LEX(1) [t.aligned=T]			         / attrs	 \
	/ unaligned			/ LEX(1) [t.unaligned=T]			         / attrs	 \
	/ unal    			/ LEX(1) [t.unaligned=T]			         / attrs	 \

	/ signed				/ LEX(1) [t.signed=T]			         / attrs	 \
	/ unsigned			/ LEX(1) [t.unsigned=T]			         / attrs	 \
	/ uns     			/ LEX(1) [t.unsigned=T]			         / attrs	 \
	
	/ structure			/ LEX(1) [t.structure=T]			         / attrs	 \
	/ variable			/ LEX(1) [t.variable=T]			         / attrs	 \

	/ <any-token>			/ Err(unknown_attribute)			         / RETURN    \
					  \" EXIT SUBROUTINE WITH ERROR: unknown attribute
          / <no-token>			/ Err(incomplete_dcl)			         / RETURN    \
					  \" EXIT SUBROUTINE WITH ERROR: incomplete <attribute-set>
					  \" END SUBROUTINE: parm ------------------------------------------
					     
					  \" SUBROUTINE: bounds, a reduction subroutine to process an
					  \"  optional precision subclause of attribute: 
					  \"	fixed float bin dec real complex prec
					  \" Inputs:
					  \"  Pthis_token -> left-paren that starts the subclause.
bounds    / (				/ LEX(1)            new_dim           start_clause         / newDim    \
	/				/					         / STACK_POP \
					  \" RETURN FROM THE SUBROUTINE: no bounds given

newDim    / <int> : <int> ,                       / set_lbound LEX(2) set_hbound LEX(2) continue_clause
							new_dim			         / newDim	 \
          / <int> : * ,                           / set_lbound LEX(2) set_hbound LEX(2) continue_clause
							new_dim			         / newDim	 \
	/ <int> ,  			/ set_bound  LEX(2)	new_dim	        continue_clause      / newDim	 \
          / * ,                                   / set_bound  LEX(2) new_dim           continue_clause      / newDim    \
                                                                      
					  \" Another dimension in the bounds subclause
	/ <int> : <int> )   		/ set_lbound LEX(2) set_hbound set_bounds_clause LEX(2)    / STACK_POP \
	/ <int> : * )        		/ set_lbound LEX(2) set_hbound set_bounds_clause LEX(2)    / STACK_POP \
	/ <int> ) 			/ set_bound		 set_bounds_clause LEX(2)    / STACK_POP \
          / * )                                   / set_bound		 set_bounds_clause LEX(2)    / STACK_POP \
					  \" RETURN FROM SUBROUTINE: end of bounds (no errors)
	/ <any-token>			/                                     PUSH(newDimErr)      / skipCl    \
					  \" Look for closing right paren of bounds clause, to put
					  \"  entire clause in an error message.
newDimErr /				/ ErrCl(bad_array_bound)			         / RETURN	 \
					  \" EXIT WITH ERROR: malformed bounds subclause
					  \" END SUBROUTINE: bounds -----------------------------------------

					  \" SUBROUTINE: prec, a reduction subroutine to process an
					  \"  optional precision subclause of attribute: 
					  \"	fixed float bin dec real complex prec
					  \" Inputs:
					  \"  Pthis_token -> left-paren that starts the subclause.
prec      /                                       /        [t.precision=T]                                   /           \
	/ ( <intPos> , <int> )                  / LEX(1) set_prec LEX(2) set_scale LEX(2)	         / STACK_POP \
	/ ( <intPos> )			/ LEX(1) set_prec LEX(2)			         / STACK_POP \
	/ (                                     /	             PUSH(precErr) start_clause          / skipCl    \
					  \" Call start_clause    ourself so clause begins with 
					  \"  attribute preceding ( char: fixed, float, bin, etc.
          /				/				                   / STACK_POP \
					  \" precision clause is optional.  Return to caller of 
					  \"  this reduction subroutine.

precErr   /				/ ErrCl(bad_precision)			         / RETURN	 \
					  \" EXIT WITH ERROR: invalid precision subclause
					  \" END SUBROUTINE: prec -------------------------------------------

					  \" SUBROUTINE: size, a reduction subroutine to process an
					  \"  optional size subclause following: bit char area
					  \" Inputs:
					  \"  Pthis_token -> left-paren that starts the subclause.
size	/ ( <intNNeg> )			/ LEX(1) set_size LEX(2)			         / STACK_POP \
	/ ( <starNameRef> )			/ LEX(1) set_nr   LEX(2)			         / STACK_POP \
	/ (     *     )			/ LEX(1) set_size LEX(2)			         / STACK_POP \
	/ (                                     /	             PUSH(sizeErr) start_clause          / skipCl    \
					  \" Call start_clause ourself so clause begins with 
					  \"  attribute preceding ( : bit, char, area
          /				/				                   / STACK_POP \
					  \" size clause is optional.  Return to caller of this
					  \"  reduction subroutine.


sizeErr   /				/ ErrCl(bad_size)    			         / RETURN	 \
					  \" EXIT WITH ERROR: invalid size subclause
					  \" END SUBROUTINE: size -------------------------------------------

					  \" SUBROUTINE: skipCl, a reduction subroutine to skip
					  \"  over a parenthesized subclause that is to be ignored
					  \"  when parsing the dclString.
					  \" Inputs:
					  \"  Pthis_token -> left-paren that starts the subclause.
skipCl    / (				/ LEX(1) [parenDepth=1]            start_clause            /           \
					  \" Initialize skipCl subr by: LEXing the starting 
					  \" left-paren; setting parenDepth of 1; and noting loc
					  \" of starting right-paren, in case of unbalanced parens.
rParenBal	/ (				/ LEX(1) [parenDepth=parenDepth+1] continue_clause         / rParenBal \
	/ <balanced_rParen>			/ LEX(1)			     clear_clause	         / STACK_POP \
					  \" RETURN FROM SUBROUTINE:
					  \"  Found balancing right-paren. 
	/ )				/ LEX(1) [parenDepth=parenDepth-1] continue_clause         / rParenBal \
	/ <any-token>			/ LEX(1)			     continue_clause         / rParenBal \
	/ <no-token>			/ ErrCl(unbalanced_parentheses)		         / RETURN	 \
					  \" EXIT WITH ERROR: unbalanced parentheses
					  \" END SUBROUTINE: skipCl -----------------------------------------
  ++*/

%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* In generating the reductions above to parse an entry declaration, the following information    */
	/* sources played a significant role in shaping the organization and terminology used for the     */
	/* reductions.  Source of this information:					        */
	/*									        */
	/* 		PL/I Language Specification Manual, AG94-02, RevE 03/81		        */
	/*									        */
	/*									        */
	/* (from page 5-2 ff)							        */
	/*									        */
	/* 5.2.1 Declare Statements							        */
	/*									        */
	/* Each <declare statement> is processed by the compiler and behaves like a <null statement>      */
	/* when executed.								        */
	/*									        */
	/* Syntax:								        */
	/*      <declare statement>::= [<label prefix>]  {declare|dcl} <declaration list>;	        */
	/*									        */
	/*      <declaration list>::= <declaration component>[,<declaration component>] ...	        */
	/*									        */
	/*      <declaration component>::= [<level>]{<declared name>|(<declaration list>)}	        */
	/*           [<attribute set>]						        */
	/*									        */
	/*      <declared name>::= <identifier>						        */
	/*									        */
	/*      <attribute set>::= <attribute> ...					        */
	/*									        */
	/*      <level>::= <decimal integer>						        */
	/*									        */
	/*									        */
	/* (from page 5-19)								        */
	/*									        */
	/* 5.4.17  Entry								        */
	/*									        */
	/* Syntax:								        */
	/*									        */
	/*      <entry attribute>::= entry[((<parameter-list>])]				        */
	/*									        */
	/*      <parameter-list>::= <parameter-descriptor>[,<parameter-descriptor>]...		        */
	/*									        */
	/*      <parameter-descriptor>::= <descriptor>					        */
	/*									        */
	/*      <descriptor>::= <level>[<attribute-set>]|[<level>]<attribute-set>		        */
	/*									        */
	/*      <attribute-set>::= <attribute>...					        */
	/*									        */
	/*									        */
	/* (from page 5-18)								        */
	/*									        */
	/* 5.4.15  Dimension							        */
	/*									        */
	/*      <dimension attribute>::= [<dim-key>][(<bound>[,<bound>]...)]			        */
	/*									        */
	/*      <dim-key>::= dimension|dim						        */
	/*									        */
	/*      <bound>::= {[<extent-expression>:]<extent-expression>}|*			        */
	/*									        */
	/*									        */
	/* (from page 5-30)								        */
	/*     [altered to reflect that PL/I disallows "...returns;" but permits "...returns();" ]        */
	/*									        */
	/* 5.4.47  Returns 								        */
	/*									        */
	/* Syntax:								        */
	/*									        */
	/*      <returns attribute>::= returns([<returns-descriptor>])			        */
	/*									        */
          /*      <returns-descriptor>::= <descriptor>[,<descriptor>]...			        */
	/*									        */
	/*      <descriptor>::= <level>[<attribute-set>]|[<level>]<attribute-set>		        */
	/*									        */
	/*      <attribute-set>::= <attribute>...					        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Source of this information:						        */
	/*									        */
	/* 		PL/I Language Specification Manual, AG94-02, RevE 03/81		        */
	/*									        */
	/* Consistent Attribute Sets (from page 5-33)					        */
	/*									        */
	/* Each <literal constant set> must be produced by the declaration of a <literal constant>        */
	/* and each <descriptor set> must be produced by the declaration derived from a <descriptor>.     */
	/* A <named constant set>, other than an external entry constant or file constant, must           */
	/* be produced by a declaration derived from a <label prefix>.			        */
	/*									        */
	/* Syntax:								        */
	/*									        */
	/*      <consistent attribute set>::= <condition set>|<builtin set>|			        */
	/*           <generic set>|<literal constant set>|				        */
	/*       	   <named constant set>|<descriptor set>|				        */
	/*           <variable set>							        */
	/*									        */
	/*      <condition set>::= external condition					        */
	/*									        */
	/*      <builtin set>::= internal builtin					        */
	/*									        */
	/*      <generic set>::= internal generic					        */
	/*									        */
	/*      <literal constant set>::= {<arithmetic>|bit|character}			        */
	/*           constant							        */
	/*									        */
	/*      <named constant set>::= internal label constant[dimension]|			        */
	/*           internal format constant|<scope><entry>constant|			        */
	/* 	   <scope>file<consistent file description>constant			        */
	/*									        */
	/*      <descriptor set>::= <data type><alignment>[dimension][member] [<sign type>]	        */
	/*									        */
	/*      <variable set>::= variable<data type><alignment>[dimension]			        */
	/*           <scope class>[initial] [<sign type>]					        */
	/*									        */
	/*      <data type>::= <arithmetic>|<string>|<entry>|structure[like]|			        */
	/*           pointer|offset|area|label[local]|format[local]|file			        */
	/*									        */
	/*      <arithmetic>::= {fixed|float}{binary|decimal}{real|complex}			        */
	/*      	   precision							        */
	/*									        */
	/*      <string>::= picture[real|complex]|					        */
	/*           {bit|character}{varying|nonvarying}					        */
	/*									        */
	/*      <entry>::= entry[options]						        */
	/*           {reducible returns|irreducible[returns]}				        */
	/*									        */
	/*      <alignment>::= aligned|unaligned					        */
	/*									        */
	/*      <scope>::= internal|external						        */
	/*									        */
	/*      <scope class>::= automatic internal|based internal|				        */
	/*           static<scope>|controlled<scope>|parameter internal|			        */
	/* 	   defined internal[position]|member internal				        */
	/*									        */
	/*      <consistent file description>::= <stream description>|<record description>	        */
	/*									        */
	/*      <stream description>::= stream{input|output[print][environment]}		        */
	/*									        */
	/*      <record description>::= record{input|output|update}				        */
	/*           {<sequential description>|<direct description>}[environment]		        */
	/*									        */
	/*      <sequential description>::= sequential[keyed]				        */
	/*									        */
	/*      <direct description>::= direct keyed					        */
	/*									        */
	/*      <sign type>::= signed|unsigned						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Tokens that might appear as attributes in an entrypoint declare statement:		        */
	/*									        */
	/*      (Words on the same line are alternate attributes of the same type/class/kind.)	        */
	/*									        */
	/*  Structure       Member							        */
	/*  Dimension								        */
	/*									        */
	/*  Automatic       Based           Controlled      Parameter      Static		        */
	/*  External        Internal							        */
	/*									        */
	/*  Bit             Character							        */
	/*  Nonvarying      Varying							        */
	/*									        */
	/*  Real            Complex							        */
	/*  Fixed           Float							        */
	/*  Binary          Decimal							        */
	/*  Signed          Unsigned							        */
	/*  Precision								        */
	/*									        */
	/*  Aligned         Unaligned							        */
	/*									        */
	/*  Entry           Pointer							        */
	/*  Returns								        */
	/*									        */
	/*  Area            Offset         Format          Label				        */
	/*									        */
	/*									        */
	/* OTHER UNGROUPED ATTRIBUTES (that don't appear in entrypoint declarations)		        */
	/*									        */
	/*  Builtin         File           Irreducible     Options         Record		        */
	/*  Condition                      Keyed           Output          Reducible		        */
	/*  Constant        Generic                        Picture         Sequential		        */
	/*  Defined         Initial        Like            Position        Stream		        */
	/*  Direct          Input          Local           Print           Update		        */
	/*  Environment                                                    Variable		        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Subroutine:  call_scalar_dcl_						        */
	/*									        */
	/* Function:  Converts a string containing a PL/I declare statement for a variable into an        */
	/* argument descriptor.  This conversion is carried out by parsing the PL/I declare statement     */
	/* into components, and building the descriptor from the semantic data obtained.                  */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

call_scalar_dcl_:
     proc (areaP, dclString, debugI, variableName, descriptorB, starSizeName, code);

  dcl  areaP ptr;					/* Area setup by translator_temp_.  Used by          (in) */
						/*  lex_string_ to hold token descriptors.	        */
  dcl  dclString char(*);				/* String holding a variable declaration.            (in) */
  dcl  debugI fixed bin(3) unsigned;			/* Display debugging information to the user.        (in) */
						/*  =0: no debugging	  =1: show inputs           */
						/*  =2: show generated PL/I	  =3: show descriptors      */
						/*  =4: trace reductions			        */
  dcl  variableName char(256) var;			/* Variable name found in dclString.                (out) */
  dcl  descriptorB bit(36) aligned;			/* Resulting arg descriptor created from dclString. (out) */
						/*  This storage may be overlaid by either arg_descriptor */
						/*  or fixed_arg_descriptor (in arg_descriptor.incl.pl1). */
  dcl  starSizeName char(*) var;			/* Name given in a char(), bit(), or area() size.   (out) */
  dcl  code fixed bin(35);				/* Status code for errors encountered.		  (out) */

						/* Declarations used throughout the program.	        */
  dcl (F init("0"b), 
       T init("1"b)) bit(1) aligned int static options(constant);
  dcl  NL char(1) int static options(constant) init("
");
  dcl  PROC char(16) aligned int static options(constant) init("call_scalar_dcl_");
  dcl  ZEROb bit(1) int static options(constant) init("0"b);

  dcl  ignoreCode fixed bin(35);

  dcl  ioa_ entry() options(variable);
  dcl  ioa_$nnl entry() options(variable);
     
  dcl (call_et_$bad_declaration,
       call_et_$too_many_bounds,
       call_et_$too_many_descriptors ) fixed bin(35) ext static;

  dcl ( addr, addrel, before, binary, collate, codeptr, dimension, divide, 
       length, ltrim, null, reverse, rtrim, search, string, substr, unspec, verify) builtin;
  dcl (cleanup, conversion) condition;

%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Initialize output parameters, for worst possible failure case.			        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     variableName = "";
     starSizeName = "";
     descriptorB = ZEROb;
     code = 0;


  dcl  dP ptr;					/* ptr to next unparsed char in dclString.	        */
  dcl  dL fixed bin(21);				/* length of unparsed chars in dclString.	        */
  dcl  d (dL) char(1) based(dP);			/* character array overlay for dclString.	        */

     dP = addr(dclString);
     dL = length(dclString);


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Parse the dclString into tokens.						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */


  dcl  breakCharsNotTokenized char(5) varying init(" """ || substr(collate(),10,4));
						/* SP HT NL VT NP				        */
  dcl  breakChars char(10) varying init("(),:;");

     breakChars = breakChars || breakCharsNotTokenized;	/* PL/I does not permit this via init statements.         */

  dcl  dclStringCharsToIgnore fixed bin(21) int static options(constant) init(0);
  dcl  lexControlChars char(128) varying int static;
  dcl  lexDelims char(128) varying int static;
  dcl  lexInitialized bit(1) aligned int static init("0"b);
  dcl  lexSwitchesUNSET bit(0) int static options(constant) init(""b);
  dcl (commentOpenUNUSED, commentClosedUNUSED) char(0) int static options(constant) init("");
  dcl (quoteOpenUNUSED, quoteClosedUNUSED) char(0) int static options(constant) init("");
  dcl  statementDelim char(1) int static options(constant) init(";");
  dcl (statementP, tokenFirstP) ptr;

  dcl  lex_string_$init_lex_delims entry (char(*), char(*), char(*), char(*), char(*), bit(*), char(*) var, char(*) var,
	char(*) var, char(*) var);
  dcl  lex_string_$lex entry (ptr, fixed bin(21), fixed bin(21), ptr, bit(*), char(*), char(*), char(*), char(*),
	char(*), char(*) var, char(*) var, char(*) var, char(*) var, ptr, ptr, fixed bin(35));

     if ^lexInitialized then do;
	call lex_string_$init_lex_delims(
	     quoteOpenUNUSED, quoteClosedUNUSED, commentOpenUNUSED, commentClosedUNUSED,
	     statementDelim, lexSwitchesUNSET, breakChars, breakCharsNotTokenized,
	     lexDelims, lexControlChars);
	lexInitialized = T;
	end;

     statementP, tokenFirstP = null();			/* start out with no input tokens.   		        */
     call lex_string_$lex(
	addr(dclString), length(dclString), dclStringCharsToIgnore, 
	areaP, lexSwitchesUNSET,
	quoteOpenUNUSED, quoteClosedUNUSED, commentOpenUNUSED, commentClosedUNUSED,
	statementDelim, breakChars, breakCharsNotTokenized, lexDelims, lexControlChars,
	statementP, tokenFirstP, code);
     if code ^= 0 then do;
	go to EXIT_from_declaration;
	end;


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* For testing purposes, display the tokens found by lex_string_.			        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     if debugI >= 4 then do;
	call ioa_$nnl("^/Lexed tokens from declaration: ------------------------------^/ ");
	Ptoken = tokenFirstP;
	do while (Ptoken ^= null());
	     Ptoken = displayToken();
	     end;
	call ioa_("^/");
	end;	

%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Do analysis of dclString tokens, using the reductions defined above.		        */
	/*  1) Declare variables set or referenced by the reductions.			        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

  dcl  parenDepth fixed bin;				/* Used by the skipCl reduction subroutine to skip over   */
						/*  a parenthesized clause that we don't want to parse.   */

  dcl  levelNotSet    fixed bin(35) int static options(constant) init(-1);
  dcl 1 struct,					/* Used by start_structure, continue_structure, 	        */
      2 startingLevel fixed bin(35) init(levelNotSet),	/*  end_structure, and no_structure_yet action routines.  */
      2 currentLevel  fixed bin(35) init(levelNotSet),
      2 components    fixed bin init(0);

  dcl 1 t like type;				/* Structure describing PL/I data type attributes.  These */
						/*  t.XXX bits are set in the reductions to keep track    */
						/*  of type attributes seen.  These are used in creating  */
						/*  the output descriptorB value.		        */

  dcl  descI fixed bin;				/* Index of current <descriptor> data in desc array.      */
  dcl  descN fixed bin;				/* Number of <descriptor>s found in the parse.	        */
  dcl 1 desc (1) aligned,				/* Array whose elements describe first/last tokens in a   */
      2 cl like clause;				/*  <descriptor> (all attributes of a declaration).       */
						/*  See Action Subroutine start_clause (far below) for    */
						/*   information about the clause structure.	        */
						/*  For call_scalar_dcl_, only one descriptor declaration */
						/*   can be parsed at a time; dimension set to 1.	        */

  dcl  boundI fixed bin;				/* Index of current <bound> data in bound array.	        */
  dcl  boundN fixed bin;				/* Number of <bound>s found in the parse.	        */
  dcl  boundUnset char(12) varying int static options(constant) init("");
  dcl 1 bound (128) aligned,
      2 (low, high) char(12) varying;
  dcl 1 bounds_clause aligned like clause;
  dcl  bounds_as_string char(40) var;

  dcl  precUnset char(8) varying int static options(constant) init("");
  dcl  precNUnset fixed bin(24) int static options(constant) init(0);
  dcl  scaleUnset char(8) varying int static options(constant) init("");
  dcl  scaleNUnset fixed bin(24) int static options(constant) init(0);
  dcl 1 precision aligned,				/* Precision data found for arithmetic declaration.       */
      2 (prec, scale) char(8) varying,
      2 (precN, scaleN) fixed bin(24);

  dcl  sizeUnset char(8) varying int static options(constant) init("");
  dcl  sizeNStar fixed bin(24) int static options(constant) init(16777215); /* = "77777777"b3 */
  dcl  sizeNUnset fixed bin(24) int static options(constant) init(0);
  dcl 1 sizeAttr aligned,
      2 size char(8) varying,
      2 sizeN fixed bin(24);

  dcl 1 parm aligned like clause;			/* Used by begin_descriptor and end_descriptor action     */
						/*  routines to record tokens corresponding to our        */
						/*  output descriptor.			        */

     descN = 0;					/* No <descriptor> found before parse; desc array empty.  */
     clause.first_tokenP, clause.last_tokenP = null();	/* No subclause found before parsing begins.	        */
     bounds_clause = clause;
     precision.prec = precUnset;
     precision.precN = precNUnset;
     precision.scale = scaleUnset;
     precision.scaleN = scaleNUnset;
     sizeAttr.size = sizeUnset;
     sizeAttr.sizeN = sizeNUnset;

     Pthis_token = tokenFirstP;			/* Begin analysis with first token found by lex_string_   */
     TRACING = (debugI >= 4);				/* Trace matching reductions, only at extreme debug level.*/
     call SEMANTIC_ANALYSIS();			/* Use reductions to parse the tokens.		        */
     if code ^= 0 then go to EXIT_from_declaration;	/*  Exit if found an error while parsing.	        */

     if (boundN > 0) & (bounds_clause.first_tokenP ^= null())  then do;
	call clauseFromTokens(bounds_clause);
	bounds_as_string = tkn_clause;
	bounds_as_string = ltrim(bounds_as_string, "(");
	end;
     else bounds_as_string = "*";

     if debugI >= 3 then do;
	call ioa_ ("^/Attributes found in declaration: ------------------------------");
	call ioa_ (" dcl ^a;", 
	     type_as_string (string(t), 1, (variableName), bounds_as_string, 
			 precision.precN, precision.scaleN, sizeAttr.sizeN, ""));
	end;

  dcl  consistent bit(1) aligned;
  dcl  t_complete bit(36) aligned;			/* Structure describing PL/I data type attributes.  These */
						/*  t.XXX bits are set in the reductions to keep track    */
						/*  of type attributes seen.  These are used in creating  */
						/*  the output descriptorB value.		        */

     consistent = type_for_descriptor_set (string(t), precision.precN, t_complete, precision.precN);
						/* type_for_descriptor also applies PL/I defaults for any */
						/*  missing attributes.			        */

     string(t) = t_complete;
     if consistent then do;				/* If declaration contains a consistent set of attributes,*/
						/*  enforce precision, area size, & string length limits. */
	if debugI >= 3 then do;
	     call ioa_ ("^/Consistent attribute set: ------------------------------");
	     call ioa_ (" dcl ^a;^/", 
		type_as_string (string(t), 1, (variableName), bounds_as_string, 
			      precision.precN, precision.scaleN, sizeAttr.sizeN, ""));
	     end;

	if t.structure then   call ErrT (no_structures, t, (variableName), bounds_as_string, precision, sizeAttr);
	if t.dimensioned then call ErrT (no_arrays,     t, (variableName), bounds_as_string, precision, sizeAttr);

	if (t.fixed & t.binary) & (precision.precN > max_p_fix_bin_2) then
	     call ErrLimit (fix_bin_prec_high, precision.prec, max_p_fix_bin_2);
	if (t.float & t.binary) & (precision.precN > max_p_flt_bin_2) then
	     call ErrLimit (flt_bin_prec_high, precision.prec, max_p_flt_bin_2);
	if (t.decimal) & (precision.precN > max_p_dec) then
	     call ErrLimit (dec_prec_high, precision.prec, max_p_dec);
	if (t.area) & (sizeAttr.sizeN < min_area_size) then
	     call ErrLimit (area_size_low, sizeAttr.size, min_area_size);
	if (t.area) & (sizeAttr.sizeN ^= sizeNStar) & (sizeAttr.sizeN > max_area_size) then
	     call ErrLimit (area_size_high, sizeAttr.size, max_area_size);
	if (t.bit) & (sizeAttr.sizeN ^= sizeNStar) & (sizeAttr.sizeN > max_bit_string) then
	     call ErrLimit (bit_long, sizeAttr.size, max_bit_string);
	if (t.char) & (sizeAttr.sizeN ^= sizeNStar) & (sizeAttr.sizeN > max_char_string) then
	     call ErrLimit (char_long, sizeAttr.size, max_char_string);
	end;
     else call ErrT (inconsistent_attrs, t, (variableName), bounds_as_string, precision, sizeAttr);

     if consistent & (code = 0) then do;		/* attributes consistent; precision/scale, size, length OK*/
	arg_descriptor_ptr = addr(descriptorB);		/*  Fill in descriptor.			        */
	arg_descriptor.flag = T;			/* New-style descriptor.			        */
	arg_descriptor.type = pl1_descriptor_type (string(t), precision.precN);
	arg_descriptor.packed = t.unaligned;
	arg_descriptor.number_dims = 0;

	if t.fixed then do;
	     fixed_arg_descriptor.precision = precision.precN;
	     fixed_arg_descriptor.scale = precision.scaleN;
	     end;
	else if t.float then 
	     arg_descriptor.size = precision.precN;
	else if (t.bit | t.char | t.area) then
	     arg_descriptor.size = sizeAttr.sizeN;
	end;

EXIT_from_declaration:
     return;
%page;

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Reduction syntax functions, and action subroutines.				        */
	/*									        */
	/* See discussion of the reductions command (in MPM Commands manual) for descriptions of	        */
	/* reductions at the start of this source, and how they make use of syntax functions, action      */
	/* subroutines, and action statements (statements shown directly in the reduction statements).    */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

begin_descriptor:					/* Action Subroutine: record start of a <descriptor>      */
     proc;					/*  clause by location of its first token.	        */
     
     unspec(t) = ""b;				/* No data type attributes given so far.	        */
     parm.first_tokenP = Pthis_token;			/* Only beginning of <parameter-descriptor> seen so far.  */
     parm.last_tokenP = null();

     boundN = 0;					/* No <dimension attribute> as yet for the <descriptor>   */

     precision.prec = precUnset;			/* No <precision> as yet for the <descriptor>             */
     precision.precN = precNUnset;
     precision.scale = scaleUnset;
     precision.scaleN = scaleNUnset;
     sizeAttr.size = sizeUnset;                             /* No string or area size, as yet.		        */
     sizeAttr.sizeN = sizeNUnset;

     end begin_descriptor;

end_descriptor:					/* Action Subroutine: record entry of a <descriptor>      */
     proc;					/*  clause by location of its last token.  Location of    */
						/*  the entire <descriptor> clause is then stored as a    */
						/*  new element in the desc array; descN is incremented.  */

     parm.last_tokenP = Pthis_token->token.Plast;		/* Token preceding the ending , or ) is last token of     */
						/*  <descriptor>.				        */

     if descN >= dimension(desc,1) then do;		/* Check for desc array out-of-bounds.		        */
	code = call_et_$too_many_descriptors;
	go to EXIT_from_declaration;
	end;

     descN = descN + 1;				/* Access new array element for this <descriptor>	        */
     desc(descN).cl = parm;				/* Store info therein about the <descriptor>	        */

     end end_descriptor;
%page;

end_structure:					/* Action Subroutine: Initialize (or complete use of)     */
no_structure_yet:					/*  <structure-descriptor> tracking data.	        */
     proc;
     struct.startingLevel = levelNotSet;
     struct.currentLevel = levelNotSet;
     struct.components = 0;
     end no_structure_yet;

start_structure:					/* Action Subroutine: Begin tracking a new 	        */
     proc;					/*  <structure-descriptor> by recording its starting      */
     struct.startingLevel = token.Nvalue;		/*  structure <level> number.  (Structure declarations    */
     struct.currentLevel = levelNotSet;			/*  may start at values higher than 1.)		        */
     struct.components = 1;
     end start_structure;

continue_structure:					/* Action Subroutine: Continue tracking 	        */
     proc;					/*  <structure-descriptor> being parsed by adding info    */
     struct.currentLevel = token.Nvalue;		/*  for an addition structure element.  This addition     */
     struct.components = struct.components + 1;		/*  only tracks the current element's <level> value,      */
     end continue_structure;				/*  which is higher than the starting <level> for this    */
						/*  <structure-descriptor>, but may be lower, equal to,   */
						/*  or higher than <level> of the preceding element.      */

level_starts_struct:				/* Syntax Function: returns true if token is a positive   */
     proc returns(bit(1) aligned);			/*  integer, and either:			        */
						/*   - preceding parm was not a <structure-descriptor>, or*/
						/*   - this token <level> <= starting level of the        */
						/*     preceding parm's structure.		        */
  dcl  do_higher_level bit(1) aligned init(T);

     do_higher_level = F;
     
level_for_item_in_struct:				/* Syntax Function: returns true if token is a positive   */
     entry returns(bit(1) aligned);			/*  integer, and currently parsing <structure-descriptor> */
						/*  and this <level> is higher than the current 	        */
						/*  structure's starting <level>.		        */

     if ^intPos() then return (F);			/* intPos can attempt the conversion to a binary number.  */
						/*  When it succeeds, token.Nvalue contains the converted */
  dcl  level fixed bin(35);				/*  value.				        */
     level = token.Nvalue;				

     if do_higher_level then do;
	if (struct.startingLevel = levelNotSet) then return (F);
						/* Cannot be higher structure level, if not dcl a struct. */
	if level > struct.startingLevel then		/* Is level higher than start of our current structure?   */
	     return (T);				/*  This ends the current structure, and starts a new one.*/
	end;
     else do;
	if (struct.startingLevel = levelNotSet) then return (T);
						/* Not currently in a structure dcl?  This starts one.    */
	if level <= struct.startingLevel then		/* Is level not higher than start of our current struct?  */
	     return (T);				/*  This ends the current structure, and starts a new one.*/
	end;

     return (F);
	
     end level_starts_struct;
%page;

  dcl 1 clause aligned,
      2 (first_tokenP, last_tokenP) ptr;

start_clause:					/* Action Subroutine: remember first token within a       */
     proc;					/*  subclause of the <descriptor>.		        */

     if clause.first_tokenP ^= null() then 
	return;					/* We've already started a clause		        */

     if Pthis_token ^= null() then do;
	if Pthis_token -> token.Plast ^= null() then	/* Try to start clause one token before current token.    */
	     clause.first_tokenP = Pthis_token->token.Plast;
	else clause.first_tokenP = Pthis_token;		/*   Most clauses are attached to a preceding attribute   */	
	clause.last_tokenP = clause.first_tokenP;	/*   which should be included in any error message.       */
	end;
     else call clear_clause();
     
     end start_clause;

continue_clause:					/* Action Subroutine: remember subsequent contiguous      */
     proc;					/*  tokens found within the subclause being captured.     */
     

     if clause.first_tokenP = null() then 
	return;					/* No clause has been started as yet.		        */
     
     if Pthis_token ^= null() then
	clause.last_tokenP = Pthis_token;
     
     end continue_clause;


clear_clause:					/* Action Subroutine: complete use of the captured        */
     proc;					/*  subclause.				        */

     clause.first_tokenP, clause.last_tokenP = null();
     
     end clear_clause;


  dcl  tkn_clauseP ptr;
  dcl  tkn_clauseL fixed bin(21);
  dcl  tkn_clause char(tkn_clauseL) based(tkn_clauseP);

clauseFromTokens:					/* Overlay string denoted by tokens within given clause.  */
     proc(cl);

  dcl 1 cl aligned like clause;
  dcl (l1, l2) fixed bin(24);
  dcl  P ptr;
  dcl  charno builtin;

     tkn_clauseL = 0;				/* Assume clause is empty.			        */
     if cl.first_tokenP ^= null() then do;
	tkn_clauseP = cl.first_tokenP -> token.Pvalue;	/*  - Clause starts at string denoted by first token.     */

	if last_tokenP = cl.first_tokenP then
	     tkn_clauseL = cl.first_tokenP->token.Lvalue; /*  - Only one token in clause.  Easy overlay.	        */
	     
	else if cl.last_tokenP ^= null() then do;	/*  - Clause continues with sequential tokens.	        */
	     P = cl.last_tokenP->token.Pvalue;		/*    Length is: charno(last_token start)	        */
	     l1 = charno(P);			/*             - charno(first_token start)	        */
	     l2 = charno(tkn_clauseP);		/*	     + length(last_token_value)	        */
	     tkn_clauseL = l1 - l2 + cl.last_tokenP->token.Lvalue;
	     end;
	end;
     else tkn_clauseP = addr(dclString);

     end clauseFromTokens;


%page;

new_dim:						/* Action Subroutine: initialize a new array dimension in */
     proc;					/*   the dimension(...) clause of <parameter-descriptor>, */
						/*   including (...) without dimension keyword at start   */
						/*   of the <parameter-descriptor>.		        */
						/*   Note: begin_descriptor sets boundN=0 each time       */
						/*         another <parameter-descriptor> starts.	        */

     if boundN >= dimension(bound,1) then do;		/* Check for dimension out-of-bounds for the storage we   */
	code = call_et_$too_many_bounds;		/*  have allocated to track such bounds. 	        */
	go to EXIT_from_declaration;
	end;

     boundN = boundN + 1;				/* Increment current bound pair; mark low/high as unset.  */
     bound(boundN).low = boundUnset;
     bound(boundN).high = boundUnset;
     
     end new_dim;

set_bound:					/* Action Subroutine: set bound pair, based on value of   */
     proc;					/*  current token.				        */

     if length(token_value) <= maxlength(bound(boundN).high) then do;
	if token_value = "*" then do;
	     bound(boundN).low = boundUnset;
	     bound(boundN).high = token_value;
	     end;
	else do;
	     bound(boundN).low = "1";
	     bound(boundN).high = token_value;
	     end;
	end;
     end set_bound;

set_lbound:					/* Action Subroutine: Set lbound of bound pair, based on  */
     proc;					/*  value of current token.			        */

     if length(token_value) <= maxlength(bound(boundN).low) then do;
	bound(boundN).low = token_value;
	end;
     end set_lbound;

set_hbound:					/* Action Subroutine: Set hbound of bound pair, based on  */
     proc;					/*  value of current token.			        */

     if length(token_value) <= maxlength(bound(boundN).high) then do;
	bound(boundN).high = token_value;
	end;
     end set_hbound;

set_bounds_clause:
     proc;

     call continue_clause();
     bounds_clause = clause;
     call clear_clause();

     end;
%page;

set_prec:						/* Action Subroutine: Set precision for a 	        */
     proc;					/*  <precision-attribute> of an arithmetic <descriptor>   */
						/*  to current token value.  Set scale element to unset.  */
     

     if length(token_value) > maxlength(precision.prec) then do;
	precision.prec = precUnset;
	precision.precN = precNUnset;
	end;
     else do;
	precision.prec = token_value;
	precision.precN = token.Nvalue;
	end;
     precision.scale = scaleUnset;
     precision.scaleN = scaleNUnset;

     end set_prec;

set_scale:					/* Action Subroutine: Set scale for a		        */
     proc;					/*  <precision-attribute> of an arithmetic <descriptor>   */
						/*  to current token value.			        */

     if length(token_value) > maxlength(precision.scale) then do;
	precision.scale = scaleUnset;
	precision.scaleN = scaleNUnset;
	end;
     else do;
	precision.scale = token_value;
	precision.scaleN = token.Nvalue;
	if      (precision.scaleN < min_scale) then call ErrTokenLimit(scale_value_low,  min_scale);
	else if (precision.scaleN > max_scale) then call ErrTokenLimit(scale_value_high, max_scale);
	end;

     end set_scale;

set_size:						/* Action Subroutine: Set size for a bit, char, or area   */
     proc;					/*  <descriptor>.				        */

     if length(token_value) > maxlength(sizeAttr.size) then do;
	sizeAttr.size = sizeUnset;
	sizeAttr.sizeN = sizeNUnset;
	end;
     else if token_value = "*" then do;
	sizeAttr.size = token_value;
	sizeAttr.sizeN = sizeNStar;
	end;
     else do;
	sizeAttr.size = token_value;
	sizeAttr.sizeN = token.Nvalue;
	end;

     end set_size;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Several syntax functions...						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

balanced_rParen:					/* Syntax Function: returns true if token is 	        */
     proc returns(bit(1) aligned);			/*  right-parenthesis (")"), and parenDepth=1 for tokens  */
						/*  parsed so far in current parenthesize clause.	        */
						/*  Note: parenDepth is initialized, incremented, and     */
						/*        decremented by action statements in reductions. */
     if token_value = ")" & parenDepth = 1 then return (T);
     return (F);

     end balanced_rParen;


int: proc returns(bit(1) aligned);			/* Syntax Function: returns true if token is a	        */
						/*  positive, negative or zero integer.		        */
						/*  Note: a side-effect of all three syntax functions in  */
						/*        this procedure is that value for a token that   */
						/*        converts OK to an integer is stored in	        */
						/*        token.Nvalue for later reuse.		        */
  dcl  my_int fixed bin(35);				
  dcl  must_be_positive bit(1) aligned;
  dcl  must_be_non_negative bit(1) aligned;

  dcl (binary, length, substr, verify) builtin;
  dcl (conversion, size) condition;


     must_be_positive = F;
     must_be_non_negative = F;
     go to COMMON;

intPos:						/* Syntax Function: returns true if token is a positive   */
     entry returns(bit(1) aligned);			/*  integer.				        */
     
     must_be_positive = T;
     must_be_non_negative = F;
     go to COMMON;

intNNeg:						/* Syntax Function: returns true if token is a	        */
     entry returns(bit(1) aligned);			/*  non-negative integer.			        */

     must_be_positive = F;
     must_be_non_negative = T;
     go to COMMON;

COMMON:
     if token.Nvalue ^= 0 then my_int = token.Nvalue;	/* If converted this token in past, use saved numeric val.*/
     else do;
	if length(token_value) = 0 then go to intEmpty;
	if verify(token_value, "+-0123456789") > 0 then go to intFirstBad;
	if length(token_value) >= 2 then
	     if verify(substr(token_value,2), "0123456789") > 0 then go to intRestBad;

	on conversion begin;
	     go to intConversion;
	     end;
	on size begin;
	     go to intSize;
	     end;
	my_int = binary(token_value, 35, 0);
	token.Nvalue = my_int;
	end;

     if must_be_positive     & token.Nvalue < 1 then go to intPositive;
     if must_be_non_negative & token.Nvalue < 0 then go to intNonNegative;

intTrue:
     return (T);

intConversion:
     return (F);
intEmpty:
     return (F);     
intFirstBad:
     return (F);          
intNonNegative:
     return (F);
intPositive:
     return (F);
intRestBad:
     return (F);
intSize:
     return (F);

     end int;


varName:						/* Syntax Function: returns true if token meets 	        */
     proc returns(bit(1) aligned);			/*  specifications for PL/I <identifier>.	        */

  dcl  token_value_first char(1) defined(token_value);
	
  dcl  IDENTIFIER char(64) int static options(constant) init("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$");
  dcl  IDENTIFIERfirst char(52) int static options(constant) init("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");

     if length(token_value) < 1 then return(F);
     if length(token_value) > maxlength(variableName) then return(F);
     if (verify(token_value_first, IDENTIFIERfirst) ^= 0) | (verify(token_value, IDENTIFIER) ^= 0) then return(F);
     return(T);

starNameRef:					/* Syntax Function: returns true if token meets specs of  */
     entry returns(bit(1) aligned);			/*  PL/I <identifier> and fits in starSizeName parameter. */

     if length(token_value) < 1 then return(F);
     if length(token_value) > maxlength(starSizeName) then return(F);
     if (verify(token_value_first, IDENTIFIERfirst) ^= 0) | (verify(token_value, IDENTIFIER) ^= 0) then return(F);
     return(T);

     end varName;

set_varName:					/* Action Subroutine: sets variableName to token_value    */
     proc;
     variableName = token_value;
     end set_varName;

set_nr:
     proc;
     
     starSizeName = token_value;
     sizeAttr.size = "*";
     sizeAttr.sizeN = sizeNStar;
     end set_nr;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Action Subroutines generating simplified error messages.				        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

    /* ERROR NAME */
  dcl (empty_string           init(1),
       no_declare_key         init(2),
       bad_varname            init(3),
       incomplete_dcl         init(4),
       multi_returns_attrs    init(5),
       unbalanced_parentheses init(6),
       expected_semicolon     init(7),
       missing_semicolon	init(8),
       unknown_attribute	init(9),
       bad_array_bound	init(10),
       bad_precision	init(11),
       bad_size		init(12),
       inconsistent_attrs	init(13),
       no_structures          init(14),
       no_arrays		init(15),
       scale_value_low    	init(16),
       scale_value_high    	init(17),
       fix_bin_prec_high      init(18),
       flt_bin_prec_high      init(19),
       dec_prec_high          init(20),
       area_size_low	init(21),
       area_size_high	init(22),
       bit_long		init(23),
       char_long		init(24),
       no_pictures            init(25)
			      ) fixed bin int static options(constant);

Err: proc (error_no);				/* Action Subroutine: emit a brief explanation of an      */
						/*  error encountered while parsing the entry declaration.*/
						/*  The message may (or may not) include the current      */
						/*  token value.				        */

  dcl error_no fixed bin;				/*  PARM: one of the ERROR NAME values from list above.   */
     
						/* Message corresponding to ERROR NAME constants.	        */
  dcl msg (25) char(100) var int static options(constant) init(
     "Declaration string is empty",
     "<declare-key> must appear first; string begins: ^a",
     "Invalid variable identifier format: ^a",
     "Declaration ends prematurely.",
     "More than one attribute in returns clause.",
     "Parenthesized clause ends prematurely: ^a",
     "Expected semicolon (;), but found: ^a",
     "Expected semicolon (;) missing.",
     "Unknown attribute: ^a",
     "Bad array bounds clause: ^a",			/* ErrCl					        */
     "Bad precision clause: ^a",			/* ErrCl					        */
     "Bad size clause: ^a",				/* ErrCl					        */
     "Inconsistent attribute set: ^a",			/* ErrT					        */
     "Structure declarations are not supported: ^a",	/* ErrT					        */
     "Array declarations are not supported: ^a",		/* ErrT					        */
     "Scale value (^a)  <  min_scale (^d)",		/* ErrTokenLimit				        */
     "Scale value (^a)  >  max_scale (^d)",		/* ErrTokenLimit				        */
     "Precision (^a)  >  max_fixed_binary_precision (^d)",  /* ErrLimit				        */
     "Precision (^a)  >  max_float_binary_precision (^d)",  /* ErrLimit				        */
     "Precision (^a)  >  max_decimal_precision (^d)",	/* ErrLimit				        */
     "Area size (^d)  <  min_area_size (^d)",		/* ErrLimit				        */
     "Area size (^d)  >  max_area_size (^d)",		/* ErrLimit				        */
     "Bit string length (^d)  >  max_bit_length (^d)",	/* ErrLimit				        */
     "Character string length (^d)  >  max_char_length (^d)",    
						/* ErrLimit				        */
     "Picture data is not supported: ^a"
     );
     
  dcl include_token bit(1) aligned;			/* =T means include token_value; =F means don't include   */
     include_token = (index(msg(error_no), "^a") ^= 0);

SIMPLE_Err:
     if debugI = 0 then;
     else if include_token & Ptoken ^= null() then
	call ioa_("^/ERROR ^d: " || msg(error_no), error_no, token_value);
     else call ioa_("^/ERROR ^d: " || msg(error_no), error_no, "");

     code = call_et_$bad_declaration; 
     return;

ErrCl:						/* Action Subroutine: emit a brief explanation of an      */
     entry (error_no);				/*  error encountered while parsing a clause within one   */
						/*  of the <descriptor>s of the entry declaration.        */
						/*  The clause is built by the start_clause and 	        */
						/*  continue_clause action subroutines, and may be        */
						/*  included in the explanation.		        */
     call clauseFromTokens(clause);
     if length(tkn_clause) = 0 then do;
	include_token = F;
	go to SIMPLE_Err;
	end;
     if debugI = 0 then;
     else call ioa_("^/ERROR ^d: " || msg(error_no), error_no, tkn_clause);
     return;
     
ErrLimit:
     entry (error_no, value_given, limit);

  dcl value_given char(*) var;

     if debugI = 0 then;
     else call ioa_("^/ERROR ^d: " || msg(error_no), error_no, value_given, limit);
     code = call_et_$bad_declaration; 
     return;

ErrTokenLimit:
     entry (error_no, limit);

  dcl limit fixed bin(31);

     if debugI = 0 then;
     else call ioa_("^/ERROR ^d: " || msg(error_no), error_no, token_value, limit);
     code = call_et_$bad_declaration; 
     return;

ErrT:
     entry (error_no, t, n, b, p, s);
  dcl 1 t like type;
  dcl  n char(*);
  dcl  b char(40) var;
  dcl 1 p like precision aligned;
  dcl 1 s like sizeAttr aligned;

     if debugI = 0 then;
     else call ioa_("^/ERROR ^d: " || msg(error_no), error_no, 
	type_as_string(string(t), 0, n, b, p.precN, p.scaleN, s.sizeN, ""));
     code = call_et_$bad_declaration; 
     return;

     end Err;

%page;

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Debugging Utilities							        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

displayToken:					/* Display a token returned by lex_string_	        */
     proc() returns(ptr);

  dcl  ioa_$nnl entry() options(variable);
     
     call ioa_$nnl ("^a ", token_value);

     return (token.Pnext);

     end displayToken;
%page;
%include arg_descriptor;
%page;
%include pl1_descriptor_type_fcn;
%page;
%include pl1_symbol_type;
%page;
%include pl1_symbol_type_fcns;
%page;
%include system;
