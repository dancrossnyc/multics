/* HISTORY COMMENTS:
  1) change(2016-03-18,GDixon), approve(2016-10-13,MCR10014),
     audit(2016-10-13,Swenson), install(2016-10-13,MR12.6f-0002):
     Initial implementation of call_entry_info_.rd.
                                                   END HISTORY COMMENTS */



%page;
/*++

BEGIN	/ declare				/ LEX(1)					         / epName	 \
	/ dcl    				/ LEX(1)					         / epName	 \
	/ <any-token>			/ Err(no_declare_key)			         / RETURN	 \
	/ <no-token>			/ Err(empty_string) [code=call_et_$bad_declaration]        / RETURN    \

epName	/ <entryName>			/ set_entryName LEX(1)			         / entryCl   \
	/ <any-token>			/ Err(bad_entryname)			         / RETURN    \
	/ <no-token>			/ Err(incomplete_dcl)			         / RETURN    \
	
entryCl	/ entry (				/ LEX(1) [phase=PhEntryParms]  PUSH(returnsCl)             / parm	 \
					  \" Before calling the parm reduction subroutine, set the
					  \"  current phase of parsing to EntryParms (looking for
					  \"  one or more <parameter-descriptor> definitions 
					  \"  inside the entry(...) clause.  
					  \" The parm reduction subroutine ends by transferring to 
					  \"  the PUSHed "returnsCl" label (immediately below here).
          / entry				/ LEX(1)					         / returnsCl \
	/ <any-token>                           / Err(no_entry_attr)			         / RETURN	 \
	/ <no-token>			/ Err(incomplete_dcl)			         / RETURN    \

returnsCl / returns (			/ LEX(1) [phase=PhReturnsParm] PUSH(endDcl)	         / parm      \
					  \" Before calling the parm reduction subroutine, set the
					  \"  current phase of parsing to ReturnsParm (looking for
					  \"  a single <return-descriptor> in the returns(...) 
					  \"  clause). 
					  \" The parm reduction subroutine ends by transferring to 
					  \"  the PUSHed "endDcl" label (immediately below here).
          / options ( variable )		/ LEX(4) [ei.options_variable=T]		         /           \
					  \" Either returns(...) or options(...) may appear on an
					  \"  entry statement, but not both on the same statement.
					  \"  The only token that may follow options is semi-colon.

endDcl    / reducible			/ LEX    [t.reducible=T]			         / endDcl    \
          / irreducible			/ LEX    [t.irreducible=T]			         / endDcl    \
          / ;				/					         / RETURN    \
          / <any-token>			/ Err(expected_semicolon)			         / RETURN	 \
	/ <no-token>			/ Err(missing_semicolon)			         / RETURN    \

					  \" SUBROUTINE: parm, a reduction subroutine that parses
					  \"  the <parameter-list> of an entry(...) attribute; or
					  \"  the <return-parameter> of a returns(...) attribute.
					  \"  Each pass thru parm subroutine finds all attributes
					  \"  forming a single data <descriptor>.
					  \" Inputs:
					  \"  Pthis_token -> left-paren that starts the clause.
					  \"  phase: must be set by caller, to control parsing of
					  \"    an entry <parameter-list> or <returns-descriptor>.
parm      / (				/ LEX(1) begin_descriptor  no_structure_yet                /           \
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
	/ <comma_validInPhase>		/ end_descriptor           LEX(1) begin_descriptor         / parmBegin \
					  \" Valid comma appears: <parameter-set> , <parameter-set> 
					  \"  It completes one parameter descriptor, and starts
					  \"  the next descriptor.
          / ,				/ Err(multi_returns_attrs)              	         / RETURN    \
					  \" EXIT SUBROUTINE WITH ERROR: multiple <parameter-set>s
					  \" A between-parameter comma (,) is not permitted inside
					  \"  a returns clause.  <returns-parameter> is only one 
					  \"  <parameter-set>.

	/ )				/ end_descriptor           LEX(1)                          / STACK_POP \
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

          / precision                             / LEX(1) [t.precision=T]   PUSH(attrs)		         / prec	 \
	/ prec                                  / LEX(1) [t.precision=T]   PUSH(attrs)		         / prec	 \

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
bounds    / (				/ LEX(1)            new_dim           start_clause(d.bCl)  / newDim    \
	/				/					         / STACK_POP \
					  \" RETURN FROM THE SUBROUTINE: no bounds given

newDim    / <int> : <int> ,                       / set_lbound LEX(2) set_hbound LEX(2) cont_clause(d.bCl)
							new_dim			         / newDim	 \
          / <int> : * ,                           / set_lbound LEX(2) set_hbound LEX(2) cont_clause(d.bCl)
							new_dim			         / newDim	 \
	/ <int> ,	          		/ set_bound  LEX(2)	new_dim	        cont_clause(d.bCl)   / newDim	 \
          / * ,                                   / set_bound  LEX(2) new_dim           cont_clause(d.bCl)   / newDim    \
                                                                      
					  \" Another dimension in the bounds subclause
	/ <int> : <int> )   		/ set_lbound LEX(2) set_hbound cont_clause(d.bCl) LEX(2)   / STACK_POP \
	/ <int> : * )       		/ set_lbound LEX(2) set_hbound cont_clause(d.bCl) LEX(2)   / STACK_POP \
	/ <int> ) 			/ set_bound		 cont_clause(d.bCl) LEX(2)   / STACK_POP \
          / * )                                   / set_bound		 cont_clause(d.bCl) LEX(2)   / STACK_POP \
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
	/ (                                     /	             PUSH(precErr) start_clause(clause)  / skipCl    \
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
	/ (     *     )			/ LEX(1) set_size LEX(2)			         / STACK_POP \
	/ (                                     /	             PUSH(sizeErr) start_clause(clause)  / skipCl    \
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
					  \"  when parsing the dString.
					  \" Inputs:
					  \"  Pthis_token -> left-paren that starts the subclause.
skipCl    / (				/ LEX(1) [parenDepth=1]            start_clause(clause)    /           \
					  \" Initialize skipCl subr by: LEXing the starting 
					  \" left-paren; setting parenDepth of 1; and noting loc
					  \" of starting right-paren, in case of unbalanced parens.
rParenBal	/ (				/ LEX(1) [parenDepth=parenDepth+1] cont_clause(clause)     / rParenBal \
	/ <balanced_rParen>			/ LEX(1)			     clear_clause(clause)    / STACK_POP \
					  \" RETURN FROM SUBROUTINE:
					  \"  Found balancing right-paren. 
	/ )				/ LEX(1) [parenDepth=parenDepth-1] cont_clause(clause)     / rParenBal \
	/ <any-token>			/ LEX(1)			     cont_clause(clause)     / rParenBal \
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
	/* PL/I Language data attribute defaults:					        */
	/* 		from PL/I Language Specification Manual, AG94-02, RevE 03/81, page 5-13 ff    */
	/*									        */
	/* 5.3.3 Language Default Rules						        */
	/*									        */
	/* Entry Defaults								        */
	/*									        */
	/*       default (returns|reducible|irreducible|options) entry;			        */
	/*       default (entry&^reducible) irreducible;					        */
	/*									        */
	/* File Default								        */
	/*									        */
	/*      default(input|output|update|stream|record|print|keyed|direct|			        */
	/* 	 sequential|environment) file;					        */
	/*									        */
	/* Arithmetic Defaults							        */
	/*									        */
	/*      default(^(character|bit|pointer|offset|area|label|format|entry|file|		        */
	/* 	 fixed|float|picture|binary|decimal|real|complex|				        */
	/* 	 builtin|generic|condition|constant)) fixed binary real;			        */
	/*      default((real|complex)&^(picture|float|constant)) fixed;			        */
	/*      default((binary|decimal)&^(float|constant)) fixed;				        */
	/*      default((fixed|float)&^(complex|constant)) real;				        */
	/*      default((fixed|float)&^(decimal|constant)) binary;				        */
	/*      default(fixed&binary&^precision&^constant) precision(17,0);			        */
	/*      default(fixed&decimal&^precision&^constant) precision(7,0);			        */
	/*      default(float&binary&^precision&^constant) precision(27);			        */
	/*      default(float&decimal&^precision&^constant) precision(10);			        */
	/*									        */
	/* String Default								        */
	/*									        */
	/*      default((character|bit)&^(varying|constant)) nonvarying;			        */
	/*									        */
	/* Scope and Storage Class Defaults						        */
	/*									        */
	/*      default((entry|file)&(automatic|based|static|parameter|			        */
	/* 	 defined|controlled|member|aligned|unaligned|				        */
	/* 	 initial) variable;							        */
	/*      default((entry|file)&range(*)&^variable) constant;				        */
	/*      default(^(constant|builtin|generic|condition)&range(*))			        */
	/*         variable;							        */
	/*      default((file|entry)&range(*)&constant&^internal) external;			        */
	/*      default(condition) external;						        */
	/*      default(^external&range(*)) internal;					        */
	/*      default(variable&external&^controlled) static;				        */
	/*      default(variable&^(based|controlled|static|defined|parameter|			        */
	/*         member)) automatic;						        */
	/*									        */
	/* Storage Mapping Defaults							        */
	/*									        */
	/*      default((character|bit|picture|structure)&^(aligned|constant))		        */
	/* 	 unaligned;							        */
	/*      default(^(constant|builtin|generic|unaligned}) aligned;			        */
	/*      default ((fixed|float)&^unsigned) signed;					        */
	/*									        */
	/* Example:								        */
	/* 	declare i fixed;							        */
	/* 	declare j float;							        */
	/* 	declare a;							        */
	/* 	declare X external;							        */
	/* 	declare E entry returns(fixed);					        */
	/*									        */
	/*									        */
	/* After application of the language defaults, these declarations are:		        */
	/*									        */
	/*       declare i fixed binary real precision(17,0)				        */
	/*       	      aligned variable automatic internal signed;				        */
	/*       declare j float binary real precision(27)				        */
	/*       	      aligned variable automatiautomatic internal signed;			        */
	/*       declare X fixed binary real precision(17,0)				        */
	/*       	      aligned variable static external signed;				        */
	/*       declare E entry constant external irreducible				        */
	/*       	      returns(fixed binary real precision(17,0) aligned signed);		        */
	/*									        */
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
call_entry_info_:
     proc ();
						/* Declarations used throughout the program.	        */
  dcl  ioa_ entry() options(variable);
  dcl  ioa_$nnl entry() options(variable);
  dcl  translator_temp_$allocate entry (ptr, fixed bin) returns(ptr);

  dcl (F init("0"b), T init("1"b)) bit(1) aligned int static options(constant);
  dcl  NL char(1) int static options(constant) init("
");
  dcl  PROC char(16) aligned int static options(constant) init("call_entry_info_");
  dcl  ZEROb init(""b) bit(0) aligned int static options(constant);

  dcl 1 ei aligned like entry_info_header;		/* Place to put header info until we allocate full struct.*/

  dcl  cleanup_dataP ptr;
  dcl 1 cd aligned based(cleanup_dataP),		/* Cleanup data, pointed to by entry_info structure.      */
						/*  It is private to this routine, therefore not visible  */
						/*  to callers.				        */
      2 pl1_files,					/* Temporary file names used to invoke PL/I compiler to   */
        3 in_use bit(1) aligned,			/*   generate descriptors from a declaration string.      */
						/*   If in_use set, cleanup removes these files.	        */
        3 prefix char(10) var,			/*    . names begin with caller-provided prefix.          */
        3 unique char(14),				/*    . unique string appended to prefix in constructing  */
						/*      temporary file names.	                            */
        3 wdir char(168) unal,			/*    . working dir in which PL/I files were created.     */
        3 (objFilename, pl1Filename, listFilename) char(32) unal,
						/*    . PL/I object, source and listing entrynames.       */
        3 foPathname char(168) unal;			/*    . file_output capturing compiler results/errors.    */

  dcl (call_et_$bad_declaration,
       call_et_$too_many_bounds,
       call_et_$too_many_descriptors,
       error_table_$noalloc,
       error_table_$noarg,
       error_table_$nodescr ) fixed bin(35) ext static;

  dcl ( addr, addrel, before, binary, character, collate, codeptr, dimension, divide, 
       length, ltrim, null, reverse, rtrim, search, size, substr, unspec, verify) builtin;
  dcl (cleanup, conversion) condition;

%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Subroutine:  call_entry_info_$from_virtual_entry				        */
	/*									        */
	/* Function:								        */
	/*  - Convert virtual_entry to entry variable.					        */
	/*  - Locate entrypoint calling sequence data.					        */
	/*  - Return basic information about this calling sequence.				        */
	/*									        */
	/* Syntax:								        */
	/*   entry_info_ptr = null();							        */
	/*   on cleanup call call_entry_info_$cleanup (entry_info_ptr);			        */
	/*   call call_entry_info_$from_virtual_entry					        */
	/*        (CALLER_NAME, virtual_entry, debugI, entry_info_ptr, code);			        */
	/*									        */
	/*     ... [caller's code that uses entry_info data]				        */
	/*									        */
	/*   call call_entry_info_$cleanup (entry_info_ptr);				        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

from_virtual_entry:
     entry (caller, virtual_entry, debugI, entry_info_ptr, code);

  dcl  caller char(*);				/* Name of calling command.  This is used in naming  (in) */
						/*  temporary segments created by call_entry_info_.       */
  dcl  virtual_entry char (*);			/* String representation of an entrypoint.  Any form      */
						/*  acceptable to cv_entry_ is permitted here.       (in) */
  dcl  debugI fixed bin(3) unsigned;			/* Display debugging information to the user.        (in) */
						/*   =0-2: no debugging       =3: basic debug info        */
/*                                                               [declared in call_entry_info_.incl.pl1 ]
  dcl  entry_info_ptr ptr;				/* Pointer to our returned data, which is an        (out) */
						/*  entry_info structure.			        */
  dcl  code fixed bin(35);				/* An error (non-fatal) occurred accessing          (out) */
						/*  virtual_entry, or its descriptors.		        */

     entry_info_ptr = null;				/* Initialize output parameters.		        */
     code = 0;

  dcl  areaP ptr;					/* Area setup by translator_temp_.  Used by lex_string_   */
						/*  to hold token/comment/statement descriptors, and by   */
     areaP = null;					/*  call_entry_info_ to hold variable-length output.      */
     on cleanup call area_janitor(areaP);		/* Release temp segments if cleanup occurs before we      */
						/*  return entry_info_ptr to caller.		        */

     call setup_ei(caller, ei, areaP, cleanup_dataP, code); /* Initialize entry_info_header.  Do other steps common   */
     if code ^= 0 then go to EXIT_from_cei;		/*  to all call_entry_info_ entrypoints.	        */


  dcl cv_entry_ entry (char (*), ptr, fixed bin(35)) returns (entry);
						/* Convert virtual_entry to an entry variable.	        */
     ei.entrypoint.entryVar = cv_entry_ (virtual_entry, null(), code);
     if code ^= 0 then do;
          call gripe (code, PROC, "Converting ^a to an entry variable", virtual_entry);
          go to EXIT_from_cei;
	end;
     ei.entrypoint.nameString = reverse (before (reverse (virtual_entry), ">")); 
%page;

  dcl  get_entry_arg_descs_$info entry (ptr, fixed bin, (*) ptr, ptr, fixed bin(35));
  dcl  descsIgnored (1) ptr;				/* Get pointer to entrypoint code sequence.	        */
  dcl  entryP ptr;

     entryP = codeptr(ei.entryVar);			/* First call to get_entry_arg_descs_ gets parm_count.    */
     call get_entry_arg_descs_$info( entryP, entry_info_parm_count, descsIgnored, addr(edi), code);
     if code = 0 | code = error_table_$nodescr then do;
	ei.parm_count = entry_info_parm_count;
	ei.function = edi.function;
	ei.options_variable = edi.variable;
	code = 0;					/* Suppress any error_table_$nodescr errors.	        */

	if debugI >= 4 then				/* If debugging, show user what get_entry_arg_descs_ got. */
	     call display_entry_sequence(entryP);
	end;
     else do;					/* Some other error (no access, ring bracket violation)   */
	call gripe(code, PROC, "Call to get_entry_arg_descs_$info failed.");
	go to EXIT_from_cei;
	end;

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* At this point, allocate and return the full entry_info structure with descriptor_ptrs.	        */
	/* Size of this full structure depends on the ei.parm_count value obtained above.	        */
	/* Caller must cleanup entry_info structure, after entry_info_ptr parameter has been set.	        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     entry_info_ptr =  translator_temp_$allocate (areaP, size(entry_info));
     if entry_info_ptr = null then do; code = error_table_$noalloc; go to EXIT_from_cei; end;
     entry_info.cleanup_data_ptr = cleanup_dataP;
     revert cleanup;				/* Caller responsible for entry_info structure cleanup.   */

     entry_info.header = ei;				/* Copy data gathered above into new entry_info structure.*/

  dcl  ignoreCode fixed bin(35);			/* Second call to get_entry_arg_descs_ gets descriptors.  */
  dcl  ignoreParmCount fixed bin;			/* Args set during first call to get_entry_arg_descs_.    */
     call get_entry_arg_descs_$info (entryP, ignoreParmCount, entry_info.descriptor_ptrs, addr(edi), ignoreCode);

     if debugI >= 3 then				/* If debugging, show user what's being returned.	        */
	call display_descriptors (entryP, entry_info_ptr);
     return;
%page;
EXIT_from_cei:					/* When fatal error is reported, we do the cleanup.       */
     entry_info_ptr = null;
     call area_janitor(areaP);
     return;

area_janitor:					/* Release temp segs, and descriptors stored therein.     */
     proc (aP);

  dcl  aP ptr;
  dcl  translator_temp_$release_all_segments entry (ptr, fixed bin(35));

     if aP ^= null then call translator_temp_$release_all_segments (aP, ignoreCode);
     end area_janitor;


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Subroutine:  call_entry_info_$cleanup					        */
	/*									        */
	/* Function:  This is the cleanup handler for call_entry_info_$from_XXX entrypoints.	        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

cleanup:
     entry(entry_info_ptr);

     if entry_info_ptr = null() then return;		/* If no data returned to caller, do nothing.	        */

     areaP = entry_info.areaP;
     cleanup_dataP = entry_info.cleanup_data_ptr;		/* Get pointers from entry_info to cleanup items.	        */

     if cd.pl1_files.in_use then do;			/* Remove any files created when calling PL/I compiler.   */
	call deleteFile(cd.wdir, cd.pl1Filename);
	call deleteFile(cd.wdir, cd.objFilename);
	call deleteFile(cd.wdir, cd.listFilename);
	call deleteFile("", cd.foPathname);
	end;

     call area_janitor(areaP);			/* Get rid of temp segments, which hold both the cd and   */
     entry_info_ptr = null();				/*  entry_info structures.  Null caller's entry_info_ptr. */

     return;					/* This completes the cleanup operation.	        */
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Subroutine:  call_entry_info_$from_declaration					        */
	/*									        */
	/* Function:  Converts a string containing a PL/I declare statement for an entrypoint into an     */
	/* actual PL/I calling sequence with parameter descriptors.  This conversion is carried out by    */
	/* parsing the PL/I declare statement into components.  From <parameter-descriptor> components,   */
	/* argument descriptors are constructed.  Pointers to these are descriptors are returned in the   */
	/* entry_info structure.							        */
	/*									        */
	/* Syntax:								        */
	/*   entry_info_ptr = null();							        */
	/*   on cleanup call call_entry_info_$cleanup (entry_info_ptr);			        */
	/*   call call_entry_info_$from_declaration					        */
	/*        (CALLER_NAME, virtual_entry, declarationString, debugI, entry_info_ptr, code);	        */
	/*									        */
	/*     ... [caller's code that uses entry_info data]				        */
	/*									        */
	/*   call call_entry_info_$cleanup (entry_info_ptr);				        */
	/*									        */
	/* Note:  An alternate method was once used to obtain descriptors, using the pl1 compiler.  With  */
	/* a simpler version of reductions (than that shown above), some entrypoint declarations were so  */
	/* complex that they could not be fully interpreted by the reductions.  The reductions could      */
	/* split an entrypoint declaration into <parameter-set> clauses, but could not construct a        */
	/* complete attribute set from each parameter, by applying default attributes, etc.  The older    */
	/* method stored the <parameter-descriptor> text found by the reductions in a small PL/I program  */
	/* file (constructed and compiled on-the-fly).  The compiler would then apply defaulting rules    */
	/* to construct a complete attribute set for each <parameter-descriptor>, and generate a small    */
	/* PL/I object file.  The entrypoint calling sequence in this small object file would thereby     */
	/* describe each parameter.  Parameter descriptors would be obtained via		        */
	/* call_entry_info_$from_virtual_entry.  However, later code now produces a complete attribute    */
	/* set from information parsed by the reductions.  The alternate method code is retained to       */
	/* verify that the newer code works properly.  If debugI = 5, both new and alternate methods are  */
	/* used to produce descriptors; and descriptors from the two methods are compared.  After	        */
	/* checking more than 500 entrypoint declarations in >sss>pl1.dcl file, the new method was        */
	/* proven correct; the alternate method is therefore not used on a regular basis.	        */
	/*									        */
	/* call_entry_info_$cleanup removes temporary files created by the alternate method, such as      */
	/*   .pl1  containing the on-the-fly PL/I program describing entrypoint parameters;               */
	/*   .list containing compiler results, retained for debugging purposes if PL/I reports an error; */
	/*   and the object containing actual descriptors.				        */
	/* Temp segments for the PL/I compiler method are created in the working directory, to permit the */
	/* user to examine the generated PL/I, and any compiler error statements which may be produced.   */
	/* All temporary segments have names beginning with the callerPrefix string.		        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

from_declaration:
     entry (caller, virtual_entry, dStringVar, debugI, entry_info_ptr, code);

  dcl  dStringVar char(*) varying;			/* String holding an entrypoint declaration.         (in) */

  dcl 1 dStr aligned based(addr(dStringVar)),		/* Overlay for dStringVar, to access the char(*) portion. */
      2 dStrLen fixed bin(35),
      2 dString char(dStr.dStrLen);

     entry_info_ptr = null;				/* Initialize output parameters.		        */
     code = 0;

     areaP = null;					/* Temporary segments to hold storage returned to caller. */
     on cleanup call area_janitor(areaP);		/* Release temp segments if cleanup occurs before we      */
						/*  return to caller.			        */

     call setup_ei(caller, ei, areaP, cleanup_dataP, code); /* Initialize entry_info_header.  Do other steps common   */
     if code ^= 0 then go to EXIT_from_cei;		/*  to all call_entry_info_ entrypoints.	        */

     ei.entrypoint.nameString = reverse (before (reverse (virtual_entry), ">")); 
     ei.entrypoint.entryVar = cv_entry_ (virtual_entry, null(), ignoreCode);
						/* Try to convert virtual_entry to an entry variable.     */
						/*  Error is ignored, because caller may just want us to  */
						/*  build descriptors for a ficticious entrypoint.        */
  dcl  cu_$make_entry_value entry (ptr, entry);
     if ignoreCode ^= 0 then call cu_$make_entry_value(null(), ei.entrypoint.entryVar);
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Parse the dStringVar into tokens.						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

  dcl  breakCharsNotTokenized char(5) varying init(" " || substr(collate(),10,4));
						/* SP HT NL VT NP				        */
  dcl  breakChars char(10) varying init("(),:;");

     breakChars = breakChars || breakCharsNotTokenized;	/* PL/I does not permit this via init statements.         */

  dcl  dStringCharsToIgnore fixed bin(21) int static options(constant) init(0);
  dcl  lexControlChars char(128) varying int static;
  dcl  lexDelims char(128) varying int static;
  dcl  lexInitialized bit(1) aligned int static init("0"b);
  dcl  lexSwitchesUNSET bit(0) int static options(constant) init(""b);
  dcl (quoteOpenUNUSED, quoteClosedUNUSED, 
       commentOpenUNUSED, commentClosedUNUSED) char(0) int static options(constant) init("");
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
	addr(dString), length(dString), dStringCharsToIgnore, 
	areaP, lexSwitchesUNSET,
	quoteOpenUNUSED, quoteClosedUNUSED, commentOpenUNUSED, commentClosedUNUSED,
	statementDelim, breakChars, breakCharsNotTokenized, lexDelims, lexControlChars,
	statementP, tokenFirstP, code);
     if code ^= 0 then do;
	go to EXIT_from_cei;
	end;


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* For testing purposes, display the tokens found by lex_string_.			        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     if debugI >= 4 then do;
	call ioa_("^/Lexed tokens from declaration: ----------------------------------");
	Ptoken = tokenFirstP;
	do while (Ptoken ^= null());
	     Ptoken = displayToken();
	     end;
	call ioa_("");
	end;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Do analysis of dString tokens, using the reductions defined above.			        */
	/*  - Declare variables that apply to the entire declaration.			        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

  dcl  parenDepth fixed bin;				/* Used by the skipCl reduction subroutine to skip over   */
						/*  a parenthesized clause that we don't want to parse.   */

  dcl  phase fixed bin init(PhUnset);			/* phase of parsing.  comma_validInPhase uses this        */
						/*  setting to determine whether a comma is a valid       */
						/*  as a separator between parms in this parse phase.     */
  dcl (PhUnset       init(-1),			/* Values permitted for phase:		        */
       PhEntryParms  init(1),
       PhReturnsParm init(2)) fixed bin int static options(constant);

  dcl  optionsVariable bit(1) aligned init(F);
						

	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/*  - Declare variables that describe a single <parameter-descriptor>.		        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

  dcl 1 clause aligned,				/* A set of tokens related to the parameter.    	        */
      2 (first_tokenP, last_tokenP) ptr;		/*  See Action Subroutine start_clause (far below) for    */
						/*   information about the clause structure.	        */

  dcl 1 bound (max_number_of_dimensions) aligned,
      2 (low, high) char(12) varying;
  dcl  boundI fixed bin;				/* Index of current <bound> data in bound array.	        */
  dcl  boundN fixed bin;				/* Number of <bound>s found in the parse.	        */
  dcl  boundUnset char(12) varying int static options(constant) init("");

  dcl 1 precision aligned,				/* Precision data found for arithmetic declaration.       */
      2 (prec, scale) char(8) varying,
      2 (precN, scaleN) fixed bin(24);
  dcl  precUnset char(8) varying int static options(constant) init("");
  dcl  precNUnset fixed bin(24) int static options(constant) init(0);
  dcl  scaleUnset char(8) varying int static options(constant) init("");
  dcl  scaleNUnset fixed bin(24) int static options(constant) init(0);

  dcl 1 sizeAttr aligned,				/* Size data for string and area declaration.	        */
      2 size char(8) varying,
      2 sizeN fixed bin(24);
  dcl  sizeUnset char(8) varying int static options(constant) init("");
  dcl  sizeNStar fixed bin(24) int static options(constant) init(16777215); /* = "77777777"b3 */
  dcl  sizeNUnset fixed bin(24) int static options(constant) init(0);

  dcl 1 struct,					/* Structure parameter attribute information.	        */
      2 startingLevel fixed bin(35) init(levelNotSet),
      2 currentLevel  fixed bin(35) init(levelNotSet),
      2 components    fixed bin init(0);
  dcl  levelNotSet    fixed bin(35) int static options(constant) init(-1);

  dcl 1 t like type;				/* PL/I attributes.  These t.XXX bits are set	        */
						/*  in the reductions to keep track of type attributes.   */

%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Declare an array to hold parameter descriptor information for each parameter in the	        */
	/* declaration string.							        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

  dcl 1 desc (128) aligned,				/* Array whose elements describe a <parameter-descriptor> */
						/*  or <returns-descriptor>.			        */
      2 Cl like clause,				/*  - All tokens defining the descriptor.	        */
      2 bCl like clause,				/*  - All tokens defining array bounds for the descriptor.*/
      2 (precN, scaleN, sizeN) fixed bin(24),		/*  - precision/scale and string/area size for descriptor.*/
      2 isReturnsDescriptor bit(1),			/*  - T if this is a <returns-descriptor>.                */
      2 structLevel fixed bin,			/*  - Summary structure information.		        */
      2 tp like type unaligned;			/*  - Individual attributes defining the descriptor.      */
  dcl  descI fixed bin;				/* Index of current <descriptor> data in desc array.      */
  dcl  descN fixed bin;				/* Number of <descriptor>s found in the parse.	        */

  dcl 1 d like desc aligned based (dP);			/* Overlay for individual elements of the desc array.     */
  dcl  dP ptr;


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Semantically analyze the tokens, to extract <parameter-descriptor> information.	        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     descN = 0;					/* No <descriptor> found before parse; desc array empty.  */

     Pthis_token = tokenFirstP;			/* Begin analysis with first token found by lex_string_   */
     TRACING = (debugI >= 4);				/* Trace matching reductions, only at extreme debug level.*/
     call SEMANTIC_ANALYSIS();			/* Use reductions to parse the tokens.		        */
     if code ^= 0 then go to EXIT_from_cei;		/*  Exit if found an error while parsing.	        */

  dcl  dStringIsFunction fixed bin;			/* Determine if dString represents a function declaration.*/
						/* It's a numeric (rather than bit-string) value, so we   */
						/* can use it arithmetically.			        */
     if (descN > 0 & desc(descN).isReturnsDescriptor) then do;
	dStringIsFunction = 1;
	ei.callingSequence.function = T;
	end;
     else dStringIsFunction = 0;

     entry_info_parm_count, ei.parm_count = descN;	/* Store parm_count information in our return structure.  */


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Get storage for the array of descriptor pointers we return to the caller, and for the actual   */
	/* descriptors (which this entrypoint creates below).				        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     entry_info_ptr =  translator_temp_$allocate (areaP, size(entry_info));
     if entry_info_ptr = null then do; code = error_table_$noalloc; go to EXIT_from_cei; end;
     entry_info.cleanup_data_ptr = cleanup_dataP;
     revert cleanup;				/* Caller responsible for entry_info structure cleanup.   */

     entry_info.header = ei;				/* Copy data gathered above into new entry_info structure.*/


  dcl  descriptorsP ptr;				/* Allocate storage for the array of actual descriptors.  */
  dcl 1 descriptors (entry_info_parm_count) like arg_descriptor aligned based (descriptorsP);

     descriptorsP = translator_temp_$allocate (areaP, size(descriptors));
     if descriptorsP = null then do; code = error_table_$noalloc; go to EXIT_from_cei; end;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Build a scalar arg_descriptor for each <parameter-descriptor>			        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     do descI = lbound(descriptors,1) to hbound(descriptors,1);
	entry_info.descriptor_ptrs(descI) = addr(descriptors(descI));
	call create_descriptor (descI, desc(descI), addr(descriptors(descI)), debugI);
	end;
     if code ^= 0 then go to EXIT_from_cei;

     if debugI >= 3 then
	call display_descriptors (null(), entry_info_ptr);
     


	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Alternative Method:							        */
	/* Store parsed entrypoint components in a temp.pl1 segment, as procedure statement, entry        */
	/* statement (from the parsed data), separate parm dcl statements (from the parsed data); and an  */
	/* end statement.  Call PL/I compiler to process this temp.pl1 segment, producing a temp object   */
	/* segment with entrypoint descriptors.						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

     if (debugI >= 5) & (entry_info_parm_count > 0) then    /* For debugging purposes, try the alternative method,    */
ALTERNATE_METHOD:					/*  and compare descriptors between the two methods.      */
     do;						

  dcl  entrypointName char(65) var;
	call setup_pl1_files (caller, cd);
	call newWdirFileWithContents( cd.wdir, pl1Filename, entrypointStatementsInProc(entrypointName) );

  dcl  pl1CompileCommand char(400) var;
	pl1CompileCommand =				/* Store compiler output, error messages in .out file     */
	     "file_output " || rtrim(cd.foPathname) || "; " ||    
	     "syn_output user_output -ssw error_output; ";
	pl1CompileCommand =	pl1CompileCommand ||	/* Compile, generating symbol .list segment, with errors. */
	     "pl1 " || rtrim(pl1Filename) || " -symbols; " ||	
	     "revert_output -ssw error_output; revert_output";

  dcl  cu_$cp entry (ptr, fixed bin(21), fixed bin(35));	/* Call cu_$cp to run the command line.		        */
	call cu_$cp (addrel(addr(pl1CompileCommand), 1), length(pl1CompileCommand), ignoreCode);
     
  dcl  pl1_severity_ fixed bin(35) ext static;		/* Max error severity from last PL/I compilation attempt. */
	if pl1_severity_ ^= 0 | ^foundFile(cd.wdir, objFilename) then do;
						/* If object file was not generated, our compile failed.  */
	     call printFileWithMsg("", cd.foPathname,	/* Display cached error messages from compile.	        */
		"Compilation of entrypoint declaration failed, as follows:",
		"Compilation of entrypoint declaration failed, for unknown reasons.");
	     call deleteFile("", cd.foPathname);
	     call ioa_("^/  ---------------");
	
	     if foundFile(cd.wdir, listFilename) then do;	/* If compiler produced temp.list, we no longer need our  */
		call deleteFile(cd.wdir, pl1Filename);	/*  temp.pl1 at this point.			        */
		call ioa_("  For details, please see file: ^a>^a", cd.wdir, listFilename);
		end;
	     else call ioa_("  For details, please see file: ^a>^a", cd.wdir, pl1Filename);
						/* No temp.list?  Then .pl1 file is best we can offer.    */
	     return;
	     end;

	call deleteFile("", cd.foPathname);		/* In any case, we no longer need file_output file.       */
     

/* ---------------------------------------------------------------------- *
 * But... get a pointer to the entrypoint we just created, with its       *
 *  descriptors, from our temporary object segment.		    *
 * ---------------------------------------------------------------------- */

  dcl  my_entryP ptr;
  dcl  my_entryVar entry variable;
  dcl  my_virtual_entry char(256);
  dcl  my_parm_count fixed bin;

	my_virtual_entry = rtrim(cd.wdir) || ">" || rtrim(cd.objFilename) || "$" || rtrim(entrypointName);
	my_entryVar = cv_entry_( my_virtual_entry, null(), ignoreCode);
	if ignoreCode ^= 0 then do;
	     call gripe (ignoreCode, PROC, "Converting temp virtual_entry ^a to an entry variable", my_virtual_entry);
	     return;
	     end;
	my_entryP = codeptr(my_entryVar);

	call get_entry_arg_descs_$info( my_entryP, my_parm_count, descsIgnored, addr(edi), ignoreCode);
	if ignoreCode = error_table_$nodescr | ignoreCode = 0 then ;
	else do;
	     call gripe(ignoreCode, PROC, "Call to get_entry_arg_descs_$info failed.");
	     return;
	     end;

	if my_parm_count ^= entry_info.parm_count then do;
	     call ioa_ ("ERROR: ^a pl1 parms (^d) differ from declaration parms (^d)", 
		entry_info.nameString, my_parm_count, entry_info.parm_count);
	     call display_entry_sequence(my_entryP);
	     call deleteFile(cd.wdir, pl1Filename);	/* Delete all files we created, except the object file    */
	     call deleteFile(cd.wdir, listFilename);	/*  (our caller still needs its descriptors).	        */
	     return;
	     end;
	

  dcl  descriptorPP ptr;
  dcl  descriptorP (my_parm_count) ptr aligned based(descriptorPP);
  dcl  descP ptr;
  dcl  desc_bv bit(36) aligned based(descP);
	descriptorPP = translator_temp_$allocate (areaP, size(descriptorP));
	if descriptorPP = null then return;

	call get_entry_arg_descs_$info( my_entryP, my_parm_count, descriptorP, addr(edi), ignoreCode);

  dcl  heading char(200) var;				/* Compare descriptors from new and alternate methods.    */
	heading = "^/ERROR: ^a parms differ:";
	do descI = lbound(descriptorP,1) to hbound(descriptorP,1);
	     if descriptorP(descI)->desc_bv ^= entry_info.descriptor_ptrs(descI)->desc_bv then do;
		if length(heading) > 0 then do;
		     call ioa_ (heading, ei.nameString);
		     heading = "";
		     end;
		descP = descriptorP(descI);
		call ioa_(" PL/I desc(^3d) (^p) -> ^.3b  ^a", descI, descP, desc_bv, descriptorString(descP));
		descP = entry_info.descriptor_ptrs(descI);
		call ioa_("  dcl desc(^3d) (^p) -> ^.3b  ^a", descI, descP, desc_bv, descriptorString(descP));
		end;
	     end;
	call deleteFile(cd.wdir, pl1Filename);		/* Delete all files we created, except the object file    */
	call deleteFile(cd.wdir, listFilename);		/*  (our caller still needs its descriptors).	        */
	end ALTERNATE_METHOD;
     return;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* This is the setup routine called by each call_entry_info_ external entrypoint.	        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

setup_ei:
     proc (c, e, areaP, cleanup_dataP, code);

  dcl  c char(*);					/* name of our caller, used in creating temp segs.   (in) */
  dcl 1 e aligned like entry_info_header;		/* entry_info structure.	                      (out) */
  dcl  areaP ptr;					/* translator_temp_ area this routine creates.	  (out) */
  dcl  cleanup_dataP ptr;				/* space allocated for cd structure.                (out) */
  dcl  code fixed bin(35);				/* Status code, indicating failure of this setup.	  (out) */

     if c = "" then do;				/* Ensure caller name specified.		        */
	code = error_table_$noarg;
	call gripe(code, PROC, "caller parameter is empty.");
	return;
	end;

  dcl  translator_temp_$get_segment entry (char(*) aligned, ptr, fixed bin(35));

     call translator_temp_$get_segment( (c), areaP, code);	/* Get an allocation space.			        */
     if areaP = null() then do;
	call gripe(code, PROC, "Calling translator_temp_$get_segment");
	return;
	end;
     e.areaP = areaP;		

  dcl 1 cleanup_data aligned like cd based(cleanup_dataP);	/* Allocate storage for cleanup data structure.	        */

     cleanup_dataP = translator_temp_$allocate(areaP, size(cd));
     if cleanup_dataP = null then do; code = error_table_$noalloc; return; end;

     cleanup_data.pl1_files.in_use = F;			

						/* Initialize entry_info_header data.		        */
     e.version = entry_info_v1;			/* - Set version of entry_info structure.	        */
     e.entrypoint.nameString = "";			/* - Each entrypoint has its own way of getting value.    */
     unspec(e.entrypoint.entryVar) = ZEROb;		/* - Zero storage for entry variable		        */

     e.callingSequence.function = F;			/* - No information about entrypoint calling sequence.    */
     e.callingSequence.options_variable = F;
     e.callingSequence.pad1 = ZEROb;
     e.callingSequence.parm_count = eiParmCountNotDetermined;

     edi.version = entry_desc_info_version_2;		/*  - Initialize inputs to get_entry_arg_descs_	        */
     edi.object_ptr = null();
     edi.bit_count = 0;

     code = 0;					/* No errors during initialization of ei header.	        */

     end setup_ei;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Name:  create_descriptor							        */
	/*									        */
	/* Function:  Use information obtained from declaration string to manufacture an arg descriptor.  */
	/*									        */
	/* For each <parameter-descriptor>:						        */
	/*  1) Determine if given attributes are mutually consistent (don't conflict with one another).   */
	/*  2) Follow guidelines in PL/I default statements to supply missing attributes.  Each	        */
	/*     descriptor will then have a complete set of attributes.			        */
	/*  3) Check whether given precision/scale, and string/area size are within acceptable limits.    */
	/*  4) Generate errors for structure and array parameters, which call cannot support.	        */
	/*  5) Create a scalar argument descriptor representing attributes for each scalar parameter.     */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

create_descriptor:
     proc(i, d, arg_descriptor_ptr, debugI);

  dcl  i fixed bin;
  dcl 1 d aligned like desc;
  dcl  debugI fixed bin(3) unsigned;

  dcl  bounds_as_string char(40) var;
  dcl  variableName char(10) var;

     variableName = "parm" || ltrim(character(i));

     if d.bCl.first_tokenP ^= null()  then do;
	call clauseFromTokens(d.bCl);
	bounds_as_string = tkn_clause;
	bounds_as_string = ltrim(bounds_as_string, "(");
	end;
     else bounds_as_string = "*";

  dcl 1 o aligned like desc;
     o = d;					/* Preserve original desc settings.		        */

  dcl  consistent bit(1) aligned;
  dcl  t_complete bit(36) aligned;			/* Structure describing PL/I data type attributes.  These */
						/*  t.XXX bits are set in the reductions to keep track    */
						/*  of type attributes seen.  These are used in creating  */
						/*  the output descriptorB value.		        */

     consistent = type_for_descriptor_set (string(o.tp), o.precN, t_complete, d.precN);
						/* type_for_descriptor also applies PL/I defaults for any */
						/*  missing attributes.			        */

     string(d.tp) = t_complete;
     if consistent then do;				/* If declaration contains a consistent set of attributes,*/
						/*  enforce precision, area size, & string length limits. */
	if debugI >= 3 then do;
	     call ioa_ (" dcl ^25a  =>  dcl ^a;", 
		type_as_string (string(o.tp), o.structLevel, (variableName), bounds_as_string, 
			      o.precN, o.scaleN, o.sizeN, "") || ";",
		type_as_string (string(d.tp), 1, (variableName), bounds_as_string, 
			      d.precN, d.scaleN, d.sizeN, ""));
	     end;

	if d.tp.structure then   call ErrDesc (no_structures, d, variableName, bounds_as_string);
	if d.tp.dimensioned then call ErrDesc (no_arrays,     d, variableName, bounds_as_string);

	if (d.tp.fixed & d.tp.binary) & (d.precN > max_p_fix_bin_2) then
	     call ErrLimit (fix_bin_prec_high, d.precN, max_p_fix_bin_2);
	if (d.tp.float & d.tp.binary) & (d.precN > max_p_flt_bin_2) then
	     call ErrLimit (flt_bin_prec_high, d.precN, max_p_flt_bin_2);
	if (d.tp.decimal) & (d.precN > max_p_dec) then
	     call ErrLimit (dec_prec_high, d.precN, max_p_dec);
	if (d.tp.area) & (d.sizeN < min_area_size) then
	     call ErrLimit (area_size_low, d.sizeN, min_area_size);
	if (d.tp.area) & (d.sizeN ^= sizeNStar) & (d.sizeN > max_area_size) then
	     call ErrLimit (area_size_high, d.sizeN, max_area_size);
	if (d.tp.bit) & (d.sizeN ^= sizeNStar) & (d.sizeN > max_bit_string) then
	     call ErrLimit (bit_long, d.sizeN, max_bit_string);
	if (d.tp.char) & (d.sizeN ^= sizeNStar) & (d.sizeN > max_char_string) then
	     call ErrLimit (char_long, d.sizeN, max_char_string);
	end;
     else call ErrDesc (inconsistent_attrs, d, variableName, bounds_as_string);

     if consistent & (code = 0) then do;		/* attributes consistent; precision/scale, size, length OK*/
						/*  Fill in descriptor.			        */
	arg_descriptor.flag = T;			/* New-style descriptor.			        */
	arg_descriptor.type = pl1_descriptor_type (string(d.tp), d.precN);
	arg_descriptor.packed = d.tp.unaligned;
	arg_descriptor.number_dims = 0;

	if d.tp.fixed then do;
	     fixed_arg_descriptor.precision = d.precN;
	     fixed_arg_descriptor.scale = d.scaleN;
	     end;
	else if d.tp.float then 
	     arg_descriptor.size = d.precN;
	else if (d.tp.bit | d.tp.char | d.tp.area) then
	     arg_descriptor.size = d.sizeN;
	end;
     return;
%page;
%include arg_descriptor;
     
     end create_descriptor;
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

     if descN >= dimension(desc,1) then do;		/* Check for desc array out-of-bounds.		        */
	code = call_et_$too_many_descriptors;
	go to EXIT_from_cei;
	end;
     descN = descN + 1;				/* Access new array element for this <descriptor>	        */
     dP = addr(desc(descN));
     unspec(d) = ZEROb;				/* Initialize the new desc structure.		        */

						/* Initialize individual structures for descriptor data.  */
     boundN = 0;					/* No <dimension attribute> as yet for the <descriptor>   */
     call no_structure_yet();
     call clear_clause (clause);			/* No subclause found before parsing begins.	        */
     precision.prec = precUnset;
     precision.precN = precNUnset;
     precision.scale = scaleUnset;
     precision.scaleN = scaleNUnset;
     sizeAttr.size = sizeUnset;
     sizeAttr.sizeN = sizeNUnset;
     string(t) = ZEROb;				/* No data type attributes given so far.	        */

     d.Cl = clause;					/* Certain descriptor elements have non-zero initial val. */
     d.bCl = clause;
     d.structLevel = struct.startingLevel;
     d.precN = precNUnset;
     d.scaleN = scaleNUnset;
     d.sizeN = sizeNUnset;
     d.Cl.first_tokenP = Pthis_token;			/* Gather a list of tokens for this <descriptor>.	        */

     end begin_descriptor;

end_descriptor:					/* Action Subroutine: record completed <descriptor>       */
     proc;					/*  clause by location of its last token.  Location of    */
						/*  the entire <descriptor> clause is then stored as a    */
						/*  new element in the desc array; descN is incremented.  */

     if lookingBackAt3Tokens("entry", "(", ")") then	/* Detect empty <parameter-list> in entry declaration.    */
	descN = 0;
     else if lookingBackAt3Tokens("returns", "(", ")") then	/* Detect empty <returns-descriptor>.		        */
	descN = descN - 1;
     else d.Cl.last_tokenP = Pthis_token->token.Plast;	/* Token preceding the ending , or ) is last token of     */
						/*  <descriptor>.				        */
     d.structLevel = struct.startingLevel;
     if t.structure then				/* For structures, don't keep any data type attributes    */
	d.tp.structure = T;				/*  other than .structure			        */
     else do;
	d.precN = precision.precN;
	d.scaleN = precision.scaleN;
	d.sizeN = sizeAttr.sizeN;
	d.tp = t;
	end;
     d.isReturnsDescriptor = (phase = PhReturnsParm);

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
						/*  which is higher than the starting <level> for this    */
						/*  <structure-descriptor>, but may be lower, equal to,   */
						/*  or higher than <level> of the preceding element.      */
     end continue_structure;


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

start_clause:					/* Action Subroutine: remember first token within a       */
     proc(Cl);					/*  subclause of the <descriptor>.		        */

  dcl 1 Cl aligned like clause;

     if Cl.first_tokenP ^= null() then 
	return;					/* We've already started a clause		        */

     if Pthis_token ^= null() then do;
	if Pthis_token -> token.Plast ^= null() then	/* Try to start clause one token before current token.    */
	     Cl.first_tokenP = Pthis_token->token.Plast;
	else Cl.first_tokenP = Pthis_token;		/*   Most clauses are attached to a preceding attribute   */	
	Cl.last_tokenP = Cl.first_tokenP;		/*   which should be included in any error message.       */
	end;
     else call clear_clause(Cl);
     return;


cont_clause:					/* Action Subroutine: remember subsequent contiguous      */
     entry(Cl);					/*  tokens found within the subclause being captured.     */

     if Cl.first_tokenP = null() then 
	return;					/* No clause has been started as yet.		        */
     
     if Pthis_token ^= null() then
	Cl.last_tokenP = Pthis_token;
     return;


clear_clause:					/* Action Subroutine: complete use of the captured        */
     entry(Cl);					/*  subclause.				        */

     Cl.first_tokenP, Cl.last_tokenP = null();
     return;

     end start_clause;


  dcl  tkn_clauseP ptr;
  dcl  tkn_clauseL fixed bin(21);
  dcl  tkn_clause char(tkn_clauseL) based(tkn_clauseP);

clauseFromTokens:
     proc(Cl);

  dcl 1 Cl aligned like clause;
  dcl (l1, l2) fixed bin(24);
  dcl  P ptr;
  dcl  charno builtin;

     tkn_clauseL = 0;
     if Cl.first_tokenP ^= null() then do;
	tkn_clauseP = Cl.first_tokenP -> token.Pvalue;

	if last_tokenP = Cl.first_tokenP then
	     tkn_clauseL = Cl.first_tokenP->token.Lvalue;
	     
	else if Cl.last_tokenP ^= null() then do;
	     P = Cl.last_tokenP->token.Pvalue;
	     l1 = charno(P);
	     l2 = charno(tkn_clauseP);
	     tkn_clauseL = l1 - l2 + Cl.last_tokenP->token.Lvalue;
	     end;
	end;
     else tkn_clauseP = addr(dString);

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
	go to EXIT_from_cei;
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

comma_validInPhase:					/* Syntax Function: returns true only if token is a       */
     proc returns(bit(1) aligned);			/*  comma (,) character, and if phase = PhEntryParms      */
						/*  (that is, phase ^= PhReturnsParm)		        */
     
     if token_value ^= "," then return (F);
     if phase = PhReturnsParm then return (F);		/* returns(...) can have only one <returns-descriptor>    */
     return (T);					/* entry(..., ...) allows several <parameter-descriptor>s */
						/*  separated from one another by a comma.	        */
     end comma_validInPhase;


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


entryName:					/* Syntax Function: returns true if token meets 	        */
     proc returns(bit(1) aligned);			/*  specifications for a virtual_entry, as required by    */
						/*  the cv_entry_ subroutine.			        */

  dcl  cv_entry_ entry (char(*), ptr, fixed bin(35)) returns(entry);
  dcl (error_table_$bad_conversion,			/* These are the error codes cv_entry_ returns if it      */
       error_table_$bigarg,				/*  encounters an error with virtual_entry format.        */
       error_table_$entlong,				/*  Only these represent an ill-formatted entryName.      */
       error_table_$improper_data_format,		/*  Other errors may be encountered trying to convert a   */
       error_table_$out_of_bounds) fixed bin(35) ext static;/*  properly-formatted string to an actual entry var.     */
						/*  These will be ignored.			        */
  dcl  entryVar entry variable;
  dcl  code fixed bin(35);

     entryVar = cv_entry_(token_value, null(), code);
     if code = error_table_$bad_conversion | code = error_table_$bigarg | code = error_table_$entlong |
	code = error_table_$improper_data_format | code = error_table_$out_of_bounds then
	return (F);
     else return (T);

     end entryName;

set_entryName:					/* Action Subroutine: sets ei.entrypoint.namestring       */
     proc;					/*  to the token value.			        */
     ei.entrypoint.nameString = token_value;

     end set_entryName;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Action Subroutines generating simplified error messages.				        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

    /* ERROR NAME */
  dcl (empty_string           init(1),
       no_declare_key         init(2),
       bad_entryname          init(3),
       no_entry_attr          init(4),
       incomplete_dcl         init(5),
       multi_returns_attrs    init(6),
       unbalanced_parentheses init(7),
       expected_semicolon     init(8),
       missing_semicolon	init(9),
       unknown_attribute	init(10),
       bad_array_bound	init(11),
       bad_precision	init(12),
       bad_size		init(13),
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
       no_pictures            init(25),
       inconsistent_attrs	init(26)
			      ) fixed bin int static options(constant);

Err: proc (error_no);				/* Action Subroutine: emit a brief explanation of an      */
						/*  error encountered while parsing the entry declaration.*/
						/*  The message may (or may not) include the current      */
						/*  token value.				        */

  dcl error_no fixed bin;				/*  PARM: one of the ERROR NAME values from list above.   */
     
						/* Message corresponding to ERROR NAME constants.	        */
  dcl msg (26) char(100) var int static options(constant) init(
     "Entrypoint declare: string is empty",
     "<declare-key> must appear first; string begins: ^a",
     "Invalid entrypoint name format: ^a",
     "Expecting entry keyword; found: ^a",
     "Entrypoint declaration ends prematurely.",
     "More than one attribute in returns clause.",
     "Parenthesized clause ends prematurely: ^a",
     "Expected semicolon (;), but found: ^a",
     "Expected semicolon (;) missing.",
     "Unknown attribute: ^a",
     "Bad array bounds clause: ^a",			/* ErrCl					        */
     "Bad precision clause: ^a",			/* ErrCl					        */
     "Bad size clause: ^a",				/* ErrCl					        */
     "Structure declarations are not supported: ^a",	/* ErrDesc				        */
     "Array declarations are not supported: ^a",		/* ErrDesc				        */
     "Scale value (^a)  <  min_scale (^d)",		/* ErrTokenLimit				        */
     "Scale value (^a)  >  max_scale (^d)",		/* ErrTokenLimit				        */
     "Precision (^d)  >  max_fixed_binary_precision (^d)",  /* ErrLimit				        */
     "Precision (^d)  >  max_float_binary_precision (^d)",  /* ErrLimit				        */
     "Precision (^d)  >  max_decimal_precision (^d)",	/* ErrLimit				        */
     "Area size (^d)  <  min_area_size (^d)",		/* ErrLimit				        */
     "Area size (^d)  >  max_area_size (^d)",		/* ErrLimit				        */
     "Bit string length (^d)  >  max_bit_length (^d)",	/* ErrLimit				        */
     "Character string length (^d)  >  max_char_length (^d)",    
						/* ErrLimit				        */
     "Picture data is not supported: ^a",
     "Inconsistent attribute set: ^a"			/* ErrDesc				        */
     );
     
  dcl include_token bit(1) aligned;			/* =T means include token_value; =F means don't include   */
     include_token = (index(msg(error_no), "^a") ^= 0);

SIMPLE_Err:
     if debugI = 0 then;
     else if include_token & Ptoken ^= null() then
	call ioa_("ERROR ^d: " || msg(error_no), error_no, token_value);
     else call ioa_("ERROR ^d: " || msg(error_no), error_no, "");

     code = call_et_$bad_declaration; 
     return;

ErrCl:						/* Action Subroutine: emit a brief explanation of an      */
     entry (error_no);				/*  error encountered while parsing a clause within one   */
						/*  of the <descriptor>s of the entry declaration.        */
						/*  The clause is built by the start_clause and 	        */
						/*  cont_clause action subroutines, and may be	        */
						/*  included in the explanation.		        */
     call clauseFromTokens(clause);
     if length(tkn_clause) = 0 then do;
	include_token = F;
	go to SIMPLE_Err;
	end;
     if debugI = 0 then;
     else call ioa_("ERROR ^d: " || msg(error_no), error_no, tkn_clause);
     return;
     
ErrLimit:
     entry (error_no, value_given, limit);

  dcl value_given fixed bin(24);
  dcl limit fixed bin(31);

     if debugI = 0 then;
     else call ioa_("^/ERROR ^d: " || msg(error_no), error_no, value_given, limit);
     code = call_et_$bad_declaration; 
     return;

ErrTokenLimit:
     entry (error_no, limit);

     if debugI = 0 then;
     else call ioa_("^/ERROR ^d: " || msg(error_no), error_no, token_value, limit);
     code = call_et_$bad_declaration; 
     return;

ErrDesc:
     entry (error_no, d, n, b);
  dcl 1 d aligned like desc;
  dcl  n char(10)var;
  dcl  b char(40) var;

     if debugI = 0 then;
     else call ioa_("^/ERROR ^d: " || msg(error_no), error_no, 
	type_as_string(string(d.tp), d.structLevel, (n), b, d.precN, d.scaleN, d.sizeN, ""));
     code = call_et_$bad_declaration; 
     return;

     end Err;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Debugging Utilities							        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

display_descriptors:
     proc(entryP, eiP);

  dcl  entryP ptr;
  dcl  eiP ptr;

  dcl 1 ei aligned like entry_info_header based(eiP);

  dcl  descP ptr;
  dcl  descValue bit(36) aligned based(descP);
  dcl  i fixed bin;
     
     if entryP ^= null then call 
	ioa_("^/ Entry Sequence at:     ^p", entryP);
     call ioa_("  flags:");
     call ioa_("           function:  ^[true^;false^]", ei.function);
     call ioa_("   options variable:  ^[true^;false^]", ei.options_variable);
     call ioa_("             n_args:  ^d", ei.parm_count);

     do i = 1 to ei.parm_count;
	descP = eiP->entry_info.descriptor_ptrs(i);
	call ioa_("     desc(^3d) (^p) -> ^.3b  ^a",  
		i, descP, descValue, descriptorString(descP));
	end;
     call ioa_("");

     end display_descriptors;


display_entry_sequence:
     proc(entryP);

  dcl  entryP ptr;

  dcl (seqP, parmDescP, descValueP) ptr;
  dcl  descValue bit(36) aligned based(descValueP);
  dcl  i fixed bin;
     
     seqP = addrel(entryP, -2);
     call ioa_("^/ Entry Sequence at:     ^p", seqP);
     call ioa_("  flags:");
     call ioa_("      basic_indicator:  ^[true^;false^]", seqP->entry_sequence.word2.flags.basic_indicator);
     call ioa_("           revision_1:  ^[true^;false^]", seqP->entry_sequence.word2.flags.revision_1);
     call ioa_("      has_descriptors:  ^[true^;false^]", seqP->entry_sequence.word2.flags.has_descriptors);
     call ioa_("             variable:  ^[true^;false^]", seqP->entry_sequence.word2.flags.variable);
     call ioa_("             function:  ^[true^;false^]", seqP->entry_sequence.word2.flags.function);

     if seqP->entry_sequence.word2.flags.has_descriptors then do;
	parmDescP = pointer(entryP, seqP->entry_sequence.word1.descr_relp_offset);
	call ioa_("  descriptors:");
	call ioa_("          relp_offset:  ^.3b (^p)", seqP->entry_sequence.word1.descr_relp_offset, parmDescP);	
	call ioa_("               n_args:  ^d", parmDescP->parm_desc_ptrs.n_args);
	do i = 1 to parmDescP->parm_desc_ptrs.n_args;
	     descValueP = pointer(parmDescP, parmDescP->parm_desc_ptrs.descriptor_relp(i));
	     call ioa_("       desc(^3d).relp:  ^.3b (^p) = ^.3b  ^a",  
		i, parmDescP->parm_desc_ptrs.descriptor_relp(i), descValueP, descValue, descriptorString(descValueP));
	     end;
	call ioa_("");
	end;
     

     end display_entry_sequence;

displayToken:					/* Display a token returned by lex_string_	        */
     proc() returns(ptr);

  dcl  ioa_$nnl entry() options(variable);
     
     call ioa_$nnl ("^a ", token_value);

     return (token.Pnext);

     end displayToken;

lookingBackAt3Tokens:
     proc (v1, v2, v3) returns(bit(1) aligned);
     
  dcl (v1, v2, v3) char(*);

     if token_value ^= v3 then return(F);
     
  dcl  tokenV2 char(token.Plast->token.Lvalue) based(token.Plast->token.Pvalue);
     if tokenV2 ^= v2 then return(F);
     
  dcl  tokenV1 char(token.Plast->token.Plast->token.Lvalue) based(token.Plast->token.Plast->token.Pvalue);
     if tokenV1 ^= v1 then return(F);
     return (T);

     end lookingBackAt3Tokens;


gripe:						/* Routine to display status codes when debugging.        */
     proc options(variable);

  dcl  com_err_ entry() options(variable);
  dcl  cu_$arg_list_ptr entry (ptr);
  dcl  cu_$generate_call entry (entry, ptr);
  dcl  arg_listP ptr;

     if debugI >= 1 then do;
	call cu_$arg_list_ptr (arg_listP);
	call cu_$generate_call (com_err_, arg_listP);
	end;

     end gripe;
%page;
/* ------------------------------------------------------------------- *
 * Convert argument descriptor to a string of PL/I data attributes.	 *
 *   NB: bound_trace_stack_::get_pl1_parm_desc_string_  performs this  *
 *       same function, but is not externally callable.  It does a     *
 *       more complete job of displaying structure declarations, but   *
 *       call does not handle entrypoints with strucuture parameters.	 *
 * ------------------------------------------------------------------- */

descriptorString:
     proc (descP) returns (char (100) var);

  dcl  descP ptr aligned;

  dcl  desc bit(36) aligned based(descP);

  dcl  code fixed bin(35);
  dcl  ret char(2000) var;

  dcl  get_pl1_parm_desc_string_ entry (ptr, char(*) var, fixed bin(35));

     ret = "";
     call get_pl1_parm_desc_string_ (descP, ret, code);
     if code ^= 0 then
	call gripe (code, PROC, "Error converting descriptor to string: ^.3b", desc);
     if length(ret) > 100 then
	call gripe (code, PROC, "Long descriptor string shortened to 100 chars: ^a", ret);
     return (ret);
     
     end descriptorString;
%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* Routine to reassemble components of an entry declaration statement as:		        */
	/*  - entry statement, followed by:						        */
	/*  - parameter declaration statements						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

entrypointStatements:
     proc(epName) returns(char(10000) var);
     
  dcl  epName char(65) var;
  dcl  ret char(10000) var;

  dcl  NL bit(1) aligned int static options(constant) init(T);
  dcl  NNL bit(1) aligned int static options(constant) init(F);

  dcl  wrapper bit(1) aligned init(T);			/* entry selector switch.			        */

     wrapper = F;


entrypointStatementsInProc:
     entry(epName) returns(char(10000) var);

     ret = "";

						/* In the actual source file we generate, the entrypoint  */
						/*  being defined cannot have a $ in its name.  Such      */
						/*  would make it impossible for cv_entry_ to find the    */
     if wrapper then do;				/*  entrypoint.  call_temp_$hcs_$initiate_count	        */
	call add(ret, "^a_^a: procedure;", NL, cd.pl1_files.prefix, cd.pl1_files.unique);
          call add(ret, "", NL);
	if index(ei.entrypoint.nameString, "$") > 0 then
	     epName = after(ei.entrypoint.nameString,"$");
	else epName = ei.entrypoint.nameString;
	call add(ret, "^a:", NL, epName);
	end;
     else call add(ret, "^a:", NL, ei.entrypoint.nameString);    
						/* Emit entry statement.			        */
     if descN = 0 then do;
	if optionsVariable then do;
	     call add(ret, "       entry() options(variable);", NL);
	     end;
	else do;
	     call add(ret, "       entry();", NL);
	     end;
	end;
     else if descN = 1 then do;
	if dStringIsFunction = 1 then do;
	     call clauseFromTokens(desc(1).Cl);
	     call add(ret, "       entry() returns( ^a );", NL, tkn_clause);
	     end;
	else if optionsVariable then do;
	     call add(ret, "       entry() options( variable );", NL);
	     end;
	else call add(ret, "       entry( parm1 );", NL);
	end;
     else if descN >= 2 then do;
	call add(ret, "       entry( parm1", NNL);
	do descI = 2 to descN - dStringIsFunction;
	     call add(ret, ", parm^d", NNL, descI);
	     end;
	if dStringIsFunction = 1 then do;
	     call clauseFromTokens(desc(descN).Cl);
	     call add(ret, " ) returns( ^a );", NL, tkn_clause);
	     end;
	else if optionsVariable then do;
	     call add(ret, " ) options( variable );", NL);
	     end;
	else call add(ret, " );", NL);
	end;

     do descI = 1 to descN - dStringIsFunction;	/* Emit parameter dcl statements.		        */
	if desc(descI).structLevel = levelNotSet then do;
	     call clauseFromTokens(desc(descI).Cl);
	     call add(ret, "  dcl parm^d ^a;", NL, descI, tkn_clause);
	     end;
	else do;

  dcl (Sstate, Sitem) fixed bin;			/* Simple finite state machine to format structure tokens */
  dcl (Sdcl init(1), Stkn init(2), Slevel init(3), Slast init(4)) fixed bin int static options(constant);
	     Sstate = Sdcl;
	     Sitem = 1;
STRUCT_emit:   do Ptoken = desc(descI).Cl.first_tokenP repeat token.Pnext while(Ptoken ^= null);
		go to STRUCT(Sstate);
STRUCT(Sdcl):	call add(ret, "  dcl ^a parm^d", NNL, token_value, descI);		
		Sstate = Stkn;
		go to STRUCT_loop;
STRUCT(Stkn):	if token_value = "," then do;
		     call add(ret, "^a", NL, token_value);
		     Sstate = Slevel;
		     end;
		else call add(ret, " ^a", NNL, token_value);
		go to STRUCT_loop;
STRUCT(Slevel):     call add(ret, "      ^a item^d", NNL, token_value, Sitem);
		Sitem = Sitem + 1;
		Sstate = Stkn;
		go to STRUCT_loop;
STRUCT(Slast):      code = call_et_$bad_declaration;
		call gripe (code, PROC, "ERROR in structure declaration: ^a", tkn_clause);
		go to EXIT_from_cei;
STRUCT_loop:	if Ptoken = desc(descI).Cl.last_tokenP
		     then go to STRUCT_end;
		end STRUCT_emit;
STRUCT_end:    call add(ret, ";", NL);
	     end;
	end;

     if wrapper then 
	call add(ret, "^/     end ^a_^a;", NL, cd.pl1_files.prefix, cd.pl1_files.unique);

     return (ret);

add:      proc() options(variable);

  dcl  toP ptr;					/* ptr -> char(*) var			        */
  dcl  toML fixed bin(21);
  dcl  to char(toML) var based(toP);			/* Append new output to end of this string.	        */

  dcl  addCtlP ptr;					/* ptr -> char(*)				        */
  dcl  addCtlL fixed bin(21);
  dcl  addCtl char(addCtlL) based(addCtlP);		/* ioa_ control string.			        */
	
  dcl  nlSP ptr;					/* ptr -> bit(1) aligned			        */
  dcl  nlS bit(1) aligned based(nlSP);			/* T: Add a newline to end of new output.	        */

  dcl  arg_listP ptr;
  dcl  code fixed bin(35);
  dcl  gen_ret char(200);
  dcl  gen_retL fixed bin(21);
  dcl  ioa_ret char(gen_retL) based(addr(gen_ret));
  dcl  NoPad bit(1) aligned int static options(constant) init(F);

  dcl  cu_$arg_ptr entry (fixed bin, ptr, fixed bin(21), fixed bin(35));
  dcl  cu_$arg_list_ptr entry (ptr);
  dcl  ioa_$general_rs_control_string entry(ptr, char(*), fixed bin, char(*), fixed bin(21), bit(1) aligned,
	bit(1) aligned);
     
	call cu_$arg_ptr(1, toP, toML, code);
	toP = addrel(toP, -1);			/* When char(*) var is passed to options(variable) entry, */
						/*  the arg ptr points to first char, rather than to      */
						/*  length word of the char(*) varying routine.	        */
	call cu_$arg_ptr(2, addCtlP, addCtlL, code);
	call cu_$arg_ptr(3, nlSP, 0, code);

	call cu_$arg_list_ptr( arg_listP );

	call ioa_$general_rs_control_string (arg_listP, addCtl, 4, gen_ret, gen_retL, NoPad, nlS);
	to = to || ioa_ret;

	end add;

     end entrypointStatements;

%page;
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */
	/*									        */
	/* File manipulation routines.						        */
	/*									        */
	/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *   */

deleteFile:
     proc ( wdir, filename );

  dcl  wdir char(*);
  dcl  filename char(*);

  dcl  dir char(168);
  dcl  ent char(32);
  dcl  ignoreCode fixed bin(35);

  dcl  expand_pathname_ entry (char(*), char(*), char(*), fixed bin(35));
  dcl  hcs_$delentry_file entry (char(*), char(*), fixed bin(35));

     if wdir = "" then				/* Is filename a full pathname?		        */
	call expand_pathname_(filename, dir, ent, ignoreCode);
     else do; dir = wdir; ent = filename; end;		/* Else, filename is in working dir.		        */

     call hcs_$delentry_file(dir, ent, ignoreCode);	/* We tried to delete (cleanup) our temp files.	        */
						/*  Either it worked, or nothing we can do about it.      */
     return;
     

foundFile:
     entry ( wdir, filename ) returns(bit(1) aligned);

  dcl  bc fixed bin(24);
  dcl  type fixed bin(2);

  dcl  hcs_$status_minf entry (char(*), char(*), fixed bin(1), fixed bin(2), fixed bin(24), fixed bin(35));
     
     if wdir = "" then				/* Is filename a full pathname?		        */
	call expand_pathname_(filename, dir, ent, ignoreCode);
     else do; dir = wdir; ent = filename; end;		/* Else, filename is in working dir.		        */

     call hcs_$status_minf( dir, ent, 1, type, bc, code);
     return (code = 0 & type = 1);
     

printFileWithMsg:
     entry ( wdir, filename, successMsg, failMsg );
     
  dcl  successMsg char(*);
  dcl  failMsg char(*);

  dcl  segP ptr;
  dcl  segL fixed bin(21);
  dcl  seg char(segL) based(segP);

     if wdir = "" then				/* Is filename a full pathname?		        */
	call expand_pathname_(filename, dir, ent, ignoreCode);
     else do; dir = wdir; ent = filename; end;		/* Else, filename is in working dir.		        */

  dcl  initiate_file_ entry (char(*), char(*), bit(*), ptr, fixed bin(24), fixed bin(35));
  dcl  terminate_file_ entry (ptr, fixed bin(24), bit(*), fixed bin(35));
     
     call initiate_file_( dir, ent, R_ACCESS, segP, bc, ignoreCode);
     if segP ^= null then do;
	segL = divide(bc, bits_per_character, 24, 0);
	call ioa_("^a^a^a^a", successMsg, NL, NL, seg);
	call terminate_file_(segP, 0, TERM_FILE_TERM, ignoreCode);
	end;
     else call ioa_("^a", failMsg);
     return;
     

newWdirFileWithContents:
     entry ( wdir, filename, contents);

  dcl  contents char(*) varying;

  dcl  contP ptr;
  dcl  contL fixed bin(24);
  dcl  cont char(contL) based(contP);

     if wdir = "" then				/* Is filename a full pathname?		        */
	call expand_pathname_(filename, dir, ent, ignoreCode);
     else do; dir = wdir; ent = filename; end;		/* Else, filename is in working dir.		        */

  dcl  hcs_$make_seg entry (char(*), char(*), char(*), fixed bin(5), ptr, fixed bin(35));
     
     call hcs_$make_seg( dir, ent, "", RW_ACCESS_BIN, contP, code);
     if contP = null then do;
	call gripe(code, PROC, "Creating temporary file: ^a>^a", dir, ent);
	go to EXIT_from_cei;
	end;
     contL = length(contents)+length(NL);
     cont = contents || NL;

     if debugI >= 3 then do;
	call ioa_("");
	call ioa_("^a now contains: ------------------------------", filename);
	call ioa_("^a", cont);
	end;

     call terminate_file_(contP, length(cont)*bits_per_character, TERM_FILE_TRUNC_BC, ignoreCode);

     end deleteFile;

%page;
setup_pl1_files:					/* Prepare, if called from an entrypoint that needs       */
     proc (caller, c);				/*  temporary files for invoking PL/I compiler.	        */

  dcl  caller char(*);

  dcl 1 c aligned like cd;

  dcl  get_pdir_ entry() returns(char(168));
  dcl  get_wdir_ entry() returns(char(168));
  dcl  uniq char(15);
  dcl  unique_chars_ entry (bit(*)) returns(char(15));

     if c.in_use then return;				/* Someone has done this work earlier.		        */
     c.pl1_files.in_use = T;

     c.prefix = rtrim(caller);

     uniq = unique_chars_(""b);			/*  - Unique string (except leading !) used in naming     */
     c.unique = substr(uniq,2);			/*    temporary PL/I sources (and their entrypoints).     */
						/*    PL/I does not like ! in names.		        */

						/*  - Set pathnames of temporary files.		        */
     c.wdir = get_wdir_();
     c.objFilename  = c.prefix || "_" || c.unique;
     c.pl1Filename  = rtrim(c.objFilename) || ".pl1";
     c.listFilename = rtrim(c.objFilename) || ".list";
     c.foPathname   = rtrim(get_pdir_()) || ">" || c.prefix || "_" || c.unique || ".out";

     end setup_pl1_files;
%page;
%include access_mode_values;
%page;
%include arg_descriptor;
%page;
%include condition_info;
%page;
%include call_entry_info_;
%page;
  dcl 1 edi aligned like entry_desc_info;

%include entry_sequence_info;
%page;
%include pl1_symbol_type;
%page;
%include pl1_symbol_type_fcns;
%page;
%include pl1_descriptor_type_fcn;
%page;
%include system;
%page;
%include terminate_file;
