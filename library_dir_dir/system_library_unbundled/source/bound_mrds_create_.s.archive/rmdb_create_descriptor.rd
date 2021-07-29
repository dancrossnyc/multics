/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (C) 1975 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems Inc.       *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(85-12-03,JBlair), approve(85-12-03,MCR7311),
     audit(86-09-24,Gilcrease), install(86-10-16,MR12.0-1187):
     Modified from the new_call subroutine. Added complex numbers. Enforce
     max string length.
                                                   END HISTORY COMMENTS */

/*++
PUSH DOWN LANGUAGE \
BEGIN	/ <no-token>		/						/ error	\

attr	/ dimension		/ 		DELETE LEX(2)			/ error   \
	/ dim			/ 		DELETE LEX(2)			/ error	\
	/ aligned			/ set (aligned, 1) 	DELETE				/ attr	\
	/ unaligned		/ set (aligned, 0) 	DELETE				/ attr	\
	/ unal			/ set (aligned, 0) 	DELETE				/ attr	\
	/ fixed			/ set (type, 1)	DELETE				/ attr	\
	/ float			/ set (type, 3)	DELETE				/ attr	\
	/ binary			/ set (base, 1)	DELETE				/ attr	\
	/ bin			/ set (base, 1)	DELETE				/ attr	\
	/ decimal			/ set (base, 2)	DELETE				/ attr	\
	/ dec			/ set (base, 2)	DELETE				/ attr	\
	/ real			/ set (mode, 1)	DELETE				/ attr	\
	/ complex			/ set (mode, 2)	DELETE				/ attr	\
	/ cplx			/ set (mode, 2)	DELETE				/ attr	\
	/ precision		/ 		DELETE LEX(2)			/ prec	\
	/ prec			/ 		DELETE LEX(2)			/ prec	\
	/ (			/        		       LEX(2)			/ prec	\
	/ bit			/ set (type, 19)	DELETE				/ length	\
	/ character		/ set (type, 21)	DELETE				/ length	\
	/ char			/ set (type, 21)	DELETE				/ length	\
	/ varying			/ set (varying, 1)	DELETE				/ attr	\
	/ var			/ set (varying, 1)	DELETE				/ attr	\
	/ nonvarying		/ set (varying, 0)	DELETE				/ attr	\
	/ signed			/ set (signed, 1)	DELETE				/ attr	\
	/ uns			/ set (signed, 0)	DELETE				/ attr	\
	/ unsigned		/ set (signed, 0)	DELETE				/ attr	\
	/ <any-token>		/ [code = mrds_error_$bad_attribute]			/ RETURN	\
	/ <no-token>		/						/ RETURN	\


error	/ ) <any-token>		/ [code = error_table_$improper_data_format]		/ RETURN	\


prec_err	/			/ [code = mrds_error_$bad_precision]			/ RETURN	\

length	/ (			/		       LEX(2)			/ length_	\
	/			/						/ attr	\
length_	/ ( <decimal-integer> )	/ LEX(-1) set(LENGTH, token.Nvalue) DELETE(-1,+1)		/ attr	\
	/ ( * )			/	set(LENGTH, 16777215)     DELETE(-2,0)		/ attr	\
	/			/						/ error	\

prec	/ ( <decimal-integer> )	/ LEX(-1) set(SIZE, token.Nvalue) DELETE(-1,+1)		/ attr	\
	/ ( <decimal-integer> ,	/ LEX(2)						/	\
	/ ( <decimal-integer> , <decimal-integer> )
				/ LEX(-3) set(SIZE, token.Nvalue)
				  LEX(+2) set(scale,token.Nvalue) DELETE(-3,+1)		/ attr	\
	/			/						/ prec_err \
										++*/

rmdb_create_descriptor: procedure (Astring, Ptemp, Pdesc, code);
						/* This internal procedure converts an argument	*/
						/*  declaration (PL/I style) into an argument	*/
						/*  descriptor.				*/
     dcl	Astring			char(*),		/* argument declaration. (In)			*/
	Ptemp			ptr,		/* ptr to a translator_temp_ segment in which	*/
						/*  allocations can be made. (In)		*/
						/* ptr to created argument descriptor. (Out)	*/
	Saddr			bit(1) aligned,	/* on if addr(declaration) was given. (Out)	*/
	code			fixed bin(35);	/* error code diagnosing any errors. (Out)	*/

     dcl	1 D			aligned,
	  2 type			fixed bin,
	  2 Spacked		bit(1),
	  2 Ndims			fixed bin,
	  2 size			fixed bin(24),
	  2 scale			fixed bin(24),
	Lit			fixed bin,
	Lstr			fixed bin,
	Ndims			fixed bin,
	Nparens			fixed bin,
	Pit			ptr,
	Pstr			ptr,
         (aligned, address, signed,
	varying)	                    fixed bin(1),
         (base, mode)		fixed bin(2),
          i			fixed bin,
         (LENGTH, SIZE)		fixed bin(24),
	scale			fixed bin(8),
	type			fixed bin(6);

     dcl	it			char(Lit) based (Pit),
	str			char(Lstr) based (Pstr),
	str_array (Lstr)		char(1) based (Pstr),
          value			bit(36) aligned based (Pdesc); 
     dcl  Pdesc	ptr;
		
	
     dcl (addr, bit, divide, length, null, search, size, string)
				builtin;

     dcl	set			generic (	set1  when (fixed bin(1),*),
					set2  when (fixed bin(2),*),
					set6  when (fixed bin(6),*),
					set8  when (fixed bin(8),*),
					set24 when (fixed bin(24),*));

     dcl (mrds_error_$bad_array_bounds,
	mrds_error_$bad_attribute,
	mrds_error_$bad_precision,
          mrds_error_$inconsistent_attributes,
	mrds_error_$invalid_string_length,
	mrds_data_$max_string_size,
	error_table_$improper_data_format,
	error_table_$unbalanced_parentheses)
				fixed bin(35) ext static;

	Saddr = "0"b;
	code = 0;					/* clear error code.			*/
	Ptoken, Pthis_token = null;			/* initialize semantic analysis variables.	*/
	Nparens = 0;				/* initialize parenthesis depth count.		*/
	Pstr = addr(Astring);			/* overlay PL/I argument declaration.		*/
	Lstr = length(Astring);
	aligned = -1;
	type = -1;
	base = -1;
	mode = -1;
	signed = -1;
	varying = -1;
	address = -1;
	Ndims = 0;
	LENGTH = -1;
	SIZE = -1;
	scale = -129;

	do while (Lstr > 0);			/* parse declaration into tokens.		*/
	     i = search (str, " _,():");
	     if i = 0 then
		i = Lstr + 1;
	     if i > 1 then do;
		Pit = Pstr;
		Lit = i-1;
		call make_token (it);
		if i <= Lstr then do;
		     Pstr = addr(str_array(i));
		     Lstr = Lstr - (i-1);
		     end;
		else Lstr = 0;
		end;
	     if Lstr > 0 then do;
		Pit = Pstr;
		Lit = 1;
		if      it = "(" then Nparens = Nparens + 1;
		else if it = ")" then Nparens = Nparens - 1;
		if      it = " " then;
		else if it = "_" then;
		else call make_token(it);
		if Lstr > 1 then
		     Pstr = addr(str_array(2));
		Lstr = Lstr - 1;
		end;
	     end;
	if Nparens ^= 0 then do;
	     code = error_table_$unbalanced_parentheses;
	     return;
	     end;
	call SEMANTIC_ANALYSIS();
	if code = -1 then do;
	     code = 0;
	     return;
	     end;
	if code ^= 0 then return;

						/* apply PL/I Lanuage Default Rules.		*/
	if type = -1 then				/* default(^(character|bit|pointer|offset|area|	*/
	     if base = -1 then			/*  label|entry|file|fixed|float|binary|decimal|	*/
		if mode = -1 then do;		/*  real|complex)) fixed binary real;		*/
		     type = 1;
		     base = 1;
		     mode = 1;
		     end;
	if type = -1 then do;
	     if mode ^= -1 then			/* default((real|complex)&^float) fixed;	*/
	          if type ^= 3 then type = 1;
	     if base ^= -1 then			/* default((binary|decimal)&^float) fixed;	*/
	          if type ^= 3 then type = 1;
	     end;
	if (type = 1) | (type = 3) then		/* default((fixed|float)&^complex) real;	*/
	     if mode ^= 2 then mode = 1;
	if (type = 1) | (type = 3) then		/* default((fixed|float)&^decimal) binary;	*/
	     if base ^= 2 then base = 1;
	if type = 1 then				/* default(fixed&binary&^precision)		*/
	     if base = 1 then			/*  precison(17,0);				*/
		if SIZE = -1 then do;
		     SIZE = 17;
		     scale = 0;
		     end;
		else if scale = -129 then
		     scale = 0;
	if type = 1 then				/* default(fixed&decimal&^precision)		*/
	     if base = 2 then			/*  precision(7,0);				*/
		if SIZE = -1 then do;
		     SIZE = 7;
		     scale = 0;
		     end;
		else if scale = -129 then
		     scale = 0;
	if type = 3 then				/* default(float&binary&^precision)		*/
	     if base = 1 then			/*  precision(27);				*/
		if SIZE = -1 then SIZE = 27;
	if type = 3 then				/* default(float&decimal&^precision)		*/
	     if base = 2 then			/*  precision(10);				*/
		if SIZE = -1 then SIZE = 10;
	if type = 18 then				/* default(character&^length) length(1024);	*/
	     if LENGTH = -1 then LENGTH = 1024;
	if (type = 19) | (type = 21) then do;		/* default((character|bit)&^length) length(1);	*/
	     if LENGTH = -1 then LENGTH = 1;
						/* default((character|bit)&^varying) nonvarying;	*/
	     if varying ^= 1 then varying = 0;
						/* default((character|bit)&^aligned) unaligned;	*/
	     if aligned ^= 1 then aligned = 0;
	     end;
	if aligned ^= 0 then aligned = 1;		/* default(^unaligned) aligned;		*/

	go to do(type);

do(1):						/* it's a fixed number.			*/
	     if base = 1 then do;			/*      a fixed binary number.		*/
		if SIZE  >   35 then type = type + 1;	/*      a fixed binary long number.		*/
		if mode  =    2 then type = type + 4;	/*      a complex fixed binary number.		*/
		if SIZE  >   71 then go to error_oob;
		if SIZE  <    1 then go to error_oob;
		if scale > +127 then go to error_oob;
		if scale < -128 then go to error_oob;
		end;
	     else  /* if base = 2 then */  do;		/*      a fixed decimal number.		*/
		type = type + 8;
		if mode  =    2 then type = type + 2;	/*      a complex fixed decimal number.		*/
		if aligned = 0 then type = type + 34;        /*      4-bit byte aligned.                       */
		if SIZE  >   59 then go to error_oob;
		if SIZE  <    1 then go to error_oob;
		if scale > +127 then go to error_oob;
		if scale < -128 then go to error_oob;
		end;
	     if varying ^= -1 then go to error;
	     if LENGTH ^= -1 then go to error;
	     if type < 3 & signed = 0 then
		type = type + 32;
	     go to join;

do(3):						/* it's a floating number.			*/
	     if base = 1 then do;			/*      a float binary number.		*/
		if SIZE  >   27 then type = type + 1;	/*      a float binary long number.		*/
		if mode  =    2 then type = type + 4;	/*      a complex float binary number.		*/
		if SIZE  >   63 then go to error_oob;
		if SIZE  <    1 then go to error_oob;
		if scale = -129 then;
		else            go to error;
		end;
	     else  /* if base = 2 then */  do;		/*      a float decimal number.		*/
		type = type + 7;
		if mode  =    2 then type = type + 2;	/*      a complex float decimal number.		*/
		if aligned = 0 then type = type + 34;        /*      4-bit byte aligned.                       */
		if SIZE  >   59 then go to error_oob;
		if SIZE  <    1 then go to error_oob;
		if scale = -129 then;
		else            go to error;
		end;
	     if varying ^= -1 then go to error;
	     if LENGTH ^= -1 then go to error;
	     scale = 0;
	     go to join;


do(19):						/* it's a bit string.			*/
do(21):						/* it's a character string.			*/
	     if varying = 1 then do;			/*      a varying string.			*/
		type = type + 1;
		if aligned ^= 1 then aligned = 1;
		end;
	     if base ^= -1 then go to error;
	     if mode ^= -1 then go to error;
	     if scale ^= -129 then go to error;
	     if SIZE ^= -1 then go to error;
	     if LENGTH < 0 then go to error_oob;
	     if (type = 19 & divide (LENGTH+35, 36, 24, 0) > mrds_data_$max_string_size) |
	        (type = 21 & LENGTH > mrds_data_$max_string_size)
	     then do;
		code = mrds_error_$invalid_string_length;
		return;
		end;
	     SIZE = LENGTH;
	     go to join;

join:
	D.type = type;
	D.Spacked = ^bit(aligned,1);
	D.Ndims = 0;
	D.size = SIZE;
	D.scale = scale;

	D.Ndims = Ndims;
	call encode_descriptor (D.type, D.Spacked, D.Ndims, D.size, D.scale, value);
	if address = 1 then Saddr = "1"b;
	return;

error:	code = mrds_error_$inconsistent_attributes;
	return;
error_array:
	code = mrds_error_$bad_array_bounds;
	return;
error_oob:
	code = mrds_error_$bad_precision;
	return;

%include translator_temp_alloc;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


make_token: proc (value);				/* internal procedure to make a token descriptor.	*/

     dcl	value			char(*);		/* value of the token.			*/
     dcl	P			ptr;		/* ptr to newly-allocated token.		*/

	P = allocate (Ptemp, size(token));
	if Ptoken = null then do;
	     P->token.Plast = null;
	     Pthis_token = P;
	     Ptoken = P;
	     end;
	else do;
	     token.Pnext = P;
	     P->token.Plast = Ptoken;
	     Ptoken = token.Pnext;
	     end;
	token.Pnext = null;
	token.Pvalue = addr(value);
	token.Lvalue = length(value);
	token.Nvalue = 0;
	token.Pstmt = null;
	token.Psemant = null;
	string(token.S) = ""b;

	end make_token;

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


set1:	procedure (var1, value);			/* if var hasn't been set1 yet, set1 it to value;	*/
						/*  else complain.				*/
     dcl	var1			fixed bin(1),
	var2			fixed bin(2),
	var6			fixed bin(6),
	var8			fixed bin(8),
	var24			fixed bin(24),
	value			fixed bin(35) unal;

	if var1 ^= -1 then go to error;
	var1 = value;
	return;


set2:	entry	(var2, value);

	if var2 ^= -1 then go to error;
	var2 = value;
	return;



set6:	entry 	(var6, value);

	if var6 ^= -1 then go to error;
	var6 = value;
	return;

set8:	entry	(var8, value);

	if var8 ^= -129 then go to error;
	var8 = value;
	return;


set24:	entry	(var24, value);

	if var24 ^= -1 then go to error;
	var24 = value;

	end set1;
%page;

encode_descriptor: procedure (type, packed, Ndims, size, scale, descriptor);

     dcl	type			fixed bin,	/* data type	*/
	packed			bit(1) aligned,	/* on if data packed*/
	Ndims			fixed bin,	/* dimension (data)	*/
	size			fixed bin (24),	/* size (data)	*/
	scale			fixed bin (24),	/* scale (data)	*/
	descriptor		bit(36) aligned;	/* descriptor (data)*/

     dcl	1 D			based (addr (descriptor)) aligned,
	 (2 flag			bit (1),
	  2 type			bit (6),
	  2 packed		bit (1),
	  2 Ndims			bit (4),
	  2 size			bit (24)) unaligned;

     dcl (addr, bit, fixed, substr)	builtin;



/*  *  *  **  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


	D.flag = "1"b;				/* new type desc.	*/
	D.type = bit (fixed (type, 6), 6);		/* set type	*/
	D.packed = packed;				/* set packed bit	*/
	go to set (type);

set( 1):				/* real fixed bin short		*/
set( 2):				/* real fixed bin long		*/
set( 3):				/* real float bin short		*/
set( 4):				/* real float bin long		*/
set( 5):				/* complex fixed bin short		*/
set( 6):				/* complex fixed bin long		*/
set( 7):				/* complex float bin short		*/
set( 8):				/* complex float bin long		*/
set( 9):				/* real fixed decimal		*/
set(10):				/* real float decimal		*/
set(11):				/* complex fixed decimal		*/
set(12):				/* complex float decimal		*/
set(33):	                              /* real fixed binary short unsigned     */
set(34):	                              /* real fixed binary long unsigned
   */
set(43):	                              /* real fixed decimal 4-bit byte-aligned*/
set(44):	                              /* real float decimal 4-bit byte_aligned*/
set(45):	                              /* complex fixed dec 4-bit byte_aligned */
set(46):	                              /* complex float dec 4-bit byte_aligned */
	D.Ndims = bit (fixed (Ndims, 4), 4);
	if scale < 0 then
	     substr (D.size, 1, 12) = bit (fixed (scale + 1000000000000b, 12), 12);
	else
	     substr (D.size, 1, 12) = bit (fixed (scale, 12), 12);
	substr (D.size, 13, 12) = bit (fixed (size, 12), 12);
	return;


set(19):				/* bit string			*/
set(20):				/* varying bit string		*/
set(21):				/* character string			*/
set(22):				/* varying character string		*/
	D.Ndims = bit (fixed (Ndims, 4), 4);
	D.size = bit (fixed (size, 24), 24);
	return;

	end encode_descriptor;
