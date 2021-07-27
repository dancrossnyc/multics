/* ******************************************************
   *                                                    *
   * Copyright, (C) Honeywell Limited, 1983             *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   ****************************************************** */

/* format: style3,^delnl,linecom */
fort_data:
     procedure;

/* This procedure exists in order to create the builtin function data base for
   the fortran optimizing compiler.  It is based on an earlier version by
   Jayne Keller.

Written:	04 Apr 78, PES

Modified:
	22 Jun 84, MHM -- Install typeless functions support.
	28 Oct 80, CRD -- Add rest of Fortran 77 intrinsics.
	26 Dec 79, PES -- Add new ANSI77 character-string diddling functions.
*/

dcl	create_data_segment_
			ext entry (ptr, fixed bin (35) aligned);
dcl	com_err_		entry options (variable);
dcl	1 cdsa		like cds_args aligned;
dcl	(get_temp_segments_, release_temp_segments_)
			entry (char (*), (*) ptr, fixed bin (35));
dcl	cleanup		condition;
dcl	code		fixed bin (35);
dcl	myname		char (12) init ("fort_data") static internal options (constant);
dcl	segptrs		(1) ptr init (null ());

dcl	addr		builtin;
dcl	hbound		builtin;

dcl	(
	intr		init (1),
	real		init (2),
	dp		init (3),
	cmpx		init (4),
	logical		init (5),
	character		init (6),
	typeless		init (7)
	)		internal static options (constant) fixed bin;

dcl	(i, abs, alog, alog10, amax1, amin1, amod, atan, atan2, cabs, ccos,
	cexp, cchar, clog, cos, cosh, csin, csqrt, dabs, datan, tan, dtan,
	dtanh, asin, dasin, acos, dacos, datan2, dcos, dcosh, ddim, dexp,
	dim, dlog, dlog10, dmax1, dmin1, dmod, dsign, dsin, dsinh, dsqrt,
	exp, iabs, ichar, idim, iindex, isign, len, lge, lgt, lle, llt, max,
	max0, min, min0, mod, sign, sin, sinh, sqrt, tanh, aint, dint,
	anint, dnint, nint, idnint, and, bool, compl, fld, ilr, ils, irl,
	irs, or, xor
	)		fixed bin;

%include cds_args;

/* The structure fort_data$ explains to the compiler about all acceptable
   builtin functions, both internal and external.  The fields have the
   following meanings:

   generic_name - On if this is the name of a generic function.

   generic_func - An array of indices referencing this table. One entry for
   each data type. A zero entry indicates no builtin for that data type.

   result_type - Specify attributes for result.
*/
dcl	p		ptr;

dcl	1 fort_data	based (p),
	  2 builtin_name,
	    3 number_of_names
			fixed bin (15),
	    3 description	(93),
	      4 name	char (8) aligned,
	      4 generic_name
			bit (1) unaligned,
	      4 reserved	bit (35) unaligned,
	      4 generic_func
			(4) fixed bin,
	      4 result_type fixed bin;

	on cleanup
	     call release_temp_segments_ (myname, segptrs, (0));
	call get_temp_segments_ (myname, segptrs, code);
	if code ^= 0
	then do;
		call com_err_ (code, myname, "Getting temp segments");
		return;
	     end;

	p = segptrs (1);

/* Initialize and create the data base. */

	i = 0;

	call build_entry ("abs     ", real);
	abs = i;
	call build_entry ("iabs    ", intr);
	iabs = i;
	call build_entry ("dabs    ", dp);
	dabs = i;
	call build_entry ("cabs    ", real);
	cabs = i;
	call build_entry ("alog    ", real);
	alog = i;
	call build_entry ("dlog    ", dp);
	dlog = i;
	call build_entry ("clog    ", cmpx);
	clog = i;
	call build_entry ("alog10  ", real);
	alog10 = i;
	call build_entry ("dlog10  ", dp);
	dlog10 = i;
	call build_entry ("atan    ", real);
	atan = i;
	call build_entry ("datan   ", dp);
	datan = i;
	call build_entry ("atan2   ", real);
	atan2 = i;
	call build_entry ("datan2  ", dp);
	datan2 = i;
	call build_entry ("cos     ", real);
	cos = i;
	call build_entry ("dcos    ", dp);
	dcos = i;
	call build_entry ("ccos    ", cmpx);
	ccos = i;
	call build_entry ("dim     ", real);
	dim = i;
	call build_entry ("idim    ", intr);
	idim = i;
	call build_entry ("ddim    ", dp);
	ddim = i;
	call build_entry ("exp     ", real);
	exp = i;
	call build_entry ("dexp    ", dp);
	dexp = i;
	call build_entry ("cexp    ", cmpx);
	cexp = i;
	call build_entry ("max     ", real);
	max = i;
	call build_entry ("amax0   ", real);
	call build_entry ("amax1   ", real);
	amax1 = i;
	call build_entry ("max0    ", intr);
	max0 = i;
	call build_entry ("max1    ", intr);
	call build_entry ("dmax1   ", dp);
	dmax1 = i;
	call build_entry ("min     ", real);
	min = i;
	call build_entry ("amin0   ", real);
	call build_entry ("amin1   ", real);
	amin1 = i;
	call build_entry ("min0    ", intr);
	min0 = i;
	call build_entry ("min1    ", intr);
	call build_entry ("dmin1   ", dp);
	dmin1 = i;
	call build_entry ("mod     ", intr);
	mod = i;
	call build_entry ("amod    ", real);
	amod = i;
	call build_entry ("dmod    ", dp);
	dmod = i;
	call build_entry ("sign    ", real);
	sign = i;
	call build_entry ("isign   ", intr);
	isign = i;
	call build_entry ("dsign   ", dp);
	dsign = i;
	call build_entry ("sin     ", real);
	sin = i;
	call build_entry ("dsin    ", dp);
	dsin = i;
	call build_entry ("csin    ", cmpx);
	csin = i;
	call build_entry ("sqrt    ", real);
	sqrt = i;
	call build_entry ("dsqrt   ", dp);
	dsqrt = i;
	call build_entry ("csqrt   ", cmpx);
	csqrt = i;
	call build_entry ("tanh    ", real);
	tanh = i;
	call build_entry ("int     ", intr);
	call build_entry ("aint    ", real);
	aint = i;
	call build_entry ("idint   ", intr);
	call build_entry ("float   ", real);
	call build_entry ("ifix    ", intr);
	call build_entry ("sngl    ", real);
	call build_entry ("real    ", real);
	call build_entry ("aimag   ", real);
	call build_entry ("dble    ", dp);
	call build_entry ("cmplx   ", cmpx);
	call build_entry ("conjg   ", cmpx);
	call build_entry ("tan     ", real);
	tan = i;
	call build_entry ("dtan    ", dp);
	dtan = i;
	call build_entry ("asin    ", real);
	asin = i;
	call build_entry ("dasin   ", dp);
	dasin = i;
	call build_entry ("acos    ", real);
	acos = i;
	call build_entry ("dacos   ", dp);
	dacos = i;
	call build_entry ("char    ", character);
	cchar = i;
	call build_entry ("ichar   ", intr);
	ichar = i;
	call build_entry ("index   ", intr);
	iindex = i;
	call build_entry ("len     ", intr);
	len = i;
	call build_entry ("lge     ", logical);
	lge = i;
	call build_entry ("lgt     ", logical);
	lgt = i;
	call build_entry ("lle     ", logical);
	lle = i;
	call build_entry ("llt     ", logical);
	llt = i;
	call build_entry ("cosh    ", real);
	cosh = i;
	call build_entry ("sinh    ", real);
	sinh = i;
	call build_entry ("dcosh   ", dp);
	dcosh = i;
	call build_entry ("dsinh   ", dp);
	dsinh = i;
	call build_entry ("dtanh   ", dp);
	dtanh = i;
	call build_entry ("dint    ", dp);
	dint = i;
	call build_entry ("anint   ", real);
	anint = i;
	call build_entry ("dnint   ", dp);
	dnint = i;
	call build_entry ("nint    ", intr);
	nint = i;
	call build_entry ("idnint  ", intr);
	idnint = i;
	call build_entry ("dprod   ", dp);
	and = i;
	call build_entry ("and     ", typeless);
	bool = i;
	call build_entry ("bool    ", typeless);
	compl = i;
	call build_entry ("compl   ", typeless);
	fld = i;
	call build_entry ("fld     ", typeless);
	ilr = i;
	call build_entry ("ilr     ", intr);
	ils = i;
	call build_entry ("ils     ", intr);
	irl = i;
	call build_entry ("irl     ", intr);
	irs = i;
	call build_entry ("irs     ", intr);
	or = i;
	call build_entry ("or      ", typeless);
	xor = i;
	call build_entry ("xor     ", typeless);

	p -> fort_data.number_of_names = i;

/* format: off */
/* Define the generic functions. */
/*			name	int	real	dp	complex */

	call create_generic (abs,	iabs,	abs,	dabs,	cabs);
	call create_generic (alog,	alog,	alog,	dlog,	clog);
	call create_generic (alog10,	alog10,	alog10,	dlog10,	0);
	call create_generic (atan,	0,	atan,	datan,	0);
	call create_generic (atan2,	0,	atan2,	datan2,	0);
	call create_generic (cos,	cos,	cos,	dcos,	ccos);
	call create_generic (dim,	idim,	dim,	ddim,	0);
	call create_generic (exp,	exp,	exp,	dexp,	cexp);
	call create_generic (max,	max0,	amax1,	dmax1,	0);
	call create_generic (min,	min0,	amin1,	dmin1,	0);
	call create_generic (mod,	mod,	amod,	dmod,	0);
	call create_generic (sign,	isign,	sign,	dsign,	0);
	call create_generic (sin,	sin,	sin,	dsin,	csin);
	call create_generic (sqrt,	sqrt,	sqrt,	dsqrt,	csqrt);
	call create_generic (tanh,	tanh,	tanh,	dtanh,	0);
	call create_generic (tan,	tan,	tan,	dtan,	0);
	call create_generic (asin,	asin,	asin,	dasin,	0);
	call create_generic (acos,	acos,	acos,	dacos,	0);
	call create_generic (cosh,	cosh,	cosh,	dcosh,	0);
	call create_generic (sinh,	sinh,	sinh,	dsinh,	0);
	call create_generic (aint,	aint,	aint,	dint,	0);
	call create_generic (anint,	anint,	anint,	dnint,	0);
	call create_generic (nint,	nint,	nint,	idnint,	0);

/* format: on */

build_entry:
     procedure (name, result_mode);
dcl	name		char (8) aligned,
	result_mode	fixed bin;

	i = i + 1;

	if i > hbound (p -> fort_data.description, 1)
	then do;
		call com_err_ (0, myname, "Builtin table overflow.");
		go to ABORT;
	     end;

	unspec (p -> fort_data.description (i)) = "0"b;
	p -> fort_data.description (i).name = name;
	p -> fort_data.description (i).result_type = result_mode;

     end build_entry;


/* this sets up the generic functions as needed. */
create_generic:
     proc (table_offset, int_func_offset, real_func_offset, dp_func_offset, complx_func_offset);
dcl
	(table_offset, int_func_offset, real_func_offset, dp_func_offset, complx_func_offset)
			fixed bin;

	p -> fort_data.description (table_offset).generic_name = "1"b;
	p -> fort_data.description (table_offset).generic_func (intr) = int_func_offset;
	p -> fort_data.description (table_offset).generic_func (real) = real_func_offset;
	p -> fort_data.description (table_offset).generic_func (dp) = dp_func_offset;
	p -> fort_data.description (table_offset).generic_func (cmpx) = complx_func_offset;
     end;

/* now call create_data_segment_ */

	unspec (cdsa) = "0"b;
	cdsa.have_text = "1"b;
	cdsa.sections (1).p = p;
	cdsa.sections (1).len = divide (length (unspec (fort_data)), 36, 17, 0);
	cdsa.sections (1).struct_name = "fort_data";
	cdsa.seg_name = myname;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then
	     call com_err_ (code, myname, "Creating ^a data segment", myname);

ABORT:
	call release_temp_segments_ (myname, segptrs, (0));

     end fort_data;
