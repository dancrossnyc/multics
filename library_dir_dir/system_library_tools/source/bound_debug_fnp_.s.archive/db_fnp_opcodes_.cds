/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */


/* DB_FNP_OPCODES_ - Procedure that defines a table of FNP opcodes */

/* Written February 1977 by Larry Johnson */

db_fnp_opcodes_: proc;

/* Automatic */

dcl  code fixed bin (35);
dcl  ptr_array (1) ptr;
dcl 1 cds like cds_args automatic;

/* Constants */

dcl  name char (15) int static options (constant) init ("db_fnp_opcodes_");
dcl  exclude_all char (32) int static options (constant) init ("**");

/* External */

dcl  get_temp_segments_ entry (char (*), dim (*) ptr, fixed bin (35));
dcl  release_temp_segments_ entry (char (*), dim (*) ptr, fixed bin (35));
dcl  create_data_segment_ entry (ptr, fixed bin (35));
dcl  com_err_ entry options (variable);
dcl  cv_oct_ entry (char (*)) returns (fixed bin (35));

dcl (addr, null, translate) builtin;

dcl  cleanup condition;


/* Setup temp segment */

	ptr_array = null;
	on cleanup call clean_up;
	call get_temp_segments_ (name, ptr_array, code);
	if code ^= 0 then do;
	     call com_err_ (code, name, "Unable to get temp segment.");
	     return;
	end;

/* Fill the opcode table */

	optablep = ptr_array (1);
	optable.cnt = 0;
	call build_table;

/* Now create the data segment */

	cds.p (1) = optablep;
	cds.len (1) = size (optable);
	cds.struct_name (1) = "optable";
	cds.p (2) = null;
	cds.len (2) = 0;
	cds.struct_name (2) = "";
	cds.seg_name = name;
	cds.num_exclude_names = 1;
	cds.exclude_array_ptr = addr (exclude_all);
	string (cds.switches) = "0"b;
	cds.have_text = "1"b;
	call create_data_segment_ (addr (cds), code);
	if code ^= 0 then call com_err_ (name, code);
	call clean_up;
	return;


/* Procedure to fill the opcode table */

build_table: proc;

	     call store (0, "ada   ", "X06X");
	     call store (0, "adcx1 ", "X42X");
	     call store (0, "adcx2 ", "X02X");
	     call store (0, "adcx3 ", "X40X");
	     call store (0, "adq   ", "X46X");
	     call store (0, "ana   ", "X34X");
	     call store (0, "ansa  ", "X32X");
	     call store (0, "aos   ", "X76X");
	     call store (0, "asa   ", "X16X");
	     call store (0, "cana  ", "X31X");
	     call store (0, "cioc  ", "X60X");
	     call store (0, "cmpa  ", "X27X");
	     call store (0, "cmpq  ", "X67X");
	     call store (0, "cmpx1 ", "X63X");
	     call store (0, "cmpx2 ", "X23X");
	     call store (0, "cmpx3 ", "X61X");
	     call store (0, "dvf   ", "X21X");
	     call store (0, "era   ", "X35X");
	     call store (0, "ersa  ", "X62X");
	     call store (0, "lda   ", "X07X");
	     call store (0, "ldex  ", "X30X");
	     call store (0, "ldi   ", "X44X");
	     call store (0, "ldq   ", "X47X");
	     call store (0, "ldx1  ", "X43X");
	     call store (0, "ldx2  ", "X03X");
	     call store (0, "ldx3  ", "X41X");
	     call store (0, "mpf   ", "X01X");
	     call store (0, "ora   ", "X37X");
	     call store (0, "orsa  ", "X72X");
	     call store (0, "sba   ", "X26X");
	     call store (0, "sbq   ", "X66X");
	     call store (0, "ssa   ", "X36X");
	     call store (0, "sta   ", "X17X");
	     call store (0, "stex  ", "X70X");
	     call store (0, "sti   ", "X54X");
	     call store (0, "stq   ", "X57X");
	     call store (0, "stx1  ", "X53X");
	     call store (0, "stx2  ", "X13X");
	     call store (0, "stx3  ", "X50X");
	     call store (0, "stz   ", "X56X");
	     call store (0, "szn   ", "X20X");
	     call store (0, "tmi   ", "X75X");
	     call store (0, "tnc   ", "X45X");
	     call store (0, "tnz   ", "X64X");
	     call store (0, "tov   ", "X55X");
	     call store (0, "tpl   ", "X65X");
	     call store (0, "tra   ", "X71X");
	     call store (0, "tsy   ", "X10X");
	     call store (0, "tze   ", "X74X");
	     call store (0, "adaq  ", "X15X");
	     call store (0, "ldaq  ", "X04X");
	     call store (0, "sbaq  ", "X24X");
	     call store (0, "staq  ", "X14X");
	     call store (1, "iaa   ", "773X");
	     call store (2, "iacx1 ", "173X");
	     call store (2, "iacx2 ", "273X");
	     call store (2, "iacx3 ", "373X");
	     call store (1, "iana  ", "022X");
	     call store (1, "iaq   ", "573X");
	     call store (1, "icana ", "222X");
	     call store (1, "icmpa ", "422X");
	     call store (1, "iera  ", "322X");
	     call store (1, "ila   ", "673X");
	     call store (1, "ilq   ", "473X");
	     call store (1, "iora  ", "122X");
	     call store (1, "sel   ", "073X");
	     call store (1, "rier  ", "012X");
	     call store (1, "ria   ", "412X");
	     call store (1, "sier  ", "052X");
	     call store (1, "sic   ", "452X");
	     call store (3, "alp   ", "3336");
	     call store (3, "alr   ", "2336");
	     call store (3, "als   ", "0336");
	     call store (3, "arl   ", "2337");
	     call store (3, "ars   ", "0337");
	     call store (3, "llr   ", "2334");
	     call store (3, "lls   ", "0334");
	     call store (3, "lrl   ", "2335");
	     call store (3, "lrs   ", "0335");
	     call store (3, "qlp   ", "7336");
	     call store (3, "qlr   ", "6336");
	     call store (3, "qls   ", "4336");
	     call store (3, "qrl   ", "6337");
	     call store (3, "qrs   ", "4337");
	     call store (4, "caq   ", "6333");
	     call store (4, "cax1  ", "4332");
	     call store (4, "cax2  ", "0332");
	     call store (4, "cax3  ", "4333");
	     call store (4, "cqa   ", "7333");
	     call store (4, "cx1a  ", "2332");
	     call store (4, "cx2a  ", "3332");
	     call store (4, "cx3a  ", "3333");
	     call store (4, "dis   ", "4331");
	     call store (4, "eni   ", "7331");
	     call store (4, "inh   ", "3331");
	     call store (4, "nop   ", "2331");
	     call store (4, "nrm   ", "1336");
	     call store (4, "nrml  ", "1334");

	     return;

	end build_table;


/* Procedure to put one entry in the table */

store:	proc (n, name, code);

dcl  n fixed bin;					/* Opcode type */
dcl  name char (6);					/* Name of opcode */
dcl  code char (4);					/* Opcode - X means dont care */

dcl  temp char (4);

	     optable.cnt = optable.cnt + 1;
	     opp = addr (optable.entry (optable.cnt));

	     op.name = name;
	     op.type = n;

	     temp = translate (code, "0", "X");		/* Replace xes by zeroes to get opcode */
	     op.code = bit (bin (cv_oct_ (temp), 12), 12);

	     temp = translate (code, "077777777", "X01234567"); /* Get mask for finding opcode */
	     op.mask = bit (bin (cv_oct_ (temp), 12), 12);

	     return;

	end store;


clean_up:	proc;

	     if ptr_array (1) ^= null then call release_temp_segments_ (name, ptr_array, code);
	     return;

	end clean_up;

%include debug_fnp_data;

%include cds_args;
     end db_fnp_opcodes_;
