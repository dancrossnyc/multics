/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(86-04-02,Dupuis), approve(86-04-02,MCR7188), audit(86-07-29,GWMay),
     install(86-07-29,MR12.0-1106):
     84-12-28 Matthew Pierret: Written so that linus would not use hard-coded
     reserved words.
                                                   END HISTORY COMMENTS */

/* DESCRIPTION:
   
   linus_lila_tokens_
*/

/* HISTORY:
Written by Matthew Pierret, 12/27/84.
Modified:
*/
/* format: style2,ind3,ll79 */
%page;

linus_lila_tokens_:
	procedure ();

/* DECLARATIONS */

/* Automatic */

dcl  (
      code			fixed bin (35),
      1 local_cds_args		aligned like cds_args,
1 local_linus_lila_tokens aligned like linus_lila_tokens
     )				automatic;

/* Constant */

dcl  (
      EXCLUDE_ARRAY			dim (1) char (32) init ("pad*"),
      MYNAME			char (32) init ("linus_lila_tokens_")
     )				internal static options (constant);
    
/* Entry */

dcl  com_err_			entry() options(variable),
     create_data_segment_		entry (ptr, fixed bin(35));

/* Builtin */

dcl  (addr, hbound, size, unspec)	builtin;

%page;

local_linus_lila_tokens.version = LINUS_LILA_TOKENS_VERSION_1;
local_linus_lila_tokens.language = LINUS_LILA_TOKENS_LANGUAGE_ENGLISH;
local_linus_lila_tokens.first_chars = "dfisuw";
local_linus_lila_tokens.dup = "dup";
local_linus_lila_tokens.differ = "differ";
local_linus_lila_tokens.from = "from";
local_linus_lila_tokens.inter = "inter";
local_linus_lila_tokens.select = "select";
local_linus_lila_tokens.unique = "unique";
local_linus_lila_tokens.union = "union";
local_linus_lila_tokens.where = "where";

/* initialize the cds structure */

	unspec (local_cds_args) = ""b;
	local_cds_args.sections (1).p = addr (local_linus_lila_tokens);
	local_cds_args.sections (1).len = size (local_linus_lila_tokens);
	local_cds_args.sections (1).struct_name = "linus_lila_tokens";
	local_cds_args.seg_name = MYNAME;
	local_cds_args.exclude_array_ptr = addr (EXCLUDE_ARRAY);
	local_cds_args.num_exclude_names = hbound (EXCLUDE_ARRAY, 1);
	local_cds_args.switches.have_text = "1"b;

/* call cds to make the segment */

	call create_data_segment_ (addr (local_cds_args), code);
	if code ^= 0
	then call com_err_ (code, MYNAME);

	return;

/* Include Files */

%page;
%include cds_args;
%page;
%include linus_lila_tokens;
%page;
%include copy_flags;

end linus_lila_tokens_;
