/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */

config_deck_data_: proc (); 

/* Written August/September of 1984 by Allen Ball to replace old, hard-to-read version. */
/* Modified March 1985 to no longer need bce/Multics versions! Keith Loepere */

/* format: style4,initcol1,indattr,declareind8,dclind4,idind33,ifthenstmt,ifthen,^indproc,delnl,insnl */

dcl Me			       char (17) static options (constant) init ("config_deck_data_");
dcl PAD			       (1) char (32) int static options (constant) init ("pad*");

dcl addr			       builtin;
dcl before		       builtin;
dcl card_number		       fixed bin;
dcl code			       fixed bin (35);
dcl com_err_		       entry () options (variable);
dcl create_data_segment_	       entry (ptr, fixed bin (35));
dcl cv_dec_check_		       entry (char (*), fixed bin (35)) returns (fixed bin (35));
dcl dimension		       builtin;
dcl field			       char (12);
dcl field_counter		       fixed bin;
dcl field_pos		       fixed bin;
dcl i			       fixed bin;
dcl ioa_			       entry () options (variable);
dcl j			       fixed bin;
dcl last			       bit (1) init ("0"b);
dcl length		       builtin;
dcl ltrim			       builtin;
dcl number_of_cards		       fixed bin;
dcl number_of_repeats	       fixed bin;
dcl 1 repeating_group	       (14) aligned,
      2 field_name		       char (12) varying,
      2 field_type		       bit (2);
dcl repeating_group_length	       fixed bin;
dcl rtrim			       builtin;
dcl size			       builtin;
dcl substr		       builtin;
dcl test_number		       fixed bin (35);
dcl unspec		       builtin;
dcl valid_deck		       bit (1) init ("1"b);
dcl valid_field		       bit (1) init ("0"b);

	/*** First check to see if there are valid types of fields. ***/
	number_of_cards = dimension (config_deck_cards, 1);
	do card_number = 1 to number_of_cards;
	     field_pos = 0;
	     call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos, last);
	     /*** Check and see if this is a valid card name. ***/
	     do i = 1 to dimension (Card_names, 1);
		if field = Card_names (i) then valid_field = "1"b;
	     end;
	     if ^valid_field | last then call bad_card;
	     valid_field = "0"b;			/* Reset the signal now. */
	     call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos, last);
	     /*** Check and see if the second field is a valid subname. ***/
	     do i = 1 to dimension (Card_subnames, 1);
		if field = Card_subnames (i) then valid_field = "1"b;
	     end;
	     if ^valid_field then call bad_card;
	     valid_field = "0"b;
	     if ^last then do;
		do while (^last);
		     call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos, last);
		     do i = 1 to dimension (Card_field_names, 1);
			if field = Card_field_names (i) then valid_field = "1"b;
		     end;
		     if ^valid_field | last then call bad_card;
						/* At this point there must be an even number of fields. */
		     valid_field = "0"b;		/* Reset before we forget. */
		     call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos, last);
		     do i = 1 to dimension (Card_data_types, 1);
			if field = Card_data_types (i) then valid_field = "1"b;
		     end;
		     if ^valid_field then do;
			test_number = cv_dec_check_ (field, code);
			if code ^= 0 then call bad_card;
						/* Not a chance of being valid. */
		     end;
		     valid_field = "0"b;
		end;
	     end;
	end;
	if ^valid_deck then
	     return;
	else call ioa_ ("^a: (First pass) This seems to be a valid deck.", Me);

%page;	/*** Now that we have a seemingly reasonable deck let's fill in the blanks. ***/
	begin;

dcl 1 cds_data		       aligned like cds_args;
dcl 1 config_deck_data_	       aligned,
	     2 num_described_cards fixed bin aligned init (number_of_cards),
		2 Config_card_field_name (number_of_cards, 14) char (12) varying aligned,
		2 Config_card_field_type (number_of_cards, 14) bit (2) unaligned,
		2 Config_card_group_length (number_of_cards) fixed bin aligned,
		2 Config_card_min_specifiable_fields (number_of_cards) fixed bin aligned,
		2 Config_card_name (number_of_cards) char (4) aligned,
		2 Config_card_num_described_fields (number_of_cards) fixed bin aligned,
		2 Config_card_subname (number_of_cards) char (4) varying aligned;

	     unspec (Config_card_field_name) = "0"b;
	     unspec (Config_card_field_type) = "0"b;
	     unspec (Config_card_group_length) = "0"b;
	     unspec (Config_card_min_specifiable_fields) = "0"b;
	     unspec (Config_card_name) = "0"b;
	     unspec (Config_card_num_described_fields) = "0"b;
	     unspec (Config_card_subname) = "0"b;
	     unspec (repeating_group) = "0"b;
	     do card_number = 1 to number_of_cards;
		field_pos = 0;
		call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos, last);
		Config_card_name (card_number) = rtrim (field);
		call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos, last);
		if field = "emp" then
		     Config_card_subname (card_number) = "";
		else Config_card_subname (card_number) = rtrim (field);
		if ^last then do;
		     do field_counter = 1 repeat field_counter + 1 while (^last);
			call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos, last);
			if field = "repeat" then do;
			     field_counter = field_counter - 1;
			     call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos,
				last);
			     if last then do;
serious_error:
				call ioa_ ("^a: Serious error in:^/^a", Me, config_deck_cards (card_number));
				return;
			     end;
			     number_of_repeats = (cv_dec_check_ (field, code));
			     if code ^= 0 then goto serious_error;
			     do repeating_group_length = 1 repeat repeating_group_length + 1 while (^last);
				call get_next_field (config_deck_cards (card_number), (field_pos), field,
				     field_pos, last);
				if field = "minimum" | field = "repeat" then goto serious_error;
				repeating_group (repeating_group_length).field_name = field;
				call get_next_field (config_deck_cards (card_number), (field_pos), field,
				     field_pos, last);
				repeating_group (repeating_group_length).field_type =
				     determine_field_type (field, code);
				if code ^= 0 then goto serious_error;
			     end;
			     Config_card_group_length (card_number) = repeating_group_length - 1;
			     if (Config_card_group_length (card_number) * number_of_repeats) + field_counter > 14
				then
				goto serious_error;
			     do i = 1 to number_of_repeats;
				do j = 1 to Config_card_group_length (card_number);
				     Config_card_field_name (card_number,
					field_counter + j + (i - 1) * Config_card_group_length (card_number)) =
					rtrim (repeating_group (j).field_name);
				     Config_card_field_type (card_number,
					field_counter + j + (i - 1) * Config_card_group_length (card_number)) =
					repeating_group (j).field_type;
				end;
			     end;
			end;
			else if field = "minimum" then do;
			     field_counter = field_counter - 1;
			     call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos,
				last);
			     Config_card_min_specifiable_fields (card_number) = (cv_dec_check_ (field, code));
			     if code ^= 0 then goto serious_error;
			end;
			else do;
			     Config_card_field_name (card_number, field_counter) = rtrim (field);
			     call get_next_field (config_deck_cards (card_number), (field_pos), field, field_pos,
				last);
			     Config_card_field_type (card_number, field_counter) =
				determine_field_type (field, code);
			     if code ^= 0 then goto serious_error;
			end;
		     end;
		     Config_card_num_described_fields (card_number) =
			field_counter + Config_card_group_length (card_number) * number_of_repeats - 1;
		end;
	     end;
	     /*** Seems to be completely valid. ***/
	     call ioa_ ("^a: (Second pass) This is a valid config deck.", Me);

	     cds_data.sections (1).p = addr (config_deck_data_);
	     cds_data.sections (1).len = size (config_deck_data_);

	     cds_data.sections (1).struct_name = Me;

	     cds_data.seg_name = Me;

	     cds_data.num_exclude_names = 1;
	     cds_data.exclude_array_ptr = addr (PAD);
	     string (cds_data.switches) = "0"b;
	     cds_data.switches.have_text = "1"b;

	     call create_data_segment_ (addr (cds_data), code);

	     if code ^= 0 then call com_err_ (code, Me);
	     return;
	end;

%page;
bad_card:
     proc ();
	call ioa_ ("^a: Bad card.  Check '^a' in:", Me, field);
	call ioa_ ("^a", config_deck_cards (card_number));
	valid_deck = "0"b;
	return;
     end;
%page;
determine_field_type:
     proc (p_string, p_code) returns (bit (2) unaligned);

dcl i			       fixed bin;
dcl p_code		       fixed bin (35) parameter;
dcl p_string		       char (12) parameter;

	do i = 1 to dimension (Card_data_types, 1);
	     if Card_data_types (i) = p_string then do;
		p_code = 0;
		return (Card_data_bit_strings (i));
	     end;
	end;
	p_code = -1;
	return ("00"b);
     end;

%page;
get_next_field:
     proc (card, previous_field_pos, p_field, new_field_pos, p_last);

dcl card			       char (210) parameter;
dcl test_field		       char (210) init ("");
dcl new_field_pos		       fixed bin parameter;
dcl p_last		       bit (1) parameter;
dcl left_over_card		       char (210);
dcl p_field		       char (12) parameter;
dcl previous_field_pos	       fixed bin parameter;

	if previous_field_pos = 0 then
	     new_field_pos = 1;			/* First field is what he wants. */
	else new_field_pos = index (substr (rtrim (card), previous_field_pos), " ") + previous_field_pos;
	/*** Skip over whitespace. ***/
	do new_field_pos = new_field_pos repeat new_field_pos + 1 while (substr (rtrim (card), new_field_pos, 1) = " ");
	end;
	left_over_card = substr (rtrim (card), new_field_pos);
	test_field = before (left_over_card, " ");
	if index (rtrim (left_over_card), " ") = 0 then
	     p_last = "1"b;
	else p_last = "0"b;
	if length (rtrim (test_field)) > 12 then
	     p_field = "";
	else p_field = ltrim (test_field);
	return;
     end;

/* format: off */
%page;%include cds_args;
%page;%include config_deck_cards_;
%page;%include config_deck_keywords_;

     end /* config_deck_data_ */;
