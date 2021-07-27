/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(85-09-09,Farley), approve(85-09-09,MCR6979),
     audit(86-03-06,GDixon), install(86-03-21,MR12.0-1033):
     Support IMU.
  2) change(86-01-07,Fawcett), approve(86-04-11,MCR7383),
     audit(86-05-12,Coppola), install(86-07-17,MR12.0-1097):
     Add support for subvolumes on MSU3380 and MSU3390.
  3) change(87-10-02,Farley), approve(88-02-26,MCR7794),
     audit(88-03-04,Fawcett), install(88-03-15,MR12.2-1035):
     Added imu_style_iom flag, so that init_early_config can create the proper
     IOM config entry.
  4) change(87-10-19,Farley), approve(88-02-26,MCR7795),
     audit(88-03-04,Fawcett), install(88-03-15,MR12.2-1035):
     Added default_time_zone and default_time_zone_delta, so that predefined
     time zone information can be available at boot time.
  5) change(87-10-19,Farley), approve(88-02-26,MCR7796),
     audit(88-03-04,Fawcett), install(88-03-15,MR12.2-1035):
     Added default_rpv_data, so that predefined RPV information can be
     available at boot time.
                                                   END HISTORY COMMENTS */
/* format: style2 */
bootload_info:
     procedure;

/* *	This is a CDS which is used to generate the bootload_info segment, used in
   *	bound_bootload_0 and various later parts of collection 1 and 2 bootload.
   *      In a later release, it will ask questions to allow certain
   *      configuration info to be "canned" onto the MST. For now,
   *      none of the answers are used, so all that is commented out/
   *      skipped.
   *
   *	11/02/80, W. Olin Sibert
   *      8/82 BIM
   *      x/83 Keith Loepere for various values, including bce_sst_sizes
   *	4/84 Keith Loepere for bce_intk_card.
   *	3/85 Keith Loepere for safe_config_deck_frec.
   */
%page;
	dcl     1 bootload_info	 aligned,		/* Data used by collection 1 bootload, mostly */
		2 structure_start	 pointer init (null ()),
						/* Beginning of this structure */
		2 structure_size	 fixed bin init (0),/* And its length in words, for use in copying */
		2 status_mask	 bit (36) aligned init ("370000770000"b3),
						/* Mask to check status word against */
		2 bce_dbr		 bit (72) aligned,	/* double word aligned */
		2 iom_boot_info	 (8) fixed bin (71) init ((8) - 1),
						/* Information copied from that left in low memory by the */
						/* IOM Bootload Program. */
		2 boot_without_query bit (1) aligned init ("0"b),
						/* info is here */
		2 iom_port_table	 (4) fixed bin (35) init ((4) - 1),
						/* SCU port for each IOM */
		2 imu_style_iom	 bit (1) aligned init ("0"b),
						/* Bootload IOM is an IMU */
		2 cold_tape_mpc	 bit (1) aligned init ("1"b),
						/* F/W in tape MPC */
		2 tape_iom_number	 fixed bin (3) init (-1),
						/* Bootload tape IOM number */
		2 tape_channel_number
				 fixed bin (6) init (-1),
						/* Bootload tape channel number */
		2 tape_device_number fixed bin (6) init (-1),
						/* Bootload tape device number, once known */
		2 tape_mpc_fw_name	 char (32) unal init (""),
		2 cold_disk_mpc	 bit (1) aligned init ("1"b),
		2 default_rpv_data	 char (24) unaligned init (""),
						/* Predefined RPV information */
		2 disk_mpc_chanid	 char (8) aligned init (""),
		2 disk_device_has_sv bit (1) aligned init ("0"b),
		2 disk_device_sv     fixed bin (17) init (-1),
		2 disk_device_number fixed bin (6) init (-1),
						/* Bootload disk device number, once known */
		2 disk_model_number	 fixed bin init (-1),
						/* Bootload disk canonical device model number */
		2 disk_mpc_model_number
				 fixed bin init (-1),
						/* Bootload disk MPC type, possibly default. */
		2 system_type	 fixed bin,	/* As in l68 vs adp */
		2 disk_mpc_fw_rev_bcd
				 bit (36) aligned init (""b),
		2 bce_part_frec	 fixed bin,
		2 bce_part_nrec	 fixed bin,
		2 mst_past_bce_frec	 fixed bin,	/* first record on rpv for mst area used for segments past bce usage (i.e. collections 2 and 3) */
		2 config_part_frec	 fixed bin,
		2 safe_config_deck_frec
				 fixed bin,
		2 console_iom_number fixed bin (3) init (0),
						/* IOM Numbers */
		2 console_channel_number
				 fixed bin (6) init (-1),
						/* Channel number of bootload console */
		2 console_model	 fixed bin (35) init (0),
		2 console_pcw_check  bit (1) aligned init ("0"b),
		2 console_uses_pcw   bit (1) aligned init ("1"b),
		2 bootload_mem_size	 fixed bin (35) init (512 * 1024),
						/* Size of bootload memory for collection 1 */
		2 contig_mem_size	 fixed bin (35) init (0),
		2 bootload_1_ptr	 pointer,		/* Pointer to base of bootload_1 */
		2 lot_ptr		 pointer,		/* Pointer to base ot lot */
		2 sys_boot_info_ptr	 pointer,		/* Pointer to our sister segment */
		2 assume_config_deck bit (1) aligned init ("1"b),
		2 config_has_been_modified
				 bit (1) aligned init ("0"b),
						/* oper should perform a reinit */
						/* assume BOS or whatever has left a valid config deck around in the canonical place */
		2 console_available	 bit (1) aligned init ("0"b),
						/* early console support turned on */
		2 sysid		 char (32) unaligned init (""),
						/* MST magic fills this in. */
		2 creation_time	 fixed bin (71) init (-1),
						/* Time this structure was created */
		2 creation_time_string
				 char (24) unaligned init (""),
						/* Character representation of the above */
		2 creator		 char (32) unaligned init (""),
						/* User who created this segment */
		2 creation_site_id	 char (32) unaligned init (""),
						/* Installation ID of site where it was created */
		2 default_time_zone  char (4) unaligned init ("gmt"),
						/* Time zone to use before config available */
		2 default_time_zone_delta
				 fixed bin (71) init (0),
						/* Binary offset of the above */
		2 rpv_cold_boot	 bit (1) aligned init ("0"b),
						/* Turned on in cold boot until RPV is formatted */
		2 bce_intk_card	 aligned like intk_card,
		2 at_bce_cl	 bit (1) aligned init ("0"b),
						/* controlled by bce_get_to_command_level */
		2 pad_align_2	 fixed bin (71) init (0),
		2 l68_fault_names	 (32) char (8) aligned init (
						/** **/
				 "shutdown", "store   ", "mme1    ", "ft1     ", "timer-ro", "command ",
				 "derail  ", "lockup  ", "connect ", "parity  ", "ipr     ", "onc     ",
				 "startup ", "overflow", "divcheck", "execute ", "segment ", "page    ",
				 "df2     ", "df3     ", "acv     ", "mme2    ", "mme3    ", "mme4    ",
				 "linkage ", "ft3     ", "fault26 ", "fault27 ", "fault28 ", "fault29 ",
				 "fault30 ", "trouble "),
		2 adp_fault_names	 (32) char (8) aligned init (
						/** **/
				 "shutdown", "store   ", "mme1    ", "ft1     ", "timer-ro", "command ",
				 "derail  ", "lockup  ", "connect ", "memsys  ", "ipr     ", "onc     ",
				 "startup ", "overflow", "divcheck", "execute ", "mme2    ", "mme3    ",
				 "segment ", "mme4    ", "page    ", "page-wrt", "acv     ", "fault23 ",
				 "linkage ", "ft3     ", "fault26 ", "fault27 ", "hype-dis", "hype-cio",
				 "hype-tro", "trouble "),
		2 bce_sst_sizes	 (0:3) fixed bin init (64, 32, 8, 8),
						/* size of sst pools for bce */
		2 end_of_bootload_info
				 pointer init (null ());
						/* Last location in the structure */


	dcl     1 cds_data		 aligned like cds_args;
						/* arguments to create_data_segment_ subr */

	dcl     code		 fixed bin (35);

	dcl     com_err_		 entry () options (variable);
	dcl     create_data_segment_	 entry (pointer, fixed binary (35));
	dcl     get_group_id_	 entry () returns (char (32));
	dcl     ioa_		 entry options (variable);
	dcl     system_info_$installation_id
				 entry (char (*));



	dcl     PAD		 (1) char (32) init ("pad*") int static options (constant);
	dcl     WHOAMI		 char (32) internal static options (constant) init ("bootload_info");


	dcl     (addr, null, size, string, unspec)
				 builtin;


	bootload_info.creator = get_group_id_ ();
	call system_info_$installation_id (bootload_info.creation_site_id);

	unspec (bootload_info.bootload_1_ptr), unspec (bootload_info.sys_boot_info_ptr),
	     unspec (bootload_info.lot_ptr) = "077777000043000000000000"b3;
						/* ring 0 pointers to base of segment -1 */

/* For the first release, ask no questions, just fill in the */
/* salient values */

	bootload_info.boot_without_query = "0"b;
	bootload_info.tape_mpc_fw_name = "";
	bootload_info.disk_mpc_chanid = "";
	bootload_info.disk_device_has_sv = "0"b;
	bootload_info.disk_device_sv = -1;
	bootload_info.disk_device_number = 0;
	bootload_info.disk_model_number = 0;
	bootload_info.disk_mpc_model_number = 0;

	bootload_info.structure_size = size (bootload_info);
						/* set it up for copying */

	cds_data.sections (1).p = addr (bootload_info);
	cds_data.sections (1).len = size (bootload_info);
	cds_data.sections (1).struct_name = WHOAMI;

	cds_data.seg_name = WHOAMI;

	cds_data.num_exclude_names = 1;
	cds_data.exclude_array_ptr = addr (PAD);

	string (cds_data.switches) = "0"b;
	cds_data.switches.have_text = "1"b;		/* only create text section */

	call create_data_segment_ (addr (cds_data), code);

	if code ^= 0
	then do;					/* Nothing can be done */
		call com_err_ (code, WHOAMI);
		return;
	     end;

	cds_data.seg_name = "sys_boot_info";		/* Create the unbound copy */

	call ioa_ ("^a: Generating sys_boot_info copy of bootload_info.", WHOAMI);

	call create_data_segment_ (addr (cds_data), code);

	if code ^= 0
	then do;					/* Nothing can be done */
		call com_err_ (code, WHOAMI);
		return;
	     end;

	return;

/* format: off */
%page; %include cds_args;
%page; %include config_intk_card;
/* format: on */
     end bootload_info;
