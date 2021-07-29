/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
/* HISTORY COMMENTS:
  1) change(85-09-09,Farley), approve(85-09-09,MCR6979),
     audit(86-01-17,CLJones), install(86-03-21,MR12.0-1033):
     Add support for FIPS
     type controlers and devices.
  2) change(86-04-21,Fawcett), approve(86-04-21,MCR7381),
     audit(86-05-13,LJAdams), install(86-07-17,MR12.0-1097):
     Correct the model for the FIPS tape drives for the 4600 series to 8200
     only for all drives.
  3) change(86-10-02,Fawcett), approve(86-10-02,PBF7383),
     audit(86-10-23,Farley), install(86-10-28,MR12.0-1200):
     Changed 3390 to 3381.
  4) change(86-10-21,Fawcett), approve(86-10-21,PBF7381),
     audit(86-10-23,Farley), install(86-10-28,MR12.0-1200):
     Removed tape drive models 3430, 3470 & 3670. The only supported
     FIPS tape drive is the STC 8200.
                                                   END HISTORY COMMENTS */
/* config_data_ -- Database of config cards, model number, etc. */
/* format: style2 */
/* Modified 830523 to add console io_type and line_leng fields for different 
   operator's consoles and to fix a size bug... -E. A. Ranzenbach */
/* Modified 840827 by Paul Farley for DAU (msp800) support. */
/* Modified 841101 by Paul Farley to add device_0_valid for disk & tape
   devices, controller entries for the IBM 3300 and the STC 3600 & 4600, and
   device entries for the IBM 3380 & 3390 and STC 3430, 3470, 3670, 4654,
   4655 & 4670. Also to remove support of the 191 & 400 disk drive. */
/* Modified 850812 by Paul Farley to change IBM3300 to IBM3880. */
(size,stringsize,stringrange,subscriptrange):
config_data_:
     procedure;


	declare c_ptr		 pointer;
	declare 1 c		 aligned based (c_ptr),
		2 chnl_cardx	 fixed binary,
		2 clok_cardx	 fixed binary,
		2 cpu_cardx	 fixed binary,
		2 fnp_cardx	 fixed binary,
		2 intk_cardx	 fixed binary,
		2 iom_cardx	 fixed binary,
		2 mem_cardx	 fixed binary,
		2 mpc_msp_cardx	 fixed binary,
		2 mpc_mtp_cardx	 fixed binary,
		2 ipc_fips_cardx	 fixed binary,
		2 mpc_urp_cardx	 fixed binary,
		2 mpc_cardx	 fixed binary,
		2 mpcs_msp_cardx	 fixed binary,
		2 mpcs_mtp_cardx	 fixed binary,
		2 mpcs_urp_cardx	 fixed binary,
		2 mpcs_cardx	 fixed binary,
		2 parm_cardx	 fixed binary,
		2 part_cardx	 fixed binary,
		2 prph_dsk_cardx	 fixed binary,
		2 prph_prt_cardx	 fixed binary,
		2 prph_rdr_cardx	 fixed binary,
		2 prph_pun_cardx	 fixed binary,
		2 prph_ccu_cardx	 fixed binary,
		2 prph_tap_cardx	 fixed binary,
		2 prph_opc_cardx	 fixed binary,
		2 prph_cardx	 fixed binary,
		2 root_cardx	 fixed binary,
		2 salv_cardx	 fixed binary,
		2 schd_cardx	 fixed binary,
		2 sst_cardx	 fixed binary,
		2 stok_cardx	 fixed binary,
		2 tbls_cardx	 fixed binary,
		2 udsk_cardx	 fixed binary,
/***** grand data base */
		2 config_cards
                         like config_data_$config_cards aligned,
		2 mpc_msp_model_names
                         like config_data_$mpc_msp_model_names aligned,
		2 mpc_mtp_model_names
		     like config_data_$mpc_mtp_model_names aligned,
		2 mpc_urp_model_names
		      like config_data_$mpc_urp_model_names aligned,
		2 ipc_msp_model_names
		      like config_data_$ipc_msp_model_names aligned,
		2 ipc_mtp_model_names
		      like config_data_$ipc_mtp_model_names aligned,
		2 disk_drive_model_names
		      like config_data_$disk_drive_model_names aligned,
		2 tape_drive_model_names
		      like config_data_$tape_drive_model_names aligned,
		2 printer_model_names
		      like config_data_$printer_model_names aligned,
		2 reader_model_names
		      like config_data_$reader_model_names aligned,
		2 ccu_model_names
		      like config_data_$ccu_model_names aligned,
		2 punch_model_names 
		      like config_data_$punch_model_names aligned,
		2 console_model_names
		      like config_data_$console_model_names aligned;


	declare (get_temp_segment_, release_temp_segment_)
				 entry (char (*), ptr, fixed bin (35));
	declare com_err_		 entry () options (variable);
	declare create_data_segment_	 entry (ptr, fixed bin (35));
	declare code		 fixed bin (35);
	declare (ccx, m)		 fixed bin;
	declare PADSTAR		 (1) char (32) init ("pad*") int static options (constant);

dcl (addr, char, hbound, ltrim, null, size, string, unspec) builtin;

dcl cleanup condition;

%include cds_args;
%include config_data_dcls;
	declare 1 CDSA		 aligned like cds_args;


	c_ptr = null ();

	on cleanup begin;
             goto clean_up;
	end;

	call get_temp_segment_ ("config_data_", c_ptr, code);
	if code ^= 0
	then call com_err_ (code, "config_data_", "Could not get a temp segment.");
	unspec (c) = ""b;

	c.config_cards.count = hbound (c.config_cards.per_card,1);
	c.chnl_cardx = 1;

	c.per_card.second_field = "";
	string (c.per_card.flags) = ""b;
	ccx = 1;

	c.per_card (ccx).name = "chnl";

	ccx = ccx + 1;
	c.clok_cardx = ccx;
	c.per_card (ccx).name = "clok";

	ccx = ccx + 1;
	c.cpu_cardx = ccx;
	c.per_card (ccx).name = "cpu";

	ccx = ccx + 1;
	c.fnp_cardx = ccx;
	c.per_card (ccx).name = "fnp";

	ccx = ccx + 1;
	c.intk_cardx = ccx;
	c.per_card (ccx).name = "intk";

	ccx = ccx + 1;
	c.iom_cardx = ccx;
	c.per_card (ccx).name = "iom";

	ccx = ccx + 1;
	c.mem_cardx = ccx;
	c.per_card (ccx).name = "mem";

	ccx = ccx + 1;
	c.mpc_msp_cardx = ccx;
	c.per_card (ccx).name = "mpc";
	c.per_card (ccx).second_field = "msp";
	c.per_card (ccx).match_second, c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.mpc_mtp_cardx = ccx;
	c.per_card (ccx).name = "mpc";
	c.per_card (ccx).second_field = "mtp";
	c.per_card (ccx).match_second, c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.ipc_fips_cardx = ccx;
	c.per_card (ccx).name = "ipc";
	c.per_card (ccx).second_field = "fips";
	c.per_card (ccx).match_second = "1"b;

	ccx = ccx + 1;
	c.mpc_urp_cardx = ccx;
	c.per_card (ccx).name = "mpc";
	c.per_card (ccx).second_field = "urp";
	c.per_card (ccx).match_second, c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.mpc_cardx = ccx;
	c.per_card (ccx).name = "mpc";

	ccx = ccx + 1;
	c.mpcs_msp_cardx = ccx;
	c.per_card (ccx).name = "mpcs";
	c.per_card (ccx).second_field = "msp";
	c.per_card (ccx).match_second, c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.mpcs_mtp_cardx = ccx;
	c.per_card (ccx).name = "mpcs";
	c.per_card (ccx).second_field = "mtp";
	c.per_card (ccx).match_second, c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.mpcs_urp_cardx = ccx;
	c.per_card (ccx).name = "mpcs";
	c.per_card (ccx).second_field = "urp";
	c.per_card (ccx).match_second, c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.mpcs_cardx = ccx;
	c.per_card (ccx).name = "mpcs";

	ccx = ccx + 1;
	c.parm_cardx = ccx;
	c.per_card (ccx).name = "parm";

	ccx = ccx + 1;
	c.prph_dsk_cardx = ccx;
	c.per_card (ccx).name = "prph";
	c.per_card (ccx).second_field = "dsk";
	c.per_card (ccx).match_second = "1"b;
	c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.prph_prt_cardx = ccx;
	c.per_card (ccx).name = "prph";
	c.per_card (ccx).second_field = "prt";
	c.per_card (ccx).match_second = "1"b;
	c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.prph_rdr_cardx = ccx;
	c.per_card (ccx).name = "prph";
	c.per_card (ccx).second_field = "rdr";
	c.per_card (ccx).match_second = "1"b;
	c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.prph_pun_cardx = ccx;
	c.per_card (ccx).name = "prph";
	c.per_card (ccx).second_field = "pun";
	c.per_card (ccx).match_second = "1"b;
	c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.prph_ccu_cardx = ccx;
	c.per_card (ccx).name = "prph";
	c.per_card (ccx).second_field = "ccu";
	c.per_card (ccx).match_second = "1"b;
	c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.prph_tap_cardx = ccx;
	c.per_card (ccx).name = "prph";
	c.per_card (ccx).second_field = "tap";
	c.per_card (ccx).match_second = "1"b;
	c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.prph_opc_cardx = ccx;
	c.per_card (ccx).name = "prph";
	c.per_card (ccx).second_field = "opc";
	c.per_card (ccx).match_second = "1"b;
	c.per_card (ccx).match_only_3 = "1"b;

	ccx = ccx + 1;
	c.prph_cardx = ccx;
	c.per_card (ccx).name = "prph";

	ccx = ccx + 1;
	c.root_cardx = ccx;
	c.per_card (ccx).name = "root";

	ccx = ccx + 1;
	c.salv_cardx = ccx;
	c.per_card (ccx).name = "salv";

	ccx = ccx + 1;
	c.schd_cardx = ccx;
	c.per_card (ccx).name = "schd";

	ccx = ccx + 1;
	c.sst_cardx = ccx;
	c.per_card (ccx).name = "sst";

	ccx = ccx + 1;
	c.stok_cardx = ccx;
	c.per_card (ccx).name = "stok";

	ccx = ccx + 1;
	c.tbls_cardx = ccx;
	c.per_card (ccx).name = "tcd";

	ccx = ccx + 1;
	c.udsk_cardx = ccx;
	c.per_card (ccx).name = "udsk";

	ccx = 1;

	c.mpc_msp_model_names.count = hbound (c.mpc_msp_model_names.names,1);

	c.mpc_msp_model_names (ccx).model = 451;
	c.mpc_msp_model_names (ccx).name = "dsc0451";
	c.mpc_msp_model_names (ccx).fw_tag = "dsc191.m191";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;

	ccx = ccx + 1;
	c.mpc_msp_model_names (ccx).model = 451;
	c.mpc_msp_model_names (ccx).name = "msp0451";
	c.mpc_msp_model_names (ccx).fw_tag = "dsc191.m191";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;

	ccx = ccx + 1;
	c.mpc_msp_model_names (ccx).model = 601;
	c.mpc_msp_model_names (ccx).name = "msp0601";
	c.mpc_msp_model_names (ccx).fw_tag = "dsc500.d500";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;
	c.mpc_msp_model_names (ccx).valid_drives (3) = 500;
	c.mpc_msp_model_names (ccx).valid_drives (4) = 501;

	ccx = ccx + 1;
	c.mpc_msp_model_names (ccx).model = 603;
	c.mpc_msp_model_names (ccx).name = "msp0603";
	c.mpc_msp_model_names (ccx).fw_tag = "dsc500.d500";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;
	c.mpc_msp_model_names (ccx).valid_drives (3) = 500;
	c.mpc_msp_model_names (ccx).valid_drives (4) = 501;

	ccx = ccx + 1;
	c.mpc_msp_model_names (ccx).model = 607;
	c.mpc_msp_model_names (ccx).name = "msp0607";
	c.mpc_msp_model_names (ccx).fw_tag = "dsc500.d500";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;
	c.mpc_msp_model_names (ccx).valid_drives (3) = 500;
	c.mpc_msp_model_names (ccx).valid_drives (4) = 501;

	ccx = ccx + 1;
	c.mpc_msp_model_names (ccx).model = 609;
	c.mpc_msp_model_names (ccx).name = "msp0609";
	c.mpc_msp_model_names (ccx).fw_tag = "dsc500.d500";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;
	c.mpc_msp_model_names (ccx).valid_drives (3) = 500;
	c.mpc_msp_model_names (ccx).valid_drives (4) = 501;

	ccx = ccx + 1;
	c.mpc_msp_model_names (ccx).model = 611;
	c.mpc_msp_model_names (ccx).name = "msp0611";
	c.mpc_msp_model_names (ccx).fw_tag = "dsc500.d500";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;
	c.mpc_msp_model_names (ccx).valid_drives (3) = 500;
	c.mpc_msp_model_names (ccx).valid_drives (4) = 501;

	ccx = ccx + 1;
	c.mpc_msp_model_names (ccx).model = 612;
	c.mpc_msp_model_names (ccx).name = "msp0612";
	c.mpc_msp_model_names (ccx).fw_tag = "dsc500.d500";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;
	c.mpc_msp_model_names (ccx).valid_drives (3) = 500;
	c.mpc_msp_model_names (ccx).valid_drives (4) = 501;

	ccx = ccx + 1;
	c.mpc_msp_model_names (ccx).model = 800;
	c.mpc_msp_model_names (ccx).name = "msp800";
	c.mpc_msp_model_names (ccx).fw_tag = "msp800.msp8";
	c.mpc_msp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_msp_model_names (ccx).valid_drives (1) = 450;
	c.mpc_msp_model_names (ccx).valid_drives (2) = 451;
	c.mpc_msp_model_names (ccx).valid_drives (3) = 500;
	c.mpc_msp_model_names (ccx).valid_drives (4) = 501;

	c.mpc_mtp_model_names.count = hbound (c.mpc_mtp_model_names.names,1);
	ccx = 1;
	c.mpc_mtp_model_names (ccx).model = 501;
	c.mpc_mtp_model_names (ccx).name = "mtc501";
	c.mpc_mtp_model_names (ccx).fw_tag = "mtc500.m500";
	c.mpc_mtp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_mtp_model_names (ccx).valid_drives (1) = 500;
	c.mpc_mtp_model_names (ccx).valid_drives (2) = 507;

	ccx = ccx + 1;
	c.mpc_mtp_model_names (ccx).model = 502;
	c.mpc_mtp_model_names (ccx).name = "mtc502";
	c.mpc_mtp_model_names (ccx).fw_tag = "mtc500.m500";
	c.mpc_mtp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_mtp_model_names (ccx).valid_drives (1) = 500;
	c.mpc_mtp_model_names (ccx).valid_drives (2) = 507;

	ccx = ccx + 1;
	c.mpc_mtp_model_names (ccx).model = 600;
	c.mpc_mtp_model_names (ccx).name = "mtp0600";
	c.mpc_mtp_model_names (ccx).fw_tag = "mtp601.m601";
	c.mpc_mtp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_mtp_model_names (ccx).valid_drives (1) = 500;
	c.mpc_mtp_model_names (ccx).valid_drives (2) = 507;
	c.mpc_mtp_model_names (ccx).valid_drives (3) = 600;
	c.mpc_mtp_model_names (ccx).valid_drives (4) = 601;
	c.mpc_mtp_model_names (ccx).valid_drives (5) = 602;


	ccx = ccx + 1;
	c.mpc_mtp_model_names (ccx).model = 601;
	c.mpc_mtp_model_names (ccx).name = "mtp0601";
	c.mpc_mtp_model_names (ccx).fw_tag = "mtp601.m601";
	c.mpc_mtp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_mtp_model_names (ccx).valid_drives (1) = 500;
	c.mpc_mtp_model_names (ccx).valid_drives (2) = 507;
	c.mpc_mtp_model_names (ccx).valid_drives (3) = 600;
	c.mpc_mtp_model_names (ccx).valid_drives (4) = 601;
	c.mpc_mtp_model_names (ccx).valid_drives (5) = 602;


	ccx = ccx + 1;
	c.mpc_mtp_model_names (ccx).model = 602;
	c.mpc_mtp_model_names (ccx).name = "mtc0602";
	c.mpc_mtp_model_names (ccx).fw_tag = "mtp601.m601";
	c.mpc_mtp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_mtp_model_names (ccx).valid_drives (1) = 500;
	c.mpc_mtp_model_names (ccx).valid_drives (2) = 507;
	c.mpc_mtp_model_names (ccx).valid_drives (3) = 600;
	c.mpc_mtp_model_names (ccx).valid_drives (4) = 601;
	c.mpc_mtp_model_names (ccx).valid_drives (5) = 602;


	ccx = ccx + 1;
	c.mpc_mtp_model_names (ccx).model = 610;
	c.mpc_mtp_model_names (ccx).name = "mtp0610";
	c.mpc_mtp_model_names (ccx).fw_tag = "mtp610.m610";
	c.mpc_mtp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_mtp_model_names (ccx).valid_drives (1) = 500;
	c.mpc_mtp_model_names (ccx).valid_drives (2) = 507;
	c.mpc_mtp_model_names (ccx).valid_drives (3) = 600;
	c.mpc_mtp_model_names (ccx).valid_drives (4) = 601;
	c.mpc_mtp_model_names (ccx).valid_drives (5) = 602;
	c.mpc_mtp_model_names (ccx).valid_drives (6) = 610;
	c.mpc_mtp_model_names (ccx).valid_drives (7) = 630;

	ccx = ccx + 1;
	c.mpc_mtp_model_names (ccx).model = 611;
	c.mpc_mtp_model_names (ccx).name = "mtp0611";
	c.mpc_mtp_model_names (ccx).fw_tag = "mtp610.m610";
	c.mpc_mtp_model_names (ccx).valid_drives (*) = 0;
	c.mpc_mtp_model_names (ccx).valid_drives (1) = 500;
	c.mpc_mtp_model_names (ccx).valid_drives (2) = 507;
	c.mpc_mtp_model_names (ccx).valid_drives (3) = 600;
	c.mpc_mtp_model_names (ccx).valid_drives (4) = 601;
	c.mpc_mtp_model_names (ccx).valid_drives (5) = 602;
	c.mpc_mtp_model_names (ccx).valid_drives (6) = 610;
	c.mpc_mtp_model_names (ccx).valid_drives (7) = 630;

	ccx = 1;

	c.ipc_msp_model_names.count = 1;

	c.ipc_msp_model_names (ccx).model = -1;
	c.ipc_msp_model_names (ccx).name = "fips-ipc";
	c.ipc_msp_model_names (ccx).fw_tag = "";
	c.ipc_msp_model_names (ccx).valid_drives (*) = 0;
	c.ipc_msp_model_names (ccx).valid_drives (1) = 3380;
	c.ipc_msp_model_names (ccx).valid_drives (2) = 3381;

	c.ipc_mtp_model_names.count = 1;
	ccx = 1;
	c.ipc_mtp_model_names (ccx).model = -1;
	c.ipc_mtp_model_names (ccx).name = "fips-ipc";
	c.ipc_mtp_model_names (ccx).fw_tag = "";
	c.ipc_mtp_model_names (ccx).valid_drives (*) = 0;
	c.ipc_mtp_model_names (ccx).valid_drives (1) = 8200;


	c.mpc_urp_model_names.count = hbound (c.mpc_urp_model_names.names,1);
	c.mpc_urp_model_names.model (1) = 2;
	c.mpc_urp_model_names.name (1) = "urc002";
	c.mpc_urp_model_names.fw_tag (1) = "urcmpc.ucmn";

	c.mpc_urp_model_names.model (2) = 600;
	c.mpc_urp_model_names.name (2) = "urp0600";
	c.mpc_urp_model_names.fw_tag (2) = "urcmpc.ucmn";

	ccx = 2;
	do m = 8001 to 8004;
	     ccx = ccx + 1;
	     c.mpc_urp_model_names.model (ccx) = m;
	     c.mpc_urp_model_names.name (ccx) = "urp" || ltrim (char (m));
	     c.mpc_urp_model_names.fw_tag (ccx) = "EURC";
	end;

	c.disk_drive_model_names.count = hbound (c.disk_drive_model_names.names,1);
	ccx = 1;
	c.disk_drive_model_names.model (ccx) = 451;
	c.disk_drive_model_names.name (ccx) = "msu0451";
	c.disk_drive_model_names.device_0_valid (ccx) = "0"b;
	ccx = ccx + 1;
	c.disk_drive_model_names.model (ccx) = 500;
	c.disk_drive_model_names.name (ccx) = "msu0500";
	c.disk_drive_model_names.device_0_valid (ccx) = "0"b;
	ccx = ccx + 1;
	c.disk_drive_model_names.model (ccx) = 501;
	c.disk_drive_model_names.name (ccx) = "msu0501";
	c.disk_drive_model_names.device_0_valid (ccx) = "0"b;
	ccx = ccx + 1;
	c.disk_drive_model_names.model (ccx) = 3380;
	c.disk_drive_model_names.name (ccx) = "msu3380";
	c.disk_drive_model_names.device_0_valid (ccx) = "1"b;
	ccx = ccx + 1;
	c.disk_drive_model_names.model (ccx) = 3381;
	c.disk_drive_model_names.name (ccx) = "msu3381";
	c.disk_drive_model_names.device_0_valid (ccx) = "1"b;

	ccx = 1;
	c.tape_drive_model_names.count = hbound (c.tape_drive_model_names.names,1);
	c.tape_drive_model_names.model (ccx) = 500;
	c.tape_drive_model_names.name (ccx) = "mtc501";
	c.tape_drive_model_names.device_0_valid (ccx) = "0"b;

	ccx = ccx + 1;
	c.tape_drive_model_names.model (ccx) = 500;
	c.tape_drive_model_names.name (ccx) = "mtc502";
	c.tape_drive_model_names.device_0_valid (ccx) = "0"b;

	ccx = ccx + 1;
	c.tape_drive_model_names.model (ccx) = 507;
	c.tape_drive_model_names.name (ccx) = "mtc502";
	c.tape_drive_model_names.device_0_valid (ccx) = "0"b;

	ccx = ccx + 1;
	c.tape_drive_model_names.model (ccx) = 600;
	c.tape_drive_model_names.name (ccx) = "mtp0600";
	c.tape_drive_model_names.device_0_valid (ccx) = "0"b;

	ccx = ccx + 1;
	c.tape_drive_model_names.model (ccx) = 601;
	c.tape_drive_model_names.name (ccx) = "mtp0601";
	c.tape_drive_model_names.device_0_valid (ccx) = "0"b;

	ccx = ccx + 1;
	c.tape_drive_model_names.model (ccx) = 602;
	c.tape_drive_model_names.name (ccx) = "mtp0602";
	c.tape_drive_model_names.device_0_valid (ccx) = "0"b;

	ccx = ccx + 1;
	c.tape_drive_model_names.model (ccx) = 610;
	c.tape_drive_model_names.name (ccx) = "mtp0610";
	c.tape_drive_model_names.device_0_valid (ccx) = "0"b;

	ccx = ccx + 1;
	c.tape_drive_model_names.model (ccx) = 630;
	c.tape_drive_model_names.name (ccx) = "mtp0630";
	c.tape_drive_model_names.device_0_valid (ccx) = "0"b;

	ccx = ccx + 1;
	c.tape_drive_model_names.model (ccx) = 8200;
	c.tape_drive_model_names.name (ccx) = "mtu8200";
	c.tape_drive_model_names.device_0_valid (ccx) = "1"b;

	ccx = 1;
	c.printer_model_names.count = hbound (c.printer_model_names.names,1);
	c.printer_model_names.model (ccx) = 301;
	c.printer_model_names.name (ccx) = "prt301";
	ccx = ccx + 1;
	c.printer_model_names.model (ccx) = 1000;
	c.printer_model_names.name (ccx) = "pru1000";
	ccx = ccx + 1;
	c.printer_model_names.model (ccx) = 1200;
	c.printer_model_names.name (ccx) = "pru1200";
	ccx = ccx + 1;
	c.printer_model_names.model (ccx) = 1600;
	c.printer_model_names.name (ccx) = "pru1600";
	ccx = ccx + 1;
	c.printer_model_names.model (ccx) = 901;
	c.printer_model_names.name (ccx) = "pru0901";

	c.reader_model_names.count = hbound (c.reader_model_names.names,1);
	ccx = 1;
	c.reader_model_names.model (ccx) = 500;
	c.reader_model_names.name (ccx) = "cru0500";
	ccx = ccx + 1;
	c.reader_model_names.model (ccx) = 501;
	c.reader_model_names.name (ccx) = "cru0501";
	ccx = ccx + 1;
	c.reader_model_names.model (ccx) = 301;
	c.reader_model_names.name (ccx) = "crz301";
	ccx = ccx + 1;
	c.reader_model_names.model (ccx) = 201;
	c.reader_model_names.name (ccx) = "crz201";

	c.ccu_model_names.count = hbound (c.ccu_model_names.names,1);
	c.ccu_model_names.model (1) = 401;
	c.ccu_model_names.name (1) = "ccu401";

	c.punch_model_names.count = hbound (c.punch_model_names.names,1);
	ccx = 1;
	c.punch_model_names.model (ccx) = 301;
	c.punch_model_names.name (ccx) = "cpz301";
	ccx = ccx + 1;
	c.punch_model_names.model (ccx) = 300;
	c.punch_model_names.name (ccx) = "cpz300";
	ccx = ccx + 1;
	c.punch_model_names.model (ccx) = 201;
	c.punch_model_names.name (ccx) = "cpz201";

	c.console_model_names.count = hbound (c.console_model_names.names,1);
	ccx = 1;

	c.console_model_names.model (ccx) = 6001;
	c.console_model_names.name (ccx) = "csu6001";
	c.console_model_names.io_type (ccx) = "pcw";
	ccx = ccx + 1;
	
	c.console_model_names.model (ccx) = 6004;
	c.console_model_names.name (ccx) = "csu6004";
	c.console_model_names.io_type (ccx) = "pcw";
	ccx = ccx + 1;
	
	c.console_model_names.model (ccx) = 6601;
	c.console_model_names.name (ccx) = "csu6601";
	c.console_model_names.io_type (ccx) = "idcw";

	unspec (CDSA) = ""b;
	CDSA.sections (1).p = addr (c);
	CDSA.sections (1).len = size (c);
	CDSA.sections (2).p = null ();
	CDSA.sections (2).len = 0;
	CDSA.sections (1).struct_name = "c";
	CDSA.num_exclude_names = 1;
	CDSA.exclude_array_ptr = addr (PADSTAR);
	CDSA.have_text = "1"b;
	CDSA.seg_name = "config_data_";

	call create_data_segment_ (addr (CDSA), code);
	if code ^= 0
	then call com_err_ (code, "config_data_");

clean_up:
	if c_ptr ^= null () then call release_temp_segment_  ("config_data_", c_ptr, code);
	return;


	end config_data_;
