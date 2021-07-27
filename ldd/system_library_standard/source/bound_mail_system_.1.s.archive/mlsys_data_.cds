/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   *********************************************************** */



/* HISTORY COMMENTS:
  1) change(86-06-11,Mills), approve(86-06-11,MCR7419),
     audit(86-06-17,Margolin), install(86-06-30,MR12.0-1080):
     Adding initializations for mlsys_data_$domains_available = 0;
                                                   END HISTORY COMMENTS */


/* format: off */

/* Constants and static data used by the Multics mail system:  See mlsys_data.incl.pl1 and mlsys_internal_data.incl.pl1
   for an explanation of the meaning/use of these values */

/* Created:  May 1981 by G. Palter */
/* Modified: June 1983 by G. Palter to merge with mlsys_internal_data_ and for multi-ring mail system */

/* format: on,style4,delnl,insnl,ifthenstmt,ifthen */


mlsys_data_:
     procedure () options (variable);


dcl  1 mlsys_constants aligned,                             /* the constant data */
       2 max_opening_retries fixed binary,
       2 max_lock_wait_retries fixed binary,
       2 mailbox_allocation fixed binary,
       2 message_body_sections_allocation fixed binary,
       2 message_redistributions_list_allocation fixed binary,
       2 message_user_fields_allocation fixed binary,
       2 message_references_list_allocation fixed binary,
       2 address_list_allocation fixed binary,
       2 mailbox_link_directory character (168) unaligned,
       2 mailer_directory character (168) unaligned,
       2 system_directory character (168) unaligned;

dcl  1 mlsys_static aligned,                                /* per-process constant (ie: static) data */
       2 forum_not_available fixed binary (1),
       2 ism_not_available fixed binary (1),
       2 subsystem_ring fixed binary (3),
       2 highest_usable_ring fixed binary (3),
       2 lowest_forum_ring fixed binary (3),
       2 temp_segment_list_ptr pointer,
       2 valid_segments,                                    /* ... insures that it's word aligned */
         3 bits (0:4095) bit (1) unaligned,
       2 subsystem_area_ptr pointer,
       2 hash_tables_segment_ptr pointer,
       2 transmit_cache_ptr pointer,
       2 user_is_anonymous bit (1) aligned,
       2 person_id character (24) varying,
       2 project_id character (12) varying,
       2 user_id character (32) varying,
       2 user_default_mailbox_address pointer,
       2 user_mail_table_address pointer,
       2 user_logbox_address pointer,
       2 domains_available;


dcl  1 cds_data aligned like cds_args;                      /* arguments to create_data_segment_ subr */

dcl  code fixed binary (35);

dcl  MLSYS_DATA_ character (32) static options (constant) initial ("mlsys_data_");

dcl  create_data_segment_ entry (pointer, fixed binary (35));
dcl  com_err_ entry () options (variable);

dcl  (addr, currentsize, null, string) builtin;
%page;
/* Define the constants */

          mlsys_constants.max_opening_retries = 5;          /* retry salvaged openings five times before punting */

          mlsys_constants.max_lock_wait_retries = 20;       /* retry message transmission to locked mailbox 20 times */

          mlsys_constants.mailbox_allocation = 32;          /* see the explanation in mlsys_internal_data.incl.pl1 ... */
          mlsys_constants.message_body_sections_allocation = 1;
          mlsys_constants.message_redistributions_list_allocation = 4;
          mlsys_constants.message_user_fields_allocation = 4;
          mlsys_constants.message_references_list_allocation = 2;
          mlsys_constants.address_list_allocation = 4;

          mlsys_constants.mailbox_link_directory = ">udd>Daemon>mailboxes";
          mlsys_constants.mailer_directory = ">udd>Daemon>Network_Server";

          mlsys_constants.system_directory = ">site>mail_system_dir";
                                                            /* directory containing per-system data (mail table/queues) */


/* Initialize static data to known values */

          mlsys_static.forum_not_available = 0;             /* 1 => forum isn't available on the system or in this ring */
          mlsys_static.ism_not_available = 0;               /* 1 => no inter-system mailer on this system */
          mlsys_static.domains_available = 0;               /* 1 => domains software on this system */
          mlsys_static.subsystem_ring = -1;                 /* ring in which the mail system is secured */
          mlsys_static.highest_usable_ring = -1;            /* highest ring of execution which may access the mail system */
          mlsys_static.lowest_forum_ring = -1;              /* lowest ring of execution with access to forum */

          mlsys_static.temp_segment_list_ptr = null ();     /* -> list of all mail system temporary segments */

          string (mlsys_static.valid_segments.bits) = ""b;  /* which segments contain data allocated by the mail system */
          mlsys_static.subsystem_area_ptr = null ();        /* -> area used by the mail system for all allocations */

          mlsys_static.hash_tables_segment_ptr = null ();   /* -> hash tables used by the mail system */

          mlsys_static.transmit_cache_ptr = null ();        /* -> cache of recently used mailboxes for mlsys_transmit_ */

          mlsys_static.user_is_anonymous = "0"b;            /* whether or not the user is an anonymous */
          mlsys_static.person_id = "";                      /* the user's Person_id, Project_id, and User_id */
          mlsys_static.project_id = "";
          mlsys_static.user_id = "";

          mlsys_static.user_default_mailbox_address,        /* -> user's default mailbox address */
               mlsys_static.user_mail_table_address,        /* -> user's mail table address */
               mlsys_static.user_logbox_address = null ();  /* -> address of the user's logbox */


/* Set up arguments for call to create_data_segment_ */

          cds_data.sections (1).p = addr (mlsys_constants);
          cds_data.sections (1).len = currentsize (mlsys_constants);
          cds_data.sections (1).struct_name = "mlsys_constants";

          cds_data.sections (2).p = addr (mlsys_static);
          cds_data.sections (2).len = currentsize (mlsys_static);
          cds_data.sections (2).struct_name = "mlsys_static";

          cds_data.seg_name = MLSYS_DATA_;

          cds_data.num_exclude_names = 0;
          cds_data.exclude_array_ptr = null ();

          string (cds_data.switches) = "0"b;
          cds_data.switches.have_text = "1"b;               /* have constants ... */
          cds_data.switches.have_static = "1"b;             /* ... and static data */
          cds_data.switches.separate_static = "1"b;         /* in case we someday are prelinked */


/* Call create_data_segment_ */

          call create_data_segment_ (addr (cds_data), code);

          if code ^= 0 then call com_err_ (code, MLSYS_DATA_);

          return;
%page;
%include cds_args;

     end mlsys_data_;
