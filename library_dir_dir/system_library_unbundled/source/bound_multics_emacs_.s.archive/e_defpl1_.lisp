;;; ***********************************************************
;;; *                                                         *
;;; * Copyright, (C) Honeywell Bull Inc., 1988                *
;;; *                                                         *
;;; * Copyright, (C) Honeywell Information Systems Inc., 1982 *
;;; *                                                         *
;;; * Copyright (c) 1978 by Massachusetts Institute of        *
;;; * Technology and Honeywell Information Systems, Inc.      *
;;; *                                                         *
;;; ***********************************************************

;;;
;;;
;;;	PL/1 Entries Used by EMACS
;;;


;;; HISTORY COMMENTS:
;;;  1) change(84-01-23,Margolin), approve(), audit(), install():
;;;     Pre-hcom journalization:
;;;     Modified: 26 November 1983 B. Margolin to remove network entrypoints.
;;;     Modified: 23 January 1984 B. Margolin to add terminate_file_.
;;;  2) change(84-12-25,Margolin), approve(86-02-24,MCR7186),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     add e_pl1_$object_check and iox_$control.
;;;  3) change(86-02-24,Margolin), approve(86-02-24,MCR7325),
;;;     audit(86-08-12,Harvey), install(86-08-20,MR12.0-1136):
;;;     Changed default input base to 10.  Added new e_multics_files_
;;;     entrypoints, corrected declaration of expand_pathname_$component,
;;;     added hcs_$get_max_length_seg, hcs_$get_uid_file,
;;;     initiate_file_$component, mlr_, several msf_manager_ entrypoints,
;;;     pathname_ and pathname_$component.
;;;  4) change(86-03-12,LJAdams), approve(86-03-12,MCR7361),
;;;     audit(86-04-17,Margolin), install(86-08-20,MR12.0-1136):
;;;     Added requote_string_.
;;;  5) change(88-01-05,Schroth), approve(88-02-29,MCR7851),
;;;     audit(88-06-08,RBarstad), install(88-08-01,MR12.2-1071):
;;;     For 8-bit I/O:  added e_pl1_$set_extended_ascii, and
;;;     e_pl1_$get_output_conv_table and changed e_pl1_$set_break_sequence.
;;;     Removed e_pl1_$get_network_flag, e_pl1_$set_dbo_sw,
;;;     e_pl1_$set_ignore_lf, and e_tasking_$get_death_flag: unused and
;;;     undocumented (also avoids lcp link limit).
;;;  6) change(89-02-08,Flegel), approve(89-02-28,MCR8065),
;;;     audit(89-03-06,Lee), install(89-04-24,MR12.3-1035):
;;;     phx21199 - added cv_dec_check_ defpl1
;;;                                                      END HISTORY COMMENTS



(eval-when (compile)
	 (setq ibase 10.))			;so numbers read in decimal

(declare

;;;	Entries in e_pl1_

(defpl1 e_pl1_$get_terminal_type "" (return char (256.) varying))

(defpl1 e_pl1_$get_real_terminal_type "" (return char (32.) varying))

(defpl1 e_pl1_$get_editing_chars "" (return char (1)) (return char (1))
        (return char (1)))

(defpl1 e_pl1_$get_iocb "" (return pointer))

(defpl1 e_pl1_$set_terminal_type "" (char (*)))

(defpl1 e_pl1_$set_line_speed_ "" (fixed bin))

(defpl1 e_pl1_$init "")

(defpl1 e_pl1_$get_char "" (return fixed bin))

(defpl1 e_pl1_$echo_negotiate_get_char "" (lisp)(lisp)(fixed bin)
        (return fixed bin))

(defpl1 e_pl1_$set_break_char "" (fixed bin)(fixed bin))

(defpl1 e_pl1_$set_break_sequence "" (fixed bin (32.)) (fixed bin (32.))
        (fixed bin (32.)) (fixed bin (32.)) (fixed bin (32.)) (fixed bin (32.))
        (fixed bin (32.)) (fixed bin (32.)))

(defpl1 e_pl1_$real_have_chars "" (return fixed bin))

(defpl1 Rtyo "e_pl1_$tyo" (fixed bin))

(defpl1 Rprinc "e_pl1_$princ" (char (*)))

(defpl1 e_pl1_$dump_output_buffer "")

(defpl1 e_pl1_$get_line_speed "" (return fixed bin))

(defpl1 e_pl1_$resetwrite "")

(defpl1 e_pl1_$will_supdup_output "" (return fixed bin (1)))

(defpl1 e_pl1_$set_line_speed_ "" (fixed bin))

(defpl1 e_pl1_$set_emacs_tty_modes "")

(defpl1 e_pl1_$set_multics_tty_modes "")

(defpl1 e_cline_ "e_pl1_$cline_executor" (char (*)))

(defpl1 e_pl1_$get_emacs_interrupt_array "" (return ptr))

(defpl1 e_pl1_$assign_channel "" (fixed binary) (return fixed binary))

(defpl1 e_pl1_$get_emacs_interrupt "" (return fixed bin)(return fixed bin))

(defpl1 e_pl1_$object_check "" (fixed bin (24.)) (pointer) (return fixed bin))

(defpl1 e_pl1_$set_extended_ascii "" (fixed bin (1)))

(defpl1 e_pl1_$get_output_conv_table "" (array (64.) fixed bin (35.)))

;;; Entries in e_terminal_io_

(defpl1 e_terminal_io_$check_printing "" (char (*)) (return fixed bin))

;;; Entries in e_info_vfilesman_

(defpl1 e_info_vfilesman_$open "" (char (*))(fixed bin (1))
        (return fixed bin (35.)))

(defpl1 e_info_vfilesman_$seek "" (char (*))(return fixed bin (35.)))

(defpl1 e_info_vfilesman_$get_recp "" (return ptr)(return fixed bin (21.))
        (return fixed bin (35.)))

(defpl1 e_info_vfilesman_$update "" (char (*))(fixed bin (1))
        (return fixed bin (35.)))

(defpl1 e_info_vfilesman_$close "")

;;;	Entries in multics_emacs

(defpl1 emacs$set_emacs_return_code "multics_emacs$set_emacs_return_code"
        (fixed bin (35.)))

(defpl1 emacs$get_my_name "multics_emacs$get_my_name" (return char (32.)))

(defpl1 emacs$get_version "" (return char (10.)))

(defpl1 emacs$get_info_ptr "" (return pointer))

(defpl1 e$get_temporary_seg "multics_emacs$get_temporary_seg" (return ptr))

(defpl1 e$release_temporary_seg "multics_emacs$release_temporary_seg" (ptr))

(defpl1 emacs$set_lisp_rdis_meters "multics_emacs$set_lisp_rdis_meters"
        (fixed bin) (fixed bin) (fixed bin) (fixed bin) (fixed bin) 
        (fixed bin) (fixed bin) (fixed bin) (fixed bin) (fixed bin))

;;; Entries in e_tasking_

(defpl1 e_tasking_$destroy_me "")

(defpl1 e_tasking_$get_tasking_flag "" (return fixed bin))

(defpl1 e_tasking_$quit "" (return fixed bin (35.)))

;;; Entries in e_multics_files_util_

(defpl1 e_multics_files_util_$force_access "" (char (*)) (return ptr)
        (return fixed bin (35.)))

(defpl1 e_multics_files_util_$force_msf_access "" (ptr) (return ptr)
        (return fixed bin (35.)))

(defpl1 e_multics_files_util_$get_dtcm "" (ptr)
        (return fixed bin (35)) (return fixed bin (35)))

(defpl1 e_multics_files_util_$get_dtcm_file "" (char (*)) (char (*))
        (return fixed bin (35)) (return fixed bin (35)))

(defpl1 e_multics_files_util_$nth_star_match "" (ptr) (fixed bin)
        (return char (32)) (return char (32)))

(defpl1 e_multics_files_util_$restore_access "" (ptr))

(defpl1 e_multics_files_util_$restore_msf_access "" (ptr) (ptr))

(defpl1 e_multics_files_util_$star_list_cleanup "" (ptr))

(defpl1 e_multics_files_util_$star_list_init "" (char (*)) (char (*))
        (char (*)) (return ptr) (return fixed bin) (return fixed bin (35)))

;;; Entries in e_argument_parse_

(defpl1 e_argument_parse_$get_ttp_info "" (return fixed bin)
        (return char (168.)))

(defpl1 e_argument_parse_$get_startup_info "" (return fixed bin)
        (return fixed bin) (return fixed bin) (return fixed bin)
        (return fixed bin) (return fixed bin) (return fixed bin))

(defpl1 e_argument_parse_$get_one_path "" (return char (168.))
        (return fixed bin))

(defpl1 e_argument_parse_$new_arguments "" (return fixed bin))

;;; Entry in emacs_search_file_caller_

(defpl1 emacs_search_file_caller_ "" (char (*)) (fixed bin) (ptr)
        (fixed bin) (char (*)) (return bit (36.)))

;;; Entries in list_emacs_ctls

(defpl1 list_emacs_ctls$list_emacs_ctls "" (char (*)))

(defpl1 list_emacs_ctls$find_ctl "" (char (*)) (return char (168.)))

;;;	Multics supplied routines

(defpl1 absolute_pathname_ "" (char (*)) (return char (168.))
        (return fixed bin (35.)))

(defpl1 absolute_pathname_$add_suffix "" (char (*)) (char (*))
        (return char (168.)) (return fixed bin (35.)))

(defpl1 archive_util_$disected_element "" (update ptr) (return ptr)
        (return char (32.)) (return fixed bin (24.))
        (return fixed bin (35.)))

(defpl1 archive_util_$first_disected "" (update ptr) (return ptr)
        (return char (32.)) (return fixed bin (24.))
        (return fixed bin (35.)))

(defpl1 check_entryname_ "" (char (*)) (return fixed bin (35.)))

(defpl1 check_star_name_$entry "" (char (*))(return fixed bin))

(defpl1 check_star_name_$path "" (char (*))(return fixed bin))

(defpl1 convert_status_code_ "" (fixed bin (35.))
        (return char (8.)) (return char (100.)))

(defpl1 delete_$path "" (char (*))(char (*))(bit (6))(char (*))
        (return fixed bin (35.)))

(defpl1 expand_pathname_ "" (char (*)) (return char (168.))
        (return char (32.)) (return fixed bin (35.)))

(defpl1 expand_pathname_$add_suffix "" (char (*)) (char (*))
        (return char (168.)) (return char (32.))
        (return fixed bin (35.)))

(defpl1 expand_pathname_$component "" (char (*))(return char (168.))
        (return char (32.))(return char (32.)) (return fixed bin (35)))

(defpl1 get_pdir_ "" (return char (168.)))

(defpl1 hcs_$get_max_length_seg "" (ptr) (return fixed bin (19))
        (return fixed bin (19)))

(defpl1 hcs_$fs_get_mode "" (ptr) (return fixed bin (5))
        (return fixed bin (35.)))

(defpl1 hcs_$get_uid_file "" (char (*)) (char (*)) (return fixed bin (35))
        (return fixed bin (35)))

(defpl1 hcs_$get_uid_seg "" (ptr) (return fixed bin (35.))
        (return fixed bin (35.)))

(defpl1 hcs_$initiate_count "" (char (*)) (char (*)) (char (*))
        (return fixed bin (24.)) (fixed bin (2)) (return ptr)
        (return fixed bin (35.)))

(defpl1 hcs_$make_ptr "" (ptr)(char (*))(char (*))(return ptr)
        (return fixed bin (35.)))

(defpl1 hcs_$make_seg "" (char (*)) (char (*)) (char (*))
        (fixed bin (5)) (return ptr) (return fixed bin (35.)))

(defpl1 hcs_$set_bc "" (char (*))(char (*))(fixed bin)(return fixed bin (35.)))

(defpl1 hcs_$set_bc_seg "" (ptr) (fixed bin (24.)) (return fixed bin (35.)))

(defpl1 hcs_$terminate_noname "" (ptr) (return fixed bin (35.)))

(defpl1 hcs_$truncate_file "" (char (*))(char (*))(fixed bin)
        (return fixed bin (35.)))

(defpl1 hcs_$truncate_seg "" (ptr) (fixed bin (18.)) (return fixed bin (35.)))

(defpl1 initiate_file_$component "" (char (*)) (char (*)) (char (*))
        (bit (3)) (return ptr) (return fixed bin (21))
        (return fixed bin (35)))

(defpl1 iox_$control "" (ptr) (char (*)) (ptr) (return fixed bin (35.)))

(defpl1 match_star_name_ "" (char (*))(char (*))(return fixed bin))

(defpl1 msf_manager_$adjust "" (ptr) (fixed bin) (fixed bin (24)) (bit (3))
        (return fixed bin (35)))

(defpl1 msf_manager_$close "" (ptr))

(defpl1 msf_manager_$get_ptr "" (ptr) (fixed bin) (bit (1)) (return ptr)
        (return fixed bin (24)) (return fixed bin (35)))

(defpl1 msf_manager_$open "" (char (*)) (char (*)) (return ptr)
        (return fixed bin (35)))

(defpl1 pathname_ "" (char (*)) (char (*)) (return char (168.)))

(defpl1 pathname_$component "" (char (*)) (char (*)) (char (*))
        (return char (194.)))

(defpl1 requote_string_ "" (char (*)) (return char(*)))

(defpl1 terminate_file_ "" (ptr) (fixed bin (24.)) (bit (4)) (return fixed bin (35)))

(defpl1 user_info_$homedir "" (return char (168.)))

(defpl1 user_info_$whoami "" (return char (32.)) (return char (32.))
        (return char (32.)))

(defpl1 cv_dec_check_ "" (char(*)) (return fixed bin(35.))
        (return fixed bin(35.)))

)	; End of big declare.
