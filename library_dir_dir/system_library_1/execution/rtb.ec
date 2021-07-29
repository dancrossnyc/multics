&  ***********************************************************
&  *                                                         *
&  * Copyright, (C) Honeywell Information Systems Inc., 1984 *
&  *                                                         *
&  ***********************************************************
&command_line off
&- ec to handle returning to bce
&- Keith Loepere, January 1984.
&-
&if [not [get_flagbox call_bce]] &then &goto non_call_entry
&-
&print bce invoked via hphcs_$call_bce.
&-
&if [not [query "Should normal recovery procedures be used?"]] &then &goto abort_auto_mode
&-
&label non_call_entry
&-
&- look at the state of things
&-
&if [not [get_flagbox ssenb]] &then &goto ss_not_enabled
&-
&- storage system enabled; take a dump and esd
&-
exec_com dump
&-
&if [nequal [severity dump] 0] &then &goto dump_okay
&- 
&print Dump failed.
&goto abort_auto_mode
&-
&label dump_okay
&-
emergency_shutdown
&- return from above is back at rtb
&-
&label ss_not_enabled
&-
&- Is everything okay?
&-
&if [nequal [shutdown_state] 4] &then &goto okay_shutdown
&-
&if [nequal [shutdown_state] 3] &then &print Shutdown with locks set.
&else &print Error during shutdown.
&goto abort_auto_mode
&-
&label okay_shutdown
&-
&- normal shutdown - see if we should reboot
&-
&if [not [get_flagbox unattended]] &then &goto abort_auto_mode
&if [not [get_flagbox auto_reboot]] &then &goto abort_auto_mode
&if [get_flagbox booting] &then &goto system_cant_boot
&-
set_flagbox rebooted true
&-
&- inform a.s. that we are doing an automatic reboot
&-
exec_com auto star
&quit
&-
&label system_cant_boot
&-
&print System crashed during boot.
&-
&label abort_auto_mode
&-
set_flagbox bce_command ""
set_flagbox auto_reboot false
set_flagbox rebooted false
&quit
