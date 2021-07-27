&  ***********************************************************
&  *                                                         *
&  * Copyright, (C) Honeywell Information Systems Inc., 1984 *
&  *                                                         *
&  ***********************************************************
&command_line off
&- automatic reboot ec for bce
&- Keith Loepere, January 1984.
&- Fixed by same to handle a failure to boot.
&-
&if [equal [bce_state] "early"] &then &goto cant_boot_early
&if [equal [bce_state] "crash"] &then &goto cant_boot_crash
&print Begin auto boot.
set_flagbox bce_command ""
set_flagbox auto_reboot true
set_flagbox booting true
&input_line off
&attach
config_edit
gp/^cpu/
gp/^iom/
gp/^mem/
q
&detach
set_flagbox bce_command "exec_com rtb"
boot &rf1
&quit
&label cant_boot_early
&print The system cannot be booted from the "early" state.
&print First use "bce" to get to the "boot" state.
&quit
&label cant_boot_crash
&print The system cannot be booted from the "crash" state.
&print First use "reinitialize" to get to the "boot" state.
&quit
