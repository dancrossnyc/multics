&  ***********************************************************
&  *                                                         *
&  * Copyright, (C) Honeywell Information Systems Inc., 1984 *
&  *                                                         *
&  ***********************************************************
&command_line off
&-
&- restart auto operation after manual bce entry
&- Keith Loepere, January 1984.
&-
set_flagbox auto_reboot true
set_flagbox rebooted false
set_flagbox booting false
set_flagbox bce_command "exec_com rtb"
go
&quit
