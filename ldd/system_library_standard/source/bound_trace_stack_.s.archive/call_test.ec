&version 2
&-
&- HISTORY COMMENTS:
&-  1) change(2016-08-11,GDixon), approve(2016-10-13,MCR10014),
&-     audit(2016-10-13,Swenson), install(2016-10-13,MR12.6f-0002):
&-     Test script for call version 01.02 command.
&-                                                      END HISTORY COMMENTS
&-
&trace &control off

&if &is_defined(1)
&then &goto &1

&label call_test_
&print -----------------
depd call_test_
&print ---
call -db 3      call_test_
&if &is_defined(1) &then &quit

&label empty_returns
&print -----------------
depd call_test_$rt
&print ---
call -db 3      call_test_$rt
&if &is_defined(1) &then &quit

&label uns
&print -----------------
depd call_test_$uns 
&print ---
call -db 3      call_test_$uns 777777777777b3 377777777777b3 377777777777777777777777b3 23e5
&print
&print ------
&print EXPECT: size condition for parm02, 03, 04
call -db 2      call_test_$uns 777777777777b3 777777777777b3 777777777777777777777777b3 23e59
&if &is_defined(1) &then &quit

&label get_line
&print -----------------
depd call_test_$get_line
&print ---
call -db 3 -oc call_test_$get_line 234|20 -o -id buff -addr "char(buffL)" -ln readN -i 60 -id buffL -o -id readN -o -code
&print
&print ------
call -db 2 -oc call_test_$get_line 234|20 -o -id buff -addr "char(buffL)" -ln readN -i 30 -id buffL -o -id readN -o -code
&print
&print EXPECT: long_record code
&if &is_defined(1) &then &quit

&label array
&print -----------------
depd call_test_$array
&print ---
call -db 3      call_test_$array 1 2 3 4
&print
&print EXPECT: errors about array and structure args not supported.
&print
&print ------
call -db 3      call_test_$array 1 2 3
&print
&print EXPECT: argument missing error -- diagnose too few initial arg_values
&if &is_defined(1) &then &quit

&label FB3
&print -----------------
depd call_test_$FB3
&print ---
call -db 3      call_test_$FB3 -23.5e3 fffffb4 ffff8b4
&print
&print EXPECT: parm2 gets size condition, because 20 bits given for fixed bin(17).
&print
&print ------
call -db 2 -oc  call_test_$FB3 -23.5e3 1ffffb4 ffff8b4
&print
&print EXPECT: Initial values OK, proper display by the entrypoint itself.
&if &is_defined(1) &then &quit

&label bit24
&print -----------------
depd call_test_$bit24
&print ---
call -db 3 -oc  call_test_$bit24 1234b4 abcdefb4
&print
&if &is_defined(1) &then &quit

&label FB5
&print "Range for float bin: 1.4693e-39   (causes underflow)"
&print "                     1.4694e-39   (very small number)"
&print "                     1.70141e+38  (very large number)"
&print "                     1.70142e+38  (causes overflow)"
&print -----------------
depd call_test_$FB5
&print ---
call -db 3 -oc  call_test_$FB5 -1 2 -3.5 1.70142e38 -1|1
&print
&print EXPECT: Overflow condition on parm 4.
&print ------
call       -oc  call_test_$FB5 -1 2 -3.5 1.70141e38 -1|1
&print
&print ------
call       -oc  call_test_$FB5 -1 2 1.4694e-39 1.70141e38 -1|1
&print
&print ------
call       -oc  call_test_$FB5 -1 2 1.4693e-39 1.70141e38 -1|1
&print
&print EXPECT: Underflow condition on parm 3.
&if &is_defined(1) &then &quit

&label FD
&print -----------------
depd call_test_$FD
&print ---
call -db 3 -oc  call_test_$FD -12345678900 23456e20
&print
&print EXPECT: Size condition on parm 3.
&print
&print ---
call       -oc  call_test_$FD -12345678.900 23456e20
&print
&if &is_defined(1) &then &quit

&label ch
&print -----------------
depd call_test_$ch
&print ---
call -db 3 -oc  call_test_$ch abcd 1234
&print
&if &is_defined(1) &then &quit

&label bts
&print -----------------
depd call_test_$bts
&print ---
call -db 3 -oc  call_test_$bts 101010 ABCDb4
&print
&if &is_defined(1) &then &quit

&label btVar
&print -----------------
depd call_test_$btVar
&print ---
call -db 3 -oc  call_test_$btVar
&print
&if &is_defined(1) &then &quit

&label area
&print -----------------
depd call_test_$area
&print ---
call -db 3 -oc  call_test_$area -ig
&print
&print ---
call -db 2 -oc  call_test_$area -ig -ml 40
&print
&if &is_defined(1) &then &quit

&label areap
&print -----------------
depd call_test_$areap
&print ---
call -db 3 -oc  call_test_$areap -ig -addr "area(*)"
&print
&print ---
call -db 2 -oc  call_test_$areap -ig -addr "area(*)" -ml 40
&print
&print ---
call -db 2 -oc  call_test_$areap -ig -addr "area(40)"
&print
&if &is_defined(1) &then &quit

&label times
&print -----------------
depd call_test_$times
&print ---
call -db 3 -oc  call_test_$times -in "2013-07-25 04:07:05.222" -time -out -date_time
&print
&print ---
call -db 2 -oc  call_test_$times -in "2013-07-25 04:07:05.222" -time -out -date
&print
&print ---
call -db 1 -oc  call_test_$times -in "2013-07-25 04:07:05.222" -time -out -time
&print
&print --- 
&print With calendar_clock as process_default for date_time
&print
std date_time calendar_clock -push
&print
call -db 1 -oc  call_test_$times -in "2013-07-25 04:07:05.222" -time -out -date_time
&print
std date_time -pop
&print
&if &is_defined(1) &then &quit

