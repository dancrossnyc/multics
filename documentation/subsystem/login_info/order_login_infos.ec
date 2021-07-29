&version 2
&-
&- HISTORY COMMENTS:
&-  1) change(87-07-27,GDixon), approve(87-07-27,MCR7679),
&-     audit(87-07-27,Brunelle), install(87-08-04,MR12.1-1055):
&-     Created.
&-                                                      END HISTORY COMMENTS
&-
&set WDIR &[wd]
cwd &ec_dir

&print Sort info segments in the directory.
l_set_ring_brackets ** 4 5 5
create_dir &!
move **.info &!>== -force
cwd &!
move ([sort_strings -asc [segments **]]) <== -force
cwd <
delete_dir &! -force
l_set_ring_brackets ** 1 5 5

&print Relink the links in the directory.
unlink **.info -brief
do "link >doc>subsystem>ssu.&&1.info &&1.info;
&+  addname &&1.info -brief
&+   [after ([status -names >doc>subsystem>ssu.&&1.info]) ssu.]"
&+ (help list_help list_requests)
cwd &(WDIR)
&quit
