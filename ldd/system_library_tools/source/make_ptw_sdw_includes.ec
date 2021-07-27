&attach
discard_output  convert_include_file ptw.l68 -name l68_ptw l68_core_ptw
discard_output  convert_include_file ptw.adp -name adp_ptw adp_core_ptw
discard_output  convert_include_file sdw.l68
discard_output  convert_include_file sdw.adp
&
delete  -brief  -force  l68_ptw.incl.alm  adp_ptw.incl.alm  l68_sdw.incl.alm  adp_sdw.incl.alm
&
copy  ptw.l68.incl.alm  l68_ptw.incl.alm
copy  ptw.adp.incl.alm  adp_ptw.incl.alm
copy  sdw.l68.incl.alm  l68_sdw.incl.alm
copy  sdw.adp.incl.alm  adp_sdw.incl.alm
&
ted
r ptw.l68.incl.alm
&input_line off
1,$s/l68_// w
1,$d
&
&input_line on
r ptw.adp.incl.alm
&input_line off
1,$s/adp_// w
1,$d
&
&input_line on
r sdw.l68.incl.alm
&input_line off
1,$s/l68_// w
1,$d
&
&input_line on
r sdw.adp.incl.alm
&input_line off
1,$s/adp_// w
q
