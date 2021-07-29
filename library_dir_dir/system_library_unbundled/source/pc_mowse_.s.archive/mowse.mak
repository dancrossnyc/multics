#  BEGIN MAKEFILE: MOWSE.MAK
#
#  B.WESTCOTT  JULY 29,1986
#  Modified Sept. 15 - R.L. - updated all dependent include files
#  Makefile for compose using MICROSOFT MAKE
#   MACROS
#
s=c:\mowse
h=c:\mowse
o=c:\mowse
#
#       MOWSE
#
autoload.obj : autoload.c ws.h ws_auto.h ws_dcls.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

buffer.obj  : buffer.asm mowsdefs.mac ws_buf.mac
        masm $*.asm,$*;

callmows.obj : callmows.c ws.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

capabil.obj : capabil.c ws.h cat.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

cretinst.obj: cretinst.c ws.h cat.h ws_dcls.h ws_error.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

conrqst.obj: conrqst.c ws.h wsmincap.h ws_mcb.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

conresp.obj: conresp.c ws.h wsmincap.h ws_mcb.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

curpos.obj: curpos.c emulator.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

dbgetkbl.obj:   dbgetkbl.c keydefs.h emulator.h ws.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

dbprocop.obj:   dbprocop.c keydefs.h emulator.h ws.h ws_dcls.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

dbtoggle.obj:   dbtoggle.c ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

debug.obj: debug.c
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

destinst.obj: destinst.c cat.h ws_func.h ws_dcls.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

disrqst.obj: disrqst.c ws.h wsmincap.h ws_mcb.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

disresp.obj: disresp.c ws.h wsmincap.h ws_mcb.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

dtm_mows.obj: dtm_mows.c mowse.h cat.h ws_msg.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

emulator.obj: emulator.c ws.h ws_dcls.h wsmincap.h emulator.h keydefs.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

em_put.obj: em_put.c emulator.h keydefs.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

em_routs.obj: em_routs.c emulator.h keydefs.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

execap.obj: execap.c ws.h alloc.h ws_mcb.h ws_error.h ws_func.h ws_dcls.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

execom.obj: execom.c ws.h ws_mcb.h ws_error.h ws_func.h ws_dcls.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

external.obj: external.c cat.h ws_msg.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

findname.obj: findname.c cat.h ws_dcls.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

findnumb.obj: findnumb.c cat.h ws_dcls.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

freeprog.obj: freeprog.asm ws.mac cat.mac
        masm $*.asm,$*;

getbgmes.obj : getbgmes.c ws.h ws_dcls.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

getkbkey.obj : getkbkey.c emulator.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

getmcbv.obj : getmcbv.c ws_error.h  cat.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

getstat.obj : getstat.c ws.h ws_msg.h ws_error.h  ws_dcls.h ws_func.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

gettdata.obj : gettdata.c ws.h ws_dcls.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

hard_int.obj: hard_int.asm mowsdefs.mac rs232err.mac wsmincap.mac ws_buf.mac util.mac
        masm $*.asm,$*;

initmows.obj: initmows.asm mowsdefs.mac xoscall.mac ws.mac ws_buf.mac
        masm $*.asm,$*;

internal.obj: internal.c ws_msg.h ws_error.h cat.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_con.obj:    i_con.c ws_error.h ws_msg.h ws_dcls.h wsmincap.h cat.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q


i_creins.obj: i_creins.c cat.h ws_dcls.h ws_stack.h ws_error.h ws_msg.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_desins.obj: i_desins.c cat.h ws_dcls.h ws_error.h ws_msg.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_execap.obj: i_execap.c ws.h ws_error.h ws_dcls.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_execom.obj: i_execom.c ws_error.h  cat.h ws_msg.h ws_dcls.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_fndcna.obj: i_fndcna.c cat.h ws_dcls.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_fndcnu.obj: i_fndcnu.c ws.h cat.h ws_dcls.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_getbgm.obj : i_getbgm.c alloc.h cat.h ws_dcls.h ws_error.h ws_msg.h ws_fgb.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_putbgm.obj: i_putbgm.c alloc.h cat.h ws_dcls.h ws_error.h ws_msg.h ws_fgb.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_reset.obj : i_reset.c ws.h ws_error.h ws_msg.h ws_dcls.h ws_fgb.h cat.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_sendbg.obj: i_sendbg.c ws.h ws_error.h ws_msg.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_sleep.obj: i_sleep.c ws.h ws_error.h ws_msg.h ws_dcls.h ws_fgb.h cat.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

i_suspnd.obj: i_suspnd.c ws.h wsmincap.h cat.h ws_dcls.h ws_msg.h ws_fgb.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

linebrk.obj : linebrk.asm mowsedefs.mac
        masm $*.asm,$*;

mio.obj : mio.c ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

mowse.obj   : mowse.c mowsdefs.h alloc.h cat.h ws_fgb.h emulator.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

narcint.obj : narcint.asm asmdefs.mac
        masm $*.asm,$*;

op_routs.obj    : op_routs.c keydefs.h emulator.h ws.h ws_dcls.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

pack.obj    : pack.c ws.h cat.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

pe.obj      : pe.c ws.h ws_msg.h wsmincap.h ws_mcb.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

pe_entry.obj: pe_entry.asm
        masm $*.asm,$*;

prime.obj   : prime.asm
        masm $*.asm,$*;

putbgmes.obj: putbgmes.asm ws.mac ws_dcls.mac ws_mcb.mac ws_func.mac
        masm $*.asm,$*;

putstat.obj : putstat.c ws.h ws_dcls.h ws_func.h ws_error.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

puttdata.obj: puttdata.c ws.h ws_dcls.h ws_func.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

rcvdata.obj : rcvdata.c ws.h alloc.h cat.h ws_fgb.h ws_buf.h ws_msg.h wsmincap.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

rcvfgdat.obj : rcvfgdat.c ws.h alloc.h cat.h ws_fgb.h ws_buf.h ws_msg.h wsmincap.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

rcvmsg.obj : rcvmsg.c ws.h alloc.h cat.h ws_fgb.h ws_buf.h ws_msg.h wsmincap.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

rcvbyte.obj: rcvbyte.asm mowsdefs.mac rs232err.mac wsmincap.mac ws_buf.mac
        masm $*.asm,$*;

resetcap.obj: resetcap.c ws.h wsmincap.h ws_mcb.h ws_dcls.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

resume.obj : resume.c ws.h wsmincap.h ws_mcb.h ws_dcls.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

rs232out.obj: rs232out.asm mowsdefs.mac rs232err.mac ws_buf.mac
        masm $*.asm,$*;

send.obj    : send.asm mowsdefs.mac
        masm $*.asm,$*;

sendimsg.obj: sendimsg.c ws.h alloc.h ws_error.h ws_msg.h ws_fgb.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

sendqrep.obj: sendqrep.c ws.h ws_msg.h ws_func.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

sendterm.obj: sendterm.asm mowsdefs.mac ws_buf.mac
        masm $*.asm,$*;

set_dta.obj : set_dta.asm
        masm $*.asm,$*;

setlock.obj : setlock.asm
     masm $*.asm,$*;

smdmstr.obj : smdmstr.asm mowsdefs.mac
        masm $*.asm,$*;

stackfix.obj: stackfix.asm util.mac mowsdefs.mac ws_stack.mac
        masm $*.asm,$*;

stayres.obj: stayres.c cat.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

suspend.obj: suspend.c ws.h wsmincap.h ws_mcb.h ws_dcls.h ws_func.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q
     
termbuff.obj: termbuff.asm mowsdefs.mac ws.mac ws_fgb.mac
        masm $*.asm,$*;

termcap.obj:  termcap.c ws.h wsmincap.h ws_mcb.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

termmows.obj: termmows.asm xoscall.mac mowsdefs.mac ws.mac ws_buf.mac
     masm $*.asm,$*;

user_int.obj: user_int.asm ws.mac ws_stack.mac ws_dcls.mac ws_error.mac wsmincap.mac
        masm $*.asm,$*;

wakeup.obj  : wakeup.c ws.h alloc.h ws_mcb.h cat.h ws_fgb.h ws_msg.h wsmincap.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

wsalloc.obj : wsalloc.c
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

wsexe.obj   : wsexe.c ws.h wsmincap.h ws_error.h ws_msg.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q

wsexecap.obj: wsexecap.asm ws.mac ws_msg.mac cat.mac
        masm $*.asm,$*;

wssleep.obj : wssleep.c ws.h wsmincap.h ws_mcb.h ws_dcls.h ws_func.h ws_error.h
     lc1 $*.c  -ic:\lc\  -d -n32
     lc2 $*.q



telib.lib: emulator.obj getkbkey.obj em_put.obj curpos.obj op_routs.obj dbprocop.obj \
           dbgetkbl.obj em_routs.obj
     plib86 bu telib.lib fi emulator.obj,getkbkey.obj,em_put.obj,curpos.obj
     ren telib.lib telib.old
     plib86 bu telib.lib fi op_routs.obj,dbprocop.obj,dbgetkbl.obj,em_routs.obj,telib.old

protolib.lib: hard_int.obj  narcint.obj mio.obj prime.obj smdmstr.obj \
              rs232out.obj buffer.obj initmows.obj linebrk.obj
     plib86 bu protolib.lib fi hard_int.obj,narcint.obj,mio.obj,prime.obj,linebrk.obj
        ren protolib.lib protolib.old
     plib86 bu protolib.lib fi smdmstr.obj,rs232out.obj,buffer.obj,initmows.obj,protolib.old

msintlib.lib: rcvdata.obj internal.obj wsexe.obj sendimsg.obj \
              set_dta.obj wsexecap.obj rcvbyte.obj rcvfgdat.obj \
              rcvmsg.obj wakeup.obj autoload.obj dtm_mows.obj \
              external.obj
     plib86 bu msintlib.lib fi rcvdata.obj,internal.obj,wsexe.obj,sendimsg.obj,set_dta.obj,rcvbyte.obj,wsexecap.obj
     ren msintlib.lib msintlib.old
     plib86 bu msintlib.lib fi rcvfgdat.obj,rcvmsg.obj,wakeup.obj,autoload.obj,dtm_mows.obj,msintlib.old,external.obj
     del msintlib.old

userlib.lib: send.obj user_int.obj termbuff.obj sendterm.obj i_execom.obj \
             i_fndcna.obj i_fndcnu.obj i_creins.obj i_desins.obj i_getbgm.obj \
             capabil.obj getmcbv.obj i_execap.obj i_reset.obj i_sleep.obj \
             i_sendbg.obj i_putbgm.obj i_suspnd.obj termmows.obj i_con.obj \
             dbtoggle.obj freeprog.obj
     plib86 bu userlib.lib fi send.obj,user_int.obj,termbuff.obj,sendterm.obj
        rename userlib.lib userlib.old
     plib86 bu userlib.lib fi i_fndcna.obj,i_fndcnu.obj,i_creins.obj,i_desins.obj,userlib.old 
        del userlib.old
        rename userlib.lib userlib.old
     plib86 bu userlib.lib fi capabil.obj,getmcbv.obj,i_execap.obj,i_reset.obj,userlib.old 
        del userlib.old
        rename userlib.lib userlib.old
     plib86 bu userlib.lib fi i_sleep.obj,i_execom.obj,i_getbgm.obj,dbtoggle.obj,userlib.old
        del userlib.old
        rename userlib.lib userlib.old
     plib86 bu userlib.lib fi i_suspnd,i_putbgm.obj,i_sendbg.obj,termmows.obj,userlib.old
        del userlib.old
        rename userlib.lib userlib.old
     plib86 bu userlib.lib fi i_con.obj,freeprog.obj,userlib.old


utillib.lib: debug.obj stackfix.obj setlock.obj
     plib86 bu utillib.lib fi debug.obj,stackfix.obj,setlock.obj

mowslib.lib: gettdata.obj execom.obj  callmows.obj puttdata.obj getbgmes.obj \
             cretinst.obj destinst.obj findname.obj findnumb.obj execap.obj \
             pe_entry.obj pe.obj conrqst.obj disrqst.obj disresp.obj  \
             getstat.obj putstat.obj resetcap.obj resume.obj wssleep.obj \
             sendqrep.obj putbgmes.obj suspend.obj pack.obj wsalloc.obj \
             conresp.obj stayres.obj termcap.obj
     plib86 bu mowslib.lib fi gettdata.obj,execom.obj,callmows.obj,puttdata.obj  
        ren mowslib.lib mowslib.old
     plib86 bu mowslib.lib fi cretinst.obj,destinst.obj,findname.obj,findnumb.obj,mowslib.old   
        del mowslib.old
        ren mowslib.lib mowslib.old
     plib86 bu mowslib.lib fi pe_entry.obj,pe.obj,conrqst.obj,disrqst.obj,mowslib.old   
        del mowslib.old
        ren mowslib.lib mowslib.old
     plib86 bu mowslib.lib fi getstat.obj,putstat.obj,resetcap.obj,resume.obj,mowslib.old  
        del mowslib.old
        ren mowslib.lib mowslib.old
     plib86 bu mowslib.lib fi sendqrep.obj,putbgmes.obj,pack.obj,wsalloc.obj,mowslib.old 
        del mowslib.old
        ren mowslib.lib mowslib.old
     plib86 bu mowslib.lib fi getbgmes.obj,execap.obj,disresp.obj,wssleep.obj,mowslib.old
        del mowslib.old
        ren mowslib.lib mowslib.old
     plib86 bu mowslib.lib fi conresp.obj,suspend.obj,stayres.obj,termcap.obj,mowslib.old

#
#   MOWSE
#
mowse.com: mowse.obj telib.lib protolib.lib msintlib.lib userlib.lib \
            utillib.lib mowslib.lib
     del *.old 
           link \lc\c\c mowse ,mowse,mowse,TELIB+PROTOLIB+MSINTLIB+USERLIB+UTILLIB+MOWSLIB+\lc\s\lc/map
           mapsym mowse
           exe2bin mowse mowse.com
           del mowse.exe

#   END MAKEFILE: MOWSE.MAK
