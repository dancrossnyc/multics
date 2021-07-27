(* *************************************************************************
   *                                                                       *
   * Copyright (c) 1980 by Centre Interuniversitaire de Calcul de Grenoble *
   * and Institut National de Recherche en Informatique et Automatique     *
   *                                                                       *
   ************************************************************************* *)




(* HISTORY COMMENTS:
  1) change(86-09-11,JMAthane), approve(86-09-11,MCR7521),
     audit(86-09-15,JPFauche), install(86-11-12,MR12.0-1212):
     Release 8.03 for MR12
                                                   END HISTORY COMMENTS *)


$OPTIONS page $

$OPTIONS switch trace := true ; switch security := true ; t + $
  PROGRAM state ;
    $IMPORT
                                                  (* IMPORTED VARIABLES *)
      'GENERE (pascal)' :
        cb,
        codesymb,
        indfich,
        mfari1,
        mfari2,
        tagsymb,
        usednameaddr ;
      'RACINE (pascal)' :
        boxheader,
        charptr,
        chnix,
        ctptr,
        display,
        disx,
        errtotal,
        intptr,
        level,
        mpcogout,
        nilptr,
        no,
        realptr,
        string_ptr,
        symbolfile,
        symbolline,
        symbolmap,
        undecptr,
        undlab,
        version ;
                                                  (* IMPORTED PROCEDURES *)
      'GENERE (pascal)' :
        gendesca,
        gendescb,
        geneism,
        genstand,
        inser ;
      'RACINE (pascal)' :
        error,
        insymbol,
        nameisref,
        nextline,
        recadre,
        sup ;
      'UNIQUE (pascal)' :
        heaperror ;
      'CONTEXTTABLE (pascal) ' :
        create_types_box,
        create_konst_box,
        create_vars_box,
        findminmax,
        packedsize ;
      'MODATTR (pascal) ' :
        freeattr,
        initattrvarbl,
        isstring,
        lvalvarbl,
        printattr ;
      'MODVARIABLE (pascal) ' :
        init_desc_address,
        variable ;
      'optimized_procedures (alm)' :
        search $

    $EXPORT
      addressvar,
      arrayboundsctp,
      asscheck,
      calcvarient,
      checkbnds,
      choicerarq,
      cltransf,
      currentbloc,
      currentpr,
      currlcstpt,
      currllcstpt,
      currrcstpt,
      currwithlist,
      currwcstpt,
      disposeused,
      resetused,
      divcheck,
      entercst,
      enterlcst,
      enterllcst,
      enterundlab,
      errorctp,
      freeallregisters,
      freebloc,
      gattr,
      gencheckmultover,
      gencstecode,
      genexceptcode,
      getpr,
      inbounds,
      initstate,
      inputctp,
      inxcheck,
      lcsave,
      linktoend,
      linktoendplace,
      linktomain,
      linktomainplace,
      loadadr,
      loadbase,
      maxinxused,
      maxprused,
      modif,
      newbloc,
      nilanaq,
      nileraq,
      nulpw,
      oldnewstor,
      opaq,
      outputctp,
      prinst,
      printstatusregister,
      psrsize,
      raisused,
      regenere,
      regname,
      revcltransf,
      rqisused,
      sauvereg,
      stack_extension,
      stack_has_been_extended,
      stattrace,
      tabacc,
      tabkind,
      tempstor,
      tmax,
      transfer,
      variab,
      variabctptr,
      withvariable,
      workformaths,
      workformathsplacew $





$OPTIONS page $

$INCLUDE 'CONSTTYPE' $



$OPTIONS page $

    VAR
                                                  (* *** REDEFINE  IMPORTED VARIABLES  * *)
                                                  (* FROM GENERE *)
      cb : integer ;
      codesymb : ARRAY [instword] OF alfa ;
      indfich : integer ;
      mfari1 : zari ;
      mfari2 : zari ;
      tagsymb : ARRAY [tag] OF PACKED ARRAY [1..4] OF char ;
      usednameaddr : ctp ;
                                                  (* FROM DECLARE *)
                                                  (* FROM RACINE *)
      boxheader : PACKED ARRAY [1..120] OF char ;
      harptr : ctp ;
      charptr : ctp ;
      chnix : integer ;
      ctptr : ctp ;
      display : ARRAY [0..displimit] OF recidscope ;
      disx : integer ;
      errtotal : integer ;
      intptr : ctp ;
      level : levrange ;
      mpcogout : text ; nilptr : ctp ;
      no : integer ;
      realptr : ctp ;
      string_ptr : ctp ;
      symbolfile : integer ;
      symbolline : integer ;
      symbolmap : boolean ;
      undecptr : ctp ;
      undlab : ARRAY [1..undmax] OF occurence ;
      version : integer ;


(*    EXPORTABLE VARIABLES *)

      arrayboundsctp : ctp ;                      (* DUMMY BOX FOR CHECKBNDS(ARRAYS) *)
      asscheck : boolean ;                        (* SET IN INSYMBOL T+,A+ FOR ASSIGN CHECK *)
      cltransf : ARRAY [1..6] OF integer ;        (* GIVES THE TRANSF CORR. TO OPER.  8,CL *)
      currentbloc : regpt ;                       (* LAST CREATED BOX REGISTER *)
      currentpr : preg ;                          (* GIVES THE POINTER REGISTER GET BY GETPR *)
      currlcstpt : lcstpt ;                       (*  "    "      LONG CONSTANT *)
      currllcstpt : llcstpt ;                     (*  "    "      SET   " *)
      currrcstpt : rcstpt ;                       (*  "    "      REAL  " *)
      currwithlist : withreflist ;
      currwcstpt : wcstpt ;                       (*  "    "      WORD  " *)
      disposeused : boolean ;
      resetused : boolean ;
      divcheck : boolean ;                        (* ZERO DIVIDE CHECK *)
      errorctp : ctp ;
      gattr : attr ;                              (* GLOBAL ATTR *)
      inputctp : ctp ;                            (*  BOX PREDECLARED FOR INPUT *)
      inxcheck : boolean ;                        (* SET BY X+    FOR INDEX *)
      lcsave : integer ;                          (* SAVING OF LC *)
      linktoend : boolean ;
      linktoendplace : integer ;
      linktomain : boolean ;
      linktomainplace : integer ;
      maxinxused : register ;                     (* LAST INDEX REGISTER USED IN GETINDEX *)
      maxprused : preg ;                          (* LAST POINTER REGISTER USED IN GETPR *)
      modif : ARRAY [nxreg..rq] OF tag ;          (* GIVES FOR A REGISTER R ITS TAG TR *)
      nilanaq,
      nileraq : setarray ;                        (* USED FOR NIL COMPARISONS *)
      nulpw : setarray ;                          (*  EMPTY SET *)
      opaq : ARRAY [typeofop, ra..reaq] OF istand ; (* GIVES INST. WITH A,Q,AQ,EAQ *)
      outputctp : ctp ;                           (* BOX PREDECLARED FOR OUTPUT *)
      prinst : ARRAY [typepr, pr1..pr6] OF istand ; (* GIVES A PR INSTRUCTION *)
      psrsize : integer ;                         (* USEFULL SIZE OF PSR *)
      regname : ARRAY [register] OF PACKED ARRAY [1..4] OF char ; (* REGIST. NAMES *)
      revcltransf : ARRAY [1..6] OF integer ;     (* GIVES  8,CL --> REVERSE TRANSF *)
      stack_has_been_extended : boolean ;
      stattrace : levtrace ;                      (* TRACE FOR MODULE STATEMENT *)
      tabacc : ARRAY [attraccess] OF alfa ;       (* MNEMONICS USED IN TRACE *)
      tabkind : ARRAY [attrkind] OF alfa ;        (* MNEMONICS USED IN TRACE *)
      variabctptr : ctp ;
      tempstor : integer ;                        (* FREE STORAGE IN STACK *)
      tmax : integer ;                            (* MAX REACHED IN CURRENT FRAME *)
      withvariable : boolean ;                    (* TRUE IF IN WITH CONTROL VARIABLE ANALYSIS *)
      workformaths : boolean ;                    (* TRUE IF WORK AREA ALLOCATED IN CURRENT FRAME FOR MATH OPS *)
      workformathsplacew : integer ;              (* OFFSET IN CURR STACK FRAME OF THIS WORK AREA *)


(*      LOCAL VARIABLES *)

      begfreelist : regpt ;                       (* FIRST FREE REGISTER BOX *)
      currentindex : register ;                   (* GIVES THE INDEX REGISTER GET BY GETINDEX *)
      dummybloc : regpt ;                         (*  DUMMY REGISTER BOX *)
      forgetbox : integer ;                       (* USED TO KNOW THE FORGOTTEN REG BOX *)
      freereg : statearray ;                      (* FALSE FOR ALL REGISTERS *)
      newtagstar : ARRAY [tn..tx7] OF tag ;       (* GIVES FOR A TAG TR --> TRY *)
      nilpseudoset : setarray ;                   (* USED TO GENERATE NIL "ITS" *)
      regcharge : statearray ;                    (* GIVES THE LOADIND STATES OF THE REGISTERS *)
      saved_stack_end_place : integer ;
      starmodif : ARRAY [nxreg..rq] OF tag ;      (* GIVES FOR A REGISTER R --> TAG TRY *)
      sversion : integer ;                        (* VERSION OF STATE *)
      xinst : ARRAY [typix, x0..x7] OF istand ;   (* GIVES AN ALM INSTRUCTION WITH XI *)


$OPTIONS page $

    $VALUE

      cltransf = (7, 8, 9, 10, 6, 2) ;
      maxinxused = x5 ;
      maxprused = pr7 ;
      modif = (tn, tx0, tx1, tx2, tx3, tx4, tx5, tx6, tx7, tn, tal, tql) ;
      nilanaq = ('1FFFC003F'x, 'FFFFC7E3F'x, 6 * 0) ;
      nileraq = ('1FFFC0023'x, '000040000'x, 6 * 0) ;
      nulpw = (8 * 0) ;
      opaq = (ilda, ildq, ildaq, idfld,
        isba, isbq, isbaq, idfsb,
        ials, iqls, ills, inop,
        iada, iadq, iadaq, idfad,
        ineg, inop, inegl, ifneg,
        icmpa, icmpq, icmpaq, idfcmp,
        ista, istq, istaq, idfst) ;
      prinst = (iepp1, iepp2, iepp5, iepp7, iepp3, iepp0, iepp4, iepp4, iepp6,
        ispri1, ispri2, ispri5, ispri7, ispri3, ispri0, ispri4, ispri4, ispri6,
        ilprp1, ilprp2, ilprp5, ilprp7, ilprp3, ilprp0, ilprp4, ilprp4, ilprp6) ;
      regname = ('NRG ', 'PR1 ', 'PR2 ', 'PR5 ', 'PR7 ', 'PR3 ', 'PR0 ', 'PRST', 'PRLK', 'PR6 ',
        'NXR ', ' X0 ', ' X1 ', ' X2 ', ' X3 ', ' X4 ', ' X5 ', ' X6 ', ' X7 ',
        '    ', ' A  ', ' Q  ', ' AQ ', 'EAQ ', 'PSR ', ' E  ', ' I  ') ;
      revcltransf = (10, 9, 8, 7, 6, 2) ;
      tabacc = (' DIRECT ', 'POINTEE ', 'POINTABL') ;
      tabkind = (' VARBL  ', ' LCOND  ', ' LVAL   ', ' CHAIN  ', ' SVAL   ') ;

      freereg = (27 * false) ;
      newtagstar = (tny, tauy, tquy, tz23, ticy, taly, tqly, tz27,
        tx0y, tx1y, tx2y, tx3y, tx4y, tx5y, tx6y, tx7y) ;
      nilpseudoset = (nilleft, nilright, 6 * 0) ;
      starmodif = (tny, tyx0, tyx1, tyx2, tyx3, tyx4, tyx5, tyx6,
        tyx7, tny, tyal, tyql) ;
      xinst = (
        iadlx0, iadlx1, iadlx2, iadlx3, iadlx4, iadlx5, iadlx6, iadlx7,
        iadx0, iadx1, iadx2, iadx3, iadx4, iadx5, iadx6, iadx7,
        isxl0, isxl1, isxl2, isxl3, isxl4, isxl5, isxl6, isxl7,
        ilxl0, ilxl1, ilxl2, ilxl3, ilxl4, ilxl5, ilxl6, ilxl7)
      $

$OPTIONS page $

(* *** NOW REDEFINE IMPORTED PROCEDURE *)


(* FROM GENERE *)
    PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ; EXTERNAL ;
    PROCEDURE geneism (fcode : ieism ; ffield : integer ; fbits : zptr) ; EXTERNAL ;
    PROCEDURE gendesca (fareg : preg ; fadr, fcn : integer ; fta : lgcar ;
      fn : integer ; frlgth : mreg) ; EXTERNAL ;
    PROCEDURE gendescb (fareg : preg ; fadr, fc, fb : integer ; fn : integer ;
      frlgth : mreg) ; EXTERNAL ;
    PROCEDURE inser (fcb : integer ; fplace : integer) ; EXTERNAL ;
                                                  (* FROM RACINE *)
    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE insymbol ; EXTERNAL ;
    PROCEDURE nameisref (p : ctp ; f, l : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    FUNCTION recadre (fnum, fmod : integer) : integer ; EXTERNAL ;
    PROCEDURE search ; EXTERNAL ;
    FUNCTION sup (fval1, fval2 : integer) : integer ; EXTERNAL ;

(* FROM UNIQUE *)
    PROCEDURE heaperror ; EXTERNAL ;

(* FROM MODVARIABLE *)

    PROCEDURE init_desc_address (fctptr : ctp ; VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE variable (fvarset : boolean) ; EXTERNAL ;

(* FROM CONTEXTTABLE *)

    PROCEDURE create_types_box (VAR fvbox : ctp ; fname : alfaid ; fform : typform ; fbool : boolean) ; EXTERNAL ;
    PROCEDURE create_konst_box (VAR fvbox : ctp ; fname : alfaid ; ftypofconst : consttype) ; EXTERNAL ;
    PROCEDURE create_vars_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;
    PROCEDURE findminmax (fctp : ctp ; VAR fmin, fmax : integer) ; EXTERNAL ;
    FUNCTION packedsize (fctp : ctp) : integer ; EXTERNAL ;

(* FROM MODATTR *)

    PROCEDURE freeattr (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE initattrvarbl (VAR fattr : attr) ; EXTERNAL ;
    FUNCTION isstring (VAR fattr : attr) : boolean ; EXTERNAL ;
    PROCEDURE lvalvarbl (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE printattr (VAR fattr : attr) ; EXTERNAL ;



$OPTIONS page $

(* ************************************ PRINTREGBOX *************************** *)

$OPTIONS compile = trace $
    PROCEDURE printregbox (fptbox : regpt) ;

(* C  CALLED  WHEN STATTRACE IS HIGH  TO  PRINT THE  CONTENT  OF  A
   SPECIFIED   REGISTER_STATE_BOX
   C *)
      BEGIN                                       (* PRINTREGBOX *)
        nextline ; write (mpcogout, boxheader) ; nextline ;
        IF fptbox = NIL THEN
	BEGIN
	  write (mpcogout, '* REGBOX REQUESTED IS NIL. TRACE STOPS') ; nextline ;
	END ELSE
	BEGIN
	  write (mpcogout, '* REGISTER BOX FOLLOWING IS AT @', ord (fptbox)) ; nextline ;
	  WITH fptbox@ DO
	    BEGIN
	      write (mpcogout, '* REGISTER IS ', regname [sregister], ' SAVEPLACE IS :',
	        saveplace : 6) ; nextline ;
	      write (mpcogout, '* NEXTBLOC AND PREDBLOC  ARE  AT @', ord (nextbloc), ' AND AT @',
	        ord (predbloc)) ; nextline ;
	    END ;
	END ;
        write (mpcogout, boxheader) ; nextline ; nextline ;
      END (* PRINTREGBOX *) ;
$OPTIONS compile = true $


$OPTIONS page $

(* ************************************ PRINTSTATUSREGISTER ****************** *)

    PROCEDURE printstatusregister ;

(* C CALLED    IN TRACE CONTEXT  IN ORDER TO EDIT THE LOADED REGISTERS        C *)
      VAR
        lreg : register ;
      BEGIN
        write (mpcogout, '*** REGISTERS LOADED ARE:') ;
        FOR lreg := pr1 TO maxprused DO
	IF regcharge [lreg] THEN write (mpcogout, regname [lreg] : 5) ;
        FOR lreg := x0 TO maxinxused DO
	IF regcharge [lreg] THEN write (mpcogout, regname [lreg] : 5) ;
        FOR lreg := ra TO psr DO
	IF regcharge [lreg] THEN write (mpcogout, regname [lreg] : 5) ;
        nextline ;
      END (* PRINTSTATUSREGISTER *) ;



$OPTIONS page $

(* ******************************************** INITSTATE ********************* *)

    PROCEDURE initstate ;

(* C    INIT ALLL VARIABLES OF STATE                                          C *)
      VAR
        it : integer ;
        lastcreate, wkpt : regpt ;
      BEGIN
        create_types_box (arrayboundsctp, blank, numeric, false) ;
        WITH arrayboundsctp^ DO
	BEGIN
	  size := bytesinword ; cadrage := bytesinword ;
	  npksize := size ;
                                                  (* NMIN and NMAX filled each time before use *)
	END ;
                                                  (*  INITIALIZATION OF REGISTER'S BOXES LIST *)
        new (dummybloc) ; IF dummybloc = NIL THEN heaperror ; (* EXIT COMP. *)
        WITH dummybloc@ DO
	BEGIN
	  sregister := nreg ; saveplace := 0 ; predbloc := NIL ;
	  nextbloc := NIL ;
	END ;
        currentbloc := dummybloc ;
        lastcreate := NIL ;
        FOR it := 1 TO longboxlist DO
	BEGIN
	  new (wkpt) ; IF wkpt = NIL THEN heaperror ; (* EXIT COMP. *)
	  WITH wkpt@ DO
	    BEGIN
	      IF lastcreate = NIL THEN
	        begfreelist := wkpt ELSE
	        lastcreate@.nextbloc := wkpt ;
	      lastcreate := wkpt ;
	      nextbloc := NIL ; predbloc := NIL ;
	    END (* WITH *) ;
	END ;                                   (* FOR IT *)
        forgetbox := 0 ;
        stack_has_been_extended := false ;
        stattrace := none ;
        errorctp := NIL ;
        inputctp := NIL ;
        outputctp := NIL ;
        linktomain := false ;
        linktoend := false ;
        sversion := 00 ;
        IF sversion > version THEN version := sversion ;
        withvariable := false ;
        disposeused := false ;
        resetused := false ;
      END (* INITSTATE *) ;


$OPTIONS page $

(* ************************************ ENTERCST ****************************** *)

    PROCEDURE entercst (fval : integer ; VAR fboxpt : wcstpt) ;

(* C  . SEARCHES IF  "FVAL"  IS ALREADY PRESENT IN THE LIST BEGINNING  AT
   CURRWCSTPT.
   IF YES   RETURNS THE POINTER  ON IT
   IF NO    RETURNS THE NEWLY  CREATED  BOX  . " "FVAL" , CSTPLACE, CSTNEXT
   C *)
(* E ERRORS DETECTED
   HEAPERROR
   E *)
      LABEL
        1 ;                                       (* EXIT  SEARCH'S WHILE *)
      VAR
        workpt : wcstpt ;
      BEGIN                                       (* ENTERCST *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT ENTERCST @@@ WITH FVAL:', fval) ; nextline ;
	END ;
$OPTIONS compile = true $
        workpt := currwcstpt ;                    (* LAST CREATED BOX,  *)
                                                  (* NIL FOR THE FIRST ENTERED CSTE *)
        WHILE workpt # NIL DO
	IF workpt@.valu = fval THEN
	  GOTO 1 (* ASSIGNS FBOXPT,EXIT PROC *) ELSE
	  workpt := workpt@.cstnext ;
                                                  (* AT THIS POINT, CST NOT FOUND *)
        new (workpt) ; IF workpt = NIL THEN heaperror ; (* EXIT COMP *)
        WITH workpt@ DO
	BEGIN valu := fval ; cstnext := currwcstpt ; (* CHAINS BOXES *)
	  cstplace := 0 ;                       (* INIT CHAIN OF UNRESOLVED *)
                                                  (* REFERENCES IN UNDLAB *)
	END ;
        currwcstpt := workpt ;
$OPTIONS compile = trace $
        IF stattrace = high THEN
	BEGIN
	  write (mpcogout, boxheader) ; nextline ;
	  write (mpcogout, '* W.CONST BOX CREATED AT @', ord (workpt), ' NEXT BOX AT @',
	    ord (workpt@.cstnext), ' VALU IS:', fval) ; nextline ;
	  write (mpcogout, boxheader) ; nextline ;
	END ;
$OPTIONS compile = true $
1 :     fboxpt := workpt ;                        (* EITHER  EXIT WHILE, EITHER  NEW BOX CREATED *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN write (mpcogout, '@@@ FIN ENTERCST @@@ WITH V.FBOXPT AT @', ord (fboxpt)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* ENTERCST *) ;


$OPTIONS page $

(* ************************************ ENTERLCST ***************************** *)

    PROCEDURE enterlcst (VAR fval : setarray ; VAR fboxpt : lcstpt) ;

(* C   SEARCHS IF THE TWO-WORDS CONSTANT  (FVAL0,FVAL1) IS ALREADY IN THE CHAIN
   WHOSE  HEAD IS POINTED BY CURRLCSTPT.
   IF NO CREATES A NEW BOX (LUNRESOLV)
   RETURNED POINTER   POINTS THE EITHER FOUND OR CREATED BOX
   C *)
(* E ERRORS DETECTED
   HEAPERROR
   E *)
      LABEL
        1 ;                                       (* EXIT WHILE *)
      VAR
        workpt : lcstpt ;
      BEGIN                                       (* ENTERLCST *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT ENTERLCST @@@ WITH FVAL0,FVAL1 :', fval [0] : 14,
	    fval [1] : 14) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        workpt := currlcstpt ;                    (* LAST CREATED CSTE *)
        WHILE workpt # NIL DO
	BEGIN
	  IF workpt@.lvalu [0] = fval [0] THEN
	    IF workpt@.lvalu [1] = fval [1] THEN
	      GOTO 1 ;                          (* ASSIGNS FBOXPT AND EXIT PROC *)
	  workpt := workpt@.lnext ;
	END ;                                   (* WHILE *)
                                                  (* CSTE NOT FOUND ==> CREATES A NEW BOX *)
        new (workpt) ; IF workpt = NIL THEN heaperror ; (* EXIT COMP *)
        WITH workpt@ DO
	BEGIN
	  lvalu := fval ;
	  lplace := 0 ;                         (* INIT CHAIN OF UNRESOLVED REF. *)
	  lnext := currlcstpt ;
	END ;
        currlcstpt := workpt ;
$OPTIONS compile = trace $
        IF stattrace = high THEN
	BEGIN
	  write (mpcogout, boxheader) ; nextline ;
	  write (mpcogout, '* LCONST BOX CREATED AT @', ord (workpt)) ; nextline ;
	  nextline ;
	  write (mpcogout, boxheader) ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* <--- *)
1 :
        fboxpt := workpt ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN ENTERLCST @@@ WITH V.FBOXPT AT @', ord (fboxpt)) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* ENTERLCST *) ;


$OPTIONS page $

(* ************************************ ENTERLLCST **************************** *)

    PROCEDURE enterllcst (VAR fval : setarray ; VAR fboxpt : llcstpt) ;

(* C.SEARCHES IF THE SET CONSTANT FVAL IS ALREADY IN THE CHAIN BEGINNING AT
   CURRLLCSTPT
   .IF YES  RETURNS A POINTER ON IT,  ELSE  CREATES A NEW BOX  AND RETURNS THE
   NEW POINTER
   C *)
(* E ERRORS DETECTED
   HEAPERROR   (EXIT COMP)
   E *)
      LABEL
        1 ;                                       (* EXIT WHILE *)
      VAR
        workpt : llcstpt ;
        it : integer ;
        equal : boolean ;
      BEGIN                                       (* ENTERLLCST *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT ENTERLLCST @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        workpt := currllcstpt ;
        WHILE workpt # NIL DO
	BEGIN
	  equal := true ;
	  FOR it := 0 TO bornesupset DO IF fval [it] # workpt@.llvalu [it] THEN
	      equal := false ;
	  IF equal THEN
                                                  (* <=== *) GOTO 1 ELSE
	    workpt := workpt@.llnext ;
	END ;                                   (* WHILE *)
                                                  (* FVAL NOT FOUND. THEN CREATES A NEW BOX *)
        new (workpt) ; IF workpt = NIL THEN heaperror ; (* EXIT COMP *)
        WITH workpt@ DO
	BEGIN llvalu := fval ; llnext := currllcstpt ; llplace := 0 ; (* LATER ON UNDLAB *)
	END ;
        currllcstpt := workpt ;
$OPTIONS compile = trace $
        IF stattrace = high THEN
	BEGIN
	  write (mpcogout, boxheader) ; nextline ;
	  write (mpcogout, '* LLCONST BOX CREATED AT @', ord (workpt), ' LLNEXT IS AT @',
	    ord (workpt@.llnext)) ; nextline ;
	  FOR it := 0 TO bornesupset DO write (mpcogout, workpt@.llvalu [it] : 15) ; nextline ;
	  write (mpcogout, boxheader) ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* <==== *)
1 :     fboxpt := workpt ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN ENTERLLCST @@@ WITH V.FBOXPT AT @', ord (fboxpt)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* ENTERLLCST *) ;


$OPTIONS page $

(* ************************************ ENTERREAL ***************************** *)

    PROCEDURE enterreal (frval : real ; VAR fboxpt : rcstpt) ;

(* C   SEARCHES IN  LIST BEGINNING AT  CURRRCSTPT  IF FRVAL  EXISTS.
   IF YES  RETURNS  POINTER  ON THIS BOX
   ELSE  CREATES  A NEW BOX.
   C *)
(* E ERRORS DETECTED
   HEAPERROR
   E *)
      LABEL
        1 ;                                       (* EXIT WHILE *)
      VAR
        workpt : rcstpt ;
      BEGIN
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT ENTERREAL @@@ WITH FRVAL:', frval) ; nextline ;
	END ;
$OPTIONS compile = true $
        workpt := currrcstpt ;
        WHILE workpt # NIL DO
	IF workpt@.rvalu = frval THEN
	  BEGIN
	    GOTO 1 ;                            (* ASSIGNS FBOXPT ; EXIT PROC *)
	  END ELSE
	  workpt := workpt@.rnext ;
                                                  (* HERE NOT FOUND *)
        new (workpt) ; IF workpt = NIL THEN heaperror ; (* EXIT COMP *)
        WITH workpt@ DO
	BEGIN
	  rvalu := frval ; rplace := 0 ;        (* INIT  FUTURE CHAIN  IN UNDLAB *)
	  rnext := currrcstpt ;
	END ;
        currrcstpt := workpt ;
$OPTIONS compile = trace $
        IF stattrace = high THEN
	BEGIN
	  write (mpcogout, boxheader) ; nextline ;
	  write (mpcogout, '* REAL CONSTANT BOX CREATED AT @', ord (workpt)) ; nextline ;
	  WITH workpt@ DO
	    write (mpcogout, '* RVALU IS: ', rvalu, ' RNEXT IS AT @', ord (rnext)) ;
	  nextline ;
	  write (mpcogout, boxheader) ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* <=== *)
1 :     fboxpt := workpt ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN ENTERREAL @@@ WITH V.FBOXPT AT @', ord (fboxpt)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* ENTERREAL *) ;


$OPTIONS page $

(* ************************************ ENTERUNDLAB *************************** *)

    PROCEDURE enterundlab (VAR fundinx : integer) ;

(* C  "FUNDINX  IS  THE BEGINNING OF  A LIST  IN UNDLAB  OF  UNRESOLVED
   REFERENCES  ( 0 MEANS  NO LIST)
   THIS  PROCEDURE   ADDS A NEW OCCURENCE IN THE LIST   OR INITIATE A NEW LIST
   INDFICH = INDEX IN FICHINTER  OF INCOMPLETE  INSTRUCTION
   CHNIX    POINTS  BEGINNING OF  FREE LIST
   C *)
(* E ERRORS DETECTED
   261: TOO MANY UNRESOLVED REFERENCES   (UNDLAB  FULL )
   E *)
      VAR
        it : integer ;
      BEGIN                                       (* ENTERUNDLAB *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT ENTERUNDLAB @@@ WITH INDFICH, CHNIX',
	    indfich : 6, chnix : 6,
	    ' FUNDINX (IN) IS ', fundinx : 6) ; nextline ;
	END ;
$OPTIONS compile = true $
        IF chnix = 0 THEN                         (* UNDLAB IS FULL *)
	error (261) ELSE
	WITH undlab [chnix] DO
	  BEGIN
	    place := indfich ; it := succ ;     (* FUTURE  BEGINNING OF FREE LIST *)
	    succ := fundinx ; fundinx := chnix ;
	    chnix := it ;
	  END ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN ENTERUNDLAB @@@ WITH FUNDINX(OUT), NEW CHNIX ', fundinx : 6,
	    chnix : 6) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* ENTERUNDLAB *) ;



$OPTIONS page $

(* ************************************ GENEXCEPTCODE ************************* *)

    PROCEDURE genexceptcode (ferrcode : integer ; freg : register) ;

(* C  CALL OF AN OPERATOR   THAT  MUST
   . PRINTS   VALUE, OFFSET , ERROR MSG
   . STOPS    EXECUTION
   C *)
      VAR
        lcode : integer ;
      BEGIN                                       (* GENEXCEPTCODE *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT_FIN DE GENEXCEPTCODE @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        CASE freg OF
	ra : lcode := 1 ;
	rq : lcode := 2 ;
	raq : lcode := 4 ;
	reaq : lcode := 8 ;
        END (* CASE FREG *) ;
        genstand (nreg, ferrcode, ieax5, tn) ;
        genstand (nreg, lcode, ieax6, tn) ;
        genstand (pr0, exceptcodeplace, itsp3, tn) ;
      END (* GENEXCEPTCODE *) ;


$OPTIONS page $

(* ************************************ GENCSTECODE *************************** *)

    PROCEDURE gencstecode (farg : integer ; finst : istand) ;

(* C  .AN INSTRUCTION WITH CSTE FARG ARGUMENT MUST BE GENERATE .
   .THIS  PROCEDURE CHECK FOR SINGLE INSTRUCTION , OR FOR LARGE CSTE .
   .IF LARGE CSTE, ENTERS IT IN WORD_CSTE LIST  AND USES UNRESOLVED
   MECHANISM
   C *)
      VAR
        short : boolean ;
        locboxpt : wcstpt ;
      BEGIN                                       (* GENCSTECODE *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT GENCSTECODE @@@ WITH FARG,FINST', farg,
	    codesymb [finst] : 9) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        short := false ;
        IF (farg >= 0) THEN
	IF farg < twoto18 THEN
	  short := true ;
        IF short THEN
	genstand (nreg, farg, finst, tdl) ELSE
	BEGIN                                   (* NOT SHORT *)
	  entercst (farg, locboxpt) ;
	  enterundlab (locboxpt@.cstplace) ;
                                                  (* ADDS A NEW OCCUR.  OF "FARG" *)
                                                  (* IN CHAIN OF UNRESOLVED OCCURENCE *)
	  genstand (nreg, 0, finst, tic) ;
	END (* NOT SHORT *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN GENCSTECODE @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* GENCSTECODE *) ;


$OPTIONS page $

(* ************************************** GENCHECKMULTOVER ******************************************** *)

    PROCEDURE gencheckmultover ;

(* C THIS PROCEDURE GENERATES CODE TO CHECK OVERFLOW AFTER MPY INSTRUCTION *)

      VAR
        locskip : integer ;

      BEGIN
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT GENCHECKMULTOVER @@@') ;
	  nextline
	END ;
$OPTIONS compile = true $
        genstand (pr6, evareaw, istaq, tn) ;
        genstand (pr6, evareaw + 1, ilda, tn) ;
        genstand (nreg, 36, ilrs, tn) ;
        genstand (pr6, evareaw, icmpaq, tn) ;
        locskip := indfich ;
        genstand (nreg, 0, itze, tic) ;
        genstand (pr6, evareaw, ildaq, tn) ;
        genexceptcode (mlterrcode, raq) ;
        inser (cb, locskip) ;
        genstand (nreg, 0, iorq, tdl) ;
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ FIN GENCHECKMULTOVER @@@') ;
	  nextline
	END ;
$OPTIONS compile = true $
      END (* GENCHECKMULTOVER *) ;


$OPTIONS page $

(* ************************************ CHECKBNDS ***************************** *)

    PROCEDURE checkbnds (errcode : integer ; freg : register ; fctp : ctp) ;

(* C .GENERATES  THE CODE TO VERIFY IF THE VALUE IN FREG IS IN THE CLOSED
   INTERVAL  GIVEN BY THE BOUNDS OF THE TYPE  "FCTP".
   .IF ERROR, CALL   GENEXCEPTCODE
   C *)
      VAR
        lmin, lmax, locskip, locexit : integer ;
        linst : istand ;
      BEGIN                                       (* CHECKBNDS *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT CHECKBNDS @@@ WITH CODE,FREG, FCTP AT', errcode : 4,
	    regname [freg] : 9, ord (fctp)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        IF fctp # intptr THEN
	BEGIN                                   (* ONLY FOR TYPE # INTEGER *)
	  findminmax (fctp, lmin, lmax) ;
	  IF freg = ra THEN linst := icmpa ELSE linst := icmpq ;
	  gencstecode (lmin, linst) ;
	  locskip := indfich ; genstand (nreg, 0, itmi, tic) ; (* SKIP  IF ERROR *)
	  gencstecode (lmax, linst) ;
	  locexit := indfich ; genstand (nreg, 0, itmoz, tic) ; (* SKIP IF OK *)
	  inser (cb, locskip) ;
	  genexceptcode (errcode, freg) ;
	  inser (cb, locexit) ;
	END (* TYPE NOT INTEGER *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN CHECKBNDS @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* CHECKBNDS *) ;


$OPTIONS page $

(* ************************************ FCT. INBOUNDS ************************* *)

    FUNCTION inbounds (fval, fmin, fmax : integer) : boolean ;

(* C  RETURNED VALUE IS  TRUE  IF  FVAL IS THE  CLOSED  INTERVAL
   FMIN..FMAX
   FALSE  OTHERWISE
   C *)
(* E ERRORS DETECTED
   406 :  FMIN  EXPECTED  TO  BE  <  FMAX
   E *)
      BEGIN                                       (* INBOUNDS *)
$OPTIONS compile = security $
        IF fmin > fmax THEN error (406) ;
$OPTIONS compile = true $
        IF fval < fmin THEN
	inbounds := false ELSE
	IF fval > fmax THEN
	  inbounds := false ELSE
	  inbounds := true ;
      END (* INBOUNDS *) ;


$OPTIONS page $

(* ************************************************   clearpsr  ******** *)

    PROCEDURE clearpsr ;
      BEGIN
        mfari1 := a0r0i0 ; mfari2 := a1r0i0 ;
        geneism (imlr, 0, p0t0r0) ;
        gendesca (nreg, 0, 0, l9, 0, tn) ;
        gendesca (pr6, psrdepw, 0, l9, bytesforset, tn) ;
      END ;


$OPTIONS page $

(* ************************************ FUNCTION OLDNEWSTOR ******************* *)

    FUNCTION oldnewstor (incrinbytes : integer) : integer ;

(* C   THIS FCT.  RETURNS  THE OLD VALUE  REALIGNED  OF  TEMPSTOR;
   INCREMENTS    TEMPSTOR   FOR FUTURE  USE;
   READJUST TMAX IF NECESSARY
   C *)
      BEGIN                                       (* OLDNEWSTOR *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT-FIN     OLDNEWSTOR  @@@ WITH TEMPSTOR,TMAX, INCREMENT',
	    tempstor, tmax, incrinbytes) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        incrinbytes := recadre (incrinbytes, bytesinword) ;
        IF incrinbytes > bytesinword THEN
	tempstor := recadre (tempstor, bytesindword) ;
                                                  (* <====== *)
        oldnewstor := tempstor ;
        tempstor := tempstor + incrinbytes ;
        IF tempstor > tmax THEN tmax := tempstor ;
      END (* OLDNEWSTOR *) ;


$OPTIONS page $

(* ************************************ NEWBLOC ******************************* *)

    PROCEDURE newbloc (freg : register) ;

(* C .CREATES A NEW REGISTER BLOC ASSOCIATED WITH "FREG"
   .RETURNS  -CURRENTBLOC
   -REGCHARGE[FREG]
   C *)
(* E ERRORS DETECTED
   254 : EXPRESSION TOO COMPLICATED
   E *)
      VAR
        lcurbloc : regpt ;
      BEGIN                                       (* NEWBLOC *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT NEWBLOC @@@ WITH FREG', regname [freg]) ; nextline ;
	END ;
$OPTIONS compile = true $
        IF begfreelist = NIL THEN error (254) ELSE
	BEGIN
	  lcurbloc := begfreelist ;
	  begfreelist := begfreelist@.nextbloc ;
	  WITH lcurbloc@ DO
	    BEGIN
	      sregister := freg ; saveplace := 0 ; nextbloc := currentbloc ; predbloc := NIL ;
	    END ;
	  forgetbox := forgetbox + 1 ;
	  currentbloc@.predbloc := lcurbloc ;
	  currentbloc := lcurbloc ;
	END ;
        regcharge [freg] := true ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '* BOX CREATED AT @', ord (currentbloc), ' PREVIOUS WAS AT @',
	    ord (currentbloc@.nextbloc)) ;
	  nextline ;
	  write (mpcogout, '@@@ FIN NEWBLOC @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* NEWBLOC *) ;


$OPTIONS page $

(* ************************************ FREEBLOC ****************************** *)

    PROCEDURE freebloc (VAR fbtofree : regpt) ;

(* C .IN ORDER TO HAVE A SHORT CHAIN OF USED REGISTERS, THIS PROCEDURE  "DELINKS"
   A BOX,EACH TIME IT IS POSSIBLE.
   .FBTOFREE  CAN BE NIL ==> NO OPERATION
   .IF   ASSOCIATED  REGISTER  IS NOT SAVED, THEN FREES IT.
   .MODIFY CURRENTBLOC  FOR LAST CREATED BOX
   .FBTOFREE IS "NIL" AFTER, EXCEPT FOR CURRENTBLOC
   C *)
(* E ERRORS DETECTED
   417  FREEBLOC CALLED WITH DUMMYBLOC
   435  REGISTER NOT SAVED  AND NOT FLAGGED  "LOADED"
   E *)
      VAR
        savecurbloc : regpt ;
      BEGIN                                       (* FREEBLOC *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT -FIN DE FREEBLOC @@@') ; nextline ;
	  IF stattrace = high THEN
	    BEGIN
	      write (mpcogout, '* THE FOLLOWING BOX HAS BEEN FREED:') ; nextline ;
	      printregbox (fbtofree) ;
	    END ;
	END ;
$OPTIONS compile = true $
$OPTIONS compile = security $
        IF fbtofree = dummybloc THEN error (417) ELSE
$OPTIONS compile = true $
	IF fbtofree # NIL THEN
	  WITH fbtofree@ DO
	    BEGIN
	      IF predbloc = NIL THEN
	        BEGIN
		savecurbloc := nextbloc ; nextbloc@.predbloc := NIL ;
	        END ELSE
	        BEGIN
		predbloc@.nextbloc := nextbloc ; nextbloc@.predbloc := predbloc ;
		savecurbloc := currentbloc ;
	        END ;
	      IF saveplace = 0 THEN
$OPTIONS cc = secuity + $
	        IF NOT regcharge [sregister] THEN error (435) ELSE
$OPTIONS cc = secuity - $
		regcharge [sregister] := false ;
	      forgetbox := forgetbox - 1 ;
	      fbtofree@.nextbloc := begfreelist ;
	      begfreelist := fbtofree ;
	      fbtofree := NIL ;
	      currentbloc := savecurbloc ;
	    END (* WITH,#NIL *) ;
      END (* FREEBLOC *) ;


$OPTIONS page $

    PROCEDURE sauvereg (freg : register ; fload : boolean) ; FORWARD ;

(* *********************** STACK_EXTENSION ************************* *)

    PROCEDURE stack_extension ;

(* THIS PROCEDUREIS CALLED FOR DYNAMIC STACK EXTENSIONS *)

(* GENERATED CODE ASSUMES THAT RQ CONTAINS NUMBER OF WORDS *)
(* PR5, MODIFIED BY pascal_operators_ MUST BE SAVED IF USED *)

      BEGIN
        IF NOT stack_has_been_extended THEN
	BEGIN
	  stack_has_been_extended := true ;
	  saved_stack_end_place := oldnewstor (bytesindword) DIV bytesinword ;
	  genstand (pr6, next_sp_place, iepp3, tny) ;
	  genstand (pr6, saved_stack_end_place, ispri3, tn) ;
	END ;
        sauvereg (pr5, false) ;
        genstand (pr0, extend_stack_op_place, itsp3, tn) ;
      END ;

$OPTIONS page $

(* ************************************ FREEALLREGISTERS ********************** *)

    PROCEDURE freeallregisters ;

(* C .FOR EACH STATEMENT'S BEGINNING , ALL REGISTERS ARE FREE
   .ALL THE CREATED BOXES ARE REMOVED
   .THE WORKING STORAGE IS FREED
   * LCSAVE = MEMORIZED  AVAILABLE  STORAGE  IN CURRENT FRAME (BYTES)
   * TEMPSTOR  = CURRENT AVAILABLE  STORAGE  IN CURRENT FRAME (BYTES)
   * DUMMYBLOC  IS CREATED  IN  ENTERBODY  FOR ALL THE PROCEDURE
   C *)
(* E   ERRORS DETECTED
   429 SOME REGISTER BOX NOT FREED
   E *)
      VAR
        it : integer ;
      BEGIN                                       (* FREEALLREGISTERS *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT FREEALLREGISTERS @@@ WITH LCSAVE,TEMPSTOR:', lcsave,
	    tempstor) ;
	  nextline ;
	END ;
        IF forgetbox # 0 THEN
	IF errtotal = 0 THEN
	  BEGIN
	    error (429) ;
	    write (mpcogout, '******** FORGETBOX IS :', forgetbox) ; nextline ;
	  END ;
$OPTIONS compile = true $
        FOR it := forgetbox DOWNTO 1 DO
	freebloc (currentbloc) ;                (* FREE FORGET BOXES *)
        regcharge := freereg ;
        workformaths := false ;
        IF stack_has_been_extended THEN
	BEGIN
	  stack_has_been_extended := false ;
	  genstand (pr6, saved_stack_end_place, iepp1, tny) ;
	  genstand (pr0, reset_stack_end_op_place, itsp3, tn) ;
	END ;
        tempstor := lcsave ;
        forgetbox := 0 ;
        currentbloc := dummybloc ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN FREEALLREGISTERS @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* FREEALLREGISTERS *) ;

$OPTIONS page $


(* ************************************ FCT  RAISUSED ************************* *)

    FUNCTION raisused : boolean ;

(* TRUE IF A-REGISTER IS USED (MAY BE A,AQ,EAQ) *)
      BEGIN                                       (* RAISUSED *)
        raisused := true ;
        IF NOT regcharge [ra] THEN
	IF NOT regcharge [raq] THEN
	  IF NOT regcharge [reaq] THEN
	    raisused := false ;
      END (* RAISUSED *) ;

$OPTIONS page $


(* ************************************ FCT.  RQISUSED ************************ *)

    FUNCTION rqisused : boolean ;

(* TRUE IF Q-REGISTER IS USED (MAY BE Q,AQ,EAQ) *)
      BEGIN                                       (* RQISUSED *)
        rqisused := true ;
        IF NOT regcharge [rq] THEN
	IF NOT regcharge [raq] THEN
	  IF NOT regcharge [reaq] THEN
	    rqisused := false ;
      END (* RQISUSED *) ;


$OPTIONS page $

(* ************************************ FCT. RAQISUSED ************************ *)

    FUNCTION raqisused : boolean ;

(* TRUE IF AQ-REGISTER IS USED (MAY BE A,Q,AQ, *)
(* USED  EAQ  RAQ  AND  REAQ *)
      BEGIN                                       (* RAQISUSED *)
        raqisused := true ;
        IF NOT regcharge [ra] THEN
	IF NOT regcharge [rq] THEN
	  IF NOT regcharge [raq] THEN
	    IF NOT regcharge [reaq] THEN
	      raqisused := false ;
      END (* RAQISUSED *) ;



$OPTIONS page $

(* ************************************ SAUVEREG ****************************** *)
    PROCEDURE sauvereg ;

(* C  .THIS PROCEDURE MUST BE CALLED EACH TIME THE CONTENT OF A REGISTER
   WILL BE ALTERED
   .IF FREG ALREADY USED, THEN SAVE IT   AND  MEMORIZES  SAVING PLACE IN
   ASSOCIATED BOX
   .THE USED REGISTERS  ARE  CHAINED  FROM CURRENTBLOC UNTIL  DUMMYBLOC.
   .IF "FLOAD"  THEN   CREATES  A NEW BOX  AND FLAG IT  LOADED
   ELSE   SAVE IT
   .SPECIAL  CASES
   FREG=RA    THEN  CHECK  AQ ,EAQ
   =RQ          CHECK  AQ, EAQ
   =AQ          CHECK  A  Q  EAQ
   =EAQ         CHECK  A  Q  EAQ
   C *)
(* E ERRORS DETECTED
   403: BOX NOT FOUND
   404: REGISTER ALREADY SAVED
   E *)
      LABEL
        1 ;                                       (* EXIT WHILE *)
      VAR
        lreg, lregq, auxreg, auxregq : register ;
        lcurrbloc : regpt ;
        linst : istand ;
        lincr : integer ;
      BEGIN                                       (* SAUVEREG *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT SAUVEREG @@@ WITH FREG,FLOAD:', regname [freg], fload) ;
	  nextline ;
	  IF stattrace = high THEN printstatusregister ;
	END ;
$OPTIONS compile = true $
        lreg := nreg ;                            (* DEFAULT MEANS THERE IS NO REGISTER TO SAVE *)
        lregq := nreg ;
        IF regcharge [freg] THEN
	lreg := freg ELSE
	BEGIN                                   (* SPECIAL FOR ACC-QUOT *)
	  IF freg >= ra THEN
	    IF freg <= reaq THEN
	      BEGIN
	        IF regcharge [reaq] THEN lreg := reaq ELSE
		IF regcharge [raq] THEN lreg := raq ELSE
		  IF freg >= raq THEN
		    BEGIN
		      IF regcharge [ra] THEN lreg := ra ;
		      IF regcharge [rq] THEN
		        IF lreg = nreg THEN lreg := rq ELSE lregq := rq ;
		    END (* >=RAQ *) ;
	      END (* RA..REAQ *) ;
	END (* SPECIAL *) ;
        IF lreg # nreg THEN
	BEGIN
                                                  (* AT LEAST ONE TO SAVE *)
                                                  (* FIND  ASSOCIATED BOX(ES) *)
	  lcurrbloc := currentbloc ;
	  auxreg := lreg ; auxregq := lregq ;
	  WHILE lcurrbloc # NIL DO
	    WITH lcurrbloc@ DO
	      BEGIN
	        IF sregister = auxreg THEN
		BEGIN
$OPTIONS compile = trace $
		  IF saveplace # 0 THEN error (404) ;
$OPTIONS compile = true $
		  lincr := bytesinword ;      (* COMMON DEFAULT *)
		  CASE lreg OF
		    pr1, pr2, pr5, pr7 : BEGIN linst := prinst [spri, lreg] ;
		        lincr := bytesindword ;
		      END ;
		    x0, x1, x2, x3, x4, x5 : linst := xinst [sxl, lreg] ;
		    ra : linst := ista ;
		    rq : linst := istq ;
		    raq : BEGIN linst := istaq ; lincr := bytesindword ;
		      END (* RAQ *) ;
		    reaq : BEGIN linst := idfst ; lincr := bytesindword ;
		      END (* REAQ *) ;
		    psr : BEGIN linst := inop ; lincr := psrinbytes ;
		      END (* PSR *) ;
		  END (* CASE LREG *) ;
		  saveplace := oldnewstor (lincr) ;
                                                  (* SAVING  INSTR. NOW *)
		  IF linst # inop THEN
		    genstand (pr6, saveplace DIV bytesinword, linst, tn) ELSE
		    BEGIN                     (* MOVE PSR *)
		      mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
		      geneism (imlr, 0 (* FILL BYTE *), p0t0r0) ;
		      gendesca (pr6, psrdepw, 0, l9, psrinbytes, tn) ; (* ORIGIN *)
		      gendesca (pr6, saveplace DIV bytesinword, 0, l9, psrinbytes, tn) ;
		    END (* MOVE PSR *) ;
		  IF auxregq = nreg THEN GOTO 1 ; (* EXIT WHILE *)
		  auxreg := nreg ;
		END (* SREGISTER = AUXREG *) ELSE
		IF sregister = auxregq THEN
		  BEGIN
$OPTIONS compile = security $
		    IF saveplace # 0 THEN error (404) ;
$OPTIONS compile = true $
		    saveplace := oldnewstor (bytesinword) ;
		    genstand (pr6, saveplace DIV bytesinword, istq, tn) ;
		    regcharge [rq] := false ;
		    IF auxreg = nreg THEN GOTO 1 ; (* EXIT WHILE *)
		    auxregq := nreg ;
		  END (* LREGQ *) ;
	        lcurrbloc := nextbloc ;
	      END (* WITH,WHILE *) ;
                                                  (* EXIT HERE MEANS COMPILER'S ERROR *)
	  error (403) ;
1 :
	  IF lreg # freg THEN
	    regcharge [lreg] := false ;
	END (* A REGISTER TO SAVE *) ;
        IF fload THEN
	newbloc (freg) ELSE regcharge [freg] := false ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '* SAVED REGISTER(S) IS(ARE):', regname [lreg], regname [lregq]) ;
	  nextline ;
	  write (mpcogout, '@@@ FIN SAUVEREG @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* SAUVEREG *) ;


$OPTIONS page $

(* ************************************ REGENERE ****************************** *)

    PROCEDURE regenere (oldbloc : regpt) ;

(* C .OLDBLOC (NOT NIL)  POINTS A REGISTER BOX WHOSE SREGISTER MUST BE
   RELOADED  (IF NOT ALREADY LOADED FOR THIS BLOC)
   .IF PREVIOUS LOADED, IT IS SAVED
   .REGCHARGE MUST BE TRUE AFTER
   C *)
(* E ERRORS DETECTED
   427: OLDBLOC IS NIL
   428: REG  NOT SAVED,NOT LOADED
   E *)
      VAR
        ltag : tag ;
        linst : istand ;
      BEGIN                                       (* REGENERE *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT REGENERE @@@') ; nextline ;
	  IF stattrace = high THEN
	    BEGIN
	      printstatusregister ;
	      printregbox (oldbloc) ;
	    END ;
	END ;
$OPTIONS compile = true $
$OPTIONS compile = security $
        IF oldbloc = NIL THEN error (427) ELSE
	IF (oldbloc@.saveplace = 0) AND
	  (NOT regcharge [oldbloc@.sregister]) THEN error (428) ELSE
$OPTIONS compile = true $
	  WITH oldbloc@ DO
	    IF saveplace # 0 THEN
	      BEGIN (* SAVED *) ltag := tn ;
	        sauvereg (sregister, false) ;
	        CASE sregister OF
		pr1, pr2, pr5, pr7 :
		  BEGIN ltag := tny ; linst := prinst [epp, sregister] ; END ;
		x0, x1, x2, x3, x4, x5, x6, x7 : linst := xinst [lxl, sregister] ;
		ra : linst := ilda ;
		rq : linst := ildq ;
		raq : linst := ildaq ;
		reaq : linst := idfld ;
		psr : linst := inop ;
	        END (* CASE SREGISTER *) ;
	        IF linst # inop THEN
		genstand (pr6, saveplace DIV bytesinword, linst, ltag) ELSE
		BEGIN                         (* RELOAD PSR *)
		  mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
		  geneism (imlr, 0 (* FILL BYTE *), p0t0r0) ;
		  gendesca (pr6, saveplace DIV bytesinword, 0, l9, psrinbytes, tn) ;
		  gendesca (pr6, psrdepw, 0, l9, psrinbytes, tn) ;
		  psrsize := psrinbytes ;
		END (* RELOAD PSR *) ;
	        saveplace := 0 ;
	        regcharge [sregister] := true ;
	      END (* REG WAS SAVED *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN REGENERE @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* REGENERE *) ;


$OPTIONS page $

(* ************************************ GETPR ********************************* *)

    PROCEDURE getpr ;

(* C  .A NEW POINTER REGISTER IS REQUESTED
   .SEARCHS A FREE IN PR1..MAXPRUSED
   IF NONE SAVE ONE (THE LAST)
   .BY CALLING SAUVEREG
   CREATES A NEW BOX POINTED BY CURRENTBLOC ,REGCHARGE TRUE
   .RETURNS CURRENTPR
   C *)
      LABEL
        1 ;                                       (* EXIT LOOP FOR *)
      VAR
        lpr : preg ;
      BEGIN                                       (* GETPR *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT GETPR @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        FOR lpr := pr1 TO maxprused DO
	IF NOT regcharge [lpr] THEN
	  GOTO 1 ;                              (* EXIT LOOP WITH LPR OK *)
                                                  (* HERE  ALL PRI'S ALREADY LOADED. *)
                                                  (* LPR BECOMES MAXPRUSED *)
        lpr := maxprused ;
1 :
        sauvereg (lpr, true) ;                    (* CURRENTBLOC, REGCHARGE OK *)
        currentpr := lpr ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN GETPR @@@ WITH CURRENTPR:', regname [currentpr]) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* GETPR *) ;


$OPTIONS page $

(* ************************************ GETINDEX ****************************** *)

    PROCEDURE getindex ;

(* C  .A NEW INDEX REGISTER IS REQUESTED.
   .SEARCHES A FREE ONE IN X0..MAXINXUSED
   IF NONE,SAVE ONE (ALWAYS THE LAST)
   .BY CALLING SAUVEREG
   CREATES A NEW BLOC ,REGCHARGE OK
   .RETURNS CURRENTINDEX
   C *)
      LABEL
        1 ;                                       (* EXIT FOR *)
      VAR
        linx : register ;
      BEGIN                                       (* GETINDEX *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN write (mpcogout, '@@@ DEBUT GETINDEX @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        FOR linx := x0 TO maxinxused DO
	IF NOT regcharge [linx] THEN
	  GOTO 1 ;                              (* EXIT LOOP WITH LINX OK *)
                                                  (* HERE ALL XI'S ALREADY LOADED. *)
                                                  (* SELECT MAXINXUSED *)
        linx := maxinxused ;
1 :
        sauvereg (linx, true) ;                   (* CURRENTBLOC, REGCHARGE OK *)
        currentindex := linx ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN GETINDEX @@@ WITH CURRENTINDEX:', regname [currentindex]) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* GETINDEX *) ;


$OPTIONS page $

(* ************************** LOADBASE **************************************** *)

    PROCEDURE loadbase (flev : integer) ;

(* C
   THIS PROCEDURE LOADS A POINTER REGISTER WITH THE BASIS OF THE STACK FRAME
   OF THE PROCEDURE DEFINED AT THE LEVEL "FLEV" ;
   IN EACH FRAME, AT DISPLACEMENT DLKDEP, THERE IS AN ITS PAIR POINTING THE
   FRAME OF THE LOGICAL MOTHER-PROCEDURE , PREPARED BY CALLING SEQUENCE,
   STORED BY ENTRY SEQUENCE
   C *)
      VAR
        it : integer ;
        linst : istand ;
      BEGIN                                       (* LOADBASE *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT LOADBASE @@@ WITH FLEV', flev : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* OBTAINS A FREE PR  IN "CURRENTPR" *)
        getpr ;                                   (* CURRENTBLOC ASSIGNED HERE *)
        linst := prinst [epp, currentpr] ;
        genstand (pr6, dlkdepw, linst, tny) ;     (* LOGICAL MOTHER *)
        FOR it := 1 TO level - flev - 1 DO
	genstand (currentpr, dlkdepw, linst, tny) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN LOADBASE @@@ WITH  CURRENTPR: ', regname [currentpr],
	    ' CURRENTBLOC AT @ ', ord (currentbloc), ' LEVEL IS:', level : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* LOADBASE *) ;


$OPTIONS page $

(* ************************************ ADDRESSVAR **************************** *)

    PROCEDURE addressvar (fctp : ctp ; VAR fattr : attr ; modif : boolean) ;

(* C ."FCTP" IS A  NOT NIL POINTER ON A CONTEXTTABLE BOX
   * VARS PROC FIELD
   .WITH BOX'S INFORMATIONS,BUILDS A "VARBL"FATTR.,USED TO ADDRESS POINTED ITEM
   . ONE FIELD  OF FCTP@ CAN BE ALTERED  .VISUSED
   C *)
(* E ERRORS DETECTED
   438 FCTP IS NIL
   E *)
      VAR
        it : integer ;

      BEGIN                                       (* ADDRESSVAR *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT ADDRESSVAR @@@ WITH FCTP AT @', ord (fctp)) ; nextline ;
	END ;
        IF fctp = NIL THEN error (438) ELSE
$OPTIONS compile = true $
	WITH fctp@ (* POINTED BOX *), fattr (* BUILT ATTR *) DO
	  BEGIN
                                                  (* COMMON FIELDS *)
	    initattrvarbl (fattr) ;
	    nameaddr := ctptr ;
                                                  (* NOW THREE MAJOR CASES VARS-FIELD-PROC *)
	    IF klass = vars THEN
	      BEGIN
	        typtr := vtype ; vlev := vlevel ;
	        pckd := false ;
	        IF vtype # NIL THEN
		IF vtype@.form = power THEN pckd := vtype@.pack ;
	        visused := true ;               (* FOR FCTP@ *)
	        IF vtype = NIL THEN
		BEGIN
		  dplmt := 0 ; itsdplmt := 0 ; basebloc := NIL ; basereg := pr6 ; vlev := level ;
		END ELSE
		IF vkind = actual THEN
		  IF vtype^.form = files THEN
		    BEGIN
		      itsdplmt := 0 ; dplmt := 0 ;
		      IF vlev = 0 THEN
		        BEGIN
			basereg := prstatic ; basebloc := NIL ; access := pointable ;
			itsdplmt := vaddr ;
		        END ELSE
		        IF vlev = level THEN
			BEGIN
			  basereg := pr6 ; basebloc := NIL ; access := pointable ;
			  itsdplmt := vaddr ;
			END ELSE
			BEGIN
			  loadbase (vlev) ;
			  basereg := currentpr ; basebloc := currentbloc ; access := pointable ;
			  itsdplmt := vaddr ;
			END ;
		    END ELSE
		    BEGIN
		      itsdplmt := 0 ; dplmt := vaddr ;
		      IF vlev = 0 THEN
		        BEGIN                 (* GLOBAL *)
			basereg := prstatic ; basebloc := NIL ; access := direct ;
		        END (* GLOBAL *) ELSE
		        IF vlev = level THEN
			BEGIN               (* LOCAL *)
			  basereg := pr6 ; basebloc := NIL ; access := direct ;
			END (* LOCAL *) ELSE
			BEGIN               (* INTERM. *)
			  loadbase (vlev) ; (* RETURNS CURRENTPR,CURRENTBLOC *)
			  basereg := currentpr ; basebloc := currentbloc ; access := pointee ;
			END (* INTERM. *) ;
		    END (* ACTUAL *) ELSE
		  IF vkind = formal THEN
		    BEGIN
		      itsdplmt := vaddr ; dplmt := 0 ; access := pointable ;
		      IF vlev = level THEN
		        BEGIN                 (* LOCAL PARM *)
			basereg := pr6 ; basebloc := NIL ;
		        END (* LOCAL *) ELSE
		        BEGIN                 (* INTERM. *)
			loadbase (vlev) ;   (* RETURNS CURRENTPR,CURRENTBLOC *)
			basereg := currentpr ; basebloc := currentbloc ;
		        END (* INTERM. *) ;
		    END (* FORMAL *) ELSE
		    IF vkind = arraybound THEN
		      BEGIN
		        itsdplmt := vaddr ;
		        dplmt := vdispl ; access := pointable ;
		        IF vlev = level THEN
			BEGIN
			  basereg := pr6 ; basebloc := NIL
			END ELSE
			BEGIN
			  loadbase (vlev) ;
			  basereg := currentpr ; basebloc := currentbloc
			END ;
		      END (* ARRAYBOUND *) ELSE
		      BEGIN                   (* IMPORTED,EXPORTABLE *)
		        basereg := prlink ; basebloc := NIL ; dplmt := 0 ; access := pointable ;
		        itsdplmt := vaddr ;
		      END (* EXTERNAL *) ;
	      END (* KLASS=VARS *) ELSE
	      IF klass = field THEN             (* FOUND UNDER A WITH *)
                                                  (* RECORD POINTED BY DISPLAY[DISX] *)
	        BEGIN
		typtr := fldtype ; basebloc := NIL ;
		WITH display [disx] DO
		  IF occur = cwith THEN       (* NOT PACKED, EASY TO ADDRESS *)
		    BEGIN
		      vlev := clevel ; pckd := false ; itsdplmt := 0 ;
		      IF vlev = 0 THEN basereg := prstatic ELSE basereg := pr6 ;
		      dplmt := cdspl + fldaddr ; access := direct ;
		      IF symbolmap THEN
		        FOR it := 1 TO creflist.nbr DO
			IF modif THEN nameisref (creflist.symbolp [it], symbolfile, -symbolline)
			ELSE nameisref (creflist.symbolp [it], symbolfile, symbolline) ;
		    END (* CWITH *) ELSE
		    BEGIN                     (* VWITH *)
                                                  (* VDSPL IS AN POINTER *)
                                                  (* STORED BY WITHSTAT *)
		      vlev := level ; itsdplmt := vdspl ; basereg := pr6 ;
		      dplmt := fldaddr ; access := pointable ;
		      IF typtr@.form <= scalar THEN
		        pckd := bytwidth < bytesinword ELSE pckd := (vpack OR typtr@.pack) ;
		      IF symbolmap THEN
		        FOR it := 1 TO vreflist.nbr DO
			IF modif THEN nameisref (vreflist.symbolp [it], symbolfile, -symbolline)
			ELSE nameisref (vreflist.symbolp [it], symbolfile, symbolline) ;
		    END (* VWITH, WITH DISPLAY *) ;
	        END (* FIELD *) ELSE
	        BEGIN                           (* KLASS = PROC *)
                                                  (* FOR A FUNCTION ASSIGNMENT *)
		typtr := proctype ; pckd := false ; itsdplmt := 0 ;
		vlev := proclevel + 1 ;
		IF vlev = level THEN
		  BEGIN
		    access := direct ; basereg := pr6 ; basebloc := NIL ;
		  END ELSE
		  BEGIN
		    loadbase (vlev) ;
		    access := pointee ; basereg := currentpr ; basebloc := currentbloc ;
		  END ;
		dplmt := fctdepl ;
                                                  (* USE OF RESERVED WORDS *)
                                                  (* IN CURRENT STACK FRAME *)
	        END (* PROC *) ;
	  END (* WITH FCTP@,FATTR *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  IF (stattrace = high) AND (fctp # NIL) THEN
	    printattr (fattr) ;
	  write (mpcogout, '@@@ FIN ADDRESSVAR @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* ADDRESSVAR *) ;


$OPTIONS page $

(* ************************************ CALCVARIENT *************************** *)

    PROCEDURE calcvarient (VAR fattr : attr ; VAR fbase : preg ; VAR fdisp : integer ;
      VAR ftag : tag) ;

(* C  GIVEN A FATTR ( IF LVAL THEN CHANGED HERE)
   TYPTR # NIL
   KIND= VARBL OR SVAL (NOT POWER) OR SAVED LVAL
   THIS PROCEDURE RETURNS THREE ITEMS NEEDED TO ADDRESS THE "WORD"
   FBASE
   FDISP    EXPRESSED IN WORDS
   FTAG
   FOR SVAL, INSTRUCTION MUST BE GENERATE AFTER CALL
   C *)
(* E ERRORS DETECTED
   412  TYPTR IS NIL
   413  KIND=LVAL (NOT SAVED)
   414  KIND=CHAIN OR LCOND
   E *)
      VAR
        locdepw, locmemw : integer ;
        wretpt : wcstpt ;
        rretpt : rcstpt ;
        lretpt : lcstpt ;
        llretpt : llcstpt ;
        linst : istand ;
$OPTIONS compile = true $
$OPTIONS compile = security $
        ltag : tag ;
      BEGIN                                       (* CALCVARIENT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT CALCVARIENT @@@') ; nextline ;
	  IF stattrace = high THEN
	    printattr (fattr) ;
	END ;
        IF fattr.typtr = NIL THEN error (412) ELSE
$OPTIONS compile = true $
	fbase := nreg ; fdisp := 0 ; ftag := tn ;
        IF fattr.kind = lval THEN
	lvalvarbl (fattr) ;
        WITH fattr DO
	IF kind = varbl THEN
	  BEGIN
	    IF basereg <= maxprused THEN
	      regenere (basebloc) ;
	    IF inxreg # nxreg THEN
	      IF inxbloc@.saveplace # 0 THEN
	        BEGIN
		IF NOT rqisused THEN inxreg := rq ELSE
		  IF NOT raisused THEN inxreg := ra ELSE inxreg := x6 ;
		inxbloc@.sregister := inxreg ;
		regenere (inxbloc) ;
	        END (* MODIFIER SAVED *) ;
	    locdepw := dplmt DIV bytesinword ;
	    locmemw := inxmem DIV bytesinword ;
	    fbase := basereg ;                  (* <=== *)
	    IF access = pointable THEN
	      BEGIN
	        fdisp := itsdplmt DIV bytesinword ; (* <=== *)
	        IF (fdisp >= twoto14) OR (fdisp < -twoto14) THEN
		BEGIN
		  genstand (nreg, fdisp, ieax7, tn) ;
		  freebloc (basebloc) ; getpr ;
		  genstand (basereg, 0, prinst [epp, currentpr], tx7) ;
		  fdisp := 0 ; basebloc := currentbloc ; basereg := currentpr ;
		END ;
	        IF inxreg = nxreg THEN
		BEGIN
		  IF locmemw = 0 THEN
		    BEGIN                     (* NO STORAGE MODIFIER *)
		      IF locdepw = 0 THEN
		        ftag := tny ELSE
		        BEGIN
			genstand (nreg, locdepw, ieax7, tn) ; ftag := tyx7 ;
		        END (* LOCDEPW#0 *) ;
		    END (* LOCMEMW=0 *) ELSE
		    BEGIN                     (* LOCMEMW#0 *)
		      genstand (pr6, locmemw, ilxl7, tn) ;
		      IF locdepw # 0 THEN
		        genstand (nreg, locdepw, iadlx7, tdu) ;
		      ftag := tyx7 ;
		    END (* LOCMEMW #0 *) ;
		END (* INXREG=NXREG *) ELSE
		BEGIN                         (* INXREG =RA RQ XI *)
		  IF locdepw # 0 THEN
		    BEGIN
		      IF inxreg = rq THEN
		        BEGIN
			linst := iadq ; ltag := tdl ;
		        END (* RQ *) ELSE
		        IF inxreg = ra THEN
			BEGIN
			  linst := iada ; ltag := tdl ;
			END (* RA *) ELSE
			BEGIN               (* XI *)
			  linst := xinst [adlx, inxreg] ; ltag := tdu ;
			END (* XI *) ;
		      genstand (nreg, locdepw, linst, ltag) ;
		    END (* LOCDEPW *) ;
		  IF locmemw # 0 THEN
		    BEGIN                     (* STORAGE MODIFIER *)
		      IF inxreg = rq THEN
		        linst := iadq ELSE
		        IF inxreg = ra THEN
			linst := iada ELSE
			linst := ilxl7 ;
		      genstand (pr6, locmemw, linst, tn) ;
		      IF linst = ilxl7 THEN
		        BEGIN                 (* CUMUL WITH PREVIOUS INXREG *)
			genstand (pr6, evareaw, istx7, tn) ; (* STORE IN  0..17 *)
			genstand (pr6, evareaw, xinst [adlx, inxreg], tn) ;
		        END (* ILXL7 *) ;
		    END (* LOCMEMW# 0 *) ;
                                                  (* <=== *)
		  ftag := starmodif [inxreg] ;
		END (* INXREG RA RQ XI *) ;
	      END (* ACCESS POINTABLE *) ELSE
	      BEGIN                             (* POINTEE,DIRECT *)
                                                  (* <=== *)
	        fdisp := locdepw ;
	        IF (fdisp >= twoto14) OR (fdisp < -twoto14) THEN
		BEGIN
		  IF inxreg = nxreg THEN
		    BEGIN
		      genstand (nreg, fdisp, ieax7, tn) ;
		      inxreg := x7 ;
		    END
		  ELSE
		    BEGIN
		      IF inxreg = rq THEN linst := iadq
		      ELSE IF inxreg = ra THEN linst := iada
		        ELSE linst := xinst [adlx, inxreg] ;
		      genstand (nreg, fdisp, linst, tdl) ;
		    END ;
		  fdisp := 0 ;
		END ;
	        IF inxreg = nxreg THEN
		BEGIN
		  IF locmemw = 0 THEN
		    ftag := tn ELSE
		    BEGIN
		      genstand (pr6, locmemw, ilxl7, tn) ; ftag := tx7 ;
		    END (* LOCMEM#0 *) ;
		END (* NXREG *) ELSE
		BEGIN                         (* # NXREG *)
		  IF locmemw # 0 THEN
		    BEGIN                     (* CUMUL *)
		      IF inxreg = rq THEN
		        linst := iadq ELSE
		        IF inxreg = ra THEN
			linst := iada ELSE linst := ilxl7 ;
		      genstand (pr6, locmemw, linst, tn) ;
		      IF linst = ilxl7 THEN
		        BEGIN
			genstand (pr6, evareaw, istx7, tn) ;
			genstand (pr6, evareaw, xinst [adlx, inxreg], tn) ;
		        END (* ILXL7 *) ;
		    END (* CUMUL *) ;
                                                  (* <==== *)
		  ftag := modif [inxreg] ;
		END (* # NXREG *) ;
	      END (* POINTEE,DIRECT *) ;
	    freebloc (basebloc) ;
	    freebloc (inxbloc) ;
	  END (* KIND=VARBL *) ELSE
	  IF kind = sval THEN
	    BEGIN (* <=== *) fbase := nreg ; fdisp := 0 ; ftag := tic ;
	      IF typtr@.form = power THEN
	        BEGIN
		IF longv = bytesindword THEN
		  BEGIN
		    enterlcst (valpw, lretpt) ; enterundlab (lretpt@.lplace) ;
		  END ELSE
		  BEGIN
		    enterllcst (valpw, llretpt) ; enterundlab (llretpt@.llplace) ;
		  END ;
	        END ELSE
	        IF typtr = nilptr THEN
		BEGIN
		  enterlcst (nilpseudoset, lretpt) ;
		  enterundlab (lretpt@.lplace) ;
		END (* NIL *) ELSE
		IF typtr = realptr THEN
		  BEGIN
		    enterreal (rsval, rretpt) ;
		    enterundlab (rretpt@.rplace) ;
		  END (* REAL *) ELSE
		  IF inbounds (val, 0, twoto17m1) THEN
		    BEGIN
                                                  (* <=== *) fdisp := val ; ftag := tdl ;
		    END ELSE
		    BEGIN
		      entercst (val, wretpt) ;
		      enterundlab (wretpt@.cstplace) ;
		    END ;
	    END                                 (* SVAL *)
$OPTIONS compile = trace $
	  ELSE
	    IF kind = lval THEN error (413) ELSE error (414)
$OPTIONS compile = true $
	      ;
                                                  (* END WITH FATTR *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN CALCVARIENT @@@ WITH FBASE,FDISP,FTAG: ', regname [fbase],
	    fdisp : 12, tagsymb [ftag] : 5) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* CALCVARIENT *) ;


$OPTIONS page $

(* ************************************ LOADADR ******************************* *)

    PROCEDURE loadadr (VAR fattr : attr ; wantedpr : preg) ;

(* C ."FATTR" DESCRIBES A VARBL OR A CHAIN
   .THIS PROC LOADS A PR. WITH THE COMPLETE ADDRESS OF ITEM
   .IF WANTEDPR = NREG THEN RETURNS CURRENTBLOC, CURRENTPR
   ELSE LOADS ONLY WANTEDPR WITHOUT SAVING ANYTHING
   .FREES BASEBLOC, INXBLOC
   C *)
(* E ERRORS DETECTED
   405: FATTR MUST BE CHAIN OR VARBL
   E *)
      VAR
        linst : istand ;
        ended : boolean ;
        prtoload, lbase : preg ;
        locdep : integer ;
      BEGIN                                       (* LOADADR *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT LOADADR @@@ WITH WANTEDPR ', regname [wantedpr]) ;
	  nextline ;
	  IF stattrace = high THEN
	    printattr (fattr) ;
	END ;
$OPTIONS compile = true $
        lbase := nreg ;
        WITH fattr DO
	IF kind = varbl THEN
	  BEGIN
	    IF wantedpr = nreg THEN
	      BEGIN
	        IF basebloc = NIL THEN          (* BASEREG PR4 OR PR6 *)
		BEGIN
		  getpr ;
		  lbase := basereg ; prtoload := currentpr ;
		END (* NIL *) ELSE
		BEGIN
		  IF basebloc@.saveplace = 0 THEN
		    BEGIN
		      freebloc (basebloc) ; newbloc (basereg) ;
		      currentpr := basereg ;
		      lbase := basereg ; prtoload := basereg ;
		    END ELSE
		    BEGIN                     (* SAVED *)
		      getpr ;
		      genstand (pr6, basebloc@.saveplace DIV bytesinword,
		        prinst [epp, currentpr], tny) ;
		      lbase := currentpr ; prtoload := currentpr ;
		      freebloc (basebloc) ;
		    END (* SAVED *) ;
		END (* # PR4,PR6 *) ;
	      END (* WANTEDPR=NREG *) ELSE
	      BEGIN
	        prtoload := wantedpr ;
	        IF basebloc = NIL THEN
		lbase := basereg ELSE
		IF basebloc@.saveplace = 0 THEN
		  BEGIN
		    freebloc (basebloc) ; lbase := basereg ;
		  END ELSE
		  BEGIN
		    genstand (pr6, basebloc@.saveplace DIV bytesinword,
		      prinst [epp, wantedpr], tny) ;
		    lbase := wantedpr ; freebloc (basebloc) ;
		  END ;
	      END (* WANTEDPR #NREG *) ;
	    IF access = pointable THEN
	      BEGIN
	        locdep := itsdplmt DIV bytesinword ;
	        IF (locdep >= twoto14) OR (locdep < -twoto14) THEN
		BEGIN
		  genstand (nreg, locdep, ieax7, tn) ;
		  genstand (lbase, 0, prinst [epp, prtoload], tx7y) ;
		END
	        ELSE
		genstand (lbase, locdep, prinst [epp, prtoload], tny) ;
	        access := pointee ;
	        lbase := prtoload ;
                                                  (* BASEREG, BASEBLOC,ITSDPLMT *)
                                                  (* NO MORE MEANINGS FULL *)
	      END ;
	    IF inxreg # nxreg THEN
	      BEGIN
	        IF inxbloc@.saveplace # 0 THEN
		BEGIN
		  IF NOT raisused THEN inxreg := ra ELSE
		    IF NOT rqisused THEN inxreg := rq ELSE inxreg := x6 ;
		  inxbloc@.sregister := inxreg ;
		  regenere (inxbloc) ;
		END ;
	      END ;
	    IF inxmem # 0 THEN
	      BEGIN
	        IF inxreg = ra THEN
		linst := iada ELSE
		IF inxreg = rq THEN
		  linst := iadq ELSE
		  IF inxreg = nxreg THEN
		    BEGIN
		      inxreg := x7 ; linst := ilxl7 ;
		    END ELSE
		    linst := inop ;
	        IF linst # inop THEN
		BEGIN
		  genstand (pr6, inxmem DIV bytesinword, linst, tn) ;
		  inxmem := 0 ;
		END ;
	      END ;
	    ended := false ;
	    IF prtoload = lbase THEN
	      IF dplmt = 0 THEN
	        IF inxreg = nxreg THEN
		ended := true ;
	    IF NOT ended THEN
	      BEGIN
	        locdep := dplmt DIV bytesinword ;
	        IF dplmt MOD bytesinword <> 0 THEN
		IF dplmt < 0 THEN
		  locdep := locdep - 1 ;
	        IF (locdep >= twoto14) OR (locdep < -twoto14) THEN
		BEGIN
		  IF inxreg = ra THEN
		    IF locdep > 0 THEN
		      genstand (nreg, locdep, iada, tdl)
		    ELSE
		      genstand (nreg, -locdep, isba, tdl)
		  ELSE IF inxreg = rq THEN
		      IF locdep > 0 THEN
		        genstand (nreg, locdep, iadq, tdl)
		      ELSE
		        genstand (nreg, -locdep, isbq, tdl)
		    ELSE IF inxreg IN [x0..x7] THEN
		        genstand (nreg, locdep, xinst [adlx, inxreg], tdu)
		      ELSE
		        BEGIN
			inxreg := x6 ;
			genstand (nreg, locdep, ieax6, tn)
		        END ;
		  locdep := 0 ;
		END ;
	        genstand (lbase, locdep, prinst [epp, prtoload],
		modif [inxreg]) ;
	        IF inxmem # 0 THEN
		BEGIN
		  genstand (pr6, inxmem DIV bytesinword, ilxl7, tn) ;
		  genstand (prtoload, 0, iawd, tx7) ;
		END ;
	        IF dplmt MOD bytesinword # 0 THEN (* ALWAYS >= 0 *)
		BEGIN
		  genstand (nreg, dplmt MOD bytesinword, ieax7, tn) ;
		  genstand (prtoload, 0, ia9bd, tx7) ;
		END ;
	      END ;
	    freebloc (inxbloc) ; inxreg := nxreg ;
	  END (* KIND=VARBL *) ELSE
	  IF kind = chain THEN
	    BEGIN
	      IF wantedpr = nreg THEN
	        BEGIN
		getpr ;
		prtoload := currentpr ;
	        END ELSE
	        prtoload := wantedpr ;
                                                  (* ALFACTP  POINTS A BOX *)
                                                  (* (KONST,ALFACONST) *)
	      enterundlab (alfactp@.unddeb) ;
	      genstand (nreg, 0, prinst [epp, prtoload], tic) ;
	    END (* CHAIN *) ELSE
	    error (405) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN LOADADR @@@ WITH LOCALES PRTOLOAD,LBASE', regname [prtoload],
	    regname [lbase]) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* LOADADR *) ;


$OPTIONS page $

(* ************************************ TRANSFER ****************************** *)

    PROCEDURE transfer (VAR fattr : attr ; inwhat : destination) ;

(* C   INWHAT . <== INACC,INQ,INPSR     FATTR BECOMES  LVAL
   . ==> OUT                 GATTR(LVAL) ==> FATTR
   . INPR     FATTR BECOMES POINTEE
   SUMMARY:  LOADS A REGISTER  WITH AN EXPRESSION DESCRIBED BY FATTR
   OR STORE  ACC INTO THE   VARBL DESCRIBED BY FATTR.
   C *)
(* E  ERRORS DETECTED
   400  LDREGBLOC = NIL
   401  LCOND SAVED
   416  LVAL SAVED
   418  INCORRECT ORIGIN
   420  FATTR.KIND # VARBL (OUT)
   421  GATTR.KIND # LVAL (OUT)
   E *)
      VAR
        lretpt : lcstpt ;
        llretpt : llcstpt ;
        target : register ;
        loadinst, lshift, rshift, rlogshift, storinst, llshift : istand ;
        lbase : preg ;
        ldisp, rightcount, leftcount, longitem, longset, longmove : integer ;
        ltag : tag ;
        tomove, callcalc
$OPTIONS compile = security $, noterr
$OPTIONS compile = true $
        : boolean ;
        lmove : ieism ;


(* ************************************ GENLOCSKIP < TRANSFER ***************** *)

      PROCEDURE genlocskip (fjump : istand) ;

(* C GENERATES A BOOLEAN USING THE SETTING OF INDICATORS
   GENERATES     FJUMP  TO  E1
   LOAD  FALSE
   SKIP  INCOND TO E2
   E1 LOAD  TRUE
   E2
   C *)
        VAR
	locskip1, locskip2 : integer ;
        BEGIN                                     (* GENLOCSKIP *)
	locskip1 := indfich ; genstand (nreg, 0, fjump, tic) ;
	genstand (nreg, ord (false), loadinst, tdl) ;
	locskip2 := indfich ; genstand (nreg, 0, itra, tic) ;
	inser (cb, locskip1) ; genstand (nreg, ord (true), loadinst, tdl) ;
	inser (cb, locskip2) ;
        END (* GENLOCSKIP *) ;

      BEGIN                                       (* TRANSFER *)
$OPTIONS compile = security $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT TRANSFER @@@ WITH INWHAT', ord (inwhat)) ; nextline ;
	  IF stattrace >= medium THEN
	    BEGIN
	      write (mpcogout, '* FATTR INPUT OF TRANSFER IS:') ; nextline ;
	      printattr (fattr) ;
	    END ;
	END ;
$OPTIONS compile = true $
        WITH fattr DO
	IF typtr # NIL THEN
	  IF typtr@.form # power THEN
	    BEGIN                               (* NOT A SET *)
	      IF inwhat # out THEN
	        BEGIN

(* LOAD SEQUENCE OF ITEM DESCRIBED BY FATTR *)
(* FIRST  FIND  THE TARGET REGISTER  AND  SUITABLE LOAD INSTR. *)
(*  INACC ==> REAQ   DFLD  FOR   REAL
   RAQ    LDAQ  FOR   POINTER
   RA     LDA   OR  LLS
   INQ   ==> RQ     LDQ    FOR   SMALL ITEMS   OR SHIFT FROM RA
   INAQ  ==> RAQ    LDAQ *)
$OPTIONS compile = security $
		noterr := true ;
$OPTIONS compile = true $
		CASE inwhat OF
		  inaq :
		    BEGIN
$OPTIONS compile = security $
		      noterr := ((kind = varbl) AND (typtr@.size = bytesindword) AND
		        (typtr # realptr)) OR
		        ((kind = lval) AND (ldreg = raq)) ;
$OPTIONS compile = true $
		      loadinst := ildaq ; target := raq ;
		    END (* INAQ *) ;
		  inq :
		    BEGIN
$OPTIONS compile = security $
		      noterr := ((kind = varbl) AND (typtr@.size <= bytesinword)) OR
		        ((kind = lval) AND (ldreg IN [ra, rq])) OR
		        ((kind = sval) AND (typtr@.size <= bytesinword)) OR
		        (kind = lcond) ;
$OPTIONS compile = true $
		      loadinst := ildq ; target := rq ;
		    END (* INQ *) ;
		  inacc :
		    BEGIN
		      IF typtr = realptr THEN
		        BEGIN loadinst := idfld ; target := reaq ;
		        END (* REAL *) ELSE
		        IF (typtr@.size = bytesindword) OR (typtr@.form = pointer) THEN
			BEGIN
			  target := raq ;   (* ENDING TARGET   *)
                                                  (* ALSO FOR PACKED POINTER *)
			  IF pckd AND (kind = varbl) AND (typtr@.form = pointer) THEN
			    loadinst := ilprp3 ELSE loadinst := ildaq ;
			END (* BYTESINDWORD *) ELSE
			BEGIN
			  target := ra ; loadinst := ilda ;
			END ;
		    END (* INACC *) ;
		  inpr :
$OPTIONS compile = security $
		    noterr := kind = varbl ;
$OPTIONS compile = true $
		END (* CASE INWHAT *) ;
$OPTIONS compile = security $
		IF NOT noterr THEN error (418) ELSE
$OPTIONS compile = true $
		  CASE kind OF
		    varbl :
		      BEGIN
		        callcalc := true ;
		        IF pckd THEN
			IF typtr@.form # pointer THEN
			  BEGIN
			    callcalc := false ;
			    loadadr (fattr, nreg) ;
			    vlev := level ;
			    itsdplmt := 0 ;
			    access := pointee ;
			    basereg := currentpr ;
			    basebloc := currentbloc ;
			    dplmt := 0 ;
			    inxreg := nxreg ;
			    inxbloc := NIL ;
			    inxmem := 0 ;
			    inxmemrw := true ;
			    pckd := true ;
			  END ;
		        IF callcalc THEN
			calcvarient (fattr, lbase, ldisp, ltag) ;
		        IF inwhat = inpr THEN
			BEGIN
			  getpr ;           (* ==> CURRENTPR,CURRENTBLOC, REGCHARGE *)
			  IF pckd THEN      (* PACKED POINTER ON ONE WORD *)
			    BEGIN
			      usednameaddr := nameaddr ;
			      genstand (lbase, ldisp, prinst [lprp, currentpr], ltag) END ELSE
			    BEGIN           (* NOT PACKED *)
			      IF ltag <= tx7 (* NO INDIRECT MODIFIER *) THEN
			        BEGIN
				usednameaddr := nameaddr ;
				genstand (lbase, ldisp, prinst [epp, currentpr],
				  newtagstar [ltag])
                                                  (*   TAG  BECOMES   TAG* *) END ELSE
			        BEGIN       (* ALREADY INDIRECT *)
				genstand (lbase, ldisp, prinst [epp, currentpr], ltag) ;
				usednameaddr := nameaddr ;
				genstand (currentpr, 0, prinst [epp, currentpr], tny) ;
			        END ;
			    END (* NOT PACKED *) ;
                                                  (* FATTR  BECOMES  POINTEE *)
			  vlev := level ; itsdplmt := 0 ;
			  access := pointee ; basereg := currentpr ;
			  basebloc := currentbloc ; dplmt := 0 ;
			  inxreg := nxreg ; inxbloc := NIL ; inxmem := 0 ;
			  inxmemrw := true ; pckd := false ;
			END (* INPR *) ELSE
			BEGIN
			  IF callcalc THEN
			    BEGIN
			      sauvereg (target, true) ; (* CURRENTBLOC ,REGCHARGE OK *)
			      usednameaddr := nameaddr ;
			      genstand (lbase, ldisp, loadinst, ltag) ;
			    END ;
                                                  (* FOR PACKED ITEMS *)
                                                  (* THE WHOLE WORD IS LOADED *)
			  IF pckd THEN
			    IF typtr@.form = pointer THEN
			      BEGIN
			        genstand (pr6, evareaw, ispri3, tn) ;
			        genstand (pr6, evareaw, ildaq, tn) ;
			      END ELSE
			      BEGIN         (* PCKD NOT POINTER *)
			        IF callcalc THEN
				BEGIN
				  rightcount
				  := bitsinword - bitsinbyte * packedsize (typtr) ;
				  leftcount := (dplmt MOD bytesinword) * bitsinbyte ;
				  IF target = ra THEN
				    BEGIN
				      lshift := ials ; rshift := iars ; rlogshift := iarl ;
				    END (* RA *) ELSE
				    BEGIN (* RQ *)
				      lshift := iqls ; rshift := iqrs ; rlogshift := iqrl ;
				    END (* RQ *) ;
				  IF leftcount # 0 THEN
				    genstand (nreg, leftcount, lshift, tn) ;
				  IF typtr@.form = numeric THEN
				    genstand (nreg, rightcount, rshift, tn) ELSE
				    genstand (nreg, rightcount, rlogshift, tn) ;
				END ELSE
				BEGIN
                                                  (* BASEREG POINTS ITEM *)
				  longitem := packedsize (typtr) ;
				  IF typtr@.form = scalar THEN
				    BEGIN
				      lmove := imrl ; longmove := bytesinword ;
				      rightcount := 0 ;
				    END ELSE
				    BEGIN
				      lmove := imlr ; longmove := longitem ;
				      rightcount := (bytesinword - longitem) * bitsinbyte ;
				    END ;
				  mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
				  geneism (lmove, 0 (* FILL 0 *), p0t0r0) ;
				  usednameaddr := nameaddr ;
				  gendesca (basereg, 0, 0, l9, longitem, tn) ;
				  gendesca (pr6, evareaw, 0, l9, longmove, tn) ;
				  IF basebloc # NIL THEN freebloc (basebloc) ;
				  sauvereg (target, true) ;
				  genstand (pr6, evareaw, loadinst, tn) ;
				  IF rightcount # 0 THEN
				    BEGIN
				      IF target = rq THEN rshift := iqrs ELSE
				        rshift := iars ;
				      genstand (nreg, rightcount, rshift, tn) ;
				    END ;
				END ;
			      END (* PCKD NOT POINTER *) ;
                                                  (* CHANGE NOW FATTR *)
			  kind := lval ; ldreg := target ; ldregbloc := currentbloc ;
			END (* NOT INPR *) ;
		      END (* VARBL *) ;
		    sval :
		      BEGIN
		        sauvereg (target, true) ;
		        calcvarient (fattr, lbase, ldisp, ltag) ;
		        genstand (lbase, ldisp, loadinst, ltag) ;
                                                  (* CHANGE FATTR *)
		        kind := lval ; ldreg := target ; ldregbloc := currentbloc ;
		      END (* SVAL *) ;
		    lval : BEGIN
$OPTIONS compile = security $
		        IF ldregbloc@.saveplace # 0 THEN error (416) ;
$OPTIONS compile = true $
                                                  (* NOOPERATION   EXCEPT *)
                                                  (* EXCHANGE BETWEEN RA<==> RQ *)
		        IF (inwhat = inacc) AND (ldreg = rq) THEN
			llshift := ills ELSE
			IF (inwhat = inq) AND (ldreg = ra) THEN
			  llshift := ilrl ELSE
			  llshift := inop ;
		        IF llshift # inop THEN
			BEGIN
			  sauvereg (target, true) ;
			  genstand (nreg, bitsinword, llshift, tn) ;
			  freebloc (ldregbloc) ;
			  ldreg := target ; ldregbloc := currentbloc ;
			END ;
		      END (* LVAL *) ;
		    lcond :
		      BEGIN
		        IF accbloc = NIL THEN
			sauvereg (target, true) ELSE
			IF target # accbloc@.sregister THEN
			  BEGIN
			    freebloc (accbloc) ; sauvereg (target, true) ;
			  END ELSE
$OPTIONS compile = security $
			  IF accbloc@.saveplace # 0 THEN error (401) ELSE
$OPTIONS compile = true $
			    BEGIN
			      freebloc (accbloc) ; newbloc (target) ;
			    END ;
		        CASE transf OF
			1 :                 (* BOOLEAN IS  IN A0 *)
			  IF target = ra THEN
			    genstand (nreg, bitsinword - 1, iarl, tn) ELSE
			    genstand (nreg, bitsindword - 1, ilrl, tn) ;
			2 :                 (* ZERO ON  <==> TRUE *)
			  genlocskip (itze) ;
			3 :                 (* BOOLEAN  IS IN A *)
			  IF target = rq THEN
			    genstand (nreg, bitsinword, ilrl, tn) ;
			4 :                 (* SVAL TRUE *)
			  genstand (nreg, ord (true), loadinst, tdl) ;
			5 :                 (* SVAL FALSE *)
			  genstand (nreg, ord (false), loadinst, tdl) ;
			6 :                 (* ZERO OFF   TRUE *)
			  genlocskip (itnz) ;
			7 :                 (* NEGATIVE ON TRUE *)
			  genlocskip (itmi) ;
			8 :                 (* NEGATIVE OR ZERO ON  TRUE *)
			  genlocskip (itmoz) ;
			9 :                 (* NEGATIVE OFF TRUE *)
			  genlocskip (itpl) ;
			10 :                (* ZERO OFF AND NEGATIVE OFF  TRUE *)
			  genlocskip (itpnz) ;
			11 :                (* CARRY  OFF   TRUE *)
			  genlocskip (itnc) ;
			12 :                (* CARRY  ON   TRUE *)
			  genlocskip (itrc) ;
			13 :                (* REVERSE BOOLEAN IN A *)
			  BEGIN
			    genstand (nreg, 1, iera, tdl) ;
			    IF target = rq THEN
			      genstand (nreg, bitsinword, ilrl, tn) ;
			  END ;
			14 :                (* BOOLEAN IS IN Q *)
			  IF target = ra THEN
			    genstand (nreg, bitsinword, ills, tn) ;
			15 :                (* REVERSE BOOLEAN IS IN Q *)
			  BEGIN
			    genstand (nreg, 1, ierq, tdl) ;
			    IF target = ra THEN
			      genstand (nreg, bitsinword, ills, tn) ;
			  END ;
		        END (* CASE TRANSF *) ;
                                                  (* NOW CHANGES FATTR *)
		        kind := lval ; ldreg := target ; ldregbloc := currentbloc ;
		      END (* LCOND *) ;
		  END (* CASE KIND *) ;
	        END (* INWHAT # OUT *) ELSE
	        BEGIN                           (* TRANSFER OUT *)
$OPTIONS compile = security $
		IF kind # varbl THEN error (420) ;
		IF gattr.kind # lval THEN error (421) ELSE
$OPTIONS compile = true $
		  regenere (gattr.ldregbloc) ;
		CASE gattr.ldreg OF
		  reaq : storinst := idfst ;
		  raq : storinst := istaq ;
		  ra : storinst := ista ;
		  rq : storinst := istq ;
		END (* CASE  GATTR.LDREG *) ;
		IF (NOT pckd) OR (typtr@.form = pointer) (* ONE ORTWOWORDS *) THEN
		  calcvarient (fattr, lbase, ldisp, ltag) ELSE
		  BEGIN
		    loadadr (fattr, nreg) ;
		    lbase := currentpr ; ldisp := 0 ; ltag := tn ;
		    freebloc (currentbloc) ;
		  END ;
		IF NOT pckd THEN
		  BEGIN
		    usednameaddr := nameaddr ;
		    genstand (lbase, ldisp, storinst, ltag) END ELSE
		  IF typtr@.form = pointer THEN
		    BEGIN
		      genstand (pr6, evareaw, istaq, tn) ;
		      genstand (pr6, evareaw, iepp3, tny) ;
		      usednameaddr := nameaddr ;
		      genstand (lbase, ldisp, isprp3, ltag) ;
		    END (* PCKD POINTER *) ELSE
		    BEGIN
                                                  (* MOVE INSTR *)
		      longitem := packedsize (typtr) ;
		      genstand (pr6, evareaw, storinst, tn) ;
		      mfari1 := a1r0i0 ; mfari2 := a1r0i0 ; (* ONLY POINTER REG *)
		      geneism (imlr, 0, p0t0r0) ;
		      gendesca (pr6, evareaw, bytesinword - longitem, l9, longitem, tn) ;
		      usednameaddr := nameaddr ;
		      gendesca (lbase, 0, 0, l9, longitem, tn) ;
		    END (* PCKD NOT POINTER *) ;
	        END (* TRANSFER OUT *) ;
	    END (* TRANSFER IN/OUT NOT FOR SET *) ELSE
	    BEGIN                               (*  SET *)
	      IF inwhat # out THEN
	        BEGIN

(* INWHAT=INACC    LENGTH <=BYTESINDWORD   ==> IN AQ   EXCEPT LVAL
   >                ==> IN PSR
   FATTR BECOMES LVAL
   *)
		longset := typtr@.size * bitsinbyte ;
		IF kind = varbl THEN
		  BEGIN
		    IF longset > bitsindword THEN inwhat := inpsr ;
		  END ELSE
		  IF kind = sval THEN
		    BEGIN
		      IF inwhat <> inaq (* Force *) THEN
		        IF (longv > bytesindword) OR
			(longset > bitsindword) THEN inwhat := inpsr ;
		    END ;
		IF inwhat IN [inacc, inaq] THEN
		  BEGIN
		    IF kind = varbl THEN
		      BEGIN
		        IF longset <= bitsinhword THEN
			BEGIN
                                                  (* MOVE SEQ *)
			  loadadr (fattr, pr3) ;
			  mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
			  geneism (icsl, 3 (* 0011=MOVE *), p0t0r0) ; (* FILL BIT=0 *)
			  usednameaddr := nameaddr ;
			  gendescb (pr3, 0, 0, 0, longset, tn) ;
			  gendescb (pr6, evareaw, 0, 0, bitsindword, tn) ;
                                                  (* LOAD SEQUENCE *)
			  sauvereg (raq, true) ;
			  genstand (pr6, evareaw, ildaq, tn) ;
			END (* <= BITSINHWORD *) ELSE
			BEGIN
			  calcvarient (fattr, lbase, ldisp, ltag) ;
			  sauvereg (raq, true) ;
			  IF longset = bitsinword THEN
			    BEGIN           (* LOAD A   CLEAR Q *)
			      usednameaddr := nameaddr ;
			      genstand (lbase, ldisp, ilda, ltag) ;
			      genstand (nreg, 0, ildq, tdl) ;
			    END ELSE
			    BEGIN
			      usednameaddr := nameaddr ;
			      genstand (lbase, ldisp, ildaq, ltag) ;
			    END ;
			END (* >BITSINHWORD *) ;
		      END (* VARBL *) ELSE
		      IF kind = sval THEN
		        BEGIN
			sauvereg (raq, true) ;
			enterlcst (valpw, lretpt) ;
			enterundlab (lretpt@.lplace) ;
			genstand (nreg, 0, ildaq, tic) ;
		        END
$OPTIONS compile = security $
		      ELSE
		        IF ldregbloc@.saveplace # 0 THEN error (416)
$OPTIONS compile = true $	;
		    IF kind # lval THEN
		      BEGIN
		        kind := lval ; ldreg := raq ; ldregbloc := currentbloc ;
		      END ;
		  END (* INWHAT=INACC *) ELSE
		  BEGIN                       (* INWHAT=INPSR *)
                                                  (* INCLUDE LONG VARBL, SVAL FOR INACC *)
		    IF kind = lval THEN
		      BEGIN
                                                  (*  AQ ==> PSR     PSR NOOP *)
		        IF ldreg = raq THEN
			BEGIN
			  sauvereg (psr, true) ;
			  regenere (ldregbloc) ; clearpsr ;
			  genstand (pr6, psrdepw, istaq, tn) ;
			  freebloc (ldregbloc) ;
			  ldreg := psr ; psrsize := bytesindword ; ldregbloc := currentbloc ;
			END                 (* RAQ *)
$OPTIONS compile = security $
		        ELSE
			IF ldregbloc@.saveplace # 0 THEN error (416)
$OPTIONS compile = true $	  ;
		      END (* LVAL *) ELSE
		      BEGIN                   (* ALWAYS A MOVE *)
		        sauvereg (psr, true) ;
                                                  (* BUILD ORIGIN *)
		        IF kind = varbl THEN
			loadadr (fattr, pr3) ELSE
			BEGIN               (* SVAL *)
			  IF longv = bytesindword THEN
			    BEGIN
			      enterlcst (valpw, lretpt) ;
			      enterundlab (lretpt@.lplace) ;
			    END ELSE
			    BEGIN           (* LONG SET *)
			      enterllcst (valpw, llretpt) ;
			      enterundlab (llretpt@.llplace) ;
			    END ;
			  genstand (nreg, 0, iepp3, tic) ;
			  longset := longv * bitsinbyte ;
			END (* SVAL *) ;
		        mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
		        geneism (icsl, 3 (* 0011=MOVE *), p0t0r0) ; (* FILL BIT=0 *)
		        IF kind = varbl THEN usednameaddr := nameaddr ;
		        gendescb (pr3, 0, 0, 0, longset, tn) ;
		        gendescb (pr6, psrdepw, 0, 0, bitsforset, tn) ;
		        kind := lval ; ldreg := psr ; ldregbloc := currentbloc ;
		        psrsize := sup (typtr@.size, bytesindword) ;
		      END (* ALWAYS A MOVE SVAL//VARBL *) ;
		  END (* INPSR *) ;
	        END (* INWHAT # OUT *) ELSE
	        BEGIN                           (* OUT *)
		tomove := true ; ldisp := 0 ;
		longset := typtr^.size * bitsinbyte ;
		longmove := longset ;
$OPTIONS compile = security $
		IF gattr.kind # lval THEN error (421) ELSE
		  IF gattr.ldregbloc = NIL THEN error (400) ELSE
		    IF gattr.ldregbloc@.saveplace # 0 THEN error (416) ELSE
$OPTIONS compile = true $
		      IF gattr.ldreg = psr THEN
		        ldisp := psrdepw ELSE
		        IF typtr@.size = bytesindword THEN
			tomove := false ELSE
			BEGIN
			  ldisp := evareaw ; genstand (pr6, evareaw, istaq, tn) ;
			  IF longmove > bitsindword THEN
			    longmove := bitsindword ;
			END ;
		IF tomove THEN
		  BEGIN
		    loadadr (fattr, pr3) ;
		    mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
		    geneism (icsl, 3 (* 0011=MOVE *), p0t0r0) ;
		    gendescb (pr6, ldisp, 0, 0, longmove, tn) ;
		    IF kind = varbl THEN usednameaddr := nameaddr ;
		    gendescb (pr3, 0, 0, 0, longset, tn) ;
		  END (* TOMOVE *) ELSE
		  BEGIN                       (* STORE AQ *)
		    calcvarient (fattr, lbase, ldisp, ltag) ;
		    IF kind = varbl THEN usednameaddr := nameaddr ;
		    genstand (lbase, ldisp, istaq, ltag) ;
		  END (* NOT TO MOVE *) ;
	        END (* TRANSFER OUT *) ;
	    END (* SET *) ;
        IF inwhat = out THEN freeattr (gattr) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN TRANSFER @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* TRANSFER *) ;



$OPTIONS page $

(* ***************************************  CHOICERARQ *********************** *)

    PROCEDURE choicerarq ;

(* C   FOR GATTR LCOND,VARBL,SVAL CHOOSES THE SUITABLE TARGET (RA,RQ)
   THEN CALL TRANSFER (INACC OR INQ )
   C *)
(* E    ERRORS DETECTED
   422 : GATTR.KIND = CHAIN (CHOICERARQ)
   E *)
      BEGIN                                       (* CHOICERARQ *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT CHOICERARQ @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        IF gattr.typtr # NIL THEN
	WITH gattr DO
	  IF typtr@.form IN [reel, pointer] THEN transfer (gattr, inacc) ELSE
	    CASE kind OF
	      varbl, sval : IF NOT rqisused THEN
		transfer (gattr, inq) ELSE
		transfer (gattr, inacc) ;
	      lval : ;
	      lcond : IF accbloc # NIL THEN
		BEGIN
		  IF accbloc@.sregister = ra THEN
		    transfer (gattr, inacc) ELSE
		    transfer (gattr, inq) ;
		END (* #NIL *) ELSE
		transfer (gattr, inacc) ;
	      chain :
$OPTIONS compile = security $
	        error (422)
$OPTIONS compile = true $
	        ;
	    END (* CASE KIND, WITH GATTR *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN CHOICERARQ @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* CHOICERARQ *) ;



$OPTIONS page $

(* ************************************ VARIAB ******************************** *)

    PROCEDURE variab (fvarset : boolean) ;

(* C  PRECALL SEQUENCE FOR "VARIABLE"
   AN  IDENTIFIER  (NO=1)  IS EXPECTED
   MUST BE VARS OR FIELD
   FVARSET IS TRUE  IF  VARIABLE IS TO BE ALTERED
   C *)
(* E ERRORS DETECTED
   2:  IDENTIFIER EXPECTED
   103:  IDENTIFIER FOUND IS NOT OF APPROPRIATE KLASS.
   104:  IDENTIFIER NOT DECLARED
   196:  VARIABLE IS READONLY
   E *)
      BEGIN                                       (* VARIAB *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT VARIAB @@@ WITH NO,FVARSET', no : 4, fvarset : 6) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        variabctptr := NIL ;
        IF no # 1 (* ID *) THEN
	BEGIN
	  error (2) ; gattr.typtr := NIL ;
	END ELSE
	BEGIN                                   (* ID *)
	  search ;
	  IF ctptr = NIL THEN
	    BEGIN                               (* ID NOT FOUND *)
	      error (104) ;
	      ctptr := undecptr ;               (* UNDECLARED VARIABLE *)
	    END ;
	  IF ctptr@.klass <= proc THEN          (* NOT VARS-FIELD *)
	    BEGIN
	      IF symbolmap THEN
	        IF fvarset THEN nameisref (ctptr, symbolfile, -symbolline)
	        ELSE nameisref (ctptr, symbolfile, symbolline) ;
	      error (103) ; insymbol ; gattr.typtr := NIL ; (* ERROR INDICATOR *)
	    END ELSE
	    BEGIN                               (* VARS-FIELD *)
	      IF ctptr@.klass = vars THEN
	        BEGIN
		IF fvarset THEN
		  BEGIN
		    IF ctptr@.visreadonly THEN error (196) ;
		    ctptr@.visset := true ;
		    variabctptr := ctptr ;
		  END ;
	        END ;
	      variable (fvarset) ;
	    END (* VARS- FIELD *) ;
	END (* ID *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN VARIAB @@@ WITH NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* VARIAB *) ;


$OPTIONS page $

    BEGIN
    END.
