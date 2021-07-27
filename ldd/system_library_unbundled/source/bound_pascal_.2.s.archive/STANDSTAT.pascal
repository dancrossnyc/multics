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


$OPTIONS switch trace := true $
  PROGRAM standstat ;
    $IMPORT
                                                  (* IMPORTED PROCEDURES  *)
      'RACINE (pascal)' :
        error,
        inserundlab,
        insymbol,
        nameisref,
        nextline,
        recadre,
        search_in_condition_attributes,
        skip,
        statement_begins,
        statement_ends,
        sup ;
      'MODATTR (pascal) ' :
        convreal,
        freeattr,
        initattrvarbl,
        isstring,
        lvalvarbl,
        varissimple ;
      'CONTEXTTABLE (pascal) ' :
        checkminmax,
        compatbin,
        conformantdim,
        packedsize ;
      'MODVARIABLE (pascal) ' :
        init_desc_address,
        variable,
        passparams ;
      'GENERE (pascal)' :
        gendesca,
        gendescb,
        geneism,
        genstand,
        inser ;
      'EXPR (pascal)' :
        expression ;
      'UNIQUE (pascal)' :
        heaperror ;
      'GENOPER (pascal)' :
        genjump ;
      'PROCSTAT (pascal)' :
        argvstat,
        dateandtime,
        delete_string,
        getput,
        insapp,
        insert_string,
        mvcir,
        newir,
        pckunpck,
        readir,
        stopstat,
        writeir ;
      'STATE (pascal)' :
        addressvar,
        calcvarient,
        checkbnds,
        choicerarq,
        currwithlist,
        enterlcst,
        enterundlab,
        freeallregisters,
        freebloc,
        gencstecode,
        genexceptcode,
        loadadr,
        loadbase,
        newbloc,
        regenere,
        sauvereg,
        transfer,
        variab,
        withvariable ;
      'optimized_procedures (alm)' :
        search ;
                                                  (* IMPORTED VARIABLES *)
      'DECLARE (pascal)' :
        clabix,
        labtab,
        lab_pdl_top,
        lkc,
        pop_lab_pdl,
        push_lab_pdl ;
      'RACINE (pascal)' :
        aval,
        boolptr,
        charptr,
        cl,
        ctptr,
        currentnode,
        display,
        envstandard,
        errcl,
        exportablecode,
        intptr,
        ival,
        lamptr,
        level,
        mapswitch,
        mpcogout,
        no,
        realptr,
        statnbr,
        string_ptr,
        sttindex,
        symbolfile,
        symbolindex,
        symbolline,
        symbolmap,
        top,
        undecptr,
        usednames ;
      'GENERE (pascal)' :
        cb,
        ic,
        indfich,
        mfari1,
        mfari2,
        mfreg2,
        usednameaddr ;
      'STATE (pascal)' :
        asscheck,
        currentbloc,
        currentpr,
        gattr,
        inxcheck,
        lcsave,
        linktomain,
        linktomainplace,
        maxprused,
        maxinxused,
        modif,
        opaq,
        prinst,
        stattrace,
        tmax$

    $EXPORT
      compstat $



$OPTIONS page $


$INCLUDE 'CONSTTYPE' $

$OPTIONS page $

    VAR

(* REDEFINE IMPORTED VARIABLES     *)

(* FROM DECLARE *)

      clabix : integer ;
      labtab : ARRAY [1..maxlabs] OF labdescr ;
      lab_pdl_top : lab_pdl_ptr ;
      lkc : integer ;

(* FROM RACINE  *)
      mpcogout : text ;
      envstandard : stdkind ;
      display : ARRAY [0..displimit] OF recidscope ;
      mapswitch : boolean ;
      top : integer ;
      sttindex : integer ;
      symbolfile : integer ;
      symbolindex : integer ;
      symbolline : integer ;
      symbolmap : boolean ;
      usednames : typusednames ;
      aval : alfaid ;
      level : levrange ;
      cl : integer ;
      no : integer ;
      intptr : ctp ;
      lamptr : ctp ;
      undecptr : ctp ;
      realptr : ctp ;
      ctptr : ctp ;
      errcl : ARRAY [norange] OF typofsymb ;
      ival : integer ;
      boolptr : ctp ;
      charptr : ctp ;
      exportablecode : boolean ;
      currentnode : blocknodeptr ;
      statnbr : integer ;
      string_ptr : ctp ;


(* FROM GENERE  *)
      ic : integer ;
      cb : integer ;
      indfich : integer ;
      mfari1 : zari ;
      mfari2 : zari ;
      mfreg2 : mreg ;
      usednameaddr : ctp ;


(* FROM STATE   *)
      inxcheck : boolean ;
      asscheck : boolean ;
      gattr : attr ;
      currentbloc : regpt ;
      prinst : ARRAY [typepr, pr1..pr6] OF istand ; (* GIVES A PR INSTRUCTION *)
      tmax : integer ;
      linktomain : boolean ;
      linktomainplace : integer ;
      lcsave : integer ;
      stattrace : levtrace ;
      maxinxused : register ;
      maxprused : preg ;
      modif : ARRAY [nxreg..rq] OF tag ;
      opaq : ARRAY [typeofop, ra..reaq] OF istand ; (* GIVES INST. WITH A,Q,AQ,EAQ *)
      currentpr : preg ;
      withvariable : boolean ;
      currwithlist : withreflist ;



(* **************    VARIABLES LOCALES    ************************* *)

      splitstat : ARRAY [norange] OF integer ;    (* USED TO SELECT THE GOOD STAT. *)


    $VALUE

      splitstat = (
        1, 2, 19 * 1, 3, 1, 4, 1, 1, 5, 1, 6,
        1, 7, 1, 8, 1, 1, 9, 12 * 1, 10, 1, 1,
        1, 1, 1, 1, 1, 1, 1) $


$OPTIONS page $

(* REDEFINE IMPORTED PROCEDURES    *)
(* FROM GENERE  *)
    PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ; EXTERNAL ;
    PROCEDURE geneism (fcode : ieism ; ffield : integer ; fbits : zptr) ; EXTERNAL ;
    PROCEDURE gendesca (fareg : preg ; fadr, fcn : integer ; fta : lgcar ;
      fn : integer ; frlgth : mreg) ; EXTERNAL ;
    PROCEDURE gendescb (fareg : preg ; fadr, fc, fb, fn : integer ; frlgth : mreg) ; EXTERNAL ;
    PROCEDURE inser (fcb : integer ; fplace : integer) ; EXTERNAL ;


(* FROM RACINE  *)
    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE insymbol ; EXTERNAL ;
    PROCEDURE search_in_condition_attributes ; EXTERNAL ;
    PROCEDURE skip (nosym : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    PROCEDURE search ; EXTERNAL ;
    FUNCTION recadre (fnum, fmod : integer) : integer ; EXTERNAL ;
    PROCEDURE inserundlab (fcb, fdebchain : integer) ; EXTERNAL ;
    PROCEDURE nameisref (p : ctp ; f, l : integer) ; EXTERNAL ;
    PROCEDURE statement_begins (genp : boolean) ; EXTERNAL ;
    PROCEDURE statement_ends (sttlength : integer) ; EXTERNAL ;
    FUNCTION sup (fval1, fval2 : integer) : integer ; EXTERNAL ;

(* FROM EXPR    *)
    PROCEDURE expression ; EXTERNAL ;

(* FROM MODATTR   *)
    PROCEDURE freeattr (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE lvalvarbl (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE initattrvarbl (VAR fattr : attr) ; EXTERNAL ;
    FUNCTION isstring (VAR fattr : attr) : boolean ; EXTERNAL ;
    FUNCTION varissimple (VAR fattr : attr) : boolean ; EXTERNAL ;
    PROCEDURE convreal (VAR fattr : attr) ; EXTERNAL ;


(* FROM CONTEXTTABLE *)

    PROCEDURE checkminmax (fvalu : integer ; fctp : ctp ; ferrnum : integer) ; EXTERNAL ;
    PROCEDURE compatbin (typleft, typright : ctp ; VAR fgeneric : ctp) ; EXTERNAL ;
    FUNCTION conformantdim (ff : ctp) : boolean ; EXTERNAL ;
    FUNCTION packedsize (fctp : ctp) : integer ; EXTERNAL ;

(* FROM MODVARIABLE *)

    PROCEDURE init_desc_address (fctp : ctp ; VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE variable (fvarset : boolean) ; EXTERNAL ;
    PROCEDURE passparams (fctplace : integer) ; EXTERNAL ;



(* FROM UNIQUE *)
    PROCEDURE heaperror ; EXTERNAL ;

(* FROM GENOPER  *)
    PROCEDURE genjump (VAR inserplace : integer ; jumpdisp : integer) ; EXTERNAL ;
                                                  (* FROM PROCSTAT *)
    PROCEDURE argvstat ; EXTERNAL ;
    PROCEDURE getput (fcode : integer) ; EXTERNAL ;
    PROCEDURE readir (fcode : integer) ; EXTERNAL ;
    PROCEDURE pckunpck (fcode : integer) ; EXTERNAL ;
    PROCEDURE writeir (fcode : integer) ; EXTERNAL ;
    PROCEDURE newir (fcode : integer) ; EXTERNAL ;
    PROCEDURE stopstat ; EXTERNAL ;
    PROCEDURE dateandtime (fcode : integer) ; EXTERNAL ;
    PROCEDURE delete_string ; EXTERNAL ;
    PROCEDURE insapp (fcode : integer) ; EXTERNAL ;
    PROCEDURE insert_string ; EXTERNAL ;
    PROCEDURE mvcir (fcode : integer) ; EXTERNAL ;

(* FROM STATE   *)
    PROCEDURE addressvar (fctp : ctp ; VAR fattr : attr ; modif : boolean) ; EXTERNAL ;
    PROCEDURE choicerarq ; EXTERNAL ;
    PROCEDURE enterlcst (VAR fval : setarray ; VAR fboxpt : lcstpt) ; EXTERNAL ;
    PROCEDURE enterundlab (VAR fundinx : integer) ; EXTERNAL ;
    PROCEDURE transfer (VAR fattr : attr ; inwhat : destination) ; EXTERNAL ;
    PROCEDURE genexceptcode (ferrcode : integer ; freg : register) ; EXTERNAL ;
    PROCEDURE variab (fvarset : boolean) ; EXTERNAL ;
    PROCEDURE freebloc (VAR fbtofree : regpt) ; EXTERNAL ;
    PROCEDURE loadadr (VAR fattr : attr ; wantedpr : preg) ; EXTERNAL ;
    PROCEDURE calcvarient (VAR fattr : attr ; VAR fbase : preg ; VAR fdisp : integer ;
      VAR ftag : tag) ; EXTERNAL ;
    PROCEDURE gencstecode (farg : integer ; finst : istand) ; EXTERNAL ;
    PROCEDURE checkbnds (errcode : integer ; freg : register ; fctp : ctp) ; EXTERNAL ;
    PROCEDURE freeallregisters ; EXTERNAL ;
    PROCEDURE loadbase (flev : integer) ; EXTERNAL ;
    PROCEDURE sauvereg (freg : register ; fload : boolean) ; EXTERNAL ;
    PROCEDURE newbloc (freg : register) ; EXTERNAL ;
    PROCEDURE regenere (oldbloc : regpt) ; EXTERNAL ;

(* FROM DECLARE *)
    PROCEDURE push_lab_pdl ; EXTERNAL ;
    PROCEDURE pop_lab_pdl ; EXTERNAL ;


$OPTIONS page $

(* **************************************************************************** *)
(* *                                                                          * *)
(* *                                                                          * *)
(* *                                    STATEMENT GROUP                       * *)
(* *                                                                          * *)
(* *                                                                          * *)
(* **************************************************************************** *)

    PROCEDURE statement ; FORWARD ;



$OPTIONS page $

(* ********************************************** COMPSTAT ******************** *)

    PROCEDURE compstat ;

(* C.USED TO COMPILE    A  COMPOUND   STATEMENT.
   "BEGIN" HAS BEEN READ.
   .EXPECTS AN "END"
   C *)
(* ERRORS DETECTED
   13: "END" EXPECTED
   14: ";"   EXPECTED
   61: ILLEGAL  BEGINNING SYMBOL FOR A STATEMENT
   E *)
      LABEL 1 ;                                   (* SKIP HERE WHEN A STATEMENT *)
                                                  (* CAN BEGIN *)
      BEGIN
        freeallregisters ;
        REPEAT
	insymbol ;
1 :	statement ;
	freeallregisters ;
	IF errcl [no] = begsy THEN
	  BEGIN
	    error (14) ; GOTO 1 ;
	  END ;
	IF no = 25 (* ELSE *) THEN
	  BEGIN
	    error (61) ; insymbol ; GOTO 1 ;
	  END ;
        UNTIL no # 16 (* ; *) ;
        IF no = 22 (* "END" *) THEN
	BEGIN
	  insymbol
	END
        ELSE error (13) ;
      END (* COMPSTAT *) ;


$OPTIONS page $

(* ************************************ WITHSTAT ****************************** *)

    PROCEDURE withstat ;

(* C  COMPILE THE PASCAL STATEMENT
   WITH  <REC> [, REC]*   DO  <STATE>
   C *)
(* E ERRORS DETECTED
   54: "DO" EXPECTED
   140: TYPE OF VARIABLE MUST BE RECORD
   250: TOO MANY NESTED SCOPES OF IDENTIFIERS
   E *)
      LABEL
        10,                                       (*  EXIT PROCEDURE *)
        20 ;                                      (*  CALL STATEMENT *)
                                                  (* WITHOUT CALLING INSYMBOL *)
      VAR
        oldtop, oldlcsave : integer ;
        currentfather, withnode : blocknodeptr ;
        withfile, withindex : integer ;
      BEGIN                                       (* WITHSTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT WITHSTAT @@@ WITH TOP,LCSAVE  ', top, lcsave) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        withnode := NIL ;
        currentfather := currentnode ;
        oldtop := top ;                           (* ACTUAL LEVEL REACHED IN DISPLAY *)
        oldlcsave := lcsave ;
        lcsave := recadre (lcsave, bytesindword) ;
        IF lcsave > tmax THEN tmax := lcsave ;
        REPEAT                                    (* LOOP ON RECORD'S LIST *)
	insymbol ;
	withfile := symbolfile ;
	withindex := symbolindex ;
	freeallregisters ;
	withvariable := true ;
	currwithlist.nbr := 0 ;
	variab (false) ;                        (* NOT ALTERED HERE *)
	withvariable := false ;
	WITH gattr DO
	  IF typtr # NIL THEN                   (* NO ERROR *)
	    IF typtr@.form # records THEN
	      error (140) ELSE
	      IF top >= displimit THEN
	        error (250) ELSE
	        BEGIN
		top := top + 1 ;
		WITH display [top] DO
		  BEGIN
		    fname := typtr@.fstfld ;  (* FIRST FIELD NAME IN THE RECORD *)
		    new (withnode, withblock) ;
		    IF withnode = NIL THEN heaperror ; (* EXIT COMP *)
		    WITH withnode^ DO
		      BEGIN
		        blocktp := withblock ;
		        father := currentnode ;
		        recordptr := typtr ;
		        wstrfile := withfile ; wstrindex := withindex ;
		        IF symbolfile = wstrfile THEN
			wstrlength := symbolindex - withindex
		        ELSE wstrlength := 0 ;
		        brother := currentnode^.son ;
		        currentnode^.son := withnode ;
		        son := NIL ;
		        codebegin := statnbr * 2 ;
		        codeend := 0 ;
		        first := typtr^.fstfld ;
		        currentnode := withnode ;
		        IF varissimple (gattr) AND (NOT typtr@.pack) THEN
			BEGIN               (* EASY TO ADDRESS *)
			  occur := cwith ; clevel := vlev ; cdspl := dplmt ;
			  IF vlev = 0 THEN wbase := statics ELSE wbase := locals ;
			  wdispl := dplmt DIV bytesinword ; windirect := false ;
			  creflist := currwithlist ;
			END (* EASY *) ELSE
			BEGIN
			  occur := vwith ; vdspl := lcsave ;
			  lcsave := lcsave + bytesindword ;
			  IF lcsave > tmax THEN tmax := lcsave ; vpack := typtr@.pack ;
                                                  (* BUILDS ITS POINTING ON THE RECORD *)
			  loadadr (gattr, nreg) ; freebloc (currentbloc) ;
			  genstand (pr6, vdspl DIV bytesinword,
			    prinst [spri, currentpr], tn) ;
			  wbase := locals ; wdispl := vdspl DIV bytesinword ; windirect := true ;
			  vreflist := currwithlist ;
			END (* NOT EASY *) ;
		      END (* WITH WITHNODE^ *) ;
		  END (* WITH DISPLAY[TOP] *) ;
	        END (* ALL IS OK,WITH GATTR *) ;
        UNTIL no # 15 (* , *) ;
        IF no # 31 (* DO *) THEN
	BEGIN
	  IF gattr.typtr # NIL THEN error (54) ;
	  skip (31) ;
	  IF no # 31 THEN
	    BEGIN
	      IF gattr.typtr = NIL THEN error (54) ;
	      IF errcl [no] = begsy THEN
	        GOTO 20 (* STATEMENT *) ELSE
	        GOTO 10 (* END PROC *) ;
	    END ;
	END (* NO#31 *) ;
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
        insymbol ;
20 :
        freeallregisters ;
        push_lab_pdl ; statement ; pop_lab_pdl ;
10 :
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
$OPTIONS compile = trace $
        IF stattrace = high THEN
	BEGIN
	  write (mpcogout, '* RETURN IN WITHSTAT . TOP, LCSAVE  ARE NOW', top, lcsave) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        WHILE currentnode <> currentfather DO
	BEGIN
	  currentnode^.codeend := statnbr * 2 ;
	  currentnode := currentnode^.father ;
	END ;
        top := oldtop ;
        lcsave := oldlcsave ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN WITHSTAT @@@ WITH NO,TOP, LCSAVE ', no, top, lcsave) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* WITHSTAT *) ;


$OPTIONS page $

(* ************************************ ASSIGN ******************************** *)

    PROCEDURE assign ;

(* C   COMPILATION  OF
   <VARIABLE>  :=    <EXPRESSION>
   C *)
(* E ERRORS DETECTED
   29: Same length strings expected

   51: ":=" EXPECTED
   109: REAL TO INT NOT ALLOWED
   145: TYPE CONFLICT
   146: FILES NOT ALLOWED HERE
   197: TRUNCATION  NOT ALLOWED
   303: VALUE ASSIGNED OUT OF BOUNDS
   E *)
      VAR
        check_done, len_in_desc : boolean ;
        strlen : integer ;
        locarray : setarray ; lretpt : lcstpt ;
        lbloc, rbloc : regpt ;
        lattr, tattr : attr ;
        generic : ctp ;
        lbase, rbase, lpr, rpr : preg ;
        ldisp, lsize, rsize, rdisp, lmod, rmod, suplr : integer ;
        ltag, lftag, rgtag : tag ;
        asserr, ended : boolean ;
        ddisp, target_length, loc1 : integer ;
        temp : ctp ;
        rqbox : regpt ;
      BEGIN                                       (* ASSIGN *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT ASSIGN @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* LEFT PART *)
        check_done := false ;
        len_in_desc := false ;
        asserr := false ;
        variable (true) ;
        lattr := gattr ;
        IF no # 20 (* := *) THEN
	BEGIN
	  IF gattr.typtr # NIL THEN error (51) ;
	  skip (20) ;
	END ;
        IF no = 20 (* := *) THEN
	BEGIN
	  insymbol ;
	  expression ;                          (* RIGHT PART  OF ASSIGNMENT *)
	  WITH gattr DO
	    IF typtr = NIL THEN
	      BEGIN
	        skip (46) ; generic := NIL ;
	      END ELSE
	      BEGIN
	        compatbin (lattr.typtr, typtr, generic) ;
	        IF generic = NIL THEN
		asserr := true ;
	      END ;
	  IF asserr THEN                        (* TRY STRING ASSIGNMENT *)
	    IF lattr.typtr <> NIL
	    THEN IF lattr.typtr^.father_schema = string_ptr THEN (* TARGET IS A STRING *)
	        WITH gattr DO
		IF typtr = charptr (* RIGHT PART IS A CHARACTER *) THEN
		  BEGIN
		    IF varissimple (lattr) THEN
		      BEGIN
		        lbase := lattr.basereg ; ldisp := lattr.dplmt DIV bytesinword ;
		        currentbloc := NIL ;
		      END ELSE BEGIN
		        loadadr (lattr, nreg) ; lbase := currentpr ;
		        ldisp := 0
		      END ;
		    CASE kind OF
		      sval : BEGIN
			IF currentbloc <> NIL THEN freebloc (currentbloc) ;
			locarray [0] := 1 ; locarray [1] := val * twoto27 ;
			enterlcst (locarray, lretpt) ;
			enterundlab (lretpt^.lplace) ;
			genstand (nreg, 0, iepp3, tic) ;
			mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
			geneism (imlr, ord (' '), p0t0r0) ;
			gendesca (pr3, 0, 0, l9, 5, tn) ;
			WITH lattr DO
			  IF kind = varbl THEN usednameaddr := nameaddr ;
			gendesca (lbase, ldisp, 0, l9, 5, tn) ;
		        END ;
		      lval : BEGIN
			IF currentbloc <> NIL THEN freebloc (currentbloc) ;
			genstand (nreg, 27, opaq [shiftl, ldreg], tn) ;
			genstand (lbase, ldisp + 1, opaq [stor, ldreg], tn) ;
			genstand (nreg, 1, opaq [load, ldreg], tdl) ;
			genstand (lbase, ldisp, opaq [stor, ldreg], tn) ;
		        END ;
		      varbl : BEGIN
			lbloc := currentbloc ;
			IF varissimple (gattr) THEN
			  BEGIN
			    rbase := gattr.basereg ; rdisp := gattr.dplmt DIV bytesinword ;
			  END ELSE BEGIN
			    loadadr (gattr, nreg) ; rbase := currentpr ;
			    rdisp := 0 ; freebloc (currentbloc) ;
			  END ;
			IF lbloc <> NIL THEN BEGIN
			    regenere (lbloc) ; freebloc (lbloc) ;
			  END ELSE IF lattr.basebloc <> NIL THEN regenere (lattr.basebloc) ;
			mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
			geneism (imlr, ord (' '), p0t0r0) ;
			usednameaddr := gattr.nameaddr ;
			IF pckd THEN gendesca (rbase, rdisp, 0, l9, 1, tn)
			ELSE gendesca (rbase, rdisp, 3, l9, 1, tn) ;
			usednameaddr := lattr.nameaddr ;
			gendesca (lbase, ldisp + 1, 0, l9, 1, tn) ;
			genstand (nreg, 1, ilda, tdl) ;
			genstand (lbase, ldisp, ista, tn) ;
		        END
		    END ;
		    freeattr (gattr) ; freeattr (lattr) ;
		  END
		ELSE
		  IF isstring (gattr) THEN
		    IF lattr.typtr^.actual_parameter_list = NIL THEN (* nothing *)
		    ELSE
		      BEGIN
		        loadadr (lattr, nreg) ; (* ADDRESS OF TARGET *)
		        lpr := currentpr ; lbloc := currentbloc ;
		        IF conformantdim (gattr.typtr) THEN
			BEGIN
			  init_desc_address (gattr.nameaddr, gattr) ;
			  sauvereg (rq, false) ;
			  regenere (gattr.descbloc) ;
			  genstand (gattr.descreg, 1, isbq, tn) ;
			  genstand (gattr.descreg, 0, isbq, tn) ;
			  genstand (nreg, 1, iadq, tdl) ;
			  rpr := gattr.basereg ; rbloc := gattr.basebloc ;
			END
		        ELSE BEGIN            (* NOT CONFORMANT *)
			  loadadr (gattr, nreg) ;
			  rpr := currentpr ; rbloc := currentbloc ;
			  CASE kind OF
			    chain : strlen := alfactp^.alfalong ;
			    varbl : strlen := typtr^.hi - typtr^.lo + 1 ;
			  END ;
			  sauvereg (rq, false) ;
			  gencstecode (strlen, ildq) ;
			  IF asscheck THEN
			    IF lattr.typtr^.actual_parameter_list^.klass = konst THEN
			      IF strlen > lattr.typtr^.actual_parameter_list^.values THEN
			        error (273) ;
			  check_done := true ;
			END ;
		        WITH lattr.typtr^.actual_parameter_list^ DO
			IF klass = konst THEN
			  IF values < twoto12 THEN len_in_desc := true ELSE
			    gencstecode (values, ilda)
			ELSE BEGIN
			    addressvar (lattr.typtr^.actual_parameter_list, tattr, false) ;
			    transfer (tattr, inacc) ; freeattr (tattr) ;
			  END ;
		        regenere (rbloc) ; regenere (lbloc) ;
		        IF len_in_desc THEN mfari2 := a1r0i0 ELSE
			mfari2 := a1r1i0 ; mfari1 := a1r1i0 ;
		        geneism (imlr, ord (' '), p0t0r0) ;
		        gendesca (rpr, 0, 0, l9, 0, tql) ;
		        IF len_in_desc THEN
			gendesca (lpr, 1, 0, l9, lattr.typtr^.actual_parameter_list^.values, tn)
		        ELSE
			gendesca (lpr, 1, 0, l9, 0, tal) ;
		        genstand (lpr, 0, istq, tn) ;
		        freebloc (rbloc) ;
		        IF asscheck THEN
			IF NOT check_done THEN
			  BEGIN
			    genstand (lpr, 0, icmpa, tn) ;
			    loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
			    genexceptcode (stringlength_assignment_error, rq) ;
			    inser (cb, loc1) ;
			  END ;
		        freebloc (lbloc) ;
		        freeattr (gattr) ; freeattr (lattr) ;
		      END
		  ELSE IF gattr.typtr^.father_schema = string_ptr THEN
		      IF lattr.typtr^.actual_parameter_list = NIL THEN (* nothing *)
		      ELSE
		        BEGIN
			loadadr (lattr, nreg) ;
			lpr := currentpr ; lbloc := currentbloc ;
			loadadr (gattr, nreg) ;
			rpr := currentpr ; rbloc := currentbloc ;
			IF asscheck THEN
			  BEGIN
			    WITH lattr.typtr^.actual_parameter_list^ DO
			      IF klass = konst THEN
			        IF gattr.typtr^.actual_parameter_list^.klass <> konst THEN
				gencstecode (values, ilda)
			        ELSE BEGIN
				  IF values >= gattr.typtr^.actual_parameter_list^.values THEN check_done := true
				END
			      ELSE BEGIN
				addressvar (lattr.typtr^.actual_parameter_list, tattr, false) ;
				transfer (tattr, inacc) ; freeattr (tattr) ;
			        END ;
			    genstand (rpr, 0, icmpa, tn) ;
			    loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
			    genexceptcode (stringlength_assignment_error, ra) ;
			    inser (cb, loc1) ;
			  END ;
			regenere (rbloc) ; regenere (lbloc) ;
			mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
			genstand (rpr, 0, ildq, tn) ; genstand (nreg, 4, iadq, tdl) ;
			geneism (imlr, ord (' '), p0t0r0) ;
			gendesca (rpr, 0, 0, l9, 0, tql) ;
			gendesca (lpr, 0, 0, l9, 0, tql) ;
			freebloc (lbloc) ; freebloc (rbloc) ;
			freeattr (gattr) ; freeattr (lattr) ;
		        END
		    ELSE error (145)
	      ELSE error (145) ;
	  IF generic # NIL THEN
	    WITH gattr DO
	      CASE generic@.form OF
	        reel :
		BEGIN
		  IF gattr.typtr # realptr THEN
		    convreal (gattr) ELSE
		    IF lattr.typtr # realptr THEN
		      error (109) ;
		  transfer (gattr, inacc) ;
		  transfer (lattr, out) ;
		END (* REEL *) ;
	        numeric, scalar :
		BEGIN
		  IF kind = sval THEN
		    checkminmax (val, lattr.typtr, 303) ELSE
		    IF asscheck THEN
		      BEGIN
		        choicerarq ;
		        checkbnds (asserrcode, ldreg, lattr.typtr) ;
		      END (* asschecks *) ;
		  ended := false ;
		  IF kind = sval THEN
		    BEGIN
		      IF lattr.pckd THEN
		        BEGIN
			IF lattr.access # pointable THEN
			  IF lattr.inxbloc = NIL THEN
			    IF lattr.inxmem = 0 THEN
			      IF packedsize (lattr.typtr) = byteinbyte
                                                  (* SHORTER SAID 1 *)
			      THEN
			        BEGIN
				IF val < 0 THEN (* 2'S COMPLEMENT *)
				  val := val + twoto9 ;
				calcvarient (lattr, lbase, ldisp, ltag) ;
				mfari1 := a0r0i0 ; (* DUMMY *)
				mfari2 := a1r0i0 ;
				mfreg2 := ltag ;
				geneism (imlr, val, p0t0r0) ;
				gendesca (nreg, 0, 0, l9, 0, tn) ;
				WITH lattr DO
				  IF kind = varbl THEN usednameaddr := nameaddr ELSE
				    IF kind = chain THEN usednameaddr := alfactp ;
				gendesca (lbase, ldisp,
				  lattr.dplmt MOD bytesinword, l9, byteinbyte, tn) ;
				ended := true ;
			        END (* SIZE 1 *) ;
		        END (* LATTR.PCKD *) ELSE
		        IF val = 0 THEN
			BEGIN
			  calcvarient (lattr, lbase, ldisp, ltag) ;
			  WITH lattr DO
			    IF kind = varbl THEN usednameaddr := nameaddr ELSE
			      IF kind = chain THEN usednameaddr := alfactp ;
			  genstand (lbase, ldisp, istz, ltag) ;
			  ended := true ;
			END (* VAL=0 *) ;
		    END (* KIND=SVAL *) ;
		  IF NOT ended THEN
		    BEGIN
		      choicerarq ;
		      transfer (lattr, out) ;
		    END (* NOT ENDED *) ;
		END (* NUMERIC,SCALAR *) ;
	        power, pointer :
		IF typtr = lamptr THEN
		  BEGIN
		    IF varissimple (lattr) THEN
		      BEGIN
		        lbase := lattr.basereg ;
		        ldisp := lattr.dplmt DIV bytesinword ;
		      END ELSE
		      BEGIN
		        loadadr (lattr, nreg) ; lbase := currentpr ;
		        freebloc (currentbloc) ; ldisp := 0 ;
		        lattr.dplmt := 0 ;
		      END ;
		    IF lattr.pckd THEN
		      lsize := packedsize (lattr.typtr) ELSE
		      lsize := lattr.typtr@.size ;
		    mfari1 := a0r0i0 ; (* DUMMY *) mfari2 := a1r0i0 ;
		    geneism (imlr, 0 (* FILL BYTE *), p0t0r0) ;
		    gendesca (nreg, 0, 0, l9, 0, tn) ;
		    WITH lattr DO
		      IF kind = varbl THEN usednameaddr := nameaddr ELSE
		        IF kind = chain THEN usednameaddr := alfactp ;
		    gendesca (lbase, ldisp, lattr.dplmt MOD bytesinword, l9, lsize, tn) ;
		  END (* LAMPTR *) ELSE
		  BEGIN
		    IF generic^.form = power THEN
		      IF gattr.kind = sval THEN
		        BEGIN
			checkminmax (gattr.val MOD 1000, lattr.typtr^.elset, 305) ;
			checkminmax (gattr.val DIV 1000, lattr.typtr^.elset, 305) ;
		        END ;
		    transfer (gattr, inacc) ;
		    transfer (lattr, out) ;
		  END ;
	        arrays, records :
		IF NOT conformantdim (lattr.typtr) THEN
		  BEGIN
		    lsize := lattr.typtr@.size ;
		    IF isstring (gattr) THEN
		      BEGIN
		        IF kind = chain THEN
			rsize := alfactp@.alfalong ELSE
			rsize := typtr@.size ;
		        IF lsize < rsize THEN
			error (197) ;
		      END (* ISSTRING *) ELSE
		      rsize := lsize ;
		    rbase := nreg ;
		    IF kind # chain THEN
		      IF varissimple (gattr) THEN
		        BEGIN
			rbase := basereg ;
			rdisp := dplmt DIV bytesinword ; rmod := dplmt MOD bytesinword ;
		        END ;
		    IF rbase = nreg THEN
		      BEGIN
		        loadadr (gattr, pr3) ;
		        rbase := pr3 ; rdisp := 0 ; rmod := 0 ;
		      END ;
		    IF varissimple (lattr) THEN
		      BEGIN
		        lbase := lattr.basereg ; ldisp := lattr.dplmt DIV bytesinword ;
		        lmod := lattr.dplmt MOD bytesinword ;
		      END ELSE
		      BEGIN
		        lbase := pr2 ; loadadr (lattr, pr2) ; ldisp := 0 ;
		        lmod := 0 ;
		      END ;
		    suplr := sup (rsize, lsize) ;
		    IF envstandard <> stdextend THEN
		      IF rsize <> lsize THEN
		        error (29) ;
		    IF suplr < twoto12 THEN
		      BEGIN
		        mfari1 := a1r0i0 ; mfari2 := a1r0i0 ; lftag := tn ; rgtag := tn ;
		      END ELSE
		      BEGIN
		        mfari1 := a1r1i0 ; mfari2 := a1r1i0 ; lftag := tx6 ; rgtag := tx7 ;
		        IF suplr > twoto17m1 THEN
			error (307) ELSE
			BEGIN
			  genstand (nreg, lsize, ieax6, tn) ;
			  genstand (nreg, rsize, ieax7, tn) ;
			END ;
		        lsize := 0 ; rsize := 0 ;
		      END ;
		    geneism (imlr, ord (' '), p0t0r0) ;
		    WITH gattr DO
		      IF kind = varbl THEN usednameaddr := nameaddr ELSE
		        IF kind = chain THEN usednameaddr := alfactp ;
		    gendesca (rbase, rdisp, rmod, l9, rsize, rgtag) ;
		    WITH lattr DO
		      IF kind = varbl THEN usednameaddr := nameaddr ELSE
		        IF kind = chain THEN usednameaddr := alfactp ;
		    gendesca (lbase, ldisp, lmod, l9, lsize, lftag) ;
		  END (* array not conf, records *) ELSE
		  BEGIN
		    init_desc_address (lattr.nameaddr, lattr) ;
                                                  (* COMPUTE SIZE NOW *)
		    sauvereg (rq, true) ; sauvereg (ra, false) ;
		    rqbox := currentbloc ;

		    ddisp := 0 ;
		    temp := lattr.typtr ;
		    WHILE conformantdim (temp^.aeltype) DO
		      BEGIN
		        ddisp := ddisp + 3 ;
		        temp := temp^.aeltype ;
		      END ;
		    regenere (lattr.descbloc) ;
		    genstand (lattr.descreg, 1 + ddisp, ildq, tn) ; (* MAX       *)
		    genstand (lattr.descreg, ddisp, isbq, tn) ; (*   - MIN   *)
		    genstand (nreg, 1, iadq, tdl) ; (*    +1     *)
		    genstand (lattr.descreg, 2 + ddisp, impy, tn) ; (*  * SUBSIZE   *)
		    IF NOT lattr.typtr^.pack THEN (* SIZE IS IN WORDS *)
		      genstand (nreg, 2, iqls, tn) ; (* IN BYTES NOW *)
		    freebloc (lattr.descbloc) ;

		    init_desc_address (gattr.nameaddr, gattr) ;
		    freebloc (gattr.descbloc) ;

		    mfari1 := a1r1i0 ;
		    mfari2 := a1r1i0 ;
		    regenere (rqbox) ; regenere (lattr.basebloc) ; regenere (gattr.basebloc) ;
		    IF lattr.typtr^.pack THEN (* SIZE IS IN BITS *)
		      BEGIN
		        geneism (icsl, 3, p0t0r0) ;
		        gendescb (gattr.basereg, 0, 0, 0, 0, tql) ;
		        gendescb (lattr.basereg, 0, 0, 0, 0, tql) ;
		      END
		    ELSE
		      BEGIN
		        geneism (imlr, ord (' '), p0t0r0) ;
		        gendesca (gattr.basereg, 0, 0, l9, 0, tql) ;
		        gendesca (lattr.basereg, 0, 0, l9, 0, tql) ;
		      END ;

		    freeattr (gattr) ; freeattr (lattr) ; freebloc (rqbox) ;
		  END ;
	        files : error (146) ;
	      END (* CASE GENERIC@.FORM,WITH GATTR *) ;
	END (* NO=20 *) ;
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN ASSIGN @@@ WITH NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* ASSIGN *) ;


$OPTIONS page $

(* ************************************ REPEATSTAT **************************** *)

    PROCEDURE repeatstat ;

(* C .COMPILATION  OF   REPEAT   <STATE> [;STATE]*  UNTIL  <EXPRESSION>
   .GENJUMP GENERATES  A SKIP IF FALSE
   C *)
(* E ERRORS DETECTED
   6: TYPE OF EXPRESSION MUST BE BOOLEAN
   14: ";"  EXPECTED
   53: "UNTIL" EXPECTED
   61: ILLEGAL BEGINNING SYMBOL FOR A STATEMENT
   E *)
      LABEL
        10,                                       (* INSYMBOL BEFORE CALL OF STATEMENT *)
        20 ;                                      (* CALL OF STATEMENT *)
      VAR
        locrpt : integer ;
        dummy : integer ;
      BEGIN                                       (* REPEATSTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT REPEATSTAT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        locrpt := cb ;                            (* RETURN PLACE *)
        push_lab_pdl ;
        REPEAT                                    (* LOOP  ON  STATEMENTS *)
10 :
	insymbol ;
20 :
	freeallregisters ;
	statement ;
	IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
	IF errcl [no] = begsy THEN
	  BEGIN
	    error (14) ; GOTO 20 ;
	  END ;
	IF no = 25 (* ELSE *) THEN
	  BEGIN
	    error (61) ; GOTO 10 ;
	  END ;
        UNTIL no # 16 (* ; *) ;
        pop_lab_pdl ;
        IF no # 29 (* UNTIL *) THEN
	error (53) ELSE
	BEGIN
	  IF mapswitch THEN statement_begins (true) ;
	  insymbol ;
	  freeallregisters ;
	  expression ;
	  IF gattr.typtr # NIL THEN
	    BEGIN
	      IF gattr.typtr # boolptr THEN
	        error (6) ELSE
	        genjump (dummy, locrpt) ;
	    END ELSE
	    skip (46) ;
	  IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
	END (* UNTIL FOUND *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN REPEATSTAT @@@ WITH NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* REPEATSTAT *) ;


$OPTIONS page $

(* ************************************ WHILESTAT ***************************** *)

    PROCEDURE whilestat ;

(* C   COMPILATION  OF  WHILE  <EXP> DO <STATE>
   C *)
(* E ERRORS DETECTED
   6: BOOLEAN EXPRESSION EXPECTED
   54: "DO" EXPECTED
   E *)
      LABEL
        10,                                       (* EXIT PROCEDURE *)
                                                  (* SKIPS CALL OF STATEMENT *)
        20 ;                                      (* CALL OF STATEMENT *)
      VAR
        locret, locskip : integer ;
      BEGIN                                       (* WHILESTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT WHILESTAT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        locret := cb ;
        locskip := 0 ;                            (* DEFAULT MEANS NO INSER *)
        insymbol ;
        freeallregisters ;
        expression ;
        IF gattr.typtr # NIL THEN
	BEGIN
	  IF gattr.typtr # boolptr THEN
	    error (6) ELSE
	    genjump (locskip, 0) ;
	END ;
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
        IF no # 31 (* DO *) THEN
	BEGIN
	  IF gattr.typtr # NIL THEN error (54) ;
	  skip (31) ;
	  IF no # 31 THEN
	    BEGIN
	      IF gattr.typtr = NIL THEN error (54) ;
	      IF errcl [no] = begsy THEN
	        GOTO 20 ELSE
	        GOTO 10 ;
	    END ;
	END (* NO#31 *) ;
        insymbol ;
20 :
        freeallregisters ;
        push_lab_pdl ; statement ; pop_lab_pdl ;
        genstand (nreg, (locret - cb) DIV bytesinword, itra, tic) ;
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
        IF locskip # 0 THEN
	inser (cb, locskip) ;
10 :
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN WHILESTAT @@@ WITH NO:', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* WHILESTAT *) ;


$OPTIONS page $

(* ************************************ FORSTAT ******************************* *)

    PROCEDURE forstat ;

(* C      CHECKS  CONTROL VARIABLE
   COMPUTE FIRST EXP
   COMPUTE SECOND EXP
   STORE   IF NOT SVAL
   TEST    ONE EXECUT
   E1   STORE   FIRST IN CONTROL
   E2
   <STATEMENT>
   RELOAD  CONTROL
   TEST    ENDED
   YES     GOTO E3
   NO      IF"TO"      AOS  ,GOTO  E2
   IF "DOWNTO" SUB 1,GOTO  E1
   E3
   C *)
(* E ERRORS DETECTED
   2: ID.  EXPECTED
   51: ":=" EXPECTED
   54: "DO" EXPECTED
   55: "TO/DOWNTO" EXPECTED
   103: ID. IS NOT OF APPROPRIATE CLASS
   104: ID. NOT DECLARED
   145: TYPE CONFLICT
   194: CONTROL VARIABLE MUST BE DECLARED AND USED AT SAME LEVEL
   195:  SCALAR OR NUMERIC FOR CONTROL VARIABLE
   196:  VARIABLE MUST NOT BE ASSIGNED
   199: CONTROL VARIABLE CANNOT BE FORMAL OR EXPORTABLE
   303:  VALUE ASSIGNED  OUT OF BOUNDS
   E *)
      LABEL
        10,                                       (* EXIT PROC *)
        20 ;                                      (* STATEMENT *)
      VAR
        lptcont : ctp ;                           (* NIL  IF ERROR ON CONTROL VARIABLE *)
        lstate : integer ;                        (* REACHES  3  IF NO ERROR *)
        oldlcsave, retdispw : integer ;
        controlnameaddr, typvar, generic : ctp ;
        ldispw, lcl, supval, highdispw, locskip1, locskip2, downret, toret : integer ;
        lbase : preg ;
        totransfer, skipall, lcompare, supissval : boolean ;
        lattr : attr ;
        ldest : destination ;
        lstor, ejump, bjump, lsub, lcomp, lload : istand ;
      BEGIN                                       (* FORSTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT FORSTAT @@@ WITH LCSAVE:', lcsave) ; nextline ;
	END ;
$OPTIONS compile = true $
        controlnameaddr := NIL ;
        lstate := 0 ;                             (* MUST BE 3  IF NO ERROR *)
        oldlcsave := lcsave ;
        highdispw := lcsave DIV bytesinword ;
        lcsave := lcsave + bytesinword ;
        IF lcsave > tmax THEN tmax := lcsave ;
        lptcont := NIL ;                          (* DEFAULT  IF ERROR *)
        typvar := NIL ;
        insymbol ;                                (* CONTROL VARIABLE *)
        IF no # 1 THEN
	error (2) ELSE
	BEGIN
	  search ;
	  IF ctptr = NIL THEN
	    error (104) ELSE
	    BEGIN
	      IF symbolmap THEN nameisref (ctptr, symbolfile, -symbolline) ;
	      IF ctptr@.klass # vars THEN
	        error (103) ELSE
	        IF ctptr@.vtype # NIL THEN
		IF NOT (ctptr@.vtype@.form IN [numeric, scalar]) THEN
		  error (195) ELSE
		  BEGIN lstate := 1 ;         (* NO ERROR HERE FLAG *)
		    IF ctptr@.vlevel # level THEN error (194) ;
		    IF ctptr@.visreadonly THEN error (196) ;
		    IF ctptr@.vkind # actual THEN error (199) ;
		    lptcont := ctptr ; typvar := ctptr@.vtype ;
		    ldispw := ctptr@.vaddr DIV bytesinword ;
		    IF level = 0 THEN
		      lbase := prstatic ELSE lbase := pr6 ;
		    controlnameaddr := ctptr ;
		    WITH ctptr@ DO
		      BEGIN
		        visused := true ;
		        visset := true ;      (* VISREADONLY AFTER "DO" *)
		      END ;
		    insymbol ;
		  END ;
	    END ;
	END (* NO=1 *) ;
                                                  (* CHECK  := *)
        IF no # 20 THEN
	BEGIN
	  IF lptcont # NIL THEN error (51) ;
	  skip (20) ;
	  IF no # 20 THEN
	    BEGIN lstate := 0 ;                 (* ERROR FLAG *)
	      IF lptcont = NIL THEN error (51) ;
	      IF errcl [no] = begsy THEN
	        GOTO 20 (* STATEMENT *) ELSE
	        GOTO 10 (* EXIT PROC *) ;
	    END ;
	END ;
                                                  (* ANALYSIS  OF  FIRST EXPRESSION *)
        freeallregisters ;
        insymbol ; expression ;
        compatbin (typvar, gattr.typtr, generic) ;
        IF (generic = NIL) OR (generic = realptr) THEN
	error (145) ELSE
	BEGIN                                   (* NO TYPE ERROR *)
	  WITH gattr DO
	    IF kind = sval THEN
	      checkminmax (val, typvar, 303) ELSE
	      BEGIN                             (* NOT SVAL *)
	        totransfer := true ;
	        IF kind = varbl THEN
		IF NOT asscheck THEN
		  IF varissimple (gattr) THEN
		    totransfer := false ;
	        IF totransfer THEN
		BEGIN
		  IF kind # lval THEN
		    transfer (gattr, inq) ;
		  IF asscheck THEN
		    checkbnds (forerricode, ldreg, typvar) ;
		END (* TOTRANSFER *) ;
	      END (* NOT SVAL, WITH GATTR *) ;
	  lstate := lstate + 1 ;                (* NO ERROR HERE FLAG *)
	END (* NO TYPE ERROR *) ;
                                                  (* ANALYSIS  OF  TO/DOWNTO *)
        IF no # 33 (* TO/DOWNTO *) THEN
	BEGIN
	  IF gattr.typtr # NIL THEN error (55) ;
	  skip (33) ;
	  IF no # 33 THEN
	    BEGIN lstate := 0 ;                 (* ERROR FLAG *)
	      IF gattr.typtr = NIL THEN error (55) ;
	      IF errcl [no] = begsy THEN
	        GOTO 20 (* STATE *) ELSE
	        GOTO 10 ;                       (* END PROC *)
	    END ;
	END (* NO#33 *) ;
                                                  (* ANALYSIS OF ENDING EXPRESSION *)
        lcl := cl ;                               (* 1:TO  2:DOWNTO *)
        lattr := gattr ;
        insymbol ; expression ;
        compatbin (typvar, gattr.typtr, generic) ;
        IF (generic = NIL) OR (generic = realptr) THEN
	error (145) ELSE
	BEGIN                                   (* NO TYPE ERR *)
	  WITH gattr DO
	    IF kind = sval THEN
	      BEGIN
	        supissval := true ; supval := val ;
	        checkminmax (val, typvar, 303) ;
	      END (* SVAL *) ELSE
	      BEGIN                             (* NOT SVAL *)
	        supissval := false ;
	        IF kind # lval THEN
		BEGIN
		  IF lattr.kind # lval THEN ldest := inacc ELSE
		    IF lattr.ldreg = ra THEN ldest := inq ELSE ldest := inacc ;
		  transfer (gattr, ldest) ;
		END ;
	        IF asscheck THEN
		checkbnds (forerrscode, ldreg, typvar) ;
	        usednameaddr := controlnameaddr ;
	        genstand (pr6, highdispw, opaq [stor, ldreg], tn) ;
	        freebloc (ldregbloc) ;
	      END (* NOT SVAL,WITH GATTR *) ;
	  lstate := lstate + 1 ;                (* NO ERROR HERE *)
	END (* NO TYPE ERROR *) ;
                                                  (* ANALYSIS FOR DO *)
        IF no # 31 (* DO *) THEN
	BEGIN
	  IF gattr.typtr # NIL THEN error (54) ;
	  skip (31) ;
	  IF no # 31 THEN
	    BEGIN lstate := 0 ;                 (* ERROR FLAG *)
	      IF gattr.typtr = NIL THEN error (54) ;
	      IF errcl [no] = begsy THEN
	        GOTO 20 (* STATEMENT *) ELSE
	        GOTO 10 (* EXIT PROC *) ;
	    END ;
	END (* NO # 31 *) ;
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
                                                  (*  CODE  GENERATION *)
        IF lstate = 3 (* NO ERROR *) THEN
	BEGIN
	  skipall := false ; lcompare := true ;
	  IF lattr.kind = sval THEN
	    BEGIN
	      IF supissval THEN
	        BEGIN
		lcompare := false ;
		IF lcl = 1 (* TO *) THEN
		  BEGIN
		    IF lattr.val > supval THEN skipall := true ;
		  END ELSE                    (* DOWNTO *)
		  IF lattr.val < supval THEN skipall := true ;
	        END (* SUPISSVAL *) ;
	    END (* LATTR IS SVAL *) ELSE
	    IF lattr.kind = lval THEN
	      lvalvarbl (lattr) ;
	  IF lattr.kind # lval THEN
	    transfer (lattr, inacc) ;
	  IF lattr.ldreg = ra THEN
	    BEGIN
	      lcomp := icmpa ; lstor := ista ; lload := ilda ; lsub := isba ;
	    END (* RA *) ELSE
	    BEGIN                               (* RQ *)
	      lcomp := icmpq ; lstor := istq ; lload := ildq ; lsub := isbq ;
	    END (* RQ *) ;
	  IF lcl = 1 (* TO *) THEN
	    BEGIN bjump := itpnz ; ejump := itpl ; END ELSE
	    BEGIN bjump := itmi ; ejump := itmoz END ;
	  IF skipall THEN
	    BEGIN
	      locskip1 := indfich ; genstand (nreg, 0, itra, tic) ;
	    END ELSE
	    IF lcompare THEN
	      BEGIN
	        IF supissval THEN
		gencstecode (supval, lcomp) ELSE
		genstand (pr6, highdispw, lcomp, tn) ;
	        locskip1 := indfich ; genstand (nreg, 0, bjump, tic) ;
	      END (* LCOMPARE *) ELSE locskip1 := 0 ;
	  downret := cb ; usednameaddr := controlnameaddr ; genstand (lbase, ldispw, lstor, tn) ;
	  freebloc (lattr.ldregbloc) ;
	  toret := cb ;
	END (* NO ERROR *) ;
        insymbol ;
20 :
        IF lptcont # NIL THEN lptcont@.visreadonly := true ;
        freeallregisters ;
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
        push_lab_pdl ; statement ; pop_lab_pdl ;
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
                                                  (* NOW  ENDING  OF LOOP *)
        IF lstate = 3 (* NO ERROR *) THEN
	BEGIN
                                                  (* RELOAD  CONTROL VARIABLE *)
	  IF mapswitch THEN statement_begins (true) ;
	  usednameaddr := controlnameaddr ;
	  genstand (lbase, ldispw, lload, tn) ;
	  IF supissval THEN
	    gencstecode (supval, lcomp) ELSE
	    genstand (pr6, highdispw, lcomp, tn) ;
	  locskip2 := indfich ; genstand (nreg, 0, ejump, tic) ;
	  IF lcl = 1 THEN
	    BEGIN
	      usednameaddr := controlnameaddr ;
	      genstand (lbase, ldispw, iaos, tn) ;
	      retdispw := (toret - cb) DIV bytesinword ;
	    END ELSE
	    BEGIN                               (* DOWNTO *)
	      genstand (nreg, 1, lsub, tdl) ;
	      retdispw := (downret - cb) DIV bytesinword ;
	    END (* DOWNTO *) ;
	  genstand (nreg, retdispw, itra, tic) ;
	  IF locskip1 # 0 THEN inser (cb, locskip1) ;
	  inser (cb, locskip2) ;
	END (* LSTATE=3  NO ERROR *) ;
        IF lptcont # NIL THEN
	lptcont@.visreadonly := false ;
10 :                                              (* EXIT PROC *)
        lcsave := oldlcsave ;
        IF mapswitch THEN statement_ends (3) ;    (* "end" *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN FORSTAT @@@ WITH LCSAVE,NO', lcsave, no) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* FORSTAT *) ;


$OPTIONS page $

(* ************************************ GOTOSTAT ****************************** *)

    PROCEDURE gotostat ;

(* C .INSTRUCTION COMPILED IS    GOTO   <INTEGER> .
   .ALL DECLARED LABELS ARE   IN LABTAB   FROM  1  TO  CLABIX
   .IF DECLARED LEVEL IS CURRENT LEVEL ,THEN IT IS A LOCAL GOTO
   (FORWARDS  IF  LABDEF=0 , BACKWARDS OTHERWISE).
   IF NOT,  RETURNS  IN A PREVIOUS  PROC, THEN IT IS NECESSARY TO CLOSE
   LOCAL LIVING FILES  ,AND TO GET THE OLD STACK FRAME.
   C *)
(* E ERRORS DETECTED
   15: INTEGER EXPECTED
   167: UNDECLARED LABEL
   E *)
      LABEL
        20 ;                                      (* EXIT OF LOOP *)
      VAR
        it : integer ;
        refbox : refptr ;
      BEGIN                                       (* GOTOSTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT GOTOSTAT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        insymbol ;
        IF (no # 2) OR (cl # 1) THEN              (* NOT AN INTEGER CSTE *)
	BEGIN
	  error (15) ; skip (46) ;
	END ELSE
	BEGIN
                                                  (* SEARCHS IVAL  IN LABTAB *)
	  FOR it := clabix DOWNTO 1 DO
	    WITH labtab [it] DO
	      IF labval = ival THEN             (* LABEL FOUND *)
	        BEGIN
		IF labbox <> NIL THEN
		  WITH labbox^ DO
		    BEGIN
		      refbox := references ;
		      IF refbox <> NIL THEN BEGIN
			IF refbox^.refnbr = maxref THEN BEGIN
			    new (refbox) ;
			    WITH refbox^ DO
			      BEGIN
			        nextref := references ;
			        references := refbox ;
			        refnbr := 1
			      END ;
			  END
			ELSE
			  WITH refbox^ DO
			    refnbr := refnbr + 1 ;
			WITH refbox^ DO
			  WITH refs [refnbr] DO BEGIN
			      filen := symbolfile ;
			      place := ic ;
			      IF lablev <> level THEN
			        linen := -symbolline
			      ELSE
			        linen := symbolline ;
			    END ;
		        END ;
		    END ;
		IF lablev # level THEN
		  BEGIN                       (* GOTO EXIT *)
                                                  (* REMOVE  FRAMES *)
		    IF (lablev = 0) AND exportablecode THEN
		      BEGIN
		        IF NOT linktomain THEN
			BEGIN
			  linktomainplace := lkc ; lkc := lkc + bytesindword ;
			  linktomain := true
			END ;
		        genstand (prlink, linktomainplace DIV bytesinword, iepp1, tny) ;
		        IF labexit = 0 THEN
			BEGIN
			  labexit := lkc ; lkc := lkc + bytesindword
			END ;
		        genstand (prlink, labexit DIV bytesinword, iepp2, tny) ;
		        genstand (pr0, gotoexitextplace, itsp3, tn) ;
		      END
		    ELSE
		      BEGIN
		        loadbase (lablev) ;
		        IF currentpr # pr1 THEN genstand (currentpr, 0, iepp1, tn) ;
		        freebloc (currentbloc) ;
		        IF labexit = 0 THEN   (* FIRST OCCUR *)
			BEGIN
			  labexit := lkc ; lkc := lkc + bytesindword ;
			END ;
		        genstand (prlink, labexit DIV bytesinword, iepp2, tny) ;
		        genstand (pr0, gotoexitplace, itsp3, tn) ;
		      END ;
                                                  (* EXIT LOOP *) GOTO 20 ;
		  END (* GOTO EXT *) ELSE
		  BEGIN                       (* LOCAL GOTO *)
		    IF labdef # 0 THEN        (* ALREADY DEFINED *)
		      genstand (nreg, (labdef - cb) DIV bytesinword, itra, tic) ELSE
		      BEGIN                   (* NOT YET RESOLVED *)
		        enterundlab (labch1) ;
		        genstand (nreg, 0, itra, tic) ;
		      END (* NOT YET RESOLV. *) ;
                                                  (* EXIT LOOP *) GOTO 20 ;
		  END (* LOCAL GOTO,IF,IF,WITH,FOR *) ;
	        END ;
                                                  (* AT THIS POINT, *)
                                                  (* NOT FOUND INTEGER IN LABTAB *)
	  error (167) ;
20 :                                              (* EXIT LOOP FOR *)
	  insymbol ;
	  IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
	END (* INTEGER FOUND *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN GOTOSTAT @@@ WITH NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* GOTOSTAT *) ;


$OPTIONS page $

(* ************************************ IFSTAT ******************************** *)

    PROCEDURE ifstat ;

(* C .COMPILATION OF   IF   <EXPRESSION>  THEN   <STATE>
   IF   <EXPRESSION>  THEN   <STATE>  ELSE  <STATE>
   .GENJUMP GENERATES A  BRANCH  USING THE SETTING OF CONDITION CODES
   C *)
(* E ERRORS DETECTED
   6 : BOOLEAN EXPRESSION EXPECTED
   52 : "THEN" EXPECTED
   E *)
      LABEL
        20,                                       (* CALL OF STATEMENT  AFTER THEN *)
        30 ;                                      (* SKIP    STATEMENT  AFTER THEN *)
      VAR
        locthen, locelse : integer ;
      BEGIN                                       (* IFSTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT IFSTAT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        locthen := 0 ;                            (* DEFAULT MEANS NO INSER TO DO *)
        freeallregisters ;
        insymbol ; expression ;
        IF gattr.typtr # NIL THEN
	BEGIN
	  IF gattr.typtr # boolptr THEN
	    error (6) ELSE
	    genjump (locthen, 0) ;
	END ;
        IF no # 24 (* THEN *) THEN
	BEGIN
	  IF gattr.typtr # NIL THEN error (52) ;
	  skip (24) ;
	  IF no # 24 THEN
	    BEGIN
	      IF gattr.typtr = NIL THEN error (52) ;
	      IF errcl [no] = endsy THEN
	        GOTO 30 ELSE
	        GOTO 20 ;
	    END ;
	END (* NO#24 *) ;
        insymbol ;
        IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
20 :
        freeallregisters ;
        push_lab_pdl ; statement ; pop_lab_pdl ;
30 :
        IF no = 25 (* ELSE *) THEN
	BEGIN
	  locelse := indfich ; genstand (nreg, 0, itra, tic) ;
	  IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
	END ;
        IF locthen # 0 THEN
	inser (cb, locthen) ;
        IF no = 25 (* ELSE *) THEN
	BEGIN
	  insymbol ;
	  freeallregisters ;
	  push_lab_pdl ; statement ; pop_lab_pdl ;
	  inser (cb, locelse) ;
	  IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
	END ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN IFSTAT @@@ WITH NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* IFSTAT *) ;


$OPTIONS page $

(* ************************************ CASESTAT ****************************** *)

    PROCEDURE casestat ;

(* C .ANALYSIS AND  CODE GENERATION  FOR THE  STATEMENT    <CASE>
   .GENERATED CODE IS THE FOLLOWING
   ********************
   *                  *
   *  SELECTOR IN RA  *
   ***              ***
   TRA   SWITCH
   ***              ***
   *                  *    FOR MIN, MIN+2
   E1 *   STATEMENT_1    *
   *                  *
   *  TRA   END       *
   ................
   *                  *    FOR MAX
   EN *   STATEMENT_N    *
   *
   *  TRA   END
   ********************
   *                  *
   SWITCH *  RA TO ZERO POINT*
   *  RA IN [MIN..MAX]*
   *  TRA  VECTOR[RA] *
   *                  *
   ********************
   VECTOR *    TRA E1        *   MINSELECT
   *    TRA END       *   MIN+1
   *    TRA E1        *   MIN+2
   *    .......       *   .....
   *    TRA EN        *   MAXSELECT
   ********************
   END.
   C *)
(* E ERRORS DETECTED
   1: SCALAR OR NUMERIC EXPECTED AS SELECTOR
   7  ":" EXPECTED
   8: "OF" EXPECTED
   13  "END" EXPECTED
   14 : ";" EXPECTED
   20  "," EXPECTED
   23  "CASE LABEL" EXPECTED
   60: "OR" NOT ALLOWED AS MONADIC OPERATOR
   61 : ILLEGAL BEGINNING SYMBOL FOR A STATEMENT
   103  IDENTIFIER IS NOT OF APPROPRIATE CLASS
   104  UNDECLARED IDENTIFIER
   105  SIGN NOT ALLOWED HERE
   147  TYPE CONFLICT  WITH THE CASE SELECTOR
   148  CASE VECTOR TRANSFER TOO LARGE
   156  DUPLICATE CASE LABEL
   304  VALUE OUT OF BOUNDS
   E *)
      LABEL
        1, (* EXIT WHILE *)                       (* INSERTION OF A NEW LABEL BOX *)
        2,                                        (* SKIP HERE IF DUPLICATE CASE LABEL *)
        3,
        10 ;                                      (* EXIT PROC BEFORE ALL THE "DISPOSE" *)
      TYPE
        ptcas = @ reccas ;
        reccas = RECORD
	next : ptcas ;                          (* LINK IN GROWING ORDER THESE BOXES *)
	cslab : integer ;                       (* SELECTING CASE LABEL  VALUE *)
	addr : integer ;                        (* "CB" VALUE OF FIRST INSTR. *)
                                                  (* FOR THIS STATEMENT *)
        END ;
                                                  (* SUCH A BOX IS BUILD *)
                                                  (* FOR  EACH CASE LABEL *)
        ptend = @ recend ;
        recend = RECORD
	succ : ptend ;                          (* REVERSE LINK *)
	indf : integer ;                        (* PLACE WHERE AN INSERTION OF *)
                                                  (* EXIT ADDRESS MUST BE MADE *)
        END ;
      VAR
        seltype, labtype, generic : ctp ;
        locitra, locminexit, locmaxexit, loctabl, otherwiseplace : integer ;
        firstetiq, firstcase, minus, sign, stoploop, noterr, errintab, ierr, otherwise : boolean ;
        headcase, ptboxcur, ptlast, workpt, savept : ptcas ;
        ptchnend, pttetend, savept2 : ptend ;
        lastgen, longtabl, maxselect, minselect, valselect : integer ;
      BEGIN                                       (* CASESTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT CASESTAT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        otherwise := false ;
        seltype := NIL ; locitra := 0 ;
        headcase := NIL ; pttetend := NIL ;
        minselect := 0 ; maxselect := 0 ;
                                                  (* *SELECTOR ANALYSIS *)
        freeallregisters ;
        insymbol ; expression ;
        WITH gattr DO
	BEGIN
	  IF typtr # NIL THEN
	    IF typtr@.form IN [numeric, scalar] THEN
	      BEGIN
	        transfer (gattr, inacc) ; freebloc (gattr.ldregbloc) ;
	        seltype := typtr ;
	        locitra := indfich ; genstand (nreg, 0, itra, tic) ;
	      END ELSE
	      error (1) ;
	END (* WITH GATTR *) ;
                                                  (* *)
                                                  (* <OF> *)
                                                  (* *)
        IF no # 27 THEN
	BEGIN
	  IF gattr.typtr # NIL THEN error (8) ;
	  skip (27) ;
	  IF no # 27 THEN
	    IF gattr.typtr = NIL THEN error (8) ;
	END ELSE
	insymbol ;
        noterr := true ;
                                                  (* *)
                                                  (* ** MAIN LOOP  ON STATEMENT  BLOCKS *)
                                                  (* *)
        firstcase := true ;
        REPEAT
	IF no = 7 (*  + - OR *) THEN
	  BEGIN
	    minus := cl = 2 ;                   (* - *)
	    IF cl = 3 THEN error (60) ;
	    insymbol ; sign := true ;
	  END ELSE
	  BEGIN
	    minus := false ; sign := false ;
	  END ;
	IF (no <= 2) THEN                       (* CAN BE  A CASE LABEL *)
	  BEGIN
	    stoploop := false ;
	    firstetiq := true ;
	    REPEAT                              (* LOOP  ON LABEL(S)   FOR ONE  BLOCK *)
	      labtype := NIL ;
	      IF no = 1 (* ID *) THEN
	        BEGIN
		search ;
		IF ctptr = NIL THEN
		  BEGIN
		    IF firstetiq AND (NOT firstcase) THEN
		      IF envstandard > stdsol THEN
		        IF (aval = usednames [6]) THEN (* OTHERWISE !! *)
			BEGIN
			  otherwise := true ;
			  otherwiseplace := cb ;
			  REPEAT
			    insymbol ;
			    freeallregisters ;
3 :			    push_lab_pdl ; statement ; pop_lab_pdl ;
			    IF errcl [no] = begsy THEN
			      BEGIN
			        error (14) ; GOTO 3 ;
			      END ;
			    IF no = 25 (* ELSE *) THEN
			      BEGIN
			        error (61) ; insymbol ; GOTO 3 ;
			      END ;
			  UNTIL no <> 16 (* ; *) ;
			  IF no <> 22 THEN BEGIN
			      error (13) ;
			      GOTO 10 ;
			    END ;
			  new (ptchnend) ; IF ptchnend = NIL THEN heaperror ; (* EXIT COMP *)
			  ptchnend@.succ := pttetend ; pttetend := ptchnend ;
			  ptchnend@.indf := indfich ;
			  genstand (nreg, 0, itra, tic) ; (* EXIT OF CASE *)
			  GOTO 10 ;
			END ;
		    error (104) ; insymbol ; skip (46) ;
		  END ELSE
		  BEGIN
		    IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		    WITH ctptr@ DO
		      IF klass # konst THEN
		        BEGIN
			IF klass >= vars THEN stoploop := true ;
			error (103) ; insymbol ; skip (46) ;
		        END (* # KONST *) ELSE
		        BEGIN                 (* KONST *)
			IF contype # NIL THEN
			  BEGIN
			    labtype := contype ;
			    IF minus THEN valselect := -values
			    ELSE valselect := values ;
			  END ;
		        END (* KONST *) ;
		  END
	        END (* NO=1 *) ELSE
	        IF no = 2 (* CSTE *) THEN
		BEGIN
		  CASE cl OF
		    1 : labtype := intptr ;
		    2, 3 : error (1) ;
		    4 : labtype := charptr ;
		  END ;
		  IF labtype # NIL THEN
		    IF minus THEN valselect := -ival ELSE valselect := ival ;
		END (* NO=2 *) ELSE
		BEGIN
		  error (23) ;
		END ;
                                                  (* TYPE  COMPATIBILTY *)
	      IF labtype # NIL THEN
	        BEGIN
		IF seltype = NIL THEN
		  seltype := labtype ELSE
		  BEGIN
		    compatbin (seltype, labtype, generic) ;
		    IF (generic = NIL) OR (generic = realptr) THEN
		      BEGIN
		        error (147) ; labtype := NIL ;
		      END ELSE
		      BEGIN
		        IF generic@.form # numeric THEN
			IF sign THEN error (105) ;
		        checkminmax (valselect, seltype, 304) ;
		      END (* GENERIC NOT NIL *) ;
		  END (* SELTYPE#NIL *) ;
	        END (* LABTYPE #NIL *) ;
	      IF labtype # NIL THEN
	        BEGIN
		noterr := true ;
		ptboxcur := headcase ; ptlast := NIL ;
		WHILE ptboxcur # NIL DO
		  BEGIN
		    IF ptboxcur@.cslab >= valselect THEN
		      BEGIN
		        IF ptboxcur@.cslab = valselect THEN
			BEGIN error (156) ; GOTO 2 ;
			END ;
		        GOTO 1 ;              (*  EXIT LOOP *)
		      END ;
		    ptlast := ptboxcur ;
		    ptboxcur := ptboxcur@.next ;
		  END ;
                                                  (* HERE  MAXSELECT MUST BE CHANGED. *)
                                                  (* BOXES ARE LINKED VIA  NEXT *)
                                                  (* IN GROWTHING ORDER *)
                                                  (* HEADCASE   POINTS  THE SMALLEST *)
		maxselect := valselect ;
1 :                                               (* CREATES A NEW LABEL BOX *)
		new (workpt) ; IF workpt = NIL THEN heaperror ; (* EXIT COMP *)
		WITH workpt@ DO
		  BEGIN
		    next := ptboxcur ; cslab := valselect ; addr := cb ;
		  END ;
		IF ptlast = NIL THEN          (* BOX =NEW BEGINNING OF LIST *)
		  BEGIN
		    headcase := workpt ; minselect := valselect ;
		  END ELSE
		  ptlast@.next := workpt ;
	        END (* LABTYPE#NIL,CREATES THEN A NEW LABEL BOX *) ;
2 :                                               (* SKIP HERE IF DUPLICATE LABEL *)
	      IF NOT stoploop THEN
	        BEGIN
		insymbol ;
		IF no = 19 (* : *) THEN
		  stoploop := true ELSE
		  BEGIN
		    IF no = 15 THEN           (* , *)
		      BEGIN
		        insymbol ;
		        IF no = 7 THEN
			BEGIN
			  minus := cl = 2 ; sign := true ;
			  IF cl = 3 THEN error (60) ;
			  insymbol ;
			END ELSE
			BEGIN
			  minus := false ; sign := false ;
			END ;
		      END (* NO=15 *) ELSE
		      error (20) ;
		    ierr := false ;
		    WHILE NOT (no IN [1, 2, 19]) AND (errcl [no] = irrelsy) DO
		      BEGIN
		        insymbol ; ierr := true ;
		      END ;
		    IF ierr THEN error (7) ;
		    IF no > 2 THEN stoploop := true ;
		  END (* NO#19 *) ;
	        END (* NOT STOPLOOP *) ;
	      firstetiq := false ;
	    UNTIL stoploop ;
	    IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
                                                  (* *)
                                                  (* STATEMENT  BLOCK *)
                                                  (* *)
	    IF (no = 19) OR (errcl [no] = begsy) THEN
	      BEGIN
	        IF no = 19 THEN insymbol ;
	        freeallregisters ;
                                                  (* ********* *)
	        push_lab_pdl ; statement ; pop_lab_pdl ;
                                                  (* ********* *)
	      END ;
	    new (ptchnend) ; IF ptchnend = NIL THEN heaperror ; (* EXIT COMP *)
	    ptchnend@.succ := pttetend ; pttetend := ptchnend ;
	    ptchnend@.indf := indfich ;
	    genstand (nreg, 0, itra, tic) ;     (* EXIT OF CASE *)
	    IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
	  END (* NO <=2  CAN BE A CASE LABEL *) ELSE
	  BEGIN
	    IF noterr THEN
	      BEGIN
	        error (23) ; noterr := false ;
	      END ;
	    skip (46) ;
	    IF no # 22 THEN
	      IF errcl [no] = begsy THEN
	        statement ELSE
	        insymbol ;
	  END (* NO >2  THEN ERROR *) ;
	IF no = 16 (* ; *) THEN
	  insymbol ELSE
	  IF (no # 22) (* END *) AND noterr THEN
	    BEGIN
	      error (13) ; GOTO 10 ;            (* EXIT LOOP *)
	    END ;
	firstcase := false ;
        UNTIL no = 22 ;                           (* END *)
10 :
        longtabl := maxselect - minselect + 1 ;
        IF longtabl > (maxfich - indfich) DIV bytesinhword THEN
	BEGIN
	  error (148) ;
	  errintab := true ; locmaxexit := 0 ;
	END ELSE
	errintab := false ;
                                                  (* *)
                                                  (* CODE GENERATION *)
                                                  (* *)
        IF mapswitch THEN statement_begins (true) ;
        IF locitra # 0 THEN
	inser (cb, locitra) ;
        IF inxcheck THEN
	IF seltype # NIL THEN
	  checkbnds (caserrcode, ra, seltype) ;
                                                  (* ZERO POINT *)
        IF minselect # 0 THEN
	gencstecode (minselect, isba) ;
                                                  (* NOOP OR STOP IF   < MIN , >MAX *)
                                                  (* *)
        genstand (nreg, 0, icmpa, tdl) ;
        IF otherwise THEN
	genstand (nreg, (otherwiseplace - cb) DIV bytesinword, itmi, tic) ELSE
	BEGIN
	  locminexit := indfich ; genstand (nreg, 0, itmi, tic) ;
	END ;
        IF NOT errintab THEN
	BEGIN
	  genstand (nreg, longtabl - 1, icmpa, tdl) ;
	  IF otherwise THEN
	    genstand (nreg, (otherwiseplace - cb) DIV bytesinword, itpnz, tic) ELSE
	    BEGIN
	      locmaxexit := indfich ; genstand (nreg, 0, itpnz, tic) ;
	    END ;
	END ;
                                                  (* HERE  EXP  IS IN  MIN..MAX *)
        loctabl := indfich ; genstand (nreg, 0, iepp3, tic) ; (* POINTS FIRST SWITCH *)
        genstand (pr3, 0, itra, tal) ;
        inser (cb, loctabl) ;
        lastgen := minselect - 1 ;
        WHILE headcase # NIL DO
	BEGIN
	  IF NOT errintab THEN
	    WHILE headcase@.cslab # lastgen + 1 DO
	      BEGIN
                                                  (* NO OP  THEN EXIT OR STOP *)
	        IF otherwise THEN
		genstand (nreg, (otherwiseplace - cb) DIV bytesinword, itra, tic) ELSE
		genstand (nreg, longtabl + minselect - 1 - lastgen, itra, tic) ;
	        lastgen := lastgen + 1 ;
	      END ;
                                                  (* HERE EQUALITY, *)
                                                  (* THEN GOTO SUITABLE STATEMENT BLOCK *)
	  IF NOT errintab THEN
	    genstand (nreg, (headcase@.addr - cb) DIV bytesinword, itra, tic) ;
	  lastgen := headcase@.cslab ;
	  savept := headcase ; headcase := headcase@.next ;
	  savept := NIL ;
	END (* WHILE *) ;
        IF NOT otherwise THEN
	BEGIN
	  inser (cb, locminexit) ;
	  IF locmaxexit # 0 THEN
	    inser (cb, locmaxexit) ;
	  IF inxcheck THEN
	    BEGIN IF minselect # 0 THEN gencstecode (minselect, iada) ;
	      genexceptcode (caserrcode, ra) ;
	    END ;
	END ;
                                                  (* INSER  ALL  ENDING JUMPS *)
        ptchnend := pttetend ;
        WHILE ptchnend # NIL DO
	BEGIN
	  inser (cb, ptchnend@.indf) ; savept2 := ptchnend ;
	  ptchnend := ptchnend@.succ ; savept2 := NIL ;
	END ;
        IF mapswitch THEN statement_ends (3) ;    (* "end" *)
        insymbol ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN CASESTAT @@@ WITH NO,CL:', no : 4, cl : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* CASESTAT *) ;

(* ************************************ STATEMENT ***************************** *)

    PROCEDURE statement ;

(* C  EACH STATEMENT CAN BE PREFIXED BY A LABEL     INT:
   AFTER LABEL ANALYSIS, SPLITSTAT [NO] IS A SWITCH TO SEVERAL PROCEDURES
   C *)

(* E ERRORS DETECTED
   7: ":" EXPECTED
   42 Sol procedure not in  PASCAL
   44 Sol procedure not yet implemented
   45 Extended pascal not allowed at this level
   61: ILLEGAL BEGINNING SYMBOL FOR A STATEMENT
   86: THIS FUNCTION MUST BE ASSIGNED IN HIS BLOCK
   103: IDENTIFIER IS NOT OF APPROPRIATE CLASS
   104: IDENTIFIER NOT DECLARED
   150: ASSIGNMENT TO STANDARD FUNCTION NOT ALLOWED
   165: MULTIDEFINED LABEL
   167: LABEL IS NOT DECLARED
   196: ASSIGNMENT NOT ALLOWED FOR THIS VARIABLE
   306: LABEL MUST HAVE AT MOST 4 DIGITS
   E *)
      LABEL
        1 ;                                       (* EXIT FOR WHEN LABEL FOUND *)
      VAR
        it : integer ;
      BEGIN                                       (* STATEMENT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT STATEMENT @@@ WITH NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* FIRST CHECK FOR LABEL *)
        IF (no = 2) THEN                          (* CSTE  *)
	IF (cl = 1) THEN                        (* "   INTEGER *)
	  BEGIN
	    IF ival > 9999 THEN error (306) ;
	    FOR it := clabix DOWNTO 1 DO
	      WITH labtab [it] DO
	        IF labval = ival THEN           (* FOUND *)
		BEGIN
		  IF labbox <> NIL THEN
		    WITH labbox^ DO
		      BEGIN
		        deffile := symbolfile ; defline := symbolline ; locinbytes := ic ;
		        WITH lab_pdl_top^ DO
			BEGIN
			  next_in_block := first_in_block ;
			  first_in_block := labbox ;
			END ;
		      END ;
		  IF lablev <> level THEN
		    error (167) ELSE
		    IF labdef <> 0 THEN       (* MULTIDEFINED *)
		      error (165) ELSE
		      BEGIN                   (* FIRST OCCUR ==> RESOLVE IT *)
		        labdef := cb ;        (* PLACE IN CODE FOR CURRENT PROCEDURE *)
		        IF labch1 # 0 THEN
			BEGIN               (* USED BEFORE DEFINITION *)
			  inserundlab (cb, labch1) ;
			  labch1 := 0 ;     (* FLAG RESOLVED *)
			END (* USED *) ;
		        IF labexit <> 0 THEN
			BEGIN               (* USED IN GOTO EXIT *)
			  genstand (pr6, pr4depw, iepp4, tny) ;
                                                  (* RESET PR4 DEL. BY UNWINDER OPER. *)
			END (* USED IN GOTO EXIT *) ;
		      END (* FIRST OCCUR,NO ERR *) ;
                                                  (* EXIT LOOP *) GOTO 1 ;
		END (* LABEL FOUND,WITH,FOR *) ;
                                                  (* HERE LABEL NOT FOUND *)
	    error (167) ;
1 :	    insymbol ;
	    IF no = 19 (* : *) THEN
	      insymbol ELSE
	      BEGIN
	        error (7) ; skip (46) ;
	      END ;
	  END (* CL=1, NO=2 *) ;
        freeallregisters ;

        IF splitstat [no] <> 1 THEN
	IF mapswitch THEN statement_begins (true) ;

        CASE splitstat [no] OF
	(* NOOP    *) 1 : (* ENDSY,IRRELSY *) ;
                                                  (* IDENTIF. *) 2 :
	  BEGIN
	    search ;
	    IF ctptr = NIL THEN
	      BEGIN
	        error (104) ; ctptr := undecptr ;
	      END ;
	    WITH ctptr@ DO
	      IF klass <= konst THEN
	        error (103) ELSE
                                                  (* VARS PROC FIELD *)
	        IF klass = proc THEN            (* PROC OR FUNCT *)
		BEGIN
		  IF proctype = ctptr THEN    (* not a function *)
		    BEGIN                     (* PROC *)
		      IF symbolmap THEN
		        nameisref (ctptr, symbolfile, symbolline) ;
		      insymbol ;
		      CASE ploc OF
		        notpredef : BEGIN     (* PROGRAMMER PROC *)
			  passparams (0) ;  (* NOT USED FOR A PROC *)
			END ;
		        instdpure :
			CASE segsize OF
			  0, 1, 2, 3 : getput (segsize) ; (* INCLUDE RESET POINTER *)
			  4, 5 : newir (segsize - 4) ;
			  6, 7 : readir (segsize - 6) ;
			  8, 9, 10 : writeir (segsize - 8) ;
			  11, 12 : pckunpck (segsize - 11) ;
			END ;
		        instdcompiler : insapp (segsize) ;
		        instdsol :
			BEGIN
			  IF envstandard = stdpure THEN
			    error (42) ;
			  CASE segsize OF
			    0, 1, 2, 3, 4, 5, 6 : getput (segsize + 4) ;
			    7 : writeir (3) ;
			    8 : argvstat ;
			    9 : stopstat ;
			  END ;
			END (* INSTDSOL *) ;
		        instdextend :
			BEGIN
			  IF envstandard <> stdextend THEN error (45) ;
			  CASE segsize OF
			    2 : mvcir (0) ;
			    0, 1 : dateandtime (segsize) ;
			    3 : insert_string ;
			    4 : delete_string ;
			  END ;
			END (* INSTDEXTEND *) ;

		      END (* case PLOC *) ;
		    END (*  PROCEDURE *) ELSE
		    BEGIN                     (* FUNCTION IDENTIFIER ASSIGNMENT *)
		      IF ploc <> notpredef THEN
		        BEGIN
			IF symbolmap THEN
			  nameisref (ctptr, symbolfile, -symbolline) ;
			error (150) ; skip (46) ;
		        END ELSE
		        BEGIN
			genstand (nreg, level - proclevel - 1, ilda, tdl) ;
			genstand (pr0, functionvaluesetplace, itsp3, tn) ;
			procisassigned := true ;

			IF NOT procinscope THEN error (86) ;


			assign ;


		        END (* NO ERRORS FOR FUNCT. ID *) ;
		    END (* FUNCT. IDENTIFIER *) ;
		END (* KLASS=PROC *) ELSE
		BEGIN                         (* VARS OR FIELD *)
		  IF klass = vars THEN
		    BEGIN
                                                  (* VISUSED SET IN ADDRESSVAR *)
		      IF visreadonly THEN error (196) ;
		      visset := true ;
		    END (* VARS *) ;


		  assign ;


		END (* VARS OR FIELD *) ;
	    IF mapswitch THEN statement_ends (symbolindex - sttindex) ;
	  END (*  IDENT., SPLITSTAT=2 *) ;
	3 (* BEGIN  *) : compstat ;
	4 (* IF     *) : ifstat ;
	5 (* CASE   *) : casestat ;
	6 (* REPEAT *) : repeatstat ;
	7 (* WHILE  *) : whilestat ;
	8 (* FOR    *) : forstat ;
	9 (* GOTO   *) : gotostat ;
	10 (* WITH   *) : withstat ;
        END (* CASE SPLITSTAT *) ;

(* FREEALLREGISTERS MUST BE CALLED HERE, BECAUSE IT MAY GENERATE CODE
   WHICH IS LOGICALLY RELATED TO CODE GENERATED DURING STATEMENT.
   THIS IS DUE TO NEW STACK EXTENSION MECHANISM, USED FOR SOME TEMPORARY
   VARIABLES, IN STRING EXPRESSIONS EVALUATION *)

        freeallregisters ;
        IF errcl [no] = irrelsy THEN
	BEGIN
	  error (61) ; skip (46) ;
	END ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN STATEMENT @@@ WITH NO=', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* STATEMENT *) ;


(* END OF STATE MODULE ******************************************* *) BEGIN
    END.
