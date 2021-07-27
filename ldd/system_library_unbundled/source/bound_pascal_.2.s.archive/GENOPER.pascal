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


$OPTIONS switch trace := true ; switch security := true $
  PROGRAM genoper ;
    $IMPORT
                                                  (* IMPORTED PROCEDURES  *)
      'CONTEXTTABLE (pascal)' :
        conformantdim,
        create_konst_box,
        create_vars_box,
        create_types_box ;

      'UNIQUE (pascal)' :
        heaperror ;

      'RACINE (pascal)' :
        error,
        nameisref,
        nextline,
        poweroftwo,
        sup ;
      'GENERE (pascal)' :
        gendesca,
        gendescb,
        geneism,
        genstand,
        inser ;
      'STATE (pascal)' :
        addressvar,
        calcvarient,
        choicerarq,
        entercst,
        enterlcst,
        enterllcst,
        enterundlab,
        freebloc,
        gencheckmultover,
        gencstecode,
        genexceptcode,
        getpr,
        loadadr,
        newbloc,
        oldnewstor,
        regenere,
        raisused,
        rqisused,
        sauvereg,
        stack_extension,
        transfer ;
      'MODVARIABLE (pascal)' :
        init_desc_address ;

      'MODATTR (pascal) ' :
        convreal,
        easyvar,
        freeattr,
        initattrvarbl,
        isstring,
        lvalvarbl,
        printattr,
        varissimple ;
                                                  (* IMPORTED VARIABLES *)
      'DECLARE (pascal)' :
        nextalf ;

      'RACINE (pascal)' :
        alfaptr,
        charptr,
        declarationpart,
        envstandard,
        level,
        intptr,
        mpcogout,
        nilptr,
        realptr,
        string_ptr,
        symbolfile,
        symbolline,
        symbolmap ;
      'GENERE (pascal)' :
        cb,
        illegal_generation,
        indfich,
        mfari1,
        mfari2,
        usednameaddr ;
      'STATE (pascal)' :
        asscheck,
        cltransf,
        currentbloc,
        currentpr,
        gattr,
        maxprused,
        modif,
        nilanaq,
        nileraq,
        opaq,
        prinst,
        psrsize,
        revcltransf,
        stattrace $

    $EXPORT
      check_dynamic_string_length,
      gen_delete,
      gen_insert,
      gen_string_comp,
      gen_substring,
      gen_string_position,
      genandor,
      gencompare,
      genconcat,
      gendivmod,
      genjump,
      genopadd,
      genopdivi,
      genopmult,
      genoppw,
      genopsub,
      genptcomp,
      genstcomp $



$OPTIONS page $


$INCLUDE 'CONSTTYPE' $

(* LOCAL TYPES *)

      string_item_info = RECORD
        length_is_known : boolean ;
	len_place : integer ; len_reg : preg ;  (* IF LENGTH OF IN MEMORY *)
	l_tag : tag ; l_val : integer ; mfari : zari ; reg_bloc, len_bloc : regpt ;
	register : preg ;
	bloc : regpt ; bloc_is_new : boolean ;
	length : integer ;
	wdisp, bdisp : integer ;
        END ;

$OPTIONS page $

      VAR

(* REDEFINE IMPORTED VARIABLES     *)
(* FROM DECLARE *)
        nextalf : ctp ;

(* FROM RACINE  *)
        alfaptr : ctp ;
        charptr : ctp ;
        declarationpart : boolean ;
        envstandard : stdkind ;
        intptr : ctp ;
        level : levrange ;
        mpcogout : text ;
        nilptr : ctp ;
        realptr : ctp ;
        string_ptr : ctp ;
        symbolfile, symbolline : integer ;
        symbolmap : boolean ;

(* FROM GENERE  *)
        cb : integer ;
        illegal_generation : boolean ;
        indfich : integer ;
        mfari1 : zari ;
        mfari2 : zari ;
        usednameaddr : ctp ;


(* FROM STATE   *)
        asscheck : boolean ;
        cltransf : ARRAY [1..6] OF integer ;      (* GIVES THE TRANSF CORR. TO OPER.  8,CL *)
        currentbloc : regpt ;
        currentpr : preg ;
        gattr : attr ;
        modif : ARRAY [nxreg..rq] OF tag ;
        maxprused : preg ;
        opaq : ARRAY [typeofop, ra..reaq] OF istand ;
        prinst : ARRAY [epp..lprp, pr1..pr6] OF istand ;
        nilanaq,
        nileraq : setarray ;                      (* USED FOR NIL COMPARISONS *)
        psrsize : integer ;                       (* USEFULL SIZE OF PSR *)
        revcltransf : ARRAY [1..6] OF integer ;   (* GIVES  8,CL --> REVERSE TRANSF *)
        stattrace : levtrace ;

(* **************************  VARIABLES DE GENOPER   *)

        clearpt : setarray ;                      (* Masque de nettoyage du numero de ring dans
                                                     GENPTCOMP  *)




$OPTIONS page $

$OPTIONS page $

      $VALUE

        clearpt = ('777777077777'o, '777777777777'o, 6 * 0) ;

        $

(* REDEFINE IMPORTED PROCEDURES    *)
(* FROM GENERE  *)
      PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ; EXTERNAL ;
      PROCEDURE geneism (fcode : ieism ; ffield : integer ; fbits : zptr) ; EXTERNAL ;
      PROCEDURE gendesca (fareg : preg ; fadr, fcn : integer ; fta : lgcar ;
        fn : integer ; frlgth : mreg) ; EXTERNAL ;
      PROCEDURE gendescb (fareg : preg ; fadr, fc, fb : integer ; fn : integer ;
        frlgth : mreg) ; EXTERNAL ;
      PROCEDURE inser (fcb : integer ; fplace : integer) ; EXTERNAL ;


(* FROM CONTEXTTABLE *)

      PROCEDURE create_konst_box (VAR box : ctp ; fname : alfaid ; typeofconst : consttype) ; EXTERNAL ;
      PROCEDURE create_types_box (VAR tp : ctp ; fname : alfaid ; form : typform ; bool : boolean) ; EXTERNAL ;
      PROCEDURE create_vars_box (VAR tp : ctp ; name : alfaid) ; EXTERNAL ;
      FUNCTION conformantdim (ftp : ctp) : boolean ; EXTERNAL ;

(* FROM UNIQUE *)

      PROCEDURE heaperror ; EXTERNAL ;

(* FROM RACINE  *)
      PROCEDURE error (errno : integer) ; EXTERNAL ;
      PROCEDURE nameisref (ctpt : ctp ; fil, lin : integer) ; EXTERNAL ;
      PROCEDURE nextline ; EXTERNAL ;
      FUNCTION poweroftwo (fval : integer) : integer ; EXTERNAL ;
      FUNCTION sup (fval1, fval2 : integer) : integer ; EXTERNAL ;




(* FROM STATE   *)
      PROCEDURE choicerarq ; EXTERNAL ;
      PROCEDURE enterundlab (VAR fundinx : integer) ; EXTERNAL ;
      PROCEDURE stack_extension ; EXTERNAL ;
      PROCEDURE transfer (VAR fattr : attr ; inwhat : destination) ; EXTERNAL ;
      PROCEDURE freebloc (VAR fbtofree : regpt) ; EXTERNAL ;
      PROCEDURE genexceptcode (errcode : integer ; freg : register) ; EXTERNAL ;
      PROCEDURE getpr ; EXTERNAL ;
      PROCEDURE loadadr (VAR fattr : attr ; wantedpr : preg) ; EXTERNAL ;
      PROCEDURE newbloc (freg : register) ; EXTERNAL ;
      FUNCTION oldnewstor (i : integer) : integer ; EXTERNAL ;
      PROCEDURE regenere (oldbloc : regpt) ; EXTERNAL ;
      PROCEDURE addressvar (fctp : ctp ; VAR fattr : attr ; modif : boolean) ; EXTERNAL ;
      PROCEDURE calcvarient (VAR fattr : attr ; VAR fbase : preg ; VAR fdisp : integer ;
        VAR ftag : tag) ; EXTERNAL ;
      PROCEDURE sauvereg (freg : register ; fload : boolean) ; EXTERNAL ;
      PROCEDURE entercst (fval : integer ; VAR box : wcstpt) ; EXTERNAL ;
      PROCEDURE enterlcst (VAR fval : setarray ; VAR fboxpt : lcstpt) ; EXTERNAL ;
      PROCEDURE enterllcst (VAR fval : setarray ; VAR fboxpt : llcstpt) ; EXTERNAL ;
      FUNCTION raisused : boolean ; EXTERNAL ;
      FUNCTION rqisused : boolean ; EXTERNAL ;
      PROCEDURE gencheckmultover ; EXTERNAL ;
      PROCEDURE gencstecode (i : integer ; finst : istand) ; EXTERNAL ;

(* FROM MODVARIABLE *)
      PROCEDURE init_desc_address (ctpt : ctp ; VAR fattr : attr) ; EXTERNAL ;

(* FROM MODATTR *)

      PROCEDURE convreal (VAR fattr : attr) ; EXTERNAL ;
      PROCEDURE printattr (VAR fattr : attr) ; EXTERNAL ;
      PROCEDURE initattrvarbl (VAR fattr : attr) ; EXTERNAL ;
      FUNCTION isstring (VAR fattr : attr) : boolean ; EXTERNAL ;
      PROCEDURE lvalvarbl (VAR fattr : attr) ; EXTERNAL ;
      FUNCTION easyvar (VAR fattr : attr) : boolean ; EXTERNAL ;
      FUNCTION varissimple (VAR fattr : attr) : boolean ; EXTERNAL ;
      PROCEDURE freeattr (VAR fattr : attr) ; EXTERNAL ;


$OPTIONS page $


$OPTIONS page $

      FUNCTION int_op (op : integer ; int_left, int_right : integer) : integer ;

(* C THIS PROCEDURE COMPUTESC THE RESULT OF OPERATION APPLIEDC TOC TWO
   GIVEN INTEGER OPERANDS
   IT CHECKS INTEGER OVERFLOW OR UNDERFLOW C *)

(* E
   ERROR DETECTED :
   228 : OVERFLOW IN INTGER EXPRESSION
   229 UNDERFLOW IN INTEGER EXPRESSION
   E *)

        VAR
	f_left, f_right, f_res : real ;
	f_min, f_max : real ;

        BEGIN
	int_op := 0 ;
	f_max := maxint ; f_min := -maxint - 1 ;
	f_left := int_left ; f_right := int_right ;
	CASE op OF
	  1 : f_res := f_left * f_right ;
	  2 : f_res := f_left / f_right ;
	  3 : f_res := f_left + f_right ;
	  4 : f_res := f_left - f_right ;
	END ;
	IF f_res > f_max THEN error (228) ELSE
	  IF f_res < f_min THEN error (229) ELSE
	    int_op := trunc (f_res) ;
        END (* INT_OP *) ;
$OPTIONS page $

(* ************************************ GENOPADD ****************************** *)

      PROCEDURE genopadd (VAR fattr : attr ; generic : ctp) ;

(* C . GATTR DESCRIBES  THE  RIGHT OPERAND
   FATTR   "         "   LEFT
   . GENERIC   IS  GENERIC-TYPE   (NUMERIC OR REAL)
   . AT OUTPUT .GATTR   DESCRIBES  RESULT
   .ADDITION IS GENERATED
   C *)
(* E ERRORS DETECTED
   432:  TYPSEQ  0  ==> CHOICE ERROR
   E *)
        VAR
	tattr,                                  (* TRANSFERED  ATTR *)
	cattr : attr ;                          (* ADRESSED  ATTR *)
	typseq : integer ;                      (* SEQUENCE CODE *)
	lbase : preg ;
	ldisp : integer ;
	ltag : tag ;
	linst : istand ;
        BEGIN                                     (* GENOPADD *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENOPADD @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
	typseq := 0 ;
	IF fattr.kind = sval THEN
	  IF fattr.typtr = realptr THEN
	    BEGIN
	      IF fattr.rsval = 0 THEN typseq := 4 ;
	    END ELSE
	    BEGIN
	      IF fattr.val = 0 THEN typseq := 4 ;
	    END
	ELSE
	  IF gattr.kind = sval THEN
	    IF gattr.typtr = realptr THEN
	      BEGIN
	        IF gattr.rsval = 0 THEN typseq := 3 ;
	      END ELSE
	      BEGIN
	        IF gattr.val = 0 THEN typseq := 3 ;
	      END ;
	IF typseq = 0 THEN
	  BEGIN
	    IF generic = realptr THEN
	      BEGIN
	        IF fattr.typtr # realptr THEN
		convreal (fattr) ELSE
		IF gattr.typtr # realptr THEN
		  convreal (gattr) ;
	        linst := idfad ;
	      END (* REAL *) ELSE
	      linst := iada ;
	    IF fattr.kind = lval THEN
	      lvalvarbl (fattr) ;
	    WITH gattr DO
	      CASE fattr.kind OF
	        varbl : BEGIN typseq := 2 ;
		  IF kind = lval THEN
		    IF ldreg = rq THEN typseq := 10 ;
		END ;
	        sval : CASE kind OF
		  varbl : typseq := 2 ;
		  lval : IF ldreg = rq THEN typseq := 10 ELSE typseq := 2 ;
		  sval : BEGIN
		      typseq := 2 ;
		      IF generic = realptr THEN
		        BEGIN
			IF abs (fattr.rsval) < maxint THEN
			  IF abs (rsval) < maxint THEN
			    IF abs (rsval) >= 1 THEN
			      typseq := 12 ;
		        END ELSE
		        typseq := 12 ;
		    END (* SVAL *) ;
		END (* CASE GATTR.KIND  FOR  FATTR  SVAL *) ;
	        lval : CASE fattr.ldreg OF
		  ra : IF kind = varbl THEN
		      IF easyvar (gattr) THEN typseq := 1 ELSE typseq := 2
		    ELSE
		      IF kind = sval THEN
		        typseq := 1 ELSE
		        typseq := 13 ;
		  rq :
		    IF gattr.kind = varbl THEN
		      IF easyvar (gattr) THEN typseq := 9 ELSE typseq := 10
		    ELSE
		      IF gattr.kind = lval THEN typseq := 14 ELSE typseq := 9 ;
		  reaq : typseq := 1 ;
		END (* CASE FATTR.LDREG *) ;
	      END (* CASE FATTR.KIND *) ;
	  END (* TYPSEQ=0 *) ;
	IF odd (typseq) THEN
	  BEGIN
	    tattr := fattr ; cattr := gattr ;
	  END ELSE
	  BEGIN
	    tattr := gattr ; cattr := fattr ;
	  END ;
	IF declarationpart AND
	  NOT (typseq IN [0, 3, 4, 12]) THEN
	  BEGIN
	    illegal_generation := true ;
	    tattr.typtr := NIL ;
	  END
	ELSE
	  CASE typseq OF
	    0 :
$OPTIONS compile = security $
	      error (432)
$OPTIONS compile = true $
	      ;
	    1, 2 : BEGIN transfer (tattr, inacc) ;
	        calcvarient (cattr, lbase, ldisp, ltag) ;
	        WITH cattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, linst, ltag) ;
	      END (* 1,2 *) ;
	    3, 4 : IF generic = realptr THEN
	        IF tattr.typtr # realptr THEN convreal (tattr) ;
	    9, 10 : BEGIN transfer (tattr, inq) ;
	        calcvarient (cattr, lbase, ldisp, ltag) ;
	        WITH cattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, iadq, ltag) ;
	      END (* 9,10 *) ;
	    12 : IF generic = realptr THEN tattr.rsval := cattr.rsval + tattr.rsval ELSE
	        tattr.val := int_op (3, cattr.val, tattr.val) (* ADD *) ;
	    13, 14 : BEGIN genstand (pr6, evareaw, istq, tn) ; freeattr (cattr) ;
	        genstand (pr6, evareaw, iada, tn) ;
	      END (* 13,14 *) ;
	  END (* CASE TYPSEQ *) ;
	gattr := tattr ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENOPADD @@@ WITH TYPSEQ:', typseq : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENOPADD *) ;


$OPTIONS page $

(* ************************************ GENOPSUB ****************************** *)

      PROCEDURE genopsub (VAR fattr : attr ; generic : ctp) ;

(* C  . GENERATES   A  SUBSTRACTION   NOT COMMUTATIVE
   . FATTR   LEFT  OPERAND
   . GATTR   RIGHT OPERAND
   *  RETURNS   GATTR.
   C *)
(* E ERRORS DETECTED
   303 : VALUE  OUT  OF  RANGE
   433 : TYPSEQ   IS   ZERO.
   E *)
        VAR
	linst, linstq, lneg : istand ;
	lbase : preg ;
	typseq, ldisp : integer ;
	ltag : tag ;
	rev : boolean ;
        BEGIN                                     (* GENOPSUB *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENOPSUB @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
	typseq := 0 ;
	WITH gattr DO
	  IF kind = sval THEN
	    BEGIN
	      IF typtr = realptr THEN
	        BEGIN
		IF rsval = 0 THEN typseq := 3 ;
	        END ELSE
	        IF val = 0 THEN typseq := 3 ;
	    END ;
	IF typseq = 0 THEN
	  BEGIN
	    IF generic = realptr THEN
	      BEGIN
	        linst := idfsb ; lneg := ifneg ;
	        IF fattr.typtr # realptr THEN
		convreal (fattr) ELSE
		IF gattr.typtr # realptr THEN
		  convreal (gattr) ;
	      END ELSE
	      BEGIN
	        linst := isba ; lneg := ineg ; linstq := isbq ;
	        IF gattr.kind = sval THEN
		BEGIN
		  IF (gattr.val # - maxint - 1) AND (gattr.val < 0) THEN
		    BEGIN
		      gattr.val := -gattr.val ; linst := iada ; linstq := iadq ; rev := true ;
		    END ELSE rev := false ;
		END ;
	      END ;
	    IF fattr.kind = lval THEN
	      lvalvarbl (fattr) ;
	    WITH gattr DO
	      CASE fattr.kind OF
	        varbl : CASE kind OF
		  lval : typseq := 2 ;
		  varbl : IF easyvar (gattr) THEN typseq := 1 ELSE typseq := 2 ;
		  sval : typseq := 1 ;
		END (* GATTR.KIND  FOR  FATTR  VARBL *) ;
	        sval : IF generic # realptr THEN
		  BEGIN
		    IF fattr.val = 0 THEN
		      BEGIN
		        IF kind = sval THEN
			BEGIN
			  IF val = -maxint - 1 THEN error (303) ELSE typseq := 12 ;
			END ELSE
			IF kind = varbl THEN
			  BEGIN
			    IF easyvar (gattr) THEN typseq := 16 ELSE typseq := 30 ;
			  END ELSE
			  typseq := 30 ;
		      END (* FATTR.VAL = 0 *) ELSE
		      BEGIN
		        CASE kind OF
			varbl : IF easyvar (gattr) THEN typseq := 1 ELSE typseq := 2 ;
			lval : typseq := 2 ;
			sval : typseq := 12 ;
		        END (* CASE KIND *) ;
		      END (* FATTR.VAL # 0 *) ;
		  END (* GENERIC # REALPTR *) ELSE
		  BEGIN                       (* = REAL *)
		    IF fattr.rsval = 0 THEN
		      IF kind = sval THEN typseq := 12 ELSE typseq := 30
                                                  (* #0 *) ELSE
		      CASE kind OF
		        varbl : IF easyvar (gattr) THEN typseq := 1 ELSE typseq := 2 ;
		        lval : typseq := 2 ;
		        sval : BEGIN typseq := 1 ;
			  IF abs (rsval) < maxint THEN
			    IF abs (rsval) >= 1 THEN
			      IF abs (fattr.rsval) < maxint THEN typseq := 12 ;
			END ;
		      END (* CASE KIND *) ;
		  END (* GENERIC=REALPTR *) ;
                                                  (* END SVAL *)
	        lval : CASE fattr.ldreg OF
		  reaq : typseq := 1 ;
		  ra : CASE kind OF
		      varbl : IF easyvar (gattr) THEN typseq := 1 ELSE typseq := 2 ;
		      sval : typseq := 1 ;
		      lval : typseq := 13 ;
		    END ;
		  rq : CASE kind OF
		      varbl : IF easyvar (gattr) THEN typseq := 9 ELSE typseq := 32 ;
		      sval : typseq := 9 ;
		      lval : typseq := 15 ;
		    END ;
		END (* CASE  FATTR.LDREG *) ;
	      END (* CASE FATTR.KIND,WITH GATTR *) ;
	  END (* TYPSEQ= 0 *) ;
	IF declarationpart AND
	  NOT (typseq IN [0, 3, 4, 12]) THEN
	  BEGIN
	    illegal_generation := true ;
	    fattr.typtr := NIL ;
	  END
	ELSE
	  CASE typseq OF
	    0 :
$OPTIONS compile = security $
	      error (433)
$OPTIONS compile = true $
	      ;
	    1 : BEGIN transfer (fattr, inacc) ;
	        calcvarient (gattr, lbase, ldisp, ltag) ;
	        WITH gattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, linst, ltag) ;
	      END ;                             (* 1 *)
	    2 : BEGIN transfer (gattr, inacc) ;
	        calcvarient (fattr, lbase, ldisp, ltag) ;
	        WITH fattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, linst, ltag) ;
	        genstand (nreg, 0, lneg, tn) ;
	      END (* 2 *) ;
	    3 : IF generic = realptr THEN
	        IF fattr.typtr # realptr THEN convreal (fattr) ;
	    9 : BEGIN transfer (fattr, inq) ;
	        calcvarient (gattr, lbase, ldisp, ltag) ;
	        WITH gattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, linstq, ltag) ;
	      END (* 9 *) ;
	    12 :
	      IF generic = realptr THEN
	        gattr.rsval := fattr.rsval - gattr.rsval ELSE
	        IF rev THEN
		gattr.val := int_op (3, fattr.val, gattr.val) ELSE (* ADD *)
		gattr.val := int_op (4, fattr.val, gattr.val) ; (* SUB *)
	    13 : BEGIN genstand (pr6, evareaw, istq, tn) ; freeattr (gattr) ;
	        genstand (pr6, evareaw, isba, tn) ;
	      END ;
	    15 : BEGIN genstand (pr6, evareaw, ista, tn) ; freeattr (gattr) ;
	        genstand (pr6, evareaw, isbq, tn) ;
	      END (* 15 *) ;
	    16 : BEGIN calcvarient (gattr, lbase, ldisp, ltag) ;
	        WITH gattr DO
		BEGIN
		  IF NOT rqisused THEN
		    BEGIN ldreg := rq ; linst := ilcq ;
		    END ELSE
		    BEGIN ldreg := ra ; linst := ilca ;
		    END ;
		  sauvereg (ldreg, true) ;
		  ldregbloc := currentbloc ;
		END ;
	        WITH gattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, linst, ltag) ;
	        gattr.kind := lval ;
	      END (* 16 *) ;
	    30 : BEGIN transfer (gattr, inacc) ;
	        genstand (nreg, 0, lneg, tn) ;
	      END (* 30 *) ;
	    32 : BEGIN transfer (gattr, inacc) ;
	        sauvereg (rq, false) ;
	        calcvarient (fattr, lbase, ldisp, ltag) ;
	        WITH fattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, linst, ltag) ;
	        genstand (nreg, 0, lneg, tn) ;
	      END (* 32 *) ;
	  END (* CASE TYPSEQ *) ;
	IF odd (typseq) THEN
	  gattr := fattr ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENOPSUB @@@ WITH TYPSEQ', typseq : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENOPSUB *) ;


$OPTIONS page $

(* ************************************ GENANDOR ****************************** *)

      PROCEDURE genandor (VAR fattr : attr ; fno : integer) ;

(* C  .CODE GENERATION  FOR   OPERATIONS "AND"  FNO=6   ON BOOLEAN.
   "OR"   FNO=7
   .FATTR   DESCRIBES  LEFT OPERAND
   .GATTR   DESCRIBES  RIGHT OPERAND
   * RETURNS   A GATTR.
   C *)
        VAR
	typseq, ldisp : integer ;
	cattr, tattr : attr ;
	lbase : preg ;
	ltag : tag ;
	isand : boolean ;
	insta, instq : istand ;
        BEGIN                                     (* GENANDOR *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENANDOR @@@ WITH FNO', fno : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
	isand := fno = 6 ;
	IF isand THEN
	  BEGIN
	    insta := iana ; instq := ianq ;
	  END ELSE
	  BEGIN                                 (* OR *)
	    insta := iora ; instq := iorq ;
	  END ;
	IF gattr.kind = lcond THEN choicerarq ;
	IF fattr.kind = lval THEN lvalvarbl (fattr) ;
	WITH gattr DO
	  IF kind = sval THEN
	    BEGIN
	      IF val = ord (false) THEN
	        typseq := 3 + ord (isand)
	      ELSE
	        typseq := 4 - ord (isand)
	    END ELSE
	    IF fattr.kind = sval THEN
	      BEGIN
	        IF fattr.val = ord (false) THEN
		typseq := 4 - ord (isand) ELSE
		typseq := 3 + ord (isand) ;
	      END ELSE
	      IF kind = varbl THEN
	        BEGIN
		IF easyvar (gattr) THEN
		  BEGIN
		    typseq := 1 ;
		    IF fattr.kind = lval THEN
		      IF fattr.ldreg # ra THEN
		        typseq := 9 ;
		  END ELSE
		  BEGIN
		    typseq := 2 ;
		    IF fattr.kind = lval THEN
		      IF fattr.ldreg # ra THEN
		        typseq := 10 ;
		  END (* NOT EASYVAR *) ;
	        END (* GATTR.KIND=VARBL *) ELSE
                                                  (* GATTR LVAL *)
	        IF ldreg = ra THEN
		IF fattr.kind = varbl THEN typseq := 2 ELSE typseq := 14 (* END RA *) ELSE
                                                  (* RQ *)
		IF fattr.kind = varbl THEN typseq := 10 ELSE typseq := 13 ;
	IF odd (typseq) THEN
	  BEGIN
	    tattr := fattr ; cattr := gattr ;
	  END ELSE
	  BEGIN
	    tattr := gattr ; cattr := fattr ;
	  END ;
	CASE typseq OF
	  1, 2 : BEGIN transfer (tattr, inacc) ;
	      calcvarient (cattr, lbase, ldisp, ltag) ;
	      WITH cattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, insta, ltag) ;
	    END (* 1,2 *) ;
	  3, 4 : BEGIN freeattr (cattr) ;
	    END (* 3,4 *) ;
	  9, 10 : BEGIN transfer (tattr, inq) ;
	      calcvarient (cattr, lbase, ldisp, ltag) ;
	      WITH cattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, instq, ltag) ;
	    END (* 9,10 *) ;
	  13, 14 : BEGIN genstand (pr6, evareaw, istq, tn) ; freeattr (cattr) ;
	      genstand (pr6, evareaw, insta, tn) ;
	    END (* 13,14 *) ;
	END (* CASE TYPSEQ *) ;
	gattr := tattr ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENANDOR @@@ WITH TYPSEQ:', typseq : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENANDOR *) ;


$OPTIONS page $

(* ************************************ GENOPDIVI ***************************** *)

      PROCEDURE genopdivi (VAR fattr : attr) ;

(* C   BEFORE  CALL,   FATTR , GATTR  ARE    REAL
   DIVCHECKS ALREADY  MADE
   AT OUTPUT   BUILDS  GATTR., GENERATES DIVISION
   FATTR  CAN BE
   ESAY 8,  RSVAL  ,  EAQ
   GATTR  CAN  BE
   EASY 8,   NOT EASY 8 , RSVAL,  EAQ
   C *)
(* E ERRORS DETECTED
   300: ZERO DIVIDE CAN BE NOT SUITABLE
   E *)
        VAR
	typseq, ldisp : integer ;
	lbase : preg ;
	ltag : tag ;
        BEGIN                                     (* GENOPDIVI *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENOPDIVI @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
	IF gattr.kind = sval THEN
	  BEGIN
	    IF gattr.rsval = 0 THEN
	      typseq := 0 ELSE
	      IF gattr.rsval = 1.0 THEN
	        typseq := 3 ELSE
	        BEGIN
		IF fattr.kind = sval THEN
		  BEGIN
		    IF fattr.rsval = 0.0 THEN
		      typseq := 3 ELSE
		      BEGIN
		        typseq := 1 ;
		        IF abs (gattr.rsval) >= 1 THEN
			IF abs (gattr.rsval) < maxint THEN IF abs (fattr.rsval) >= 1 THEN
			    IF abs (fattr.rsval) < maxint THEN typseq := 12 ;
		      END ;
		  END (* FATTR.SVAL *) ELSE
		  typseq := 1 ;
	        END ;
	  END (* GATTR SVAL *) ELSE
	  BEGIN
	    IF fattr.kind = lval THEN
	      lvalvarbl (fattr) ;
	    CASE fattr.kind OF
	      varbl : typseq := 2 ;
	      lval : typseq := 1 ;
	      sval : IF fattr.rsval = 0.0 THEN
		typseq := 3 ELSE
		typseq := 2 ;
	    END (* CASE *) ;
	  END (* GATTR ^=SVAL *) ;
	CASE typseq OF
	  0 : error (300) ;
	  1 : BEGIN
	      transfer (fattr, inacc) ;
	      calcvarient (gattr, lbase, ldisp, ltag) ;
	      WITH gattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, idfdv, ltag) ;
	      gattr := fattr ;
	    END ;
	  2 : BEGIN
	      transfer (gattr, inacc) ;
	      calcvarient (fattr, lbase, ldisp, ltag) ;
	      WITH fattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, idfdi, ltag) ;
                                                  (* GATTR UNCHANGED *)
	    END ;
	  3 : BEGIN freeattr (gattr) ;
	      gattr := fattr ;
	    END ;
	  12 : gattr.rsval := fattr.rsval / gattr.rsval ;
	END (* CASE TYPSEQ *) ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENOPDIVI @@@ WITH TYPSEQ', typseq : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENOPDIVI *) ;


$OPTIONS page $

(* ************************************ GENDIVMOD ***************************** *)

      PROCEDURE gendivmod (VAR fattr : attr ; fcl : integer) ;

(* C .CODE GENERATION  FOR   DIV, MOD  ON NUMERIC OPERANDS
   FCL=4  ==> DIV
   FCL=5  ==> MOD
   .FATTR IS LEFT OPERAND,  GATTR  RIGHT OPERAND
   .SPECIAL CASES    SVAL  0,1,2**N
   .   [Q] OPERAND   DIV   Y OPERAND  ==>   QUOTIENT IN [Q]
   REMAINDER IN [A]
   . RETURNS  GATTR
   C *)
(* E ERRORS DETECTED
   308 : RIGHT ARGUMENT OF DIV IS NULL
   309 : RIGHT ARGUMENT OF MOD IS NEGATIVE OR NULL
   E *)
        VAR
	locskip, typseq, ldisp : integer ;
	ismod : boolean ;
	lbase : preg ;
	ltag : tag ;
        BEGIN                                     (* GENDIVMOD *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENDIVMOD @@@ WITH FCL', fcl : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
	ismod := fcl = 5 ;
	IF fattr.kind = lval THEN
	  lvalvarbl (fattr) ;
	WITH gattr DO
	  CASE fattr.kind OF
	    varbl : CASE kind OF
	        varbl : IF easyvar (gattr) THEN typseq := 25 ELSE typseq := 32 ;
	        lval : typseq := 25 ;
	        sval : IF ismod THEN
		  IF val <= 0 THEN typseq := 1
		  ELSE typseq := 25
		ELSE IF val = 0 THEN typseq := 0
		  ELSE IF val = 1 THEN typseq := 3 ELSE typseq := 25 ;
	      END ;
	    sval : IF kind = sval THEN
	        IF ismod THEN
		IF val <= 0 THEN typseq := 1
		ELSE IF fattr.val = 0 THEN typseq := 3 ELSE typseq := 12
	        ELSE IF val = 0 THEN typseq := 0
		ELSE IF fattr.val = 0 THEN typseq := 3 ELSE typseq := 12
	      ELSE IF fattr.val = 0 THEN typseq := 3
	        ELSE IF kind = varbl THEN
		  IF easyvar (gattr) THEN typseq := 25 ELSE typseq := 32
		ELSE typseq := 25 ;
	    lval : CASE kind OF
	        varbl : IF easyvar (gattr) THEN typseq := 25
		ELSE IF fattr.ldreg = ra THEN typseq := 27 ELSE typseq := 32 ;
	        sval : IF ismod THEN
		  IF val <= 0 THEN typseq := 1 ELSE typseq := 25
		ELSE IF val = 0 THEN typseq := 0
		  ELSE IF val = 1 THEN typseq := 3 ELSE typseq := 25 ;
	        lval : IF ldreg = rq THEN typseq := 27 ELSE typseq := 25 ;
	      END ;
	  END ;
	CASE typseq OF
	  0 : error (308) ;
	  1 : error (309) ;
	  3 : freeattr (gattr) ;
	  12 : IF ismod THEN
	      fattr.val := fattr.val MOD gattr.val ELSE
	      fattr.val := fattr.val DIV gattr.val ;
	  25 : BEGIN
                                                  (* Temporary correction of a bug 25= Sequence 32   *)
                                                  (* A ameliorer plus tard                           *)
	      transfer (gattr, inacc) ;
	      sauvereg (ra, false) ;
	      IF fattr.kind = lval THEN lvalvarbl (fattr) ;
	      transfer (fattr, inq) ;
	      calcvarient (gattr, lbase, ldisp, ltag) ;
	      WITH gattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, idiv, ltag) ;
	      IF ismod THEN
	        BEGIN
		genstand (nreg, bitsinword, ilrs, tn) ;
		locskip := indfich ;
		genstand (nreg, 0, itpl, tic) ;
		calcvarient (gattr, lbase, ldisp, ltag) ;
		WITH gattr DO
		  IF kind = varbl THEN usednameaddr := nameaddr ;
		genstand (lbase, ldisp, iadq, ltag) ;
		inser (cb, locskip) ;
	        END ;
	    END (* 25 *) ;
	  27 : BEGIN transfer (gattr, inq) ; transfer (fattr, inq) ;
	      calcvarient (gattr, lbase, ldisp, ltag) ;
	      WITH gattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, idiv, ltag) ;
	      IF ismod THEN
	        BEGIN
		genstand (nreg, bitsinword, ilrs, tn) ;
		locskip := indfich ;
		genstand (nreg, 0, itpl, tic) ;
		calcvarient (gattr, lbase, ldisp, ltag) ; (* NOT NECESSARY *)
		WITH gattr DO
		  IF kind = varbl THEN usednameaddr := nameaddr ;
		genstand (lbase, ldisp, iadq, ltag) ;
		inser (cb, locskip) ;
	        END ;
	    END (* 27 *) ;
	  32 : BEGIN transfer (gattr, inacc) ; sauvereg (ra, false) ; transfer (fattr, inq) ;
	      calcvarient (gattr, lbase, ldisp, ltag) ;
	      WITH gattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, idiv, ltag) ;
	      IF ismod THEN
	        BEGIN
		genstand (nreg, bitsinword, ilrs, tn) ;
		locskip := indfich ;
		genstand (nreg, 0, itpl, tic) ;
		calcvarient (gattr, lbase, ldisp, ltag) ; (* NOT NECESSARY *)
		WITH gattr DO
		  IF kind = varbl THEN usednameaddr := nameaddr ;
		genstand (lbase, ldisp, iadq, ltag) ;
		inser (cb, locskip) ;
	        END ;
	    END (* 32 *) ;
	END (* CASE TYPSEQ *) ;
	gattr := fattr ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENDIVMOD @@@ WITH TYPSEQ', typseq : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENDIVMOD *) ;


$OPTIONS page $

(* ************************************ GENOPMULT ***************************** *)

      PROCEDURE genopmult (VAR fattr : attr ; generic : ctp) ;

(* C *CODE GENERATION FOR A MULTIPLICATION
   .SPECIAL CASES   SVAL  0,1  , 2**N
   FATTR  IS LEFT  OPERAND
   GATTR  IS RIGHT  OPERAND
   *RETURNS  GATTR
   C *)
(* E ERRORS DETECTED
   419: TYPSEQ  IS  0
   E *)
        VAR
	typseq, itl, itg, ldisp, nbshif : integer ;
	ltag : tag ;
	lbase : preg ;
	isreal : boolean ;
	linst : istand ;
	tattr, cattr : attr ;
        BEGIN                                     (* GENOPMULT *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENOPMULT @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
	typseq := 0 ; itg := 0 ; itl := 0 ;
	IF fattr.kind = sval THEN
	  WITH fattr DO
	    BEGIN
	      IF typtr = realptr THEN
	        BEGIN
		IF rsval = 0 THEN typseq := 3 ELSE
		  IF rsval = 1 THEN typseq := 4
	        END ELSE
	        IF val = 0 THEN typseq := 3 ELSE
		IF val = 1 THEN typseq := 4 ELSE
		  itl := poweroftwo (val) ;
	    END (* WITH FATTR, FATTR.KIND=SVAL *) ELSE
	  IF gattr.kind = sval THEN
	    WITH gattr DO
	      BEGIN
	        IF typtr = realptr THEN
		BEGIN
		  IF rsval = 0 THEN typseq := 4 ELSE
		    IF rsval = 1 THEN typseq := 3
		END ELSE
		IF val = 0 THEN typseq := 4 ELSE
		  IF val = 1 THEN typseq := 3 ELSE
		    itg := poweroftwo (val) ;
	      END (* WITH GATTR, GATTR.KIND=SVAL *) ;
	IF typseq = 0 THEN
	  BEGIN
	    IF generic = realptr THEN
	      BEGIN
	        IF fattr.typtr # realptr THEN
		convreal (fattr) ELSE
		IF gattr.typtr # realptr THEN
		  convreal (gattr) ;
	        linst := idfmp ;
	      END (* REALPTR *) ELSE
	      linst := impy ;
	    IF fattr.kind = lval THEN
	      lvalvarbl (fattr) ;
	    isreal := generic = realptr ;
	    WITH gattr DO
	      CASE fattr.kind OF
	        varbl : IF NOT isreal THEN
		  BEGIN
		    IF itg > 0 THEN
		      IF NOT rqisused THEN typseq := 33 ELSE typseq := 29
		    ELSE
		      typseq := 36
		  END (* NOT REAL *) ELSE
		  typseq := 2 ;
	        sval : IF isreal THEN
		  BEGIN
		    typseq := 2 ;
		    IF kind = sval THEN
		      IF abs (rsval) >= 1 THEN
		        IF abs (rsval) < maxint THEN
			IF abs (fattr.rsval) < maxint THEN typseq := 12 ;
		  END (* ISREAL *) ELSE
		  BEGIN                       (* NOT REAL *)
		    IF kind = sval THEN
		      BEGIN
		        typseq := 12 ;
		      END (* GATTR SVAL *) ELSE
		      IF itl > 0 THEN
		        BEGIN
			IF kind = varbl THEN
			  IF NOT rqisused THEN typseq := 34 ELSE typseq := 30
			ELSE
			  IF ldreg = ra THEN typseq := 30 ELSE typseq := 34
		        END (* ITL > 0 *) ELSE
		        typseq := 36 ;
		  END (* NOT REAL, FATTR.KIND=SVAL *) ;
	        lval : IF isreal THEN typseq := 1 ELSE
		  CASE kind OF
		    varbl : IF easyvar (gattr) THEN typseq := 35 ELSE typseq := 36 ;
		    sval : IF itg > 0 THEN
		        IF fattr.ldreg = ra THEN typseq := 29 ELSE typseq := 33
		      ELSE
		        typseq := 35 ;
		    lval : IF ldreg = rq THEN typseq := 35 ELSE typseq := 36 ;
		  END (* CASE KIND, NOT ISREAL,  FATTR LVAL *) ;
	      END (* CASE FATTR.KIND,WITH GATTR *) ;
	  END (* TYPSEQ IS 0 *) ;
	IF odd (typseq) THEN
	  BEGIN
	    tattr := fattr ; cattr := gattr ; nbshif := itg ;
	  END ELSE
	  BEGIN
	    tattr := gattr ; cattr := fattr ; nbshif := itl ;
	  END ;
	IF declarationpart AND
	  NOT (typseq IN [0, 3, 4, 12]) THEN
	  BEGIN
	    illegal_generation := true ;
	    tattr.typtr := NIL ;
	  END
	ELSE
	  CASE typseq OF
	    0 :
$OPTIONS compile = trace $
	      error (419)
$OPTIONS compile = true $
	      ;
	    1, 2 : BEGIN transfer (tattr, inacc) ;
	        calcvarient (cattr, lbase, ldisp, ltag) ;
	        WITH cattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, linst, ltag) ;
	        IF linst = impy THEN
		IF asscheck THEN gencheckmultover ;
	      END (* 1,2 *) ;
	    3, 4 : BEGIN freeattr (cattr) ;
	        IF generic = realptr THEN
		IF tattr.typtr # realptr THEN convreal (tattr) ;
	      END (* 3,4 *) ;
	    12 : IF generic = realptr THEN
	        tattr.rsval := cattr.rsval * tattr.rsval ELSE
	        tattr.val := int_op (1, cattr.val, tattr.val) ;
	    29, 30 : BEGIN transfer (tattr, inacc) ;
	        genstand (nreg, nbshif, ials, tn) ;
	      END (* 29,30 *) ;
	    33, 34 : BEGIN transfer (tattr, inq) ;
	        genstand (nreg, nbshif, iqls, tn) ;
	      END (* 33,34 *) ;
	    35, 36 : BEGIN transfer (tattr, inq) ;
	        sauvereg (ra, false) ;
	        calcvarient (cattr, lbase, ldisp, ltag) ;
	        WITH cattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        genstand (lbase, ldisp, linst, ltag) ;
	        IF linst = impy THEN
		IF asscheck THEN gencheckmultover ;
	      END (* 35,36 *) ;
	  END (* CASE  TYPSEQ *) ;
	gattr := tattr ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENOPMULT @@@ WITH TYPSEQ :', typseq : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENOPMULT *) ;


$OPTIONS page $

(* ************************************ GENPTCOMP ***************************** *)

      PROCEDURE genptcomp (VAR fattr : attr ; fcl : integer) ;

(* C . FATTR  LEFT OPERAND
   GATTR  RIGHT OPERAND
   ."NIL"  IS SVAL CF. CALCVARIENT
   .PRODUCES A GATTR LCOND.
   C *)
        VAR
	typseq : integer ;
	lretpt : lcstpt ;
        BEGIN                                     (* GENPTCOMP *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENPTCOMP @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
	typseq := 2 ;
	IF fattr.typtr = nilptr THEN
	  BEGIN transfer (gattr, inacc) ; typseq := 4 ;
	  END ELSE
	  IF gattr.typtr = nilptr THEN
	    BEGIN transfer (fattr, inacc) ; typseq := 3 ;
	    END ;
	IF typseq <= 2 THEN
	  BEGIN

	    transfer (gattr, inacc) ;
	    enterlcst (clearpt, lretpt) ;
	    enterundlab (lretpt^.lplace) ;
	    genstand (nreg, 0, ianaq, tic) ;
	    genstand (pr6, evareaw, istaq, tn) ;
	    freebloc (gattr.ldregbloc) ;

	    IF fattr.kind = lval THEN
	      lvalvarbl (fattr) ;
	    transfer (fattr, inacc) ;
	    enterlcst (clearpt, lretpt) ;
	    enterundlab (lretpt^.lplace) ;
	    genstand (nreg, 0, ianaq, tic) ;
	    genstand (pr6, evareaw, icmpaq, tn) ;
	    typseq := 1 ;                       (* REVERSE COMPARAISON *)
	  END ELSE
	  BEGIN enterlcst (nileraq, lretpt) ; enterundlab (lretpt@.lplace) ;
	    genstand (nreg, 0, ieraq, tic) ;
	    enterlcst (nilanaq, lretpt) ; enterundlab (lretpt@.lplace) ;
	    genstand (nreg, 0, ianaq, tic) ;
	  END ;
	freeattr (gattr) ; freeattr (fattr) ;
	WITH gattr DO
	  BEGIN
	    kind := lcond ; accbool := false ; accbloc := NIL ;
	    IF odd (typseq) THEN
	      transf := cltransf [fcl] ELSE
	      transf := revcltransf [fcl] ;
                                                  (* TYPTR OUTSIDE *)
	  END ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENPTCOMP @@@ WITH  FCL,TRANSF :', fcl : 4, gattr.transf) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENPTCOMP *) ;


$OPTIONS page $

(* ************************************ GENSTCOMP ***************************** *)

      PROCEDURE genstcomp (VAR fattr : attr ; fcl : integer) ;

(* C   . FATTR  IS  LEFT  OPERAND
   GATTR  IS  RIGHT  OPERAND
   . OUTPUT  PROCEDURE IS A GATTR LCOND
   C *)
(* E ERRORS DETECTED
   29 : SAME LENGTH STRINGS EXPECTED HERE
   131 :   LENGTH TOO LARGE(CONFLICT)
   307 :   LENGTH TOO LARGE ( LIMIT  IMPLEMENTATION)
   E *)
        VAR
	lfbase, rgbase : preg ;
	lfchain, rgchain : boolean ;
	lflong, rglong, lfdisp, rgdisp, lfmod, rgmod, suplr, ltransf : integer ;
	lftag, rgtag : tag ;
        BEGIN                                     (* GENSTCOMP *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENSTCOMP @@@ WITH FCL', fcl : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
	WITH fattr DO                           (* LEFT OPER. *)
	  BEGIN
	    IF kind = chain THEN
	      BEGIN
	        loadadr (fattr, nreg) ;
	        lfbase := currentpr ; lflong := alfactp@.alfalong ; lfchain := true ;
	        lfdisp := 0 ; lfmod := 0 ;
	        WITH fattr DO
		BEGIN
		  kind := varbl ; access := pointee ;
		  basebloc := currentbloc ; basereg := currentpr ;
		  inxbloc := NIL ; inxmem := 0 ; dplmt := 0 ; inxmemrw := false ; pckd := true ;
		  vlev := level ;
		END ;
	      END (* CHAIN *) ELSE
	      BEGIN                             (* VARBL *)
	        lfchain := false ;
	        IF basereg <= maxprused THEN
		regenere (basebloc) ;
	        lfbase := basereg ; lfdisp := dplmt DIV bytesinword ;
	        lflong := typtr@.size ; lfmod := dplmt MOD bytesinword ;
	      END (* VARBL *) ;
	  END (* WITH FATTR *) ;
	WITH gattr DO                           (* RIGHT  OPERAND *)
	  BEGIN
	    IF kind = chain THEN
	      BEGIN
	        loadadr (gattr, pr3) ;
	        rgbase := pr3 ; rglong := alfactp@.alfalong ; rgchain := true ; rgdisp := 0 ;
	        rgmod := 0 ;
	      END (* CHAIN *) ELSE
	      BEGIN                             (* VARBL *)
	        rgchain := false ; rglong := typtr@.size ;
	        IF NOT varissimple (gattr) THEN
		BEGIN
		  loadadr (gattr, pr3) ;
		  rgbase := pr3 ; rgmod := 0 ; rgdisp := 0 ;
		END ELSE
		BEGIN
		  rgbase := basereg ; rgdisp := dplmt DIV bytesinword ;
		  rgmod := dplmt MOD bytesinword ;
		END ;
	      END (* VARBL *) ;
	  END (* WITH GATTR *) ;
	IF lfchain THEN
	  BEGIN
	    IF lflong > rglong THEN error (131) ;
	  END ELSE
	  IF rgchain THEN
	    BEGIN
	      IF lflong < rglong THEN error (131) ;
	    END ;
	suplr := sup (lflong, rglong) ;
	IF envstandard <> stdextend THEN
	  IF lflong # rglong THEN error (29) ;
	IF suplr < twoto12 THEN
	  BEGIN
	    mfari1 := a1r0i0 ; mfari2 := a1r0i0 ; lftag := tn ; rgtag := tn ;
	  END ELSE
	  BEGIN
	    mfari1 := a1r1i0 ; mfari2 := a1r1i0 ; lftag := tx6 ; rgtag := tx7 ;
	    IF suplr > twoto17m1 THEN
	      error (307) ELSE
	      BEGIN
	        genstand (nreg, lflong, ieax6, tn) ;
	        genstand (nreg, rglong, ieax7, tn) ;
	      END ;
	    lflong := 0 ; rglong := 0 ;
	  END ;
	geneism (icmpc, ord (' '), p0t0r0) ;
	IF fcl IN [2, 4] THEN
	  BEGIN                                 (* <=  > *)
	    WITH gattr DO
	      IF kind = varbl THEN usednameaddr := nameaddr ELSE
	        IF kind = chain THEN usednameaddr := alfactp ;
	    gendesca (rgbase, rgdisp, rgmod, l9, rglong, rgtag) ;
	    WITH fattr DO
	      IF kind = varbl THEN usednameaddr := nameaddr ELSE
	        IF kind = chain THEN usednameaddr := alfactp ;
	    gendesca (lfbase, lfdisp, lfmod, l9, lflong, lftag) ;
	    IF fcl = 2 THEN
	      fcl := 3 (* >= *) ELSE fcl := 1 ; (* < *)
	  END ELSE
	  BEGIN
	    WITH fattr DO
	      IF kind = varbl THEN usednameaddr := nameaddr ELSE
	        IF kind = chain THEN usednameaddr := alfactp ;
	    gendesca (lfbase, lfdisp, lfmod, l9, lflong, lftag) ;
	    WITH gattr DO
	      IF kind = varbl THEN usednameaddr := nameaddr ELSE
	        IF kind = chain THEN usednameaddr := alfactp ;
	    gendesca (rgbase, rgdisp, rgmod, l9, rglong, rgtag) ;
	  END ;
	CASE fcl OF
	  1 : ltransf := 11 ;                   (*  CARRY OFF   TRUE *)
	  3 : ltransf := 12 ;                   (*  CARRY ON    TRUE *)
	  5 : ltransf := 6 ;                    (*  ZERO  OFF   TRUE *)
	  6 : ltransf := 2 ;                    (*  ZERO  ON    TRUE *)
	END (* CASE FCL *) ;
	freeattr (fattr) ;
	freeattr (gattr) ;
	WITH gattr DO                           (* TYPTR OUTSIDE *)
	  BEGIN
	    kind := lcond ; accbool := false ; accbloc := NIL ;
	    transf := ltransf ;
	  END ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENSTCOMP @@@ WITH TRANSF', ltransf) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENSTCOMP *) ;


$OPTIONS page $

(* ************************************ GENJUMP ******************************* *)

      PROCEDURE genjump (VAR inserplace : integer ; jumpdisp : integer) ;

(* C .CALLED IN ORDER TO GENERATE  THE  JUMP IF FALSE  FOR
   REPEAT     NO  INSER,  JUMPDISP   KNOWN  BACKWARDS
   WHILE,IF   INSER    ,  JUMPDISP=0
   .BEFORE CALL   GATTR  IS TESTED AND HAS  TYPTR=BOOLPTR
   C *)
        VAR
	linst : istand ;
	locinser : integer ;
        BEGIN                                     (* GENJUMP *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENJUMP @@@ WITH JUMPDISP', jumpdisp) ; nextline ;
	  END ;
$OPTIONS compile = true $
	WITH gattr DO
	  IF kind = lcond THEN
	    BEGIN
	      CASE transf OF
	        1, 7 : linst := itpl ;          (* JUMP IF NEGATIVE  OFF *)
	        2, 13, 15 : linst := itnz ;     (* JUMP IF ZERO  OFF *)
	        3, 6, 14 : linst := itze ;      (* JUMP IF ZERO  ON *)
	        4 : linst := inop ;             (* NO JUMP *)
	        5 : linst := itra ;             (* INCONDITIONAL  JUMP *)
	        8 : linst := itpnz ;            (* JUMP IF NEGATIVE OFF AND  ZERO OFF *)
	        9 : linst := itmi ;             (* JUMP IF NEGATIVE ON *)
	        10 : linst := itmoz ;           (* JUMP IF NEGATIVE ON OR   ZERO ON *)
	        11 : linst := itrc ;            (* JUMP IF CARRY ON *)
	        12 : linst := itnc ;            (* JUMP IF CARRY OFF *)
	      END (* CASE TRANSF *) ;
	      IF accbloc # NIL THEN freebloc (accbloc) ;
	      locinser := indfich ;
	    END (* LCOND *) ELSE
	    IF kind = sval THEN
	      BEGIN
	        IF val = ord (false) THEN
		linst := itra ELSE linst := inop ;
	        locinser := indfich ;
	      END (* SVAL *) ELSE
	      BEGIN
	        transfer (gattr, inacc) ;       (* SET INDICATORS *)
	        locinser := indfich ; linst := itze ; (* SKIP IF ZERO OFF =FALSE *)
	        freebloc (ldregbloc) ;
	      END (* NEITHER LCOND, NOR SVAL *) ;
	IF linst # inop THEN
	  BEGIN
	    IF jumpdisp # 0 (*     REPEAT  BACKWARDS *) THEN
	      BEGIN
	        genstand (nreg, (jumpdisp - cb) DIV bytesinword, linst, tic) ;
	      END ELSE
	      BEGIN                             (* WHILE,IF ==> FORWARDS *)
	        inserplace := locinser ;
	        genstand (nreg, 0, linst, tic) ;
	      END ;
	  END ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENJUMP @@@ WITH V.INSERPLACE', inserplace) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENJUMP *) ;


$OPTIONS page $

(* ************************************ GENCOMPARE **************************** *)

      PROCEDURE gencompare (VAR fattr : attr ; fcl : integer ; generic : ctp) ;

(* C . GATTR IS RIGHT OPERAND
   FATTR IS LEFT  OPERAND
   .  GENERIC  TYPE
   . AT OUTPUT  PRODUCES A GATTR   LCOND
   WITH  TRANSF  FUNCTION  OF INDICATORS SET
   C *)
(* E  ERROR DETECTED
   434 TYPSEQ = 0
   E *)
        VAR
	tattr, cattr : attr ;
	lbase : preg ;
	ldisp, typseq : integer ;
	ltag : tag ;
	linst : istand ;
        BEGIN                                     (* GENCOMPARE *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENCOMPARE @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
	IF generic = realptr THEN
	  BEGIN linst := idfcmp ;
	    IF gattr.typtr # realptr THEN
	      convreal (gattr) ELSE
	      IF fattr.typtr # realptr THEN
	        convreal (fattr) ;
	  END ELSE
	  linst := icmpa ;
	IF gattr.kind = lcond THEN choicerarq ;
	IF fattr.kind = lval THEN
	  lvalvarbl (fattr) ;
	typseq := 0 ;
	WITH gattr DO
	  CASE fattr.kind OF
	    varbl : CASE kind OF
	        varbl : typseq := 2 ;
	        lval : IF ldreg = rq THEN typseq := 10 ELSE typseq := 2 ;
	        sval : IF generic = realptr THEN
		  BEGIN IF rsval = 0 THEN typseq := 29 ELSE typseq := 2 ;
		  END ELSE
		  BEGIN
		    IF val = 0 THEN typseq := 17 ELSE typseq := 2 ;
		  END ;
	      END (* CASE GATTR.KIND *) ;
	    sval : IF generic = realptr THEN
	        BEGIN
		IF fattr.rsval = 0 THEN typseq := 30 ELSE typseq := 2 ;
	        END ELSE
	        BEGIN
		IF fattr.val # 0 THEN
		  BEGIN typseq := 2 ;
		    IF kind = lval THEN
		      IF ldreg = rq THEN typseq := 10 ;
		  END ELSE
		  BEGIN typseq := 30 ;
		    IF kind = varbl THEN
		      BEGIN
		        IF easyvar (gattr) THEN typseq := 18 ;
		      END ELSE
		      IF kind = lval THEN
		        IF ldreg = rq THEN typseq := 34 ;
		  END ;
	        END (* NOT REAL *) ;
	    lval : CASE fattr.ldreg OF
	        ra :
		CASE kind OF
		  varbl : IF easyvar (gattr) THEN typseq := 1 ELSE typseq := 2 ;
		  sval : IF val = 0 THEN typseq := 29 ELSE typseq := 1 ;
		  lval : typseq := 13 ;
		END (* CASE GATTR.KIND FOR RA *) ;
	        rq :
		CASE kind OF
		  varbl : IF easyvar (gattr) THEN typseq := 9 ELSE typseq := 10 ;
		  sval : IF val = 0 THEN typseq := 33 ELSE typseq := 9 ;
		  lval : typseq := 14 ;
		END ;                         (* CASE GATTR.KIND FOR RQ *)
	        reaq : BEGIN typseq := 1 ;
		  IF kind = sval THEN
		    IF rsval = 0.0 THEN typseq := 29 ;
		END ;
	      END (* CASE FATTR.LDREG *) ;
	  END (* CASE FATTR.KIND, WITH GATTR *) ;
	IF odd (typseq) THEN
	  BEGIN
	    tattr := fattr ; cattr := gattr ;
	  END ELSE
	  BEGIN
	    tattr := gattr ; cattr := fattr ;
	  END ;
	CASE typseq OF
	  0 :
$OPTIONS compile = trace $
	    error (434)
$OPTIONS compile = true $
	    ;
	  1, 2 : BEGIN transfer (tattr, inacc) ;
	      calcvarient (cattr, lbase, ldisp, ltag) ;
	      WITH cattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, linst, ltag) ;
	    END (* 1,2 *) ;
	  9, 10 : BEGIN transfer (tattr, inq) ;
	      calcvarient (cattr, lbase, ldisp, ltag) ;
	      WITH cattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, icmpq, ltag) ;
	    END (* 9,10 *) ;
	  13, 14 : BEGIN genstand (pr6, evareaw, istq, tn) ; freeattr (cattr) ;
	      genstand (pr6, evareaw, icmpa, tn) ;
	    END (* 13,14 *) ;
	  17, 18 : BEGIN calcvarient (tattr, lbase, ldisp, ltag) ;
	      WITH tattr DO
	        IF kind = varbl THEN usednameaddr := nameaddr ;
	      genstand (lbase, ldisp, iszn, ltag) ;
	    END (* 17,18 *) ;
	  29, 30 : transfer (tattr, inacc) ;
	  33, 34 : transfer (tattr, inq) ;
	END (* CASE TYPSEQ *) ;
	freeattr (tattr) ; freeattr (cattr) ;
	WITH gattr DO
	  BEGIN
                                                  (* TYPTR  OUTSIDE *)
	    kind := lcond ; accbloc := NIL ; accbool := false ;
	    IF odd (typseq) THEN
	      transf := cltransf [fcl] ELSE
	      transf := revcltransf [fcl] ;
	  END (* WITH GATTR *) ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENCOMPARE @@@ WITH TYPSEQ,TRANSF', typseq : 4,
	      gattr.transf) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENCOMPARE *) ;


$OPTIONS page $

(* ****************************************************  GENOPPW ************** *)

      PROCEDURE genoppw (VAR fattr : attr ; fno, fcl : integer) ;

(* C GENERATES CODE  FOR  SETS  OPERATION
   .GATTR IS  RIGHT OPERAND
   .FATTR IS  LEFT  OPERAND
   .FNO= 6    FCL= 1      SET  INTERSECTION
   .FNO= 7    FCL= 1      SET UNION
   FCL= 2      SET DIFFERENCE   (NOT COMMUTATIV)
   .FNO= 8    FCL= 2  ( <=)    SET INCLUSION
   FCL= 3  ( >=)
   FCL= 5  ( # )
   FCL= 6  ( = )
   .RETURNS A GATTR
   .BEFORE CALL    FATTR  CAN BE  .LVAL   IN  AQ  *GATTR CAN BE  .LVAL AQ
   .LVAL   IN  PSR *              .LVAL PSR
   .SVAL   8       *              .SVAL  8
   .VARBL  EASY 8  *              .SVAL  MAX
   .VARBL EASY MAX *              .VAR EASY 8
   .SVAL   MAX     *              .VAR EASY MAX
   .VAR NOT EASY
   C *)
(* E ERRORS DETECTED
   E *)
        VAR
	typseq : integer ;
	bolr, revbolr : integer ;
	linstaq : istand ;
	lbase : preg ;
	ldisp, fattsize, gattsize, tattsize, cattsize, ltransf : integer ;
	ltag : tag ;
	lretpt : lcstpt ;
	llretpt : llcstpt ;
	tattr, cattr : attr ;
	rshort, lshort, classe1 : boolean ;
        BEGIN                                     (* GENOPPW *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT GENOPPW @@@ WITH FNO,FCL:', fno : 4, fcl : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
	typseq := 0 ;
	fattsize := fattr.typtr^.setlength ;
	gattsize := gattr.typtr^.setlength ;
	WITH fattr DO
	  CASE kind OF
	    varbl : ;
	    sval : IF longv = bytesforset THEN fattsize := bitsforset ELSE fattsize := bitsindword ;
	    lval : IF ldreg = psr THEN fattsize := bitsforset ELSE fattsize := bitsindword ;
	  END (* CASE KIND,WITH FATTR *) ;
	WITH gattr DO
	  CASE kind OF
	    varbl : ;
	    sval : IF longv = bytesforset THEN gattsize := bitsforset ELSE gattsize := bitsindword ;
	    lval : IF ldreg = psr THEN gattsize := bitsforset ELSE gattsize := bitsindword ;
	  END (* CASE KIND,WITH GATTR *) ;
                                                  (* FNO+FCL  GIVES  EACH OPERATOR. *)
	CASE fno + fcl OF
	  7 : (* 6+1 *)                         (* AND *)
	    BEGIN bolr := 1 ; revbolr := 1 ; linstaq := ianaq ;
	    END ;
	  8 : (* 7+1 *)                         (* OR *)
	    BEGIN bolr := 7 ; revbolr := 7 ; linstaq := ioraq ;
	    END ;
	  9 : (* 7+2 *)                         (* - *)
	    BEGIN bolr := 4 ; revbolr := 2 ; linstaq := inop ;
	    END ;
	  10 : (* 8+2 *)                        (* <= *)
	    BEGIN bolr := 2 ;                   (* A<=B   <--->  A * NOT(B) = [] *)
	      revbolr := 2 ; linstaq := inop ;
	      ltransf := 2 ;                    (* ZERO ON = TRUE *)
	    END ;
	  11 : (* 8+3 *)                        (* >= *)
	    BEGIN bolr := 2 ; revbolr := 4 ; linstaq := inop ; ltransf := 2 ;
	    END ;
	  13 : (* 8+5 *)                        (*  # *)
	    BEGIN bolr := 6 ; (* 0110 = EXCLUSIVE OR *) revbolr := 6 ; linstaq := icmpaq ;
	      ltransf := 6 ;                    (* ZERO OFF =TRUE *)
	    END ;
	  14 : (* 8+6 *)                        (*  = *)
	    BEGIN bolr := 6 ; revbolr := 6 ; linstaq := icmpaq ;
	      ltransf := 2 ;                    (* ZERO ON =TRUE *)
	    END ;
	END (* CASE FNO+FCL *) ;
	lshort := fattsize = bitsindword ;
	rshort := gattsize = bitsindword ;
	IF gattr.kind = varbl THEN
	  IF (NOT varissimple (gattr)) OR (gattr.pckd) THEN
	    rshort := false ;
	IF fattr.kind = lval THEN
	  IF fattr.ldregbloc@.saveplace # 0 THEN
	    lvalvarbl (fattr) ;
	classe1 := (fno + fcl) IN [9..11] ;     (*  -  <=  >= *)
$OPTIONS compile = trace $
	IF stattrace = high THEN
	  BEGIN
	    write (mpcogout, ' GENOPPW: FATTR and GATTR are:') ; nextline ;
	    printattr (fattr) ; printattr (gattr) ;
	    write (mpcogout, 'Fattsize, Gattsize, Lshort, Rshort are:',
	      fattsize, gattsize, lshort : 7, rshort : 7) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
	IF classe1 THEN
	  BEGIN
	    typseq := 6 ;
	  END (* CLASSE1 *) ELSE
	  BEGIN
	    WITH gattr DO
	      CASE fattr.kind OF
	        sval, varbl : IF lshort AND rshort THEN typseq := 2 ELSE
		  typseq := 6 ;
	        lval : IF lshort THEN
		  BEGIN
		    IF kind = lval THEN typseq := 8 ELSE
		      IF rshort THEN typseq := 1 ELSE
		        BEGIN
			typseq := 5 ;
			IF kind = varbl THEN
			  IF NOT varissimple (gattr) THEN typseq := 8 ;
		        END
		  END ELSE
		  BEGIN
		    IF (kind = lval) AND rshort THEN typseq := 7 ELSE
		      BEGIN
		        typseq := 5 ;
		      END ;
		  END (* LVAL *) ;
	      END (* CASE FATTR.KIND, WITH GATTR *) ;
	  END (* NOT CLASSE1 *) ;
	IF odd (typseq) THEN
	  BEGIN
	    tattr := fattr ; cattr := gattr ; tattsize := fattsize ; cattsize := gattsize ;
	  END (* ODD *) ELSE
	  BEGIN
	    tattr := gattr ; cattr := fattr ; tattsize := gattsize ; cattsize := fattsize ;
	    bolr := revbolr ;
	  END ;
	CASE typseq OF
	  0 : ;
	  1, 2 : BEGIN transfer (tattr, inaq) ;
	      calcvarient (cattr, lbase, ldisp, ltag) ;
	      IF cattr.kind = varbl THEN usednameaddr := cattr.nameaddr ;
	      genstand (lbase, ldisp, linstaq, ltag) ;
	    END ;
	  5, 6 : BEGIN transfer (tattr, inpsr) ; psrsize := bytesforset ;
	      IF cattr.kind = varbl THEN
	        IF varissimple (cattr) THEN
		calcvarient (cattr, lbase, ldisp, ltag) ELSE
		BEGIN
		  loadadr (cattr, pr3) ; lbase := pr3 ; ldisp := 0 ;
		END ELSE
	        IF cattr.kind = sval THEN
		BEGIN
		  IF cattr.longv = bytesindword THEN
		    BEGIN enterlcst (cattr.valpw, lretpt) ; cattsize := bitsindword ;
		      enterundlab (lretpt@.lplace) ;
		    END ELSE
		    BEGIN enterllcst (cattr.valpw, llretpt) ;
		      enterundlab (llretpt@.llplace) ;
		    END ;
		  genstand (nreg, 0, iepp3, tic) ; lbase := pr3 ; ldisp := 0 ;
		END ELSE
		calcvarient (cattr, lbase, ldisp, ltag) ;
	      mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
	      geneism (icsl, bolr, p0t0r0 (* FILL 0 *)) ;
	      IF cattr.kind = varbl THEN usednameaddr := cattr.nameaddr ;
	      gendescb (lbase, ldisp, 0, 0, cattsize, tn) ;
	      gendescb (pr6, psrdepw, 0, 0, bitsforset, tn) ;
	    END ;
	  7, 8 : BEGIN transfer (tattr, inpsr) ; psrsize := bytesforset ;
	      genstand (pr6, evareaw, istaq, tn) ;
	      mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
	      geneism (icsl, bolr, p0t0r0) ;
	      gendescb (pr6, evareaw, 0, 0, cattsize, tn) ;
	      gendescb (pr6, psrdepw, 0, 0, bitsforset, tn) ;
	    END ;
	END (* CASE TYPSEQ *) ;
	IF fno <= 7 THEN
	  BEGIN
	    gattr := tattr ; freeattr (cattr) ;
	  END ELSE
	  BEGIN
	    freeattr (tattr) ; freeattr (cattr) ;
	    WITH gattr DO
	      BEGIN
                                                  (* TYPTR  OUTSIDE *)
	        kind := lcond ; accbool := false ; accbloc := NIL ;
	        transf := ltransf ;
	      END ;
	  END (* FNO=8   RELATIONAL OPERATOR *) ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN GENOPPW @@@  WITH  TYPSEQ :', typseq : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* GENOPPW *) ;

(* ******************************* CHECK_DYNAMIC_STRING_LENGTH *************************** *)

      PROCEDURE check_dynamic_string_length (VAR fattr : attr) ;

        VAR
	loaded_reg : register ;
	string_attr : attr ; string_base : preg ; string_disp, loc1, loc2 : integer ;
	string_bloc : regpt ;


        BEGIN
	string_bloc := NIL ;
	IF fattr.typtr <> NIL THEN
	  IF fattr.typtr^.father_schema = string_ptr THEN
	    IF fattr.typtr^.actual_parameter_list <> NIL THEN
	      BEGIN
	        IF varissimple (fattr) THEN
		BEGIN
		  string_bloc := fattr.basebloc ;
		  string_base := fattr.basereg ; string_disp := fattr.dplmt DIV bytesinword ;
		END ELSE BEGIN
		  loadadr (fattr, nreg) ;
		  string_base := currentpr ; string_disp := 0 ;
		  WITH fattr DO
		    BEGIN
		      access := pointee ; basereg := currentpr ; basebloc := currentbloc ;
		      dplmt := 0 ;
		    END
		END ;
	        WITH fattr.typtr^ DO
		BEGIN
		  IF raisused THEN
		    BEGIN
		      loaded_reg := rq ;
		      sauvereg (rq, false) ;
		    END
		  ELSE BEGIN
		      loaded_reg := ra ;
		    END ;
		  IF actual_parameter_list^.klass <> konst THEN
		    BEGIN
		      addressvar (actual_parameter_list, string_attr, false) ;
		      IF loaded_reg = rq THEN
		        transfer (string_attr, inq)
		      ELSE
		        transfer (string_attr, inacc) ;
		      freeattr (string_attr) ;
		    END
		  ELSE
		    gencstecode (actual_parameter_list^.values, opaq [load, loaded_reg]) ;
		  IF string_bloc <> NIL THEN regenere (string_bloc) ;
		  genstand (string_base, string_disp, iszn, tn) ;
		  loc1 := indfich ; genstand (nreg, 0, itmi, tic) ;
		  genstand (string_base, string_disp, opaq [cmp, loaded_reg], tn) ;
		  loc2 := indfich ; genstand (nreg, 0, itpl, tic) ;
		  inser (cb, loc1) ;
		  genexceptcode (stringlength_range_error, loaded_reg) ;
		  inser (cb, loc2) ;
		END ;
	      END
        END ;


$OPTIONS page$

(* ************************************* PREPARE STRING *********************************** *)


      PROCEDURE prepare_string (VAR fattr : attr ; VAR info : string_item_info ; len_dest : destination) ;

        VAR
	a_or_q : register ;
	locbox : wcstpt ;

        PROCEDURE get_a_or_q ;
	BEGIN
	  IF len_dest = out THEN
	    IF raisused THEN
	      BEGIN
	        sauvereg (rq, false) ;
	        a_or_q := rq ;
	      END
	    ELSE a_or_q := ra
	  ELSE IF len_dest = inacc THEN a_or_q := ra
	    ELSE a_or_q := rq ;
	END ;

        PROCEDURE get_adr ;
	BEGIN
	  WITH info, fattr DO
	    IF varissimple (fattr) THEN
	      BEGIN
	        register := basereg ; bloc := basebloc ;
	        wdisp := dplmt DIV bytesinword ; bdisp := dplmt MOD bytesinword ;
	      END
	    ELSE BEGIN
	        loadadr (fattr, nreg) ; bloc_is_new := true ;
	        register := currentpr ; bloc := currentbloc ;
	        wdisp := 0 ; bdisp := 0 ;
	      END ;
	END (* get_adr *) ;

        BEGIN                                     (* prepare string *)
	WITH info DO
	  BEGIN
	    bloc_is_new := false ; len_bloc := NIL ;
	    l_tag := tn ; l_val := -1 ; mfari := a1r0i0 ; reg_bloc := NIL ;
	  END ;
	WITH fattr, info DO
	  IF typtr = charptr THEN
	    BEGIN                               (* CHAR *)
	      length := 1 ; l_val := 1 ;
	      CASE kind OF
	        varbl : BEGIN
		  get_adr ;
		  IF NOT pckd THEN bdisp := bdisp + 3 ;
		END ;
	        lval : BEGIN
		  wdisp := oldnewstor (bytesinword) DIV bytesinword ; bdisp := 3 ;
		  register := pr6 ; bloc := NIL ;
		  IF fattr.ldregbloc <> NIL THEN regenere (fattr.ldregbloc) ;
		  genstand (pr6, wdisp, opaq [stor, fattr.ldreg], tn) ;
		  freeattr (fattr) ;
		END ;
	        sval : BEGIN
		  entercst (val, locbox) ;
		  getpr ; register := currentpr ; bloc := currentbloc ;
		  bloc_is_new := true ;
		  enterundlab (locbox^.cstplace) ;
		  genstand (nreg, 0, prinst [epp, register], tic) ;
		  wdisp := 0 ; bdisp := 3 ;
		  freeattr (fattr) ;
		END ;
	      END ;
	    END
	  ELSE IF isstring (fattr) THEN
	      IF conformantdim (typtr) THEN
	        BEGIN
		get_a_or_q ;
		init_desc_address (fattr.nameaddr, fattr) ;
		register := basereg ; bloc := basebloc ; wdisp := 0 ; bdisp := 0 ;
		regenere (fattr.descbloc) ;
		IF len_dest <> out THEN
		  BEGIN
		    sauvereg (a_or_q, true) ; reg_bloc := currentbloc ;
		  END ;
		genstand (fattr.descreg, 1, opaq [load, a_or_q], tn) ;
		genstand (fattr.descreg, 0, opaq [sub, a_or_q], tn) ;
		genstand (nreg, 1, opaq [add, a_or_q], tdl) ; (* reg contains actual length *)
		IF len_dest = out THEN
		  BEGIN
		    len_place := oldnewstor (bytesinword) DIV bytesinword ;
		    genstand (pr6, len_place, opaq [stor, a_or_q], tn) ;
		    len_reg := pr6 ;
		  END ;
		freebloc (gattr.descbloc) ;
		mfari := a1r1i0 ; l_tag := modif [a_or_q] ;
	        END
	      ELSE
	        BEGIN
		CASE kind OF
		  chain : BEGIN
		      loadadr (fattr, nreg) ; register := currentpr ; bloc := currentbloc ;
		      wdisp := 0 ; bdisp := 0 ; length := alfactp^.alfalong ;
		      bloc_is_new := true ;
		    END ;
		  varbl : BEGIN
		      get_adr ;
		      length := typtr^.hi - typtr^.lo + 1 ;
		    END ;
		END ;
		IF length > twoto12 THEN
		  BEGIN
		    get_a_or_q ;
		    IF len_dest <> out THEN
		      BEGIN
		        sauvereg (a_or_q, true) ; reg_bloc := currentbloc ;
		      END ;
		    gencstecode (length, opaq [load, a_or_q]) ;
		    IF len_dest = out THEN
		      BEGIN
		        len_place := oldnewstor (bytesinword) DIV bytesinword ;
		        len_reg := pr6 ;
		        genstand (pr6, len_place, opaq [stor, a_or_q], tn)
		      END ;
		    mfari := a1r1i0 ; l_tag := modif [a_or_q] ;
		  END
		ELSE l_val := length ;
	        END
	    ELSE IF typtr^.father_schema = string_ptr THEN
	        BEGIN
		get_adr ;
		IF len_dest <> out THEN
		  BEGIN
		    get_a_or_q ;
		    sauvereg (a_or_q, true) ; reg_bloc := currentbloc ;
		    IF bloc <> NIL THEN regenere (bloc) ;
		    genstand (register, wdisp, opaq [load, a_or_q], tn) ;
		    mfari := a1r1i0 ; l_tag := modif [a_or_q] ;
		  END
		ELSE BEGIN
		    len_place := wdisp ; len_reg := register ; len_bloc := bloc ;
		  END ;
		wdisp := wdisp + 1 ; bdisp := 0 ;
	        END ;
	WITH info DO
	  IF l_val = -1 THEN
	    BEGIN
	      l_val := 0 ; length_is_known := false
	    END
	  ELSE length_is_known := true ;
        END (* prepare_string *) ;

$OPTIONS page $

(* ************************************ GENCONCAT **************************** *)

      PROCEDURE genconcat (VAR fattr : attr) ;

        TYPE
	item_info = RECORD
	  register : preg ;
	  bloc : regpt ; bloc_is_new : boolean ;
	  length, length_place : integer ;
	  wdisp, bdisp : integer ;
	END ;
        VAR
	first_alfa, current_alfa : alfapt ;
	result_place : integer ;
	fattr_info, gattr_info : item_info ;
	target_pointer : preg ; target_bloc : regpt ;
	total_length, total_place : integer ;

        PROCEDURE prepare (VAR fattr : attr ; VAR info : item_info) ;

	PROCEDURE add_length ;
	  BEGIN
	    WITH info DO
	      IF total_place = 0 THEN
	        total_length := total_length + length
	      ELSE
	        BEGIN
		gencstecode (length, ildq) ;
		genstand (pr6, total_place, iasq, tn)
	        END ;
	  END ;
	PROCEDURE add_variable_length ;
	  BEGIN
	    WITH info DO
	      BEGIN
	        IF total_place = 0 THEN
		BEGIN
		  total_place := oldnewstor (bytesinword) DIV bytesinword ;
		  IF total_length <> 0 THEN
		    gencstecode (total_length, iadq) ;
		  genstand (pr6, total_place, istq, tn) ;
		END
	        ELSE
		genstand (pr6, total_place, iasq, tn) ;
	      END ;
	  END ;

	PROCEDURE get_adr ;
	  BEGIN
	    WITH info, fattr DO
	      IF varissimple (fattr) THEN
	        BEGIN
		register := basereg ; bloc := basebloc ;
		wdisp := dplmt DIV bytesinword ; bdisp := dplmt MOD bytesinword ;
	        END
	      ELSE BEGIN
		loadadr (fattr, nreg) ; bloc_is_new := true ;
		register := currentpr ; bloc := currentbloc ;
		wdisp := 0 ; bdisp := 0 ;
	        END ;
	  END (* get_adr *) ;

	BEGIN                                   (* prepare *)
	  info.length_place := 0 ; info.bloc_is_new := false ;
	  WITH fattr, info DO
	    IF typtr = charptr THEN
	      BEGIN                             (* CHAR *)
	        length := 1 ; length_place := 0 ;
	        CASE kind OF
		varbl : BEGIN
		    get_adr ;
		    IF NOT pckd THEN bdisp := bdisp + 3 ;
		  END ;
		lval, sval : BEGIN
		    wdisp := oldnewstor (bytesinword) DIV bytesinword ; bdisp := 3 ;
		    register := pr6 ; bloc := NIL ;
		    IF kind = lval THEN
		      BEGIN
		        IF fattr.ldregbloc <> NIL THEN regenere (fattr.ldregbloc) ;
		        genstand (pr6, wdisp, opaq [stor, fattr.ldreg], tn)
		      END
		    ELSE BEGIN
		        sauvereg (ra, false) ;
		        genstand (nreg, fattr.val, ilda, tdl) ;
		        genstand (pr6, wdisp, ista, tn)
		      END ;
		    freeattr (fattr) ;
		  END ;
	        END ;
	        add_length ;
	      END
	    ELSE IF isstring (fattr) THEN
	        IF conformantdim (typtr) THEN
		BEGIN
		  init_desc_address (fattr.nameaddr, fattr) ;
		  register := basereg ; bloc := basebloc ; wdisp := 0 ; bdisp := 0 ;
		  regenere (fattr.descbloc) ;
		  sauvereg (rq, false) ;
		  genstand (fattr.descreg, 1, ildq, tn) ;
		  genstand (fattr.descreg, 0, isbq, tn) ;
		  genstand (nreg, 1, iadq, tdl) ; (* Q contains actual length *)
		  length_place := oldnewstor (bytesinword) DIV bytesinword ;
		  genstand (pr6, length_place, istq, tn) ;
		  add_variable_length ;
		  freebloc (fattr.descbloc) ;
		END
	        ELSE
		BEGIN
		  CASE kind OF
		    chain : BEGIN
		        loadadr (fattr, nreg) ; register := currentpr ; bloc := currentbloc ;
		        wdisp := 0 ; bdisp := 0 ; length := alfactp^.alfalong ; length_place := 0 ;
		        bloc_is_new := true ;
		      END ;
		    varbl : BEGIN
		        get_adr ;
		        length := typtr^.hi - typtr^.lo + 1 ; length_place := 0 ;
		      END ;
		  END ;
		  add_length ;
		END
	      ELSE IF typtr^.father_schema = string_ptr THEN
		BEGIN
		  get_adr ;
		  IF bloc <> NIL THEN regenere (bloc) ;
		  sauvereg (rq, false) ;
		  genstand (register, wdisp, ildq, tn) ;
		  wdisp := wdisp + 1 ; bdisp := 0 ;
		  add_variable_length ;
		END ;
	END (* prepare *) ;

        PROCEDURE concat_item (VAR fattr : attr) ; (* ADD CONSTANT CHAIN *)

	VAR
	  it : integer ;
	  current_box : alfapt ;
	PROCEDURE add_char (ch : char) ;

	  BEGIN
	    total_length := total_length + 1 ;
	    IF current_alfa^.longfill = longalfbox THEN
	      BEGIN
	        new (current_alfa^.nextval) ; IF current_alfa^.nextval = NIL THEN heaperror ;
	        current_alfa := current_alfa^.nextval ;
	        WITH current_alfa^ DO
		BEGIN
		  longfill := 0 ;
		  nextval := NIL ;
		  alfaval := '  ' ;
		END ;
	      END ;
	    WITH current_alfa^ DO
	      BEGIN
	        longfill := longfill + 1 ;
	        alfaval [longfill] := ch ;
	      END ;
	  END ;

	BEGIN
	  WITH fattr DO
	    IF kind = sval THEN add_char (chr (val))
	    ELSE BEGIN
	        current_box := alfactp^.alfadeb ;
	        WHILE current_box <> NIL DO
		BEGIN
		  WITH current_box^ DO
		    FOR it := 1 TO longfill DO add_char (alfaval [it]) ;
		  current_box := current_box^.nextval ;
		END ;
	      END ;
	  freeattr (fattr) ;
	END ;

        BEGIN                                     (* genconcat *)
	IF ((fattr.kind = chain) OR (fattr.kind = sval))
	  AND ((gattr.kind = chain) OR (gattr.kind = sval)) THEN
	  BEGIN                                 (* BOTH ARE KNOWN CONSTANTS *)
	    new (first_alfa) ; IF first_alfa = NIL THEN heaperror ;
	    current_alfa := first_alfa ; WITH current_alfa^ DO
	      BEGIN
	        nextval := NIL ;
	        longfill := 0 ;
	        alfaval := '  ' ;
	      END ;
	    total_length := 0 ;
	    concat_item (fattr) ; concat_item (gattr) ;
	    WITH gattr DO
	      BEGIN
	        kind := chain ; typtr := alfaptr ;
	        create_konst_box (alfactp, '  ', alfaconst) ;
	        WITH alfactp^ DO
		BEGIN
		  contype := alfaptr ; succ := nextalf ;
		  alfadeb := first_alfa ;
		  alfalong := total_length ;
		END ;
	        nextalf := alfactp ;
	      END
	  END
	ELSE
	  BEGIN                                 (* DYNAMIC EVALUATION *)
	    total_place := 0 ; total_length := 0 ;
	    prepare (fattr, fattr_info) ;
	    prepare (gattr, gattr_info) ;
	    sauvereg (ra, false) ; sauvereg (rq, false) ;
	    IF total_place = 0 THEN             (* total is known *)
	      BEGIN
	        gencstecode (total_length, ildq) ;
	        result_place := oldnewstor (total_length + 4) DIV bytesinword ;
	        genstand (pr6, result_place, iepp3, tn) ;
	      END
	    ELSE
	      BEGIN
	        genstand (pr6, total_place, ildq, tn) ;
	        stack_extension ;
	        genstand (pr6, evareaw, iepp3, tny) ;
	      END ;
	    genstand (pr3, 0, istq, tn) ;
	    genstand (pr3, 1, prinst [epp, pr3], tn) ;
	    WITH fattr_info, fattr DO
	      BEGIN
	        IF bloc <> NIL THEN regenere (bloc) ;
	        IF length_place = 0 THEN
		IF typtr^.father_schema = string_ptr THEN
		  genstand (register, wdisp - 1, ildq, tn)
		ELSE gencstecode (length, ildq)
	        ELSE genstand (pr6, length_place, ildq, tn) ;
	        mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
	        IF bloc <> NIL THEN regenere (bloc) ;
	        geneism (imlr, ord (' '), p0t0r0) ;
	        IF kind = varbl THEN usednameaddr := nameaddr
	        ELSE IF kind = chain THEN usednameaddr := alfactp ;
	        gendesca (register, wdisp, bdisp, l9, 0, tql) ;
	        gendesca (pr3, 0, 0, l9, 0, tql) ;
	        IF bloc_is_new THEN freebloc (bloc) ;
	      END ;
	    freeattr (fattr) ;
	    genstand (pr3, 0, ia9bd, tql) ;
	    WITH gattr_info, gattr DO
	      BEGIN
	        IF bloc <> NIL THEN regenere (bloc) ;
	        IF length_place = 0 THEN
		IF typtr^.father_schema = string_ptr THEN
		  genstand (register, wdisp - 1, ildq, tn)
		ELSE gencstecode (length, ildq)
	        ELSE genstand (pr6, length_place, ildq, tn) ;
	        mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
	        geneism (imlr, ord (' '), p0t0r0) ;
	        IF kind = varbl THEN usednameaddr := nameaddr
	        ELSE IF kind = chain THEN usednameaddr := alfactp ;
	        gendesca (register, wdisp, bdisp, l9, 0, tql) ;
	        gendesca (pr3, 0, 0, l9, 0, tql) ;
	        IF bloc_is_new THEN freebloc (bloc) ;
	      END ;
	    freeattr (gattr) ;
	    initattrvarbl (gattr) ;
	    getpr ; target_pointer := currentpr ; target_bloc := currentbloc ;
	    genstand (pr3, 0, prinst [epp, target_pointer], tn) ;
	    WITH gattr DO
	      BEGIN
	        IF total_place = 0 THEN
		genstand (pr6, result_place, prinst [epp, target_pointer], tn)
	        ELSE
		genstand (pr6, evareaw, prinst [epp, target_pointer], tny) ;
	        temporary := true ;
	        basereg := target_pointer ; basebloc := target_bloc ; dplmt := 0 ;
	        create_types_box (typtr, blank, records, false) ;
	        WITH typtr^ DO
		BEGIN
		  father_schema := string_ptr ;
		  IF total_place = 0 THEN
		    BEGIN
		      create_konst_box (actual_parameter_list, 'maxlength', wordconst) ;
		      WITH actual_parameter_list^ DO
		        BEGIN
			values := total_length ; contype := intptr ;
			nxtel := NIL ;
		        END
		    END
		  ELSE BEGIN
		      create_vars_box (actual_parameter_list, 'maxlength') ;
		      WITH actual_parameter_list^ DO
		        BEGIN
			vtype := intptr ;
			vaddr := total_place ;
			nxtel := NIL ;
		        END ;
		    END ;
		END ;
	      END ;
	  END
        END (* genconcat *) ;

$OPTIONS page $

(* ************************** GEN_STRING_COMP ******************************** *)

      PROCEDURE gen_string_comp (VAR fattr : attr ; fcl : integer) ;

(* THIS PROCEDURE IS SIMILAR TO genstcomp, BUT IS MORE GENERAL BECAUSE
   IT COMPARES ANY STRING EXPRESSION TO ANY OTHER STRING EXPRESSION.

   (STRING EXPRESSION MAY BE CHAR, PACKED ARRAY OF CHAR (CONFORMANT OR NOT), OR STRING )

*)

        VAR
	result_place, ltransf : integer ;
	fattr_info, gattr_info : string_item_info ;

        BEGIN
	IF fcl IN [2, 4] THEN
	  BEGIN
	    IF fcl = 2 THEN fcl := 3 ELSE fcl := 1 ;
	    prepare_string (fattr, gattr_info, inacc) ;
	    prepare_string (gattr, fattr_info, inq) ;
	  END
	ELSE BEGIN
	    prepare_string (fattr, fattr_info, inacc) ;
	    prepare_string (gattr, gattr_info, inq) ;
	  END ;
	WITH fattr_info DO
	  BEGIN
	    IF reg_bloc <> NIL THEN regenere (reg_bloc) ;
	    IF bloc <> NIL THEN regenere (bloc) ;
	  END ;
	WITH gattr_info DO
	  BEGIN
	    IF reg_bloc <> NIL THEN regenere (reg_bloc) ;
	    IF bloc <> NIL THEN regenere (bloc) ;
	  END ;
	mfari1 := fattr_info.mfari ; mfari2 := gattr_info.mfari ;
	geneism (icmpc, 0, p0t0r0) ;
	WITH fattr_info DO
	  gendesca (register, wdisp, bdisp, l9, l_val, l_tag) ;
	WITH gattr_info DO
	  gendesca (register, wdisp, bdisp, l9, l_val, l_tag) ;
	CASE fcl OF
	  1 : ltransf := 11 ;                   (*  CARRY OFF   TRUE *)
	  3 : ltransf := 12 ;                   (*  CARRY ON    TRUE *)
	  5 : ltransf := 6 ;                    (*  ZERO  OFF   TRUE *)
	  6 : ltransf := 2 ;                    (*  ZERO  ON    TRUE *)
	END (* CASE FCL *) ;
	WITH fattr_info DO
	  BEGIN
	    IF bloc_is_new THEN freebloc (bloc) ;
	    IF reg_bloc <> NIL THEN freebloc (reg_bloc) ;
	  END ;
	WITH gattr_info DO
	  BEGIN
	    IF bloc_is_new THEN freebloc (bloc) ;
	    IF reg_bloc <> NIL THEN freebloc (reg_bloc) ;
	  END ;
	freeattr (fattr) ;
	freeattr (gattr) ;
	WITH gattr DO                           (* TYPTR OUTSIDE *)
	  BEGIN
	    kind := lcond ; accbool := false ; accbloc := NIL ;
	    transf := ltransf ;
	  END ;
        END (* GEN STRING COMP *) ;

$OPTIONS page$

(* **************************** GEN_STRING_POSITION ************************************* *)

      PROCEDURE gen_string_position (VAR fattr : attr) ;

(* GENERATES CODE TO FIND POSITION OF STRING DESCRIBED BY FATTR
   IN STRING DESCRIBED BY GATTR                                          *)

        VAR
	to_find_info, to_scan_info : string_item_info ;
	fattr_info, gattr_info : string_item_info ;
	loc1, loc2, temp_place, retplace : integer ;

        BEGIN
	temp_place := oldnewstor (bytesinword) DIV bytesinword ;
	prepare_string (fattr, to_find_info, inacc) ; (* STRING TO FIND *)
	IF to_find_info.l_tag = tal THEN        (* LENGTH IS IN A *)
	  genstand (pr6, temp_place, ista, tn) ;
	prepare_string (gattr, to_scan_info, inq) ; (* STRING TO SCAN *)
	WITH to_scan_info DO
	  IF length_is_known THEN
	    BEGIN
	      sauvereg (rq, false) ;
	      gencstecode (l_val, ildq) ;
	    END ;
	IF to_find_info.l_tag = tal THEN        (* LENGTH IS IN A *)
	  genstand (pr6, temp_place, isbq, tn)
	ELSE
	  genstand (nreg, to_find_info.l_val, isbq, tdl) ;
	loc1 := indfich ; genstand (nreg, 0, itmi, tic) ;
	genstand (pr6, temp_place, istq, tn) ;
	genstand (nreg, 0, ildq, tdl) ;
	WITH to_scan_info DO
	  BEGIN
	    IF register IN [prstatic, prlink, pr6] THEN
	      BEGIN
	        getpr ;
	        IF bloc <> NIL THEN regenere (bloc) ;
	        genstand (register, wdisp, prinst [epp, currentpr], tn) ;
	        register := currentpr ; bloc := currentbloc ; bloc_is_new := true ;
	        wdisp := 0 ;
	      END ;
	  END ;
	WITH to_find_info DO
	  BEGIN
	    IF reg_bloc <> NIL THEN regenere (reg_bloc) ;
	    IF bloc <> NIL THEN regenere (bloc) ;
	  END ;
	sauvereg (x7, false) ;
	genstand (nreg, 1, ildx7, tdu) ;
	retplace := cb ;                        (* LOOP BEGINNING *)
	genstand (nreg, 1, iadq, tdl) ;
	mfari1 := to_find_info.mfari ; mfari2 := to_find_info.mfari ;
	geneism (icmpc, 0, p0t0r0) ;
	WITH to_scan_info DO
	  gendesca (register, wdisp, bdisp, l9, to_find_info.l_val, to_find_info.l_tag) ;
	WITH to_find_info DO
	  gendesca (register, wdisp, bdisp, l9, l_val, l_tag) ;
	loc2 := indfich ; genstand (nreg, 0, itze, tic) ; (* TRANSFER IF FOUND *)
	genstand (to_scan_info.register, 0, ia9bd, tx7) ;
	genstand (pr6, temp_place, icmpq, tn) ;
	genstand (nreg, (retplace - cb) DIV bytesinword, itmoz, tic) ;
	inser (cb, loc1) ;
	genstand (nreg, 0, ildq, tdl) ;
	inser (cb, loc2) ;
	WITH to_scan_info DO
	  BEGIN
	    IF bloc_is_new THEN freebloc (bloc) ;
	    IF reg_bloc <> NIL THEN freebloc (reg_bloc) ;
	  END ;
	WITH to_find_info DO
	  BEGIN
	    IF bloc_is_new THEN freebloc (bloc) ;
	    IF reg_bloc <> NIL THEN freebloc (reg_bloc) ;
	  END ;
	freeattr (fattr) ;
	freeattr (gattr) ;
	initattrvarbl (gattr) ;
	WITH gattr DO
	  BEGIN
	    kind := lval ; typtr := intptr ; ldreg := rq ;
	    newbloc (rq) ; ldregbloc := currentbloc ;
	  END ;
	genstand (nreg, 0, iorq, tdl) ;         (* TO SET INDICATORS : STANDARD FUNCTION RETURN OR PASCAL *)
        END ;

$OPTIONS page$

(* ********************************* GEN_SUBSTRING ****************************** *)


      PROCEDURE gen_substring (VAR string_attr, disp_attr, len_attr : attr) ;

        VAR
	loc1, temp_place : integer ;
	check_done : boolean ;
	string_info : string_item_info ;
	total_length, total_place : integer ;
	result_pointer : preg ; result_bloc : regpt ; result_place : integer ;
	loaded_reg : register ;
	dm1_place, from_wdisp, from_bdisp, dm1_value : integer ;
	disp_in_desc : boolean ; i : integer ;
	from_bloc : regpt ; from_reg : preg ; from_bloc_is_new : boolean ;

        BEGIN
	total_length := -1 ; result_place := 0 ; (* NOT KNOWN *)
                                                  (* COMPUTE "DISP - 1" - ERROR IF NEGATIVE -
                                                     STORE IT AT "DM1_PLACE" IN STACK IF NOT KNOWN    *)
	dm1_place := 0 ;
	WITH disp_attr DO
	  BEGIN
	    CASE kind OF
	      varbl : IF raisused THEN
		BEGIN loaded_reg := rq ; sauvereg (rq, false) ; transfer (disp_attr, inq) END
	        ELSE BEGIN loaded_reg := ra ; transfer (disp_attr, inacc) END ;
	      lval : BEGIN
		loaded_reg := ldreg ; IF ldregbloc <> NIL THEN regenere (ldregbloc)
	        END ;
	      sval : IF val - 1 < 0 THEN
		BEGIN error (278) ; dm1_value := 0 END
	        ELSE dm1_value := val - 1 ;
	    END ;
	    IF kind IN [varbl, lval] THEN
	      BEGIN
	        dm1_place := oldnewstor (bytesinword) DIV bytesinword ;
	        genstand (nreg, 1, opaq [sub, loaded_reg], tdl) ;
	        IF asscheck THEN
		BEGIN
		  loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
		  genexceptcode (substring_offset_error, loaded_reg) ;
		  inser (cb, loc1) ;
		END ;
	        genstand (pr6, dm1_place, opaq [stor, loaded_reg], tn) ;
	      END ;
	  END ;
	freeattr (disp_attr) ;
	WITH len_attr DO                        (* GET LENGTH IN Q *)
	  BEGIN
	    CASE kind OF
	      varbl : BEGIN
		sauvereg (rq, false) ;
		transfer (len_attr, inq) ;
	        END ;
	      sval : BEGIN
		IF raisused THEN
		  BEGIN sauvereg (rq, false) ; loaded_reg := rq ; END
		ELSE loaded_reg := rq ;
		IF val < 0 THEN
		  BEGIN error (279) ; total_length := 0 END
		ELSE total_length := val ;
		result_place := oldnewstor (4 + total_length + 3) DIV bytesinword ;
		gencstecode (total_length, opaq [load, loaded_reg]) ;
		genstand (pr6, result_place, opaq [stor, loaded_reg], tn) ;
	        END ;
	      lval : BEGIN
		IF ldregbloc <> NIL THEN regenere (ldregbloc) ;
		IF ldreg = ra THEN
		  BEGIN
		    sauvereg (rq, false) ;
		    genstand (nreg, 36, ilrs, tn) ;
		    IF asscheck THEN genstand (nreg, 0, iorq, tdl) ; (* TO SET INDICATORS *)
		  END ;
	        END ;
	    END ;
	    freeattr (len_attr) ;
	  END ;
	IF result_place = 0 THEN                (* DYNAMIC ALLOCATION *)
	  BEGIN
	    IF asscheck THEN
	      BEGIN
	        loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
	        genexceptcode (substring_negative_length_error, rq) ;
	        inser (cb, loc1) ;
	      END ;
	    stack_extension ;                   (* GET SPACE FOR RESULT *)
	    genstand (pr6, evareaw, istq, tny) ; (* STORE LENGTH IN RESULT STRING *)
	    total_place := oldnewstor (bytesinword) DIV bytesinword ;
	    genstand (pr6, total_place, istq, tn) ; (* FOR MAXLENGTH VARIABLE OF RESULT TYPE *)
	    loaded_reg := rq ;
	  END ;
	prepare_string (string_attr, string_info, inacc) ;
	WITH string_info DO
	  BEGIN
	    IF reg_bloc <> NIL THEN freebloc (reg_bloc) ;
	    IF asscheck THEN
	      BEGIN                             (* CHECK THAT ACTUAL_LENGTH - (DISP-1) >= LEN *)
	        check_done := false ;
	        IF length_is_known THEN
		IF dm1_place = 0 THEN
		  IF total_length <> -1 THEN
		    BEGIN
		      IF (l_val - dm1_value < total_length) THEN
		        error (280) ;
		      check_done := true ;
		    END
		  ELSE
		    BEGIN
		      sauvereg (ra, false) ;
		      gencstecode (l_val - dm1_value, ilda) ;
		    END
		ELSE
		  BEGIN
		    sauvereg (ra, false) ;
		    gencstecode (l_val, ilda) ;
		    genstand (pr6, dm1_place, isba, tn) ;
		  END
	        ELSE
		IF dm1_place = 0 THEN
		  gencstecode (dm1_value, isba)
		ELSE
		  genstand (pr6, dm1_place, isba, tn) ;
	        IF NOT check_done THEN
		BEGIN
		  IF total_length <> -1 THEN
		    gencstecode (total_length, icmpa)
		  ELSE
		    genstand (pr6, total_place, icmpa, tn) ;
		  loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
		  genexceptcode (substring_too_long_error, ra) ;
		  inser (cb, loc1) ;
		END ;
	      END (* ASSCHECK *) ;
                                                  (* NOW COMPUTE ADRESSES AND LENGTH FOR STRING TO MOVE *)
	    disp_in_desc := false ; from_bloc_is_new := false ;
	    IF dm1_place = 0 THEN
	      BEGIN
	        i := (wdisp * bytesinword) + bdisp + dm1_value ;
	        from_wdisp := i DIV bytesinword ;
	        from_bdisp := i MOD bytesinword ;
	        IF from_wdisp < twoto17 THEN
		BEGIN
		  from_reg := register ; from_bloc := bloc ;
		  disp_in_desc := true ;
		END ;
	      END ;
	    IF NOT disp_in_desc THEN
	      BEGIN
	        IF register IN [prstatic, prlink, pr6] THEN
		BEGIN
		  getpr ;
		  IF bloc <> NIL THEN regenere (bloc) ;
		  genstand (register, 0, prinst [epp, currentpr], tn) ;
		  from_reg := currentpr ; from_bloc := currentbloc ; from_bloc_is_new := true ;
		END
	        ELSE BEGIN
		  from_bloc := bloc ; from_bloc_is_new := false ; from_reg := register ;
		END ;
	        IF dm1_place <> 0 THEN
		genstand (pr6, dm1_place, ildq, tn)
	        ELSE gencstecode (dm1_value, ildq) ;
	        IF from_bloc <> NIL THEN regenere (from_bloc) ;
	        genstand (from_reg, 0, ia9bd, tql) ;
	        from_wdisp := wdisp ; from_bdisp := bdisp ;
	      END ;
	    l_val := 0 ; l_tag := tal ;
	    IF total_length <> -1 THEN
	      BEGIN
	        result_pointer := pr6 ; result_bloc := NIL ;
	        IF total_length < twoto12 THEN
		BEGIN
		  l_val := total_length ; l_tag := tn
		END
	        ELSE
		gencstecode (total_length, ilda)
	      END
	    ELSE
	      BEGIN
	        getpr ; result_place := 0 ; result_pointer := currentpr ; result_bloc := currentbloc ;
	        genstand (pr6, evareaw, prinst [epp, result_pointer], tny) ;
	        genstand (pr6, total_place, ilda, tn) ;
	      END ;
	    IF l_tag = tn THEN
	      BEGIN
	        mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
	      END
	    ELSE BEGIN
	        mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
	      END ;
	    IF from_bloc <> NIL THEN regenere (from_bloc) ;
	    IF result_bloc <> NIL THEN regenere (result_bloc) ;
	    geneism (imlr, 0, p0t0r0) ;
	    gendesca (from_reg, from_wdisp, from_bdisp, l9, l_val, l_tag) ;
	    gendesca (result_pointer, result_place + 1, 0, l9, l_val, l_tag) ;
	    IF bloc_is_new THEN freebloc (bloc) ;
	    freeattr (string_attr) ;
	    IF from_bloc_is_new THEN freebloc (from_bloc) ;
	  END ;
	initattrvarbl (gattr) ;
	WITH gattr DO
	  BEGIN
	    temporary := true ;
	    basereg := result_pointer ; basebloc := result_bloc ; dplmt := result_place * bytesinword ;
	    create_types_box (typtr, blank, records, false) ;
	    WITH typtr^ DO
	      BEGIN
	        father_schema := string_ptr ;
	        IF total_length <> -1 THEN
		BEGIN
		  create_konst_box (actual_parameter_list, 'maxlength', wordconst) ;
		  WITH actual_parameter_list^ DO
		    BEGIN
		      values := total_length ; contype := intptr ;
		      nxtel := NIL ;
		    END
		END
	        ELSE BEGIN
		  create_vars_box (actual_parameter_list, 'maxlength') ;
		  WITH actual_parameter_list^ DO
		    BEGIN
		      vtype := intptr ;
		      vaddr := total_place ;
		      nxtel := NIL ;
		    END ;
		END ;
	      END ;
	  END ;
        END ;

$OPTIONS page$

(* ************************************* GEN_DELETE ****************************************** *)

      PROCEDURE gen_delete (VAR string_attr, disp_attr, del_len_attr : attr) ;

(* GENERATES CODE FOR

   DELETE (<STRING VARIABLE>, DISP, LEN) ;

   *)
        VAR
	string_info : string_item_info ;
	dm1_place, dm1_value : integer ;
	loaded_reg : register ; loc1 : integer ;
	del_len_place, del_len_value : integer ; del_len_bloc : regpt ;
	del_len_reg : preg ;
	remaining_length : integer ; check_done : boolean ;
	from_reg, to_reg : preg ; from_bloc, to_bloc : regpt ;
	from_offset_in_desc, to_offset_in_desc : boolean ;
	from_bloc_is_new, to_bloc_is_new : boolean ;
	i : integer ; l_len : integer ; l_tag : tag ;
	to_wdisp, to_bdisp, from_wdisp, from_bdisp : integer ;
	del_len_bloc_is_new : boolean ;

        BEGIN
                                                  (* COMPUTE "DISP - 1" - ERROR IF NEGATIVE -
                                                     STORE IT AT "DM1_PLACE" IN STACK IF NOT KNOWN    *)
	dm1_place := 0 ;
	WITH disp_attr DO
	  BEGIN
	    CASE kind OF
	      varbl : IF raisused THEN
		BEGIN loaded_reg := rq ; sauvereg (rq, false) ; transfer (disp_attr, inq) END
	        ELSE BEGIN loaded_reg := ra ; transfer (disp_attr, inacc) END ;
	      lval : BEGIN
		loaded_reg := ldreg ; IF ldregbloc <> NIL THEN regenere (ldregbloc)
	        END ;
	      sval : IF val - 1 < 0 THEN
		BEGIN error (276) ; dm1_value := 0 END
	        ELSE dm1_value := val - 1 ;
	    END ;
	    IF kind IN [varbl, lval] THEN
	      BEGIN
	        dm1_place := oldnewstor (bytesinword) DIV bytesinword ;
	        genstand (nreg, 1, opaq [sub, loaded_reg], tdl) ;
	        IF asscheck THEN
		BEGIN
		  loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
		  genexceptcode (delete_offset_error, loaded_reg) ;
		  inser (cb, loc1) ;
		END ;
	        genstand (pr6, dm1_place, opaq [stor, loaded_reg], tn) ;
	      END ;
	  END ;
	freeattr (disp_attr) ;
                                                  (* GET INFO ABOUT LEN. IF KNOWN, LEN_VALUE IS <> -1 *)
	del_len_reg := nreg ; del_len_bloc := NIL ; del_len_bloc_is_new := false ;
	del_len_value := -1 ; del_len_place := 0 ;
	WITH del_len_attr DO
	  BEGIN
	    CASE kind OF
	      varbl : IF varissimple (del_len_attr) THEN
		BEGIN
		  del_len_reg := basereg ; del_len_bloc := basebloc ; del_len_place := dplmt DIV bytesinword
		END
	        ELSE BEGIN
		  loadadr (del_len_attr, nreg) ; del_len_reg := currentpr ; del_len_bloc := currentbloc ;
		  del_len_bloc_is_new := true
		END ;
	      sval : IF val < 0 THEN BEGIN error (277) ; del_len_value := 0 END
	        ELSE del_len_value := val ;
	      lval : BEGIN
		del_len_place := oldnewstor (bytesinword) DIV bytesinword ; del_len_reg := pr6 ;
		IF ldregbloc <> NIL THEN regenere (ldregbloc) ;
		genstand (del_len_reg, del_len_place, opaq [stor, ldreg], tn) ;
	        END ;
	    END ;
	    IF asscheck THEN
	      IF kind IN [varbl, lval] THEN
	        BEGIN
		IF del_len_attr.kind = varbl THEN
		  IF symbolmap THEN nameisref (del_len_attr.nameaddr, symbolfile, symbolline) ;
		genstand (del_len_reg, del_len_place, iszn, tn) ;
		loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
		genexceptcode (delete_negative_length_error, ra) ;
		inser (cb, loc1) ;
	        END ;
	  END ;
	remaining_length := -1 ;
	prepare_string (string_attr, string_info, inacc) ;
	WITH string_info DO
	  BEGIN
	    IF reg_bloc <> NIL THEN freebloc (reg_bloc) ;
	    check_done := false ;
	    IF length_is_known THEN
	      BEGIN
	        sauvereg (ra, false) ; check_done := false ;
	        IF del_len_value <> -1 THEN
		BEGIN
		  IF (l_val - del_len_value) < 0 THEN
		    BEGIN error (276) ; del_len_value := 0 END ;
		  IF dm1_place = 0 THEN
		    BEGIN
		      remaining_length := l_val - del_len_value - dm1_value ;
		      IF remaining_length < 0 THEN
		        BEGIN error (276) ; remaining_length := 0 END ;
		    END ;
		  gencstecode (l_val - del_len_value, ilda) ; check_done := true ;
		END
	        ELSE BEGIN
		  gencstecode (l_val, ilda) ;
		  IF del_len_bloc <> NIL THEN regenere (del_len_bloc) ;
		  IF del_len_attr.kind = varbl THEN
		    IF symbolmap THEN nameisref (del_len_attr.nameaddr, symbolfile, symbolline) ;
		  genstand (del_len_reg, del_len_place, isba, tn)
		END
	      END
	    ELSE IF del_len_value <> -1 THEN
	        gencstecode (del_len_value, isba)
	      ELSE BEGIN
		IF del_len_bloc <> NIL THEN regenere (del_len_bloc) ;
		IF del_len_attr.kind = varbl THEN
		  IF symbolmap THEN nameisref (del_len_attr.nameaddr, symbolfile, symbolline) ;
		genstand (del_len_reg, del_len_place, isba, tn)
	        END ;
	    IF bloc <> NIL THEN regenere (bloc) ;
	    genstand (register, wdisp - 1, ista, tn) ; (* STORE NEW LENGTH OF THE STRING *)
                                                  (* NOW, GET IN RA LENGTH OF STRING TO BE MOVED *)
	    l_len := 0 ; l_tag := tal ;
	    IF remaining_length = -1 THEN
	      IF dm1_place = 0 THEN
	        IF dm1_value <> 0 THEN
		gencstecode (dm1_value, isba)
	        ELSE                            (* nothing *)
	      ELSE BEGIN
		genstand (pr6, dm1_place, isba, tn) ;
		IF asscheck THEN
		  BEGIN
		    loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
		    genexceptcode (delete_too_long_error, ra) ;
		    inser (cb, loc1) ;
		  END ;
	        END
	    ELSE
	      BEGIN
	        l_len := remaining_length ; l_tag := tn ;
	        gencstecode (remaining_length, ilda) ;
	      END ;
                                                  (* COMPUTE ADDRESSES OF MOVE *)
	    from_bloc := NIL ; to_bloc := NIL ;
	    to_offset_in_desc := false ; from_offset_in_desc := false ;
	    to_bloc_is_new := false ; from_bloc_is_new := false ;
	    IF dm1_place = 0 THEN
	      BEGIN
	        i := (wdisp * bytesinword) + dm1_value + bdisp ;
	        to_wdisp := i DIV bytesinword ;
	        to_bdisp := i MOD bytesinword ;
	        IF to_wdisp < twoto17 THEN
		BEGIN
		  to_bloc := bloc ; to_reg := register ;
		  to_offset_in_desc := true ;
		  IF del_len_value <> -1 THEN
		    BEGIN
		      i := i + del_len_value ;
		      from_wdisp := i DIV bytesinword ;
		      from_bdisp := i MOD bytesinword ;
		      IF from_wdisp < twoto17 THEN
		        BEGIN
			from_bloc := to_bloc ; from_reg := to_reg ;

			from_offset_in_desc := true ;
		        END ;
		    END ;
		END ;
	      END ;
	    IF NOT to_offset_in_desc THEN
	      BEGIN
	        to_wdisp := wdisp ; to_bdisp := bdisp ;
	        IF register IN [prstatic, prlink, pr6] THEN
		BEGIN
		  getpr ; to_bloc := currentbloc ; to_reg := currentpr ;
		  to_bloc_is_new := true ;
		  IF bloc <> NIL THEN regenere (bloc) ;
		  genstand (register, 0, prinst [epp, to_reg], tn) ;
		END
	        ELSE BEGIN
		  to_bloc := bloc ; to_reg := register ;
		END ;
	        IF dm1_place = 0 THEN
		gencstecode (dm1_value, ildq)
	        ELSE genstand (pr6, dm1_place, ildq, tn) ;
	        IF to_bloc <> NIL THEN regenere (to_bloc) ;
	        genstand (to_reg, 0, ia9bd, tql) ;
	      END ;
	    IF NOT from_offset_in_desc THEN
	      IF del_len_value <> -1 THEN
	        BEGIN
		i := (to_wdisp * bytesinword) + to_bdisp + del_len_value ;
		from_wdisp := i DIV bytesinword ;
		from_bdisp := i MOD bytesinword ;
		IF from_wdisp < twoto17 THEN
		  BEGIN
		    from_bloc := to_bloc ; from_reg := to_reg ;
		    from_offset_in_desc := true ;
		  END ;
	        END ;
	    IF NOT from_offset_in_desc THEN
	      BEGIN
	        from_bdisp := bdisp ; from_wdisp := wdisp ;
	        IF del_len_value <> -1 THEN
		gencstecode (del_len_value, ildq)
	        ELSE BEGIN
		  IF del_len_bloc <> NIL THEN regenere (del_len_bloc) ;
		  genstand (del_len_reg, del_len_place, ildq, tn) ;
		END ;
	        IF del_len_bloc_is_new THEN freebloc (del_len_bloc) ;
	        freeattr (del_len_attr) ;
	        getpr ; from_bloc := currentbloc ; from_reg := currentpr ;
	        from_bloc_is_new := true ;
	        IF from_bloc <> NIL THEN regenere (from_bloc) ;
	        genstand (to_reg, 0, prinst [epp, from_reg], tn) ;
	        genstand (from_reg, 0, ia9bd, tql) ;
	      END
	    ELSE BEGIN
	        IF del_len_bloc_is_new THEN freebloc (del_len_bloc) ;
	        freeattr (del_len_attr)
	      END ;
	    mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
	    IF to_bloc <> NIL THEN regenere (to_bloc) ; IF from_bloc <> NIL THEN regenere (from_bloc) ;
	    geneism (imlr, 0, p0t0r0) ;
	    gendesca (from_reg, from_wdisp, from_bdisp, l9, l_len, l_tag) ;
	    gendesca (to_reg, to_wdisp, to_bdisp, l9, l_len, l_tag) ;
	    IF bloc_is_new THEN freebloc (bloc) ;
	    IF from_bloc_is_new THEN freebloc (from_bloc) ;
	    IF to_bloc_is_new THEN freebloc (to_bloc) ;
	    freeattr (string_attr) ;
	  END ;
        END (* GEN_DELETE *) ;

$OPTIONS page$

(* ************************************* GEN_INSERT ****************************************** *)

      PROCEDURE gen_insert (VAR insert_attr, string_attr, disp_attr : attr) ;

(* GENERATES CODE FOR

   INSERT (<STRING EXPRESSION>, <STRING VARIABLE>, DISP) ;

   *)
        VAR
	maxl_attr : attr ;
	string_info : string_item_info ;
	insert_info : string_item_info ;
	dm1_place, dm1_value : integer ;
	loaded_reg : register ; loc1, loc2, loc3 : integer ;
	check_done : boolean ;
	from_reg, to_reg : preg ; from_bloc, to_bloc : regpt ;
	from_offset_in_desc, to_offset_in_desc : boolean ;
	from_bloc_is_new, to_bloc_is_new : boolean ;
	i : integer ; l_len : integer ; l_tag : tag ;
	to_wdisp, to_bdisp, from_wdisp, from_bdisp : integer ;

        BEGIN
                                                  (* COMPUTE "DISP - 1" - ERROR IF NEGATIVE -
                                                     STORE IT AT "DM1_PLACE" IN STACK IF NOT KNOWN    *)
	dm1_place := 0 ;
	WITH disp_attr DO
	  BEGIN
	    CASE kind OF
	      varbl : IF raisused THEN
		BEGIN loaded_reg := rq ; sauvereg (rq, false) ; transfer (disp_attr, inq) END
	        ELSE BEGIN loaded_reg := ra ; transfer (disp_attr, inacc) END ;
	      lval : BEGIN
		loaded_reg := ldreg ; IF ldregbloc <> NIL THEN regenere (ldregbloc)
	        END ;
	      sval : IF val - 1 < 0 THEN
		BEGIN error (276) ; dm1_value := 0 END
	        ELSE dm1_value := val - 1 ;
	    END ;
	    IF kind IN [varbl, lval] THEN
	      BEGIN
	        dm1_place := oldnewstor (bytesinword) DIV bytesinword ;
	        genstand (nreg, 1, opaq [sub, loaded_reg], tdl) ;
	        IF asscheck THEN
		BEGIN
		  loc1 := indfich ; genstand (nreg, 0, itpl, tic) ;
		  genexceptcode (delete_offset_error, loaded_reg) ;
		  inser (cb, loc1) ;
		END ;
	        genstand (pr6, dm1_place, opaq [stor, loaded_reg], tn) ;
	      END ;
	  END ;
	freeattr (disp_attr) ;
                                                  (* GET INFO ABOUT TARGET *)
	prepare_string (string_attr, string_info, out) ;
                                                  (* GET INFO ABOUT INSERT.  *)
	prepare_string (insert_attr, insert_info, out) ;
                                                  (* CHECK THAT LENGTH (STRING) IS VALID,
                                                     AND THAT LENGTH (STRING) + LENGTH (INSERT) IS NOT > MAXLENGTH (STRING) *)
	IF asscheck THEN
	  WITH string_attr, string_info DO
	    IF typtr^.actual_parameter_list <> NIL THEN
	      BEGIN
	        WITH typtr^ DO
		BEGIN
		  IF raisused THEN
		    BEGIN
		      loaded_reg := rq ;
		      sauvereg (rq, false) ;
		    END
		  ELSE BEGIN
		      loaded_reg := ra ;
		    END ;
		  IF actual_parameter_list^.klass <> konst THEN
		    BEGIN
		      addressvar (actual_parameter_list, maxl_attr, false) ;
		      IF loaded_reg = rq THEN
		        transfer (maxl_attr, inq)
		      ELSE
		        transfer (maxl_attr, inacc) ;
		      freeattr (maxl_attr) ;
		    END
		  ELSE
		    gencstecode (actual_parameter_list^.values, opaq [load, loaded_reg]) ;
		  IF bloc <> NIL THEN regenere (bloc) ;
		  genstand (register, wdisp - 1, iszn, tn) ;
		  loc1 := indfich ; genstand (nreg, 0, itmi, tic) ;
		  genstand (register, wdisp - 1, opaq [cmp, loaded_reg], tn) ;
		  loc2 := indfich ; genstand (nreg, 0, itmi, tic) ;
		  WITH insert_info DO
		    IF length_is_known THEN
		      gencstecode (length, opaq [sub, loaded_reg])
		    ELSE
		      BEGIN
		        IF len_bloc <> NIL THEN regenere (len_bloc) ;
		        genstand (len_reg, len_place, opaq [sub, loaded_reg], tn) ;
		      END ;
		  genstand (register, wdisp - 1, opaq [cmp, loaded_reg], tn) ;
		  loc3 := indfich ; genstand (nreg, 0, itpl, tic) ;
		  genexceptcode (insert_overflow_error, loaded_reg) ;
		  inser (cb, loc1) ; inser (cb, loc2) ;
		  genexceptcode (stringlength_range_error, loaded_reg) ;
		  inser (cb, loc3) ;
		END ;
	      END ;
	WITH string_info DO
	  BEGIN
                                                  (* NOW, GET IN RA LENGTH OF STRING TO BE MOVED *)
	    genstand (register, wdisp - 1, ilda, tn) ;
	    IF dm1_place = 0 THEN
	      IF dm1_value <> 0 THEN
	        gencstecode (dm1_value, isba)
	      ELSE                              (* nothing *)
	    ELSE
	      genstand (pr6, dm1_place, isba, tn) ;
                                                  (* STORE NEW LENGTH IN STRING *)
	    IF insert_info.length_is_known THEN
	      IF insert_info.length <> 0 THEN
	        BEGIN
		gencstecode (insert_info.length, ildq) ;
		IF bloc <> NIL THEN regenere (bloc) ;
		genstand (register, wdisp - 1, iasq, tn) ;
	        END
	      ELSE                              (* nothing *)
	    ELSE
	      BEGIN
	        IF insert_info.len_bloc <> NIL THEN regenere (insert_info.len_bloc) ;
	        genstand (insert_info.len_reg, insert_info.len_place, ildq, tn) ;
	        IF bloc <> NIL THEN regenere (bloc) ;
	        genstand (register, wdisp - 1, iasq, tn) ;
	      END ;
                                                  (* COMPUTE ADDRESSES OF MOVE *)
	    to_bloc := NIL ; from_bloc := NIL ;
	    from_offset_in_desc := false ; to_offset_in_desc := false ;
	    from_bloc_is_new := false ; to_bloc_is_new := false ;
	    IF dm1_place = 0 THEN
	      BEGIN
	        i := (wdisp * bytesinword) + dm1_value + bdisp ;
	        from_wdisp := i DIV bytesinword ;
	        from_bdisp := i MOD bytesinword ;
	        IF from_wdisp < twoto17 THEN
		BEGIN
		  from_bloc := bloc ; from_reg := register ;
		  from_offset_in_desc := true ;
		  IF insert_info.length_is_known THEN
		    BEGIN
		      i := i + insert_info.length ;
		      to_wdisp := i DIV bytesinword ;
		      to_bdisp := i MOD bytesinword ;
		      IF to_wdisp < twoto17 THEN
		        BEGIN
			to_bloc := from_bloc ; to_reg := from_reg ;
			to_offset_in_desc := true ;
		        END ;
		    END ;
		END ;
	      END ;
	    IF NOT from_offset_in_desc THEN
	      BEGIN
	        from_wdisp := wdisp ; from_bdisp := bdisp ;
	        IF register IN [prstatic, prlink, pr6] THEN
		BEGIN
		  getpr ; from_bloc := currentbloc ; from_reg := currentpr ;
		  from_bloc_is_new := true ;
		  IF bloc <> NIL THEN regenere (bloc) ;
		  genstand (register, 0, prinst [epp, from_reg], tn) ;
		END
	        ELSE BEGIN
		  from_bloc := bloc ; from_reg := register ;
		END ;
	        IF dm1_place = 0 THEN
		gencstecode (dm1_value, ildq)
	        ELSE genstand (pr6, dm1_place, ildq, tn) ;
	        IF from_bloc <> NIL THEN regenere (from_bloc) ;
	        genstand (from_reg, 0, ia9bd, tql) ;
	      END ;
	    IF NOT to_offset_in_desc THEN
	      IF insert_info.length_is_known THEN
	        BEGIN
		i := (from_wdisp * bytesinword) + from_bdisp + insert_info.length ;
		to_wdisp := i DIV bytesinword ;
		to_bdisp := i MOD bytesinword ;
		IF to_wdisp < twoto17 THEN
		  BEGIN
		    to_bloc := from_bloc ; to_reg := from_reg ;
		    to_offset_in_desc := true ;
		  END ;
	        END ;
	    IF NOT to_offset_in_desc THEN
	      BEGIN
	        to_bdisp := bdisp ; to_wdisp := wdisp ;
	        IF insert_info.length_is_known THEN
		gencstecode (insert_info.length, ildq)
	        ELSE BEGIN
		  genstand (insert_info.len_reg, insert_info.len_place, ildq, tn) ;
		END ;
	        getpr ; to_bloc := currentbloc ; to_reg := currentpr ;
	        to_bloc_is_new := true ;
	        IF to_bloc <> NIL THEN regenere (to_bloc) ;
	        genstand (from_reg, 0, prinst [epp, to_reg], tn) ;
	        genstand (to_reg, 0, ia9bd, tql) ;
	      END ;
	    mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
	    IF from_bloc <> NIL THEN regenere (from_bloc) ; IF to_bloc <> NIL THEN regenere (to_bloc) ;
	    geneism (imrl, 0, p0t0r0) ;
	    gendesca (from_reg, from_wdisp, from_bdisp, l9, 0, tal) ;
	    gendesca (to_reg, to_wdisp, to_bdisp, l9, 0, tal) ;
	    IF to_bloc_is_new THEN freebloc (to_bloc) ;
	    mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
	    WITH insert_info DO
	      BEGIN
	        l_tag := tal ; l_val := 0 ;
	        IF bloc <> NIL THEN regenere (bloc) ;
	        IF length_is_known THEN
		IF length < twoto12 THEN
		  BEGIN
		    l_tag := tn ; l_val := length ;
		    mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
		  END
		ELSE
		  gencstecode (length, ilda)
	        ELSE
		genstand (len_reg, len_place, ilda, tn) ;
	        geneism (imlr, 0, p0t0r0) ;
	        gendesca (register, wdisp, bdisp, l9, l_val, l_tag) ;
	        gendesca (from_reg, from_wdisp, from_bdisp, l9, l_val, l_tag) ;
	      END ;
	    IF bloc_is_new THEN freebloc (bloc) ;
	    IF from_bloc_is_new THEN freebloc (from_bloc) ;
	    WITH insert_info DO
	      IF bloc_is_new THEN freebloc (bloc) ;
	    freeattr (insert_attr) ;
	    freeattr (string_attr) ;
	  END ;
        END (* GEN_INSERT *) ;

      BEGIN
      END.                                        (* END OF MODULE *)
