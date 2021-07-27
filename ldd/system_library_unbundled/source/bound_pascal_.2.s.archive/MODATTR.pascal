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


PROGRAM modattr ;

$OPTIONS switch trace := true ; switch security := true ; t + $


    $IMPORT
                                                  (* IMPORTED PROCEDURES    *)
      'RACINE (pascal) ' :
        error,
        nextline ;
      'STATE  (pascal) ' :
        freebloc,
        newbloc,
        printstatusregister,
        sauvereg ;
      'STATE (pascal) ' :
        transfer ;
      'GENERE (pascal) ' :
        genstand ;

(* IMPORTED VARIABLES *)
      'GENERE (pascal)' :
        illegal_generation ;
      'RACINE (pascal) ' :
        boxheader,
        charptr,
        envstandard,
        declarationpart,
        level,
        mpcogout,
        nilptr,
        realptr,
        string_ptr ;
      'STATE (pascal) ' :
        currentbloc,
        maxprused,
        regname,
        stattrace,
        tabacc,
        tabkind
      $

    $EXPORT
      convreal,
      easyvar,
      is_pl1_varying_char,
      freeattr,
      initattrvarbl,
      is_possible_string,
      isstring,
      lvalvarbl,
      printattr,
      varissimple
    $

$INCLUDE 'CONSTTYPE' $

$OPTIONS page $

    VAR
                                                  (* REDEFINE IMPORTED VARIABLES   *)
                                                  (* FROM RACINE *)

      boxheader : PACKED ARRAY [1..120] OF char ;
      charptr : ctp ;
      declarationpart : boolean ;
      envstandard : stdkind ;
      level : levrange ;
      mpcogout : text ;
      nilptr : ctp ;
      realptr : ctp ;
      string_ptr : ctp ;

(* FROM GENERE *)
      illegal_generation : boolean ;


(* FROM STATE *)

      currentbloc : regpt ;                       (* LAST CREATED BOX REGISTER *)
      maxprused : preg ;                          (* LAST POINTER REGISTER USED IN GETPR *)
      regname : ARRAY [register] OF PACKED ARRAY [1..4] OF char ; (* REGIST. NAMES *)
      stattrace : levtrace ;                      (* TRACE FOR MODULE STATEMENT *)
      tabacc : ARRAY [attraccess] OF alfa ;       (* MNEMONICS USED IN TRACE *)
      tabkind : ARRAY [attrkind] OF alfa ;        (* MNEMONICS USED IN TRACE *)
$OPTIONS page $

(* REDEFINE IMPORTED PROCEDURES *)
(* FROM RACINE *)

    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;

(* FROM STATE *)

    PROCEDURE freebloc (VAR fbtofree : regpt) ; EXTERNAL ;
    PROCEDURE newbloc (freg : register) ; EXTERNAL ;
    PROCEDURE printstatusregister ; EXTERNAL ;
    PROCEDURE sauvereg (freg : register ; fload : boolean) ; EXTERNAL ;

(* FROM STATE *)

    PROCEDURE transfer (VAR fattr : attr ; inwhat : destination) ; EXTERNAL ;

(* FROM GENERE *)

    PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ; EXTERNAL ;

$OPTIONS page $

(* ************************************ PRINTATTR ***************************** *)

    PROCEDURE printattr (VAR fattr : attr) ;

(* C  USED IN TRACE CONTEXT IN ORDER TO PRINT
   . LOADED REGISTERS
   . MEANINGSFULL FIELDS OF FATTR
   C *)
      VAR
        it : integer ;
      BEGIN                                       (* PRINTATTR *)
        write (mpcogout, boxheader) ; nextline ;
        printstatusregister ;
        WITH fattr DO
	BEGIN
	  IF typtr = NIL THEN
	    write (mpcogout, '* REQUESTED ATTR HAS A TYPTR AT NIL') ELSE
	    BEGIN
	      write (mpcogout, '* TYPTR IS AT @', ord (typtr),
	        ' FATTR_KIND IS:', tabkind [kind]) ;
	      nextline ;
	      CASE kind OF
	        varbl : BEGIN
		  write (mpcogout, '* VLEV,BASEREG ARE:', vlev : 4, regname [basereg] : 9,
		    ' BASEBLOC IS AT @', ord (basebloc), ' DPLMT,PCKD ARE:', dplmt,
		    pckd : 6) ;
		  nextline ;
		  write (mpcogout, '* INXREG,INXMEM,INXMEMRW ARE:', regname [inxreg] : 9,
		    inxmem, inxmemrw : 6, ' INXBLOC IS AT @', ord (inxbloc)) ;
		  nextline ;
		  write (mpcogout, '* ACCESS IS:', tabacc [access],
		    ' ITSDPLMT IS:', itsdplmt) ;
		  write (mpcogout, ' DESCREG IS ', regname [descreg], ', DESCBLOC AT @', ord (descbloc)) ;
		END (* VARBL *) ;
	        lval : BEGIN
		  write (mpcogout, '* LDREG IS ', regname [ldreg], ' LDREGBLOC IS AT @',
		    ord (ldregbloc)) ;
		  IF ldreg = psr THEN write (mpcogout, ' PSRSIZE IS:', psrsize) ;
		END (* LVAL *) ;
	        chain : write (mpcogout, '* ALFACTP IS AT @', ord (alfactp)) ;
	        sval : BEGIN
		  IF typtr = realptr THEN
		    write (mpcogout, '* RSVAL IS', rsval) ELSE
		    IF typtr = nilptr THEN
		      write (mpcogout, '*  SVAL IS "NIL" ') ELSE
		      IF typtr@.form = power THEN
		        BEGIN
			write (mpcogout, '* SETS. LONGV IS', longv, ' VALPW0..7 ARE') ;
			FOR it := 0 TO 7 DO
			  write (mpcogout, '*', valpw [it]) ;
		        END (* SETS *) ELSE
		        write (mpcogout, '* VAL IS :', val) ;
		END (* SVAL *) ;
	        lcond : BEGIN
		  write (mpcogout, '* ACCBOOL IS:', accbool : 6,
		    ' ACCBLOC IS AT @', ord (accbloc),
		    ' TRANSF IS:', transf : 4) ;
		END (* LCOND *) ;
	      END (* CASE KIND *) ;
	    END (* TYPTR # NIL *) ;
	END (* WITH FATTR *) ;
        nextline ;
        write (mpcogout, boxheader) ; nextline ;
      END (* PRINTATTR *) ;

$OPTIONS page $

(* ******************************************** INITATTRVARBL ******************************** *)

    PROCEDURE initattrvarbl (VAR fattr : attr) ;

      BEGIN
        WITH fattr DO
	BEGIN
	  kind := varbl ;
	  vlev := level ;
	  basereg := pr6 ;
	  basebloc := NIL ;
	  access := direct ;
	  dplmt := 0 ;
	  inxreg := nxreg ;
	  inxbloc := NIL ;
	  inxmem := 0 ;
	  inxmemrw := true ;
	  itsdplmt := 0 ;
	  pckd := false ;
	  nameaddr := NIL ;
	  descreg := nreg ;
	  descbloc := NIL ;
	  temporary := false ;
	END ;
      END (* INITATTRVARBL *) ;

$OPTIONS page $

(* ******************************* IS_PL1_VARYING_CHAR *********************** *)

    FUNCTION is_pl1_varying_char (VAR typectp : ctp) : boolean ;

      VAR
        locbool : boolean ;
        locctp_1, locctp_2 : ctp ;
        locmax : integer ;

      BEGIN
        locbool := false ;
        IF typectp <> NIL THEN
	WITH typectp^ DO
	  IF form = records THEN
	    IF (recvar = NIL) AND (fstfld <> NIL) THEN
	      IF (fstfld^.fldtype <> NIL) AND (fstfld^.nxtel <> NIL) THEN
	        BEGIN
		locctp_1 := fstfld ;
		WITH locctp_1^.fldtype^ DO
		  BEGIN
		    IF form = numeric THEN
		      IF nmin = 0 THEN
		        BEGIN
			locmax := nmax ;
			locctp_2 := locctp_1^.nxtel ;
			IF (locctp_2 <> NIL) AND (locctp_2^.nxtel = NIL) THEN
			  IF locctp_2^.fldtype <> NIL THEN
			    WITH locctp_2^.fldtype^ DO
			      IF form = arrays THEN
			        IF pack THEN
				IF aeltype = charptr THEN
				  IF inxtype <> NIL THEN
				    IF inxtype^.form = numeric THEN
				      IF (lo = 1) AND (hi = locmax) THEN
				        locbool := true ;
		        END ;
		  END ;
	        END ;
        is_pl1_varying_char := locbool ;
      END (* IS_PL1_VARYING_CHAR *) ;
$OPTIONS page $

(* ************************************ FCT. ISSTRING ************************* *)

    FUNCTION isstring (VAR fattr : attr) : boolean ;

(* C  RETURNS TRUE   IF  FATTR  DESCRIBES   A "STRING"
   .  PACKED  ARRAY  OF  CHAR
   .  CHAIN                       *  FATTR IS NOT ALTERED
   C *)
      BEGIN                                       (* ISSTRING *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT-FIN  ISSTRING @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        isstring := false ;
        WITH fattr DO
	IF typtr # NIL THEN
	  IF kind = chain THEN
                                                  (* <--- *) isstring := true ELSE
	    WITH typtr@ DO
	      IF form = arrays THEN
	        IF pack THEN
		IF aeltype = charptr THEN
		  IF inxtype # NIL THEN
		    IF inxtype@.form = numeric THEN
		      IF envstandard <> stdextend THEN
		        BEGIN
			IF NOT conformant THEN
			  IF lo = 1 THEN isstring := true ;
		        END ELSE
		        isstring := true ;
      END (* ISSTRING *) ;


$OPTIONS page $

(* ************************************ FCT.  VARISSIMPLE ********************* *)
    FUNCTION varissimple (VAR fattr : attr) : boolean ;

(* C RETURNS TRUE  WHEN  FATTR DESCRIBES A VARIABLE  EASY TO ADDRESS
   NO CHANGE FOR FATTR
   C *)
      VAR
        variss : boolean ;
      BEGIN                                       (* VARISSIMPLE *)
        variss := false ;
        WITH fattr DO
	IF access = direct THEN
	  IF (vlev = 0) OR (vlev = level) THEN
	    IF inxreg = nxreg THEN
	      IF inxmem = 0 THEN
                                                  (* <--- *)
	        variss := true ;
        varissimple := variss ;                   (*    <---------   *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ Fin de VARISSIMPLE @@@ avec valeur retournee:',
	    variss) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* VARISSIMPLE *) ;


$OPTIONS page $

(* ************************************ FCT. EASYVAR ************************** *)

    FUNCTION easyvar (VAR fattr : attr) : boolean ;

(* C ."FATTR" IS NOT CHANGED
   .RETURNS TRUE FOR AN EASY ADDRESSED  VARIABLE
   * NOT PACKED, INDEX NOT SAVED, NO STORAGE INDEX
   C *)
(* E ERRORS DETECTED
   430  TYPTR = NIL
   431  KIND  # VARBL
   E *)
      VAR
        easyv : boolean ;
      BEGIN                                       (* EASYVAR *)
        easyv := false ;
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT EASYVAR @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
$OPTIONS compile = security $
        IF fattr.typtr = NIL THEN error (430) ELSE
	IF fattr.kind # varbl THEN error (431) ELSE
$OPTIONS compile = true $
	  WITH fattr DO
	    IF NOT pckd THEN
	      IF inxmem = 0 THEN
	        IF inxbloc = NIL THEN
		easyv := true ELSE
		IF inxbloc@.saveplace = 0 THEN
		  easyv := true ;
        easyvar := easyv ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN EASYVAR @@@ WITH RETURNED VALUE:', easyv) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* EASYVAR *) ;

$OPTIONS page $

(* ************************************ FREEATTR ****************************** *)

    PROCEDURE freeattr (VAR fattr : attr) ;

(* C  THIS PROCEDURE  DOESN'T MODIFY FATTR,  BUT FREES  ALL ASSOCIATED  BOXES
   (AND REGISTERS)
   C *)
      BEGIN                                       (* FREEATTR *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT FREEATTR @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        WITH fattr DO
	CASE kind OF
	  lval : freebloc (ldregbloc) ;
	  varbl :
	    BEGIN IF basereg <= maxprused THEN freebloc (basebloc) ;
	      IF inxreg # nxreg THEN freebloc (inxbloc) ;
	      freebloc (descbloc) ;
	    END (* VARBL *) ;
	  lcond : IF accbool THEN freebloc (accbloc) ;
	  sval, chain : ;
	END (* CASE KIND,WITH FATTR *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN FREEATTR @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* FREEATTR *) ;

$OPTIONS page $

(* ************************************ LVALVARBL ***************************** *)

    PROCEDURE lvalvarbl (VAR fattr : attr) ;

(* C  .MUST BE CALLED ONLY FOR  "LVAL"
   .CHANGES  THIS ATTR  INTO A VARIABLE  DIRECT FROM CURRENT LEVEL.
   EITHER  FOR  A  SAVED REGISTER
   EITHER  FOR  PSR
   .IN OTHER CASES  NO OPERATION
   C *)
(* E ERRORS DETECTED
   425  FATTR.KIND MUST BE LVAL
   426  FATTR.LDREGBLOC IS NIL
   E *)
      VAR
        locdep : integer ;
      BEGIN                                       (* LVALVARBL *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN write (mpcogout, '@@@ DEBUT LVALVARBL @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
$OPTIONS compile = security $
        IF fattr.kind # lval THEN error (425) ELSE
	IF fattr.ldregbloc = NIL THEN error (426) ELSE
$OPTIONS compile = true $
	  WITH fattr DO
	    BEGIN
	      IF ldregbloc@.saveplace # 0 THEN  (* SAVED REGISTER *)
	        locdep := ldregbloc@.saveplace ELSE
	        IF ldreg = psr THEN
		locdep := psrdepb ELSE
		locdep := 0 ;
	      IF locdep # 0 THEN
	        BEGIN
		freebloc (ldregbloc) ;
                                                  (* NOW CHANGE  FATTR *)
		initattrvarbl (fattr) ;
		dplmt := locdep ;
$OPTIONS compile = trace $
		IF stattrace > none THEN
		  BEGIN write (mpcogout, '* LVALVARBL .ATTR RECEIVED BECOMES DIRECT') ;
		    nextline ;
		  END ;
$OPTIONS compile = true $
	        END ;
	    END (* WITH FATTR *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN LVALVARBL @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* LVALVARBL *) ;

$OPTIONS page $

(* ************************************ CONVREAL ****************************** *)

    PROCEDURE convreal (VAR fattr : attr) ;

(* C   WORKS  ON A NUMERIC ATTR
   .SVAL   CHANGE IT  IN RSVAL
   .NOT LVAL    TRANSFER  AND  CALL  OPERATOR
   .LVAL SAVED   TRANSFER. ,OPERATOR.
   C *)
(* E ERRORS DETECTED
   402: TYPTR@.FORM MUST BE NUMERIC
   411: TYPTR IS NIL
   E *)
      VAR
        lop : integer ;
      BEGIN                                       (* CONVREAL *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT CONVREAL @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
$OPTIONS compile = security $
        IF fattr.typtr = NIL THEN error (411) ELSE
	IF fattr.typtr@.form # numeric THEN error (402) ELSE
$OPTIONS compile = true $
	  WITH fattr DO
	    BEGIN
	      IF kind = sval THEN
	        rsval := val ELSE
	        IF declarationpart THEN illegal_generation := true
	        ELSE
		BEGIN
		  IF kind = lval THEN
		    lvalvarbl (fattr) ;
		  IF kind # lval THEN
		    transfer (fattr, inacc) ;
		  IF ldreg = ra THEN
		    BEGIN
		      sauvereg (rq, false) ; lop := rafltplace ;
		    END ELSE
		    BEGIN
		      sauvereg (ra, false) ; lop := rqfltplace ;
		    END ;
		  genstand (pr0, lop, itsp3, tn) ;
		  freebloc (ldregbloc) ; newbloc (reaq) ;
		  ldreg := reaq ; ldregbloc := currentbloc ;
		END ;
	      typtr := realptr ;
	    END (* WITH FATTR *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN CONVREAL @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* CONVREAL *) ;

$OPTIONS page $

(* ***************************** IS_POSSIBLE_STRING *********************** *)

    FUNCTION is_possible_string (VAR fattr : attr) : boolean ;

      BEGIN
        IF fattr.typtr = NIL THEN is_possible_string := false ELSE
	is_possible_string := isstring (fattr) OR (fattr.typtr^.father_schema = string_ptr) OR (fattr.typtr = charptr) ;
      END ;

    BEGIN
    END.
