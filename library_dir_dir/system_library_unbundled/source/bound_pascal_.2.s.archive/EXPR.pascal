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
  PROGRAM expr ;

    $IMPORT
                                                  (* IMPORTED PROCEDURES  *)
      'RACINE (pascal)' :
        crealfabox,
        error,
        insymbol,
        nameisref,
        nextline,
        skip,
        warning ;
      'GENERE (pascal)' :
        enterreftosymbol,
        gendesca,
        gendescb,
        geneism,
        genstand,
        inser ;
      'CONTEXTTABLE (pascal)' :
        checkminmax,
        compatbin,
        conformantdim,
        create_konst_box,
        findminmax,
        warningminmax ;
      'MODATTR (pascal)' :
        convreal,
        easyvar,
        freeattr,
        initattrvarbl,
        is_possible_string,
        isstring,
        lvalvarbl,
        printattr,
        varissimple ;
      'MODVARIABLE (pascal) ' :
        init_desc_address,
        passparams,
        variable ;

      'STATE (pascal)' :
        addressvar,
        calcvarient,
        checkbnds,
        choicerarq,
        entercst,
        enterlcst,
        enterllcst,
        enterundlab,
        freebloc,
        gencheckmultover,
        gencstecode,
        genexceptcode,
        inbounds,
        loadadr,
        loadbase,
        newbloc,
        oldnewstor,
        raisused,
        regenere,
        sauvereg,
        transfer,
        variab ;
      'GENOPER (pascal)' :
        check_dynamic_string_length,
        genandor,
        gencompare,
        genconcat,
        gendivmod,
        genopadd,
        genopdivi,
        genopmult,
        genoppw,
        genopsub,
        genptcomp,
        gen_string_comp,
        gen_string_position,
        gen_substring,
        genstcomp ;
      'optimized_procedures (alm)' :
        search,
        srchrec ;
                                                  (* IMPORTED VARIABLES *)
      'RACINE (pascal)' :
        alfaptr,
        boolptr,
        charptr,
        cl,
        ctptr,
        declarationpart,
        envstandard,
        exportablecode,
        interactive,
        intptr,
        ival,
        lamptr,
        level,
        longchaine,
        longstring,
        mpcogout,
        next,
        nilptr,
        no,
        pascalfrench,
        pnumptr,
        realptr,
        rval,
        string_ptr,
        symbolfile,
        symbolline,
        symbolmap,
        textfilectp,
        undecptr ;
      'DECLARE (pascal)' :
        lkc,
        nextalf ;
      'GENERE (pascal)' :
        cb,
        indfich,
        mfari1,
        mfari2,
        usednameaddr ;
      'STATE (pascal)' :
        arrayboundsctp,
        asscheck,
        currentbloc,
        currentpr,
        divcheck,
        gattr,
        inputctp,
        inxcheck,
        linktomain,
        linktomainplace,
        maxinxused,
        maxprused,
        modif,
        nulpw,
        opaq,
        prinst,
        psrsize,
        stattrace,
        workformaths,
        workformathsplacew $

    $EXPORT
      expression $



$OPTIONS page $


$INCLUDE 'CONSTTYPE' $

$OPTIONS page $

    VAR

(* REDEFINE IMPORTED VARIABLES     *)
(* FROM RACINE  *)
      declarationpart : boolean ;
      next : ctp ;
      longstring : integer ;
      mpcogout : text ; nilptr : ctp ;
      cl : integer ;
      envstandard : stdkind ;
      lamptr : ctp ;
      longchaine : integer ;
      no : integer ;
      pascalfrench : boolean ;
      pnumptr : ctp ;
      realptr : ctp ;
      rval : real ;
      string_ptr : ctp ;
      symbolfile : integer ;
      symbolline : integer ;
      symbolmap : boolean ;
      ctptr : ctp ;
      intptr : ctp ;
      textfilectp : ctp ;
      undecptr : ctp ;
      ival : integer ;
      alfaptr : ctp ;
      boolptr : ctp ;
      charptr : ctp ;
      level : levrange ;
      exportablecode : boolean ;
      interactive : boolean ;


(* FROM GENERE  *)
      cb : integer ;
      indfich : integer ;
      mfari1 : zari ;
      mfari2 : zari ;
      usednameaddr : ctp ;


(* FROM DECLARE *)
      nextalf : ctp ;
      lkc : integer ;


(* FROM STATE   *)
      arrayboundsctp : ctp ;
      divcheck : boolean ;
      inxcheck : boolean ;
      asscheck : boolean ;
      gattr : attr ;
      currentbloc : regpt ;
      inputctp : ctp ;
      maxprused : preg ;
      maxinxused : register ;
      nulpw : setarray ;
      stattrace : levtrace ;
      psrsize : integer ;
      linktomain : boolean ;
      linktomainplace : integer ;
      opaq : ARRAY [typeofop, ra..reaq] OF istand ; (* GIVES INST. WITH A,Q,AQ,EAQ *)
      prinst : ARRAY [typepr, pr1..pr6] OF istand ;
      currentpr : preg ;
      modif : ARRAY [nxreg..rq] OF tag ;
      workformaths : boolean ;
      workformathsplacew : integer ;


$OPTIONS page $

(* REDEFINE IMPORTED PROCEDURES    *)
(* FROM GENERE  *)
    PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ; EXTERNAL ;
    PROCEDURE geneism (fcode : ieism ; ffield : integer ; fbits : zptr) ; EXTERNAL ;
    PROCEDURE gendesca (fareg : preg ; fadr, fcn : integer ; fta : lgcar ;
      fn : integer ; frlgth : mreg) ; EXTERNAL ;
    PROCEDURE gendescb (fareg : preg ; fadr, fc, fb : integer ; fn : integer ;
      frlgth : mreg) ; EXTERNAL ;
    PROCEDURE inser (fcb : integer ; fplace : integer) ; EXTERNAL ;
    FUNCTION enterreftosymbol (ctplace : ctp) : integer ; EXTERNAL ;


(* FROM RACINE  *)
    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE insymbol ; EXTERNAL ;
    PROCEDURE nameisref (p : ctp ; f, l : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    PROCEDURE crealfabox (VAR fkonstbox : ctp) ; EXTERNAL ;
    PROCEDURE srchrec (VAR first : ctp) ; EXTERNAL ;
    PROCEDURE search ; EXTERNAL ;
    PROCEDURE skip (symbcode : integer) ; EXTERNAL ;
    PROCEDURE warning (fno : integer) ; EXTERNAL ;



(* IMPORTED PROCEDURES FROM CONTEXTTABLE *)

    PROCEDURE checkminmax (fvalu : integer ; fctp : ctp ; ferrnum : integer) ; EXTERNAL ;
    PROCEDURE compatbin (typleft, typright : ctp ; VAR fgeneric : ctp) ; EXTERNAL ;
    FUNCTION conformantdim (ff : ctp) : boolean ; EXTERNAL ;
    PROCEDURE create_konst_box (VAR fvbox : ctp ; fname : alfaid ; ftypofconst : consttype) ; EXTERNAL ;
    PROCEDURE findminmax (fctp : ctp ; VAR fmin, fmax : integer) ; EXTERNAL ;
    PROCEDURE warningminmax (fvalu : integer ; fctp : ctp ; ferrnum : integer) ; EXTERNAL ;

(* FROM STATE   *)
    PROCEDURE choicerarq ; EXTERNAL ;
    PROCEDURE enterlcst (VAR fval : setarray ; VAR fboxpt : lcstpt) ; EXTERNAL ;
    PROCEDURE enterllcst (VAR fval : setarray ; VAR fboxpt : llcstpt) ; EXTERNAL ;
    PROCEDURE enterundlab (VAR fundinx : integer) ; EXTERNAL ;
    PROCEDURE transfer (VAR fattr : attr ; inwhat : destination) ; EXTERNAL ;
    PROCEDURE newbloc (freg : register) ; EXTERNAL ;
    PROCEDURE entercst (fval : integer ; VAR fboxpt : wcstpt) ; EXTERNAL ;
    FUNCTION oldnewstor (incrinbytes : integer) : integer ; EXTERNAL ;
    FUNCTION raisused : boolean ; EXTERNAL ;
    PROCEDURE freebloc (VAR fbtofree : regpt) ; EXTERNAL ;
    PROCEDURE loadadr (VAR fattr : attr ; wantedpr : preg) ; EXTERNAL ;
    FUNCTION inbounds (fval, fmin, fmax : integer) : boolean ; EXTERNAL ;
    PROCEDURE regenere (oldbloc : regpt) ; EXTERNAL ;
    PROCEDURE calcvarient (VAR fattr : attr ; VAR fbase : preg ; VAR fdisp : integer ;
      VAR ftag : tag) ; EXTERNAL ;
    PROCEDURE gencompare (VAR fattr : attr ; fcl : integer ; generic : ctp) ; EXTERNAL ;
    PROCEDURE genconcat (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE genptcomp (VAR fattr : attr ; fcl : integer) ; EXTERNAL ;
    PROCEDURE gen_string_comp (VAR fattr : attr ; fcl : integer) ; EXTERNAL ;
    PROCEDURE gen_string_position (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE gen_substring (VAR string_attr, disp_attr, len_attr : attr) ; EXTERNAL ;
    PROCEDURE genstcomp (VAR fattr : attr ; fcl : integer) ; EXTERNAL ;
    PROCEDURE sauvereg (freg : register ; fload : boolean) ; EXTERNAL ;
    PROCEDURE gencstecode (farg : integer ; finst : istand) ; EXTERNAL ;
    PROCEDURE checkbnds (errcode : integer ; freg : register ; fctp : ctp) ; EXTERNAL ;
    PROCEDURE genopadd (VAR fattr : attr ; generic : ctp) ; EXTERNAL ;
    PROCEDURE genopsub (VAR fattr : attr ; generic : ctp) ; EXTERNAL ;
    PROCEDURE genoppw (VAR fattr : attr ; fno, fcl : integer) ; EXTERNAL ;
    PROCEDURE check_dynamic_string_length (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE genandor (VAR fattr : attr ; fno : integer) ; EXTERNAL ;
    PROCEDURE gencheckmultover ; EXTERNAL ;
    PROCEDURE addressvar (fctp : ctp ; VAR fattr : attr ; modif : boolean) ; EXTERNAL ;
    PROCEDURE genopmult (VAR fattr : attr ; generic : ctp) ; EXTERNAL ;
    PROCEDURE genopdivi (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE gendivmod (VAR fattr : attr ; fcl : integer) ; EXTERNAL ;
    PROCEDURE genexceptcode (ferrcode : integer ; freg : register) ; EXTERNAL ;
    PROCEDURE loadbase (flev : integer) ; EXTERNAL ;
    PROCEDURE variab (fvarset : boolean) ; EXTERNAL ;


(* FROM MODATTR *)

    FUNCTION easyvar (VAR fattr : attr) : boolean ; EXTERNAL ;
    FUNCTION is_possible_string (VAR fattr : attr) : boolean ; EXTERNAL ;
    FUNCTION isstring (VAR fattr : attr) : boolean ; EXTERNAL ;
    FUNCTION varissimple (VAR fattr : attr) : boolean ; EXTERNAL ;
    PROCEDURE convreal (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE freeattr (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE initattrvarbl (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE lvalvarbl (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE printattr (VAR fattr : attr) ; EXTERNAL ;

(* FROM MODVARIABLE *)

    PROCEDURE init_desc_address (fctp : ctp ; VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE passparams (fctplace : integer) ; EXTERNAL ;
    PROCEDURE variable (fvarset : boolean) ; EXTERNAL ;


(* ************************   FORWARD   ******************************* *)
    PROCEDURE expression ; FORWARD ;



$OPTIONS page $


(* *****************************************    COMPAREIR     ******** *)

    PROCEDURE compareir ;

(* C   Compilation of CCSUBARR -1 <
   0  =
   1  >
   C *)

      LABEL
        10 ;                                      (* Exit procedure *)

      VAR
        erro, errt, errl : boolean ;
        typelem : ctp ;
        easyo, easyt, easyl : boolean ;
        baseo, baset : preg ;
        dplmtow, dplmttw, dplmtob, dplmttb : integer ;
        temp1, temp2, temp3, temp4 : integer ;
        basebloco, basebloct : regpt ;
        longop : integer ;
        longreg : register ;

      BEGIN                                       (* COMPAREIR *)

$OPTIONS cc = trace + $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ debut COMPAREIR @@@ ') ;
	  nextline ;
	END ;
$OPTIONS cc = trace - $
        erro := true ; errt := true ; errl := true ;
        basebloco := NIL ; basebloct := NIL ;
                                                  (* ORIGIN ANALYSIS *)
        insymbol ;
        variab (false) ;
        WITH gattr DO
	IF typtr <> NIL THEN
	  BEGIN
	    erro := false ;

	    IF varissimple (gattr) THEN
	      BEGIN
	        easyo := true ; baseo := basereg ; dplmtow := dplmt DIV bytesinword ;
	        dplmtob := dplmt MOD bytesinword ;
	      END (* varissimple *) ELSE
	      BEGIN                             (* not easy *)
	        easyo := false ; dplmtow := 0 ; dplmtob := 0 ;
	        loadadr (gattr, nreg) ;
	        baseo := currentpr ; basebloco := currentbloc ;
	      END (* not easy *) ;
	  END (* TYPTR not nil for origin *) ;
        IF no <> 15 THEN
	BEGIN
	  error (20) ; skip (46) ; GOTO 10 ;
	END ;

(* TARGET *)
        insymbol ;
        variab (true) ;
        WITH gattr DO
	IF typtr <> NIL THEN
	  BEGIN
	    errt := false ;
	    IF varissimple (gattr) THEN
	      BEGIN
	        easyt := true ; baset := basereg ; dplmttw := dplmt DIV bytesinword ;
	        dplmttb := dplmt MOD bytesinword ;
	      END ELSE
	      BEGIN                             (* not easy *)
	        easyt := false ; dplmttw := 0 ; dplmttb := 0 ;
	        loadadr (gattr, nreg) ;
	        baset := currentpr ; basebloct := currentbloc ;
	      END (* not easy *) ;
	  END (* TYPTR not nil for target *) ;
        IF no <> 15 (* , *) THEN
	BEGIN
	  error (20) ; skip (46) ; GOTO 10 ;
	END ;
                                                  (* THIRD PARAMETER *)
        insymbol ;
        expression ;
        WITH gattr DO
	IF typtr <> NIL THEN
	  BEGIN
	    IF typtr^.form <> numeric THEN error (15) ELSE
	      BEGIN                             (* NUMERIC *)
	        errl := false ;
	        IF kind = sval THEN
		BEGIN
		  easyl := true ; longop := val ;
		END (* SVAL *) ELSE
		BEGIN                         (* NOT SVAL *)
		  easyl := false ;
		  IF kind <> lval THEN
		    transfer (gattr, inacc) ;
		  longreg := gattr.ldreg ;
		END (* NOT SVAL *) ;

	      END ;                             (* NUMERIC *)
	  END (* typtr not nil for third paramater *) ;
        IF NOT (erro OR errt OR errl) THEN
	BEGIN
	  IF NOT easyo THEN regenere (basebloco) ;
	  IF NOT easyt THEN regenere (basebloct) ;
	  IF easyl THEN
	    BEGIN
	      mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
	      geneism (icmpc, ord (' '), p0t0r0) ;
	      gendesca (baseo, dplmtow, dplmtob, l9, longop, tn) ;
	      gendesca (baset, dplmttw, dplmttb, l9, longop, tn) ;
	    END (* EASYL *) ELSE
	    BEGIN                               (* register loaded with length *)
	      mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
	      geneism (icmpc, ord (' '), p0t0r0) ;
	      gendesca (baseo, dplmtow, dplmtob, l9, 0, modif [longreg]) ;
	      gendesca (baset, dplmttw, dplmttb, l9, 0, modif [longreg]) ;
	    END (* not easy *) ;
	  freebloc (basebloco) ; freebloc (basebloct) ;
	  IF NOT easyl THEN freebloc (gattr.ldregbloc) ;
                                                  (*
                                                     After CMPC   INDICATOR ZERO ON MEANS EQUAL
                                                     INDICATOR CARRY ON MEANS >=
                                                     *)

	  temp3 := indfich ; genstand (nreg, 0, itnz, tic) ;
	  genstand (nreg, 0, ilda, tdl) ;
	  temp4 := indfich ; genstand (nreg, 0, itra, tic) ;
	  inser (cb, temp3) ;
                                                  (* ICI ZERO OFF *)
	  temp1 := indfich ; genstand (nreg, 0, itrc, tic) ;
	  gencstecode (-1, ilda) ;              (* Carry off *)
	  temp2 := indfich ; genstand (nreg, 0, itra, tic) ;
	  inser (cb, temp1) ;
	  genstand (nreg, 1, ilda, tdl) ;
	  inser (cb, temp4) ; inser (cb, temp2) ;
	END ;
        IF no <> 10 THEN
	BEGIN
	  error (4) ; skip (46) ;
	END ;
10 :                                              (* EXIT IF ERRORS *)
$OPTIONS cc = trace + $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ fin COMPAREIR @@@ with NO,CL ', no : 4, cl : 4) ;
	  nextline ;
	END ;
$OPTIONS cc = trace - $

      END (* COMPAREIR *) ;

$OPTIONS page $

(* ******************************** PREDEFFUNCT   ***************************** *)

    PROCEDURE predeffunct ;

(* C This procedure is called only one time in FACTOR for generation and
   analysis of predefined functions.
   Before the call, The first INSYMBOL following the name of the function
   has alresdy been made.
   CTPTR points the box found in CONTEXTTABLE.This box represents

   a PROC PREDEFPROC true PROCTYPE <> REALPTR

   As output, GATTR describes the resulting expression
   GATTR.TYPTR nil if error
   the code for function is generated
   C *)
(* E ERRORS DETECTED
   4 ")" expected
   9 "(" expected
   44  Extension used is SOL, but is not yet implemented
   73  Extension used is neither SOL, neither Standard.
   75  Extension used is  SOL, but not Standard.
   125 Illegal argument for predefined function
   175 INPUT used and not present in program header
   190 Text file expected
   303 Value out of bounds

   E *)

      CONST
        stringiostringaddrplacew = 0 ;
        stringiomaxlplacew = 2 ;
        stringioindexplacew = 3 ;
        stringiovalplacew = 4 ;
        stringiolongplacew = 6 ;
        stringioscaleplacew = 7 ;
        stringiosizeplacew = 7 ;
        stringiosubindexplacew = 8 ;
        stringiostackptrplacew = 10 ;
        stringioworksizew = 12 ;
      VAR
        catfonct : integer ;
        isopbrack : boolean ;
        lattr : attr ;
        lmax : integer ;
        lmin : integer ;
        locerr : integer ;
        operplace : integer ;
        typofarg : ctp ;
        lbase : preg ;
        locskip : integer ;
        locexit : integer ;
        totransfer : boolean ;
        lstor : istand ;
        lerr : boolean ;
        ltag : tag ;
        dummy_bool : boolean ;
        lbloc : regpt ; l_val : integer ;
        ldisp : integer ;
        string_attr, disp_attr, len_attr : attr ;
        linst : istand ;
        locop : integer ;
        lreg : register ;


(* ************************************ SWRITEIR < PREDEFFUNCTION ******************************* *)

      PROCEDURE swriteir ;

(* COMPILES CALL TO SOL PREDEFINED FUNCTION SWRITE
   C *)
(* E ERRORS DETECTED
   4: ")" EXPECTED
   9: "(" EXPECTED
   15: INTEGER EXPECTED
   19: STRING VARIABLE EXPECTED
   20: "," EXPECTED
   144: ILLEGAL TYPE OF EXPRESSION
   191: SCALING FACTOR ONLY FOR REAL
   E *)
        LABEL
	100,
	1 (* EXIT PROC *) ;

        VAR

	deflength : integer ;
	hardlength : boolean ;
	ddisp : integer ;
	locreg : preg ;
	locbox : regpt ;
	errintype : boolean ;
	exprismade : boolean ;
	finloop : boolean ;
	lengthst : integer ;
	linst : istand ;
	typecode : integer ;
	sattr : attr ;
	aisknown : boolean ;
	acont : integer ;
	workplacew : integer ;

        BEGIN                                     (* SWRITEIR *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '^^^ DEBUT SWRITEIR ^^^ ') ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
	typecode := 0 ;
	locbox := NIL ;
	workplacew := oldnewstor (stringioworksizew * bytesinword) DIV bytesinword ;
                                                  (* "(" ALLREADY READ IN PREDEFFUNCT *)
	insymbol ;
	IF no <> 1 THEN
	  BEGIN
	    error (19) ;
	    skip (15) ;
	  END
	ELSE
	  BEGIN
	    variab (true) ;                     (* TARGET STRING *)
	    IF isstring (gattr) THEN
	      IF conformantdim (gattr.typtr) THEN
	        BEGIN
		init_desc_address (gattr.nameaddr, gattr) ;
		regenere (gattr.basebloc) ;
		genstand (pr6, workplacew + stringiostringaddrplacew, prinst [spri, gattr.basereg], tn) ;
                                                  (* COMPUTE SIZE NOW *)
		sauvereg (ra, false) ;

		regenere (gattr.descbloc) ;
		ddisp := 0 ;
		genstand (gattr.descreg, ddisp + 1, ilda, tn) ; (* MAX       *)
		genstand (gattr.descreg, ddisp, isba, tn) ; (*   - MIN   *)
		genstand (nreg, 1, iada, tdl) ; (*    +1     *)
		freeattr (gattr) ;
		genstand (pr6, workplacew + stringiomaxlplacew, ista, tn) ;
	        END ELSE
	        BEGIN
		loadadr (gattr, pr3) ;
		genstand (pr6, workplacew + stringiostringaddrplacew, ispri3, tn) ;
		sauvereg (ra, false) ;
		gencstecode (gattr.typtr^.size, ilda) ;
		genstand (pr6, workplacew + stringiomaxlplacew, ista, tn) ;
	        END
	    ELSE
	      error (19) ;
	  END ;
	IF no <> 15 THEN                        (* "," *)
	  BEGIN error (20) ; skip (15) END
	ELSE insymbol ;
	expression ;                            (* PLACE IN STRING *)
	WITH gattr DO
	  BEGIN
	    IF typtr <> NIL THEN
	      IF typtr^.form <> numeric THEN
	        BEGIN
		error (15) ; skip (15)
	        END
	      ELSE
	        BEGIN
		choicerarq ;
		linst := opaq [stor, ldreg] ;
		freebloc (gattr.ldregbloc) ;
		genstand (pr6, workplacew + stringioindexplacew, linst, tn) ;
	        END ;
	  END ;
	IF no <> 15 THEN                        (* "," *)
	  BEGIN error (20) ; skip (15) END
	ELSE insymbol ;
	REPEAT                                  (* LOOP ON EXPRESSIONS TO BE WRITTEN *)
	  expression ;
	  WITH gattr DO
	    IF typtr <> NIL THEN
	      BEGIN
	        IF typtr^.father_schema = string_ptr THEN
		BEGIN
		  typecode := 256 ;
		  loadadr (gattr, pr3) ;
		  freeattr (gattr) ;
		  genstand (pr3, 0, ilda, tn) ;
		  genstand (pr6, workplacew + stringiosizeplacew, ista, tn) ;
		  genstand (pr3, 1, iepp3, tn) ;
		  genstand (pr6, workplacew + stringiovalplacew, ispri3, tn) ;
		  hardlength := true ;
		  GOTO 100 ;
		END
	        ELSE
		BEGIN
		  linst := inop ;
		  IF typtr^.form <= pointer THEN
		    BEGIN
		      choicerarq ;
		      linst := opaq [stor, ldreg] ;
		      freebloc (gattr.ldregbloc) ;
		    END (* <=POINTER *) ELSE
		    IF typtr^.form < files THEN
		      BEGIN
		        IF NOT conformantdim (gattr.typtr) THEN
			BEGIN
			  loadadr (gattr, pr3) ;
			  linst := ispri3 ;
			END ELSE
			BEGIN
			  init_desc_address (gattr.nameaddr, gattr) ;
			  regenere (gattr.basebloc) ;
			  locbox := gattr.descbloc ;
			  linst := prinst [spri, gattr.basereg] ;
			  sattr := gattr ;
			  freebloc (sattr.basebloc) ;
			END ;
		      END ;
		  IF linst <> inop THEN
		    BEGIN
		      genstand (pr6, workplacew + stringiovalplacew, linst, tn) ;
		    END ;
		END ;
	        errintype := false ;
	        hardlength := false ;
                                                  (* SELECT TYPECODE, *)
                                                  (* LENGTH FOR EACH TYPE *)
	        CASE typtr^.form OF
		reel : BEGIN
		    typecode := 8 ; deflength := deflreal ;
		  END (* REEL *) ;
		numeric : BEGIN
		    typecode := 4 ; deflength := deflnum ;
		  END (* NUMERIC *) ;
		scalar : BEGIN IF typtr^.subrng THEN typtr := typtr^.typset ;
		    IF typtr = boolptr THEN
		      BEGIN typecode := 2 ; deflength := deflbool ;
		      END ELSE
		      IF typtr = charptr THEN
		        BEGIN typecode := 1 ; deflength := deflchar ;
		        END ELSE
		        IF envstandard <> stdextend THEN
			BEGIN
			  errintype := true ;
			END
		        ELSE
			BEGIN
			  typecode := 128 ; deflength := maxident ;
			  genstand (nreg, enterreftosymbol (typtr), ilda, tdl) ;
			  genstand (pr6, workplacew + stringioscaleplacew, ista, tn) ;
			END
		  END (* SCALAR *) ;
		pointer, records, power :
		  errintype := true ;
		files : errintype := true ;
		arrays :
		  BEGIN
		    IF isstring (gattr) THEN
		      BEGIN
		        typecode := 32 ; hardlength := false ;
		        IF typtr = alfaptr THEN
			lengthst := alfactp^.alfalong ELSE
			IF typtr^.conformant THEN
			  hardlength := true ELSE
			  lengthst := typtr^.size ;
		        deflength := lengthst ;
		      END ELSE
		      errintype := true ;
		  END ;
	        END (* CASE TYPTR^.FORM *) ;
	        IF errintype THEN
		BEGIN error (144) ; typecode := 4 ; deflength := deflnum ;
		END ;
	      END                               (* TYPTR  <>  nil, WITH GATTR *)
	    ELSE sattr := gattr ;
	  aisknown := false ;
100 :
	  IF no = 19 (* : *) THEN
	    BEGIN
	      insymbol ; expression ;
	      IF gattr.typtr <> NIL THEN
	        IF gattr.typtr^.form <> numeric THEN error (15) ELSE
		BEGIN
		  transfer (gattr, inacc) ;
		  freebloc (gattr.ldregbloc) ;
		  hardlength := false ;
		  freebloc (locbox) ;
		END ;
	    END ELSE
	    IF sattr.typtr <> NIL THEN
	      IF NOT hardlength THEN
	        BEGIN
		aisknown := true ;
		acont := deflength ;
		gencstecode (deflength, ilda) ;
		IF (typecode = 2) AND (NOT pascalfrench) THEN
		  BEGIN
		    genstand (pr6, workplacew + stringiovalplacew, iszn, tn) ;
		    genstand (nreg, 2, itnz, tic) ;
		    genstand (nreg, 1, iada, tdl) ; (* LENGTH + 1 if "FALSE" *)
		  END
	        END ELSE
	        IF typecode <> 256 THEN
		BEGIN
		  regenere (sattr.descbloc) ;
		  locbox := NIL ;
                                                  (* COMPUTE SIZE NOW *)

		  ddisp := 0 ;
		  genstand (sattr.descreg, ddisp + 1, ilda, tn) ; (* MAX       *)
		  genstand (sattr.descreg, ddisp, isba, tn) ; (*   - MIN   *)
		  genstand (nreg, 1, iada, tdl) ; (*    +1     *)
		  freebloc (sattr.descbloc) ;
		END ;
                                                  (* STORE   LENGTH *)
	  genstand (pr6, workplacew + stringiolongplacew, ista, tn) ;
	  IF no = 19 (* : *) THEN
	    BEGIN
	      IF NOT (typecode IN [8, 32, 256]) (* REAL OR STRING *) THEN error (191) ;
	      typecode := typecode * 2 ;
	      aisknown := false ;
	      insymbol ; expression ;
	      IF gattr.typtr <> NIL THEN
	        IF gattr.typtr^.form <> numeric THEN error (15) ELSE
		BEGIN
		  transfer (gattr, inacc) ;
		  freebloc (gattr.ldregbloc) ;
		  IF typecode = 16 THEN
		    genstand (pr6, workplacew + stringioscaleplacew, ista, tn)
		  ELSE
		    genstand (pr6, workplacew + stringiosubindexplacew, ista, tn) ;
		END ;
	    END ;
	  IF NOT hardlength THEN
	    BEGIN
	      IF (typecode IN [32, 64]) (* CHAINE *) THEN
	        BEGIN
		IF NOT (aisknown AND (acont = lengthst)) THEN
		  BEGIN
		    gencstecode (lengthst, ilda) ;
		    aisknown := true ; acont := lengthst ;
		  END ;
		genstand (pr6, workplacew + stringiosizeplacew, ista, tn) ;
	        END ;

	    END ELSE
	    IF (typecode <> 256) AND (typecode <> 512) THEN
	      BEGIN
	        genstand (pr6, workplacew + stringiosizeplacew, ista, tn) ;
	      END ;
	  sauvereg (pr1, false) ;
	  genstand (pr6, workplacew, iepp1, tn) ;
	  CASE typecode OF
	    0 : ;
	    1 : genstand (pr0, swritecharplace, itsp3, tn) ;
	    2 : genstand (pr0, swritebooleanplace, itsp3, tn) ;
	    4 : genstand (pr0, swriteintegerplace, itsp3, tn) ;
	    8 : genstand (pr0, swriterealeplace, itsp3, tn) ;
	    16 : genstand (pr0, swriterealdplace, itsp3, tn) ;
	    32, 256 : genstand (pr0, swritestringplace, itsp3, tn) ;
	    64, 512 : genstand (pr0, swritesubstringplace, itsp3, tn) ;
	    128 : genstand (pr0, swriteenumplace, itsp3, tn) ;
	  END ;
                                                  (* IS   LOOP   ENDED  OR NOT *)
	  finloop := true ;
	  IF no <> 10 (* ) *) THEN
	    IF no = 15 (*  , *) THEN
	      BEGIN
	        insymbol ; finloop := false ;
	      END ELSE
	      BEGIN
	        error (20) ; skip (15) ;
	        insymbol ; finloop := false ;
	      END ;
	UNTIL finloop ;
                                                  (* LOAD RA WITH INDEX . READY FOR PREDEFFUNCT *)
	genstand (pr6, workplacew + stringioindexplacew, ilda, tn) ;
1 :                                               (* EXIT PROCEDURE *)
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '^^^ FIN SWRITEIR ^^^ WITH NO :', no : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* SWRITEIR *) ;


(* ************************************ SREADIR < PREDEFFUNCTION ******************************** *)

      PROCEDURE sreadir ;

(* C COMPILES CALL TO SOL PREDEFINED FUNCTION SREAD
   (* E ERRORS DETECTED
   4: ")"  EXPECTED
   9: "("  EXPECTED
   15 : NUMERIC TYPE EXPECTED
   19: STRING VARIABLE EXPECTED
   20: ","  EXPECTED
   153: TYPE ERROR IN READ
   E *)
        LABEL
	1 ;                                     (* EXIT OF PROCEDURE *)
        VAR

	finloop : boolean ;
	lattr : attr ;
	lerr : boolean ;
	workplacew : integer ;
	loctype : ctp ;
	typecode : integer ;
	locic : integer ;
	ddisp : integer ;


        BEGIN                                     (* SREADIR *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '^^^ DEBUT SREADIR ^^^ ') ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
	typecode := 0 ;
	workplacew := oldnewstor (stringioworksizew * bytesinword) DIV bytesinword ;
                                                  (* "(" ALLREADY READ IN PREDEFFUNCT *)
	insymbol ;
	IF no <> 1 THEN
	  BEGIN
	    error (19) ;
	    skip (15) ;
	  END
	ELSE
	  BEGIN
	    variab (false) ;                    (* TARGET STRING *)
	    IF isstring (gattr) THEN
	      IF conformantdim (gattr.typtr) THEN
	        BEGIN
		init_desc_address (gattr.nameaddr, gattr) ;
		regenere (gattr.basebloc) ;
		genstand (pr6, workplacew + stringiostringaddrplacew, prinst [spri, gattr.basereg], tn) ;
                                                  (* COMPUTE SIZE NOW *)
		sauvereg (ra, false) ;
		regenere (gattr.descbloc) ;
		ddisp := 0 ;
		genstand (gattr.descreg, ddisp + 1, ilda, tn) ; (* MAX       *)
		genstand (gattr.descreg, ddisp, isba, tn) ; (*   - MIN   *)
		genstand (nreg, 1, iada, tdl) ; (*    +1     *)
		freeattr (gattr) ;
		genstand (pr6, workplacew + stringiomaxlplacew, ista, tn) ;
	        END ELSE
	        BEGIN
		BEGIN
		  loadadr (gattr, pr3) ;
		  genstand (pr6, workplacew + stringiostringaddrplacew, ispri3, tn) ;
		  sauvereg (ra, false) ;
		  gencstecode (gattr.typtr^.size, ilda) ;
		  genstand (pr6, workplacew + stringiomaxlplacew, ista, tn) ;
		END
	        END
	    ELSE
	      error (19) ;
	  END ;
	IF no <> 15 THEN
	  BEGIN error (20) ; skip (15) END
	ELSE insymbol ;
	expression ;                            (* PLACE IN STRING *)
	WITH gattr DO
	  BEGIN
	    IF typtr <> NIL THEN
	      IF typtr^.form <> numeric THEN
	        BEGIN
		error (15) ; skip (15)
	        END
	      ELSE
	        BEGIN
		choicerarq ;
		linst := opaq [stor, ldreg] ;
		freebloc (gattr.ldregbloc) ;
		genstand (pr6, workplacew + stringioindexplacew, linst, tn) ;
	        END ;
	  END ;
	IF no <> 15 THEN
	  BEGIN error (20) ; skip (15) END
	ELSE insymbol ;
	REPEAT
	  variab (true) ;                       (* VARIABLE IS SET HERE *)
	  WITH gattr DO
	    IF typtr <> NIL THEN
	      BEGIN
	        lerr := false ;
	        IF typtr^.form = scalar THEN
		BEGIN
		  IF typtr^.subrng THEN loctype := typtr^.typset ELSE
		    loctype := typtr ;
		  IF loctype <> charptr THEN
		    lerr := true ELSE
		    typecode := 1 ;
		END (* SCALAR *) ELSE
		IF typtr^.form = numeric THEN
		  typecode := 4 ELSE
		  IF typtr = realptr THEN
		    typecode := 8 ELSE
		    lerr := true ;
	        IF lerr THEN
		error (153) ELSE
		BEGIN
                                                  (* SAVE  LOADED  REGISTERS *)
		  IF basereg <= maxprused THEN sauvereg (basereg, false) ;
		  IF inxreg <> nxreg THEN sauvereg (inxreg, false) ;
		  lattr := gattr ;
                                                  (* NOW  CALL  OPERATOR *)
		  genstand (pr6, workplacew, iepp1, tn) ;
		  CASE typecode OF
		    0 : ;
		    1 : genstand (pr0, sreadcharplace, itsp3, tn) ;
		    4 : genstand (pr0, sreadintegerplace, itsp3, tn) ;
		    8 : genstand (pr0, sreadrealplace, itsp3, tn) ;
		  END ;
                                                  (* Genere skip if error detected *)
		  genstand (pr6, workplacew + stringioindexplacew, iszn, tn) ;
		  locic := indfich ;
		  genstand (nreg, 0, itmi, tic) ;
                                                  (* NOW ACC IS LOADED *)
                                                  (* WITH GATTR *)
		  kind := lval ;
		  IF typtr = realptr THEN
		    ldreg := reaq ELSE
		    ldreg := ra ;
		  newbloc (ldreg) ; ldregbloc := currentbloc ;
		  IF asscheck THEN
		    IF typtr <> realptr THEN
		      checkbnds (asserrcode, ra, typtr) ;
		  transfer (lattr, out) ;     (* ASSIGNS *)
		  inser (cb, locic) ;
		END (* NOT LERR *) ;
	      END (* TYPTR  <>  nil,WITH GATTR *) ;
                                                  (* IS LOOP ENDED OR NOT *)
	  finloop := true ;
	  IF no <> 10 (* ) *) THEN
	    IF no = 15 THEN
	      BEGIN
	        insymbol ; finloop := false ;
	      END ELSE
	      BEGIN
	        error (20) ; skip (15) ;
	        insymbol ; finloop := false ;
	      END ;
	UNTIL finloop ;
                                                  (* LOAD RA WITH INDEX . READY FOR PREDEFFUNCT *)
	genstand (pr6, workplacew + stringioindexplacew, ilda, tn) ;
1 :                                               (* EXIT PROCEDURE *)
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '^^^ FIN SREADIR ^^^ WITH NO:', no : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* SREADIR *) ;

      BEGIN                                       (* PREDEFFUNCT *)

$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ Debut DE PREDEFFUNCT ^^^ avec NO =', no : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $


        catfonct := ctptr^.segsize ;
        isopbrack := no = 9 ;                     (* Before call NO and CTPTR set in FACTOR *)
        CASE ctptr^.ploc OF
	instdpure :
	  BEGIN
	    IF NOT isopbrack THEN
	      BEGIN
	        IF NOT (catfonct IN [3, 4]) (* EOF,EOLN       *) THEN
		BEGIN gattr.typtr := NIL ; error (9) ;
		END ELSE
		IF inputctp <> NIL THEN
		  addressvar (inputctp, gattr, false) ELSE
		  BEGIN gattr.typtr := NIL ; error (175) ;
		  END ;
	      END (* NO  <>  9 *) ELSE
	      BEGIN
	        insymbol ; expression ;
	      END ;

	    typofarg := gattr.typtr ;
	    WITH gattr DO
	      IF typofarg <> NIL THEN
	        CASE catfonct OF
		0 :                           (* ODD *)
		  BEGIN
		    IF typofarg^.form <> numeric THEN
		      error (125) ELSE
		      BEGIN
		        IF kind = sval THEN
			BEGIN
			  IF odd (val) THEN
			    transf := 4 ELSE transf := 5 ;
			  accbool := false ; accbloc := NIL ;
			  kind := lcond ;
			END ELSE
			BEGIN
			  transfer (gattr, inacc) ;
			  genstand (nreg, 1, iana, tdl) ;
                                                  (* BOOLEAN IS IN RA *)
			  accbloc := ldregbloc ; accbool := true ;
			  transf := 3 ; kind := lcond ;
			END (* not SVAL *) ;
		      END (* NO ERROR *) ;
		    typtr := boolptr ;
		  END (* ODD *) ;
		1 :                           (* ORD *)
		  BEGIN
		    IF typofarg^.form = scalar THEN
		      BEGIN
		        totransfer := false ;
		        IF kind = lcond THEN totransfer := true ELSE
			IF kind = varbl THEN
			  IF NOT easyvar (gattr) THEN totransfer := true ;
		        IF totransfer THEN
			choicerarq ;
		        typtr := intptr ;
		      END (* SCALAR *) ELSE
		      IF typofarg^.form = pointer THEN
		        BEGIN
			IF envstandard <> stdextend THEN error (125) ;
			transfer (gattr, inacc) ; (* RAQ =FULL ITS *)
			freebloc (ldregbloc) ;
			newbloc (rq) ;
			ldreg := rq ; ldregbloc := currentbloc ;
			genstand (nreg, bitsinhword, iqrl, tn) ;
                                                  (* SHIFT WORD OFFSET *)
			typtr := intptr ;
		        END (* POINTER *) ELSE
		        IF typtr^.form <> numeric THEN
			BEGIN error (125) ; gattr.typtr := NIL ;
			END ;
		  END (* ORD *) ;
		2 :                           (* CHR *)
		  BEGIN
		    IF typofarg^.form <> numeric THEN error (125) ELSE
		      IF kind = sval THEN
		        warningminmax (val, charptr, 303) ELSE
		        IF asscheck THEN
			BEGIN
			  choicerarq ;
			  checkbnds (chrerrcode, ldreg, charptr) ;
			END ;
		    typtr := charptr ;
		  END (* CHR *) ;
		3, 4 :                        (* EOF,EOLN *)
		  BEGIN
		    IF typofarg^.form <> files THEN
		      BEGIN
		        typtr := NIL ; error (125) ;
		      END ELSE
		      BEGIN
		        IF interactive THEN
			IF typofarg = textfilectp THEN
			  BEGIN
			    sauvereg (pr5, false) ;
			    loadadr (gattr, pr5) ;
			    newbloc (pr5) ;
			    WITH gattr DO
			      BEGIN
			        vlev := level ;
			        basereg := pr5 ;
			        basebloc := currentbloc ;
			        dplmt := 0 ;
			        inxreg := nxreg ;
			        inxmem := 0 ;
			        inxmemrw := true ;
			        access := pointee ;
			        itsdplmt := 0 ;
			      END ;
			    IF catfonct = 3 THEN
			      genstand (pr0, checkbeforeeofplace, itsp3, tn)
			    ELSE
			      genstand (pr0, checkbeforeeolnplace, itsp3, tn) ;
			  END ;
		        IF catfonct = 3 (* EOF *) THEN
			BEGIN
			  dplmt := dplmt + eofb ;
			END (* EOF *) ELSE
			BEGIN               (* EOLN *)
			  dplmt := dplmt + eolnb ;
			  IF typofarg <> textfilectp THEN error (190) ;
			END ;
		        typtr := boolptr ;
		        IF asscheck THEN
			BEGIN
			  transfer (gattr, inacc) ;
			  checkbnds (eofeolnerrcode, gattr.ldreg, boolptr) ;
			  IF gattr.ldreg = rq THEN
			    genstand (nreg, 0, iorq, tdl) ELSE
			    genstand (nreg, 0, iora, tdl) ;
                                                  (* RESET BOOLEAN INDICATORS *)
			END ;
		      END (* FILES *) ;
		  END (* EOF,EOLN *) ;
		5 :                           (* ABS *)
		  BEGIN
		    IF typofarg^.form > numeric THEN
		      BEGIN error (125) ; gattr.typtr := NIL ;
		      END ELSE
		      BEGIN                   (* REEL, NUMERIC *)
		        IF typofarg = realptr THEN
			BEGIN
			  linst := ifneg ;
			END ELSE
			BEGIN
			  linst := ineg ;
			END ;
		        transfer (gattr, inacc) ;
		        locskip := indfich ; genstand (nreg, 0, itpl, tic) ;
		        genstand (nreg, 0, linst, tn) ;
		        inser (cb, locskip) ;
		        IF typofarg <> realptr THEN
			typtr := intptr ;
		      END (* NO TYPE ERROR *) ;
		  END (* ABS *) ;
		6, 7 :                        (* TRUNC,ROUND *)
		  BEGIN
		    IF typofarg <> realptr THEN
		      BEGIN
		        typtr := NIL ; error (125) ;
		      END ELSE
		      BEGIN
		        transfer (gattr, inacc) ;
		        IF catfonct = 6 (* TRUNC *) THEN
			operplace := truncplace ELSE
			operplace := roundplace ;
		        genstand (pr0, operplace, itsp3, tn) ; (* RESULT IN RA *)
		        freebloc (ldregbloc) ;
		        newbloc (ra) ;
		        ldregbloc := currentbloc ;
		        ldreg := ra ;
		        typtr := intptr ;
		      END ;
		  END (* TRUNC,ROUND *) ;
		8, 9 :                        (* PRED,SUCC *)
		  BEGIN
		    IF NOT (typofarg^.form IN [numeric, scalar]) THEN
		      BEGIN error (125) ; gattr.typtr := NIL ;
		      END ELSE
		      BEGIN
		        IF catfonct = 8 (* PRED *) THEN
			BEGIN
			  linst := isba ;
			  IF kind = sval THEN
			    BEGIN
			      IF val = -maxint - 1 THEN error (303) ELSE
			        val := val - 1 ;
			    END ;
			END (* PRED *) ELSE
			BEGIN               (* SUCC *)
			  linst := iada ;
			  IF kind = sval THEN
			    IF val = maxint THEN error (303) ELSE
			      val := val + 1 ;
			END (* SUCC *) ;
		        IF kind = sval THEN
			checkminmax (val, typofarg, 303) ELSE
			BEGIN
			  transfer (gattr, inacc) ;
			  IF asscheck THEN
			    BEGIN
			      findminmax (typofarg, lmin, lmax) ;
			      IF catfonct = 8 (* PRED *) THEN
			        BEGIN
				lmin := lmin + 1 ; locerr := prderrcode ;
			        END ELSE
			        BEGIN
				lmax := lmax - 1 ; locerr := sucerrcode ;
			        END ;
			      gencstecode (lmin, icmpa) ;
			      locskip := indfich ;
			      genstand (nreg, 0, itmi, tic) ;
			      gencstecode (lmax, icmpa) ;
			      locexit := indfich ;
			      genstand (nreg, 0, itmoz, tic) ;
			      inser (cb, locskip) ;
			      genexceptcode (locerr, ra) ;
			      inser (cb, locexit) ;
			    END ;
			  genstand (nreg, 1, linst, tdl) ;
			END (* not SVAL *) ;
		      END (* NO TYPERR *) ;
		  END (* PRED,SUCC *) ;
		10 :                          (* SQR *)
		  IF typofarg^.form > numeric THEN
		    BEGIN error (125) ; gattr.typtr := NIL ;
		    END ELSE
		    BEGIN
		      lattr := gattr ;
		      IF typofarg = realptr THEN
		        BEGIN
			linst := idfmp ; lstor := idfst ;
			transfer (gattr, inacc) ;
		        END ELSE
		        BEGIN
			linst := impy ; lstor := istq ;
			transfer (gattr, inq) ;
			sauvereg (ra, false) ;
			typofarg := intptr ;
		        END ;
		      IF NOT varissimple (gattr) THEN
		        BEGIN
			genstand (pr6, evareaw, lstor, tn) ;
			genstand (pr6, evareaw, linst, tn) ;
		        END (* not EASY *) ELSE
		        BEGIN
			calcvarient (lattr, lbase, ldisp, ltag) ;
			WITH lattr DO
			  IF kind = varbl THEN usednameaddr := nameaddr ;
			genstand (lbase, ldisp, linst, ltag) ;
		        END ;
		      IF linst = impy THEN
		        IF asscheck THEN gencheckmultover ;
		      typtr := typofarg ;
		    END (* NO ERROR IN SQR *) ;
	        END (* CASE CATFONCT *) ;
	  END (* INSTDPURE *) ;
	instdsol :
	  BEGIN
	    IF NOT isopbrack THEN
	      BEGIN
	        IF NOT (catfonct IN [0, 1, 2, 6]) (* FSIZE,FPOS,FLLENGTH,ARGC *) THEN
		BEGIN gattr.typtr := NIL ; error (9) ;
		END ELSE
		IF catfonct IN [6] THEN
		  BEGIN
		    gattr.typtr := intptr ;
		  END ELSE
		  IF inputctp <> NIL THEN
		    addressvar (inputctp, gattr, false) ELSE
		    BEGIN gattr.typtr := NIL ; error (175) ;
		    END ;
	      END (* NO  <>  9 *) ELSE
	      IF NOT (catfonct IN [4, 5]) THEN
	        BEGIN
		insymbol ; expression ;
	        END ;

	    IF envstandard = stdpure THEN
	      error (75) ;
	    typofarg := gattr.typtr ;
	    WITH gattr DO
	      IF typofarg <> NIL THEN
	        CASE catfonct OF
		0, 1, 2 :                     (* FSIZE,FPOS,FLLENTGH *)
		  BEGIN
		    IF typofarg^.form <> files THEN
		      error (125) ELSE
		      BEGIN
		        IF catfonct = 0 (* FSIZE *) THEN
			BEGIN
			  dplmt := dplmt + fsizeb ;
			END (* FSIZE *) ELSE
			IF catfonct = 1 (* FPOS *) THEN
			  BEGIN
			    dplmt := dplmt + fposb ;
			  END (* FPOS *) ELSE
			  BEGIN             (* FLLENGTH *)
			    dplmt := dplmt + fllengthb ;
			    IF typofarg <> textfilectp THEN error (190) ;
			  END ;
		      END (* FILES *) ;
		    typtr := intptr ;
		  END (* FSIZE,FPOS,FLLENGTH *) ;
		3 :                           (* FSTATUS  *)
		  BEGIN
		    error (44) ;
		    typtr := intptr ;
		  END (* FSTATUS  *) ;
		4, 5 :                        (* SREAD, SWRITE *)
		  BEGIN
		    FOR lreg := pr1 TO maxprused DO sauvereg (lreg, false) ;
		    FOR lreg := x0 TO maxinxused DO sauvereg (lreg, false) ;
		    FOR lreg := ra TO reaq DO sauvereg (lreg, false) ;
		    IF catfonct = 4 THEN sreadir ELSE swriteir ;
		    kind := lval ;
		    newbloc (ra) ;
		    ldregbloc := currentbloc ;
		    psrsize := 0 ;
		    ldreg := ra ;
		    typtr := intptr ;
		  END (* SREAD,SWRITE *) ;
		6 :                           (* ARGC     *)
		  BEGIN
		    IF level = 0 THEN
		      locop := argcshortplace ELSE
		      BEGIN
		        IF NOT exportablecode THEN
			BEGIN
			  loadbase (0) ;
			  IF currentpr <> pr1 THEN
			    genstand (currentpr, 0, iepp1, tn) ;
                                                  (* PR1 points MAIN stack frame   *)
			  freebloc (currentbloc) ;
			  locop := argcplace ;
			END ELSE
			BEGIN
			  IF NOT linktomain THEN
			    BEGIN
			      linktomainplace := lkc ;
			      lkc := lkc + bytesindword ;
			      linktomain := true ;
			    END ;
			  genstand (prlink, linktomainplace DIV bytesinword, iepp1, tny) ;
                                                  (* PR1 points MAIN entry point *)
			  locop := argcextplace ;
			END (* EXPORTABLE *) ;

		      END ;                   (* OPERATOR SELECTION *)

		    genstand (pr0, locop, itsp3, tn) ;

(* At return RA is loaded with the number of arguments *)
		    WITH gattr DO
		      BEGIN
		        kind := lval ;
		        newbloc (ra) ;
		        ldregbloc := currentbloc ; ldreg := ra ;
		        typtr := intptr ;
		      END ;
		  END (* ARGC     *) ;
	        END (* CASE CATFONCT *) ;
	  END (* INSTDSOL *) ;
	instdextend :
	  BEGIN
	    IF NOT isopbrack THEN
	      BEGIN
	        IF catfonct <> 0 (* CLOCK *) THEN
		BEGIN error (9) ; gattr.typtr := NIL ;
		END ELSE
		gattr.typtr := realptr ;
	      END (* NO  <>  9 *) ELSE
	      BEGIN
	        IF NOT (catfonct = 2) THEN
		BEGIN
		  insymbol ; expression ;
		END ;
	      END ;

	    IF envstandard <> stdextend THEN
	      error (73) ;
	    typofarg := gattr.typtr ;
	    WITH gattr DO
	      IF typofarg <> NIL THEN
	        CASE catfonct OF
		0 :                           (* CLOCK *)
		  WITH gattr DO
		    BEGIN
		      typtr := realptr ; kind := lval ;
		      ldreg := reaq ; sauvereg (reaq, true) ;
		      ldregbloc := currentbloc ;
		      genstand (pr0, clockopplace, itsp3, tn) ;
                                                  (* NOW FLOAT REGISTER IS LOADED *)
                                                  (* WITH NUMBER OF MICSEC *)
		    END (* with GATTR, CLOCK *) ;
		1 :                           (* CVPTRINT *)
		  BEGIN
		    IF typofarg^.form = pointer THEN
		      BEGIN
		        transfer (gattr, inacc) ; (* RAQ =FULL ITS *)
		        freebloc (ldregbloc) ;
		        newbloc (rq) ; ldreg := rq ;
		        ldregbloc := currentbloc ;
		        genstand (nreg, bitsinhword, iqrl, tn) ;
                                                  (* SHIFT WORD OFFSET *)
		        typtr := intptr ;
		      END (* POINTER *) ELSE
		      BEGIN error (125) ; gattr.typtr := NIL ;
		      END ;
		  END (* CVPTRINT *) ;
		2 :                           (* CCSUBARR *)
		  WITH gattr DO
		    BEGIN
		      FOR lreg := pr1 TO maxprused DO sauvereg (lreg, false) ;
		      FOR lreg := x0 TO maxinxused DO sauvereg (lreg, false) ;
		      FOR lreg := ra TO reaq DO sauvereg (lreg, false) ;
		      compareir ;
		      kind := lval ;
		      newbloc (ra) ;
		      ldregbloc := currentbloc ;
		      psrsize := 0 ;
		      ldreg := ra ;
		      typtr := intptr ;
		    END (* with GATTR, CCSUBARR *) ;
		3 :                           (* LENGTH *)
		  BEGIN
		    IF (NOT is_possible_string (gattr)) OR (typtr = NIL) THEN
		      BEGIN
		        error (274) ; freeattr (gattr) ;
		        kind := sval ; val := 0 ;
		      END
		    ELSE BEGIN
		        IF typtr = charptr THEN
			BEGIN
			  freeattr (gattr) ;
			  kind := sval ; val := 1 ;
			END ELSE
			IF isstring (gattr) THEN
			  IF conformantdim (typtr) THEN
			    BEGIN
			      init_desc_address (nameaddr, gattr) ;
			      regenere (descbloc) ;
			      sauvereg (rq, true) ; lbloc := currentbloc ;
			      genstand (descreg, 1, ildq, tn) ;
			      genstand (descreg, 0, isbq, tn) ;
			      genstand (nreg, 1, iadq, tdl) ;
			      freebloc (descbloc) ;
			      freeattr (gattr) ;
			      ldreg := rq ; ldregbloc := lbloc ;
			      kind := lval ;
			    END
			  ELSE
			    BEGIN
			      IF kind = chain THEN
			        l_val := alfactp^.alfalong
			      ELSE
			        l_val := typtr^.size ;
			      freeattr (gattr) ;
			      kind := sval ; val := l_val ;
			    END
			ELSE IF typtr^.father_schema = string_ptr THEN
			    BEGIN
			    END ;
		      END ;
		    typtr := intptr ;
		  END ;
		4 :                           (* MAXLENGTH *)
		  BEGIN
		    IF NOT (gattr.kind = varbl) THEN error (275) ;
		    IF NOT (gattr.typtr^.father_schema = string_ptr) THEN error (275) ; (* STRING VARIABLE EXPECTED *)
		    typofarg := gattr.typtr ;
		    freeattr (gattr) ;
		    WITH gattr DO
		      BEGIN
		        typtr := intptr ;
		        IF typofarg^.actual_parameter_list = NIL THEN
			WITH gattr DO
			  BEGIN
			    kind := sval ; val := 0 ;
			  END
		        ELSE
			WITH typofarg^.actual_parameter_list^ DO
			  IF klass = konst THEN
			    WITH gattr DO
			      BEGIN
			        kind := sval ; val := values ;
			      END
			  ELSE addressvar (actual_parameter_list, gattr, false) ;
		      END ;
		  END ;
		5 :                           (* POSITION *)
		  BEGIN
		    lerr := false ;
		    IF NOT is_possible_string (gattr) THEN
		      BEGIN error (274) ; lerr := true END ;
		    IF no <> 15 THEN
		      BEGIN error (20) ; lerr := true END
		    ELSE insymbol ;
		    lattr := gattr ;
		    expression ;
		    IF NOT is_possible_string (gattr) THEN
		      BEGIN error (274) ; lerr := true END ;
		    IF NOT lerr THEN gen_string_position (lattr)
		    ELSE BEGIN
		        freeattr (gattr) ; freeattr (lattr) ;
		        WITH gattr DO
			BEGIN
			  typtr := intptr ; kind := sval ; val := 0 ;
			END ;
		      END ;
		  END ;
		6 :                           (* SUBSTR *)
		  BEGIN
		    lerr := false ;
		    IF NOT is_possible_string (gattr) THEN
		      BEGIN error (274) ; lerr := true END ;
		    string_attr := gattr ;
		    IF no <> 15 THEN
		      BEGIN error (20) ; lerr := true END
		    ELSE insymbol ;
		    expression ;
		    IF gattr.typtr = NIL THEN lerr := true
		    ELSE IF gattr.typtr^.form <> numeric THEN
		        BEGIN
			lerr := true ; error (15)
		        END ;
		    disp_attr := gattr ;
		    IF no <> 15 THEN
		      BEGIN error (20) ; lerr := true END
		    ELSE insymbol ;
		    expression ;
		    IF gattr.typtr = NIL THEN lerr := true
		    ELSE IF gattr.typtr^.form <> numeric THEN
		        BEGIN
			lerr := true ; error (15)
		        END ;
		    len_attr := gattr ;
		    IF NOT lerr THEN gen_substring (string_attr, disp_attr, len_attr)
		    ELSE BEGIN
		        freeattr (string_attr) ; freeattr (disp_attr) ; freeattr (len_attr) ;
		        WITH gattr DO
			BEGIN
			  typtr := charptr ; kind := sval ; val := ord (' ') ;
			END ;
		      END ;
		  END ;
	        END (* CASE CATFONCT *) ;
	  END (* INSTDEXTEND *) ;
        END (* case CTPTR^.PLOC *) ;

        IF isopbrack THEN
	IF no = 10 (* ) *) THEN
	  insymbol ELSE
	  BEGIN error (4) ; gattr.typtr := NIL ;
	  END ;

$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '^^^ Fin de PREDEFFUNCT ^^^ avec CATFONCT=', catfonct : 6) ;
	  nextline ;
	END ;
$OPTIONS compile = true $

      END (* PREDEFFUNCT *) ;

$OPTIONS page $

(* ************************************ ELEMENT         ********************** *)

    PROCEDURE element (VAR fattr : attr ; VAR fvsetelctp : ctp ; VAR fvlpsval : setarray ; VAR fvmax, fvmin : integer ; VAR fmaxallow : integer) ;

(* C .ANALYSES  OF  AN ELEMENT  IN  A  SET EXPRESSION  [ .....,  ,...]
   * EITHER  X
   * EITHER  X..Y
   .CONSTANT PART  IS  COMPUTED  IN  FVLPSVAL
   .AS RESULT .(FATTR   <----------   ) ,   LVAL   IN  RAQ, or PSR
   SVAL   8  or  MAX
   . FVMAX  IS  MAX CSTE FOUND  if  KIND  IS SVAL
   . FVMIN same for minimum value
   . FVSETELCTP  POINTS  GENERIC TYPE  OF ELEMENTS
   .FMAXALLOW PROPAGATES FROM CALL TO CALL MAX VALUE FOR ELEMENT
   C *)
(* E ERRORS DETECTED
   1: SCALAR or NUMERIC EXPECTED
   102: LOW BOUND MUST not EXCEED HIGH BOUND
   129: TYPE CONFLICT
   305: SET ELEMENT   OUT OF BOUNDS
   E *)
      LABEL
        1 ;
      VAR
        generic : ctp ;
        infattr : attr ;
        infissval : boolean ;
        infval : integer ;
        it : integer ;
        ldisp : integer ;
        lerr : boolean ;
        locexit : integer ;
        lload : istand ;
        ltag : tag ;
        stag : tag ;
        stpospr : register ;
        toloadq : boolean ;

(* ************************************ INITPSR < ELEMENT         ************* *)
      PROCEDURE initpsr ;
        BEGIN                                     (* INITPSR *)
	IF fattr.kind = sval THEN
	  BEGIN
                                                  (* FIRST ITEM VARIABLE *)
	    sauvereg (psr, true) ;
	    fattr.kind := lval ; fattr.ldreg := psr ; fattr.ldregbloc := currentbloc ;
	    IF fmaxallow >= bitsindword THEN
	      psrsize := bytesforset ELSE psrsize := bytesindword ;
                                                  (* INIT ZONE  with  "000...0" *)
	    mfari1 := a0r0i0 ; mfari2 := a1r0i0 ;
	    geneism (imlr, 0 (* PADDING *), p0t0r0) ;
	    gendesca (nreg, 0, 0, l9, 0, tn) ;
	    gendesca (pr6, psrdepw, 0, l9, bytesforset, tn) ;
	  END (* INIT PSR *) ELSE
	  regenere (fattr.ldregbloc) ;
        END (* INITPSR *) ;

      BEGIN                                       (* ELEMENT  *)
        lerr := true ;
1 :     expression ;
        IF gattr.typtr = NIL THEN
	BEGIN
	  IF no IN [15, 39] THEN                (* ,  .. *)
	    BEGIN
	      insymbol ; GOTO 1 ;
	    END ;
	END ;
        IF gattr.typtr <> NIL THEN
	IF fvsetelctp = NIL THEN
	  BEGIN                                 (* FIRST ITEM WITHOUT ERROR *)
	    WITH gattr.typtr^ DO
	      IF form = numeric THEN
	        BEGIN
		fvsetelctp := intptr ; fmaxallow := bitsforset - 1 ; lerr := false ;
	        END (* NUMERIC *) ELSE
	        IF form = scalar THEN
		BEGIN
		  IF subrng THEN
		    fvsetelctp := typset ELSE
		    fvsetelctp := gattr.typtr ;
		  fmaxallow := fvsetelctp^.fconst^.values ; lerr := false ;
		END ELSE
		error (1) ;
	  END (* FIRST ITEM *) ELSE
	  BEGIN
	    compatbin (fvsetelctp, gattr.typtr, generic) ;
	    IF (generic = NIL) OR (generic = realptr) THEN
	      error (129) ELSE lerr := false ;
	  END ;                                 (* end ALSO  for GATTR.TYPTR <> nil *)
        arrayboundsctp^.nmin := 0 ; arrayboundsctp^.nmax := fmaxallow ;
        IF (fvsetelctp <> NIL) AND (NOT lerr) THEN
	BEGIN
	  WITH gattr DO
	    IF kind = sval THEN
	      BEGIN
	        infissval := true ; infval := 0 ;
	        IF (val < 0) OR (val > fmaxallow) THEN
		error (305) ELSE
		BEGIN infval := val ;
		  IF val < fvmin THEN fvmin := val ;
		END ;
	      END ELSE
	      BEGIN
	        infissval := false ;
	        transfer (gattr, inq) ;
	        infattr := gattr ;
	      END (* not SVAL, with GATTR *) ;
	  IF no <> 39 (* .. *) THEN
	    BEGIN
	      IF infissval THEN
	        BEGIN
		insert_ (1, (bitsinword - 1) - (infval MOD bitsinword),
		  fvlpsval [infval DIV bitsinword]) ;
		IF infval > fvmax THEN fvmax := infval ;
	        END (* SVAL *) ELSE
	        BEGIN                           (* LVAL *)
		initpsr ;
		IF inxcheck THEN
		  checkbnds (seterrcode, rq, arrayboundsctp) ;
		sauvereg (ra, false) ;
		genstand (nreg, bitsinword, idiv, tdl) ; (* RA BIT DISP, RQ WORD DISP *)
		genstand (nreg, 0, ieax7, tal) ;
		genstand (nreg, -twoto17, ilda, tdu) ; (* 10000....  00 *)
		genstand (nreg, 0, iarl, tx7) ; genstand (pr6, psrdepw, iorsa, tql) ;
		freebloc (infattr.ldregbloc) ;
	        END (* LVAL *) ;
	    END (* NO <> 39  .. *) ELSE
	    BEGIN                               (* NO=39 *)
	      insymbol ; expression ;
	      WITH gattr DO
	        IF typtr <> NIL THEN
		BEGIN
		  compatbin (fvsetelctp, typtr, generic) ;
		  IF (generic = NIL) OR (generic = realptr) THEN
		    error (129) ELSE
		    BEGIN
		      IF infissval THEN
		        BEGIN
			IF kind = sval THEN
			  BEGIN             (* CST1..CST2 *)
			    IF val < infval THEN
			      warning (102) ELSE
			      BEGIN
			        IF (val < 0) OR (val > fmaxallow) THEN error (305) ELSE
				FOR it := infval TO val DO
				  insert_ (1, (bitsinword - 1) - (it MOD bitsinword),
				    fvlpsval [it DIV bitsinword]) ;
			        IF val > fvmax THEN
				IF val <= fmaxallow THEN fvmax := val ;
			      END ;
			  END (* CST1..CST2 *) ELSE
			  BEGIN             (* CST1..EXP2 *)
			    IF kind <> lval THEN
			      transfer (gattr, inacc) ;
			    IF ldreg = ra THEN
			      BEGIN
			        ltag := tal ; stag := tql ; stpospr := rq ; lload := ildq ;
			      END ELSE
			      BEGIN
			        ltag := tql ; stag := tal ; stpospr := ra ; lload := ilda ;
			      END ;
			    IF inxcheck THEN
			      checkbnds (seterrcode, ldreg, arrayboundsctp) ;
			    IF infval = 0 THEN
			      genstand (nreg, 1, opaq [add, ldreg], tdl)
			    ELSE
			      genstand (nreg, infval - 1, opaq [sub, ldreg], tdl) ;
                                                  (* LDREG  NOW IS  LENGTH IN BITS *)
			    locexit := indfich ; genstand (nreg, 0, itmoz, tic) ;
                                                  (* NO OP if LENGTH <=0 *)
			    sauvereg (stpospr, false) ;
			    genstand (nreg, infval, lload, tdl) ; (* STARTING BIT *)
			    initpsr ;
			    genstand (pr6, psrdepw, iepp3, tn) ;
			    genstand (pr3, 0, iabd, stag) ;
			    mfari1 := a0r0i0 ; (* DUMMY *) mfari2 := a1r1i0 ; (* TARGET *)
			    geneism (icsl, 15 (* 1111=MOVE 1 *), p1t0r0) ;
			    gendescb (nreg, 0, 0, 0, 0, tn) ;
			    gendescb (pr3, 0, 0, 0, 0, ltag) ;
			    inser (cb, locexit) ;
			    freebloc (ldregbloc) ;
			  END (* CST1..EXP2 *) ;
		        END (* INFISSVAL *) ELSE
		        BEGIN                 (* INF  IS or WAS IN RQ *)
			initpsr ;
			transfer (gattr, inacc) ;
			IF inxcheck THEN
			  checkbnds (seterrcode, ra, arrayboundsctp) ;
			IF infattr.ldregbloc^.saveplace = 0 THEN
			  BEGIN
			    toloadq := false ; sauvereg (rq, false) ;
			  END ELSE
			  toloadq := true ;
			ldisp := infattr.ldregbloc^.saveplace DIV bytesinword ;
			genstand (pr6, ldisp, isba, tn) ;
			genstand (nreg, 1, iada, tdl) ; (* LENGTH = SUP-INF+1 *)
			locexit := indfich ; genstand (nreg, 0, itmoz, tic) ;
			IF toloadq THEN
			  genstand (pr6, ldisp, ildq, tn) ; (* STARTING BIT *)
			mfari1 := a0r0i0 ; mfari2 := a1r1i0 ;
			genstand (pr6, psrdepw, iepp3, tn) ;
			genstand (pr3, 0, iabd, tql) ;
			geneism (icsl, 15 (* BOLR=IIII =MOVE 1 *), p1t0r0) ;
			gendescb (nreg, 0, 0, 0, 0, tn) ;
			gendescb (pr3, 0, 0, 0, 0, tal) ;
			inser (cb, locexit) ;
			freebloc (ldregbloc) ;
			freebloc (infattr.ldregbloc) ;
		        END (* INF WAS IN RQ *) ;
		    END (* NO ERROR *) ;
		END (* TYPTR  <>  nil, with GATTR *) ;
	    END (* NO=39 *) ;
	END (* OK for  FVSETELCTP, LERR *) ELSE
	IF NOT (no IN [15, 12]) (* ,  ] *) THEN
	  insymbol ;
      END (* ELEMENT *) ;

(* PAGE *)
(* *********************************** FACTOR ********************************* *)

    PROCEDURE factor ;

(* C .BUILD A GATTR   FOR SEVERAL ITEMS
   .FOLLOWING CASES
   IDENT  KONST     ==> GATTR
   VARS,FIELD==> VARIABLE
   PROC      ==> GATTR
   PASSPARAMS
   CONST            ==> GATTR
   nil              ==>   "
   not FACTOR             "
   (  EXPRESSION )      EXPRESSION
   [  EXPR (, EXPR)* ]    "
   C *)
(* E ERRORS DETECTED
   4: ")" EXPECTED
   9: "(" EXPECTED
   12: "]" EXPECTED
   58: ILLEGAL BEGINNING SYMBOL FOR A FACTOR
   73  Extension used is neither SOL, neither Standard.
   103: IDENTIFIER IS not OF APPROPRIATE CLASS
   104: UNDECLARED ID.
   125: ILLEGAL ARGUMENT TYPE FOR A STANDARD FUNCTION
   135: TYPE OF OPERAND MUST BE BOOLEAN
   187: procedure USED AS A FUNCTION
   E *)
      VAR
        catfonct : integer ;
        equal : boolean ;
        it : integer ;
        lattr : attr ;
        lretpt : lcstpt ;
        llretpt : llcstpt ;
        lmaxallow : integer ;
        lmaxcst : integer ;
        lmincst : integer ;
        longop : integer ;
        lp : ctp ;
        lpsval : setarray ;
        lreg : register ;
        ltemp : integer ;
$OPTIONS compile = trace $
        newattr : boolean ;
$OPTIONS compile = true $
        setelctp : ctp ;
        wretpt : wcstpt ;

(* ***********************************   FACTERR < FACTOR   *************** *)

      PROCEDURE facterr (errnum : integer) ;
        BEGIN
	error (errnum) ; gattr.typtr := NIL ;
        END (* FACTERR *) ;



      BEGIN                                       (* FACTOR *)
$OPTIONS compile = trace $
        newattr := true ;
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT FACTOR ^^^ with NO:', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
        IF no = 1 (* IDENTIFIER *) THEN
	BEGIN
	  IF declarationpart THEN
	    BEGIN
	      srchrec (next) ;
	      IF ctptr = NIL THEN search
	    END
	  ELSE
	    search ;
	  IF ctptr = NIL THEN
	    BEGIN
	      error (104) ; ctptr := undecptr ;
	    END (* nil *) ;
	  CASE ctptr^.klass OF
	    schema, types :
	      BEGIN
	        IF symbolmap THEN
		nameisref (ctptr, symbolfile, symbolline) ;
	        error (103) ; gattr.typtr := NIL ; insymbol ;
	      END (* TYPES *) ;
	    konst :
	      BEGIN
	        IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
	        WITH gattr, ctptr^ DO
		BEGIN
		  typtr := contype ;
		  IF typtr = alfaptr THEN
		    BEGIN                     (* CHAIN  CONSTANT *)
		      kind := chain ; alfactp := ctptr ;
		      IF (NOT declarationpart) AND (succ = ctptr) THEN
		        BEGIN                 (* not YET USED *)
			succ := nextalf ; nextalf := ctptr ;
		        END (* not YET USED *) ;
		    END (* CHAIN *) ELSE
		    BEGIN
		      kind := sval ;
		      IF typtr = realptr THEN
		        rsval := valreel ELSE
		        val := values ;
		    END (* not ALFAPTR *) ;
		  insymbol ;
		END (* with GATTR,........ *) ;
	      END (* KONST *) ;
	    vars, field :
	      BEGIN
	        variable (false) ;
	        IF gattr.typtr <> NIL THEN
		IF asscheck THEN
		  IF gattr.typtr^.father_schema = string_ptr THEN
		    IF gattr.typtr^.actual_parameter_list <> NIL THEN
		      check_dynamic_string_length (gattr) ;
$OPTIONS compile = trace $
	        newattr := false ;
$OPTIONS compile = true $
	      END (* VARS,FIELD *) ;
	    proc : BEGIN
	        IF symbolmap THEN
		nameisref (ctptr, symbolfile, symbolline) ;
	        insymbol ;
	        IF ctptr^.proctype = NIL THEN
		gattr.typtr := NIL ELSE
		IF ctptr^.proctype = ctptr THEN
		  facterr (187) ELSE
		  WITH ctptr^ DO
		    IF predefproc THEN

(* PREDEFINED  FUNCTIONS AND SCIENTIFIC  SUBROUTINES *************** *)
		      BEGIN
		        IF proctype <> realptr THEN
			BEGIN
                                                  (* REALPTR AS PROCTYPE  FOR SCIENTIFIC, *)
                                                  (* NILPTR  FOR  OTHERS *)

			  predeffunct ;

			END (* PROCTYPE  <> REALPTR =PREDEFINED FUNCTIONS *) ELSE
			BEGIN               (* SCIENTIFIC  FUNCTIONS *)
			  IF no <> 9 (* "("  *) THEN
			    facterr (9) ELSE
			    BEGIN
			      catfonct := segsize ;
			      IF catfonct = log10switch THEN
			        IF envstandard <> stdextend THEN
				error (73) ;
			      insymbol ; expression ;
			      IF gattr.typtr <> NIL THEN
			        IF gattr.typtr^.form > numeric THEN
				error (125) ELSE
				BEGIN
				  IF gattr.typtr^.form = numeric THEN
				    convreal (gattr) ;
				  transfer (gattr, inacc) ;
				  IF NOT workformaths THEN
				    BEGIN
				      workformathsplacew := (oldnewstor (mathworksizew * bytesinword)) DIV bytesinword ;
				      workformaths := true ;
				    END ;
				  sauvereg (pr2, false) ;
				  genstand (pr6, workformathsplacew, iepp2, tn) ;
				  genstand (pr0, scientplace + catfonct, itsp3, tn) ;
				END ;
			      gattr.typtr := realptr ;
			      IF no = 10 THEN (* ) *)
			        insymbol ELSE
			        facterr (4) ;
			    END (* NO  WAS  9 *) ;
			END                 (* SCIENTIFIC SUBROUTINE *)
		      END (* PREDEFPROC *) ELSE
		      BEGIN                   (* PROGRAMMER  FUNCTION *)
		        ltemp := oldnewstor (bytesindword) ;
		        WITH lattr DO
			BEGIN
			  typtr := proctype ;
			  IF (NOT exportablecode) AND (prockind < formal) THEN
			    BEGIN
			      kind := lval ;
			      IF typtr = realptr THEN
			        BEGIN
				ldreg := reaq ;
			        END ELSE
			        IF typtr^.form = pointer THEN
				BEGIN
				  ldreg := raq ;
				END ELSE
				BEGIN
				  ldreg := ra ;
				END ;
                                                  (* LDREGBLOC LATER AFTER PASSPARAMS *)
			    END (* PASCAL *) ELSE
			    BEGIN           (* not PASCAL *)
			      initattrvarbl (lattr) ;
			      dplmt := ltemp ;
			    END (* not PASCAL *) ;
			END (* with LATTR *) ;
                                                  (* SAVE ALL PREVIOUS LOADED REGISTERS *)
		        FOR lreg := pr1 TO maxprused DO sauvereg (lreg, false) ;
		        FOR lreg := x0 TO maxinxused DO sauvereg (lreg, false) ;
		        FOR lreg := ra TO reaq DO sauvereg (lreg, false) ;
                                                  (* ****************************** *)
		        passparams (ltemp) ;
                                                  (* ***************************** *)
                                                  (* RETURN CODE   *)
                                                  (* LOAD  RA,RAQ OR REAQ  *)
                                                  (* AND ASSIGNS PRG|LTEMP *)
		        WITH lattr DO
			IF kind = lval THEN
			  BEGIN             (* PASC FUNCTION LOCAL *)
			    newbloc (ldreg) ;
			    ldregbloc := currentbloc ;
			  END ;
		        gattr := lattr ;
		      END                     (* PROGRAMMER FUNCTION , with  CTPTR^ *)
	      END (* PROC *) ;
	  END (* CASE CTPTR^.KLASS *) ;
	END (* NO=1 IDENTIFIER *) ELSE
	IF no = 2 (* EXPLICIT  CONSTANT *) THEN
	  BEGIN
	    WITH gattr DO
	      CASE cl OF
	        1 :                             (* INTEGER *)
		BEGIN kind := sval ; typtr := intptr ; val := ival ;
		END (* 1 *) ;
	        2 :                             (* REAL *)
		BEGIN kind := sval ; typtr := realptr ; rsval := rval ;
		END (* 2 *) ;
	        3 :                             (* ALFA *)
		BEGIN kind := chain ; typtr := alfaptr ;
		  longstring := longchaine ;
		  create_konst_box (lp, blank, alfaconst) ;
		  lp^.contype := alfaptr ;
		  IF NOT declarationpart THEN
		    BEGIN
		      lp^.succ := nextalf ;
		      nextalf := lp ;
		    END ;
		  crealfabox (lp) ;
		  alfactp := lp ;
		END (* 3 *) ;
	        4 :                             (* CHAR *)
		BEGIN
		  kind := sval ; typtr := charptr ; val := ival ;
		END (* 4 *) ;
	      END (* CASE CL,with GATTR *) ;
	    insymbol ;
	  END (* NO=2 *) ELSE
	  IF no = 36 (* nil *) THEN
	    BEGIN
	      WITH gattr DO
	        BEGIN
		kind := sval ; typtr := nilptr ; val := 0 ; (* DUMMY HERE NILLEFT,NILRIGHT *)
		IF symbolmap THEN nameisref (nilptr, symbolfile, symbolline) ;
	        END ;
	      insymbol ;
	    END (* NO=36 *) ELSE
	    IF no = 9 (* ( *) THEN
	      BEGIN
	        insymbol ; expression ;
$OPTIONS compile = trace $
	        newattr := false ;
$OPTIONS compile = true $
	        IF no = 10 (* ) *) THEN
		insymbol ELSE
		facterr (4) ;
	      END (* NO= 9 *) ELSE
	      IF no = 5 (* not *) THEN
	        BEGIN
		insymbol ; factor ;
		WITH gattr DO
		  IF typtr <> boolptr THEN
		    facterr (135) ELSE
		    BEGIN
		      CASE kind OF
		        lcond : CASE transf OF
			  1 : BEGIN transf := 9 ; freebloc (accbloc) ; accbool := false ;
			    END ;
			  2 : transf := 6 ;
			  3 : transf := 13 ;
			  4 : transf := 5 ;
			  5 : transf := 4 ;
			  6 : transf := 2 ;
			  7 : transf := 9 ;
			  8 : transf := 10 ;
			  9 : transf := 7 ;
			  10 : transf := 8 ;
			  11 : transf := 12 ;
			  12 : transf := 11 ;
			  13 : transf := 3 ;
			  14 : transf := 15 ;
			  15 : transf := 14 ;
			END (* CASE TRANSF, LCOND *) ;
		        sval : val := ord (true) - ord (val) ;
		        varbl, lval :
			BEGIN
			  IF kind <> lval THEN
			    IF raisused THEN transfer (gattr, inq) ELSE
			      transfer (gattr, inacc) ;
			  WITH gattr DO
			    BEGIN
			      accbloc := ldregbloc ; kind := lcond ; accbool := true ;
			      IF accbloc^.sregister = ra THEN transf := 13 ELSE
			        transf := 15 ;
			    END ;
			END ;
		      END (* CASE KIND *) ;
		    END (* NO ERROR,with GATTR *) ;
	        END (* NO=5  not *) ELSE
	        IF no = 11 (* [ *) THEN
		BEGIN                         (* SET  EXPRESSION *)
		  insymbol ;
		  IF no = 12 (* ] *) THEN
		    BEGIN
                                                  (* EMPTY SET *)
		      WITH gattr DO
		        BEGIN
			typtr := lamptr ; kind := sval ;
			longv := bytesforset ; valpw := nulpw ;
		        END ;
		      insymbol ;
		    END (* EMPTY *) ELSE
		    BEGIN                     (* not EMPTY. *)
                                                  (* BUILT IN LATTR BY SUCCESSIVE *)
                                                  (* CALLS OF ELEMENT *)
		      WITH lattr DO
		        BEGIN
			typtr := NIL ;      (* FLAG NO ERROR *)
			kind := sval ; longv := bytesforset ; valpw := nulpw ;
		        END (* INIT LATTR *) ;
		      lmaxcst := 0 ; lmincst := maxset ; lmaxallow := 0 ;
		      lpsval := nulpw ;
		      setelctp := NIL ;
		      element (lattr, setelctp, lpsval, lmaxcst, lmincst, lmaxallow) ;
		      WHILE no = 15 (* ; *) DO
		        BEGIN
			insymbol ; element (lattr, setelctp, lpsval, lmaxcst, lmincst, lmaxallow) ;
		        END ;
		      WITH lattr DO
		        BEGIN
			IF kind = sval THEN
			  BEGIN
			    valpw := lpsval ; val := lmaxcst * 1000 + lmincst ;
			    IF lmaxcst < bitsindword THEN
			      longv := bytesindword ;
			  END (* SVAL SET *) ELSE
			  BEGIN             (* LVAL *)
                                                  (* TWO PARTS: *)
                                                  (* LPSVAL COMPUTED PART BY COMPILER *)
                                                  (* PSR    RUN-COMPUTED *)
			    equal := true ;
			    FOR it := 0 TO bornesupset DO
			      IF lpsval [it] <> nulpw [it] THEN equal := false ;
			    IF NOT equal THEN
			      BEGIN
			        IF lmaxcst < bitsinword THEN
				BEGIN
				  entercst (lpsval [0], wretpt) ;
				  enterundlab (wretpt^.cstplace) ;
				END ELSE
				IF lmaxcst < bitsindword THEN
				  BEGIN
				    enterlcst (lpsval, lretpt) ;
				    enterundlab (lretpt^.lplace) ;
				  END ELSE
				  BEGIN
				    enterllcst (lpsval, llretpt) ;
				    enterundlab (llretpt^.llplace) ;
				  END ;
			        longop := lmaxcst + 1 ;
			        genstand (nreg, 0, iepp3, tic) ;
			        mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
			        geneism (icsl, 7 (* 0111=OR *), p0t0r0) ;
			        gendescb (pr3, 0, 0, 0, longop, tn) ;
			        gendescb (pr6, psrdepw, 0, 0, longop, tn) ;
			      END (*  <>  NULPW *) ;
			  END (* LVAL *) ;
                                                  (* SETELCTP  POINTS THE  *)
                                                  (* TYPE OF ELEMENT(S) *)
			IF setelctp = intptr THEN
			  typtr := pnumptr ELSE
			  IF setelctp <> NIL THEN
			    typtr := setelctp^.sptcstepw ;
		        END (* with LATTR *) ;
		      gattr := lattr ;
		      IF no = 12 (* ] *) THEN
		        insymbol ELSE
		        facterr (12) ;
		    END (* not EMPTY *) ;
		END (* NO=11  SET EXPR *) ELSE
		facterr (58) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  IF stattrace = high THEN
	    IF newattr THEN
	      BEGIN
	        write (mpcogout, '* GATTR BUILT IN FACTOR IS:') ; nextline ;
	        printattr (gattr) ;
	      END ;
	  write (mpcogout, '^^^ FIN FACTOR with NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* FACTOR *) ;

$OPTIONS page $

(* ********************************** TERM ********************************** *)

    PROCEDURE term ;

(* C  . COMPILES   A TERM ::=  <FACTOR>  [  <MULT-OD>  <FACTOR>]*
   . MULT-OP  ARE  CODED  NO=6   CL=  1,2,3,4,5
   CL=1 *    REEL,NUMERIC     , SET INTERSECTION
   CL=2 /    REEL,NUMERIC  GIVES A REAL
   CL=3 AND  BOOLEAN
   CL=4 DIV  NUMERIC
   CL=5 MOD  NUMERIC
   C *)
(* E ERRORS DETECTED
   129:  OPERANDS  TYPE CONFLICT
   134:  ILLEGAL   OPERAND TYPE
   E *)

      VAR

        loczerodiv : integer ;
        lmopcl : integer ;
$OPTIONS compile = trace $
        newattr : boolean ;
$OPTIONS compile = true $
        lattr : attr ;
        generic : ctp ;
        ljump : istand ;
      BEGIN                                       (* TERM *)
$OPTIONS compile = trace $
        newattr := false ;
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT TERM ^^^') ; nextline ;
	END ;
$OPTIONS compile = true $
        factor ;
        WHILE no = 6 DO
	BEGIN (* MULT. OPERATOR *)              (*  *  /  AND  DIV  MOD *)
	  lmopcl := cl ;
	  WITH gattr DO
	    IF typtr <> NIL (* LEFT OPERAND *) THEN
	      IF (lmopcl = 2) (* / *) AND (typtr^.form = numeric) THEN
	        BEGIN
		convreal (gattr)              (* LVAL  EAQ , OR  RSVAL *)
	        END ELSE
	        CASE kind OF
		varbl : IF NOT easyvar (gattr) THEN
		    transfer (gattr, inacc) ;
		sval, lval : ;
		chain : BEGIN error (134) ; gattr.typtr := NIL ;
		  END ;
		lcond : choicerarq ;
	        END (* CASE, WITH GATTR do *) ;
	  lattr := gattr ;
$OPTIONS compile = trace $
	  newattr := true ;
$OPTIONS compile = true $
	  insymbol ;
	  factor ;
	  IF (lattr.typtr <> NIL) AND (gattr.typtr <> NIL) THEN
	    BEGIN
	      compatbin (lattr.typtr, gattr.typtr, generic) ;
	      IF generic = NIL THEN error (129) ELSE
	        IF generic^.form <> power THEN
		BEGIN
		  CASE lmopcl OF
		    1 (* * *) : IF generic^.form > numeric THEN
		        error (134) ELSE
		        genopmult (lattr, generic) ;
		    2 (* / *) :               (* GENERIC IS REAL *)
		      WITH gattr DO
		        BEGIN
			IF typtr <> realptr THEN convreal (gattr) ;
			IF divcheck THEN
			  IF kind <> sval THEN
			    BEGIN
			      transfer (gattr, inacc) ;
			      loczerodiv := indfich ; genstand (nreg, 0, itnz, tic) ;
                                                  (* SKIP if NOT ZERO ON *)
			      genexceptcode (diverrcode, reaq) ;
			      inser (cb, loczerodiv) ;
			    END ;
			genopdivi (lattr) ;
		        END (* WITH GATTR *) ;
		    3 (* AND *) : IF generic <> boolptr THEN error (134) ELSE
		        genandor (lattr, 6) ; (* NO=6 ==> AND *)
		    4, 5 (* DIV,MOD *) :
		      IF generic^.form <> numeric THEN error (134) ELSE
		        WITH gattr DO
			BEGIN
			  IF divcheck THEN
			    IF kind <> sval THEN
			      BEGIN
			        transfer (gattr, inq) ;
			        IF lmopcl = 4 (* DIV *) THEN ljump := itnz ELSE
				ljump := itpnz ;
			        loczerodiv := indfich ; genstand (nreg, 0, ljump, tic) ;
                                                  (* SKIP if ZERO OFF *)
			        genexceptcode (diverrcode, rq) ;
			        inser (cb, loczerodiv) ;
			      END ;
			  gendivmod (lattr, lmopcl) ;
			END ;
		  END (* CASE LMOPCL *) ;
		  gattr.typtr := generic ;
		END (*  <>  SET *) ELSE
		BEGIN
		  IF lmopcl <> 1 THEN error (134) ELSE
		    genoppw (lattr, 6, 1) ;
		  gattr.typtr := generic ;
		END (* SET , GENERIC  <>  NIL *) ;
	    END (* LATTR  <> NIL, GATTR  <> NIL *) ;
	END (* while NO=6 *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  IF (stattrace = high) AND newattr THEN
	    printattr (gattr) ;
	  write (mpcogout, '^^^ FIN TERM ^^^  WITH  NO,CL:', no : 4, cl : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* TERM *) ;

$OPTIONS page $

(* ********************************* SIMPLEEXP ***************************** *)

    PROCEDURE simpleexp ;

(* C   COMPILES  A  SIMPLE-EXPRESSION ::=
   [ +/-]  <TERM>   [ <+,-,OR>  <TERM>]*
   NO=7    CL=1  +    REAL,NUMERIC      SET UNION
   CL=2  -    REAL,NUMERIC      SET DifFER
   CL=3  OR   BOOLEAN
   C *)
(* E ERRORS DETECTED
   60:  OR  MONADIC  NOT ALLOWED
   129:  TYPE  CONFLICT
   134:  ILLEGAL TYPE OF OPERAND
   135:  BOOLEAN OPERAND EXPECTED
   303:  VALUE OUT OF RANGE
   E *)
      VAR
        minus, plus
$OPTIONS compile = trace $, newattr
$OPTIONS compile = true $
        : boolean ;
        ldisp, ladopcl : integer ;
        lbase : preg ;
        ltag : tag ;
        lattr : attr ;
        generic : ctp ;
      BEGIN                                       (* SIMPLEEXP *)
$OPTIONS compile = trace $
        newattr := false ;
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT SIMPLEEXP ^^^') ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* TEST FOR MONADIC OPERATOR *)
        minus := false ; plus := false ;
        IF no = 7 (*  + - OR *) THEN
	BEGIN
	  IF cl = 2 (* - *) THEN
	    minus := true ELSE
	    IF cl = 3 (* OR *) THEN error (60) ELSE plus := true ;
	  insymbol ;
	END ;
                                                  (* ************************** *)
        term ;
                                                  (* *************************** *)
        IF plus THEN
	BEGIN IF gattr.typtr <> NIL THEN
	    IF gattr.typtr^.form > numeric THEN
	      error (134)
	END ELSE
	IF minus THEN
	  WITH gattr DO
	    IF typtr <> NIL THEN
	      IF typtr^.form > numeric THEN
	        error (134) ELSE
	        BEGIN
$OPTIONS compile = trace $
		newattr := true ;
$OPTIONS compile = true $
		CASE kind OF
		  sval : IF typtr = realptr THEN
		      rsval := -rsval ELSE
		      IF val <> -maxint - 1 THEN
		        val := -val ELSE
		        error (303) ;
		  lval : BEGIN transfer (gattr, inacc) ;
		      genstand (nreg, 0, opaq [neg, ldreg], tn) ;
		    END ;
		  varbl : IF easyvar (gattr) AND (typtr <> realptr) THEN
		      BEGIN
		        calcvarient (gattr, lbase, ldisp, ltag) ;
		        sauvereg (ra, true) ;
		        usednameaddr := nameaddr ;
		        genstand (lbase, ldisp, ilca, ltag) ;
		        kind := lval ; ldreg := ra ; ldregbloc := currentbloc ;
		      END (* EASY *) ELSE
		      BEGIN transfer (gattr, inacc) ;
		        genstand (nreg, 0, opaq [neg, ldreg], tn) ;
		      END (* NOT EASY, VARBL *) ;
		END (* CASE KIND *) ;
	        END (* MINUS *) ;
        WHILE no = 7 DO
	BEGIN                                   (*    CL=1  +    CL=2 -   CL=3  OR *)
$OPTIONS compile = trace $
	  newattr := true ;
$OPTIONS compile = true $
	  ladopcl := cl ;
	  WITH gattr DO
	    IF typtr <> NIL THEN
	      IF typtr^.father_schema <> string_ptr THEN
	        IF (typtr^.form = power) AND (ladopcl = 2) THEN
		transfer (gattr, inpsr) ELSE
		CASE kind OF
		  sval, lval : ;
		  chain : IF envstandard <> stdextend THEN
		      BEGIN
		        error (134) ; gattr.typtr := NIL ;
		      END ;
		  varbl : IF NOT easyvar (gattr) THEN
		      transfer (gattr, inacc) ;
		  lcond : choicerarq ;
		END (* CASE KIND *) ;
	  lattr := gattr ;
                                                  (* ************************** *)
	  insymbol ;
	  term ;
                                                  (* **************************** *)
	  IF (lattr.typtr <> NIL) AND (gattr.typtr <> NIL) THEN
	    BEGIN
	      compatbin (lattr.typtr, gattr.typtr, generic) ;
	      IF (envstandard = stdextend)
	        AND is_possible_string (gattr) AND is_possible_string (lattr) AND (ladopcl = 1) THEN
	        genconcat (lattr) ELSE
	        IF generic = NIL THEN error (129) ELSE BEGIN
		  IF generic^.form <> power THEN
		    CASE ladopcl OF
		      1 : (* + *) IF generic^.form > numeric THEN error (134) ELSE
			genopadd (lattr, generic) ;
		      2 : (* - *) IF generic^.form > numeric THEN error (134) ELSE
			genopsub (lattr, generic) ;
		      3 : (* OR *) IF generic <> boolptr THEN error (135) ELSE
			genandor (lattr, 7 (* OR *)) ;
		    END (* CASE LADOPCL   <> POWER *) ELSE
		    BEGIN                     (* POWER *)
		      IF ladopcl = 3 THEN error (134) ELSE
		        genoppw (lattr, 7, ladopcl) ;
		    END (* POWER *) ;
		  gattr.typtr := generic ;
		END (* GENERIC <> nil *) ;
	    END (* NOT nil *) ;
	END (* WHILE NO=7 *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  IF (stattrace = high) AND newattr THEN
	    printattr (gattr) ;
	  write (mpcogout, '^^^ FIN SIMPLEEXP ^^^ WITH NO,CL', no : 4, cl : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* SIMPLEEXP *) ;

$OPTIONS page $

(* *********************************** EXPRESSION **************************** *)

    PROCEDURE expression ;

(* C . COMPILES     <SIMPLEEXP>  [ <RELAT>  <SIMPLEEXP> ]
   . NO=8   CL=1   <
   2   <=
   3   >=
   4   >
   5   <>
   6   =
   7   IN
   . AS OUPUT   A GATTR  LCOND IS PRODUCED
   C *)
(* E ERRORS DETECTED
   108: FILES/CLASS  NOT ALLOWED
   129: TYPE  CONFLICT
   134: ILLEGAL TYPE OF OPERAND
   E *)
      VAR

        bitselect : integer ;
        generic : ctp ;
        lattr : attr ;
        lbase : preg ;
        lcomp : istand ;
        ldisp : integer ;
        lerr : boolean ;
        llretpt : llcstpt ;
        lmax : integer ;
        lmin : integer ;
        locmax : integer ;
        locmin : integer ;
        locskip : integer ;
        lreopcl : integer ;
        lres : boolean ;
        lretpt : lcstpt ;
        ltag : tag ;
$OPTIONS compile = trace $
        newattr : boolean ;
$OPTIONS compile = true $
        tofind : integer ;
        totest : integer ;

(* ****************************************** EERROR < EXPRESSION ********* *)

      PROCEDURE eerror (errnum : integer) ;
        BEGIN
                                                  (* DUMMY VALUE *)
	gattr.typtr := boolptr ; gattr.kind := sval ; gattr.val := 0 (* false *) ;
	error (errnum) ;
        END (* EERROR *) ;

      BEGIN                                       (* EXPRESSION *)
$OPTIONS compile = trace $
        newattr := false ;
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT EXPRESSION ^^^') ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* ************************ *)
        simpleexp ;
                                                  (* ************************ *)
        IF no = 8 (* RELATIONAL OPERATOR *) THEN
	BEGIN
	  lreopcl := cl ;                       (*  <  <=  >= >  <>  = IN *)
	  WITH gattr DO                         (* LEFT OPERAND *)
	    IF typtr <> NIL THEN
	      BEGIN                             (* NO ERROR *)
	        IF typtr^.form < power THEN
		BEGIN
		  CASE kind OF
		    lval, sval : ;
		    lcond : choicerarq ;
		    varbl : IF NOT easyvar (gattr) THEN transfer (gattr, inacc) ;
		  END (* CASE KIND *) ;
		END (* < POWER *) ELSE
		IF typtr^.form = power THEN
		  BEGIN
		    IF lreopcl IN [2, 3] THEN
		      transfer (gattr, inpsr) ELSE
		      CASE kind OF
		        varbl : IF NOT easyvar (gattr) THEN transfer (gattr, inacc) ;
		        sval, lval : ;
		      END (* case KIND *) ;
		  END (* = POWER *) ELSE
		  IF typtr^.form < files THEN
		    BEGIN                     (* ARRAYS RECORDS *)
		      IF kind = varbl THEN
		        IF NOT varissimple (gattr) THEN
			BEGIN
			  loadadr (gattr, nreg) ;
			  basereg := currentpr ; basebloc := currentbloc ;
			  dplmt := 0 ; itsdplmt := 0 ;
			  inxreg := nxreg ; inxbloc := NIL ; inxmem := 0 ;
			  inxmemrw := true ; pckd := false ;
			  access := pointee ;
			END ;
		    END (* ARRAYS,RECORDS *) ELSE
		    error (134) ;
	      END (* TYPTR <> nil, with GATTR *) ;
	  lattr := gattr ;
                                                  (* ******************* *)
	  insymbol ;
	  simpleexp ;
                                                  (* ********************** *)
	  IF (gattr.typtr <> NIL) AND (lattr.typtr <> NIL) THEN
	    BEGIN
	      IF lreopcl <> 7 THEN
	        BEGIN                           (* OPERATORS  < ...  = *)
		compatbin (lattr.typtr, gattr.typtr, generic) ;
		IF generic = NIL THEN
		  IF (envstandard = stdextend)
		    AND is_possible_string (lattr) AND is_possible_string (gattr) THEN
		    gen_string_comp (lattr, lreopcl) ELSE
		    eerror (129) ELSE
		  CASE generic^.form OF
		    reel, numeric, scalar : gencompare (lattr, lreopcl, generic) ;
		    pointer :
		      BEGIN
		        IF envstandard <> stdextend THEN
			IF lreopcl <= 4 THEN eerror (134) ;
		        genptcomp (lattr, lreopcl) ;
		      END ;
		    records :
		      IF (envstandard = stdextend) AND
		        is_possible_string (lattr) AND is_possible_string (gattr) THEN
		        gen_string_comp (lattr, lreopcl) ELSE
		        BEGIN
			IF (envstandard <> stdextend) OR (lreopcl <= 4) THEN eerror (134) ;
			genstcomp (lattr, lreopcl) ;
		        END ;
		    arrays :
		      BEGIN
		        lerr := true ;
		        IF isstring (lattr) THEN
			IF isstring (gattr) THEN
			  lerr := false ;
		        IF envstandard = stdextend THEN
			IF lreopcl > 4 THEN
			  lerr := false ;
		        IF lerr THEN
			eerror (134) ELSE
			genstcomp (lattr, lreopcl) ;
		      END (* ARRAYS *) ;
		    power :
		      BEGIN
		        IF lreopcl IN [2, 3, 5, 6] THEN
			genoppw (lattr, 8 (* NO *), lreopcl) ELSE
			eerror (134) ;
		      END ;
		    files, aliastype : eerror (108) ;
		  END (* GENERIC^.FORM *) ;
	        END (* LREOPCL  <>  7 *) ELSE
	        BEGIN                           (* OPERATOR IN *)
		lerr := true ;
		IF gattr.typtr^.form = power THEN
		  IF lattr.typtr^.form <= scalar THEN
		    BEGIN
		      compatbin (lattr.typtr, gattr.typtr^.elset, generic) ;
		      IF (generic <> NIL) THEN
		        IF generic <> realptr THEN
			lerr := false ;
		    END ;
		IF lerr THEN
		  eerror (129) ELSE
                                                  (* OK FOR TYPES. LET'S GO *)

(* LATTR MAY BE *)
(*    SVAL *)
(*    VARBL  EASY TO ADRESS *)
(*    LVAL   IN RA OR RQ .  SAVED OR NOT *)
(* GATTR MAY BE *)
(*    SVAL    8  OR MAX *)
(*    LVAL    RAQ  PSR *)
(*    VARBL   ANY SIZE *)
		  WITH gattr DO
		    BEGIN
		      findminmax (typtr^.elset, lmin, lmax) ;
		      IF lattr.kind = sval THEN
		        BEGIN
			IF kind = sval THEN
			  BEGIN             (* COMPILER KNOWN *)
			    IF inbounds (lattr.val, 0, maxset) THEN
			      BEGIN
			        totest := valpw [lattr.val DIV bitsinword] ;
			        tofind := lattr.val MOD bitsinword ;
			        append_ (totest, tofind, 0) ;
			        lres := totest < 0 ;
			      END ELSE
			      lres := false ;
                                                  (* GATTR *)
			    IF lres THEN
			      transf := 4 (* true *) ELSE
			      transf := 5 (* false *) ;
			    accbloc := NIL ; accbool := false ;
			  END (* GATTR  SVAL *) ELSE
			  BEGIN
			    IF inbounds (lattr.val, lmin, lmax) THEN
			      BEGIN
			        IF kind = lval THEN
				BEGIN
                                                  (* RAQ ==> SHifT    PSR ==> VARBL *)
				  IF ldreg = raq THEN
				    BEGIN
				      genstand (nreg, lattr.val, ills, tn) ;
                                                  (* NEGATIVE ON=true *)
				      freebloc (ldregbloc) ; newbloc (ra) ;
				      transf := 1 ;
				      accbloc := currentbloc ; accbool := true ;
				    END (* RAQ *) ELSE
				    lvalvarbl (gattr) ;
				END (* GATTR WAS LVAL *) ;
			        IF kind = varbl THEN
				BEGIN     (* INCLUDES  OLD PSR *)
                                                  (* MODIFY DPLMT  TO *)
                                                  (* POINT  THE RIGHT BYTE *)
				  dplmt := dplmt + lattr.val DIV bitsinbyte ;
				  bitselect := lattr.val MOD bitsinbyte ;
				  loadadr (gattr, pr3) ;
				  mfari1 := a0r0i0 ; mfari2 := a1r0i0 ;
				  geneism (icmpb, 0, p1t0r0) ; (* FILL BIT 1 *)
				  gendescb (nreg, 0, 0, 0, 0, tn) ; (* DUMMY *)
				  usednameaddr := nameaddr ;
				  gendescb (pr3, 0, 0, bitselect, 1, tn) ;
                                                  (* ONE BIT OPERAND *)
                                                  (* ZERO ON  <==> true *)
				  transf := 2 ; accbool := false ; accbloc := NIL ;
				END (* KIND=VARBL *) ;
			      END (* INBOUNDS *) ELSE
			      BEGIN         (* false *)
			        freeattr (gattr) ;
			        transf := 5 ;
			        accbool := false ; accbloc := NIL ;
			      END (* false *) ;
			  END (* GATTR NOT SVAL *) ;
		        END (* LATTR.SVAL *) ELSE
		        BEGIN
			IF kind = lval THEN (* GATTR  IN AQ OR PSR *)
			  BEGIN
			    IF ldreg = raq THEN
			      BEGIN
			        IF lattr.kind = lval THEN
				lvalvarbl (lattr) ;
			        calcvarient (lattr, lbase, ldisp, ltag) ;
			        WITH lattr DO
				IF kind = varbl THEN usednameaddr := nameaddr ;
			        genstand (lbase, ldisp, ilxl7, ltag) ;
                                                  (* X7 = VALUE  TO TEST IN AQ *)
                                                  (* FIRST  CHECK MIN, MAX  then  SHifT *)
			        genstand (nreg, lmin, icmpx7, tdu) ;
			        locmin := indfich ; genstand (nreg, 0, itmi, tic) ;
                                                  (* SKIP if < *)
			        genstand (nreg, lmax, icmpx7, tdu) ;
			        locmax := indfich ; genstand (nreg, 0, itpnz, tic) ;
                                                  (* SKIP if > *)
			        genstand (nreg, 0, ills, tx7) ; (* NOW SHifT *)
                                                  (* true ==  NEGATIVE ON *)
			        locskip := indfich ; genstand (nreg, 0, itra, tic) ;
			        inser (cb, locmin) ;
			        inser (cb, locmax) ;
			        genstand (nreg, ord (false), ilda, tdl) ;
			        inser (cb, locskip) ;
			        freebloc (ldregbloc) ; newbloc (ra) ; transf := 1 ;
			        accbool := true ; accbloc := currentbloc ; (* LCOND  LATER *)
			      END (* LDREG=RAQ *) ELSE
			      lvalvarbl (gattr) ;
			  END (* KIND=LVAL *) ;
			IF kind <> lval THEN
			  BEGIN
			    IF kind = sval THEN
			      BEGIN
			        IF longv = bytesindword THEN
				BEGIN
				  enterlcst (valpw, lretpt) ;
				  IF lmax > bitsindword - 1 THEN lmax := bitsindword - 1 ;
				  enterundlab (lretpt^.lplace) ;
				END ELSE
				BEGIN     (* LONG SET *)
				  enterllcst (valpw, llretpt) ;
				  enterundlab (llretpt^.llplace) ;
				END (* LONGSET *) ;
			        genstand (nreg, 0, iepp3, tic) ;
			      END (* SVAL *) ELSE
			      loadadr (gattr, pr3) ;
			    IF lattr.kind = lval THEN
			      regenere (lattr.ldregbloc) ELSE
			      transfer (lattr, inacc) ;
                                                  (* NOW RA OR RQ LOADED *)
			    lcomp := opaq [cmp, lattr.ldreg] ;
			    genstand (nreg, lmin, lcomp, tdl) ;
			    locmin := indfich ;
			    genstand (nreg, 0, itmi, tic) ; (* SKIP if < *)
			    genstand (nreg, lmax, lcomp, tdl) ;
			    locmax := indfich ;
			    genstand (nreg, 0, itpnz, tic) ; (* SKIP if > *)
                                                  (* ADD BIT DISP AT PR3 *)
			    genstand (pr3, 0, iabd, modif [lattr.ldreg]) ;
			    mfari1 := a0r0i0 ; mfari2 := a1r0i0 ;
			    geneism (icmpb, 0, p1t0r0) ; (* FILL BIT TO 1 *)
			    gendescb (nreg, 0, 0, 0, 0, tn) ; (* DUMMY *)
			    IF kind = varbl THEN usednameaddr := nameaddr ;
			    gendescb (pr3, 0, 0, 0, 1, tn) ; (* ONE BIT OPER *)
                                                  (* ZERO  ON  true *)
			    inser (cb, locmin) ; (* HERE ZERO OFF if  SKIP  OR  false *)
			    inser (cb, locmax) ;
			    freebloc (lattr.ldregbloc) ;
			    accbool := false ; accbloc := NIL ; transf := 2 ;
			  END (* GATTR.KIND   <>  LVAL *) ;
		        END (* LATTR NOT SVAL *) ;
		      gattr.kind := lcond ;
		    END (* with GATTR,NO ERROR(129) *) ;
	        END (* LREOPCL=7 *) ;
	      gattr.typtr := boolptr ;
	    END (* NOT nil FOR GATTR,LATTR *) ;
$OPTIONS compile = trace $
	  newattr := true ;
$OPTIONS compile = true $
	END (* NO=8  RELATIONAL OPERATOR *) ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  IF (stattrace = high) AND newattr THEN
	    printattr (gattr) ;
	  write (mpcogout, '^^^ FIN EXPRESSION with NO,CL ', no : 4, cl : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* EXPRESSION *) ;

    BEGIN
    END.                                          (* Fin du module d ' analyse des expressions    *)
