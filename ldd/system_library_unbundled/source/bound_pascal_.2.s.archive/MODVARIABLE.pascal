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


PROGRAM modvariable ;

$OPTIONS switch trace := true ; switch security := true ; t + $


    $IMPORT
      'STATE (pascal) ' :
        addressvar,
        checkbnds,
        choicerarq,
        enterlcst,
        enterllcst,
        enterundlab,
        freebloc,
        gencstecode,
        genexceptcode,
        getpr,
        inbounds,
        loadadr,
        loadbase,
        newbloc,
        oldnewstor,
        raisused,
        regenere,
        sauvereg,
        stack_extension,
        transfer,
        variab,
        variabctptr ;
      'CONTEXTTABLE (pascal) ' :
        areconformeq,
        checkminmax,
        compatbin,
        conformantdim,
        findminmax,
        legalconfarrsubstitution,
        packedsize ;
      'RACINE (pascal) ' :
        error,
        insymbol,
        nameisref,
        nextline,
        recadre,
        skip,
        sup ;
      'optimized_procedures (alm)' :
        search,
        srchrec ;
      'MODATTR(pascal)' :
        convreal,
        freeattr,
        initattrvarbl,
        is_pl1_varying_char,
        isstring,
        printattr,
        varissimple ;
      'EXPR (pascal) ' :
        expression ;
      'GENOPER (pascal)' :
        check_dynamic_string_length ;
      'GENERE (pascal) ' :
        gendesca,
        inser,
        genstand,
        geneism ;
      'RACINE (pascal) ' :
        alfaptr,
        charptr,
        ctptr,
        envstandard,
        interactive,
        intptr,
        level,
        mpcogout,
        no,
        realptr,
        string_ptr,
        symbolfile,
        symbolline,
        symbolmap,
        textfilectp ;
      'GENERE (pascal) ' :
        cb,
        indfich,
        mfari1,
        mfari2,
        usednameaddr ;
      'STATE (pascal) ' :
        arrayboundsctp,
        asscheck,
        currentbloc,
        currentpr,
        currwithlist,
        gattr,
        inxcheck,
        modif,
        opaq,
        prinst,
        regcharge,
        stattrace,
        withvariable ;
    $


    $EXPORT
      init_desc_address,
      passparams,
      variable

    $
$INCLUDE 'CONSTTYPE' $


    VAR

(* REDEFINE IMPORTED VARIABLES FROM RACINE   *)

      alfaptr : ctp ;
      charptr : ctp ;
      ctptr : ctp ;
      envstandard : stdkind ;
      interactive : boolean ;
      intptr : ctp ;
      level : levrange ;
      mpcogout : text ;
      no : integer ;
      realptr : ctp ;
      string_ptr : ctp ;
      symbolfile : integer ;
      symbolline : integer ;
      symbolmap : boolean ;
      textfilectp : ctp ;

(* REDEFINE IMPORTED VARIABLES FROM GENERE   *)

      cb : integer ;
      indfich : integer ;
      mfari1 : zari ;
      mfari2 : zari ;
      usednameaddr : ctp ;

(* REDEFINE IMPORTED VARIABLES FROM STATE    *)

      arrayboundsctp : ctp ;
      asscheck : boolean ;
      currentbloc : regpt ;
      currentpr : preg ;
      currwithlist : withreflist ;
      gattr : attr ;
      inxcheck : boolean ;
      modif : ARRAY [nxreg..rq] OF tag ;          (* GIVES FOR A REGISTER R ITS TAG TR *)
      opaq : ARRAY [typeofop, ra..reaq] OF istand ;
      prinst : ARRAY [typepr, pr1..pr6] OF istand ;
      regcharge : statearray ;
      stattrace : levtrace ;
      variabctptr : ctp ;
      withvariable : boolean ;

(* REDEFINE IMPORTED PROCEDURES FROM STATE *)

    PROCEDURE addressvar (fctp : ctp ; VAR fattr : attr ; modif : boolean) ; EXTERNAL ;
    PROCEDURE checkbnds (errcode : integer ; freg : register ; fctp : ctp) ; EXTERNAL ;
    PROCEDURE choicerarq ; EXTERNAL ;
    PROCEDURE enterlcst (VAR fval : setarray ; VAR fboxpt : lcstpt) ; EXTERNAL ;
    PROCEDURE enterllcst (VAR fval : setarray ; VAR fboxpt : llcstpt) ; EXTERNAL ;
    PROCEDURE enterundlab (VAR fundinx : integer) ; EXTERNAL ;
    PROCEDURE freebloc (VAR fbtofree : regpt) ; EXTERNAL ;
    PROCEDURE gencstecode (farg : integer ; finst : istand) ; EXTERNAL ;
    PROCEDURE genexceptcode (ferrcode : integer ; freg : register) ; EXTERNAL ;
    PROCEDURE getpr ; EXTERNAL ;
    FUNCTION inbounds (fval, fmin, fmax : integer) : boolean ; EXTERNAL ;
    PROCEDURE loadadr (VAR fattr : attr ; wantedpr : preg) ; EXTERNAL ;
    PROCEDURE loadbase (flev : integer) ; EXTERNAL ;
    PROCEDURE newbloc (freg : register) ; EXTERNAL ;
    FUNCTION oldnewstor (incrinbytes : integer) : integer ; EXTERNAL ;
    FUNCTION raisused : boolean ; EXTERNAL ;
    PROCEDURE regenere (oldbloc : regpt) ; EXTERNAL ;
    PROCEDURE sauvereg (freg : register ; fload : boolean) ; EXTERNAL ;
    PROCEDURE stack_extension ; EXTERNAL ;
    PROCEDURE transfer (VAR fattr : attr ; inwhat : destination) ; EXTERNAL ;
    PROCEDURE variab (fvarset : boolean) ; EXTERNAL ;

(* REDEFINE IMPORTED PROCEDURES FROM CONTEXTTABLE *)

    FUNCTION areconformeq (f1, f2 : ctp) : boolean ; EXTERNAL ;
    PROCEDURE checkminmax (fvalu : integer ; fctp : ctp ; ferrnum : integer) ; EXTERNAL ;
    PROCEDURE compatbin (typleft, typright : ctp ; VAR fgeneric : ctp) ; EXTERNAL ;
    FUNCTION conformantdim (ffound : ctp) : boolean ; EXTERNAL ;
    PROCEDURE findminmax (fctp : ctp ; VAR fmin, fmax : integer) ; EXTERNAL ;
    FUNCTION legalconfarrsubstitution (ffound, fdecl : ctp) : boolean ; EXTERNAL ;
    FUNCTION packedsize (fctp : ctp) : integer ; EXTERNAL ;

(* REDEFINE IMPORTED PROCEDURES FROM RACINE *)

    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE insymbol ; EXTERNAL ;
    PROCEDURE nameisref (p : ctp ; f, l : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    FUNCTION recadre (fnum, fmod : integer) : integer ; EXTERNAL ;
    PROCEDURE search ; EXTERNAL ;
    PROCEDURE skip (nosym : integer) ; EXTERNAL ;
    PROCEDURE srchrec (fdebsrch : ctp) ; EXTERNAL ;
    FUNCTION sup (fval1, fval2 : integer) : integer ; EXTERNAL ;

(* REDFINE IMPORTED PROCEDURES FROM MODATTR *)

    FUNCTION is_pl1_varying_char (VAR typeptr : ctp) : boolean ; EXTERNAL ;
    PROCEDURE convreal (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE initattrvarbl (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE freeattr (VAR fattr : attr) ; EXTERNAL ;
    FUNCTION isstring (VAR fattr : attr) : boolean ; EXTERNAL ;
    PROCEDURE printattr (VAR fattr : attr) ; EXTERNAL ;
    FUNCTION varissimple (VAR fattr : attr) : boolean ; EXTERNAL ;

(* REDEFINE IMPORTED PROCEDURES FROM EXPRESSION *)

    PROCEDURE expression ; EXTERNAL ;

(* REDEFINE IMPORTED PROEDURES FORM GENOPER *)

    PROCEDURE check_dynamic_string_length (VAR fattr : attr) ; EXTERNAL ;

(* REDEFINE IMPORTED PROCEDURES FROM GENERE     *)

    PROCEDURE gendesca (fareg : preg ; fadr, fcn : integer ; fta : lgcar ;
      fn : integer ; frlgth : mreg) ; EXTERNAL ;
    PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ; EXTERNAL ;
    PROCEDURE inser (fcb : integer ; fplace : integer) ; EXTERNAL ;
    PROCEDURE geneism (fcode : ieism ; ffield : integer ; fbits : zptr) ; EXTERNAL ;


$OPTIONS page $

(* **************************    INIT_DESC_ADDRESS     ********* *)

    PROCEDURE init_desc_address (fctptr : ctp ; VAR fattr : attr) ;

(* C A conformant array or schema variable described by FATTR is input.
   As output, a pointer register on dopevector (and his box)
   FATTR points real variable
   C *)

      VAR
        ldisp : integer ;
        locpt : ctp ;
        lreg : preg ;
        lbloc : regpt ;


      BEGIN                                       (* INIT_DESC_ADDRESS *)

        IF fattr.descreg = nreg THEN
	BEGIN
$OPTIONS compile = trace $
	  IF stattrace = high THEN
	    BEGIN
	      write (mpcogout, '^^^ Debut de INIT_DESC_ADDRESS ^^^ ') ; nextline ;
	    END ;
$OPTIONS compile = true $

	  locpt := fattr.nameaddr ;
	  ldisp := 1 ;
	  IF locpt <> NIL THEN
	    IF locpt^.vtype <> NIL THEN
	      IF locpt^.vtype^.father_schema <> NIL THEN ldisp := 0 ;
                                                  (* Return DOPEVECTOR BASE *)
	  getpr ;
	  genstand (fattr.basereg, fctptr^.vdescaddr DIV bytesinword, prinst [epp, currentpr], tny) ;
	  genstand (currentpr, ldisp (* Header *), prinst [epp, currentpr], tn) ;
	  lreg := currentpr ;
	  lbloc := currentbloc ;

	  loadadr (fattr, nreg) ;               (* Returns   CURRENTPR  and CURRENTBLOC  *)

	  initattrvarbl (fattr) ;
	  WITH fattr DO
	    BEGIN
	      access := pointee ; basereg := currentpr ; basebloc := currentbloc ;
	      nameaddr := locpt ;
	      descreg := lreg ; descbloc := lbloc ;
	    END ;

	END ;

$OPTIONS compile = trace $
        IF stattrace = high THEN
	BEGIN
	  write (mpcogout, '^^^ Fin   de INIT_DESC_ADDRESS ^^^') ; nextline ;
	END ;
$OPTIONS compile = true $

      END (* INIT_DESC_ADDRESS *) ;

$OPTIONS page $

(* *************************** VARIABLE ****************************** *)

    PROCEDURE variable (fvarset : boolean) ;

(* C
   BUILD  A GATTR   FOR    ELEMENT ARRAY       NO=11   [
   POINTED ITEM          =18   ^
   RECORD FIELD          =17   .
   FILE ELEMENT          =18   ^

   FIRST CALL  ADDRESSVAR
   C *)

(* E  ERRORS DETECTED
   2: IDENTIFIER EXPECTED
   12: "]" EXPECTED
   139: INDEX TYPE NOT COMPATIBLE with DECLARATION
   140: RECORDS EXPECTED
   141: FILES or POINTER EXPECTED
   142: ARRAYS  EXPECTED
   152: NO  SUCH FIELD in THIS RECORD
   302: INDEX OUT OF BOUNDS
   E *)

      VAR
        loc1, loc2 : integer ;
        string_base : preg ; string_disp : integer ;
        loaded_reg : register ;
        lattr : attr ;
        lerr, smallelem, isconform, totransfer, stoprepeat : boolean ;
        locvariabctptr : ctp ;
        nextdimisconform, done_with_index : boolean ;
        arraytype, generic : ctp ;
        destused : destination ;
        regused : register ;
$OPTIONS compile = trace $
        newattr : boolean ;
$OPTIONS compile = true $
        subarraysize, pointzero, twopower, lmin, lmax : integer ;
        lbase : preg ;
        lcomp : istand ;
        oldline, oldfile : integer ;
        lp, oldptr : ctp ;
        checkismade : boolean ;
        locdopevectordisp : integer ;
        previouswasarrow, savewithflag : boolean ;
        it : integer ;
        refs : RECORD
	nbr : integer ;
	ref : ARRAY [1..maxfield] OF
	RECORD
	  symbp : ctp ;
	  rfile, rline : integer
	END
        END ;


(* *************************************************** ENTERREF **************************** *)

      PROCEDURE enterref ;

        BEGIN
	IF oldptr <> NIL THEN
	  IF refs.nbr < maxfield THEN
	    BEGIN
	      refs.nbr := refs.nbr + 1 ;
	      WITH refs.ref [refs.nbr] DO
	        BEGIN
		symbp := oldptr ;
		rfile := symbolfile ;
		rline := symbolline ;
	        END ;
	      oldptr := NIL ;
	    END
        END ;

      BEGIN                                       (* VARIABLE *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT VARIABLE ^^^') ;
	  nextline ;
	END ;
        newattr := false ;
$OPTIONS compile = true $

        locvariabctptr := ctptr ;
        addressvar (ctptr, lattr, fvarset) ;
        locdopevectordisp := 0 ;



        oldfile := symbolfile ; oldline := symbolline ; oldptr := ctptr ;
        insymbol ;
        previouswasarrow := false ;
        refs.nbr := 0 ;
        WHILE no IN [11, 17, 18] DO               (* [  .  ^ *)
	BEGIN
$OPTIONS compile = trace $
	  newattr := true ;
$OPTIONS compile = true $
	  IF no = 11 THEN                       (* ARRAY'S  ELEMENT *)
	    BEGIN
	      savewithflag := withvariable ;
	      withvariable := false ;
	      done_with_index := false ;
	      IF lattr.typtr <> NIL THEN
	        WITH lattr.typtr^ DO
		IF (father_schema = string_ptr) AND (no = 11) THEN
		  BEGIN                       (* STRING INDEX. SPECIAL SEQUENCE *)
		    done_with_index := true ;
		    IF asscheck THEN
		      check_dynamic_string_length (lattr) ;
		    IF varissimple (lattr) THEN
		      BEGIN
		        string_base := lattr.basereg ; string_disp := lattr.dplmt DIV bytesinword ;
		      END ELSE BEGIN
		        loadadr (lattr, nreg) ;
		        string_base := currentpr ; string_disp := 0 ;
		        WITH lattr DO
			BEGIN
			  access := pointee ; basereg := currentpr ; basebloc := currentbloc ;
			  dplmt := 0 ;
			END
		      END ;
		    lerr := false ;
		    insymbol ; expression ;
		    compatbin (intptr, gattr.typtr, generic) ;
		    IF (generic = NIL) OR (generic = realptr) THEN
		      BEGIN
		        lerr := true ; error (280) ;
		      END ;
		    IF no <> 12 THEN
		      BEGIN lerr := true ; error (12) END
		    ELSE insymbol ;
		    IF NOT lerr THEN
		      BEGIN
		        WITH gattr DO
			CASE kind OF
			  varbl : BEGIN
			      IF raisused THEN
			        BEGIN
				loaded_reg := rq ; sauvereg (rq, false) ;
				transfer (gattr, inq) ;
			        END
			      ELSE BEGIN
				loaded_reg := ra ;
				transfer (gattr, inacc) ;
			        END ;
			    END ;
			  sval : BEGIN
			      IF raisused THEN
			        BEGIN
				loaded_reg := rq ; sauvereg (rq, false) ;
			        END ELSE
			        loaded_reg := ra ;
			      gencstecode (val, opaq [load, loaded_reg]) ;
			    END ;
			  lval : BEGIN
			      loaded_reg := ldreg ;
			      IF asscheck THEN
			        genstand (nreg, 0, opaq [add, loaded_reg], tdl) ; (* TO SET INDIC *)
			    END ;
			END ;
		        freeattr (gattr) ;
		        IF asscheck THEN
			BEGIN
			  loc2 := indfich ; genstand (nreg, 0, itmoz, tic) ;
			  genstand (string_base, string_disp, opaq [cmp, loaded_reg], tn) ;
			  loc1 := indfich ; genstand (nreg, 0, itmoz, tic) ;
			  inser (cb, loc2) ;
			  genexceptcode (bad_string_index, loaded_reg) ;
			  inser (cb, loc1) ;
			END ;
		        WITH lattr DO
			BEGIN
			  IF basereg IN [prstatic, prlink, pr6] THEN
			    BEGIN
			      getpr ; genstand (basereg, 0, prinst [epp, currentpr], tn) ;
			      basereg := currentpr ; basebloc := currentbloc ;
			    END ;
			  genstand (basereg, 0, ia9bd, modif [loaded_reg]) ;
			  dplmt := dplmt + 3 ;
			  nameaddr := NIL ;
			  pckd := true ;
			END ;
		      END ;
		    lattr.typtr := charptr ;
		  END ;

	      IF NOT done_with_index THEN
	        BEGIN
		REPEAT                        (* LOOP ON EACH DIMENSION *)
		  WITH lattr DO               (* DESCRIBE  PREVIOUS *)
		    BEGIN
		      IF typtr <> NIL THEN
		        BEGIN                 (* NO PREV. FATAL ERROR *)
			IF typtr^.form <> arrays THEN
			  BEGIN
			    typtr := NIL ;
			    error (142) ;
			  END (* ERR *) ELSE
			  WITH typtr^ DO
			    IF aeltype <> NIL THEN
			      BEGIN         (* ARRAYS *)
			        IF conformantdim (typtr) THEN
				BEGIN
				  IF symbolmap THEN
				    BEGIN
				      nameisref (pthigh, oldfile, oldline) ;
				      nameisref (ptlow, oldfile, oldline) ;
				    END ;
				  IF lattr.descreg = nreg THEN
				    BEGIN
				      lp := typtr ;
				      WHILE conformantdim (lp^.aeltype) DO
				        BEGIN
					locdopevectordisp := locdopevectordisp + 3 ;
					lp := lp^.aeltype
				        END ;
				      init_desc_address (locvariabctptr, lattr) ;
				    END ;
				END ;
			        smallelem := cadrage < bytesinword ;
			        pckd := smallelem OR ((aeltype^.form = pointer)
				AND pack) ;
			        isconform := conformant ;
			        nextdimisconform := conformantdim (aeltype) ;
			      END (* ARRAYS *) ;
		        END (* TYPTR <>nil *) ;
		      arraytype := typtr ;
		    END (* with LATTR *) ;
                                                  (* *)
                                                  (* ANALYSIS  FOR CURRENT *)
                                                  (* INDEX EXPRESSION *)
                                                  (* (* *)

		  insymbol ; expression ;

		  IF gattr.typtr <> NIL THEN
		    IF arraytype <> NIL THEN
		      WITH gattr, arraytype^ DO
		        BEGIN
			compatbin (arraytype^.inxtype, typtr, generic) ;
			IF (generic = NIL) OR (generic = realptr) THEN
			  error (139) ELSE
			  BEGIN             (* TYPES COMPAT *)

$OPTIONS compile = trace $
			    IF stattrace = high THEN
			      BEGIN
			        write (mpcogout, '&&& Variable. Break point 3.') ; nextline ;
			        write (mpcogout, '    SMALLELEM =', smallelem : 8, ' PCKD = ', pckd : 8,
				' ISCONFORM = ', isconform : 8) ; nextline ;
			        write (mpcogout, ' GATTR Follows:') ; nextline ;
			        printattr (gattr) ;
			        write (mpcogout, ' LATTR Follows:') ; nextline ;
			        printattr (lattr) ;
			        write (mpcogout, '&&& Variable. Break point 3 .Fin.') ; nextline ;
			      END ;
$OPTIONS compile = true $
			    IF isconform THEN
			      BEGIN
			        IF gattr.kind = sval THEN
				checkminmax (gattr.val, arraytype^.inxtype, 302) ;
			        transfer (gattr, inq) ;
			        checkismade := false ; destused := inq ; regused := rq ;
			      END (* ISCONFORM *) ELSE
			      BEGIN         (* STANDARD ARRAY *)
			        subarraysize := subsize ;
			        twopower := opt2 ;
			        pointzero := lattr.dplmt - lo * subarraysize ;
                                                  (* FIND DESTINATION REGISTER *)
			        IF twopower >= 2 (* SIZE 4,8,16... *) THEN
				CASE kind OF
				  lval :
				    regused := ldreg ;
				  sval, lcond, varbl :
				    IF raisused THEN
				      regused := rq ELSE
				      regused := ra ;
				END (* CASE KIND,TWOPOWER>=2 *) ELSE
				regused := rq ; (* MULTIPLICAND in RQ *)
			        IF regused = ra THEN
				destused := inacc ELSE
				destused := inq ;
			      END (* STANDARD ARRAYS *) ;
			    IF (kind = sval) THEN
			      BEGIN
			        arrayboundsctp^.nmin := lo ;
			        arrayboundsctp^.nmax := hi ;
			        checkminmax (val, arrayboundsctp, 302) ;
			        checkismade := true ;
			        IF lattr.pckd THEN
				transfer (gattr, destused) ;
			      END ELSE
			      checkismade := false ;
			    IF kind = sval THEN (* ONLY STANDARD *)
			      BEGIN
			        lattr.dplmt := pointzero + val * subarraysize ;
			      END (* SVAL *) ELSE
			      BEGIN         (* NOT SVAL *)
			        IF inxcheck THEN
				BEGIN
				  transfer (gattr, destused) ;
				  IF isconform THEN
				    BEGIN
				      regenere (lattr.descbloc) ;
				      genstand (lattr.descreg, locdopevectordisp, icmpq, tn) ;
				      lmin := indfich ; genstand (nreg, 0, itmi, tic) ;
				      genstand (lattr.descreg, locdopevectordisp + 1, icmpq, tn) ;
				    END ELSE
				    IF NOT checkismade THEN
				      BEGIN
				        lcomp := opaq [cmp, regused] ;
				        gencstecode (lo, lcomp) ;
				        lmin := indfich ;
				        genstand (nreg, 0, itmi, tic) ; (* ERR if NEG ON *)
				        gencstecode (hi, lcomp) ;
				      END (* NOT CONF *) ;
                                                  (* COMMON  SECTION *)
				  IF NOT checkismade THEN
				    BEGIN
				      lmax := indfich ;
				      genstand (nreg, 0, itmoz, tic) ; (* OK if <= *)
				      inser (cb, lmin) ; genexceptcode (inxerrcode, ldreg) ;
				      inser (cb, lmax) ;
				    END ;
				END (* INXCHECK *) ELSE
				IF NOT isconform THEN
				  BEGIN   (* NOT INXCHECKS *)
				    totransfer := true ;
				    IF kind = varbl THEN
				      IF vlev <> 0 THEN
				        IF subarraysize = bytesinword THEN
					IF varissimple (gattr) THEN
					  IF lattr.inxmem = 0 THEN
					    BEGIN
					      totransfer := false ;
					      lattr.dplmt := pointzero ;
					      lattr.inxmem := dplmt ;
					      lattr.inxmemrw := false ; (* READ-ONLY *)
					    END ;
				    IF totransfer THEN
				      transfer (gattr, destused) ;
				  END (* NOT INXCHECK *) ;
			      END (* NOT SVAL *) ;
                                                  (* NOW INDEX IS in REGUSED , *)
                                                  (* EXCEPT   SVAL   endED *)
                                                  (* VARBL  INXMEMRW F   endED *)

$OPTIONS compile = trace $
			    IF stattrace = high THEN
			      BEGIN
			        write (mpcogout, '&&& Variable. Break point 2.') ; nextline ;
			        write (mpcogout, '    REGUSED =', ord (regused) : 4,
				'    DESTUSED  =', ord (destused) : 4) ; nextline ;

			        write (mpcogout, ' GATTR Follows:') ; nextline ;
			        printattr (gattr) ;
			        write (mpcogout, ' LATTR Follows:') ; nextline ;
			        printattr (lattr) ;
			        write (mpcogout, '&&& Variable. Break point 2 .Fin.') ; nextline ;
			      END ;
$OPTIONS compile = true $
			    IF kind = lval THEN (* COMPUTE DISP *)
			      BEGIN
			        IF isconform THEN
				BEGIN
                                                  (* Zero point correction *)
				  regenere (lattr.descbloc) ;
				  genstand (lattr.descreg, locdopevectordisp, isbq, tn) ;
				  sauvereg (ra, false) ;
				  genstand (lattr.descreg, locdopevectordisp + 2, impy, tn) ;
				  IF NOT nextdimisconform THEN
				    BEGIN
				      freebloc (lattr.descbloc) ;
				      lattr.descreg := nreg ;
				    END ;

				  regenere (lattr.basebloc) ;
				  IF pack THEN genstand (lattr.basereg, 0, iabd, tql)
				  ELSE genstand (lattr.basereg, 0, iawd, tql) ;
				  freebloc (gattr.ldregbloc) ;
				  regused := nreg ;

				  locdopevectordisp := locdopevectordisp - 3 ; (* Next dim *)

				END ELSE
				BEGIN     (* STANDARD *)
                                                  (* COMMON PART *)
                                                  (* ZERO POINT CORRECTION *)
				  IF lo <> 0 THEN
				    BEGIN
				      IF (NOT smallelem) AND
				        inbounds (pointzero, -twoto16, twoto16 - 1) THEN
				        lattr.dplmt := pointzero ELSE
				        gencstecode (lo, opaq [sub, regused]) ;
				    END (* LO<>0 *) ;
				  IF NOT smallelem THEN
				    BEGIN (* WORD DISP *)
				      IF twopower > 2 (*   8 16 32 .. *) THEN
				        genstand (nreg, twopower - 2, opaq [shiftl, regused], tn) ELSE
                                                  (* TWOPOWER =2    NO-OP ; *)
                                                  (* 0,1  IMPOSSIBLE  HERE *)
				        IF twopower < 0 THEN
					BEGIN
					  transfer (gattr, inq) ;
					  sauvereg (ra, false) ;
					  genstand (nreg, subarraysize DIV bytesinword, impy, tdl) ;
					  regused := rq ;
					END (* TWOPOWER < 0 *) ;
				    END (* WORD DISP *) ELSE
				    BEGIN (* PACKED *)
                                                  (* ADD BYTES DISP TO A PR *)
				      loadadr (lattr, nreg) ;
				      WITH lattr DO (* CAUTION NESTED ATTR *)
				        BEGIN
					basereg := currentpr ; basebloc := currentbloc ;
					dplmt := 0 ;
					inxreg := nxreg ; inxbloc := NIL ;
					inxmem := 0 ; inxmemrw := true ;
					itsdplmt := 0 ; access := pointee ;
				        END (* NESTED *) ;
                                                  (* RA RQ BECOMES "BYTES" DISP *)
				      IF twopower > 0 THEN
				        genstand (nreg, twopower, opaq [shiftl, regused], tn) ELSE
                                                  (* NO-OP  FOR BYTE *)
				        IF twopower < 0 THEN
					BEGIN
					  transfer (gattr, inq) ;
					  sauvereg (ra, false) ;
					  gencstecode (subarraysize, impy) ;
					  regused := rq ;
					END (* < 0 *) ;
				      IF size >= twoto15 THEN
				        BEGIN
					IF regused = ra THEN
					  BEGIN
					    sauvereg (rq, false) ;
					    regused := rq ; (* SEE A9BD LATER *)
					    genstand (nreg, 2, ilrl, tn) ;
					  END (* RA *) ELSE
					  BEGIN
					    sauvereg (ra, false) ;
					    genstand (nreg, 34, ills, tn) ;
					  END ;
					genstand (nreg, 34, iqrl, tn) ;
                                                  (* NOW  A=WORD  DISP. *)
                                                  (*     Q=BYTES DISP. *)
					genstand (currentpr, 0, iawd, tal) ;
				        END (* LONG *) ;
				      genstand (currentpr, 0, ia9bd, modif [regused]) ;
				      freebloc (ldregbloc) ;
				      regused := nreg ;
				    END (* PACKED *) ;
				END (* STANDARD ARRAY *) ;

$OPTIONS compile = trace $
			        IF stattrace = high THEN
				BEGIN
				  write (mpcogout, '&&& Variable. Break point 1.') ; nextline ;
				  write (mpcogout, '    REGUSED =', ord (regused) : 4,
				    '    DESTUSED  =', ord (destused) : 4) ; nextline ;

				  write (mpcogout, ' GATTR Follows:') ; nextline ;
				  printattr (gattr) ;
				  write (mpcogout, ' LATTR Follows:') ; nextline ;
				  printattr (lattr) ;
				  write (mpcogout, '&&& Variable. Break point 1 .Fin.') ; nextline ;
				END ;
$OPTIONS compile = true $
			        IF regused <> nreg THEN
				WITH lattr DO (* CAUTION   NESTED ATTR *)
				  BEGIN
				    IF inxreg = nxreg THEN
				      BEGIN
				        inxreg := regused ; (* BITS  18..35 *)
				        inxbloc := gattr.ldregbloc ;
				      END (* NXREG *) ELSE
				      IF inxreg = regused THEN
				        BEGIN (* NECESSARY  SAVED *)
					IF inxmem = 0 THEN
					  BEGIN
					    inxmem := inxbloc^.saveplace ;
					  END (* INXMEM=0 *) ELSE
					  BEGIN (* <>0 *)
                                                  (* ADD SAVED OLD INDEX AT NEW INDEX *)
					    genstand (pr6, inxbloc^.saveplace DIV bytesinword,
					      opaq [add, regused], tn) ;
					  END (* <>0 *) ;
					freebloc (inxbloc) ;
					inxbloc := gattr.ldregbloc ;
				        END (* NECESSARY SAVED *) ELSE
                                                  (* OLD INDEX IS OTHER  *)
                                                  (* REGISTER  A <==>Q *)
				        IF inxmem = 0 THEN
					BEGIN
					  IF inxbloc^.saveplace <> 0 THEN
					    BEGIN
					      inxreg := regused ;
					      inxmem := inxbloc^.saveplace ;
					      freebloc (inxbloc) ;
					      inxbloc := gattr.ldregbloc ;
					    END ELSE
					    BEGIN (* OLD INDEX NOT SAVED *)
					      inxmem := oldnewstor (bytesinword) ;
					      genstand (pr6, inxmem DIV bytesinword, ista, tn) ;
					      IF inxreg = rq THEN
					        freebloc (gattr.ldregbloc) ELSE
					        BEGIN
						freebloc (inxbloc) ; inxreg := rq ;
						inxbloc := gattr.ldregbloc ;
					        END ;
					    END (* OLD INDEX NOT SAVED *) ;
					END (* INXMEM=0 *) ELSE
					BEGIN (* INXMEM <> 0 *)
					  IF inxbloc^.saveplace <> 0 THEN
					    BEGIN (* OLD SAVED *)
					      genstand (pr6, inxbloc^.saveplace DIV bytesinword,
					        opaq [add, regused], tn) ;
					      freebloc (inxbloc) ;
					      inxbloc := gattr.ldregbloc ;
					      inxreg := regused ;
					    END (* OLD SAVED *) ELSE
					    BEGIN (* OLD NOT SAVED *)
					      IF inxmemrw THEN
					        genstand (pr6, inxmem DIV bytesinword, iasa, tn) ELSE
					        BEGIN (* READ-ONLY STORAGE *)
						genstand (pr6, inxmem DIV bytesinword, iada, tn) ;
						inxmem := oldnewstor (bytesinword) ;
						inxmemrw := true ;
						genstand (pr6, inxmem DIV bytesinword, ista, tn) ;
					        END ;
					      IF regused = rq THEN
					        BEGIN
						freebloc (inxbloc) ; inxreg := rq ;
						inxbloc := gattr.ldregbloc ;
					        END ELSE
					        freebloc (gattr.ldregbloc) ;
					    END (* OLD NOT SAVED *) ;
					END (* INXMEM<>0 *) ;
				  END (* with LATTR ==>   with GATTR *) ;
			      END (* GATTR.KIND=LVAL *) ;
			  END (* TYPES COMPAT *) ;

			lattr.typtr := aeltype ; (* GET NEXT DIM *)

		        END (* with GATTR,ARRAYTYPE^, NO TYPE ERROR *) ;
		  IF no = 15 (* , *) THEN
		    stoprepeat := false ELSE
		    IF no = 12 (* ] *) THEN
		      BEGIN
		        insymbol ; stoprepeat := no <> 11 ; (* [ *)
		      END ELSE
		      BEGIN
		        insymbol ;
		        error (12) ; stoprepeat := true ;
		      END ;
		UNTIL stoprepeat ;
	        END ;
	      withvariable := savewithflag ;
	      previouswasarrow := false ;
	    END (* NO=11 ARRAY ELEMENT *) ELSE
	    IF no = 17 (* . RECORD FIELD *) THEN
	      BEGIN
	        IF symbolmap THEN
		BEGIN
		  enterref ;
		  IF previouswasarrow THEN
		    BEGIN
		      FOR it := 1 TO refs.nbr DO
		        WITH refs.ref [it] DO
			nameisref (symbp, rfile, rline) ;
		      refs.nbr := 0 ;
		    END
		END ;
	        insymbol ;
	        oldfile := symbolfile ; oldline := symbolline ; oldptr := NIL ;
	        IF no <> 1 THEN                 (* NOT ID *)
		BEGIN
		  error (2) ; lattr.typtr := NIL ;
		END ELSE
		WITH lattr DO
		  BEGIN
		    IF typtr <> NIL THEN
		      IF typtr^.form <> records THEN
		        BEGIN
			error (140) ; typtr := NIL ;
		        END ELSE
		        BEGIN
			srchrec (typtr^.fstfld) ;
			IF ctptr = NIL THEN
			  BEGIN
			    error (152) ; typtr := NIL ;
			  END ELSE
			  BEGIN
			    nameaddr := ctptr ;
			    oldptr := ctptr ;
			    WITH ctptr^ DO
			      BEGIN
			        dplmt := dplmt + fldaddr ;
			        pckd := false ;
			        IF typtr^.pack THEN
				IF (bytwidth < bytesinword) OR ((fldtype^.form = power) AND
				  (bytwidth <= bytesindword)) THEN
				  pckd := true ELSE
				  IF fldtype^.form = pointer THEN
				    pckd := true ;
			        typtr := fldtype ;
			        IF pckd THEN
				IF access = direct THEN
				  access := pointee ;
			      END (* with CTPTR, CTPTR<>nil *) ;
			  END ;
		        END (* NO TYPE ERROR *) ;
		    insymbol ;
		  END (* with LATTR, NO=1 *) ;
	        previouswasarrow := false ;
	      END (* NO=17 *) ELSE
	      BEGIN (* NO=18 *)                 (* ^  FILE or POINTER *)
	        WITH lattr DO
		IF typtr <> NIL THEN
		  IF typtr^.form = pointer THEN
		    BEGIN
		      totransfer := false ;
		      IF access = pointable THEN
		        totransfer := true ELSE
		        IF access = direct THEN
			BEGIN
			  IF (inxmem <> 0) OR (inxreg <> nxreg) THEN
			    totransfer := true ;
			END ELSE
			IF access = pointee THEN
			  IF pckd OR (inxmem <> 0) OR (inxreg <> nxreg) THEN
			    totransfer := true ;
		      IF totransfer THEN
		        transfer (lattr, inpr) (* BECOMES POINTEE *) ELSE
		        BEGIN
			itsdplmt := dplmt ; dplmt := 0 ; access := pointable ;
		        END ;
		      typtr := typtr^.eltype ;
		    END (* POINTER *) ELSE
		    IF typtr^.form = files THEN
		      BEGIN
		        IF interactive THEN
			IF typtr = textfilectp THEN
			  BEGIN
			    IF basereg <> pr5 THEN
			      BEGIN
			        sauvereg (pr5, true) ;
			        freebloc (basebloc) ;
			        basebloc := currentbloc ;
			      END ;
			    genstand (basereg, itsdplmt DIV bytesinword, iepp5, tny) ;
			    basereg := pr5 ;
			    access := pointee ; itsdplmt := 0 ; dplmt := 0 ;
			    genstand (pr0, checkbeforetextreferenceplace, itsp3, tn) ;
			  END ;
		        dplmt := fdescsize ;
		        pckd := false ;
		        IF typtr^.pack THEN
			IF packedsize (typtr^.feltype) < bytesinword THEN
			  pckd := true ELSE
			  IF typtr^.feltype^.form = pointer THEN
			    pckd := true ;
		        typtr := typtr^.feltype ;
		        IF pckd THEN
			IF access = direct THEN
			  access := pointee ;
		      END (* FILES *) ELSE
		      BEGIN
		        error (141) ; typtr := NIL ;
		      END ;
	        insymbol ;
	        previouswasarrow := true ;
	      END (* NO=18 *) ;
	END (*  while  NO in [11,17,18] *) ;
        IF symbolmap THEN
	BEGIN
	  enterref ;
	  IF previouswasarrow THEN
	    BEGIN
	      FOR it := 1 TO refs.nbr DO
	        WITH refs.ref [it] DO
		nameisref (symbp, rfile, rline) ;
	      refs.nbr := 0 ;
	    END
	  ELSE
	    FOR it := 1 TO refs.nbr DO
	      WITH refs.ref [it] DO
	        IF fvarset THEN nameisref (symbp, rfile, -rline)
	        ELSE nameisref (symbp, rfile, rline) ;
	  IF withvariable THEN
	    BEGIN
	      currwithlist.nbr := refs.nbr ;
	      FOR it := 1 TO refs.nbr DO
	        currwithlist.symbolp [it] := refs.ref [it].symbp ;
	    END ;
	END ;
        gattr := lattr ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  IF (stattrace = high) AND newattr THEN
	    printattr (gattr) ;
	  write (mpcogout, '^^^ FIN VARIABLE with NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* VARIABLE *) ;

$OPTIONS page $
    PROCEDURE passparams (fctplace : integer) ;

(* C. CALLED IN ORDER  TO
   . ANALYSE  ACTUAL PARAMETERS  FOR  A  PROCEDURE, FUNCTION CALL
   . BUILD   ARGUMENT'S LIST
   * STANDARD HEADER
   * POINTERS  LIST   ON PARAMETERS
   * FOR A FUNCTION  ONE MORE "ITS" POINTING THE  PLACE TO BE
   ASSIGNED
   . FOR   ACTUAL  PROCEDURE(FUNCTION) PARAMETER   TWO   "ITS" ARE GIVEN
   *  THE  RIGHT ITS  IN LINKAGE SECTION
   *  THE COMPUTED DYNAMIC LINK
   . FOR   A CONFORMANT   ARRAY       FOUR ITEMS
   * "ITS"  ON  REAL ARRAY
   * LOW BOUND,  HIGHBOUND,  DIM SIZE IN WORDS
   FCTPLACE  IS  THE DISP IN CALLER FRAME WHERE RETURNED VALUE MUST BE PUT
   . CTPTR POINTS  THE BOX "PROC" OF THE CALLED PROCEDURE
   . FIRST INSYMBOL  ALREADY DONE
   C *)
(* E ERRORS DETECTED
   4: ")" EXPECTED
   15: "," EXPECTED
   28: PREDEF PROC/FUNCT NOT ALLOWED HERE
   103: UNAPPROPRIATE CLASS FOR ID.
   104: UNDECLARED ID.
   126: NUMBER OF PARAMETERS  DOES  NOT AGREE WITH  DECLARATION
   127: ILLEGAL PARAMETER SUBSTITUTION
   128: PARAMETER CONFLICT IN FORMAL PROC.
   133: ILLEGAL CONFORMANT ARRAY SUBSTITUTION
   230 : EFFICTIVE PARAMETER PASSED BY VALUE CAOONT BE CONFORMANT ARRAY
   303: VALUE ASSIGNED OUT OF RANGE
   318: PARAMETER PROCEDURE PASSED TO AN EXTERNAL PROCEDURE MUST BE EXPORTABLE
   E *)
      LABEL
        2,
        1 ;                                       (* EXIT PROC WHEN FATAL ERROR *)
                                                  (* IS DETECTED *)
      VAR
        itisafunct, pisformal, pisext, paramisproc, paramisvar, ended, lerr : boolean ;
        procnameaddr, parmctp, foundtype, decltype, generic : ctp ;
        plevel, procplacew, nbparm, longlist, deplist, curritsw, currparmw, currparb : integer ;
        lbase : preg ;
        declsize, foundsize, ldisp, lmod, lpad, suplr : integer ;
        ltag, lftag, rgtag : tag ;
        lattr : attr ;
        lretpt : lcstpt ; llretpt : llcstpt ;
        prevdecltype : ctp ;
        temppt, tempact : ctp ;
        nbofdim : integer ;
        locdisp : integer ;
        prevfoundtype : ctp ;
        dvdispw : integer ;
        multiplier, lowbound, highbound : integer ;
        arrconfblockw : integer ;
        firstoflist : boolean ;
        wlength, alfalow, alfahigh : integer ;
        all_descriptors, pisimported : boolean ;
        procbox : ctp ;
        pr5bloc : regpt ;
        formal_length : integer ;
        done : boolean ;
        nbofparm : integer ; parm_attr : attr ;
        loaded_reg : register ;


(* ************************************ LOADLINK < PASSPARAMS ***************** *)

      PROCEDURE loadlink (fpreg : preg ; fplev : levrange) ;

(* C.LOAD  FPREG  WITH THE DYNAMIC LINK  SUITABLE.
   .THREE CASES
   *CURRENT  LEVEL=  CALLED-LEVEL
   CALL OF A SUBPROCEDURE    DYN-LINK = PR6 OF CALLER
   * OR      SEARCHS   PREVIOUS CALLER(S)  D-LINK
   .CAUTION     WHEN LEVEL IS N,  PROCLEVEL IS N-1
   C *)
        VAR
	linst : istand ;
	it : integer ;
        BEGIN
	linst := prinst [epp, fpreg] ;
	IF level = fplev THEN
	  genstand (pr6, 0, linst, tn) ELSE
	  BEGIN
	    genstand (pr6, dlkdepw, linst, tny) ;
	    FOR it := 1 TO level - fplev - 1 DO
	      genstand (fpreg, dlkdepw, linst, tny) ;
	  END ;
        END (* LOADLINK *) ;


(* ************************************ FCT COMPATLIST< PASSPARAMS ************ *)

      FUNCTION compatlist (declproc, foundproc : ctp) : boolean ;

(* C .DECLPARM   POINTS  THE PROCEDURE BOX
   FOUNDPARM  POINTS  THE     "      "
   .RETURNS  TRUE  OR FALSE
   C *)
        VAR
	iscompat, lerr, lerrvarval : boolean ;
	declparm, foundparm : ctp ;
	decltype, foundtype : ctp ;

        FUNCTION both_are_string_param : boolean ;

(* SAYS IF BOTH PARAMETERS ARE DECLARED " : STRING" *)

	BEGIN
	  IF (decltype^.father_schema <> NIL) AND (decltype^.actual_parameter_list <> NIL)
	    AND (decltype^.father_schema = foundtype^.father_schema)
	    AND (foundtype^.actual_parameter_list <> NIL)
	  THEN
	    both_are_string_param :=
	      (decltype^.actual_parameter_list^.vkind = arraybound)
	    AND (foundtype^.actual_parameter_list^.vkind = arraybound)
	  ELSE both_are_string_param := false
	END ;
        BEGIN                                     (* COMPATLIST *)
$OPTIONS compile = trace $
	IF stattrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ DEBUT COMPATLIST @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
	declparm := declproc@.formals ; foundparm := foundproc@.formals ;
	iscompat := true ; lerrvarval := false ;
	WHILE (declparm # NIL) AND iscompat DO
	  BEGIN
	    IF foundparm = NIL THEN
	      iscompat := false ELSE
	      BEGIN
	        IF declparm@.klass # foundparm@.klass THEN
		iscompat := false ELSE
		BEGIN
		  IF declparm@.klass = proc THEN
		    iscompat := compatlist (declparm, foundparm) ELSE
		    BEGIN
		      IF declparm@.varparam # foundparm@.varparam THEN
		        lerrvarval := true ;
		      decltype := declparm@.vtype ; lerr := false ;
		      foundtype := foundparm@.vtype ;
		      WHILE (decltype # foundtype) AND NOT lerr DO
		        BEGIN
			lerr := true ;
			IF decltype # NIL THEN
			  IF foundtype # NIL THEN
			    IF both_are_string_param THEN
			      BEGIN
			        decltype := foundtype ; (* TO STOP WHILE LOOP *)
			        lerr := false (* SCHEMA OK *)
			      END
			    ELSE
			      IF decltype@.form = arrays THEN
			        IF foundtype@.form = arrays THEN
				IF decltype@.conformant THEN
				  IF foundtype@.conformant THEN
				    IF decltype@.inxtype = foundtype@.inxtype THEN
				      IF decltype^.pack = foundtype^.pack THEN
				        BEGIN
					lerr := false ;
					decltype := decltype@.aeltype ;
					foundtype := foundtype@.aeltype ;
				        END ; (* EQUIVALENT CONFORMANT SCHEMAS *)
		        END ;                 (* TYPES # AND NO ERR *)
		      iscompat := NOT (lerr OR lerrvarval) ;
		    END (* NOT PROC *) ;
		  declparm := declparm@.nxtel ; foundparm := foundparm@.nxtel ;
		END (* SAME KLASS *) ;
	      END (* FOUNDPARM#NIL *) ;
	  END (* WHILE *) ;
	IF (declparm = NIL) AND (foundparm # NIL) THEN iscompat := false ;

(* NOW  CHEK IF IT IS TWO PROC OR TWO FUNCTIONS *)
	IF iscompat THEN
	  IF declproc@.proctype # declproc THEN
	    BEGIN
	      IF foundproc@.proctype = foundproc THEN
	        iscompat := false ELSE
	        iscompat := declproc@.proctype = foundproc@.proctype ;
	    END ELSE
	    iscompat := foundproc@.proctype = foundproc ;
	compatlist := iscompat ;
$OPTIONS compile = trace $
	IF stattrace > low THEN
	  BEGIN
	    write (mpcogout, '@@@ FIN COMPATLIST @@@ WITH RETURNED VALUE ', iscompat : 6) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
        END (* COMPATLIST *) ;





      BEGIN                                       (* PASSPARAMS *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT PASSPARAMS @@@ WITH FCTPLACE', fctplace) ; nextline ;
	END ;
$OPTIONS compile = true $
        WITH ctptr@ DO
	BEGIN
	  itisafunct := proctype # ctptr ;
	  parmctp := formals ;                  (* FIRST DECLARED PARAMETER *)
	  pisformal := prockind = formal ;
	  pisext := prockind > formal ;
	  pisimported := prockind = imported ;
	  plevel := proclevel ;
	  procplacew := procaddr DIV bytesinword ; (* PR4 PR6  WORD OFFSET *)
	  nbparm := nbparproc ;                 (* NUMBER OF "ITS" IN PARAMETER LIST *)
	  all_descriptors := pwantdescs ;
	  procbox := ctptr ;
	END (* WITH CTPTR@ *) ;
        procnameaddr := ctptr ;
                                                  (* PREPARE CURRENT ARGUMENT LIST *)
        longlist := bytesindword (* HEADER *) + nbparm * bytesindword ;
        IF ctptr^.phasdescriptor OR all_descriptors THEN
	longlist := longlist + nbparm * bytesindword ;
        deplist := oldnewstor (longlist) ;        (* POINTED LATER BY ARGUMENT POINTER *)
        curritsw := (deplist + bytesindword) DIV bytesinword ;
        IF all_descriptors AND (nbparm <> 0) THEN
	BEGIN
	  newbloc (pr5) ;
	  pr5bloc := currentbloc ;
	  usednameaddr := procnameaddr ;
	  genstand (prlink, procplacew, iepp5, tny) ;
	  mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
	  geneism (imlr, 0, p0t0r0) ;
	  gendesca (prstatic, procbox^.pdescsaddrplace DIV bytesinword, 0, l9, nbparm * bytesindword, tn) ;
	  gendesca (pr6, deplist DIV bytesinword + 2 + nbparm * 2, 0, l9, nbparm * bytesindword, tn) ;
	END ;
        IF no = 9 (* ( *) THEN
	BEGIN
	  prevdecltype := NIL ; prevfoundtype := NIL ;
	  dvdispw := 0 ; arrconfblockw := 0 ;
	  REPEAT                                (* LOOP  ON ACTUAL  PARAMETER'S  LIST *)
	    IF parmctp = NIL THEN
	      BEGIN
	        error (126) ; skip (46) ; GOTO 1 ; (* EXIT PROC *)
	      END ;
	    paramisproc := parmctp@.klass = proc ;
	    IF NOT paramisproc THEN
	      paramisvar := parmctp@.varparam ;
	    insymbol ;
	    IF paramisproc THEN
	      BEGIN                             (* PROC OR FUNCT TO BE PASSED *)
	        IF no # 1 THEN
		BEGIN error (2) ; skip (15) ; (* , *)
		END ELSE
		BEGIN                         (* ID *)
		  search ;
		  IF ctptr = NIL THEN
		    error (104) ELSE
		    BEGIN
		      IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		      WITH ctptr@ DO
		        IF klass # proc THEN
			error (103) ELSE
			IF NOT compatlist (parmctp, ctptr) THEN
			  error (128) ELSE
			  BEGIN
			    IF predefproc THEN error (28) ;
			    IF prockind # formal THEN (* ACTUAL PROCEDURE PASSED *)
			      BEGIN
			        currparmw := oldnewstor (procparmsize) DIV bytesinword ;
			        IF proclevel = level THEN
				genstand (pr6, currparmw + 2, ispri6, tn) ELSE
				BEGIN
				  loadlink (pr3, proclevel) ;
				  genstand (pr6, currparmw + 2, ispri3, tn) ;
				END ;
			        genstand (prlink, procaddr DIV bytesinword, iepp3, tny) ;
			        genstand (pr6, currparmw, ispri3, tn) ;
			        IF prockind > formal THEN ldisp := extcallplace
			        ELSE IF pisimported THEN error (318) ELSE ldisp := intcallplace ;
			        genstand (nreg, ldisp, ilda, tdl) ;
			        genstand
				(pr6, currparmw + 4, ista, tn) ; (* USED IN CALL SEQ. *)
                                                  (* NOW LOAD PR3 *)
                                                  (* WITH "ITS" ON CURRPARM *)
			        usednameaddr := ctptr ;
			        genstand (pr6, currparmw, iepp3, tn) ;
			        genstand (pr6, curritsw, ispri3, tn) ;
			      END (* NOT FORMAL *) ELSE
			      BEGIN         (* FORMAL *)
			        IF proclevel = level THEN
				lbase := pr6 ELSE
				BEGIN
				  loadbase (proclevel) ; freebloc (currentbloc) ;
				  lbase := currentpr ;
				END ;
			        usednameaddr := ctptr ;
			        genstand (lbase, procaddr DIV bytesinword, iepp3, tny) ;
			        genstand (pr6, curritsw, ispri3, tn) ;
			        IF pisimported THEN
				BEGIN
				  genstand (pr3, 4, ilda, tn) ; (* CALL OP NUMBER *)
				  genstand (pr0, parmproccheckplace, itsp3, tn) ;
				END ;
			      END (* FORMAL *) ;
			    curritsw := curritsw + 2 ;
			  END (* NO ERRORS  IN PASSING A PROCEDURE/FUNCTION AS PARAMETER *) ;
		    END ;
		  insymbol ;
		END (* ID *) ;
	      END (* PARAMISPROC *) ELSE
	      IF paramisvar THEN
	        BEGIN
		variab (true) ;
		done := false ;
		WITH gattr DO
		  IF typtr # NIL THEN
		    IF parmctp^.vtype <> NIL THEN
		      BEGIN
		        IF (parmctp^.vtype^.father_schema <> NIL) THEN
			IF (parmctp^.vtype^.actual_parameter_list = NIL) THEN (* nothing *) ELSE
			  IF (parmctp^.vtype^.actual_parameter_list^.vkind = arraybound) THEN
                                                  (* FORMAL PARAMETER IS A SCHEMA. PASS ACTUAL BOUNDS IN DESCRIPTOR *)
			    IF (typtr^.father_schema <> parmctp^.vtype^.father_schema) THEN error (127)
			    ELSE
			      BEGIN
			        decltype := parmctp^.vtype ; foundtype := typtr ;
			        lerr := false ;
			        IF prevdecltype = decltype THEN
				BEGIN     (* list of parameters of same schema *)
				  firstoflist := false ;
				  IF prevfoundtype <> foundtype THEN
				    BEGIN
				      lerr := true ; error (127) ;
				    END ;
				END ELSE
				BEGIN
				  firstoflist := true ;
				  prevfoundtype := foundtype ;
				  prevdecltype := decltype ;
				END ;
			        IF NOT lerr THEN
				BEGIN
				  IF foundtype^.actual_parameter_list^.vkind = arraybound THEN
				    BEGIN (* PASSED PARAMETER IS ITSELF A VARIABLE SCHEMA. KEEP HIS DESC *)
				      IF gattr.descbloc = NIL THEN
				        BEGIN
					init_desc_address (variabctptr, gattr) ;
					genstand (pr6, curritsw, prinst [spri, gattr.basereg], tn) ;
					freebloc (gattr.basebloc) ;
				        END
				      ELSE
				        BEGIN
					loadadr (gattr, pr3) ;
					genstand (pr6, curritsw, ispri3, tn) ;
				        END ;
				      IF firstoflist THEN
				        BEGIN
					temppt := decltype^.actual_parameter_list ;
					nbofparm := 0 ;
					WHILE temppt <> NIL DO
					  BEGIN
					    nbofparm := nbofparm + 1 ; ; temppt := temppt^.nxtel
					  END ;
					wlength := 2 (* MULTICS EXTENDED ARG DESC *) + 1 (* SIZE *) + nbofparm (* ONE WORD PER PARM *) ;
					dvdispw := oldnewstor (wlength * bytesinword) DIV bytesinword ;
					regenere (gattr.descbloc) ;
					mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
					geneism (imlr, 0, p0t0r0) ;
					gendesca (gattr.descreg, 2, 0, l9, (wlength - 2) * bytesinword, tn) ;
					gendesca (pr6, dvdispw + 2, 0, l9, (wlength - 2) * bytesinword, tn) ;
				        END ;
				      freebloc (gattr.descbloc) ;
				      getpr ;
				      genstand (pr6, dvdispw, prinst [epp, currentpr], tn) ;
				      genstand (pr6, curritsw + nbparm * 2, prinst [spri, currentpr], tn) ;
				      freebloc (currentbloc) ;
				      curritsw := curritsw + 2 ;
				    END (* Actual is Schema *) ELSE
				    BEGIN
				      loadadr (gattr, pr3) ;
				      genstand (pr6, curritsw, ispri3, tn) ;

				      IF firstoflist THEN
				        BEGIN
                                                  (* Evaluation du nombre de parametres *)
					temppt := decltype^.actual_parameter_list ;
					nbofparm := 0 ;
					WHILE temppt <> NIL DO
					  BEGIN
					    nbofparm := nbofparm + 1 ; ; temppt := temppt^.nxtel
					  END ;
					wlength := 2 (* MULTICS EXTENDED ARG DESC *) + 1 (* SIZE *) + nbofparm (* ONE WORD PER PARM *) ;
					dvdispw := oldnewstor (wlength * bytesinword) DIV bytesinword ;
					temppt := typtr^.actual_parameter_list ; tempact := foundtype ; locdisp := 2 ;
					gencstecode (typtr^.size, ilda) ;
					genstand (pr6, dvdispw + locdisp, ista, tn) ;
					locdisp := locdisp + 1 ;
					WHILE temppt <> NIL DO
					  BEGIN
					    sauvereg (ra, false) ;
					    IF temppt^.klass <> konst THEN
					      BEGIN
					        addressvar (temppt, parm_attr, false) ;
					        transfer (parm_attr, inacc) ;
					        freeattr (parm_attr)
					      END
					    ELSE gencstecode (temppt^.values, ilda) ;
					    genstand (pr6, dvdispw + locdisp, ista, tn) ;

					    locdisp := locdisp + 1 ;
					    temppt := temppt^.nxtel ;
					  END ;
				        END (* FIRSTOFLIST *) ;

				      genstand (pr6, dvdispw, iepp3, tn) ;
				      genstand (pr6, curritsw + nbparm * 2, ispri3, tn) ;
				      curritsw := curritsw + 2 ;
				    END (* Actual not Schema *) ;
				END ;
			        done := true ;
			      END ;
		        IF NOT done THEN
			IF (gattr.pckd) AND (NOT parmctp^.vtype^.pack) THEN error (127) ELSE
			  IF typtr = parmctp@.vtype THEN
			    BEGIN
			      loadadr (gattr, pr3) ;
			      IF procnameaddr^.pwantspl1descriptors AND
			        is_pl1_varying_char (parmctp^.vtype) THEN
			        genstand (pr3, 1, iepp3, tn) ;
			      genstand (pr6, curritsw, ispri3, tn) ; curritsw := curritsw + 2 ;
			    END (* SAME TYPE *) ELSE
			    IF NOT conformantdim (parmctp^.vtype) THEN
			      error (127) ELSE
			      BEGIN         (* Not Same Type *)
			        decltype := parmctp^.vtype ; foundtype := gattr.typtr ;
			        lerr := false ;
			        IF NOT legalconfarrsubstitution (foundtype, decltype) THEN
				BEGIN
				  error (127) ;
                                                  (* SKIP BOUNDS PARAM *)
				  WHILE parmctp^.vkind = arraybound DO
				    BEGIN
				      parmctp := parmctp ^.nxtel ;
				    END ;
				END (* not Legal Substitution *) ELSE
				BEGIN
				  IF prevdecltype = decltype THEN
				    BEGIN (* Liste *)
				      firstoflist := false ;
				      IF prevfoundtype <> foundtype THEN
				        BEGIN
					lerr := true ; error (127) ;
				        END ;
				    END ELSE
				    BEGIN
				      firstoflist := true ;
				      prevfoundtype := foundtype ;
				      prevdecltype := decltype ;
				    END ;
				  IF NOT lerr THEN
				    BEGIN
				      IF conformantdim (foundtype) THEN
				        BEGIN
                                                  (* Load PR3 with previous descriptor on block parameter *)
					IF gattr.descbloc = NIL THEN
					  BEGIN
					    init_desc_address (variabctptr, gattr) ;
					    genstand (pr6, curritsw, prinst [spri, gattr.basereg], tn) ;
					    freebloc (gattr.basebloc) ;
					  END
					ELSE
					  BEGIN
					    loadadr (gattr, pr3) ;
					    genstand (pr6, curritsw, ispri3, tn) ;
					  END ;
					IF firstoflist THEN
					  BEGIN
					    temppt := decltype ;
					    nbofdim := 0 ;
					    WHILE conformantdim (temppt) DO
					      BEGIN
					        nbofdim := nbofdim + 1 ;
					        temppt := temppt^.aeltype ;
					        parmctp := parmctp^.nxtel^.nxtel ;
					      END ;
					    wlength := nbofdim * dopevectorsize DIV bytesinword ;
					    dvdispw := oldnewstor ((wlength + 2) * bytesinword) DIV bytesinword ;
					    IF all_descriptors THEN
					      BEGIN
					        getpr ;
					        genstand (pr6, curritsw + (nbparm * 2), prinst [epp, currentpr], tny) ;
					        mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
					        geneism (imlr, 0, p0t0r0) ;
					        gendesca (currentpr, 0, 0, l9, (wlength + 2) * bytesinword, tn) ;
					        gendesca (pr6, dvdispw, 0, l9, (wlength + 2) * bytesinword, tn) ;
					        freebloc (currentbloc) ;
					      END ;
					    regenere (gattr.descbloc) ;
					    geneism (imlr, 0, p0t0r0) ;
					    gendesca (gattr.descreg, 0, 0, l9, wlength * bytesinword, tn) ;
					    gendesca (pr6, dvdispw + 1, 0, l9, wlength * bytesinword, tn) ;
					  END ;
					freebloc (gattr.descbloc) ;

					getpr ;
					genstand (pr6, dvdispw, prinst [epp, currentpr], tn) ;
					genstand (pr6, curritsw + nbparm * 2, prinst [spri, currentpr], tn) ;
					freebloc (currentbloc) ;

					curritsw := curritsw + 2 ;

				        END (* Actual Is Conformant *) ELSE
				        BEGIN

					loadadr (gattr, pr3) ;
					genstand (pr6, curritsw, ispri3, tn) ;

					IF firstoflist THEN
					  BEGIN
                                                  (* Evaluation du nombre de dimensions *)
					    temppt := decltype ;
					    nbofdim := 0 ;
					    WHILE conformantdim (temppt) DO
					      BEGIN
					        nbofdim := nbofdim + 1 ;
					        temppt := temppt^.aeltype ;
					      END ;

(* Acquisition dope vector *)
					    dvdispw := oldnewstor (nbofdim * dopevectorsize + 8) DIV bytesinword ;
					    wlength := nbofdim * dopevectorsize DIV bytesinword ;
					    IF all_descriptors THEN
					      BEGIN
					        getpr ;
					        genstand (pr6, curritsw + (nbparm * 2), prinst [epp, currentpr], tny) ;
					        mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
					        geneism (imlr, 0, p0t0r0) ;
					        gendesca (currentpr, 0, 0, l9, (wlength + 2) * bytesinword, tn) ;
					        gendesca (pr6, dvdispw, 0, l9, (wlength + 2) * bytesinword, tn) ;
					        freebloc (currentbloc) ;
					      END ;

(* Incrementation et passage des bornes *)
					    temppt := decltype ; tempact := foundtype ; locdisp := 3 * nbofdim - 2 ;
					    WHILE conformantdim (temppt) DO
					      BEGIN
                                                  (* PAsse Low Bound et remplit premoer mot du dope vector *)
					        lowbound := tempact^.lo ;
					        sauvereg (ra, false) ; gencstecode (lowbound, ilda) ;
					        genstand (pr6, dvdispw + locdisp, ista, tn) ;

(* Passe high bound et remplit deuxieme mot du dope vector *)
					        highbound := tempact^.hi ;
					        gencstecode (highbound, ilda) ;
					        genstand (pr6, dvdispw + locdisp + 1, ista, tn) ;

(* Passe MULTIPLIER *)
					        IF tempact^.pack THEN
						multiplier := packedsize (tempact^.aeltype) * bitsinbyte ELSE
						multiplier := sup (tempact^.aeltype^.size, bytesinword) DIV bytesinword ;
					        gencstecode (multiplier, ilda) ;
					        genstand (pr6, dvdispw + locdisp + 2, ista, tn) ;

(* Prepare dimension suivante *)
					        locdisp := locdisp - 3 ;
					        temppt := temppt^.aeltype ; tempact := tempact^.aeltype ;
					        parmctp := parmctp^.nxtel^.nxtel ;

					      END ;
					  END (* FIRSTOFLIST *) ;

					genstand (pr6, dvdispw, iepp3, tn) ; (* Dope vector address *)
					genstand (pr6, curritsw + nbparm * 2, ispri3, tn) ;
					curritsw := curritsw + 2 ;
				        END (* Actual not conformant *) ;
				    END (* not LERR *) ;
				END (* Legal Substitution *) ;
			      END (* Not Same Type *) ;
		      END (* TYPTR#NIL,WITH GATTR *) ;
	        END (* PARAMISVAR *) ELSE
	        BEGIN                           (* VALUE PARAMETER *)
		expression ;
		WITH gattr DO
		  IF typtr <> NIL THEN
		    BEGIN
		      compatbin (parmctp^.vtype, typtr, generic) ;
		      IF generic = NIL THEN
		        BEGIN
			IF parmctp^.vtype = NIL THEN (* nothing *)
			ELSE IF parmctp^.vtype^.father_schema = string_ptr
			  THEN IF (parmctp^.vtype^.actual_parameter_list = NIL) THEN (* nothing *)
			    ELSE
			      BEGIN
			        IF parmctp^.vtype^.actual_parameter_list^.klass = konst THEN
				formal_length := parmctp^.vtype^.actual_parameter_list^.values
			        ELSE formal_length := 0 ; (* ERROR SOMEWHERE BEFORE *)
			        currparmw := oldnewstor (formal_length + 4) DIV bytesinword ;
			        IF typtr^.father_schema = string_ptr THEN
				BEGIN
				  loadadr (gattr, pr3) ;
				  genstand (pr3, 0, ildq, tn) ;
				  genstand (nreg, 4, iadq, tdl) ;
				  gencstecode (formal_length + 4, ilda) ;
				  mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
				  geneism (imlr, 0, p0t0r0) ;
				  gendesca (pr3, 0, 0, l9, 0, tql) ;
				  gendesca (pr6, currparmw, 0, l9, 0, tal) ;
				  IF procnameaddr^.pwantspl1descriptors THEN
				    genstand (pr6, currparmw + 1, iepp3, tn)
				  ELSE
				    genstand (pr6, currparmw, iepp3, tn) ;
				  genstand (pr6, curritsw, ispri3, tn) ;
				END ELSE
				IF typtr = charptr THEN
				  BEGIN
				    IF kind = lval THEN loaded_reg := ldreg
				    ELSE
				      IF raisused THEN
				        BEGIN
					loaded_reg := rq ; sauvereg (rq, false) ;
					transfer (gattr, inq) ;
				        END ELSE
				        BEGIN
					loaded_reg := ra ;
					transfer (gattr, inacc) ;
				        END ;
				    freeattr (gattr) ;
				    genstand (nreg, 27, opaq [shiftl, loaded_reg], tn) ;
				    genstand (pr6, currparmw + 1, opaq [stor, loaded_reg], tn) ;
				    genstand (nreg, 1, opaq [load, loaded_reg], tdl) ;
				    genstand (pr6, currparmw, opaq [stor, loaded_reg], tn) ;
				    IF procnameaddr^.pwantspl1descriptors THEN
				      genstand (pr6, currparmw + 1, iepp3, tn)
				    ELSE
				      genstand (pr6, currparmw, iepp3, tn) ;
				    genstand (pr6, curritsw, ispri3, tn) ;
				  END
				ELSE IF isstring (gattr) THEN
				    BEGIN
				      IF NOT conformantdim (typtr) THEN
				        BEGIN
					loadadr (gattr, pr3) ;
					sauvereg (ra, false) ;
					IF kind = chain THEN
					  BEGIN
					    IF alfactp^.alfalong > formal_length THEN error (127) ;
					    gencstecode (alfactp^.alfalong, ilda) ;
					  END
					ELSE BEGIN
					    IF typtr^.size > formal_length THEN error (127) ;
					    gencstecode (typtr^.size, ilda) ;
					  END ;
					mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
					genstand (pr6, currparmw, ista, tn) ;
					geneism (imlr, 0, p0t0r0) ;
					gendesca (pr3, 0, 0, l9, 0, tal) ;
					gendesca (pr6, currparmw + 1, 0, l9, 0, tal) ;
					IF procnameaddr^.pwantspl1descriptors THEN
					  genstand (pr6, currparmw + 1, iepp3, tn)
					ELSE
					  genstand (pr6, currparmw, iepp3, tn) ;
					genstand (pr6, curritsw, ispri3, tn) ;
				        END
				      ELSE (* conformant string *)
				        BEGIN
					init_desc_address (gattr.nameaddr, gattr) ;
					sauvereg (rq, false) ;
					genstand (gattr.descreg, 1, ildq, tn) ;
					genstand (gattr.descreg, 0, isbq, tn) ;
					genstand (nreg, 1, iadq, tdl) ; (* rq contains actual length *)
					freebloc (gattr.descbloc) ;
					genstand (pr6, currparmw, istq, tn) ;
					mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
					gencstecode (formal_length, ilda) ;
					genstand (pr6, currparmw, iepp3, tn) ;
					IF gattr.basebloc <> NIL THEN regenere (gattr.basebloc) ;
					geneism (imlr, 0, p0t0r0) ;
					gendesca (gattr.basereg, gattr.dplmt DIV bytesinword, gattr.dplmt MOD bytesinword, l9, 0, tql) ;
					gendesca (pr3, 1, 0, l9, 0, tal) ;
					freebloc (gattr.basebloc) ;
					IF procnameaddr^.pwantspl1descriptors THEN
					  genstand (pr3, 1, iepp3, tn) ;
					genstand (pr6, curritsw, ispri3, tn) ;
				        END
				    END ELSE error (127) ;
			        curritsw := curritsw + 2 ;
			      END ELSE
			    BEGIN
			      IF conformantdim (parmctp^.vtype) THEN
			        BEGIN       (* CONFORMARRAY VALUE SUBSTITUTION *)
				decltype := parmctp^.vtype ; foundtype := gattr.typtr ;
				lerr := false ;
				IF NOT legalconfarrsubstitution (foundtype, decltype) THEN
				  BEGIN
				    error (127) ;
				  END (* not LEGAL SUBSTITUTION *) ELSE
				  BEGIN
				    IF prevdecltype = decltype THEN
				      BEGIN (* LISTE *)
				        firstoflist := false ;
				        IF prevfoundtype <> foundtype THEN
					BEGIN
					  lerr := true ; error (127) ;
					END ;
				      END ELSE
				      BEGIN
				        firstoflist := true ;
				        prevfoundtype := foundtype ;
				        prevdecltype := decltype ;
				      END ;
				    IF NOT lerr THEN
				      BEGIN
                                                  (* RECOPIE TABLEAU ACTUEL  *)
				        IF gattr.typtr = alfaptr THEN
					BEGIN
					  foundsize := alfactp^.alfalong ;
					  alfalow := 1 ; alfahigh := foundsize ;
					END ELSE
					BEGIN
					  foundsize := gattr.typtr^.size ;
					  alfalow := 0 ; alfahigh := 0 ;
					END ;

				        currparb := oldnewstor (recadre (foundsize, bytesinword)) ;
				        currparmw := currparb DIV bytesinword ;
				        WITH lattr DO
					BEGIN
					  typtr := parmctp^.vtype ;
					  initattrvarbl (lattr) ;
					  dplmt := currparb ; pckd := parmctp^.vtype^.pack ;
					END ;
				        lbase := nreg ;
				        lpad := ord (' ') ;
				        IF gattr.kind = varbl THEN
					IF varissimple (gattr) THEN
					  BEGIN
					    lbase := basereg ; ldisp := dplmt DIV bytesinword ;
					    lmod := dplmt MOD bytesinword ;
					  END ;
				        IF lbase = nreg THEN
					BEGIN
					  loadadr (gattr, pr3) ;
					  lbase := pr3 ; ldisp := 0 ; lmod := 0 ;
					END ;

				        IF foundsize < twoto12 THEN
					BEGIN
					  mfari1 := a1r0i0 ; mfari2 := a1r0i0 ; lftag := tn ; rgtag := tn ;
					END ELSE
					BEGIN
					  mfari1 := a1r1i0 ; mfari2 := a1r1i0 ; lftag := tx6 ;
					  rgtag := tx7 ;
					  IF foundsize > twoto17m1 THEN
					    error (307) ELSE
					    BEGIN
					      genstand (nreg, foundsize, ieax6, tn) ;
					      genstand (nreg, foundsize, ieax7, tn) ;
					    END ;
					  foundsize := 0 ;
					END ;
				        geneism (imlr, lpad, p0t0r0) ;
				        IF kind = varbl THEN usednameaddr := nameaddr ;
				        gendesca (lbase, ldisp, lmod, l9, foundsize, rgtag) ;
				        gendesca (pr6, currparmw, 0, l9, foundsize, lftag) ;
				        genstand (pr6, currparmw, iepp3, tn) ;
				        genstand (pr6, curritsw, ispri3, tn) ;

				        IF firstoflist THEN
					BEGIN
                                                  (* EVALUATION DU NOMBRE DE DIMENSIONS *)
					  temppt := decltype ;
					  nbofdim := 0 ;
					  WHILE conformantdim (temppt) DO
					    BEGIN
					      nbofdim := nbofdim + 1 ;
					      temppt := temppt^.aeltype ;
					    END ;
                                                  (* ACQUISITION DOPE VECTOR *)
					  dvdispw := oldnewstor (nbofdim * dopevectorsize + 8) DIV
					  bytesinword ;
					  wlength := nbofdim * dopevectorsize DIV bytesinword ;
					  IF all_descriptors THEN
					    BEGIN
					      getpr ;
					      genstand (pr6, curritsw + (nbparm * 2), prinst [epp, currentpr], tny) ;
					      mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
					      geneism (imlr, 0, p0t0r0) ;
					      gendesca (currentpr, 0, 0, l9, (wlength + 2) * bytesinword, tn) ;
					      gendesca (pr6, dvdispw, 0, l9, (wlength + 2) * bytesinword, tn) ;
					      freebloc (currentbloc) ;
					    END ;

(* INCREMENTATION ET PASSAGE DES BORNES *)
					  temppt := decltype ; tempact := foundtype ; locdisp := 3 * nbofdim - 2 ;
					  WHILE conformantdim (temppt) DO
					    BEGIN
                                                  (* PASSE LOW BOUND ET REMPLIT PREMOER MOT DU DOPE VECTOR *)
					      IF alfalow <> 0 THEN
					        lowbound := alfalow ELSE
					        lowbound := tempact^.lo ;
					      sauvereg (ra, false) ; gencstecode (lowbound, ilda) ;
					      genstand (pr6, dvdispw + locdisp, ista, tn) ;

(* PASSE HIGH BOUND ET REMPLIT DEUXIEME MOT DU DOPE VECTOR *)
					      IF alfahigh <> 0 THEN
					        highbound := alfahigh ELSE
					        highbound := tempact^.hi ;
					      gencstecode (highbound, ilda) ;
					      genstand (pr6, dvdispw + locdisp + 1, ista, tn) ;

(* PASSE MULTIPLIER *)
					      IF tempact^.pack THEN
					        multiplier := packedsize (tempact^.aeltype) * bitsinbyte ELSE
					        multiplier := sup (tempact^.aeltype^.size, bytesinword) DIV bytesinword ;
					      gencstecode (multiplier, ilda) ;
					      genstand (pr6, dvdispw + locdisp + 2, ista, tn) ;

(* PREPARE DIMENSION SUIVANTE *)
					      locdisp := locdisp - 3 ;
					      temppt := temppt^.aeltype ; tempact := tempact^.aeltype ;
					      parmctp := parmctp^.nxtel^.nxtel ;

					    END ;
					END (* FIRSTOFLIST *) ;

				        genstand (pr6, dvdispw, iepp3, tn) ;
				        genstand (pr6, curritsw + nbparm * 2, ispri3, tn) ;
				        curritsw := curritsw + 2 ;
				      END (* not LERR *) ;
				  END (* LEGAL SUBSTITUTION *) ;
			        END (* CONFORMANT ARRAY VALUE SUBSTITUTION *) ELSE
			        error (127) ;
			    END ;
		        END (* GENERIC WAS nil *) ELSE

		        BEGIN declsize := parmctp@.vtype@.size ;
			CASE parmctp@.vtype@.form OF
			  reel : IF typtr # realptr THEN convreal (gattr) ;
			  numeric, scalar :
			    IF typtr = realptr THEN error (127) ELSE
			      IF kind = sval THEN
			        checkminmax (val, parmctp@.vtype, 303) ELSE
			        IF asscheck THEN
				BEGIN
				  IF kind # lval THEN transfer (gattr, inacc) ;
				  checkbnds (parerrcode, ldreg, parmctp@.vtype) ;
				END ;
			  pointer, records, power : foundsize := typtr@.size ;
			  arrays : BEGIN
			      lerr := false ;
			      IF typtr = alfaptr THEN
			        BEGIN foundsize := alfactp@.alfalong ;
				IF envstandard <> stdextend THEN
				  BEGIN
				    IF foundsize # declsize THEN lerr := true ;
				  END ELSE
				  IF foundsize > declsize THEN lerr := true ;
			        END (* ALFAPTR *) ELSE
			        BEGIN
				foundsize := typtr@.size ;
				IF foundsize # declsize THEN lerr := true ;
			        END ;
			      IF lerr THEN error (127) ;
			    END (* ARRAYS *) ;
			END (* CASE *) ;
			currparb := oldnewstor (recadre (declsize, bytesinword)) ;
			currparmw := currparb DIV bytesinword ;
			WITH lattr DO
			  BEGIN
			    typtr := parmctp@.vtype ;
			    initattrvarbl (lattr) ;
			    dplmt := currparb ;
			    pckd := parmctp@.vtype@.pack ;
			  END ;
			IF typtr@.form < power THEN
			  BEGIN
			    choicerarq ;
			    transfer (lattr, out) ;
			  END (* < POWER *) ELSE
			  BEGIN
			    IF kind = lval THEN (* ONLY POWER *)
			      transfer (lattr, out) ELSE
			      BEGIN
			        IF typtr@.form = power THEN
				BEGIN lpad := 0 ;
				  IF kind = sval THEN
				    BEGIN
				      IF longv = bytesindword THEN
				        BEGIN enterlcst (valpw, lretpt) ;
					enterundlab (lretpt@.lplace) ;
					foundsize := bytesindword ;
				        END ELSE
				        BEGIN enterllcst (valpw, llretpt) ;
					enterundlab (llretpt@.llplace) ;
				        END ;
				      genstand (nreg, 0, iepp3, tic) ;
				      lbase := pr3 ;
				      ldisp := 0 ;
				      lmod := 0 ;
				    END (* SVAL *) ELSE
				    lbase := nreg ;
				END (* POWER *) ELSE
				BEGIN lpad := ord (' ') ; lbase := nreg ;
				END ;
			        IF kind = varbl THEN
				IF varissimple (gattr) THEN
				  BEGIN
				    lbase := basereg ;
				    ldisp := dplmt DIV bytesinword ;
				    lmod := dplmt MOD bytesinword ;
				  END ;
			        IF lbase = nreg THEN
				BEGIN
				  loadadr (gattr, pr3) ;
				  lbase := pr3 ;
				  ldisp := 0 ;
				  lmod := 0 ;
				END ;
			        suplr := sup (foundsize, declsize) ;
			        IF suplr < twoto12 THEN
				BEGIN
				  mfari1 := a1r0i0 ; mfari2 := a1r0i0 ; lftag := tn ; rgtag := tn ;
				END ELSE
				BEGIN
				  mfari1 := a1r1i0 ; mfari2 := a1r1i0 ; lftag := tx6 ; rgtag := tx7 ;
				  IF suplr > twoto17m1 THEN
				    error (307) ELSE
				    BEGIN
				      genstand (nreg, declsize, ieax6, tn) ;
				      genstand (nreg, foundsize, ieax7, tn) ;
				    END ;
				  declsize := 0 ; foundsize := 0 ;
				END ;
			        geneism (imlr, lpad, p0t0r0) ;
			        IF kind = varbl THEN usednameaddr := nameaddr ;
			        gendesca (lbase, ldisp, lmod, l9, foundsize, rgtag) ;
			        gendesca (pr6, currparmw, 0, l9, declsize, lftag) ;
			        BEGIN
			        END ;
			      END (* >=POWER *) ;
			  END ;
			IF procnameaddr^.pwantspl1descriptors AND
			  is_pl1_varying_char (parmctp^.vtype) THEN
			  genstand (pr6, currparmw + 1, iepp3, tn)
			ELSE
			  genstand (pr6, currparmw, iepp3, tn) ;
			genstand (pr6, curritsw, ispri3, tn) ;
			curritsw := curritsw + 2 ;
		        END (* GENERIC NOT NIL *) ;
		    END (* TYPTR # NIL,WITH GATTR *) ;
	        END (* VALUE PARAMETER *) ;
	    parmctp := parmctp@.nxtel ;
2 :	    IF parmctp <> NIL THEN              (* FOR SECURITY, IN CASE OF ERROR, SKIP CONF. ARRAY DIMS *)
	      IF parmctp^.vkind = arraybound THEN
	        BEGIN
		parmctp := parmctp^.nxtel ;
		GOTO 2
	        END ;
	    IF no <> 15 THEN                    (* NOT , *)
	      BEGIN
	        IF no <> 10 (*    )    *) THEN
		IF parmctp <> NIL THEN
		  BEGIN error (15) ; skip (15) ;
		  END ;
	      END ;
	  UNTIL no # 15 ;                       (* , *)
	  IF no = 10 THEN
	    insymbol ELSE
	    BEGIN
	      error (4) ; skip (46) ;
	    END ;
	END (* NO=9 *) ;
        IF parmctp # NIL THEN
	error (126) ;
        IF itisafunct THEN
	BEGIN
	  genstand (pr6, fctplace DIV bytesinword, iepp3, tn) ;
	  genstand (pr6, curritsw, ispri3, tn) ;
	END ;
        IF pisformal THEN
	BEGIN
	  ltag := tx7 ; ldisp := 0 ;
	  IF plevel = level THEN
	    lbase := pr6 ELSE
	    BEGIN
	      loadbase (plevel) ; lbase := currentpr ; freebloc (currentbloc) ;
	    END ;
	  usednameaddr := procnameaddr ;
	  genstand (lbase, procplacew, iepp5, tny) ; (* ITS ON PROC INFO *)
	  genstand (pr5, 2, iepp1, tny) ;       (* PR1 = D-LINK *)
                                                  (* NOW  LOAD X7  WITH *)
                                                  (* CODE  INTERNAL-EXTERNAL  CALL *)
	  genstand (pr5, 4, ilxl7, tn) ;        (* OPERATOR NUMBER *)
	  genstand (pr5, 0, iepp5, tny) ;       (*  PROCEDURE ENTRY POINT *)
	END (* FORMAL *) ELSE
	BEGIN
	  ltag := tn ;
	  IF pisext THEN
	    ldisp := extcallplace ELSE
	    ldisp := intcallplace ;
	  loadlink (pr1, plevel) ;
	  IF all_descriptors AND (nbparm <> 0) THEN
	    BEGIN
	      regenere (pr5bloc) ;
	      freebloc (pr5bloc) END ELSE
	    BEGIN
	      usednameaddr := procnameaddr ;
	      genstand (prlink, procplacew, iepp5, tny) ;
	    END ;
	END ;
                                                  (* LOAD  X1  WITH *)
                                                  (* PARAMETER LIST DISPLACEMENT *)
        genstand (pr6, deplist DIV bytesinword, ieax1, tn) ;
                                                  (* 2* NBPARPROC IN A 0..17 *)
        genstand (nreg, 2048 * nbparm, ifld, tdl) ;
        IF all_descriptors THEN
	genstand (nreg, nbparm * 2, ildq, tdu) ;
        genstand (pr0, ldisp, itsp3, ltag) ;
        IF pisext OR ((envstandard <> stdpure) AND pisformal) THEN
	genstand (pr6, pr4depw, iepp4, tny) ;
1 :
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN PASSPARAMS @@@  WITH NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* PASSPARAMS *) ;

    BEGIN
    END.
