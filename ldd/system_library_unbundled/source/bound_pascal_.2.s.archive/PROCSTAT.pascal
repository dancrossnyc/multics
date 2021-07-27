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
  PROGRAM procstat ;
    $IMPORT
                                                  (* IMPORTED PROCEDURES  *)
      'RACINE (pascal)' :
        error,
        insymbol,
        nameisref,
        nextline,
        recadre,
        skip ;
      'GENOPER (pascal)' :
        check_dynamic_string_length,
        gen_insert,
        gen_delete ;
      'GENERE (pascal)' :
        gendesca,
        geneism,
        genstand,
        inser ;
      'EXPR (pascal)' :
        expression ;
      'STATE (pascal)' :
        addressvar,
        calcvarient,
        checkbnds,
        choicerarq,
        freeallregisters,
        freebloc,
        gencstecode,
        loadadr,
        loadbase,
        genexceptcode,
        newbloc,
        oldnewstor,
        regenere,
        sauvereg,
        transfer,
        variab ;
      'MODVARIABLE (pascal) ' :
        init_desc_address,
        variable ;
      'CONTEXTTABLE (pascal) ' :
        checkminmax,
        compatbin,
        conformantdim ;
      'MODATTR (pascal)' :
        freeattr,
        initattrvarbl,
        isstring,
        is_possible_string,
        varissimple ;
      'optimized_procedures (alm)' : search ;

(* IMPORTED VARIABLES *)
      'RACINE (pascal)' :
        alfaptr,
        boolptr,
        charptr,
        cl,
        ctptr,
        envstandard,
        errcl,
        exportablecode,
        ival,
        level,
        intptr,
        mpcogout,
        no,
        pascalfrench,
        realptr,
        symbolfile,
        symbolline,
        string_ptr,
        symbolmap,
        textfilectp ;
      'DECLARE (pascal)' :
        getpr4afterstop,
        lkc ;
      'GENERE (pascal)' :
        cb,
        indfich,
        mfari1,
        mfari2,
        usednameaddr ;
      'STATE (pascal)' :
        resetused,
        disposeused,
        arrayboundsctp,
        asscheck,
        currentbloc,
        currentpr,
        gattr,
        inputctp,
        inxcheck,
        linktoend,
        linktoendplace,
        linktomain,
        linktomainplace,
        maxprused,
        modif,
        opaq,
        outputctp,
        prinst,
        stattrace$

    $EXPORT
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
      writeir $



$OPTIONS page $


$INCLUDE 'CONSTTYPE' $

$OPTIONS page $

    VAR

(* REDEFINE IMPORTED VARIABLES     *)
(* FROM RACINE  *)
      mpcogout : text ;
      cl : integer ;
      no : integer ;
      pascalfrench : boolean ;
      realptr : ctp ;
      string_ptr : ctp ;
      ctptr : ctp ;
      envstandard : stdkind ;
      errcl : ARRAY [norange] OF typofsymb ;
      textfilectp : ctp ;
      intptr : ctp ;
      ival : integer ;
      alfaptr : ctp ;
      boolptr : ctp ;
      charptr : ctp ;
      exportablecode : boolean ;
      level : levrange ;
      symbolmap : boolean ;
      symbolfile, symbolline : integer ;

(* FROM DECLARE *)
      getpr4afterstop : boolean ;
      lkc : integer ;

(* FROM GENERE  *)
      cb : integer ;
      indfich : integer ;
      mfari1 : zari ;
      mfari2 : zari ;
      usednameaddr : ctp ;


(* FROM STATE   *)
      arrayboundsctp : ctp ;
      resetused : boolean ;
      disposeused : boolean ;
      inxcheck : boolean ;
      asscheck : boolean ;
      gattr : attr ;
      currentbloc : regpt ;
      outputctp : ctp ;
      inputctp : ctp ;
      maxprused : preg ;
      prinst : ARRAY [typepr, pr1..pr6] OF istand ; (* GIVES A PR INSTRUCTION *)
      stattrace : levtrace ;
      opaq : ARRAY [typeofop, ra..reaq] OF istand ; (* GIVES INST. WITH A,Q,AQ,EAQ *)
      currentpr : preg ;
      modif : ARRAY [nxreg..rq] OF tag ;
      linktomain : boolean ;
      linktomainplace : integer ;
      linktoendplace : integer ;
      linktoend : boolean ;




$OPTIONS page $
                                                  (* FROM GENOPER *)
    PROCEDURE check_dynamic_string_length (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE gen_insert (VAR inserted_attr, target_attr, disp_attr : attr) ; EXTERNAL ;
    PROCEDURE gen_delete (VAR string_attr, disp_attr, len_attr : attr) ; EXTERNAL ;
                                                  (* FROM GENERE  *)
                                                  (* REDEFINE IMPORTED PROCEDURES    *)
                                                  (* FROM GENERE  *)
    PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ; EXTERNAL ;
    PROCEDURE geneism (fcode : ieism ; ffield : integer ; fbits : zptr) ; EXTERNAL ;
    PROCEDURE gendesca (fareg : preg ; fadr, fcn : integer ; fta : lgcar ;
      fn : integer ; frlgth : mreg) ; EXTERNAL ;
    PROCEDURE inser (fcb : integer ; fplace : integer) ; EXTERNAL ;


(* FROM RACINE  *)
    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE insymbol ; EXTERNAL ;
    PROCEDURE skip (nosym : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    PROCEDURE search ; EXTERNAL ;
    PROCEDURE nameisref (box : ctp ; fil, lin : integer) ; EXTERNAL ;
    FUNCTION recadre (fnum, fmod : integer) : integer ; EXTERNAL ;


(* FROM EXPR    *)
    PROCEDURE expression ; EXTERNAL ;


(* FROM STATE   *)
    PROCEDURE choicerarq ; EXTERNAL ;
    PROCEDURE transfer (VAR fattr : attr ; inwhat : destination) ; EXTERNAL ;
    PROCEDURE newbloc (freg : register) ; EXTERNAL ;
    PROCEDURE variab (fvarset : boolean) ; EXTERNAL ;
    PROCEDURE loadbase (lev : integer) ; EXTERNAL ;
    FUNCTION oldnewstor (incrinbytes : integer) : integer ; EXTERNAL ;
    PROCEDURE freebloc (VAR fbtofree : regpt) ; EXTERNAL ;
    PROCEDURE genexceptcode (ferrcode : integer ; freg : register) ; EXTERNAL ;
    PROCEDURE loadadr (VAR fattr : attr ; wantedpr : preg) ; EXTERNAL ;
    PROCEDURE regenere (oldbloc : regpt) ; EXTERNAL ;
    PROCEDURE addressvar (fctp : ctp ; VAR fattr : attr ; modif : boolean) ; EXTERNAL ;
    PROCEDURE calcvarient (VAR fattr : attr ; VAR fbase : preg ; VAR fdisp : integer ;
      VAR ftag : tag) ; EXTERNAL ;
    PROCEDURE sauvereg (freg : register ; fload : boolean) ; EXTERNAL ;
    PROCEDURE gencstecode (farg : integer ; finst : istand) ; EXTERNAL ;
    PROCEDURE checkbnds (errcode : integer ; freg : register ; fctp : ctp) ; EXTERNAL ;
    PROCEDURE freeallregisters ; EXTERNAL ;
                                                  (* FROM MODVARIABLE *)
    PROCEDURE init_desc_address (fctp : ctp ; VAR fattr : attr) ; EXTERNAL ;

    PROCEDURE variable (fvarset : boolean) ; EXTERNAL ;

(* FROM CONTEXTTABLE *)

    PROCEDURE compatbin (typleft, typright : ctp ; VAR fgeneric : ctp) ; EXTERNAL ;
    PROCEDURE checkminmax (fvalu : integer ; fctp : ctp ; ferrnum : integer) ; EXTERNAL ;
    FUNCTION conformantdim (ff : ctp) : boolean ; EXTERNAL ;

(* FROM MODATTR *)

    FUNCTION is_possible_string (VAR fattr : attr) : boolean ; EXTERNAL ;
    FUNCTION isstring (VAR fattr : attr) : boolean ; EXTERNAL ;
    FUNCTION varissimple (VAR fattr : attr) : boolean ; EXTERNAL ;
    PROCEDURE freeattr (VAR fattr : attr) ; EXTERNAL ;
    PROCEDURE initattrvarbl (VAR fattr : attr) ; EXTERNAL ;



$OPTIONS page $

(* ************************************ WRITEIR ******************************* *)

    PROCEDURE writeir (typewrite : integer) ;

(* C   COMPILES  THE CALL   OF   WRITE     TYPEWRITE = ( 0 )  Standard
   WRITELN   TYPEWRITE = ( 1 )  Standard
   PAGE      TYPEWRITE = ( 2 )  Standard
   FLUSH     TYPEWRITE = ( 3 )  SOL extension
   if FILE IS OMITTED, then OUTPUT ASSUMED
   C *)
(* E ERRORS DETECTED
   4: ")" EXPECTED
   9: "(" EXPECTED
   15: INTEGER EXPECTED
   20: "," EXPECTED
   144: ILLEGAL TYPE OF EXPRESSION
   176: OUTPUT USED ,NOT  DECLARED
   191: SCALING FACTOR ONLY FOR REAL
   198: OPERATION ALLOWED ONLY FOR TEXT FILE
   E *)
      LABEL
        1 (* EXIT PROC *) ;
      VAR
        pr3bloc : regpt ;
        loc1, loc2 : integer ;
        lattr : attr ;
        defaultfile : boolean ;
        deflength : integer ;
        errintype : boolean ;
        exprismade : boolean ;
        fileonly : boolean ;
        finloop : boolean ;
        itisput : boolean ;
        hardlength : boolean ;
        locreg : preg ;
        locbox : regpt ;
        lengthst : integer ;
        linst : istand ;
        locctptr : ctp ;
        notwrite : boolean ;
        typecode : integer ;
        aisknown : boolean ;
        acont : integer ;

      BEGIN                                       (* WRITEIR *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT WRITEIR ^^^ WITH TYPEWRITE:', typewrite : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        fileonly := false ; exprismade := false ;
        locbox := NIL ;
        notwrite := typewrite <> 0 ;
        locctptr := NIL ;
        IF no <> 9 (* ( *) THEN
	BEGIN
	  IF notwrite THEN
	    BEGIN
	      IF outputctp <> NIL THEN
	        BEGIN
		usednameaddr := outputctp ;
		IF symbolmap THEN
		  nameisref (outputctp, symbolfile, symbolline) ;
		genstand (prlink, outputctp^.vaddr DIV bytesinword, iepp3, tny) ;
		genstand (pr6, fsbadrw, ispri3, tn) ;
	        END (*  <>  nil *) ELSE
	        BEGIN
		IF errcl [no] = endsy THEN error (176) ELSE error (9) ;
		skip (46) ;
		GOTO 1 ;
	        END ;
	    END (*  NOTWRITE *) ELSE
	    BEGIN                               (* WRITE *)
	      error (9) ; skip (46) ; GOTO 1 ;
	    END ;
	END (* NO <> 9 *) ELSE
	BEGIN                                   (* NO=9 *)
	  insymbol ;
	  defaultfile := true ;
	  IF no = 1 (* ID *) THEN
	    BEGIN
	      search ;
	      IF ctptr <> NIL THEN
	        IF ctptr^.klass = vars THEN
		IF ctptr^.vtype <> NIL THEN
		  IF ctptr^.vtype^.form = files THEN
		    BEGIN
		      locctptr := ctptr ;
		      expression ;

		      IF gattr.typtr <> NIL THEN
		        BEGIN
			IF gattr.typtr^.form = files THEN
			  BEGIN
			    usednameaddr := gattr.nameaddr ;
			    loadadr (gattr, pr3) ;
			    genstand (pr6, fsbadrw, ispri3, tn) ;
			    defaultfile := false ;
			    IF no = 10 (* ) *) THEN
			      BEGIN
			        IF NOT notwrite THEN error (20) ;
			        fileonly := true ;
			      END (* ID *) ELSE
			      IF no = 15 (* , *) THEN
			        BEGIN
				IF typewrite >= 2 (* PAGE FLUSH *) THEN error (4) ;
				insymbol ;
			        END (* 15 *) ELSE
			        error (20) ;
			  END (* FILES *) ELSE
			  BEGIN exprismade := true ; locctptr := NIL ;
			  END
		        END (* TYPTR not nil *) ELSE
		        BEGIN exprismade := true ; locctptr := NIL ;
		        END ;
		    END (* FILE IDENT *) ;
	    END (* ID *) ;
	  IF defaultfile THEN
	    IF outputctp <> NIL THEN
	      BEGIN
	        usednameaddr := outputctp ;
	        IF symbolmap THEN
		nameisref (outputctp, symbolfile, symbolline) ;
	        genstand (prlink, outputctp^.vaddr DIV bytesinword, iepp3, tny) ;
	        genstand (pr6, fsbadrw, ispri3, tn) ;
	      END ELSE
	      error (176) ;
	  IF NOT fileonly THEN
	    BEGIN
	      REPEAT                            (* LOOP ON EXPRESSIONS TO BE WRITTEN *)
	        lengthst := -1 ;
	        deflength := -1 ;
	        acont := -1 ;
	        IF NOT exprismade THEN
		BEGIN
		  freeallregisters ; expression ;
		END ELSE
		exprismade := false ;
	        WITH gattr DO
		IF typtr <> NIL THEN
		  BEGIN
                                                  (* CHECK  FOR PUT *)
		    itisput := false ; linst := inop ;
		    IF NOT notwrite THEN
		      IF locctptr <> NIL THEN
		        IF locctptr^.vtype^.feltype = typtr THEN
			IF locctptr^.vtype <> textfilectp THEN
			  itisput := true ;
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
			    locreg := nreg ; locbox := NIL ;
			    init_desc_address (gattr.nameaddr, gattr) ;
			    locreg := gattr.descreg ; locbox := gattr.descbloc ;
			    linst := prinst [spri, gattr.basereg] ;
			    freebloc (gattr.basebloc) ;
			  END ;
		        END ;
		    errintype := false ;
		    hardlength := false ;
		    aisknown := false ;
                                                  (* SELECT TYPECODE, *)
                                                  (* LENGTH FOR EACH TYPE *)
		    IF typtr^.father_schema = string_ptr THEN
		      BEGIN
		        typecode := 32 ;
		        genstand (pr3, 0, ilda, tn) ;
		        aisknown := true ; acont := deflength ;
		        genstand (pr3, 1, iepp3, tn) ;
		        genstand (pr6, valplacew, ispri3, tn)
		      END ELSE
		      BEGIN
		        IF linst <> inop THEN
			BEGIN
			  genstand (pr6, valplacew, linst, tn) ;
			END ;
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
			        BEGIN
				IF itisput THEN
				  BEGIN typecode := 4 ; (* AS INTEGER *)
				  END ELSE
				  errintype := true ;
			        END ;
			  END (* SCALAR *) ;
			pointer, records, power :
			  IF itisput THEN
			    typecode := 64 ELSE
			    errintype := true ;
			files : errintype := true ;
			arrays :
			  IF itisput THEN
			    typecode := 64 ELSE
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
		      END ;
		    IF errintype THEN
		      BEGIN error (144) ; typecode := 4 ; deflength := deflnum ;
		      END ;
		  END (* TYPTR  <>  nil, WITH GATTR *) ;
	        IF itisput THEN
		BEGIN

(*  6|FSBADRW =  ITS ON FSB
   6|VALPLACW=  TWO-WORDS VALUE *)
		  genstand (nreg, typecode, ilda, tdl) ; (* CODE FOR VALUE TYPE *)
		  genstand (pr0, writeseqplace, itsp3, tn) ; (* OPERATOR CALL *)
		END (* PUT *) ELSE
		BEGIN                         (* WRITE ON A TEXT FILE *)
		  IF locctptr <> NIL THEN
		    IF locctptr^.vtype <> textfilectp THEN error (198) ;
		  IF no = 19 (* : *) THEN
		    BEGIN
		      insymbol ; expression ;
		      IF hardlength THEN
		        BEGIN
			hardlength := false ;
			aisknown := true ;
			acont := lengthst ;
		        END ;
		      freebloc (locbox) ;
		      IF gattr.typtr <> NIL THEN
		        IF gattr.typtr^.form <> numeric THEN error (15) ELSE
			BEGIN
			  transfer (gattr, inacc) ;
			  freebloc (gattr.ldregbloc) ;
			END ;
		    END ELSE
		    BEGIN
		      IF NOT hardlength THEN
		        BEGIN
			IF NOT aisknown THEN
			  BEGIN
			    aisknown := true ;
			    acont := deflength ;
			    gencstecode (deflength, ilda) ;
			    hardlength := false ; freebloc (locbox) ;
			  END ;
			IF (typecode = 2) AND (NOT pascalfrench) THEN
			  BEGIN
			    genstand (pr6, valplacew, iszn, tn) ;
			    genstand (nreg, 2, itnz, tic) ;
			    genstand (nreg, 1, iada, tdl) ; (* LENGTH + 1 if "FALSE" *)
			  END
		        END ELSE
		        BEGIN
			regenere (gattr.descbloc) ; ; locbox := NIL ;
                                                  (* COMPUTE SIZE NOW *)
			sauvereg (ra, false) ;

			genstand (locreg, 1, ilda, tn) ; (* MAX       *)
			genstand (locreg, 0, isba, tn) ; (*   - MIN   *)
			genstand (nreg, 1, iada, tdl) ; (*    +1     *)
			freeattr (gattr) ;
			aisknown := true ; acont := lengthst ;
		        END ;
		    END ;
                                                  (* STORE   LENGTH *)
		  genstand (pr6, longplacew, ista, tn) ;
		  IF no = 19 (* : *) THEN
		    BEGIN
		      IF typecode <> 8 (* REAL *) THEN error (191) ;
		      typecode := 16 ;
		      aisknown := false ;
		      freeallregisters ;
		      insymbol ; expression ;
		      IF gattr.typtr <> NIL THEN
		        IF gattr.typtr^.form <> numeric THEN error (15) ELSE
			BEGIN
			  transfer (gattr, inacc) ;
			  freebloc (gattr.ldregbloc) ;
			  genstand (pr6, scaleplacew, ista, tn) ;
			END ;
		    END ;
		  IF NOT hardlength THEN
		    BEGIN
		      IF typecode = 32 (* CHAINE *) THEN
		        BEGIN
			IF NOT (aisknown AND (acont = lengthst)) THEN
			  BEGIN
			    gencstecode (lengthst, ilda) ;
			    aisknown := true ; acont := lengthst ;
			  END ;
			genstand (pr6, longstplacew, ista, tn) ;
		        END ;
		      IF NOT (aisknown AND (acont = typecode)) THEN
		        genstand (nreg, typecode, ilda, tdl) ;
		    END ELSE
		    BEGIN
		      genstand (pr6, longstplacew, ista, tn) ;
		      genstand (nreg, typecode, ilda, tdl) ;
		    END ;

(*  PR6| FSBADRW      ITS   ON FSB
   PR6| VALPLACEW    VALUE  OR ITS ON VALUE
   PR6| LONGPLACEW   REQUESTED LENGTH
   PR6| SCALEPLACEW  DIGITS   FOR REAL
   OR
   LONGSTPLACEW REAL SIZE   FOR A STRING
   RA   CODE  FOR VALUE TYPE *)
		  genstand (pr0, writetextplace, itsp3, tn) ;
		END (* not A PUT *) ;
                                                  (* IS   LOOP   ENDED  OR NOT *)
	        finloop := true ;
	        IF no = 10 (*  ) *) THEN insymbol ELSE
		IF no = 15 (*  , *) THEN
		  BEGIN
		    insymbol ; finloop := false ;
		  END ELSE
		  BEGIN
		    error (20) ; skip (15) ;
		    IF no = 15 (* , *) THEN
		      BEGIN
		        insymbol ; finloop := false ;
		      END ;
		  END ;
	      UNTIL finloop ;
	    END (* not FILEONLY *) ELSE
	    IF no <> 10 (* ) *) THEN
	      BEGIN
	        error (4) ; skip (46) ;
	      END ELSE
	      insymbol ;
	END (* NO= 9 *) ;
        IF notwrite THEN
	BEGIN
	  IF locctptr <> NIL THEN
	    IF locctptr^.vtype <> textfilectp THEN
	      error (198) ;
	  IF typewrite = 1 (* WRITELN *) THEN
	    BEGIN
	      genstand (pr0, writelnplace, itsp3, tn) ;
	    END ELSE
	    IF typewrite = 2 (* PAGE *) THEN
	      BEGIN
	        genstand (pr0, pageplace, itsp3, tn) ;
	      END ELSE
	      BEGIN
	        genstand (pr0, flushplace, itsp3, tn) ;
	      END ;
	END ;
1 :                                               (* EXIT PROCEDURE *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '^^^ FIN WRITEIR ^^^ WITH NO :', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* WRITEIR *) ;

$OPTIONS page $

$OPTIONS page $

(* ************************************ READIR ******************************** *)

    PROCEDURE readir (typeread : integer) ;

(* C .CALLED BY STATEMENT FOR STANDARD  PROCEDURES  - READ     TYPEREAD IS 0
   - READLN   TYPEREAD IS 1
   .ON NOT TEXT FILES  , READ IS   ASSIGN  FOLLOWED BY  GET
   .THE FILE "INPUT"  CAN BE OMITTED.
   .READLN  CAN BE USED ONLY  ON TEXT FILES
   C *)
(* E ERRORS DETECTED
   4: ")"  EXPECTED
   9: "("  EXPECTED
   20: ","  EXPECTED
   153: TYPE ERROR IN READ
   175: INPUT USED AND NOT DECLARED
   198: OPERATION ALLOWED ONLY FOR TEXT FILE
   E *)
      LABEL
        1 ;                                       (* EXIT OF PROCEDURE *)
      VAR

        defaultfile : boolean ;
        variabismade : boolean ;
        fileonly : boolean ;
        finloop : boolean ;
        isreadln : boolean ;
        itisget : boolean ;
        lattr : attr ;
        lerr : boolean ;
        locctptr : ctp ;
        loctype : ctp ;
        typecode : integer ;


      BEGIN                                       (* READIR *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT READIR ^^^ WITH TYPEREAD :', typeread : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        isreadln := typeread = 1 ;
        locctptr := NIL ;
        variabismade := false ;
        fileonly := false ;
        IF no <> 9 (* ( *) THEN
	BEGIN
	  IF isreadln THEN
	    BEGIN
	      IF inputctp <> NIL THEN
	        BEGIN
		usednameaddr := inputctp ;
		IF symbolmap THEN
		  nameisref (inputctp, symbolfile, symbolline) ;
		genstand (prlink, inputctp^.vaddr DIV bytesinword, iepp3, tny) ;
		genstand (pr6, fsbadrw, ispri3, tn) ;
	        END (*  <> nil *) ELSE
	        BEGIN
		IF errcl [no] = endsy THEN error (175) ELSE error (9) ;
		skip (46) ; GOTO 1 ;
	        END (* =nil *) ;
	    END (* READLN *) ELSE
	    BEGIN                               (* READ *)
	      error (9) ; skip (46) ; GOTO 1 ;
	    END ;
	END (* NO <> 9 *) ELSE
	BEGIN                                   (* NO=9 *)
	  insymbol ;
	  defaultfile := true ;
	  IF no = 1 (* ID *) THEN
	    BEGIN
	      search ;
	      IF ctptr <> NIL THEN
	        IF ctptr^.klass = vars THEN
		IF ctptr^.vtype <> NIL THEN
		  IF ctptr^.vtype^.form = files THEN
		    BEGIN
		      locctptr := ctptr ;
		      freeallregisters ;
		      variable (false) ;
		      IF gattr.typtr <> NIL THEN
		        IF gattr.typtr^.form = files THEN
			BEGIN
			  loadadr (gattr, pr3) ;
			  genstand (pr6, fsbadrw, ispri3, tn) ;
			  defaultfile := false ;
			  IF no = 10 (* ) *) THEN
			    BEGIN
			      IF NOT isreadln THEN error (20) ;
			      fileonly := true ;
			    END (* NO=10 *) ELSE
			    IF no = 15 (* , *) THEN
			      insymbol ELSE
			      error (20) ;
			END (* FILE FOUND *) ELSE
			BEGIN
			  variabismade := true ; locctptr := NIL ;
			END ELSE
		        BEGIN
			variabismade := true ; locctptr := NIL ;
		        END
		    END (* FILE IDENTIFIER *) ;
	    END (* NO=1 *) ;
	  IF defaultfile THEN
	    IF inputctp <> NIL THEN
	      BEGIN
	        usednameaddr := inputctp ;
	        IF symbolmap THEN
		nameisref (inputctp, symbolfile, symbolline) ;
	        genstand (prlink, inputctp^.vaddr DIV bytesinword, iepp3, tny) ;
	        genstand (pr6, fsbadrw, ispri3, tn) ;
	      END ELSE
	      error (175) ;
	  IF NOT fileonly THEN
	    BEGIN
	      REPEAT                            (* LOOP ON READ ITEMS *)
	        IF NOT variabismade THEN
		BEGIN
		  freeallregisters ;
		  variab (true) ;             (* VARIABLE IS SET HERE *)
		END ELSE
		variabismade := false ;
	        WITH gattr DO
		IF typtr <> NIL THEN
		  BEGIN
		    itisget := false ;
		    IF NOT isreadln THEN
		      IF locctptr <> NIL THEN
		        IF locctptr^.vtype^.feltype = typtr THEN
			IF locctptr^.vtype <> textfilectp THEN
			  itisget := true ;
		    IF itisget THEN
		      BEGIN
		        loadadr (gattr, pr1) ;
		        genstand (pr0, readseqplace, itsp3, tn) ;
		      END (* GET *) ELSE
		      BEGIN                   (* READ ON TEXT FILE *)
		        IF locctptr <> NIL THEN
			IF locctptr^.vtype <> textfilectp THEN error (198) ;
		        lerr := false ;
		        IF typtr^.father_schema = string_ptr THEN
			BEGIN
			  loadadr (gattr, pr3) ;
			  genstand (pr6, valplacew, ispri3, tn) ;
			  freeattr (gattr) ;
			  IF typtr^.actual_parameter_list^.klass <> konst THEN
			    BEGIN
			      addressvar (typtr^.actual_parameter_list, lattr, false) ;
			      transfer (lattr, inacc) ;
			      freeattr (lattr) ;
			    END
			  ELSE gencstecode (typtr^.actual_parameter_list^.values, ilda) ;
			  genstand (pr6, longstplacew, ista, tn) ;
			  typecode := 16 ;
			END ELSE
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
			  genstand (nreg, typecode, ilda, tdl) ;
			  genstand (pr0, readtextplace, itsp3, tn) ;
                                                  (* NOW ACC IS LOADED *)
                                                  (* WITH GATTR *)
			  IF typecode <> 16 THEN
			    BEGIN
			      kind := lval ;
			      IF typtr = realptr THEN
			        ldreg := reaq ELSE
			        ldreg := ra ;
			      newbloc (ldreg) ; ldregbloc := currentbloc ;
			      IF asscheck THEN
			        IF typtr <> realptr THEN
				checkbnds (asserrcode, ra, typtr) ;
			      transfer (lattr, out) ; (* ASSIGNS *)
			    END ;
			END (* NOT LERR *) ;
		      END (* READ ON TEXT FILE *) ;
		  END (* TYPTR  <>  nil,WITH GATTR *) ;
                                                  (* IS LOOP ENDED OR NOT *)
	        finloop := true ;
	        IF no = 10 (* ) *) THEN
		insymbol ELSE
		IF no = 15 THEN
		  BEGIN
		    insymbol ; finloop := false ;
		  END ELSE
		  BEGIN
		    error (20) ; skip (15) ;
		    IF no = 15 (* , *) THEN
		      BEGIN
		        insymbol ; finloop := false ;
		      END ;
		  END ;
	      UNTIL finloop ;
	    END (* NOT FILEONLY *) ELSE
	    IF no <> 10 (* ) *) THEN
	      BEGIN
	        error (4) ; skip (46) ;
	      END ELSE
	      insymbol ;
	END (* NO=9 *) ;
        IF isreadln THEN
	BEGIN
	  IF locctptr <> NIL THEN
	    IF locctptr^.vtype <> textfilectp THEN
	      error (198) ;
	  genstand (pr0, readlnplace, itsp3, tn) ;
	END ;
1 :                                               (* EXIT PROCEDURE *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '^^^ FIN READIR ^^^ WITH NO:', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* READIR *) ;

$OPTIONS page $

(* ************************************* GETPUT  ****************************** *)

    PROCEDURE getput (typeio : integer) ;

(* C COMPILATION OF ALL INPUT/OUTPUT PREDECLARED PROCEDURES
   . CALLED IN STATEMENT WITH FOLLOWING CODES
   Codes 0..3 are for standard procedures
   4..10 are for SOL procedures

   0: GET       4: FCONNECT              8: FCLOSE
   1:PUT        5: FUPDATE               9: FAPPEND
   2:RESET      6: FGET                 10: FREOPEN
   3:REWRITE    7: FPUT

   . INCLUDE ALSO RESET FOR A POINTER ( Extended Pascal only )
   C *)
(* E ERRORS DETECTED
   4: ')' EXPECTED
   9: '(' EXPECTED
   15:  INTEGER EXPECTED
   19:  STRING EXPECTED
   20: ',' EXPECTED
   66: ILLEGAL OPERATION FOR THIS TYPE OF FILE
   68: RESET ON POINTER NOT ALLOWED IN STANDARD
   125: ERROR ON TYPE FOR STANDARD FUNCT/PROC
   256: FCONNECT autorise que sur fichier permanent
   E *)
      LABEL
        10 ;                                      (* EXIT PROCEDURE *)
      VAR

        istext : boolean ;
        loclong : integer ;
        operdepw : integer ;

      BEGIN                                       (* GETPUT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT GETPUT ^^^ WITH TYPEIO :', typeio) ; nextline ;
	END ;
$OPTIONS compile = true $

        IF no <> 9 (* ( *) THEN
	BEGIN error (9) ; skip (46) ; GOTO 10 ;
	END ;
        insymbol ; freeallregisters ;
        variab (true) ;
        IF gattr.typtr <> NIL THEN
	IF gattr.typtr^.form = files THEN
	  BEGIN
	    usednameaddr := gattr.nameaddr ;
	    loadadr (gattr, pr3) ;
	    genstand (pr6, fsbadrw, ispri3, tn) ;
                                                  (* FIND NOW SUITABLE OPERATOR *)
	    istext := gattr.typtr = textfilectp ;
	    CASE typeio OF
	      0 :                               (* GET *)
	        IF istext THEN operdepw := gettextplace ELSE operdepw := getseqplace ;
	      1 :                               (* PUT *)
	        IF istext THEN operdepw := puttextplace ELSE operdepw := putseqplace ;
	      2 : (* RESET *) operdepw := resetplace ;
	      3 : (* REWRITE *) operdepw := rewriteplace ;
	      4 :                               (* FCONNECT *)
	        operdepw := connectplace ;
	      5 : (* FUPDATE *) IF istext THEN error (66) ELSE operdepw := fupdtplace ;
	      6 : (* FGET *) IF istext THEN error (66) ELSE operdepw := getdirplace ;
	      7 : (* FPUT *) IF istext THEN error (66) ELSE operdepw := putdirplace ;
	      8 : (* FCLOSE *) operdepw := fcloseplace ;
	      9 : (* FAPPEND *) operdepw := fappendplace ;
	      10 : (* FREOPEN *) operdepw := freopenplace ;
	    END (* case TYPEIO *) ;
	    IF typeio IN [4, 6, 7] THEN
	      BEGIN                             (* FCONNECT,FGET,FPUT *)
	        IF no <> 15 (* , *) THEN
		BEGIN error (20) ; skip (46) ; GOTO 10 ;
		END ;
	        freeallregisters ;
	        insymbol ; expression ;
	        IF gattr.typtr <> NIL THEN
		IF typeio = 4 (* FCONNECT *) THEN
		  BEGIN
		    IF isstring (gattr) THEN
		      BEGIN
		        IF gattr.kind = chain THEN (* PACKED ARRAY OF CHAR *)
			loclong := gattr.alfactp^.alfalong ELSE
			loclong := gattr.typtr^.size ;
		        loadadr (gattr, pr2) ;
		        genstand (nreg, loclong, ilda, tdl) ;
		      END                     (* STRING *)
		    ELSE IF gattr.typtr^.father_schema = string_ptr THEN (* VAR STRING *)
		        BEGIN
			loadadr (gattr, pr2) ;
			genstand (pr2, 0, ilda, tn) ;
			genstand (pr2, 1, iepp2, tn) ;
		        END
		      ELSE error (19) ;
		  END (* 4 *) ELSE
		  BEGIN                       (* FGET,FPUT *)
		    IF gattr.typtr^.form <> numeric THEN
		      error (15) ELSE
		      BEGIN
		        transfer (gattr, inacc) ;
		        freebloc (gattr.ldregbloc) ;
		      END
		  END ;
	      END (* FCONNECT,FGET,FPUT *) ;
	    genstand (pr0, operdepw, itsp3, tn) ;
	  END (* FORM=FILES *) ELSE
	  IF gattr.typtr^.form = pointer THEN
	    BEGIN
	      IF envstandard <> stdextend THEN error (68) ;
	      IF typeio = 2 THEN
	        BEGIN
		resetused := true ;
		transfer (gattr, inacc) ;
		freebloc (gattr.ldregbloc) ;
		genstand (pr0, resetheapplace, itsp3, tn) ;
	        END ELSE error (125) ;
	    END (* RESET POINTER *) ELSE
	    error (125) ;
        IF no <> 10 (* ) *) THEN
	BEGIN
	  error (4) ; skip (46) ;
	END ELSE
	insymbol ;
10 :                                              (* EXIT PROCEDURE *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '^^^ FIN GETPUT ^^^ WITH NO:', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* GETPUT *) ;

$OPTIONS page $

(* ************************************ NEWIR ******************************** *)

    PROCEDURE newir (fcode : integer) ;

(* C  .CALLED BY STATEMENT  FOR  STANDARD PROCEDURE
   NEW   FCODE  IS  0
   DISPOSE   FCODE  IS  1
   .GENERATES  THE  CALL  OF PASCAL OPERATORS
   C *)
(* E ERRORS DETECTED
   4: ')' EXPECTED
   9: '(' EXPECTED
   103: IDENTIFIER IS NOT OF APPROPRIATE CLASS
   104: IDENTIFIER NOT DECLARED
   107: ERROR IN SELECTOR.
   125: ERROR IN TYPE OF ARGUMENT OF STANDARD PROCEDURE
   145: TYPE CONFLICT
   158: MISSING CORRESPONDING VARIANT DECLARATION
   344: Too large item
   345: Dispose pas compatible avec extensions
   E *)

      LABEL
        10 ;                                      (* EXIT PROCEDURE *)
      VAR

        generic : ctp ;                           (* RETURNED BY COMPATBIN *)
        isnew : boolean ;                         (* TRUE  FOR NEW, FALSE FOR DISPOSE *)
        harddispose, ptpack : boolean ;
        lattr : attr ;                            (* USED TO ASSIGN POINTER *)
                                                  (* AFTER NEW OPERATOR *)
        lerr : boolean ;
        etendu : boolean ;
        savegattr : attr ;
        locctp : ctp ;
        ltemp, locval, locop : integer ;
        linst : istand ;
        notfound : boolean ;
        pt : ctp ;
        sizeofnew : integer ;                     (* SIZE TO BE ALLOCATE IN WORDS *)
        ltag : tag ;


(* ************************************ LSKIPERROR< NEWIR   ***************** *)

      PROCEDURE lskiperror (ferrnum : integer) ;
        BEGIN
	error (ferrnum) ; skip (46) ; GOTO 10 ; (* EXIT OF NEWIR *)
        END (* LSKIPERROR *) ;

      BEGIN                                       (* NEWIR *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '^^^ DEBUT NEWIR ^^^ with FCODE:', fcode : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
        isnew := fcode = 0 ;                      (* true  FOR STANDARD PROCEDURE "NEW" *)
        etendu := false ;
        IF NOT isnew THEN
	disposeused := true ;
        IF no <> 9 THEN lskiperror (9) ;
        freeallregisters ;
        insymbol ; variab (true) ;                (* SETTING OF THE VARIABLE *)
        WITH gattr DO
	IF typtr <> NIL THEN
	  WITH typtr^ DO
	    BEGIN
	      IF form <> pointer THEN lskiperror (125) ;
	      IF eltype = NIL THEN              (* PREVIOUS ERROR *)
	        BEGIN skip (46) ; GOTO 10 ;
	        END ;
	      pt := eltype ;
	    END (* with TYPTR^,GATTR *) ELSE
	  BEGIN                                 (* ERROR *)
	    skip (46) ; GOTO 10 ;
	  END ;
                                                  (* COMPUTE  ALLOCATION SIZE *)
        IF no = 15 (* , *) THEN
	BEGIN
	  pt := pt^.recvar ;
	  REPEAT
	    IF pt = NIL THEN lskiperror (158) ;
	    insymbol ;
	    IF no = 1 (* ID *) THEN
	      BEGIN
	        search ;
	        IF ctptr = NIL THEN lskiperror (104) ;
	        IF ctptr^.klass <> konst THEN lskiperror (103) ;
	        compatbin (pt^.casetype, ctptr^.contype, generic) ;
	        IF (generic = NIL) OR (generic = realptr) THEN lskiperror (145) ;
	        locval := ctptr^.values ;
	      END (* NO=1 *) ELSE
	      IF (no = 2) AND (cl IN [1, 4]) THEN (* INT,CHAR CSTE *)
	        BEGIN
		lerr := true ;
		IF cl = 1 (* INT *) THEN
		  BEGIN
		    IF pt^.casetype^.form = numeric THEN lerr := false
		  END ELSE
		  WITH pt^ DO
		    BEGIN
		      IF casetype^.subrng THEN
		        BEGIN IF casetype^.typset = charptr THEN lerr := false ;
		        END ELSE
		        IF casetype = charptr THEN lerr := false ;
		    END ;
		IF lerr THEN lskiperror (145) ;
		locval := ival ;
	        END ELSE
	        lskiperror (107) ;
                                                  (* SEARCHS SELECTOR IN VARIANT LIST *)
	    notfound := true ;
	    locctp := pt^.variants ;
	    WHILE (locctp <> NIL) AND notfound DO
	      WITH locctp^ DO
	        IF caseval = locval THEN notfound := false ELSE locctp := nxtel ;
	    IF notfound THEN lskiperror (158) ;
	    sizeofnew := locctp^.casesize ;
	    sizeofnew := recadre (sizeofnew, bytesinword) DIV bytesinword ;
	    pt := locctp^.variants ;
	    insymbol ;
	  UNTIL no <> 15 (* , *) ;
	END (* NO=15 *) ELSE
	sizeofnew := recadre (pt^.size, bytesinword) DIV bytesinword ;
        IF no = 49 THEN                           (* -> *)
	BEGIN
	  IF NOT isnew THEN error (345) ;
                                                  (* Save all registers *)
	  IF gattr.inxreg <> nxreg THEN sauvereg (gattr.inxreg, false) ;
	  IF gattr.basereg <= maxprused THEN sauvereg (gattr.basereg, false) ;
	  savegattr := gattr ;

	  insymbol ; expression ;
	  WITH gattr DO
	    BEGIN
	      IF typtr = NIL THEN
	        BEGIN
		skip (46) ; GOTO 10 ;
	        END ELSE
	        IF typtr^.form <> numeric THEN
		lskiperror (15) ELSE
		BEGIN
		  etendu := true ;
		  transfer (gattr, inacc) ;

(* Words now *)
		  genstand (nreg, 1, isba, tdl) ; genstand (nreg, 2, iars, tn) ;
		  genstand (nreg, 1, iada, tdl) ;

		  freebloc (gattr.ldregbloc) ;
		  gattr := savegattr ;
		  linst := iada ; ltag := tdl ; locop := newplace ;
		END (* OK for type *) ;
	    END (* With gattr *) ;
	END (* NO=49 *) ;
        harddispose := false ;
        IF NOT isnew THEN
	IF NOT varissimple (gattr) THEN
	  harddispose := true ;
        IF NOT harddispose THEN
	BEGIN
	  IF gattr.inxreg <> nxreg THEN sauvereg (gattr.inxreg, false) ;
	  IF gattr.basereg <= maxprused THEN sauvereg (gattr.basereg, false) ;
	END ;                                   (* EASY DISPOSE *)
        lattr := gattr ;
        IF etendu THEN ELSE
	IF isnew THEN
	  BEGIN
	    linst := ilda ; ltag := tdl ; locop := newplace ;
	  END ELSE
	  BEGIN                                 (* DISPOSE *)
	    linst := ieax7 ; ltag := tn ; locop := disposeplace ;
	    IF harddispose THEN
	      BEGIN
	        ptpack := gattr.pckd ;
	        loadadr (gattr, pr3) ;
	        genstand (pr6, evareaw, ispri3, tn) ;
                                                  (* ADDRESS OF ITEM *)
	        sauvereg (raq, false) ;
	        IF ptpack THEN
		BEGIN
		  genstand (pr3, 0, ilprp3, tn) ;
		  ltemp := oldnewstor (bytesindword) DIV bytesinword ;
		  genstand (pr6, ltemp, ispri3, tn) ;
		  genstand (pr6, ltemp, ildaq, tn) ;
		END ELSE
		genstand (pr3, 0, ildaq, tn) ;
	      END ELSE
	      BEGIN
	        transfer (gattr, inacc) ;
	        freebloc (gattr.ldregbloc) ;
	      END ;
	  END (* DISPOSE *) ;
        IF sizeofnew <= maxnewsize THEN
	genstand (nreg, sizeofnew, linst, ltag) ELSE
	error (344) ;
        genstand (pr0, locop, itsp3, tn) ;
                                                  (* RETURNS "ITS" IN AQ *)
                                                  (* (nil FOR DISPOSE) *)
        WITH gattr DO
	BEGIN
	  kind := lval ;
	  ldreg := raq ; newbloc (raq) ;
	  ldregbloc := currentbloc ;
	END ;
        IF harddispose THEN
	BEGIN
	  genstand (pr6, evareaw, iepp3, tny) ;
	  genstand (pr6, evareaw, istaq, tn) ;
	  freebloc (currentbloc) ;
	  genstand (pr6, evareaw, iepp1, tny) ;
	  IF ptpack THEN
	    BEGIN
	      genstand (pr3, 0, isprp1, tn) ;
	    END ELSE
	    BEGIN
	      genstand (pr3, 0, ispri1, tn) ;
	    END ;
	END ELSE
	transfer (lattr, out) ;
        IF no <> 10 THEN lskiperror (4) ;
        insymbol ;
10 :                                              (* EXIT PROC *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '^^^ FIN NEWIR ^^^ with NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* NEWIR *) ;

$OPTIONS page $

(* *****************************************     STOPSTAT     ***************** *)

    PROCEDURE stopstat ;

(* C Compilation de la procedure predefinie SOL     STOP ( returncode )

   On appelle un runtime dont les fonction sont les suivantes
   . fermeture des fichiers
   . retour au systeme et renvoie d'un code d'erreur

   C *)

(* E ERRORS DETECTED
   4 : ")" expected
   9 : "(" expected
   15 : Numeric type expected

   E *)

      LABEL
        10 ;                                      (* Exit if error *)

      CONST
                                                  (* nd01 *)
        param2disp = 8 ;
        param3disp = 16 ;
        param4disp = 20 ;
                                                  (* nf01 *)

      VAR
        locop : integer ;

      BEGIN                                       (* STOPSTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ Debut de STOPSTAT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $

        IF no <> 9 (*   (   *) THEN
	BEGIN
	  error (9) ; skip (46) ;
	  GOTO 10 ;
	END ;

        insymbol ; expression ;                   (* ANALYSIS OF GIVEN RETURNCODE *)
        IF gattr.typtr <> NIL THEN
	BEGIN
	  IF gattr.typtr^.form <> numeric THEN
	    error (15) ELSE
	    BEGIN
	      transfer (gattr, inacc) ;         (* Found return code value *)
                                                  (* On stocke la valeur trouvee *)
	      freebloc (gattr.ldregbloc) ;
	      IF level = 0 THEN
	        locop := stopshortplace ELSE
	        BEGIN
		IF NOT exportablecode THEN
		  BEGIN
		    loadbase (0) ;
		    IF currentpr <> pr1 THEN
		      genstand (currentpr, 0, iepp1, tn) ;
                                                  (* PR1 points MAIN stack frame   *)
		    freebloc (currentbloc) ;
		    locop := stopplace ;
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
		    locop := stopextplace ;
		  END (* EXPORTABLE *) ;
		getpr4afterstop := true ;

	        END ;                           (* OPERATOR SELECTION *)

(* Charge PR2 avec adresse sequence de retour du main *)
	      IF NOT linktoend THEN
	        BEGIN
		linktoendplace := lkc ;
		lkc := lkc + bytesindword ;
		linktoend := true ;
	        END ;
	      genstand (prlink, linktoendplace DIV bytesinword, iepp2, tny) ;

	      genstand (pr0, locop, itsp3, tn) ;

	    END (* Numeric found *) ;
	END (* Gattr.typtr <> nil *) ;

        IF no <> 10 (* ) *) THEN
	BEGIN
	  error (4) ; skip (46) ;
	END ELSE
	insymbol ;

10 :                                              (* Error exit *)

$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ Fin de STOPSTAT @@@ avec NO :', no : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* STOPSTAT *) ;

$OPTIONS page $

(* *****************************************     ARGVSTAT     ***************** *)

    PROCEDURE argvstat ;

(* C Compilation de la procedure predefinie SOL ARGV( rang, string    )

   On appelle un runtime

   C *)

(* E ERRORS DETECTED
   4 : ")" expected
   9 : "(" expected
   15 : Numeric type expected
   19 : String variable expected
   20 : ","   expected

   E *)

      LABEL
        10 ;                                      (* Exit if error *)

      CONST

      VAR

        is_var_string : boolean ;
        string_attr : attr ;
        addrplace : integer ;
        errinrang : boolean ;
        errintarget : boolean ;
        rangattr : attr ;
        stringbloc : regpt ;
        stringpr : register ;
        locop : integer ;
      BEGIN                                       (* ARGVSTAT *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ Debut de ARGVSTAT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $

        is_var_string := false ;
        errinrang := true ; errintarget := true ;


        IF no <> 9 (*   (   *) THEN
	BEGIN
	  error (9) ; skip (46) ;
	  GOTO 10 ;
	END ;

        insymbol ; expression ;                   (* ANALYSIS OF GIVEN RANG *)
        IF gattr.typtr <> NIL THEN
	BEGIN
	  IF gattr.typtr^.form <> numeric THEN
	    error (15) ELSE
	    BEGIN
	      transfer (gattr, inq) ;
	      rangattr := gattr ;
	      errinrang := false ;
	    END (* Numeric found *) ;
	END (* Gattr.typtr <> nil *) ;

        IF no <> 15 (* , *) THEN
	BEGIN
	  IF gattr.typtr <> NIL THEN
	    error (20) ;
	  skip (20) ;
	  IF no <> 15 THEN
	    BEGIN
	      IF gattr.typtr = NIL THEN
	        error (20) ;
	      skip (46) ; GOTO 10 ;
	    END ;
	END ;

        insymbol ;
        variab (true) ;

        IF gattr.typtr <> NIL THEN
	BEGIN
	  IF NOT isstring (gattr) THEN
	    IF gattr.typtr^.father_schema = string_ptr THEN
	      BEGIN
	        IF gattr.typtr^.actual_parameter_list <> NIL THEN
		BEGIN
		  errintarget := false ;
		  is_var_string := true ;
		  loadadr (gattr, nreg) ;
		  stringbloc := currentbloc ;
		  stringpr := currentpr ;
		  WITH gattr.typtr^ DO
		    BEGIN
		      IF actual_parameter_list^.klass = konst THEN
		        gencstecode (actual_parameter_list^.values, ilda)
		      ELSE
		        BEGIN
			addressvar (actual_parameter_list, string_attr, false) ;
			transfer (string_attr, inacc) ;
			freeattr (string_attr) ;
		        END ;
		    END ;
		END
	      END ELSE
	      error (19) ELSE
	    BEGIN
	      loadadr (gattr, nreg) ;
	      stringbloc := currentbloc ;
	      stringpr := currentpr ;
	      gencstecode (gattr.typtr^.size, ilda) ;
	      errintarget := false ;
	    END (* OK for string *) ;
	END (* GATTR.TYPTR <> nil *) ;

(* NOW CODE GENERATION *)
        IF NOT (errinrang OR errintarget) THEN
	BEGIN

	  regenere (rangattr.ldregbloc) ;       (* RQ ok = Rang desire *)
	  freebloc (rangattr.ldregbloc) ;

(* PR1 = TARGET STRING OK *)

(* RA ok = String long *)

(* SELECT OPERATOR *)
	  IF level = 0 THEN
	    locop := argvshortplace ELSE
	    BEGIN
	      IF NOT exportablecode THEN
	        BEGIN
		loadbase (0) ;
		IF currentpr <> pr2 THEN
		  genstand (currentpr, 0, iepp2, tn) ;
                                                  (* PR2 points MAIN stack frame   *)
		regenere (stringbloc) ;
		regenere (currentbloc) ;
		IF stringpr <> pr1 THEN
		  genstand (stringpr, 0, iepp1, tn) ;
		freebloc (currentbloc) ;
		locop := argvplace ;
	        END ELSE
	        BEGIN
		IF NOT linktomain THEN
		  BEGIN
		    linktomainplace := lkc ;
		    lkc := lkc + bytesindword ;
		    linktomain := true ;
		  END ;
		genstand (prlink, linktomainplace DIV bytesinword, iepp2, tny) ;
                                                  (* PR2 points MAIN entry point *)
		locop := argvextplace ;
	        END (* EXPORTABLE *) ;

	    END ;                               (* OPERATOR SELECTion *)
	  freebloc (stringbloc) ;

	  IF is_var_string THEN
	    BEGIN
	      addrplace := oldnewstor (bytesindword) DIV bytesinword ;
	      genstand (pr6, addrplace, ispri1, tn) ;
	      genstand (pr1, 1, iepp1, tn) ;
	    END ;

	  genstand (pr0, locop, itsp3, tn) ;

	  IF is_var_string THEN
	    genstand (pr6, addrplace, ista, tny) ;

	END (* no ERROR *) ;

        IF no <> 10 (* ) *) THEN
	BEGIN
	  error (4) ; skip (46) ;
	END ELSE
	insymbol ;

10 :                                              (* Error exit *)

$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ Fin de ARGVSTAT @@@ avec NO :', no : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* ARGVSTAT *) ;

$OPTIONS page $

(* *********************************************** DATE AND TIME ********** *)

    PROCEDURE dateandtime (whatisit : integer) ;

(* E ERRORS DETECTED
   4    )   expected
   9    ( expected
   74    string or packed array of char with size 8 expected

   E *)

(* C  Analysis and code generation for the non-standard predefined procedures
   DATE   and TIME
   (0)       (1)      for the parameter  WHATISIT

   C *)

      LABEL
        10 ;                                      (* EXIT IF ERROR *)

      VAR
        string_attr : attr ;
        var_string : boolean ;
        lerr : boolean ;
        lopplace : integer ;
      BEGIN                                       (* DATE AND TIME *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT DATE AND TIME @@@ WHIT PARAM', whatisit : 6) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        IF no # 9 (*   ( *) THEN
	BEGIN
	  error (9) ; skip (46) ; GOTO 10
	END ;
        freeallregisters ; insymbol ; variab (true) ;
        WITH gattr DO
	IF typtr # NIL THEN
	  BEGIN
                                                  (* CHECK PARAMETER TYPE *)
	    lerr := true ;
	    var_string := false ;
	    IF isstring (gattr) THEN
	      lerr := (typtr^.size <> alfaleng)
	    ELSE
	      IF gattr.typtr^.father_schema = string_ptr THEN
	        WITH gattr.typtr^ DO
		IF actual_parameter_list <> NIL THEN
		  BEGIN
		    var_string := true ;
		    IF actual_parameter_list^.klass = konst THEN
		      lerr := actual_parameter_list^.values < 8
		    ELSE
		      BEGIN
		        lerr := false ;
		        addressvar (actual_parameter_list, string_attr, false) ;
		        transfer (string_attr, inacc) ;
		        freeattr (string_attr) ;
		        genstand (nreg, 8, icmpa, tdl) ;
		        genstand (nreg, 4, itpl, tic) ;
		        genexceptcode (26, ra) ;
		      END ;
		  END ;

	    IF lerr THEN error (74) ELSE
	      BEGIN                             (* NOT ERR *)
	        IF whatisit = 0 (* DATE *) THEN lopplace := dateopplace ELSE
                                                  (* TIME *) lopplace := timeopplace ;
	        loadadr (gattr, pr3) ;
	        IF var_string THEN
		BEGIN
		  genstand (nreg, 8, ilda, tdl) ;
		  genstand (pr3, 0, ista, tn) ; (* STORE LENGTH 8 FOR STRING PARAMETER *)
		  genstand (pr3, 1, iepp3, tn) ; (* GIVE REAL STRING ADDR *)
		END ;
	        genstand (pr6, evareaw, ispri3, tn) ;
                                                  (* CALL OPERATOR *)
	        genstand (pr0, lopplace, itsp3, tn) ;

(* NOW RAQ IS LOADED WITH CHARS
   MM/DD/YY FOR DATE
   HH:MM:SS FOR TIME *)
	        genstand (pr6, evareaw, iepp3, tny) ;
	        genstand (pr6, evareaw, istaq, tn) ;
	        mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
	        geneism (imlr, ord (' '), p0t0r0) ;
	        gendesca (pr6, evareaw, 0, l9, alfaleng, tn) ;
	        WITH gattr DO
		IF kind = varbl THEN usednameaddr := nameaddr ;
	        gendesca (pr3, 0, 0, l9, alfaleng, tn) ;
	      END (* NOT ERR *) ;
	  END (* TYPTR NOT NIL *) ;

        IF no = 10 (* ) *) THEN
	insymbol ELSE
	BEGIN error (4) ; skip (46) ;
	END ;

10 :                                              (* EXIT HERE IF ERROR *)

$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN DATE AND TIME @@@ WITH NO,CL ', no : 4, cl : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $

      END (* DATE AND TIME *) ;

$OPTIONS page $

(* ************************************ INSAPP ******************************** *)

    PROCEDURE insapp (typefct : integer) ;

(* C  COMPILATION OF INSERT  ,TWO PREDECLARED  PROC. USED FOR AUTOCOMPILATION.
   APPEND
   *INSERT(A,B,C) -SHIFTS      THE CONTENT  OF A   LEFT  B BITS   AND
   'OR'S  THEM INTO C
   -A,B UNCHANGED
   *APPEND(A,B,C) +SHIFTS   THE CONTENT OF A   LEFT  B BITS  AND
   'OR'S  C  INTO IT.  B AND C  UNCHANGED
   TYPEFCT= 0   FOR  INSERT
   = 1   FOR  APPEND
   C *)
(* E ERRORS DETECTED
   4: ')' EXPECTED
   9: '(' EXPECTED
   15: NUMERIC EXPECTED
   20: ',' EXPECTED
   21: ILLEGAL SHIFT COUNT
   26 : PACKED NOT ALLOWED HERE
   E *)
      LABEL
        10 ;                                      (* EXIT PROCEDURE *)
      VAR
        isinsert, lerr, easyo : boolean ;
        assattr, lattr : attr ;
        ltag : tag ;
        lcount, ldisp, ldep, lad : integer ;
        lbase : preg ;
      BEGIN                                       (* INSAPP *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT INSAPP @@@ WITH TYPEFCT:', typefct : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
        IF no # 9 (* ( *) THEN
	BEGIN
	  error (9) ; skip (46) ; GOTO 10 ;
	END ;
        isinsert := typefct = 0 ;
        freeallregisters ;
        lerr := true ;
        insymbol ;
        IF isinsert THEN
	expression ELSE variab (true) ;
        IF gattr.typtr # NIL THEN
	IF gattr.typtr@.form # numeric THEN error (15) ELSE
	  BEGIN
	    IF NOT isinsert THEN
	      BEGIN
	        IF varissimple (gattr) THEN
		BEGIN
		  easyo := true ; assattr := gattr ;
		END ELSE
		BEGIN
		  easyo := false ; lad := 0 ;
		  IF gattr.pckd THEN error (26) ELSE
		    BEGIN
		      loadadr (gattr, nreg) ;
		      gattr.basereg := currentpr ; gattr.basebloc := currentbloc ;
		      gattr.dplmt := 0 ; gattr.itsdplmt := 0 ;
		      lad := oldnewstor (bytesindword) DIV bytesinword ;
		      genstand (pr6, lad, prinst [spri, currentpr], tn) ;
		    END ;
		END ;
	      END ;
	    transfer (gattr, inq) ;
	    lattr := gattr ;
	    lerr := false ;
	  END ;
        IF no # 15 (* , *) THEN
	BEGIN
	  error (20) ; skip (46) ; GOTO 10 ;
	END ;
                                                  (*  RQ = INITIAL VALUE  OF  "A" *)
                                                  (* . TO  BE KEPT *)
        insymbol ; expression ;                   (* SHIFT  COUNT *)
        WITH gattr DO
	IF typtr # NIL THEN
	  IF typtr@.form # numeric THEN error (15) ELSE
	    BEGIN
	      IF kind = sval THEN
	        BEGIN
		IF (val < 0) OR (val > bitsinword - 1) THEN
		  BEGIN
		    error (21) ; val := 0 ;
		  END ;
		ltag := tn ; lcount := val ;
	        END (* SVAL *) ELSE
	        BEGIN
		transfer (gattr, inacc) ;
		ltag := tal ; lcount := 0 ;
	        END ;
	      IF NOT lerr THEN
	        regenere (lattr.ldregbloc) ;
	      genstand (nreg, lcount, iqls, ltag) ;
	      freeattr (gattr) ;
	    END (* SHIFT "A"   LEFT  "B" *) ;
        IF no # 15 (* , *) THEN
	BEGIN
	  error (20) ; skip (46) ; GOTO 10 ;
	END ;
        insymbol ;
        IF isinsert THEN
	variab (true) ELSE expression ;
        IF gattr.typtr # NIL THEN
	IF gattr.typtr@.form # numeric THEN error (15) ELSE IF NOT lerr THEN
	    WITH gattr DO
	      IF isinsert THEN
	        BEGIN
		IF varissimple (gattr) THEN
		  genstand (basereg, dplmt DIV bytesinword, iorsq, tn) ELSE
		  IF gattr.pckd THEN error (26) ELSE
		    BEGIN
		      ldep := lattr.ldregbloc@.saveplace ;
		      IF ldep = 0 THEN
		        BEGIN
			genstand (pr6, evareaw, istq, tn) ;
			ldep := evareaw ;
		        END ELSE
		        ldep := ldep DIV bytesinword ;
		      freebloc (lattr.ldregbloc) ;
		      sauvereg (rq, true) ;   (* TO RESERVE RQ *)
		      calcvarient (gattr, lbase, ldisp, ltag) ;
		      genstand (lbase, ldisp, ildq, ltag) ;
		      genstand (pr6, ldep, iorq, tn) ;
		      genstand (lbase, ldisp, istq, ltag) ;
		      freebloc (currentbloc) ; (* FREE NOW RQ *)
		    END ;
	        END (* ISINSERT *) ELSE
	        BEGIN
		transfer (gattr, inq) ;       (* SAVE LATTR IF NOT SAVED *)
		genstand (pr6, lattr.ldregbloc@.saveplace DIV bytesinword, iorq, tn) ;
		IF easyo THEN
		  transfer (assattr, out) ELSE
		  BEGIN
		    genstand (pr6, lad, istq, tny) ;
		    freebloc (gattr.ldregbloc) ;
		  END ;
		freebloc (lattr.ldregbloc) ;
	        END (* APPEND *) ;
                                                  (* WITH GATTR,NOTLERR,NUMERIC, *)
                                                  (* #NIL  ENDED *)
        IF no = 10 THEN
	insymbol ELSE
	BEGIN
	  error (4) ; skip (46) ;
	END ;
10 :
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN INSAPP @@@ WITH NO:', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* INSAPP *) ;

$OPTIONS page $

(* ************************************ PCKUNPCK ****************************** *)

    PROCEDURE pckunpck (code : integer) ;

(* C  . COMPILATION   OF   PACK(
   UNPACK(
   .CODE = 0   FOR  PACK  ( A,I,Z)
   = 1   FOR  UNPACK( Z,A,I)
   WHERE   A  IS AN   ARRAY[ S1  ]  OF T
   Z  IS AN  PACKED ARRAY[ U..V] OF T
   I  STARTING POINT  IN  A
   .PACK   MOVES  A[I]....  A[I +(V-U)]   IN  Z[U].. Z[V]
   .UNPACK MOVES  Z[U]..Z[V]              IN  A[I].. A[ I+(V-U)]
   C *)
(* E ERRORS DETECTED
   4: ')' EXPECTED
   9: '(' EXPECTED
   20: ',' EXPECTED
   139: INDEX TYPE NOT COMPATIBLE
   142: ARRAY EXPECTED
   143: Element type allowed is scalar,pointer or numeric
   159: UNPACKED ARRAY EXPECTED
   160: PACKED ARRAY  EXPECTED
   161: CONFORMANT ARRAY  NOT READY
   162: ORIGIN AND TARGET MUST HAVE SAME ELEMENT TYPE
   163: ELEMENT TYPE TOO LARGE
   302: INDEX OUT  OF  BOUNDS
   E *)
      LABEL
        10 ;                                      (* EXIT  PROCEDURE *)
      VAR
        loa, hia, loz, hiz, oincr, tincr, lincr, locexit, locloop : integer ;
        itype, generic : ctp ;
        oattr, tattr, iattr : attr ;
        erro, errt, erri, oisconf, tisconf, ispack : boolean ;
        prtoadd, oripr, tarpr : preg ;
        lload, lstor, ladd : istand ;
      BEGIN                                       (* PCKUNPCK *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT PCKUNPCK @@@ WITH CODE:', code : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
        ispack := code = 0 ;
        IF no # 9 (* ( *) THEN
	BEGIN
	  error (9) ; skip (46) ; GOTO 10 ;
	END ;
        erro := true ; errt := true ; erri := true ;
        itype := NIL ; oattr.typtr := NIL ; tattr.typtr := NIL ; iattr.typtr := NIL ;
                                                  (* ANALYSIS  OF ORIGIN, *)
                                                  (* A FOR PACK, Z FOR UNPACK *)
        insymbol ;
        freeallregisters ;
        variab (false) ;
        WITH gattr DO
	IF typtr # NIL THEN
	  BEGIN
	    IF typtr@.form # arrays THEN error (142) ELSE
	      IF NOT (typtr^.aeltype^.form IN [numeric, scalar, pointer]) THEN
	        error (143) ELSE
	        IF typtr@.pack = ispack THEN
		BEGIN
		  IF ispack THEN error (159) ELSE error (160) ;
		END ELSE
		BEGIN                         (* ORIGIN OK *)
		  IF typtr@.conformant THEN
		    BEGIN
		      error (161) ; oisconf := true ; erro := true ;
		    END ELSE
		    BEGIN
		      oisconf := false ; oincr := typtr@.subsize ;
		      WITH typtr@ DO
		        IF ispack THEN
			BEGIN
			  loa := lo ; hia := hi ; itype := inxtype ;
			END (* ISPACK *) ELSE
			BEGIN               (* UNPACK *)
			  loz := lo ; hiz := hi ;
			END ;
		      erro := false ;
		      loadadr (gattr, nreg) ;
		      WITH oattr DO           (* POINTS  ELEMENT OF ORIGIN *)
		        BEGIN
			initattrvarbl (oattr) ;
			typtr := gattr.typtr@.aeltype ;
			vlev := gattr.vlev ;
			basereg := currentpr ;
			basebloc := currentbloc ;
			access := pointee ;
			pckd := NOT ispack ;
		        END (* WITH OATTR *) ;
		    END (* NOT CONFORMANT *) ;
		END (* OK FOR ORIGIN *) ;
	  END (* TYPTR # NIL *) ;
        IF no = 15 (* , *) THEN
	insymbol ELSE
	BEGIN
	  IF gattr.typtr # NIL THEN error (20) ;
	  skip (15) ;
	  IF no # 15 THEN
	    BEGIN
	      IF gattr.typtr = NIL THEN error (20) ;
	      GOTO 10 ;
	    END ELSE insymbol ;
	END ;
        IF ispack THEN
	expression ELSE variab (true) ;
        WITH gattr DO
	IF typtr # NIL THEN
	  IF ispack THEN
	    BEGIN
	      compatbin (itype, typtr, generic) ;
	      IF (generic = NIL) OR (generic = realptr) THEN
	        error (139) ELSE
	        BEGIN
		IF oisconf THEN
		  BEGIN
                                                  (* TO BE SUPPLIED *)
		  END ELSE
		  BEGIN
		    arrayboundsctp@.nmin := loa ; arrayboundsctp@.nmax := hia ;
		    IF kind = sval THEN
		      BEGIN
		        checkminmax (val, arrayboundsctp, 302) ; val := val - loa ;
		      END (* SVAL *) ELSE
		      BEGIN
		        IF kind # lval THEN transfer (gattr, inacc) ;
		        IF inxcheck THEN
			checkbnds (pckerrcode, ldreg, arrayboundsctp) ;
		        IF loa # 0 THEN
			gencstecode (loa, opaq [sub, ldreg]) ;
		      END ;                   (* NOT SVAL *)
		    iattr := gattr ;
		  END ;                       (* NOT CONFORMANT *)
		erri := false ;
	        END ;                           (* SUITABLE GENERIC *)
	    END (* ISPACK *) ELSE
	    BEGIN                               (* UNPACK *)
	      IF typtr@.form # arrays THEN error (142) ELSE
	        IF typtr@.pack THEN error (159) ELSE
		IF typtr@.aeltype # oattr.typtr THEN error (162) ELSE
		  BEGIN
		    IF typtr@.conformant THEN
		      BEGIN
		        error (161) ; tisconf := true ; errt := true ;
		      END ELSE
		      BEGIN
		        tisconf := false ; errt := false ;
		        loa := typtr@.lo ; hia := typtr@.hi ; itype := typtr@.inxtype ;
		        tincr := typtr@.subsize ;
		        loadadr (gattr, nreg) ;
		        WITH tattr DO
			BEGIN
			  initattrvarbl (tattr) ;
			  typtr := gattr.typtr@.aeltype ;
			  vlev := gattr.vlev ;
			  basereg := currentpr ;
			  basebloc := currentbloc ;
			  access := pointee ;
			END ;
		      END (* NOT CONFORM *) ;
		  END (* NO ERROR *) ;
	    END (* UNPACK *) ;
        IF no = 15 THEN                           (* , *)
	insymbol ELSE
	BEGIN
	  IF gattr.typtr # NIL THEN error (20) ;
	  skip (15) ;
	  IF no # 15 THEN
	    BEGIN
	      IF gattr.typtr = NIL THEN error (20) ;
	      GOTO 10 ;
	    END ;
	END ;
        IF ispack THEN
	variab (true) ELSE expression ;
        WITH gattr DO
	IF typtr # NIL THEN
	  BEGIN
	    IF ispack THEN
	      BEGIN
	        IF typtr@.form # arrays THEN error (142) ELSE
		IF NOT typtr@.pack THEN error (160) ELSE
		  IF typtr@.aeltype # oattr.typtr THEN error (162) ELSE
		    BEGIN
		      IF typtr@.conformant THEN
		        BEGIN
			error (161) ; tisconf := true ; errt := true ;
		        END ELSE
		        BEGIN
			errt := false ; tisconf := false ;
			loz := typtr@.lo ; hiz := typtr@.hi ; tincr := typtr@.subsize ;
			loadadr (gattr, nreg) ;
			WITH tattr DO
			  BEGIN
			    initattrvarbl (tattr) ;
			    typtr := gattr.typtr@.aeltype ;
			    vlev := gattr.vlev ;
			    basereg := currentpr ;
			    basebloc := currentbloc ;
			    access := pointee ;
			    pckd := true ;
			  END ;
		        END (* NOT CONF. *) ;
		    END (* NO ERR *) ;
	      END (* PACK *) ELSE
	      BEGIN                             (* UNPACK *)
	        compatbin (itype, typtr, generic) ;
	        IF (generic = NIL) OR (generic = realptr) THEN
		error (139) ELSE
		BEGIN
		  IF tisconf THEN
		    BEGIN
                                                  (* TO BE SUPPLIED *)
		    END ELSE
		    BEGIN
		      arrayboundsctp@.nmin := loa ;
		      arrayboundsctp@.nmax := hia ;
		      IF kind = sval THEN
		        BEGIN
			checkminmax (val + hiz - loz, arrayboundsctp, 302) ;
			checkminmax (val, arrayboundsctp, 302) ;
			val := val - loa ;
		        END (* SVAL *) ELSE
		        BEGIN
			IF kind # lval THEN transfer (gattr, inacc) ;
			IF inxcheck THEN
			  checkbnds (pckerrcode, ldreg, arrayboundsctp) ;
			IF loa # 0 THEN
			  gencstecode (loa, opaq [sub, ldreg]) ;
		        END (* NOT SVAL *) ;
		      iattr := gattr ;
		    END (* NOT CONF *) ;
		  erri := false ;
		END (* NO ERR *) ;
	      END (* UNPACK *) ;
	  END (* TYPTR #NIL *) ;
        IF NOT erro THEN
	IF NOT errt THEN
	  IF NOT erri THEN
	    BEGIN
	      regenere (oattr.basebloc) ; regenere (tattr.basebloc) ;
	      IF iattr.kind # sval THEN
	        regenere (iattr.ldregbloc) ELSE
	        transfer (iattr, inacc) ;
	      IF inxcheck THEN
	        BEGIN
		IF iattr.ldreg = rq THEN
		  BEGIN lstor := istq ; ladd := iadq ; lload := ildq ;
		  END ELSE
		  BEGIN lstor := ista ; ladd := iada ; lload := ilda ;
		  END ;
		genstand (pr6, evareaw, lstor, tn) ;
		gencstecode ((hiz - loz), ladd) ;
		arrayboundsctp@.nmin := 0 ; arrayboundsctp@.nmax := hia - loa ;
		checkbnds (pckerrcode, iattr.ldreg, arrayboundsctp) ;
		genstand (pr6, evareaw, lload, tn) ;
	        END ;
	      IF ispack THEN
	        BEGIN
		prtoadd := oattr.basereg ; lincr := oincr ;
	        END ELSE
	        BEGIN
		prtoadd := tattr.basereg ; lincr := tincr ;
	        END ;
	      oripr := oattr.basereg ; tarpr := tattr.basereg ;
	      IF lincr # 1 THEN
	        BEGIN
		IF lincr > 4 THEN
		  error (163) ELSE
		  genstand (nreg, lincr DIV 2, opaq [shiftl, iattr.ldreg], tn) ;
	        END ;
	      genstand (prtoadd, 0, ia9bd, modif [iattr.ldreg]) ; (* POINTS NOW A[I] *)
                                                  (* INIT NOW  LOOP    U..V *)
	      freeattr (iattr) ;
	      genstand (nreg, oincr, ieax6, tn) ;
	      genstand (nreg, tincr, ieax7, tn) ;
	      gencstecode (loz, ilda) ;
	      locloop := cb ; transfer (oattr, inq) ;
	      gattr := oattr ;
	      transfer (tattr, out) ;
                                                  (* NOW  CHECK LAST MOVE *)
	      genstand (nreg, 1, iada, tdl) ;
	      gencstecode (hiz, icmpa) ;
	      locexit := indfich ; genstand (nreg, 0, itpnz, tic) ;
                                                  (* HERE  LOOP NOT  ENDED. *)
                                                  (* POINTS NEXT ELEMENTS *)
	      genstand (oripr, 0, ia9bd, tx6) ;
	      genstand (tarpr, 0, ia9bd, tx7) ;
	      genstand (nreg, (locloop - cb) DIV bytesinword, itra, tic) ;
	      inser (cb, locexit) ;
	    END (* NOT  ERRI, ERRO, ERRT *) ;
        IF no = 10 THEN
	insymbol ELSE
	BEGIN error (4) ; skip (46) ;
	END ;
10 :                                              (* EXIT PROCEDURE *)
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN PCKUNPCK @@@ WITH NO,CL:', no : 4, cl : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* PCKUNPCK *) ;

$OPTIONS page $

(* *****************************************    MVCIR     ******** *)

    PROCEDURE mvcir (codop : integer) ;

(* C   ISCLEAN    1 for SUBARRAY
   0 for MVC
   C *)

      LABEL
        10 ;                                      (* Exit procedure *)

      VAR
        erro, errt, errl : boolean ;
        typelem : ctp ;
        easyo, easyt, easyl : boolean ;
        baseo, baset : preg ;
        dplmtow, dplmttw, dplmtob, dplmttb : integer ;
        basebloco, basebloct : regpt ;
        longop : integer ;
        longreg : register ;
        isclean : boolean ;

      BEGIN                                       (* MVCIR *)

$OPTIONS cc = trace + $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ debut MVCIR @@@ with CODOP', codop : 4) ;
	  nextline ;
	END ;
$OPTIONS cc = trace - $
        erro := true ; errt := true ; errl := true ;
        basebloco := NIL ; basebloct := NIL ;
        isclean := false ;

(* ORIGIN ANALYSIS *)
        freeallregisters ;
        insymbol ;
        variab (false) ;
        WITH gattr DO
	IF typtr <> NIL THEN
	  BEGIN
	    IF isclean THEN
	      BEGIN
	      END ELSE
	      BEGIN
	        erro := false ;
	      END (* NOT CLEAN *) ;
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
	    IF isclean THEN
	      BEGIN
	      END ELSE
	      BEGIN
	        errt := false ;
	      END (* NOT CLEAN *) ;
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
	        IF isclean THEN
		BEGIN
		END (* ISCLEAN *) ELSE
		BEGIN
		  IF kind = sval THEN
		    BEGIN
		      easyl := true ; longop := val ;
		    END (* SVAL *) ELSE
		    BEGIN                     (* NOT SVAL *)
		      easyl := false ;
		      IF kind <> lval THEN
		        transfer (gattr, inacc) ;
		      longreg := gattr.ldreg ;
		    END (* NOT SVAL *) ;
		END                           (* NOT CLEAN *)
	      END ;                             (* NUMERIC *)
	  END (* typtr not nil for third paramater *) ;
        IF NOT (erro OR errt OR errl) THEN
	BEGIN
	  IF NOT easyo THEN regenere (basebloco) ;
	  IF NOT easyt THEN regenere (basebloct) ;
	  IF easyl THEN
	    BEGIN
	      mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
	      geneism (imlr, ord (' '), p0t0r0) ;
	      gendesca (baseo, dplmtow, dplmtob, l9, longop, tn) ;
	      gendesca (baset, dplmttw, dplmttb, l9, longop, tn) ;
	    END (* EASYL *) ELSE
	    BEGIN                               (* register loaded with length *)
	      mfari1 := a1r1i0 ; mfari2 := a1r1i0 ;
	      geneism (imlr, ord (' '), p0t0r0) ;
	      gendesca (baseo, dplmtow, dplmtob, l9, 0, modif [longreg]) ;
	      gendesca (baset, dplmttw, dplmttb, l9, 0, modif [longreg]) ;
	    END (* not easy *) ;
	  freebloc (basebloco) ; freebloc (basebloct) ;
	  IF NOT easyl THEN freebloc (gattr.ldregbloc) ;
	END ;
        IF no <> 10 THEN
	BEGIN
	  error (4) ; skip (46) ;
	END ELSE
	insymbol ;
10 :                                              (* EXIT IF ERRORS *)
$OPTIONS cc = trace + $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ fin mvcir @@@ with NO,CL ', no : 4, cl : 4) ;
	  nextline ;
	END ;
$OPTIONS cc = trace - $

      END (* MVCIR *) ;

$OPTIONS page$

(* ************************************ INSERT_STRING ********************************** *)

    PROCEDURE insert_string ;

      LABEL
        1 ;
      VAR
        string_attr, disp_attr, insert_attr : attr ;
        dummy, l_err : boolean ;
      BEGIN
        l_err := false ;
        IF no <> 9 THEN
	BEGIN error (9) ; skip (46) ; GOTO 1 END ;

        initattrvarbl (string_attr) ; initattrvarbl (disp_attr) ; initattrvarbl (insert_attr) ;
        insymbol ;
        expression ;
        IF NOT is_possible_string (gattr) THEN
	BEGIN l_err := true ; error (274) END ;
        insert_attr := gattr ;
        IF no <> 15 THEN
	BEGIN
	  error (20) ; l_err := true
	END
        ELSE insymbol ;
        variab (true) ;
        string_attr := gattr ;
        IF string_attr.typtr = NIL THEN l_err := true
        ELSE IF string_attr.typtr^.father_schema <> string_ptr THEN
	  BEGIN error (275) ; l_err := true END ;
        IF no <> 15 THEN
	BEGIN
	  error (20) ; l_err := true
	END
        ELSE insymbol ;
        expression ;
        IF gattr.typtr = NIL THEN l_err := true
        ELSE
	IF gattr.typtr^.form <> numeric THEN
	  BEGIN
	    error (15) ; l_err := true
	  END ;
        disp_attr := gattr ;
        IF no <> 10 THEN
	BEGIN
	  error (4) ; skip (15)
	END
        ELSE insymbol ;
        IF NOT l_err THEN
	gen_insert (insert_attr, string_attr, disp_attr)
        ELSE BEGIN
	  freeattr (string_attr) ; freeattr (disp_attr) ; freeattr (insert_attr)
	END ;
1 :
      END (* INSERT_STRING *) ;


$OPTIONS page$

(* **************************************************** DELETE_STRING ************************ *)

    PROCEDURE delete_string ;

      LABEL
        1 ;
      VAR
        string_attr, disp_attr, len_attr : attr ;
        dummy, l_err : boolean ;
      BEGIN
        l_err := false ;
        IF no <> 9 THEN
	BEGIN error (9) ; skip (46) ; GOTO 1 END ;

        initattrvarbl (string_attr) ; initattrvarbl (disp_attr) ; initattrvarbl (len_attr) ;
        insymbol ;
        variab (true) ;
        string_attr := gattr ;
        IF string_attr.typtr = NIL THEN l_err := true
        ELSE IF string_attr.typtr^.father_schema <> string_ptr THEN
	  BEGIN error (275) ; l_err := true END ;
        check_dynamic_string_length (string_attr) ;
        IF no <> 15 THEN
	BEGIN
	  error (20) ; l_err := true
	END
        ELSE insymbol ;
        expression ;
        IF gattr.typtr = NIL THEN l_err := true
        ELSE
	IF gattr.typtr^.form <> numeric THEN
	  BEGIN
	    error (15) ; l_err := true
	  END ;
        disp_attr := gattr ;
        IF no <> 15 THEN
	BEGIN
	  error (20) ; l_err := true
	END
        ELSE insymbol ;
        expression ;
        IF gattr.typtr = NIL THEN l_err := true
        ELSE
	IF gattr.typtr^.form <> numeric THEN
	  BEGIN
	    error (15) ; l_err := true
	  END ;
        len_attr := gattr ;
        IF no <> 10 THEN
	BEGIN
	  error (4) ; skip (15)
	END
        ELSE insymbol ;
        IF NOT l_err THEN
	gen_delete (string_attr, disp_attr, len_attr)
        ELSE BEGIN
	  freeattr (string_attr) ; freeattr (disp_attr) ; freeattr (len_attr)
	END ;
1 :
      END (* DELETE_STRING *) ;
    BEGIN
    END.                                          (* Fin des procedures predefinies    *)
