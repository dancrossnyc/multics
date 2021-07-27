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
  PROGRAM declare ;
    $IMPORT
                                                  (* IMPORTED PROCEDURES *)
      'pascal_context_ (alm)' : asciiformataddr, octalformataddr ;
                                                  (* FROM PL1 *)
      'pascal_gen_bin_area (pl1)' : genbinarea ;
      'pascal_gen_ext_variable (pl1)' : genextvariable ;
      'pascal_gen_entry_point (pl1)' : genentrypoint ;
      'pascal_gen_export_file (pl1)' : genexportfile ;
      'pascal_gen_rel_$text (pl1)' : genreltext ;
      'RACINE (pascal)' :
        crealfabox,
        error,
        generrorlink,
        geninputlink,
        genoutputlink,
        inconst,
        inserundlab,
        insymbol,
        nameisref,
        nextline,
        poweroftwo,
        recadre,
        skip,
        skipextd,
        skiptochapter,
        statement_begins,
        statement_ends,
        sup,
        warning ;
      'UNIQUE (pascal)' :
        heaperror ;
      'STANDSTAT (pascal)' :
        compstat ;
      'CONTEXTTABLE (pascal)' :
        boundary,
        bytesneeded,
        checkminmax,
        compatbin,
        create_vars_box,
        create_types_box,
        create_proc_box,
        create_field_box,
        create_konst_box,
        create_tagfield_box,
        create_dummyclass_box,
        existfileintype,
        packedcadre,
        packedsize,
        printrec ;

      'GENERE (pascal)' :
        closefile,
        exitlabel,
        enterreftosymbol,
        genalfa,
        genc,
        gencodfonct,
        geninsertion,
        gen_init_fsb_trap_structures,
        genmulticsnil,
        genpgexit,
        genprcentry,
        genprcexit,
        genprolog,
        genr,
        genstand,
        genstring,
        infich,
        initiozone,
        writout ;
      'optimized_procedures (alm)' : search, srchrec ;
                                                  (* IMPORTED VARIABLES *)
      'RACINE (pascal)' :
        alfaptr,
        aval,
        boxheader,
        bufval,
        charptr,
        check_id,
        cl,
        conint,
        conreel,
        ctptr,
        currentnode,
        declarationpart,
        display,
        environt,
        envstandard,
        errtotal,
        exportablecode,
        extcalltrapplace,
        forbidden_id,
        init_fsb_trap_flag,
        intptr,
        ival,
        lastproc,
        level,
        listyes,
        longchaine,
        longstring,
        majmin,
        mapswitch,
        mpcogerr,
        mpcogout,
        next,
        nilptr,
        no,
        progname,
        realptr,
        staticswordcount,
        statnbr,
        string_ptr,
        symbolfile,
        symbolindex,
        symbolline,
        symbolmap,
        top,
        usednames,
        version,
        xc ;
      'GENERE (pascal)' :
        cb,
        fichinter,
        ic,
        indfich,
        usednameaddr ;
      'STATE (pascal)' :
        currlcstpt,
        currllcstpt,
        currrcstpt,
        currwcstpt,
        lcsave,
        linktoend,
        linktoendplace,
        tmax$

    $EXPORT
      body,
      analyzing_schema,
      building_from_schema,
      checkexternalitem,
      clabix,
      createexternalbox,
      decltrace,
      externallistheader,
      filpts,
      filtop,
      firstlabbox,
      forbidden_id_list,
      getpr4afterstop,
      hdrfile,
      hdrindex,
      hdrlength,
      hdrline,
      initdeclare,
      labtab,
      lc,
      lkc,
      maxctp,
      nextalf,
      lab_pdl_top,
      push_lab_pdl,
      pop_lab_pdl,
      symbtabl,
      tabform,
      tabkinds,
      tabklass,
      tabkonst,
      tabpdef$






$INCLUDE 'CONSTTYPE' $



$OPTIONS page $

    VAR
                                                  (* REDEFINE NOW IMPORTED VARIABLES *)
                                                  (* FROM RACINE *)
      alfaptr : ctp ;
      aval : alfaid ;
      boxheader : PACKED ARRAY [1..120] OF char ;
      bufval : ARRAY [1..maxval] OF char ;
      charptr : ctp ;
      check_id : boolean ;
      cl : integer ;
      conint : integer ;
      conreel : real ;
      currentnode : blocknodeptr ;
      ctptr : ctp ;
      declarationpart : boolean ;
      display : ARRAY [0..displimit] OF recidscope ;
      entrylength : integer ;
      environt : contexte ;
      envstandard : stdkind ;
      errtotal : integer ;
      exportablecode : boolean ;
      extcalltrapplace : integer ;
      forbidden_id : alfaid ;
      init_fsb_trap_flag : boolean ;
      functionflag : boolean ;
      intptr : ctp ;
      ival : integer ;
      lastproc : blocknodeptr ;
      level : levrange ;
      listyes : boolean ;
      longchaine : integer ;
      longstring : integer ;
      majmin : ARRAY [0..127] OF integer ;
      mpcogerr : text ;
      mpcogout : text ;
      next : ctp ;
      nilptr : ctp ;
      no : integer ;
      progname : alfaid ;
      realptr : ctp ;
      staticswordcount : integer ;
      statnbr : integer ;
      string_ptr : ctp ;
      symbolfile : integer ;
      symbolindex : integer ;
      symbolline : integer ;
      symbolmap : boolean ;
      top : integer ;
      usednames : typusednames ;
      version : integer ;
      xc : integer ;
                                                  (* FROM GENERE *)
      mapswitch : boolean ;
      cb : integer ;
      fichinter : ^binartype ;
      ic : integer ;
      indfich : integer ;
      usednameaddr : ctp ;
                                                  (* FROM STATE *)
      currlcstpt : lcstpt ;
      currllcstpt : llcstpt ;
      currrcstpt : rcstpt ;
      currwcstpt : wcstpt ;
      lcsave : integer ;
      tmax : integer ;
      linktoend : boolean ;
      linktoendplace : integer ;
                                                  (* FROM ALM OR PL1 *)
      asciiformataddr : ctp ;
      octalformataddr : ctp ;


(* NOW  DEFINE  EXPORTABLE  VARIABLES *)

      analyzing_schema,
      building_from_schema : schema_status ;
      clabix : integer ;
                                                  (* POINTS LAST USED ENTRY IN LABTAB *)
      decltrace : levtrace ;                      (* TO USE TRACE IN COMPILATION OF DECLARE *)
      externallistheader : ptexternalitem ;
      filpts : ARRAY [0..fillimit] OF ctp ;       (* CONTAINS POINTERS ON BOXES "VAR" *)
                                                  (* FOR EACH DECLARED FILE *)
      filtop : integer ;
      firstlabbox : labelblockptr ;
      forbidden_id_list : alfalistptr ;
      getpr4afterstop : boolean ;                 (* TRUE IF STOP USES UNWINDER *)
                                                  (* POINTS LAST USED ENTRY IN  FILPTS *)
      hdrfile : integer ;                         (* FILE OF PROGRAM OR PROCEDURE HEADER *)
      hdrindex : integer ;                        (* INDEX OF PROGRAM OR PROCEDURE HEADER *)
      hdrlength : integer ;                       (* LENGTH OF PROGRAM OR PROCEDURE HEADER *)
      hdrline : integer ;                         (* LINE OF PROGRAM OR PROCEDURE HEADER *)
      lab_pdl_top : lab_pdl_ptr ;                 (* CURRENT PUSH-POP LABEL BLOCK BLOCK PTR *)
      labtab : ARRAY [1..maxlabs] OF labdescr ;
                                                  (* FOR EACH LEVEL, DECLARED LABELS ARE *)
                                                  (* MEMORIZED FROM FSTIX (BODY)  --> CLABIX  *)
      lc : integer ;
                                                  (* DISPLACEMENT  COUNTER OF STACK'S ELEMENTS *)
      lkc : integer ;                             (* OFFSET IN LINK. SECTION FOR ALL EXT. ITEMS *)
      maxctp : ctp ;
                                                  (* MAX  POSITION  REACHED IN HEAP *)
      nextalf : ctp ;                             (* GIVES THE BEGINNING OF THE CHAIN OF *)
                                                  (* USED 'ALFA CONSTANTES' IN  A PROCEDURE  *)
      symbtabl : boolean ;                        (*  INDICATES  IF INFORMATIONS FOR *)
                                                  (* SYMBOLIC DUMP IS TO BE GENERATED *)

(* NOW DEFINE  INTERNAL  VARIABLES *)
      cadre : integer ;
                                                  (* USED IN TYPEDECL TO FIND THE NEEDED *)
                                                  (* BOUNDARY FOR A VARIABLE,FIELD *)
      dversion : integer ;                        (* VERSION OF DECLARE *)
      err : boolean ;
      exportscode : boolean ;
                                                  (* PROPAGATES AN ERROR CONDITION *)
      filev : ARRAY [levrange] OF integer ;
                                                  (* FILES DECLARED AT LEVEL "N"  *)
                                                  (* ARE MEMORIZED IN  ENTRIES *)
                                                  (* FILEV[N] ..FILTOP  IN ARRAY FILPTS *)
      first_forbidden_id : alfalistptr ;
      globnbpar : integer ;                       (* NUMBER OF PARAMETERS USED IN A PROCEDURE. *)
                                                  (* +1 FOR FUNCTION , +4 FOR EACH CONF. ARRAY *)
      globdescriptors : boolean ;
      longparam : integer ;
                                                  (* LENGTH  OF  PARAMETER'S LIST *)
      old_check_id : boolean ;
      np : blocknodeptr ;
      pendingtypeallowed : boolean ;
      ptlist : ARRAY [0..ptlimit] OF
      RECORD
        hname : alfaid ;                          (* USED BUT NOT DECLARED NAME *)
        pptr : ctp ;                              (* BOX ASSOCIATED WITH POINTER ON THIS NAME *)
        rfil, rlin : integer ;
      END ;
      ptx : integer ;
                                                  (*  POINTS  FIRST FREE ENTRY  IN PTLIST  *)
      structispack : boolean ;
                                                  (* ASSOCIATED  WITH "PACKED" FOR A STRUCTURE *)
      tabklass : ARRAY [idklass] OF alfa ;
      tabform : ARRAY [typform] OF alfa ;
      tabkonst : ARRAY [consttype] OF alfa ;
      tabkinds : ARRAY [idkinds] OF alfa ;
      tabpdef : ARRAY [idprocdef] OF alfa ;
                                                  (* TRACES . ALFA  ASSOCIATED WITH SCALAR TYPES *)
      terrcl : ARRAY [norange] OF typofsymb ;
                                                  (* ERR. RECOVERY IN TYPE DECL. PART *)
      valuenb : integer ;                         (* VALUEDECL'CALLS COUNTER .MUST BE 1 *)

(* END OF VARIABLES FOR  MODULE  DECLARE *)


$OPTIONS page $

    $VALUE
      tabklass = ('TYPES   ', 'KONST   ', 'PROC    ', 'VARS    ', 'FIELD   ', 'TAGFIELD',
        'DUMMYCLA') ;
      tabform = ('REEL    ', 'NUMERIC ', 'SCALAR  ', 'POINTER ', 'POWER   ', 'ARRAYS  ',
        'RECORDS ', 'FILES   ', 'ALIASTYP') ;
      tabkonst = ('WORDCONS', 'DWORCONS', 'ALFACONS') ;
      tabkinds = ('ACTUAL  ', 'FORMAL  ', 'ARRAYBOU', 'EXPORTAB', 'IMPORTED') ;
      tabpdef = ('STANDDEF', 'FORWDEF', 'EXTDEF') ;
      terrcl = (9 * irrelsy,
        begsy,                                    (*  9  (          *)
        endsy,                                    (* 10  )     *)
        irrelsy,
        endsy,                                    (* 12  ]     *)
        3 * irrelsy,
        endsy,                                    (* 16  ;     *)
        irrelsy,
        begsy,                                    (* 18  ^     *)
        2 * irrelsy,
        3 * endsy,                                (* 21  BEGIN  22  END  23  IF  *)
        2 * irrelsy,
        endsy,                                    (* 26  CASE  *)
        irrelsy,
        endsy,                                    (* 28  REPEAT *)
        irrelsy,
        endsy,                                    (* 30  WHILE *)
        irrelsy,
        endsy,                                    (* 32  FOR   *)
        2 * irrelsy,
        endsy,                                    (* 35  GOTO  *)
        irrelsy,
        endsy,                                    (* 37  TYPE  *)
        begsy,                                    (* 38  ARRAY RECORD FILE SET  *)
        irrelsy,
        2 * endsy,                                (*  40 LABEL  41  CONST  *)
        irrelsy,
        3 * endsy,                                (* 43 VAR  44  FUNCTION  45  PROCEDURE *)
        2 * irrelsy,
        endsy,                                    (* 48  WITH  *)
        irrelsy,
        endsy,                                    (* 50  PROGRAM *)
        7 * endsy) (* 51  $RENAME  52  $IMPORT  53  $EXPORT  54  $VALUE  57  $  *) $



$OPTIONS page $

(*  IMPORTED   PROCEDURES   HEADERS   *)
(*   FROM RACINE *)
    FUNCTION recadre (fnumber, fmod : integer) : integer ; EXTERNAL ;
    PROCEDURE insymbol ; EXTERNAL ;
    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE srchrec (fbegsearch : ctp) ; EXTERNAL ;
    PROCEDURE inconst (VAR code : integer ; VAR restype : ctp ; fnxt : ctp ; expression_allowed : boolean) ; EXTERNAL ;
    PROCEDURE crealfabox (VAR fkonstbox : ctp) ; EXTERNAL ;
    PROCEDURE skip (nosymb : integer) ; EXTERNAL ;
    PROCEDURE skipextd (nosymb : setofno) ; EXTERNAL ;
    PROCEDURE skiptochapter ; EXTERNAL ;
    PROCEDURE search ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    FUNCTION sup (fval1, fval2 : integer) : integer ; EXTERNAL ;
    PROCEDURE inserundlab (fcb, fdebchn : integer) ; EXTERNAL ;
    FUNCTION poweroftwo (fval : integer) : integer ; EXTERNAL ;
    PROCEDURE nameisref (p : ctp ; f, l : integer) ; EXTERNAL ;
    PROCEDURE statement_begins (genp : boolean) ; EXTERNAL ;
    PROCEDURE statement_ends (sttlength : integer) ; EXTERNAL ;
    PROCEDURE warning (errno : integer) ; EXTERNAL ;

(*   FROM  UNIQUE *)
    PROCEDURE heaperror ; EXTERNAL ;

(* PROCEDURES FROM STANDSTAT *)

    PROCEDURE compstat ; EXTERNAL ;

(* PROCEDURES FROM CONTEXTTABLE *)

    FUNCTION boundary (objform : typform ; ispack : boolean ; pcksize : integer) : integer ; EXTERNAL ;
    FUNCTION bytesneeded (objform : typform ; highest : integer ; ispack : boolean) : integer ; EXTERNAL ;
    PROCEDURE checkminmax (fvalue : integer ; fctp : ctp ; ferrnum : integer) ; EXTERNAL ;
    PROCEDURE compatbin (typleft, typright : ctp ; VAR fgeneric : ctp) ; EXTERNAL ;
    PROCEDURE create_vars_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;
    PROCEDURE create_types_box (VAR fvbox : ctp ; fname : alfaid ; fform : typform ; fbool : boolean) ; EXTERNAL ;
    PROCEDURE create_proc_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;
    PROCEDURE create_field_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;
    PROCEDURE create_konst_box (VAR fvbox : ctp ; fname : alfaid ; ftypofconst : consttype) ; EXTERNAL ;
    PROCEDURE create_tagfield_box (VAR fvbox : ctp ; fname : alfaid ; ftagval : boolean) ; EXTERNAL ;
    PROCEDURE create_dummyclass_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;
    FUNCTION existfileintype (ptontype : ctp) : boolean ; EXTERNAL ;
    FUNCTION packedcadre (ftype : ctp) : integer ; EXTERNAL ;
    FUNCTION packedsize (ftype : ctp) : integer ; EXTERNAL ;
    PROCEDURE printrec (ptbox : ctp) ; EXTERNAL ;


(*   FROM   GENERE OR PL1  *)
    FUNCTION enterreftosymbol (ctplace : ctp) : integer ; EXTERNAL ;
    PROCEDURE genreltext (relcode, halfwordcount : integer) ; EXTERNAL ;
    PROCEDURE gen_init_fsb_trap_structures (filpt : ctp) ; EXTERNAL ;
    PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ; EXTERNAL ;
    PROCEDURE genmulticsnil ; EXTERNAL ;
    PROCEDURE genexportfile (nam : alfaid ; pr4disp : integer ; VAR returncode : integer) ; EXTERNAL ;
    PROCEDURE genprolog (VAR fplace : integer ; VAR fdebic : integer) ; EXTERNAL ;
    PROCEDURE genpgexit ; EXTERNAL ;
    PROCEDURE genprcentry (VAR fplace : integer ; fptproc : ctp ; VAR fdebic : integer) ; EXTERNAL ;
    PROCEDURE writout (zonedisp, endcode : integer) ; EXTERNAL ;
    PROCEDURE closefile (filept : ctp) ; EXTERNAL ;
    PROCEDURE exitlabel (flabinx : integer ; flabplace : integer) ; EXTERNAL ;
    PROCEDURE geninsertion (fplace : integer ; fptproc : ctp) ; EXTERNAL ;
    PROCEDURE gencodfonct (fptproc : ctp) ; EXTERNAL ;
    PROCEDURE genprcexit (fptproc : ctp) ; EXTERNAL ;
    PROCEDURE infich (fval : integer) ; EXTERNAL ;
    PROCEDURE genr (frval : real) ; EXTERNAL ;
    PROCEDURE genc (fval : integer) ; EXTERNAL ;
    PROCEDURE genstring (falfapt : ctp) ; EXTERNAL ;
    PROCEDURE genalfa ; EXTERNAL ;
    PROCEDURE initiozone (filpt : ctp) ; EXTERNAL ;

(* **************************************** GENEXTVARIABLE  ******************** *)

    PROCEDURE genextvariable (segname, varname, generator : alfaid ;
      pr4disp, varlength, endpoint : integer ;
      VAR binarea : binartype ;
      VAR returncode : integer) ; EXTERNAL ;

(* C     VARNAME       NAME OF THE VARIABLE
   Can be found in IMPORT_STRING
   SEGNAME       32 CHARS STRING
   GENERATOR      "   ""    ""
   PR4DISP       BYTES OFFSET OF "ITS" WANTED
   . < 0 FOR IMPORTED VARS
   VARLENGTH     BYTES SIZE OF THE VARIABLE
   ENDPOINT      MAX INDEX REACHED IN BINAREA
   BINAREA        BINARY ITEMS
   RETURNCODE 0 MEANS OK
   C *)


$OPTIONS page $

(* *********************************************  GENBINAREA ****************** *)

    PROCEDURE genbinarea (bytdisp, codearea, endpoint, endcode : integer ;
      VAR binarea : binartype ;
      VAR returncode : integer) ; EXTERNAL ;

(* C          BYTDISP         OFFSET IN AREA OF FIRST BYTE TO BE INIT.
   CODEAREA        1 = TEXT ; 3 =  STATIC(INIT)
   4 = STATIC(NON INIT)
   ENDPOINT        MAX INDEX REACHED IN BINAREA
   FOR "4" NUMBER OF HALFWORDS
   ENDCODE         LAST RELOCATABLE ITEM (TEXT SECTION)
   BINAREA         BINARY ITEMS TO BE GENERATED
   RETURNCODE      0 means OK
   C *)


(* ************************************ GENENTRYPOINT  (PL/1) ***************** *)

    PROCEDURE genentrypoint (textbytes, pr4bytes, typofentry : integer ;
      segname, entryname : alfaid ; functionflag : boolean ; VAR entrylength : integer ;
      VAR returncode : integer) ; EXTERNAL ;

(* C .TYPOFENTRY   0   PASCAL INTERNAL PROCEDURE
   1   PASCAL EXPORTABLE PROCEDURE
   2   IMPORTED PROCEDURE  ===>  NO ENTRY SEQUENCE
   4   EXIT LABEL          ===> NO ENTRY SEQUENCE
   .TEXTBYTES    OFFSET IN BYTES IN TEXT SECTION OF ENTRY POINT
   (NO MEANINGS IF TYPOFENTRY=2)
   .PR4BYTES     BYTES OFFSET OF AN EVEN-WORD IN LINKAGE SECTION TO BE FILLED
   WITH AN ITS
   .SEGNAME      32 CHARS STRING  BLANK FOR EXPORTABLE or LOCAL
   FOUND IN IMPORTSTRING FOR IMPORTED
   .ENTRYNAME    32 CHARS STRING  Pascal name ( LOCAL or EXPORT)
   FOUND IN IMPORTSTRING
   .RETURNCODE   0 means OK

   (NO MEANING FOR 0,4 )
   C *)


    PROCEDURE geninputlink (pr4disp : integer ; VAR returncode : integer) ; EXTERNAL ;
    PROCEDURE genoutputlink (pr4disp : integer ; VAR returncode : integer) ; EXTERNAL ;
    PROCEDURE generrorlink (pr4disp : integer ; VAR returncode : integer) ; EXTERNAL ;
                                                  (* END OF IMPORTED PROCEDURES  *)




$OPTIONS page $

(* ******************************************** INITDECLARE ******************* *)

    PROCEDURE initdeclare ;

(* C     THIS PROCEDURE INITIALIZES THE GLOBALS OF DECLARE AND IS CALLED IN
   INITIALISE IN THE MODULE UNIQUE                                      C *)
      BEGIN                                       (* INITDECLARE *)
        analyzing_schema.on := false ;
        building_from_schema.on := false ;
        clabix := 0 ;
        exportscode := false ;
        externallistheader := NIL ;
        globdescriptors := false ;
        filtop := -1 ; filev [0] := 0 ;
        new (firstlabbox) ;
        IF firstlabbox = NIL THEN heaperror ;
        WITH firstlabbox^ DO
	BEGIN
	  new (next) ;
	  IF next = NIL THEN heaperror ;
	  number := -1 ;
	  WITH next^ DO
	    BEGIN
	      next := NIL ;
	      number := 10000 ;
	    END ;
	END ;
        new (lab_pdl_top) ;
        WITH lab_pdl_top^ DO
	BEGIN
	  first_in_block := NIL ;
	  start := -1 ;
	  previous := NIL ;
	  next := NIL
	END ;
        new (first_forbidden_id) ;
        WITH first_forbidden_id^ DO
	BEGIN
	  previous := NIL ;
	  next := NIL ;
	  name := '  '
	END ;
        forbidden_id_list := first_forbidden_id ;
        functionflag := false ;
        getpr4afterstop := false ;
        lc := 0 ;
        lkc := 0 ;
        decltrace := none ;
        ptx := 0 ;
        symbtabl := false ;
        dversion := 00 ;
        IF dversion > version THEN version := dversion ;
        valuenb := 0 ;                            (* COUNTER FOR CALLS OF VALUEDECL *)
      END (* INITDECLARE *) ;


$OPTIONS page $


(* ********+**************************** PRINTEXTERNALBOX ********************* *)

$OPTIONS compile = trace $
    PROCEDURE printexternalbox (boxtoprint : ptexternalitem) ;

      BEGIN
        WITH boxtoprint^ DO
	BEGIN
	  nextline ; write (mpcogout, boxheader) ; nextline ;
	  write (mpcogout, '* This extern box is pointed by ^', ord (boxtoprint)) ;
	  nextline ;
	  write (mpcogout, '* EXTERNNAME, EXTNEXT,EXTRFILE1, LINE1, FILE2, LINE2 and EXTDECL are:',
	    extname, ' ^', ord (extnext), ' ^', extrfile1, extrline1, extrfile2, extrline2,
	    ord (extdecl)) ; nextline ;
	  write (mpcogout, '* EXTIMTEMTYPE is (ORD)', ord (extitemtype), '  EXTKIND is',
	    tabkinds [extkind]) ; nextline ;
	  write (mpcogout, '* EXTPLTDISP,EXTAREADISP and EXTLONG are :',
	    extpltdisp : 8, extareadisp, extlong) ; nextline ;
	  write (mpcogout, '* EXTSEGNAME,GENERATOR,ENTRYNAME are :',
	    '%', extsegname, '%', extgenerator, '%', extentryname, '%') ;
	  nextline ;
	  write (mpcogout, boxheader) ; nextline ; nextline ;
	END ;
      END (* PRINTEXTERNALBOX *) ;
$OPTIONS compile = true $



$OPTIONS page $

(* *************************************************   CHECKEXTERNALITEM   *)

    PROCEDURE checkexternalitem (fname : alfaid ; VAR foundext : ptexternalitem) ;

(* C
   DURING THE COMPILATION OF GLOBAL VARIABLES , FUNCTIONS AND
   PROCEDURES , EACH TIME A NEW IDENTIFIER ARRRIVES WE MUST VERIFY IF IT IS
   THE DEFINITION OF A YET DECLARED IMPORTED OR EXPORTED ITEM.
   IF FOUND, RETURNS THE POINTER TO THE EXTERNALITEM BOX EITHER RETURNS NIL.
   IF FOUND, THE FIELD EXTDECL WILL BE FILLED AFTER SUCCESSFUL COMPILATION OF ITEM

   C *)
(* E
   446  : THE EXTERNAL NAME FOUND WAS ALREADY DEFINED
   E *)

      LABEL
        1 ;                                       (* EXIT WHILE *)
      VAR
        workpt : ptexternalitem ;

      BEGIN                                       (* CHECKEXTERNALITEM *)
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ begining of CHECKEXTERNALITEM @@@ for name:',
	    fname) ; nextline ;
	END ;
$OPTIONS compile = true $
        workpt := externallistheader ;
        foundext := NIL ;                         (* default means "not found" *)
        WHILE workpt <> NIL DO
	BEGIN
	  IF workpt^.extname = fname THEN
	    BEGIN
	      foundext := workpt ;
	      GOTO 1 (* exit while *) ;
	    END ELSE
	    workpt := workpt^.extnext ;
	END (* while *) ;
1 :                                               (* exit while *)
$OPTIONS compile = security $
        IF foundext <> NIL THEN
	IF foundext^.extdecl <> NIL THEN
	  IF foundext^.extdecl^.klass = vars THEN error (446) ELSE
	    IF foundext^.extdecl^.procdef <> forwdef THEN
	      error (446) ;
$OPTIONS compile = true $
$OPTIONS compile = trace $
        IF decltrace = high THEN
	BEGIN
	  write (mpcogout, ' @@@ fin de CHECKEXTERNALITEM @@@ avec pointeur retournee a ^', ord (foundext)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* CHECKEXTERNALITEM *) ;


$OPTIONS page $

(* *****************************   CREATEEXTERNALBOX *********************** *)

    PROCEDURE createexternalbox (fname : alfaid ; fitemtype : externalitemtype ;
      fkind : idkinds ; VAR fvextbox : ptexternalitem) ;

(* C Creates a external box with specified values.
   Returns the pointer on created box
   Modify EXTERNALLISTHEADER ( new box created )
   C *)

(* E Errors detected
   Heaperror
   E *)

      VAR
        wkexternpt : ptexternalitem ;

      BEGIN                                       (* CREATEEXTERNALBOX *)
        new (wkexternpt) ; IF wkexternpt = NIL THEN heaperror ; (* Exit comp *)
        WITH wkexternpt^ DO
	BEGIN
	  extname := fname ; extrfile1 := symbolfile ; extrline1 := symbolline ;
	  extrfile2 := 0 ; extrline2 := 0 ;
	  extnext := externallistheader ; externallistheader := wkexternpt ;
	  extsegname := blank ; extgenerator := blank ; extentryname := blank ;
	  extdecl := NIL ;                      (* Filled later if item is declared *)
	  extkind := fkind ; extitemtype := fitemtype ;
	  extpltdisp := 0 ; extareadisp := 0 ; extlong := 0 ;
	END (* with *) ;

(*  <----- *)
        fvextbox := wkexternpt ;
      END (* CREATEEXTERNALBOX *) ;


$OPTIONS page $

    PROCEDURE push_lab_pdl ;

(* PUSH-POP LABEL BLOCK SYSTEM *)

      BEGIN
        IF lab_pdl_top^.next = NIL THEN
	BEGIN
	  new (lab_pdl_top^.next) ;
	  WITH lab_pdl_top^.next^ DO
	    BEGIN
	      previous := lab_pdl_top ;
	      next := NIL ;
	    END
	END ;
        lab_pdl_top := lab_pdl_top^.next ;
        WITH lab_pdl_top^ DO
	BEGIN
	  start := ic ;
	  first_in_block := NIL ;
	END
      END (* PUSH_LAB_PDL *) ;

$OPTIONS page $

    PROCEDURE pop_lab_pdl ;

(* PUSH-POP LABEL BLOCK SYSTEM *)

      VAR
        lbp : labelblockptr ;

      BEGIN
        WITH lab_pdl_top^ DO
	BEGIN
	  lbp := first_in_block ;
	  WHILE lbp <> NIL DO
	    BEGIN
	      lbp^.ref_allowed.ic_from := start ;
	      lbp^.ref_allowed.ic_to := ic - 1 ;
	      lbp := lbp^.next_in_block ;
	    END
	END ;

        IF lab_pdl_top^.previous <> NIL THEN      (* SECURITY *)
	lab_pdl_top := lab_pdl_top^.previous ;
      END (* POP_LAB_PDL *) ;

$OPTIONS page $

(* ********************************  CHECKDEFININGPOINT  *************** *)

    PROCEDURE checkdefiningpoint (fname : alfaid ; fbegsearch : ctp) ;

(* C
   A new identifier is to be  defined at this level.
   Before we must verify that this name is not already used:
   . as the name of a normally declared item
   . to identify an item declared in an englobing procedure
   and already used in the level we try to redeclare it
   . as the name of an item in course of declaration
   C *)

(* E ERRORS DETECTED
   101 : Identifier declared twice
   118 : Identifier already used at this level with another meaning
   E *)

      LABEL
        1 (* exit while *) ;

      VAR
                                                  (*  WORKPT  : PTLOCKEDITEM *)

      BEGIN                                       (* CHECKDEFININGPOINT *)
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ Debut de CHECKDEFININGPOINT @@@ ',
	    ' avec FBEGSEARCH ^', ord (fbegsearch) : 8,
	    ' et le nom:', fname) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        srchrec (fbegsearch) ;
        IF ctptr <> NIL THEN
	BEGIN
	  IF symbolmap THEN
	    nameisref (ctptr, symbolfile, symbolline) ;
	  error (101) ;
	END ELSE
	BEGIN                                   (* new identifier *)
                                                  (* This name was not already declared at this level.
                                                     Is it already used or pending ?       *)
                                                  (*
                                                     WORKPT:= LOCKEDLISTHEADER ;
                                                     while WORKPT<>nil do
                                                     if WORKPT^.LOCKEDNAME=FNAME then
                                                     begin
                                                     ERROR(118) ; goto 1 ;
                                                     end else
                                                     WORKPT:= WORKPT^.LOCKEDNEXT ;
                                                     *)
1 :                                               (* exit while *)
	END (* new identifier *) ;
$OPTIONS compile = trace $
        IF decltrace = high THEN
	BEGIN
	  write (mpcogout, '@@@ Fin   de CHECKDEFININGPOINT @@@ ') ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* CHECKDEFININGPOINT *) ;

$OPTIONS page $

(* *********************************************************TYPEDECL*********** *)

    PROCEDURE typedecl (VAR returnsize : integer ; VAR returntype : ctp) ;

(* C   CALLED AT EACH OCCURENCE OF <TYPE> IN PASCAL'S GRAMMAR:
   EITHER TO RECOGNIZE AN EXISTING TYPE
   OR TO CREATE BOX(ES) ASSOCIATED WITH A NEW TYPE.
   IN BOTH CASES,RETURNS SIZE OF AN OBJECT OF THIS TYPE AND POINTER ON THIS
   TYPE.
   WHEN AN ERROR IS FOUND, RETURNTYPE IS NIL.                             C *)
(* E   ERRORS DETECTED :
   HEAPERROR
   2  IDENTIFIER EXPECTED
   4  ')' EXPECTED
   8  'OF' EXPECTED
   10  ERROR IN TYPE DECLARATION
   11  '[' EXPECTED
   12  ']' EXPECTED
   13  'END' EXPECTED
   15  INTEGER EXPECTED
   62 Pointed type not defined
   96  ILLEGAL POINTED ITEM
   98  'PACKED' NOT ALLOWED HERE
   108 File not allowed here
   112 TOO LARGE ARRAY
   115  BASE TYPE MUST BE SCALAR OR NUMERIC
   169  ERROR IN BASE TYPE OF A SET
   268  TOO MANY FORWARD DEFINED POINTERS
   305  VALUE IN A SET OUT OF BOUNDS                                      E *)
      LABEL
        11,                                       (* ANALYSIS FOR TYPE OF ARRAY ELEMENT *)
        19 ;                                      (* END OF ARRAY TYPE *)
      VAR
        bigsize : real ;
        indexflag, lerr, packflag : boolean ;
        li, lh, lcad, elsize, displ, bdispl, sl, sh : integer ;
        nxta, lp, lt, eltyp, rtyp, nxtf, lastfld, recvpt,
        spt, locpt, lfpt, oldnxtf, pp : ctp ;
        check_id_saved : boolean ;


(* *********************************************************SKIPT < TYPEDECL*** *)

      PROCEDURE skipt (fno : integer) ;

(* C SKIPS ANY SYMBOL WHICH IS NOT BEGSY,ENDSY OR THE SPECIFIED
   ITEM   " FNO "                                                       C *)
        BEGIN
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, ' @@@ DEBUT SKIPT @@@ WITH   FNO ', fno : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $
	WHILE (terrcl [no] = irrelsy) AND (fno # no) DO insymbol ;
        END (* SKIPT *) ;


(* *********************************************************TYPERR < TYPEDECL** *)

      PROCEDURE typerr (ferrno : integer) ;

(* C   ASSIGNS RETURNED PARAMETERS OF TYPEDECL WITH DEFAULT VALUES, SKIPS
   ANY IRRELEVANT SYMBOL AND PRODUCES AN ERROR MESSAGE                   C *)
        BEGIN
	returnsize := 0 ; err := true ;
	returntype := NIL ;
	error (ferrno) ;
	skipt (46) ;                            (* 46 IS NOT ASSIGNED => SYMBOLS ARE *)
                                                  (* SKIPPED UNTIL BEGSY OR ENDSY *)
        END (* TYPERR *) ;


(* ***********************************************SIMPLETYPE<TYPEDECL********** *)

      PROCEDURE simpletype (VAR sretmin, sretmax : integer ; VAR srettype : ctp) ;

(* C   THIS PROCEDURE IS CALLED IN ORDER  TO
   EITHER RECOGNIZE A TYPE IDENTIFIER
   EITHER CREATE A SCALAR TYPE  (ID1,ID2,....)
   EITHER CREATE A SUBRANGE TYPE  CST1..CST2
   OR FIND MIN,MAX OF A SUBRANGE  WITHOUT CREATING ATYPE  (INDEXFLAG  TRUE)
   AND ASSIGNS RETURNSIZE FOR TYPEDECL
   C *)
(* E   ERRORS DETECTED
   2: IDENTIFIER EXPECTED
   4: ')' EXPECTED
   99: ILLEGAL BEGINNING ITEM FOR A SIMPLE TYPE
   101: IDENTIFIER DECLARED TWICE
   103: IDENTIFIER IS NOT OF APPROPRIATE CLASS
   104: IDENTIFIER NOT DECLARED
   113: INDEX    TYPE MUST BE SCALAR OR NUMERIC
   E *)
        LABEL
	2 ;                                     (* SKIP HERE IF ERROR *)
                                                  (* IN IDENTIFIER'S LIST *)
        VAR
	lerr : boolean ;
	cv : integer ;
	lp, nxtc, ltyp : ctp ;
	lnext, saved_next, lctp, generic, ctype : ctp ;
	ltop, saved_top, ccode, it : integer ;


(* *************************************SUBRANGE < SIMPLETYPE < TYPEDECL******* *)

        PROCEDURE subrange (VAR lowbound, highbound : integer ; VAR typcstes : ctp ;
	fbegsearch : ctp) ;

(* C USED TO RECOGNIZE A SUBRANGE(FIRST SYMBOL OF THE SUBRANGE HAS YET BEEN READ)
   THE BOUNDS ARE RETURNED IN LOWBOUND AND HIGHBOUND.
   THE TYPE OF THE CONSTANTS  IS RETURNED IN TYPCSTES.
   FBEGSEARCH GIVES THE FIRST ITEM TO BE INSPECTED IN CONTEXTTABLE.
   THE GLOBAL VARIABLE ERR GETS THE VALUE "TRUE" IF AN ERROR OCCURS(NO SKIP).
   C *)
(* E   ERRORS:   5 '..' EXPECTED
   102 HIGHBOUND MUST NOT BE LOWER THAN LOWBOUND
   113 INDEX TYPE MUST BE SCALAR OR NUMERIC
   114 BASE  TYPE MUST BE SCALAR OR NUMERIC
   145 TYPE CONFLICT                                              E *)
	VAR
	  dummy : integer ;
	  lowtype, hightype : ctp ;
	BEGIN
$OPTIONS compile = trace $
	  IF decltrace > none THEN
	    BEGIN
	      write (mpcogout, ' @@@ DEBUT SUBRANGE @@@ WITH  FBEGSEARCH', ord (fbegsearch)) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $
	  inconst (dummy, lowtype, fbegsearch, false) ;
	  IF symbolmap THEN
	    IF lowtype <> NIL THEN
	      IF lowtype^.name <> blank THEN
	        nameisref (lowtype, symbolfile, symbolline) ;
	  typcstes := lowtype ;
	  IF lowtype # NIL THEN
	    IF lowtype@.form IN [numeric, scalar] THEN
	      BEGIN
	        lowbound := conint ;            (* CONINT ASSIGNED BY INCONST *)
	        IF no = 39 (* .. *) THEN
		insymbol ELSE
		BEGIN error (5) ; err := true ;
		END ;
	        inconst (dummy, hightype, next, false) ;
	        IF lowtype # hightype THEN
		BEGIN
		  IF symbolmap THEN
		    IF hightype <> NIL THEN
		      IF hightype^.name <> blank THEN
		        nameisref (hightype, symbolfile, symbolline) ;
		  error (145) ; err := true ;
		END ELSE
		BEGIN
		  highbound := conint ;       (* SEE INCONST *)
		  IF lowbound > highbound THEN
		    BEGIN
		      error (102) ; err := true ;
		    END ;
		END ;                         (* NO ERROR IN HIGHTYPE *)
	      END (* NO ERROR IN LOWTYPE *) ELSE
	      BEGIN
	        err := true ; IF indexflag THEN error (113) ELSE error (114) ;
	      END (* TYPE NOT SCALAR OR NUMERIC *) ELSE (* LOWTYPE = NIL *)
	    err := true ;                       (* ERROR IS CALLED BY INCONST *)
$OPTIONS compile = trace $
	  IF decltrace > low THEN
	    BEGIN
	      write (mpcogout, ' @@@ FIN SUBRANGE  @@@  WITH V.LOW,HIGH,TYP BOUNDS', lowbound,
	        highbound, ord (typcstes)) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $
	END (* SUBRANGE *) ;


(* *************************************SCALDECL < SIMPLETYPE < TYPEDECL******* *)

        PROCEDURE scaldecl (fbegsearch : ctp) ;

(* C THIS PROCEDURE IS CALLED IN ORDER TO BUILD THE BOX ASSOCIATED WITH A TYPE
   CST1..CST2   (NOT CALLED FOR INDEX)
   THE BUILT TYPE IS EITHER (TYPES,NUMERIC)
   EITHER (TYPES,SCALAR,TRUE)
   THE RETURNED VALUES ARE:
   SRETMIN,SRETMAX (SIMPLETYPE)
   SRETTYPE        (SIMPLETYPE)
   RETURNSIZE      (TYPEDECL)
   CADRE           (GLOBAL)                         C *)
	VAR
	  lmin, lmax : integer ;
	  i1, i2 : integer ;
	  lp, lpp : ctp ;
	BEGIN
$OPTIONS compile = trace $
	  IF decltrace > none THEN
	    BEGIN
	      write (mpcogout, ' @@@ DEBUT SCALDECL @@@ WITH  FBEGSEARCH', ord (fbegsearch)) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $
	  subrange (lmin, lmax, lpp, fbegsearch) ;
	  IF NOT err THEN
	    BEGIN
	      IF lpp@.form = scalar THEN
	        BEGIN
		create_types_box (lp, blank, scalar, true) ;
		WITH lp^ DO
		  BEGIN
		    spksize := bytesneeded (scalar, lmax, true) ;
		    smin := lmin ; smax := lmax ;
		    typset := lpp ;
		    cadrage := boundary (scalar, packflag, spksize) ;
		    IF packflag THEN
		      size := spksize ELSE
		      size := bytesneeded (scalar, 0, false) ;
		  END ;
	        END (* SCALAR *) ELSE
	        BEGIN                           (* NUMERIC *)
		create_types_box (lp, blank, numeric, false) ;
		WITH lp^ DO
		  BEGIN
		    IF lmin >= 0 THEN
		      i1 := lmin ELSE
		      i1 := lmin + 1 ;
		    IF lmax >= 0 THEN
		      i2 := lmax ELSE
		      i2 := lmax + 1 ;
		    npksize := bytesneeded (numeric, sup (abs (i1), abs (i2)), true) ;
		    nmin := lmin ; nmax := lmax ;
		    cadrage := boundary (numeric, packflag, npksize) ;
		    IF packflag THEN
		      size := npksize ELSE
		      size := bytesneeded (numeric, 0, false) ;
		  END ;
	        END ;                           (* NUMERIC *)
	      WITH lp@ DO
	        BEGIN
		name := blank ; nxtel := NIL ; klass := types ; pack := packflag ;
		references := NIL ;
	        END ;
$OPTIONS compile = trace $
	      printrec (lp) ;
$OPTIONS compile = true $
	      sretmin := lmin ; sretmax := lmax ; (* FOR SIMPLETYPE *)
	      srettype := lp ;                  (* FOR SIMPLETYPE *)
	      returnsize := lp@.size ;          (* FOR TYPEDECL   *)
	      cadre := sup (cadre, lp@.cadrage) ;
	    END (* NOT ERR *) ELSE
	    srettype := NIL ;
$OPTIONS compile = trace $
	  IF decltrace > low THEN
	    BEGIN
	      write (mpcogout, ' @@@ FIN SCALDECL @@@ WITH SRET MIN, MAX, TYPE;RETSIZE,CADRE',
	        sretmin, sretmax, ord (srettype), returnsize, cadre) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $
	END (* SCALDECL *) ;


        BEGIN                                     (* SIMPLETYPE *)
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout,
	      ' @@@ DEBUT SIMPLETYPE @@@ WITH NEXT, NO,CADRE,PACKFLAG', ord (next),
	      no, cadre, packflag) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
	IF no = 1 (* ID *) THEN
	  BEGIN
	    lerr := err ; err := false ;
	    srchrec (next) ; IF ctptr = NIL THEN search ;
	    IF ctptr = NIL (* ID. NOT FOUND *) THEN
	      BEGIN
	        error (104) ; skipt (16) ; (* ; *) srettype := NIL ; err := true ;
	      END ELSE
	      IF ctptr^.klass = schema THEN
	        BEGIN
		IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		WITH building_from_schema DO
		  IF on THEN typerr (511)
		  ELSE
		    BEGIN
		      schema_ptr := ctptr ;
		      current_parameter := ctptr^.formal_parameter_list ;
		      current_token := ctptr^.token_list ;
		      insymbol ;              (* "(" *)
		      IF no <> 9 THEN typerr (9) ;
		      lnext := ctptr^.next_for_schema ;
		      WHILE (NOT err) AND (current_parameter <> NIL) DO
		        BEGIN
			insymbol ;
			inconst (ccode, ctype, next, true) ;
			compatbin (ctype, current_parameter^.vtype, generic) ;
			IF (generic = NIL) OR (generic = realptr) THEN typerr (271) (* ILLEGAL SHEMA PARAMETER SUBSTITUTION *)
			ELSE
			  BEGIN
			    checkminmax (conint, current_parameter^.vtype, 272) ;
			    create_konst_box (lctp, current_parameter^.name, wordconst) ;
			    WITH lctp^ DO
			      BEGIN
			        values := conint ; contype := generic ;
			        succ := lnext ;
			      END ;
			    lnext := lctp ;
			  END ;
			current_parameter := current_parameter^.nxtel ;
			IF NOT err THEN
			  IF current_parameter <> NIL THEN
			    IF no <> 15 (* , *) THEN typerr (20) ELSE insymbol
			  ELSE
			    IF no <> 10 THEN typerr (4)
		        END ;                 (* PARAMETER WHILE LOOP *)
		      IF err THEN srettype := NIL
		      ELSE
		        BEGIN
			on := true ;
			insymbol ;
			saved_next := next ; saved_top := top ;
			next := lnext ; top := schema_ptr^.top_for_schema ;
			typedecl (returnsize, srettype) ;
			next := saved_next ; top := saved_top ;
			on := false ;
			IF srettype <> NIL THEN
			  WITH srettype^ DO
			    BEGIN
			      father_schema := schema_ptr ;
			      actual_parameter_list := lnext ;
			      lctp := lnext ;
			      it := father_schema^.parameter_count ;
			      WHILE it <> 1 DO
			        BEGIN
				it := it - 1 ;
				lctp := lctp^.nxtel
			        END ;
			      lctp^.nxtel := NIL ; (* END OF ACTUAL PARAMETER LIST *)
			    END ;
		        END ;
		    END                       (* BUILDING FROM SHEMA *)
	        END
	      ELSE
	        BEGIN                           (* ID. FOUND *)
		IF ctptr@.klass = types (* ID. TYPE *) THEN
		  BEGIN
		    IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		    IF ctptr@.form = aliastype THEN ctptr := ctptr@.realtype ;
		    IF packflag THEN returnsize := packedsize (ctptr) ELSE
		      returnsize := ctptr@.size ;
		    IF indexflag (* ARRAY INDEX *) THEN
		      IF NOT (ctptr@.form IN [scalar, numeric]) THEN
		        BEGIN
			error (113) ; err := true ;
		        END ;
		    srettype := ctptr ;
		    IF ctptr@.form > pointer THEN
		      cadre := ctptr@.cadrage ELSE
		      cadre := boundary (ctptr@.form, packflag, returnsize) ;
		    WITH ctptr@ DO
		      CASE form OF
		        numeric : BEGIN sretmin := nmin ; sretmax := nmax ;
			END ;
		        scalar : IF subrng THEN (* SUBRANGE *)
			  BEGIN sretmin := smin ; sretmax := smax ;
			  END ELSE          (* NO SUBRANGE *)
			  BEGIN sretmin := 0 ; sretmax := fconst@.values ;
			  END ;
		        power, pointer, arrays, records, reel, files, aliastype
		        : BEGIN sretmin := 0 ; sretmax := 0 ; (* NO MEANING *)
			END ;
		      END ;                   (* CASE , WITH *)
		    insymbol ;
		  END (* TYPE ID *) ELSE
		  IF ctptr@.klass = konst (* CONST. ID. *) THEN
		    IF indexflag THEN         (* ARRAY INDEX *)
		      subrange (sretmin, sretmax, srettype, ctptr) ELSE (* NOT INDEX *)
		      scaldecl (ctptr) ELSE   (* NOT A CONSTANT *)
		    BEGIN
		      IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		      typerr (103)
		    END ;
	        END (* CTPTR # NIL *) ;
	    IF NOT err THEN err := lerr ;
	  END (* NO = 1 *) ELSE
	  IF no IN [2, 7] (* CONST,SIGN *) THEN
	    BEGIN
	      lerr := err ; err := false ;
	      IF indexflag THEN                 (* ARRAY INDEX *)
	        subrange (sretmin, sretmax, srettype, next) ELSE
	        scaldecl (next) ;
	      IF NOT err THEN err := lerr ;
	    END (* SUBRANGE *) ELSE
	    IF no = 9 (* ( *) THEN
	      BEGIN                             (* IDENTIFIER LIST *)
	        cv := -1 (* COUNTER GIVES A VALUE FOR EACH IDENTIFIER *) ;
	        lerr := err ; err := false ;
	        create_types_box (lp, blank, scalar, false) ;
	        WITH lp^ DO
		BEGIN
		  pack := packflag ;
		END ;
	        ltyp := lp ; nxtc := NIL ;      (* CHAIN OF CONST *)
	        REPEAT
		insymbol ; cv := cv + 1 ;
		IF no # 1 (* ID. *) THEN
		  BEGIN
		    error (2) ; skipt (15) ;  (* , *)
		    GOTO 2 ;                  (* BEFORE UNTIL *)
		  END ;
		srchrec (next) ;
		IF ctptr <> NIL THEN BEGIN
		    IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		    error (101)
		  END
		ELSE
		  BEGIN
		    create_konst_box (lp, aval, wordconst) ;
		    WITH lp^ DO
		      BEGIN
		        contype := ltyp ; values := cv ; succ := nxtc ;
		        next := lp ; nxtc := lp ;
		      END ;
		    next := lp ; nxtc := lp ;
		  END ;
$OPTIONS compile = trace $
		printrec (lp) ;
$OPTIONS compile = true $
		insymbol ;
2 :                                               (* HAVE WE ,? *)
	        UNTIL no # 15 ;                 (* SYMBOL READ NOT, *)
	        WITH ltyp@ DO
		BEGIN
		  fconst := next ;            (* LAST CREATED BOX *)
		  spksize := bytesneeded (scalar, cv, true) ;
		  cadrage := boundary (scalar, packflag, spksize) ;
		  size := bytesneeded (scalar, cv, packflag) ;
		END ;
	        returnsize := ltyp@.size ; cadre := sup (cadre, ltyp@.cadrage) ;
	        sretmin := 0 ; sretmax := cv ; srettype := ltyp ;
                                                  (* NOW CREATES SET BOX *)
	        create_types_box (lp, blank, power, false) ;
	        WITH lp^ DO
		BEGIN
		  ppksize := bytesneeded (power, cv, true) ;
		  setlength := cv + 1 ;
		  pack := packflag ;
		  cadrage := boundary (power, packflag, ppksize) ;
		  size := bytesneeded (power, cv, packflag) ;
		  elset := ltyp ;
		END ;
	        ltyp@.sptcstepw := lp ;
$OPTIONS compile = trace $
	        printrec (ltyp) ; printrec (lp) ;
$OPTIONS compile = true $
	        IF no = 10 (* ) *) THEN
		insymbol ELSE typerr (4) ;
	        IF NOT err THEN err := lerr ;
	      END (* IDENTIFIER LIST *) ELSE
	      typerr (99) ;
$OPTIONS compile = trace $
	IF decltrace > low THEN
	  BEGIN
	    write (mpcogout, ' @@@ FIN SIMPLETYPE @@@ WITH V.SRET MIN,MAX,TYPE;CADRE;RETURNSIZE',
	      sretmin, sretmax, ord (srettype), cadre, returnsize) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
        END ;                                     (* SIMPLETYPE *)


(* ***********************************************FIELDLIST < TYPEDECL********* *)

      PROCEDURE fieldlist (VAR maxsize : integer ; VAR varptr, nxtf : ctp) ;

(* C   ANALYZES A LIST OF FIELDS + VARIANT PART . EACH LIST IN THE CASE IS
   ANALYZED BY CALLING AGAIN FIELDLIST.
   RETURNS  MAXSIZE : MAX. SIZE OF THE RECORD
   VARPTR :  POINTER ON THE TAGFIELD BOX
   NXTF:POINTER ON THE LAST FIELD(HAS THE SAME MEANING IN INPUT) C *)
(* E   ERRORS    2 IDENTIFIER EXPECTED
   4 ')' EXPECTED
   7 ':' EXPECTED
   8 'OF' EXPECTED
   9 '(' EXPECTED
   50 ERROR IN CONSTANT
   101 IDENTIFIER DECLARED TWICE
   103 IDENTIFIER IS NOT OF APPROPRIATE CLASS
   104 IDENTIFIER NOT DECLARED
   108 File not allowed here
   110 ERROR IN THE TYPE IDENTIFIER OF A TAGFIELD
   111 INCOMPATIBLE WITH TAGFIELD TYPE
   301 CASE VARIANT OUT OF BOUND                                  E *)
        VAR
	tagflag, lerr, casefield, llast : boolean ;
	nbfield, lcad, i, lsize, it, minsize, casebytes, mxl, fieldsize : integer ;
	auxalf : alfaid ;
	lp, lpp, pp, nxt, fieldtype, nxtdeb, nxtc, tempctptr, tagtype : ctp ;
	selfield, oldnxt, ffld : ctp ;
	oldfile, oldline : integer ;
	checkcase : SET OF 0..maxset ;
	origin, max, ccount, k : integer ;
	negative : boolean ;


(* *************************************ADJUST < FIELDLIST < TYPEDECL********** *)

        PROCEDURE adjust ;

(* C   PROCEDURE USED IN ORDER TO ADJUST THE BOUNDARY OF A FIELD IN A
   PACKED PART OF A RECORD
   IF IT IS THE FIRST FIELD OF THE RECORD, NOTHING IS DONE.
   OTHERWISE : 1)IF THE LAST FIELD IS NOT A TAGFIELD:
   -MOVE IT TO THE RIGHT OF THE WORD
   -SET ITS WIDTH TO WORD SIZE IF IT IS THE ONLY FIELD OF THE
   WORD AND IF IT IS SMALLER THAN A WORD.
   2)ALWAYS INCREASE DISPL AND RESET BDISPL
   ASSERTION : AN ITEM GREATER THAN A WORD BEGINS AT A WORD BOUNDARY       C *)
	BEGIN
$OPTIONS compile = trace $
	  IF decltrace > none THEN
	    BEGIN
	      write (mpcogout, ' @@@ DEBUT ADJUST @@@ WITH DISPL,BDISPL,TAGFLAG,LASTFLD:', displ,
	        bdispl : 4, tagflag : 7, ord (lastfld)) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $
	  IF lastfld # NIL THEN                 (* NOT FIRST FIELD *)
	    BEGIN
	      IF NOT tagflag THEN WITH lastfld@ DO (* NOT A TAGFIELD *)
		IF fldtype@.form <= power THEN
		  IF fldaddr MOD bytesinword = 0 THEN (* FIRST FIELD OF A WORD *)
		    BEGIN
		      IF bytwidth < bytesinword THEN bytwidth := bytesinword
		    END ELSE
		    BEGIN
		      fldaddr := recadre (fldaddr, bytesinword) - bytwidth ;
		    END ;
	      displ := displ + bytesinword - bdispl ;
	      bdispl := 0 ;
	    END ;
$OPTIONS compile = trace $
	  IF decltrace > low THEN
	    BEGIN
	      write (mpcogout, ' @@@ FIN ADJUST @@@ WITH DISPL,BDISPL:', displ, bdispl : 4) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $
	END (* ADJUST *) ;


        BEGIN                                     (* FIELDLIST *)
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, ' @@@ DEBUT FIELDLIST @@@  WITH NXTF AT', ord (nxtf)) ; nextline ;
	  END ;
$OPTIONS compile = true $
	tagflag := true (* FIRST FIELD OF A RECORD OR OF A LIST IN THE CASE *) ;
	nxt := nxtf (* LAST FIELD FOUND IN THE SAME RECORD INITIALY NIL TYPEDECL *) ;

(* ANALYSIS OF FIXED PART NO#26 'CASE' *)
	REPEAT                                  (* LOOP ON  X,Y,Z:TYPID; *)
	  IF no # 26 (* NOT CASE *) THEN
	    BEGIN
	      IF no = 1 (* ID *) THEN
	        BEGIN
		nbfield := 0 ; (* NB OF ID OF THE SAME TYPE *) ; nxtdeb := NIL ; (* DEFAULT *)
		REPEAT                        (* LOOP ON  X,Y,... *)
		  srchrec (nxt) ;
		  IF ctptr # NIL THEN         (* TWO IDENTICAL FIELDS *)
		    BEGIN
		      IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		      error (101)
		    END
		  ELSE
		    BEGIN                     (* NEW ID. AT THIS LEVEL *)
		      create_field_box (lp, aval) ;
		      WITH lp^ DO
		        BEGIN
			nxtel := nxt ;
		        END ;
		      IF nbfield # 0 THEN nxt@.fldtype := lp (* FORWARD LINKAGE *) ELSE
		        nxtdeb := lp (* POINTS ON THE FIRST FIELD OF THE LIST *) ;
		      nxt := lp ; nbfield := nbfield + 1 ;
		    END ;                     (* NEW ID. *)
		  insymbol ;
		  IF no = 15 (* , *) THEN
		    BEGIN
		      insymbol ;
		      IF no # 1 (* ID *) THEN
		        BEGIN
			error (2) ; skipt (46) ;
		        END ;
		    END ;
		UNTIL no # 1 ;
		nxt@.fldtype := NIL ;         (* ENDS FORWARD LINKAGE *)

		check_id := old_check_id ;

		IF no # 19 (* : *) THEN error (7) ELSE
		  insymbol ;
		lcad := cadre ; cadre := 0 ;
		lerr := err ; err := false ;
		llast := structispack ;
		typedecl (fieldsize, fieldtype) ;

		check_id := false ;

		structispack := llast ;
		IF (fieldtype = NIL) OR err THEN
		  err := true ELSE
		  BEGIN
		    IF cadre = 0 THEN cadre := bytesinword ; (* Security *)
		    IF fieldtype@.form > records THEN
		      BEGIN error (108) ; err := true ;
		      END ELSE
		      BEGIN
		        IF NOT structispack THEN
			BEGIN               (* UNPACKED *)
			  IF cadre < bytesinword THEN cadre := bytesinword ;
			  displ := recadre (displ, cadre) ;
			  IF nbfield > 1 THEN fieldsize := recadre (fieldsize, cadre) ;
			  pp := nxtdeb ;    (* FIRST FIELD OF THE LIST *)
			  FOR i := 1 TO nbfield DO
			    BEGIN
			      lp := pp ; lp@.fldaddr := displ ;
			      lp@.bytwidth := fieldsize ;
			      pp := lp@.fldtype ; (* FORWARD LINKAGE *) ;
			      lp@.fldtype := fieldtype ;
			      displ := displ + fieldsize ;
$OPTIONS compile = trace $
			      printrec (lp) ;
$OPTIONS compile = true $
			    END ;
			END (* UNPACKED *) ELSE
			BEGIN               (* PACKED *)
			  IF fieldtype@.form = pointer THEN cadre := bytesinword ;
			  IF fieldtype@.form >= power THEN
			    lsize := fieldtype@.size ELSE
			    lsize := packedsize (fieldtype) ;
			  pp := nxtdeb ;    (* FIRST FIELD OF THE LIST *)
			  IF fieldtype@.form <= power THEN (* NEITHER ARRAY NOR RECORD *)
			    FOR i := 1 TO nbfield DO
			      BEGIN         (* A FIELD > 1 WORD MUST BEGIN *)
                                                  (* AT A WORD BOUNDARY *)
			        IF ((bdispl + lsize) > bytesinword) AND (bdispl # 0)
			        THEN adjust ;
			        WITH pp@ DO
				BEGIN
				  bytwidth := lsize ;
				  displ := recadre (displ, cadre) ;
				  fldaddr := displ ;
				END ;
			        displ := displ + lsize ; bdispl := displ MOD bytesinword ;
			        lp := pp ; pp := lp@.fldtype ; lp@.fldtype := fieldtype ;
			        tagflag := false ; lastfld := lp ;
$OPTIONS compile = trace $
			        printrec (lp) ;
$OPTIONS compile = true $
			      END ELSE      (* ARRAYS AND RECORDS MUST *)
                                                  (* START AT WORD LIMIT *)
			    FOR i := 1 TO nbfield DO
			      BEGIN
			        IF bdispl # 0 THEN adjust ;
			        WITH pp@ DO
				BEGIN
				  displ := recadre (displ, cadre) ;
				  fldaddr := displ ; bytwidth := lsize ;
				END ;
			        bdispl := lsize MOD bytesinword ; displ := displ + lsize ;
			        lp := pp ; pp := lp@.fldtype ; lp@.fldtype := fieldtype ;
			        tagflag := false ; lastfld := lp ;
$OPTIONS compile = trace $
			        printrec (lp) ;
$OPTIONS compile = true $
			      END ;         (* ARRAYS AND RECORDS *)
			END ;               (* PACKED *)
		      END ;                   (* FORM <= RECORD AND *)
                                                  (* NO PREVIOUS ERROR *)
		  END ;
		IF err THEN
		  BEGIN                       (* SET FLDTYPE TO NIL *)
		    pp := nxt ;
		    FOR i := nbfield DOWNTO 1 DO
		      BEGIN
		        pp@.fldtype := NIL ;
$OPTIONS compile = trace $
		        printrec (pp) ;
$OPTIONS compile = true $
		        pp := pp@.nxtel ;
		      END ;
		  END ELSE err := lerr ;
		cadre := sup (cadre, lcad) ;
	        END ;                           (* NO = 1 *)
	      IF no = 16 THEN
	        insymbol ELSE
	        BEGIN
		IF no = 1 THEN error (14) ;
	        END ;
	    END ;                               (* NO# 26 'CASE' *)
	UNTIL NOT (no IN [1, 16]) ;             (* ; ID *)
	maxsize := displ ; varptr := NIL ;
	IF no = 26 (* CASE *) THEN
	  BEGIN
	    insymbol ;
	    IF no # 1 (* ID *) THEN error (2) ELSE
	      BEGIN
	        srchrec (nxt) ; tempctptr := ctptr ; (* IT MAY BE A FIELD OR *)
	        srchrec (next) ;
	        IF ctptr = NIL THEN search ;    (* A TYPE IDENTIFIER *)
	        auxalf := aval ;
	        oldfile := symbolfile ; oldline := symbolline ;
	        insymbol ;
	        IF no = 19 (* : *) THEN         (* SELECTOR HAS A FIELD *)
		BEGIN
		  IF tempctptr # NIL THEN error (101) (* ALLREADY USED *) ELSE
		    BEGIN
		      create_field_box (lp, auxalf) ;
		      WITH lp^ DO
		        BEGIN
			nxtel := nxt ; deffile := oldfile ; defline := oldline ;
		        END ;
		      nxt := lp ;
		      selfield := lp ;
		    END ;                     (* TAG FIELD IS NEW FIELD *)
		  insymbol ;                  (* LOOK AT THE TYPE IDENTIFIER *)
		  IF no # 1 (* ID *) THEN error (2) ELSE
		    BEGIN
		      srchrec (next) ;
		      IF ctptr = NIL THEN search ;
		    END ;
		  casefield := true ;
		  oldfile := symbolfile ; oldline := symbolline ;
		  insymbol ;
		END ELSE                      (* SELECTOR HAS NO FIELD *)
		casefield := false ;
	        IF ctptr = NIL THEN error (104) (* UNKNOWN TYPE *) ELSE
		BEGIN
		  IF symbolmap THEN nameisref (ctptr, oldfile, oldline) ;
		  IF ctptr@.klass # types THEN error (110) ELSE
		    BEGIN
		      IF ctptr@.form = aliastype THEN ctptr := ctptr@.realtype ;
		      origin := 0 ; max := 0 ;
		      WITH ctptr^ DO
		        IF form = numeric THEN
			IF ctptr = intptr THEN error (106) ELSE
			  BEGIN
			    origin := nmin ; max := nmax - nmin ;
			  END
		        ELSE
			IF form = scalar THEN
			  IF subrng THEN
			    BEGIN
			      origin := smin ; max := smax - smin ;
			    END ELSE
			    BEGIN
			      origin := 0 ; max := fconst^.values ;
			    END
			ELSE
			  error (110) ;

		      ccount := -1 ; checkcase := [] ;
		      IF max > maxset THEN
		        BEGIN
			error (32) ; max := maxset ;
		        END ;
		    END ;
		END ;
	        IF no # 27 (* OF *) THEN error (8) ;
	        tagtype := ctptr ;
	        IF casefield THEN
		BEGIN                         (* CASE ID: TYPE OF *)
		  IF tagtype # NIL THEN
		    BEGIN
		      IF NOT structispack THEN
		        BEGIN
			displ := recadre (displ, tagtype@.cadrage) ;
			lsize := tagtype@.size ;
		        END ELSE
		        BEGIN
			lsize := packedsize (tagtype) ;
			IF (bdispl # 0) AND (bdispl + lsize > bytesinword) THEN adjust ;
			tagflag := false ; lastfld := lp ;
		        END ;                 (* PACKED *)
		      WITH lp@ DO
		        BEGIN
			fldaddr := displ ; bytwidth := lsize ; fldtype := tagtype ;
		        END ;
$OPTIONS compile = trace $
		      printrec (lp) ;
$OPTIONS compile = true $
		      displ := displ + lsize ; bdispl := displ MOD bytesinword ;
		    END ;                     (* TAGTYPE # NIL *)
		END ;                         (* TAG IDENTIFIER *)
	        minsize := displ ; maxsize := minsize ; nxtc := NIL ;
	        casebytes := bdispl ;
	        insymbol ;
	        REPEAT                          (* LOOP ON CASE 'LABELS' *)
		i := 0 ;                      (* COUNT THE CONSTANTS FOR ONE CASE *)
		REPEAT                        (* SAME CASE *)
		  IF (no = 7) AND (cl = 2) THEN
		    BEGIN
		      negative := true ;
		      insymbol
		    END
		  ELSE negative := false ;
		  IF (no > 2) OR ((no = 2) AND (NOT (cl IN [1, 4]))) THEN
		    BEGIN                     (* ILLEGAL CASE LABEL *)
		      error (50) ; skipt (46) ;
		    END ELSE
		    BEGIN
		      IF tagtype # NIL THEN
		        IF no = 1 (* ID *) THEN
			BEGIN
			  srchrec (next) ; IF ctptr = NIL THEN search ;
			  IF ctptr = NIL THEN error (104) ELSE
			    BEGIN
			      IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
			      WITH ctptr@ DO
			        IF klass # konst THEN error (103) ELSE
				BEGIN
				  IF ((tagtype@.form = scalar) AND (contype # tagtype) AND
				    (tagtype@.typset # contype)) OR
				    ((tagtype@.form = numeric) AND (contype # intptr)) THEN
				    BEGIN
				      error (111) ; it := 0 ;
				    END ELSE
				    IF (tagtype^.form = scalar) AND negative THEN
				      BEGIN
				        error (50) ; it := 0
				      END
				    ELSE
				      BEGIN
				        it := values ; IF negative THEN it := -it ;
				        checkminmax (it, tagtype, 301) ;
				      END ;
				END       (* ELSE,WITH *)
			    END ;
			END (* NO=1 *) ELSE (* EXPLICIT CONST *)
			BEGIN
			  IF negative THEN ival := -ival ;
			  it := ival ;
			  IF ((cl = 1) AND (tagtype@.form # numeric)) OR ((cl = 4) AND
			    (tagtype # charptr) AND (tagtype@.typset # charptr))
			  THEN error (111) ;
			  checkminmax (it, tagtype, 301) ;
			END (* NUMERIC *) ELSE (* TAGTYPE = NIL *)
		        it := 0 ;
		      k := it - origin ;
		      IF (k >= 0) AND (k <= max) THEN
		        IF k IN checkcase THEN error (310) ELSE
			BEGIN
			  checkcase := checkcase + [k] ;
			  ccount := ccount + 1 ;
			END ELSE
		        error (312) ;
		      create_tagfield_box (lp, blank, true) ;
		      WITH lp^ DO
		        BEGIN
			nxtel := nxtc ; caseval := it ;
		        END ;
		      nxtc := lp ; i := i + 1 ;
		      insymbol ;
		    END ;                     (* CONSTANT *)
		  IF no <> 19 THEN
		    IF no = 15 THEN insymbol
		    ELSE error (317) ;
		UNTIL (no > 2) AND (no <> 7) ;
		IF no # 19 (* : *) THEN error (7) ELSE insymbol ;
		oldnxt := nxt ;
		IF no = 9 (* ( *) THEN
		  BEGIN                       (* START OF FIELDS LIST *)
		    displ := minsize ; bdispl := casebytes ; insymbol ;
		    fieldlist (mxl, pp, nxt) ;
		    IF no = 10 (* ) *) THEN insymbol ELSE error (4) ;
		  END (* NO=9 *) ELSE
		  BEGIN
		    error (9) ; pp := NIL ; mxl := minsize ; skipt (46) ;
		  END ;
		lpp := nxtc ;
		IF nxt = oldnxt THEN ffld := NIL
		ELSE
		  BEGIN
		    ffld := nxt ;
		    WHILE ffld^.nxtel <> oldnxt DO
		      ffld := ffld^.nxtel ;
		  END ;
		FOR i := i DOWNTO 1 DO        (* END OF THE FILLING OF *)
                                                  (* TAG VALUES RECORDS *)
		  IF lpp # NIL THEN WITH lpp@ DO
		      BEGIN
		        casesize := mxl ; variants := pp ;
		        firstfield := ffld ;
$OPTIONS compile = trace $
		        printrec (lpp) ;
$OPTIONS compile = true $
		        lpp := nxtel ;
		      END ;                   (* THEN,WITH *)
		maxsize := sup (mxl, maxsize) ; (* MAX. SIZE OF THE RECORD *)
		IF no = 16 (* ; *) THEN insymbol ;
	        UNTIL no > 2 ;                  (* LOOP ON CASE 'LABELS' *)
	        IF ccount <> max THEN
		IF envstandard <> stdextend THEN error (311) ELSE warning (313) ;
	        create_tagfield_box (lp, blank, false) ;
	        WITH lp^ DO
		BEGIN
		  casesize := maxsize ; variants := nxtc ; casetype := tagtype ;
		  IF casefield THEN selectorfield := selfield
		END ;
$OPTIONS compile = trace $
	        printrec (lp) ;
$OPTIONS compile = true $
	        varptr := lp ;
	      END ;                             (* NO=1 *)
	  END ;                                 (* NO=26 'CASE' *)
	nxtf := nxt ;
$OPTIONS compile = trace $
	IF decltrace > low THEN
	  BEGIN
	    IF decltrace = high THEN
	      BEGIN
	        write (mpcogout, ' DISPL,BDISPL,CADRE,ERR ', displ, bdispl, cadre, err) ;
	        nextline ;
	      END ;
	    write (mpcogout, ' @@@ FIN FIELDLIST @@@ WITH  V.MAXSIZE, V.VARPTR, V.NXTF', maxsize,
	      ord (varptr), ord (nxtf)) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
        END (* FIELDLIST *) ;


      BEGIN                                       (* TYPEDECL *)
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout,
	    ' @@@ DEBUT TYPEDECL @@@ WITH CADRE,ERR,NO,CL,STRUCTISPACK', cadre : 4,
	    err, no : 4, cl : 4, structispack) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        packflag := structispack ; structispack := false ;
        indexflag := false (* USED IN SIMPLETYPE FOR ARRAY'S INDEX *) ;
        IF no = 42 (* PACKED *) THEN
	BEGIN
	  insymbol ;
	  IF no IN [1, 2, 7, 9, 18] THEN        (* ID,CONST,SIGN,(,@ *)
	    error (98) ELSE
	    BEGIN structispack := true ; packflag := true ;
	    END ;
	END ;
        IF no IN [1, 2, 7, 9] THEN                (* ID,CONST,SIGN,( *)
	simpletype (li, lh, returntype) (* RETURNSIZE IS ASSIGNED IN PROC *) ELSE
	IF no = 38 (* STRUCTURED TYPES *) THEN
	  BEGIN
	    CASE cl OF
	      1 : BEGIN                         (* ARRAYS *)
		insymbol ;
		IF no # 11 (* [ *) THEN
		  BEGIN
		    error (11) ;
		    IF NOT (no IN [1, 2, 7]) THEN (* NOT  SUBRANGE BEGINNING *)
		      insymbol ;
		  END ;
		indexflag := true ;           (* FOR EACH DIMENSION *)
		nxta := NIL (* USED TO CHAIN SUBARRAYS VIA AELTYPE  *) ;
		REPEAT                        (* LOOP ON DIMENSIONS *)
                                                  (* ONE BOX 'ARRAYS' FOR EACH DIM.  *)
		  create_types_box (lp, blank, arrays, false) ;
		  WITH lp^ DO
		    BEGIN
		      pack := structispack ;
		      aeltype := nxta ;
                                                  (* Temporary reverse linkage *)
		    END ;
		  nxta := lp ;
		  insymbol ;
		  lerr := err ; err := false ;
		  simpletype (li, lh, lt) ;   (* DIMENSION 'S INDEX = SUBRANGE *)
                                                  (* CHECK FOR TYPE OF INDEX  MADE  *)
                                                  (* EITHER IN SIMPLETYPE, *)
                                                  (* EITHER IN SUBRANGE    *)
		  IF err THEN
		    BEGIN
		      skipt (15) ; (* FIND , *) li := 0 ; lh := 0 ; lt := NIL ;
		    END ELSE
		    err := lerr ;
		  WITH nxta@ DO
		    BEGIN
		      lo := li ; hi := lh ; inxtype := lt ;
		    END ;
		UNTIL no # 15 (* , *) ;
		indexflag := false ;
		IF no # 12 (* ] *) THEN
		  BEGIN
		    error (12) ; skipt (27) ; (* ==> OF *)
		    IF terrcl [no] = begsy THEN GOTO 11 ; (* TYPE OF ELEMENT *)
		    IF no = 27 (* OF *) THEN
		      BEGIN
		        insymbol ; GOTO 11 ;
		      END ;
		    IF no # 12 (* ] *) THEN
		      BEGIN
		        returntype := NIL ; returnsize := 0 ; GOTO 19 ; (* END OF ARRAY TYPE *)
		      END ;
		  END (* NO#12 *) ;
		insymbol ;
		IF no = 27 (* OF *) THEN
		  insymbol ELSE error (8) ;
11 :                                              (* ANALYSIS OF ELEMENT TYPE  *)
		lcad := cadre ; cadre := 0 ; lerr := err ; err := false ;
		typedecl (elsize, eltyp) ;
		IF eltyp # NIL THEN
		  IF eltyp@.form > records THEN
		    BEGIN error (108) ; eltyp := NIL ; err := true ;
		    END ELSE
		    IF (cadre = 0) OR err THEN (* PREVIOUS ERROR(S) *)
		      BEGIN
		        eltyp := NIL ; err := true ;
		      END ELSE
		      BEGIN
		        REPEAT
			WITH nxta@ DO
			  BEGIN
			    IF NOT pack THEN cadre := sup (cadre, bytesinword) ;
			    elsize := recadre (elsize, cadre) ;
			    subsize := elsize ; opt2 := poweroftwo (elsize) ;
			    bigsize := hi ; bigsize := bigsize - lo + 1 ;
			    bigsize := bigsize * elsize ;
			    IF bigsize >= twoto18 * bytesinword THEN
			      BEGIN error (112) ; bigsize := 1 ;
			        hi := 1 ; lo := 1 ; (* PROTECT *)
			      END ;
			    elsize := round (bigsize) ;
			    size := elsize ; cadrage := cadre ;
			    lp := aeltype ; aeltype := eltyp ; (* REVERSE  LINKAGE *)
			  END ;
$OPTIONS compile = trace $
			printrec (nxta) ;
$OPTIONS compile = true $
			eltyp := nxta ; nxta := lp ;
		        UNTIL nxta = NIL ;
		        returnsize := elsize ; (* SIZE FOR THE TOTAL ARRAY *)
		        err := lerr ;
		      END ;
		returntype := eltyp ;         (* MAY BE NIL *)
		cadre := sup (lcad, cadre) ;
19 :	        END (* ARRAYS NO=38 CL=1 *) ;
	      2 : BEGIN                         (* RECORDS *)
		create_types_box (lp, blank, records, false) ;
		WITH lp^ DO
		  BEGIN
		    pack := structispack ;
		  END ;
		rtyp := lp ;

		old_check_id := check_id ; check_id := false ;

		insymbol ;
		nxtf := NIL ;
		displ := 0 ; bdispl := 0 ;    (* DISP. IN RECORD AND IN WORD (IN BYTES) *)
		lastfld := NIL (* TO INHIBIT USE OF ADJUST FUNCTION WITH FIRST FIELD *) ;
		lerr := err ; lcad := cadre ;
		err := false ; cadre := 0 ;
		fieldlist (returnsize, recvpt, nxtf) ; (* ANALYZIS OF FIELDS' LIST *)

		check_id := old_check_id ;

		IF no # 22 (* END *) THEN error (13) ;
		IF err THEN
		  typerr (10) ELSE
		  BEGIN                       (* NO PREVIOUS ERROR *)
		    err := lerr ;
		    returntype := rtyp ;
		    cadre := sup (lcad, cadre) ;
		    IF nxtf # NIL THEN        (* REVERSE FIELDS'POINTERS *)
                                                  (* TO HAVE REAL ORDER *)
		      BEGIN
		        oldnxtf := nxtf ; pp := nxtf@.nxtel ;
		        WHILE pp # NIL DO
			BEGIN
			  lp := pp ; pp := lp@.nxtel ; lp@.nxtel := nxtf ; nxtf := lp ;
			END ;
		        oldnxtf@.nxtel := NIL ;
		      END ;
		    WITH rtyp@ DO
		      BEGIN
		        size := returnsize ; fstfld := nxtf ; recvar := recvpt ;
		        cadrage := cadre ;
		      END ;
$OPTIONS compile = trace $
		    printrec (rtyp) ;
$OPTIONS compile = true $
		  END ;                       (* NO ERROR *)
		IF no = 22 (* END *) THEN insymbol ;
	        END (* RECORDS NO=38 CL=2 *) ;
	      3 : BEGIN                         (* FILES *)
		create_types_box (lp, blank, files, false) ;
		WITH lp^ DO
		  BEGIN
		    pack := structispack ;
		    cadrage := boundary (files, false, 0) ;
		  END ;
		insymbol ;
		IF no = 27 (* OF *) THEN
		  insymbol ELSE error (8) ;
		lcad := cadre ; cadre := 0 ;
		lerr := err ; err := false ;
		typedecl (returnsize, lfpt) ;
		IF (lfpt = NIL) OR (cadre = 0) OR err THEN
		  typerr (10) ELSE
		  IF lfpt@.form > records THEN
		    BEGIN
		      error (108) ; returntype := NIL ; err := true ;
		    END ELSE
		    BEGIN
		      locpt := lp ;           (* BOX FILES *)
		      cadre := locpt@.cadrage ; err := lerr ;
		      lp@.feltype := lfpt ;
		      lp@.size := fsbpointersize ;
$OPTIONS compile = trace $
		      printrec (lp) ;
$OPTIONS compile = true $
		      returntype := lp ;
		    END ;
		cadre := sup (cadre, lcad) ;
	        END ;                           (* FILES NO=38 CL=3 *)
	      4 : BEGIN                         (* SET *)
		insymbol ;
		IF no = 27 (* OF *) THEN
		  insymbol ELSE error (8) ;
		lerr := err ; err := false ; lcad := cadre ; cadre := 0 ;
                                                  (* SET IS PACKED ONLY IF *)
                                                  (* WE HAVE "PACKED SET" *)
		packflag := structispack ;
		simpletype (sl, sh, spt) ;    (* CHECK MADE HERE FOR TYPE *)
		IF err OR (spt = NIL) OR (cadre = 0) THEN
		  typerr (169) ELSE
		  BEGIN
		    err := lerr ;
		    IF NOT (spt@.form IN [numeric, scalar]) THEN typerr (115) ELSE
		      IF (sl < 0) OR (sh > maxset) THEN
		        typerr (305) ELSE
		        BEGIN
			create_types_box (lp, blank, power, false) ;
			WITH lp^ DO
			  BEGIN
			    ppksize := bytesneeded (power, sh, true) ;
			    setlength := sh + 1 ;
			    pack := structispack ;
			    cadrage := boundary (power, packflag, ppksize) ;
			    size := bytesneeded (power, sh, packflag) ;
			    elset := spt ;
			  END ;
$OPTIONS compile = trace $
			printrec (lp) ;
$OPTIONS compile = true $
			returnsize := lp@.size ;
			returntype := lp ;
			cadre := sup (lcad, lp@.cadrage) ;
		        END ;
		  END ;
	        END ;                           (* POWER NO=38 CL=4 *)
	    END                                 (* CASE CL *)
	  END (* NO = 38 *) ELSE
	  IF no = 18 (* @ *) THEN
	    BEGIN                               (* POINTER *)
	      check_id_saved := check_id ; check_id := false ;
	      insymbol ;
	      check_id := check_id_saved ;
	      IF no # 1 (* ID *) THEN
	        typerr (2) ELSE
	        BEGIN
		returnsize := bytesneeded (pointer, 0, packflag) ;
		create_types_box (lp, blank, pointer, false) ;
		WITH lp^ DO
		  BEGIN
		    size := returnsize ;
		    pack := packflag ;
		    ptpksize := bytesneeded (pointer, 0, true) ;
		    cadrage := boundary (pointer, packflag, ptpksize) ;
		  END ;
		srchrec (next) ; IF ctptr = NIL THEN search ;
		IF ctptr # NIL (* ID. FOUND *) THEN BEGIN
		    IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		    IF (ctptr@.klass = vars) AND (ctptr@.vtype = NIL) THEN
		      ctptr := NIL ;          (* ERROR SECURITY; *)
		    IF ctptr^.klass = types THEN
		      IF ctptr^.tlevel < level THEN
		        IF pendingtypeallowed THEN
			ctptr := NIL ;      (* IN CASE OF DEFINITION ALTER AT THIS LEVEL *)
                                                  (* ERROR DETECTED AT END OF VARDECL *)
		  END ;
		IF ctptr = NIL THEN
		  BEGIN                       (* UNDEC TYPE *)
		    IF NOT pendingtypeallowed THEN
		      BEGIN
		        error (62) ; returntype := NIL ; returnsize := 0 ;
		      END ELSE
		      IF ptx > ptlimit THEN
		        BEGIN
			error (268) ; returntype := NIL ; returnsize := 0 ;
		        END ELSE
		        WITH ptlist [ptx], lp@ DO
			BEGIN
			  hname := aval ; pptr := lp ;
			  domain := lp ; eltype := lp ;
			  returntype := lp ; ptx := ptx + 1 ; (* POINTS  NEXT FREE ENTRY *)
			  rfil := symbolfile ; rlin := symbolline ;
			END ;
		    insymbol ;
		  END (* UNDECLAR  *) ELSE
		  BEGIN returntype := lp ; insymbol ;
		    IF ctptr@.klass = types THEN
		      IF ctptr@.form = aliastype THEN ctptr := ctptr@.realtype ;
		    WITH ctptr@ DO
		      BEGIN
		        IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		        IF (klass = types) AND (form <= records) THEN
			BEGIN
			  lp@.domain := lp ;
			  lp@.eltype := ctptr ; (* DOMAIN FLAG NEW ON HEAP *)
			END ELSE
			error (96) ;
		      END ;
		  END ;
$OPTIONS compile = trace $
		printrec (lp) ;
$OPTIONS compile = true $
		cadre := sup (cadre, lp@.cadrage) ;
	        END                             (* NOT ERROR *)
	    END (* POINTER *) ELSE
	    typerr (10) ;
$OPTIONS compile = trace $
        IF decltrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN TYPEDECL @@@ WITH V.RETSIZE,TYPE ;CADRE,ERR,STRUCTISPACK ',
	    returnsize, ord (returntype), cadre, err, structispack) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* TYPEDECL *) ;


$OPTIONS page $

(* *******************************************************************BODY***** *)

    PROCEDURE body (surrptr, firstentry : ctp) ;

(* C  THIS PROCEDURE  COMPILES  A PASCAL  'BLOCK'.
   LABEL    DECLARATION
   CONST     "     "                           .....   INCONST
   TYPE      "     "                           .....   TYPEDECL
   VAR       "     "                           .....   VARDECL
   VALUE                                                VALUEDECL
   PROCEDURE (FUNCTION)
   INSTRUCTION                                 .....   ENTERBODY
   COMPSTAT
   LEAVEBODY.
   ALL NEEDED INFORMATIONS ARE  MEMORIZED  IN BOXES OF SEVERAL KLASS
   TYPES,KONST,VARS, PROC   AND SOON
   THESE BOXES ARE BUILT  IN  SEVERAL  PROCEDURES,ALL CALLED  BY 'BODY'
   ARE USED   IN  GENERATION PART
   PARAMETER'S   MEANINGS
   SURRPTR  POINTS  ON THE  BOX  'PROC'  WHICH   DECLARATION PART IS
   ACTUALLY  COMPILED
   NIL    FOR THE MAIN
   FIRSTENTRY    POINTS   A BOX 'DUMMYCLASS'  WHICH  IDENTIFIES  THE
   BEGINNING  OF  ALL BOXES   LIVING   THE SAME   TIME
   AS  THE  PROCEDURE   COMPILED  (USE WITH RESET )
   C *)
(* E  ERRORS DETECTED
   HEAPERROR
   2  IDENTIFIER EXPECTED
   4  ')' EXPECTED
   7  ':' EXPECTED
   14  ';' EXPECTED
   15  INTEGER EXPECTED
   16  '=' EXPECTED
   17  'BEGIN' EXPECTED
   20  ',' EXPECTED
   65  VALUE PART ONLY FOR GLOBALS
   87  PROC  MUST BE DEFINED IN EXTERNAL LIST
   88  INVALID   DIRECTIVE
   101  IDENTIFIER DECLARED TWICE
   103  IDENTIFIER NOT OF APPROPRIATE CLASS
   104  IDENTIFIER NOT DECLARED
   108 File not allowed here
   116 Forward redefinition conflict with declaration
   117  UNDEF  FORWARD   PROCEDURE
   119  REPETITION OF PARAMETER LIST  NOT ALLOWED (FORWARD)
   120  FUNCTION TYPE MUST BE REAL,NUMERIC,SCALAR OR  POINTER
   123  RESULT  TYPE IDENTIFIER EXPECTED
   166  MULTIDECLARED LABELS
   214  SIZE ALLOWED FOR GLOBALS EXCEEDED
   251  TOO MANY  NESTED PROC  AND (OR)  FUNCTIONS
   267  TOO MANY LABELS  (MAXLABS)
   306  LABEL MUST HAVE AT MOST 4 DIGITS
   E *)
      LABEL
        1 ;                                       (* BODY BEGINS. USED FOR ERR. RECOVERY *)
      VAR
        lca, lic : integer ;                      (* INITIALIZED(AND USED)  *)
                                                  (* IN  ENTERBODY(LEAVEBODY) *)
        saved_level, it : integer ;
        lprockind : idkinds ;
        typofproc : idprocdef ;
        lp, procptr, lfirstentry : ctp ;
        fstix, lno, oldlev, oldlc, nestproc, locreturncode : integer ;
        locerr : boolean ;
        lextpt : ptexternalitem ;
        locsegname, locentryname : alfaid ;
        workextp : ptexternalitem ;

(* ***********************************************FINDSEMICOLON < BODY********* *)

      PROCEDURE findsemicolon ;

(* C  USED TO VERIFY IF THE READ SYMBOL IS ;AND TO PERFORM THE NEXT INSYMBOL
   IF ; IS NOT FOUND  THEN
   SKIP  UNTIL  ;  USING ERRCL
   IF  ; NOT FOUND  THEN  GOTO EXIT 1  IN BODY (LABEL PART)
   C *)
(* E  ERROR(S)  DETECTED
   14 : ';' EXPECTED
   E *)
        BEGIN                                     (* FINDSEMICOLON *)
	IF no # 16 (* ; *) THEN
	  BEGIN
	    error (14) ; skip (16) ;
	    IF no # 16 (* ; *) THEN GOTO 1 ;    (* EXIT AT LABEL PART IN BODY *)
	  END ;
	insymbol ;
        END (* FINDSEMICOLON *) ;


(* ***********************************************ENTERBODY < BODY************* *)

      PROCEDURE enterbody ;

(* C  CALLED  AT BEGINNING OF  THE STATEMENT  PART OF A PROC,(PROGRAM)
   . GENERATES   CODE  TO  OPEN  FILES
   . GENERATES    PROCEDURE  (PROGRAM)    PROLOG
   . INITIALIZES   LOCAL  TABLES  FOR THIS LEVEL
   . INITIALIZE    PHYSICAL  POINTER ON CLASSES
   .  INIT  LIC, LCA   (DEFINED IN BODY)
   LIC HAS THE VALUE OF THE INITIAL IC
   LCA IS OBTAINED EITHER BY GENPROLOG OR BY GENPROCENTRY AND GIVES THE
   ADDRESS OF AN UNCOMPLETED WORD OF PUSH WHICH WILL BE FILLED IN
   LEAVEBODY
   C *)
        VAR
	it : integer ;
        BEGIN
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, ' @@@ DEBUT ENTERBODY @@@  PROCPTR,IC,LC ', ord (procptr), ic, lc) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
                                                  (* PROGRAM OR PROCEDURE ENTRY CODE *)
	environt := code ;
	lic := ic ;                             (* DEPL OF FIRST INSTR. OF THIS *)
                                                  (* PROCEDURE,LIC DEFINED IN BODY *)
	cb := 0 ;
                                                  (*  BY LEVEL  INITIALIZE  *)
                                                  (* CONSTANT'S LISTS *)
	currwcstpt := NIL ;                     (* WORDS *)
	currlcstpt := NIL ;                     (* DOUBLE-WORDS *)
	currllcstpt := NIL ;                    (* EIGHT-WORDS (SETS) *)
	currrcstpt := NIL ;                     (* REAL *)
	nextalf := NIL ;
	IF mapswitch THEN BEGIN
	    WITH currentnode^ DO
	      BEGIN
	        symbolindex := hdrind ;
	        symbolfile := hdrfil ;
	        symbolline := hdrlin ;
	      END ;
	    statement_begins (false) ;
	  END ;
	IF level = 0 THEN                       (* MAIN PROGRAM *)
	  BEGIN
	    genprolog (lca, lic) ;
	    lc := pascdebstacklocal ;
	  END ELSE
                                                  (* PASCAL PROCEDURE ENTRY CODE *)
	  genprcentry (lca, surrptr, lic) ;
	IF mapswitch THEN BEGIN
	    statement_ends (5) ;                (* "begin" *)
	    statement_begins (true) ;
	  END ;
                                                  (* NOW GENERATES LOCAL FILES FSB *)
	FOR it := filev [level] TO filtop DO initiozone (filpts [it]) ;
                                                  (* RECADRE  LC AND INITIALIZES  TMAX AND LCSAVE *)
	lc := recadre (lc, bytesindword) ;
	lcsave := lc ; tmax := lc ;
	IF mapswitch THEN statement_ends (5) ;  (* "begin" *)
$OPTIONS compile = trace $
	IF decltrace > low THEN
	  BEGIN
	    write (mpcogout, ' @@@ FIN ENTERBODY @@@ WITH IC AT', ic) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END ;                                     (* ENTERBODY *)


(* *****************************************LEAVEBODY < BODY******************** *)

      PROCEDURE leavebody ;

(* C FUNCTIONS OF THIS PROCEDURE
   . CLOSE  FILES
   . CHECK FOR UNDEFINED LABELS AND EXIT LABELS
   . FREES  LOCAL TABLES
   . EXIT CODE  FOR A FUNCTION
   . GENERATES  IN LINES CSTES
   . GENERATES  EXIT CODE  FOR PROC OR PROGRAM.
   C *)
(* E ERRORS DETECTED
   155: FUNCTION IDENTIFIER HAS NOT BEEN ASSIGNED
   168: UNDEFINED LABEL ;  SEE MESSAGE
   227 : SOME LABELS DECLARED IN THIS PROCEDURE ARE ILLEGALLY REFERENCED.
   E *)
        VAR
	it, endcode : integer ;
	lerr : boolean ;
	locreturncode : integer ;
	lp, lpaux : ctp ;
	locintext : integer ;
	trans : RECORD
	  CASE boolean OF
	  true : (name : alfaid) ;
	  false : (half_wd : PACKED ARRAY [1..4] OF shrtint) ;
	END ;
	message : PACKED ARRAY [1..132] OF char ;
	iter, index : integer ;
	ref_err : boolean ;
	refbox : refptr ;
        BEGIN                                     (* LEAVEBODY *)
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, ' @@@ DEBUT LEAVEBODY @@@ WITH  LIC,LCA, IC', lic, lca, ic) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
	IF mapswitch THEN statement_begins (true) ;
	IF level = 0 THEN
	  BEGIN
	    genstand (pr0, returnzeroplace, itsp3, tn) ;
	    IF linktoend THEN
	      IF errtotal = 0 THEN
	        BEGIN
		genentrypoint (ic, linktoendplace, 4 (* EXIT LABEL *),
		  blank, blank,
		  functionflag, entrylength,
		  locreturncode) ;
		IF locreturncode <> 0 THEN
		  error (510) ;
		IF getpr4afterstop THEN
		  genstand (pr6, pr4depw, iepp4, tny) ;
		IF mapswitch THEN BEGIN
		    statement_ends (1) ;
		    statement_begins (true) ;
		  END ;
	        END ;
	  END ;
	(*  CLOSE  FILES   AND FREES   FILEV *) (* FILES            *)
	FOR it := filev [level] TO filtop DO
	  closefile (filpts [it]) ;
	(* * CHECK FOR UNDEFINED LABELS, FREES LABTAB *) (* LABELS           *)
	ref_err := false ;
	lerr := false ;
	FOR it := fstix TO clabix DO
	  WITH labtab [it] DO
	    IF labdef = 0 THEN
	      BEGIN
	        nextline ;
	        writeln (mpcogerr, ' ***** UNDEFINED LABEL :', labval : 5) ;
	        write (mpcogout, ' ***** UNDEFINED LABEL :', labval : 5) ; nextline ;
	        lerr := true ;
	      END ELSE
	      BEGIN
	        IF labexit # 0 THEN
		exitlabel (labexit, lic + labdef) ;
	        WITH labbox^ DO
		BEGIN
		  refbox := references ;
		  WHILE refbox <> NIL DO
		    BEGIN
		      WITH refbox^ DO
		        FOR iter := 1 TO refnbr DO
			WITH refs [iter] DO
			  IF (place < ref_allowed.ic_from) OR
			    (place > ref_allowed.ic_to) THEN
			    BEGIN
			      ref_err := true ;
			      index := swrite (message, 1, ' ***** ILLEGAL REFERENCE TO LABEL ', labval : 1, ' AT LINE ') ;
			      IF filen <> 0 THEN
			        index := swrite (message, index, filen : 1, '-') ;
			      IF linen > 0 THEN
			        index := swrite (message, index, linen : 1) ELSE
			        index := swrite (message, index, -linen : 1) ;
			      write (mpcogout, message : index - 1) ; nextline ;
			      writeln (mpcogerr, message : index - 1)
			    END ;
		      refbox := refbox^.nextref
		    END
		END
	      END ;
	IF ref_err THEN error (227) ;
	IF lerr THEN error (168) ;
	clabix := fstix - 1 ;
	(* INSER  MAX STACK DEPL IN INST GENERATED IN PROLOG *) (* INSER            *)
	IF lca # 0 THEN                         (* NOT PREVIOUS ERROR *)
	  geninsertion (lca, surrptr) ;         (* LCA INIT  IN  ENTERBODY *)
                                                  (* BY  GENPROCENTRY *)
	IF mapswitch THEN BEGIN
	    statement_ends (1) ;
	    statement_begins (true) ;
	  END ;
	IF surrptr = NIL THEN level := 0 ;      (* FOR ERRORS SAVING *)
	(* FUNCTION CODE *)                     (* FUNCTION         *)
	IF level # 0 THEN
	  IF surrptr@.proctype # surrptr (* FUNCTION FLAG *) THEN
	    BEGIN
	      IF NOT surrptr@.procisassigned THEN error (155) ;
	      surrptr@.procinscope := false ;
	      genstand (pr0, functionvaluecheckplace, itsp3, tn) ;
	      gencodfonct (surrptr) ;
	    END ;
	(* GENERATES   PROCEDURE  ( PROGRAM) EXIT *) (* EXIT CODE        *)
	IF level = 0 THEN
	  genpgexit ELSE
	  genprcexit (surrptr) ;
	IF mapswitch THEN BEGIN
	    statement_ends (1) ;
	    statement_begins (false) ;
	  END ;
                                                  (* GENERATE F.REF. INFO IF FSB INIT. BY TRAP *)
	IF (level = 0) AND init_fsb_trap_flag THEN
	  FOR it := filev [0] TO filtop DO
	    gen_init_fsb_trap_structures (filpts [it]) ;

	filtop := filev [level] - 1 ;


(* SCANS PROC DEF. AT THIS LEVEL *)
(* AS FORWARD AND NOT DEFINED *)
	lp := next ;
	WHILE lp <> NIL DO
	  WITH lp@ DO
	    IF klass = proc THEN
	      BEGIN
	        IF prockind = imported THEN
		BEGIN
                                                  (* GENERATES LINK "ITS" *)
                                                  (* FOR IMPORTED PROCEDURES *)
		  IF errtotal = 0 THEN
		    BEGIN
		      IF procextitem <> NIL THEN
		        BEGIN
			locsegname := procextitem^.extsegname ;
			locentryname := procextitem^.extentryname ;
		        END ELSE
		        BEGIN
			locsegname := blank ; locentryname := blank ;
		        END ;

		      IF pwantdescs THEN locintext := ic ELSE locintext := 0 ;
		      genentrypoint (locintext, procaddr, 2,
		        locsegname, locentryname,
		        functionflag, entrylength,
		        locreturncode) ;
		      IF locreturncode <> 0 THEN
		        error (505) ;
		      IF pwantdescs THEN BEGIN
			usednameaddr := octalformataddr ;
			lp^.pextcalltrapinfoplace := ic DIV bytesinword ;
                                                  (* FILL NOW TRAP INFO STRUCTURE FOR EXT CALL WANTING DESCS.
                                                     FOR CONTENTS SEE : pascal_ext_call_trap_info.incl.pl1 *)
			infich (1) ;        (* VERSION NUMBER IN TRAP INFO STRUCTURE *)
			infich (0) ;        (* REL OFFSET TO PARM DESCS _ FILLED IN PASCAL_CREATE_TABLES *)
			usednameaddr := octalformataddr ;
			infich (enterreftosymbol (lp)) ;
			infich (pdescsaddrplace DIV bytesinword) ;
			genreltext (absl, 3) ; genreltext (int18, 1) ;
			trans.name := lp^.procextitem^.extgenerator ;
			FOR it := 1 TO 4 DO
			  BEGIN
			    usednameaddr := asciiformataddr ;
			    infich (trans.half_wd [it]) ;
			  END ;
			genreltext (absl, 4) ;
		        END ;
		    END ;
		END ELSE
		IF procdef = forwdef THEN
		  BEGIN
		    nextline ;
		    write (mpcogout, ' ***** PROC NOT DEFINED :', name) ; nextline ;
		    error (117) ;
		  END ;
	        lp := nxtel ;
	      END ELSE
	      lp := nxtel ;
	endcode := indfich - 1 ;
                                                  (* GENERATION OF WORD CSTES *)
	WHILE currwcstpt # NIL DO
	  WITH currwcstpt@ DO
	    BEGIN
	      inserundlab (cb, cstplace) ;
	      usednameaddr := octalformataddr ;
	      genc (valu) ;
	      currwcstpt := cstnext ;
	    END ;
	IF ic MOD bytesindword # 0 THEN genc (0) ;
                                                  (* GENERATION OF D-WORD CSTES *)
	WHILE currlcstpt # NIL DO
	  WITH currlcstpt@ DO
	    BEGIN
	      inserundlab (cb, lplace) ;
	      usednameaddr := octalformataddr ; genc (lvalu [0]) ; usednameaddr := octalformataddr ; genc (lvalu [1]) ;
	      currlcstpt := lnext ;
	    END ;
                                                  (* GENERATION OF REAL CSTES *)
	WHILE currrcstpt # NIL DO
	  WITH currrcstpt@ DO
	    BEGIN
	      inserundlab (cb, rplace) ;
	      genr (rvalu) ;
	      currrcstpt := rnext ;
	    END ;
                                                  (* GENERATION OF SET(8 W) CSTES *)
	WHILE currllcstpt # NIL DO
	  WITH currllcstpt@ DO
	    BEGIN
	      inserundlab (cb, llplace) ;
	      FOR it := 0 TO bornesupset DO BEGIN usednameaddr := octalformataddr ; genc (llvalu [it]) ; END ;
	      currllcstpt := llnext ;
	    END ;
                                                  (* NOW GENERATES  ALFA STRINGS *)
	lp := nextalf ;                         (* LAST  ALFA CONST USED *)
	WHILE lp # NIL DO
	  WITH lp@ DO
	    BEGIN
	      IF unddeb <> 0 THEN
	        BEGIN
		inserundlab (cb, unddeb) ;
		genstring (lp) ;
		IF NOT odd (indfich) THEN infich (0) ; unddeb := 0 ;
	        END ;
	      lpaux := lp ; lp := succ ; lpaux@.succ := lpaux ; (* NEXT AND FREE OLD OCC. *)
	    END ;
	writout (lic, endcode) ;
	IF mapswitch THEN
	  statement_ends (0) ;
$OPTIONS compile = trace $
	IF decltrace > low THEN
	  BEGIN
	    write (mpcogout, ' @@@ FIN LEAVEBODY @@@ WITH  IC,CB ', ic, cb) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* LEAVEBODY *) ;


(* ***********************************************VALUEDECL  < BODY *********** *)

      PROCEDURE valuedecl ;

(* C     THIS PROCEDURE IS USED IN ORDER TO ANALYZE THE VALUE PART OF A MAIN
   PROGRAM. SPACE IS KEPT FOR ALL VARIABLES AND VARIABLES WHICH
   OCCUR  IN VALUE PART ARE INITIALIZED.                              C *)
(* E     2 IDENTIFIER EXPECTED
   15 INTEGER EXPECTED
   16 '=' EXPECTED
   64 ',' OR ')' EXPECTED IN VALUE PART
   69 VALUE PART NOT ALLOWED (STANDARD)
   104 IDENTIFIER NOT DECLARED
   130 NIL NO MORE ALLOWED (STANDARD)
   138 TYPE OF THE VARIABLE IS NOT ARRAY OR RECORD
   145 TYPE CONFICT
   178 ALPHANUMERIC STRING IS TOO LONG
   179 INITIALIZATION LIST IS TOO LONG
   180 INITIALIZATION OF IMPORTED VARIABLE NOT ALLOWED
   181 VARIABLE MUST BE ARRAY OR RECORD
   182 PACKED VARIABLE NOT ALLOWED HERE
   183 ILLEGAL VARIABLE TYPE IN VALUE PART
   184 IDENTIFIER MUST BE VARIABLE (VALUE)
   185 VARIABLES MUST BE INITIALIZED IN THEIR DECLARATION ORDER           E *)
        LABEL 10,                                 (* END OF VALUE PART *)
	20,                                     (* EMERGENCY LABEL USED IF *)
                                                  (* SEVERAL CALLS OCCUR *)
	5 ;                                     (* STOPS LIST INSPECTION *)
        VAR
	wkextpt : ptexternalitem ;
	itisstring, invalue, valerr : boolean ;
	addcurrent, nbpack, alfamax, nbitem, nrep, i, nitem, it, kt : integer ;
	cstkind : 1..4 ; strlen : integer ;
	oldnext, generic, before, curritem, pt, pteltype, toinit : ctp ;
	filesize : integer ;
	locreturncode : integer ;
	wkname : alfaid ;


(* *************************************VALERROR < VALUEDECL < BODY************ *)

        PROCEDURE valerror (fnoerr : integer) ;

(* C  PRODUCES AN ERROR MESSAGE AND FINDS A SEMI-COLON. VALERR IS SET TRUE    C *)
	BEGIN
	  error (fnoerr) ;
	  valerr := true ;
	  skip (16) ;
	  IF no # 16 THEN
	    BEGIN error (14) ; GOTO 1 ; (* LABEL PART IN BODY *) END ; insymbol ;
	END (* VALERROR *) ;


        BEGIN                                     (* VALUEDECL *)
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, ' @@@ DEBUT VALUEDECL @@@ WITH NEXT ,XC ', ord (next), xc) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
	valuenb := valuenb + 1 ;                (* IF NO ERROR MUST BE ONE *)
	IF valuenb > 1 THEN GOTO 20 ;           (* EXIT WITH EMERGENCY *)
                                                  (* REVERSE LINKAGE OF THE *)
                                                  (* CONTEXTE TABLE TO THIS LEVEL *)
	IF next # NIL THEN
	  BEGIN
	    oldnext := NIL ; before := next@.nxtel ;
	    WHILE before # NIL DO
	      BEGIN
	        next@.nxtel := oldnext ;
	        oldnext := next ;
	        next := before ;
	        before := before@.nxtel ;
	      END ;
	    next@.nxtel := oldnext ;
	  END ;
	IF no = 54 (* VALUE *) THEN
	  BEGIN
	    IF envstandard = stdpure THEN
	      error (69) ;
	    insymbol ; invalue := true ;
	  END ELSE invalue := false ;
	addcurrent := xc ;                      (* CURRENT ADDRESS IN BYTES *)
	curritem := next ;                      (* CURRENT ITEM OF CONTEXTABLE *)
	toinit := NIL ;                         (* LAST INITIALIZED VALUE *)
	oldnext := next ;                       (* FIRST INITIALIZABLE VAR *)
	WHILE curritem # NIL DO                 (* SCAN CONTEXTABLE *)
	  WITH curritem@ DO
	    IF klass # vars THEN curritem := nxtel (* NOT A VARIABLE *) ELSE
	      IF vtype = NIL THEN curritem := nxtel (* ERROR IN TYPE  *) ELSE
	        BEGIN
		IF vtype^.form = files THEN
		  BEGIN
		    IF vkind <> imported THEN
		      BEGIN
		        IF vkind = exportable THEN
			BEGIN
			  environt := linkage ; vaddr := lkc ;
			  lkc := lkc + bytesindword ;
			  IF errtotal = 0 THEN
			    BEGIN
			      genexportfile (name, vaddr, locreturncode) ;
			      IF locreturncode <> 0 THEN
			        error (509) ;
			    END ;
			  indfich := 1 ; environt := data ;
			END (* EXPORTABLE *) ELSE
			BEGIN
			  addcurrent := vaddr ;
			  genmulticsnil ;
			  writout (addcurrent, 0) ;
			  addcurrent := vaddr + bytesneeded (files, 0, false) ;
			END (* STATIC or PERMANENT FILE *) ;
		      END (* not IMPORTED *) ;
		  END (* FILES *) ELSE
		  IF invalue THEN             (* LOOK IF THE CURRENT VARIABLE *)
                                                  (* IS IN VALUE LIST *)
		    BEGIN
		      WHILE no # 1 DO         (* SEEKS INITIALIZED IDENTIFIERS *)
		        BEGIN
			IF no IN [21, 44, 45, 55] (* BEGIN,PROCEDURE,FUNCTION $ *) THEN
			  BEGIN
			    IF no <> 55 THEN
			      error (76) ELSE
			      insymbol ;
			    invalue := false ; GOTO 10 (* EXIT INVALUE *) ;
			  END ;
			error (2) ; skip (46) ;
			IF no # 16 (* ; *) THEN
			  BEGIN
			    invalue := false ; GOTO 10 (* EXIT INVALUE *) ;
			  END ;
			insymbol ;
		        END ;
                                                  (* IDENTIFIER HAS BEEN FOUND *)
		      srchrec (next) ;        (* LOOKS IN CONTEXTTABLE *)
		      IF ctptr = NIL THEN valerror (104) (* UNDECLARED *) ELSE
		        IF ctptr@.klass # vars THEN valerror (184) (* NOT A VARIABLE *) ELSE
			toinit := ctptr ;
		      IF toinit = curritem THEN
		        BEGIN                 (* DECLARED IDENTIFIER *)
			srchrec (oldnext) ;
			IF symbolmap THEN
			  nameisref (ctptr, symbolfile, -symbolline) ;
			IF ctptr = NIL THEN valerror (185) ELSE
			  WITH ctptr@ DO    (* INITIALIZABLE *)
			    IF vkind = imported THEN valerror (180) ELSE
			      BEGIN
			        valerr := false ;
			        IF vkind = exportable THEN
				environt := linkage ELSE
				addcurrent := vaddr ;
			        insymbol ;
			        IF (no # 8) OR (cl # 6) (* = *)
			        THEN error (16) ELSE insymbol ;
			        pteltype := vtype ; itisstring := false ; nbpack := 0 ;
			        WHILE pteltype@.form = arrays DO
				BEGIN
				  IF pteltype@.pack THEN
				    BEGIN
				      nbpack := nbpack + 1 ; (* NBR OF SUBTYPES PACKED *)
				      IF nbpack = 1 THEN
				        IF pteltype@.aeltype = charptr THEN
					BEGIN
					  itisstring := true ;
                                                  (* SIZE OF ALFA STRING : *)
					  alfamax := pteltype@.size ;
					END (* STRING OF CHAR *) ;
				    END ;
				  pteltype := pteltype@.aeltype ;
				  IF pteltype = NIL THEN valerror (183) ;
				END ;
			        IF pteltype^.father_schema = string_ptr THEN
				BEGIN
				  cstkind := 4 ;
				  alfamax := pteltype^.actual_parameter_list^.values ;
				END ELSE
				BEGIN
				  IF pteltype@.form IN
				    [pointer, power, files, aliastype] THEN
				    valerror (183) ELSE
				    IF (nbpack # 0) THEN
				      IF itisstring THEN cstkind := 3 ELSE
				        valerror (182) ELSE
				      IF pteltype = realptr
				      THEN cstkind := 2 ELSE cstkind := 1 ;
				  IF NOT valerr THEN
				    IF (vtype@.form = records) OR
				      ((vtype@.form = arrays) AND (NOT vtype@.pack))
				    THEN
				      BEGIN
				        IF no # 9 (* ( *) THEN valerror (009) ;
				      END
				    ELSE
				      IF no = 9 THEN valerror (138) ;
				END ;
			        IF NOT valerr THEN
				BEGIN
				  IF no = 9 THEN (* LIST OF VALUES *)
				    BEGIN
				      IF vtype@.form = records THEN
				        BEGIN
					pteltype := intptr ; cstkind := 1 ;
				        END ;
				      CASE cstkind OF
				        1 : nbitem := vtype@.size DIV intptr@.size ;
				        2 : nbitem := vtype@.size DIV realptr@.size ;
				        3 : nbitem := vtype@.size DIV alfamax ;
				        4 : nbitem := vtype^.size DIV (alfamax + 4) ;
				      END (* CASE *) ;
				      nitem := 0 ;
				      REPEAT
				        insymbol ; inconst (i, pt, next, false) ;
				        nrep := 1 ;
				        IF (no = 6) AND (cl = 1) (* * *) THEN
					BEGIN
					  IF (i = 1) AND (conint > 0)
					  THEN nrep := conint ELSE error (15) ;
					  insymbol ; inconst (i, pt, next, true) ;
					END ;
				        nitem := nitem + nrep ;
				        IF nitem > nbitem THEN (* TOO MANY ITEMS IN THE LIST *)
					BEGIN
					  valerror (179) ;
					  GOTO 5 ; (* SKIP INITIAL VALUE -> ; *)
					END ;
				        CASE cstkind OF
					1 : BEGIN
					    compatbin (pteltype, pt, generic) ;
					    IF (generic = NIL) OR (generic = realptr)
					    THEN error (145) ELSE
					      BEGIN
					        IF pteltype # intptr THEN
						checkminmax (conint, pteltype, 303) ;
					        FOR it := 1 TO nrep DO genc (conint) ;
					      END ;
					  END ;
					2 : BEGIN
					    IF pt # pteltype THEN
					      IF pt = intptr
					      THEN conreel := conint ELSE
					        error (145) ;
					    FOR it := 1 TO nrep DO genr (conreel) ;
					  END ;
					3 : IF pt # alfaptr THEN error (145) ELSE
					    BEGIN
					      IF longstring > alfamax THEN error (178) ;
					      longstring := alfamax ; (* TRUNC OR PAD *)
					      FOR it := 1 TO nrep DO genalfa ;
					    END ;
					4 : IF pt <> alfaptr THEN
					    IF pt = charptr THEN
					      FOR it := 1 TO nrep DO
					        BEGIN
						genc (1) ; genc (conint * twoto27) ;
						IF alfamax > 4 THEN
						  FOR kt := 1 TO ((alfamax - 1) DIV 4) DO genc (0) ;
					        END
					    ELSE error (145)
					  ELSE
					    BEGIN
					      IF longstring > alfamax THEN
					        BEGIN
						error (178) ;
						strlen := alfamax ;
					        END ELSE
					        strlen := longstring ;
					      longstring := alfamax ; (* TRUNC OR PAD *)
					      FOR it := 1 TO nrep DO
					        BEGIN
						genc (strlen) ;
						genalfa ;
					        END ;
					    END ;
				        END (* CASE *) ;
				        IF NOT (no IN [10, 15]) (* , ) *) THEN error (64) ;
				      UNTIL no # 15 ;
				      IF no = 10 THEN insymbol ;
				    END ELSE (* ONE CONSTANT ONLY *)
				    BEGIN
				      inconst (i, pt, next, true) ;
				      CASE cstkind OF
				        1 : BEGIN
					  compatbin (pteltype, pt, generic) ;
					  IF (generic = NIL) OR (generic = realptr) THEN
					    error (145) ELSE
					    BEGIN
					      IF pteltype # intptr THEN
					        checkminmax (conint, pteltype, 303) ;
					      genc (conint) ;
					    END ;
					END ;
				        2 : BEGIN
					  IF pt # pteltype THEN
					    IF pt = intptr
					    THEN conreel := conint ELSE error (145) ;
					  genr (conreel) ;
					END ;
				        3 : IF pt # alfaptr THEN error (145) ELSE
					  BEGIN
					    IF longstring > alfamax THEN error (178) ;
					    longstring := alfamax ;
					    genalfa ;
					  END ;
				        4 : IF pt <> alfaptr THEN
					  IF pt = charptr THEN
					    BEGIN
					      genc (1) ; genc (conint * twoto27) ;
					      IF alfamax > 4 THEN
					        FOR it := 1 TO ((alfamax - 1) DIV 4) DO genc (0) ;
					    END
					  ELSE error (145)
					ELSE
					  BEGIN
					    IF longstring > alfamax THEN
					      BEGIN
					        error (178) ;
					        genc (alfamax) ;
					      END
					    ELSE
					      genc (longstring) ;
					    longstring := alfamax ;
					    genalfa ;
					  END ;
				      END (* CASE *) ;
				    END (* ONE CONSTANT ONLY *) ;
				  IF NOT (no IN [16, 55]) THEN
				    BEGIN
				      error (76) ; skip (46) ;
				    END ELSE
				    IF no = 16 (* ; *) THEN
				      BEGIN
				        insymbol ;
				        IF no = 55 THEN
					BEGIN insymbol ; invalue := false ;
					END ELSE
					IF no <> 1 THEN
					  error (76) ;
				      END ELSE
				      BEGIN insymbol ; invalue := false ;
				      END ;
				END (* NOT VALERR *) ;
5 :
			        IF environt = linkage THEN
				BEGIN
				  vaddr := lkc ;
				  lkc := lkc + bytesindword ;
				  IF errtotal = 0 THEN
				    BEGIN
				      genextvariable (blank, name, blank,
				        vaddr, vtype^.size, indfich - 1, fichinter^,
				        locreturncode) ;
				      IF locreturncode <> 0 THEN
				        error (508) ;
				    END ;
				  indfich := 1 ;
				  environt := data ;
				END ELSE
				BEGIN
				  writout (addcurrent, 0) ;
				  addcurrent := recadre (addcurrent + vtype@.size,
				    bytesinword) ;
				END ;
			      END ;         (* WITH CTPTR,INITIALIZABLE, *)
                                                  (* NOT IMPORTED *)
		        END ;                 (* DECLARED ID. *)
		    END ;                     (* INVALUE *)
10 :		curritem := curritem@.nxtel ;
	        END ;                           (* SCAN CONTEXT TABLE *)
	IF addcurrent < lc THEN
	  IF errtotal = 0 THEN
	    BEGIN
	      genbinarea (addcurrent, 4,
	        (lc - addcurrent) DIV bytesinhword,
	        0, fichinter^,
	        locreturncode) ;
	      IF locreturncode <> 0 THEN
	        error (507) ;
	    END ;
                                                  (* NOW CREATES "ITS" FOR IMPORT *)
                                                  (* OR EXPORTED NOT YET RESOLVED *)
	wkextpt := externallistheader ;
	WHILE wkextpt <> NIL DO
	  BEGIN
	    WITH wkextpt^ DO
	      IF extdecl <> NIL THEN
	        IF extdecl^.klass = vars THEN
		IF extdecl^.vaddr = -1 THEN
		  IF extdecl^.vtype <> NIL THEN
		    IF (extdecl^.vtype^.form = files) AND (extdecl^.vkind = exportable) THEN ELSE
		      WITH extdecl^ DO
		        BEGIN
			vaddr := lkc ; lkc := lkc + bytesindword ;
			IF vtype # NIL THEN
			  IF vkind = imported THEN i := -vtype@.size ELSE i := vtype@.size ;
			IF errtotal = 0 THEN
			  BEGIN
			    IF name = usednames [1] THEN
			      geninputlink (vaddr, locreturncode) ELSE
			      IF name = usednames [2] THEN
			        genoutputlink (vaddr, locreturncode) ELSE
			        IF name = usednames [3] THEN
				generrorlink (vaddr, locreturncode) ELSE
				BEGIN
				  IF i < 0 THEN
				    wkname := extentryname (* IMPORTED *) ELSE
				    wkname := extname ;
				  genextvariable (extsegname, wkname, extgenerator,
				    vaddr, i, 0,
				    fichinter^,
				    locreturncode) ;
				END ;
			    IF locreturncode <> 0 THEN
			      error (506) ;
			  END ;
		        END ;
	    wkextpt := wkextpt ^.extnext ;
	  END ;
$OPTIONS compile = trace $
	IF decltrace > low THEN
	  BEGIN
	    write (mpcogout, ' @@@ FIN VALUEDECL @@@  WITH LKC,LC ', lkc, lc) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
20 :                                              (* SKIP HERE IF NOT FIRST *)
                                                  (* CALL OF VALUEDECL *)
        END (* VALUEDECL *) ;


(* ***********************************************FORMPARM < BODY************** *)

      PROCEDURE formparm ;

(* C   ANALYZES  THE  LIST OF PARAMETERS OF A PROCEDURE OR A FUNCTION
   IF NESTPROC =0  PARAMETERS ARE RELEVANT
   IF NESTPROC #0  PARAMETERS ARE DUMMY AND USED ONLY FOR TYPE COMPATIBILITY
   IN PROCEDURE OR FUNCTION PARAMETERS
   CONFORMANT ARRAY TYPES ARE ANALYZED WITH PROCEDURE CONFORMARRAY
   C *)
(* E    HEAPERROR
   2 IDENTIFIER EXPECTED
   4 ')' EXPECTED
   7 ':' EXPECTED
   101 IDENTIFIER DECLARED TWICE
   103 IDENTIFIER IS NOT OF  APPROPRIATE CLASS
   104 IDENTIFIER NOT DECLARED
   120 FUNCTION RESULT TYPE MUST BE SCALAR,SUBRANGE,REAL OR POINTER
   121 FILE MUST BE VAR PARAMETER
   123 MISSING RESULT IDENTIFIER IN FUNCTION DECLARATION
   E *)
        VAR
	it, nbpar, savenbpar, lcaux : integer ;
	lp, lp1, savenext : ctp ;
	itisproc, itisvar, rep : boolean ;
	savedescriptors : boolean ;
	locended : boolean ;
	loccounter : integer ;
	locad : integer ;
	lctop : integer ;
	nbofdim : integer ;
	lctp : ctp ;
	lctp1, lctp2, lctp3 : ctp ;
	schema_parameter_count : integer ;


        PROCEDURE conformarray (VAR fvnombofdim : integer) ;

(* C
   As output FVNOMBOFDIM is the number of pseudo-parameters
   created for Read-Only bounds.

   Analyses a <conformant array schema>
   Is a local procedure of FORMPARM to avoid a "too long procedure"
   error in FORMPARM.
   Is invocated with NO=42          "packed"
   or   NO=38 and CL=1 "array"
   Expects :
   [packed] array "[" <id>..<id> : <type-id> [ ; <id>..<id> : <type-id> ]*
   "]" of <param_type>
   At the call, the descriptive boxe(s) of the parameter(s) have been
   constructed with a nil VTYPE. NEXT is the head of the backward chain
   of parameters. NBPAR is the number of variables of the conformant array
   schema to be analysed.
   It constructs as many array boxes as dimensions declared and two VAR
   READONLY boxes (one for lower bound, one for higher bound) by dimension.
   The bound boxes are inserted in the backward parameter's chain.
   At the end of CONFORMARRAY the variable boxes are completed with
   VTYPE and VADDR.
   C *)

(* E ERRORS DETECTED
   2    identifier expected
   5    .. expected
   7    :  expected
   8    OF expected
   11    [ expected
   12    ] expected
   56    type identifier or conformant array schema expected
   57    conformant array schema expected
   103    identifier is not of the appropriate CLASS
   104    identifier not declared
   71 Pack allowed only on last dimension
   113 Index type must be scalar or numeric
   E *)

	LABEL
	  1 ;                                   (* exit in case of non recoverable error *)

	VAR
	  it : integer ;
	  conformagain : boolean ;
	  lp : ctp ;
	  packedfound : boolean ;
	  ptfirstbound : ctp ;
	  ptfirstdim : ctp ;
	  ptlastdim : ctp ;
	  ptfirstvar : ctp ;
	  ptsecondvar : ctp ;
	  ptlastvar : ctp ;
	  nbofdim : integer ;
	  ptsecondbound : ctp ;

	BEGIN                                   (* CONFORMARRAY *)

$OPTIONS compile = trace $
	  IF decltrace > none THEN
	    BEGIN
	      write (mpcogout, '@@@ Debut de CONFORMARRAY @@@ avec NO :',
	        no : 5, ' CL:', cl : 5, ' NBPAR :', nbpar : 5, ' NEXT en^', ord (next)) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $


	  fvnombofdim := 0 ;
	  nbofdim := 0 ;
	  ptfirstbound := NIL ;
	  lctop := 0 ;
	  ptsecondbound := NIL ; packedfound := false ;
	  ptfirstdim := NIL ;
	  ptlastdim := NIL ;
	  ptlastvar := next ;

	  REPEAT
	    conformagain := false ;

(*  CHECK IF SYMBOL FOUND IS "PACKED"    OR   "ARRAY"    *)

	    IF no = 42 THEN                     (* packed *)
	      BEGIN
	        insymbol ;
	        IF packedfound THEN
		error (71) ;
	        packedfound := true ;
	      END ELSE
	      packedfound := false ;

	    IF NOT ((no = 38) AND (cl = 1)) THEN (* array *)
	      BEGIN
	        error (57) ;
	        skipextd ([10]) ;
	        GOTO 1 ;
	      END ELSE
	      BEGIN                             (* ARRAY  *)
	        insymbol ;
	        IF no <> 11 THEN                (* [ *)
		BEGIN
		  error (11) ;
		  skipextd ([10]) ;
		  GOTO 1 ;
		END ELSE
		BEGIN

		  REPEAT                      (* LOOP ON DIMENSIONS *)

(* FIRST BOUND  *)

		    insymbol ;
		    IF no <> 1 THEN
		      BEGIN
		        error (2) ;
		        skipextd ([10]) ;
		        GOTO 1 ;
		      END ;
		    checkdefiningpoint (aval, next) ;
		    create_vars_box (lp, aval) ;
		    WITH lp^ DO
		      BEGIN
		        vkind := arraybound ;
		        visset := true ;
		        visreadonly := true ;
		      END ;
		    next := lp ;
		    ptfirstbound := lp ;
		    fvnombofdim := fvnombofdim + 1 ; (* <-------- *)

(* BOUNDS SEPARATOR ..    *)

		    insymbol ;
		    IF no <> 39 THEN
		      BEGIN
		        error (5) ;
		        skipextd ([10, 39]) ;
		        IF no <> 39 THEN
			GOTO 1 ;
		      END ;

(* SECOND BOUND   *)

		    insymbol ;
		    IF no <> 1 THEN
		      BEGIN
		        error (2) ;
		        skipextd ([10]) ;
		        GOTO 1 ;
		      END ;
		    checkdefiningpoint (aval, next) ;
		    create_vars_box (lp, aval) ;
		    WITH lp^ DO
		      BEGIN
		        vkind := arraybound ;
		        visset := true ;
		        visreadonly := true ;
		      END ;
		    next := lp ;
		    fvnombofdim := fvnombofdim + 1 ; (* <-------- *)
		    ptsecondbound := lp ;

(* DECLARED TYPE FOR BOUNDS  *)

		    insymbol ;
		    IF no <> 19 THEN          (* : *)
		      BEGIN
		        error (7) ;
		        skipextd ([10, 19]) ;
		        IF no <> 19 THEN
			GOTO 1 ;
		      END ;
		    insymbol ;
		    IF no <> 1 THEN
		      BEGIN
		        error (2) ;
		        skipextd ([10]) ;
		        GOTO 1 ;
		      END ;
		    srchrec (next) ; IF ctptr = NIL THEN search ;
		    IF ctptr = NIL THEN       (* not found *)
		      BEGIN
		        error (104) ;
		      END ELSE
		      BEGIN
		        IF symbolmap THEN
			nameisref (ctptr, symbolfile, symbolline) ;
		        IF ctptr^.klass = types THEN
			BEGIN
                                                  (* LOCKNAME(AVAL,USEDNAME) ;  *)
			  IF ctptr^.form = aliastype THEN
			    ctptr := ctptr^.realtype ;
			  IF NOT (ctptr^.form IN [scalar, numeric]) THEN
			    error (113) ;
			END ELSE
			BEGIN
			  error (103) ;
			  ctptr := NIL ;
			END ;
		      END ;
		    next^.vtype := ctptr ;
		    next^.nxtel^.vtype := ctptr ;

(* CREATE CONFORMANT ARRAY DIMENSION BOX *)

		    create_types_box (lp, blank, arrays, true) ;
		    WITH lp^ DO
		      BEGIN
		        pack := packedfound ;
		        inxtype := ctptr ;
		        ptlow := ptfirstbound ;
		        pthigh := ptsecondbound ;
		      END ;

		    IF ptlastdim = NIL THEN
		      ptfirstdim := lp ELSE
		      ptlastdim^.aeltype := lp ;
		    lp^.nxtel := ptlastdim ;
		    ptlastdim := lp ;
		    nbofdim := nbofdim + 1 ;

		    insymbol ;
		    IF (no <> 16) AND (no <> 12) THEN (* ; or ] *)
		      BEGIN
		        error (12) ;
		        skipextd ([10, 12]) ;
                                                  (* it would be hazardous to consider ";" (if found by SKIP)
                                                     as a separator of dimensions instead of a separator
                                                     of parameters *)
		        IF no <> 12 THEN
			GOTO 1 ;
		      END ;

$OPTIONS compile = trace $
		    IF decltrace > none THEN
		      BEGIN
		        write (mpcogout, '@ CONFORMARRAY ( until NO <> 16) :') ; nextline ;
		        write (mpcogout, '@ CONFORMARRAY (until ..). NEXT est en ^',
			ord (next), ' PTLASTDIM,PTFIRSTDIM en ^',
			ord (ptlastdim), ord (ptfirstdim)) ;
		        nextline ;
		      END ;
$OPTIONS compile = true $

		  UNTIL no <> 16 ;            (* ; *)
                                                  (* FIN LOOP ON DIMENSIONS *)



(* EXPECTED SYMBOLS ARE NOW
   12     ]
   27     of
   1     Type_Identifier
   *)

		  IF no <> 12 THEN            (* ] *)
		    BEGIN
		      error (12) ;
		      skipextd ([10, 12]) ;
		      IF no <> 12 THEN
		        GOTO 1 ELSE
		        insymbol ;
		    END ELSE
		    insymbol ;
		  IF no <> 27 THEN            (* of *)
		    BEGIN
		      error (8) ;
		      skipextd ([10, 27]) ;
		      IF no <> 27 THEN
		        GOTO 1 ELSE
		        insymbol ;
		    END ELSE
		    insymbol ;

(* TYPE IDENTIFIER   ?   *)

		  IF no = 1 THEN
		    BEGIN
		      srchrec (next) ; IF ctptr = NIL THEN search ;
		      IF ctptr = NIL THEN     (* not found *)
		        BEGIN
			error (104) ;
		        END ELSE
		        BEGIN
			IF symbolmap THEN
			  nameisref (ctptr, symbolfile, symbolline) ;
			IF ctptr^.klass = types THEN
			  BEGIN
                                                  (* LOCKNAME(AVAL,USEDNAME) ;  *)
			    IF ctptr^.form = aliastype THEN
			      ctptr := ctptr^.realtype ;
			  END ELSE
			  BEGIN
			    error (103) ;
			    ctptr := NIL ;
			  END ;
		        END ;
		      ptlastdim^.aeltype := ctptr ;
		      IF ctptr <> NIL THEN
		        BEGIN
			lp := ptlastdim ;
			WHILE lp <> NIL DO
			  BEGIN
			    IF packedfound THEN
			      lp^.cadrage := packedcadre (ctptr) ELSE
			      lp^.cadrage := ctptr^.cadrage ;
			    lp := lp^.nxtel ;
			  END ;
		        END ;
		    END ELSE
		    IF (no = 42) OR (no = 38) AND (cl = 1) THEN (* array *)
		      BEGIN
		        conformagain := true ;
		      END ELSE
		      BEGIN
		        error (56) ;
		        skipextd ([10]) ;
		        GOTO 1 ;
		      END ;
		END (* NO=11 *) ;
	      END (* NO=38 and CL=1 *) ;
	  UNTIL NOT conformagain ;

	  lp := ptfirstdim ;                    (* FILL NOW VDISPL FIELD IN BOUNDS BOXES *)
	  FOR it := nbofdim DOWNTO 1 DO
	    IF lp <> NIL THEN
	      BEGIN
	        lp^.ptlow^.vdispl := it * 12 - 8 ;
	        lp^.pthigh^.vdispl := it * 12 - 4 ;
	        lp := lp^.aeltype
	      END ;

	  IF nbpar = 1 THEN
	    BEGIN
	      ptlastvar^.vtype := ptfirstdim ;
	    END ELSE
	    BEGIN
                                                  (* Break NXTEL chain *)
	      lp := ptlastvar ;
	      FOR it := 1 TO nbpar DO
	        BEGIN
		lp^.vtype := ptfirstdim ;
		lp := lp^.nxtel ;
	        END ;
	      ptfirstvar := ptlastvar ;
	      FOR it := 1 TO nbpar - 1 DO
	        BEGIN
		ptsecondvar := ptfirstvar ;
		ptfirstvar := ptfirstvar^.nxtel ;
	        END ;
	      ptsecondvar^.nxtel := next ;
	      next := ptlastvar ;
	      ptfirstbound^.nxtel := ptfirstvar ;
	    END ;
1 :

$OPTIONS compile = trace $
	  IF decltrace = high THEN
	    BEGIN
	      write (mpcogout, '@@@ Fin   de CONFORMARRAY @@@ avec NO,CL:', no : 5, cl : 5, ' ',
	        'PTLASTDIM,PTFIRSTDIM ^', ord (ptlastdim), ord (ptfirstdim),
	        ' PTFIRSTVAR,PTSECONDVAR,PTLASTVAR en ^',
	        ord (ptfirstvar), ord (ptsecondvar), ord (ptlastvar)) ;
	      nextline ;
	      write (mpcogout, '@@@ CONFORMARRAY returns FVNOMBOFDIM =', fvnombofdim) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $
	END (* CONFORMARRAY *) ;


        BEGIN                                     (* FORMPARM *)
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, ' @@@ DEBUT FORMPARM @@@ WITH NESTPROC,LC,NEXT', nestproc, lc,
	      ord (next)) ; nextline ;
	  END ;
$OPTIONS compile = true $
	REPEAT
	  IF no IN [44, 45] (* FUNC OR PROC PARAMETER *) THEN
	    BEGIN
	      itisproc := no = 45 ; insymbol ;
	      IF no # 1 THEN error (2) ELSE
	        BEGIN
		srchrec (next) ;
		IF ctptr # NIL THEN error (101) (* PARAMETER YET USED *) ELSE
		  BEGIN
		    create_proc_box (lp, aval) ;
		    WITH lp^ DO
		      BEGIN
		        proctype := lp ; prockind := formal ;
		        IF nestproc = 0 THEN
			BEGIN
			  procaddr := lc ;
			  lc := lc + bytesindword ;
			END ;
		      END ;
		    globnbpar := globnbpar + 1 ;
		    next := lp ;
		    nestproc := nestproc + 1 ;
		    savenext := next ; next := NIL ;
		    insymbol ;
		    savenbpar := globnbpar ;
		    globnbpar := 0 ;
		    savedescriptors := globdescriptors ;
		    globdescriptors := false ;
		    IF no = 9 THEN            (* ( *)
		      BEGIN
		        insymbol ; formparm ;
		        IF no = 10 THEN insymbol ;
		      END ;
		    IF NOT itisproc THEN
		      BEGIN
		        IF no = 19 (* : *) THEN insymbol ELSE error (7) ;
		        IF no # 1 THEN
			BEGIN
			  error (123) ; lp@.proctype := NIL ; skip (46) ;
			END ELSE
			BEGIN
			  search ;
			  IF ctptr # NIL THEN
			    BEGIN
			      IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
			      IF ctptr@.klass # types THEN
			        BEGIN
				error (103) ; ctptr := NIL ;
			        END ELSE
			        BEGIN
				IF ctptr@.form = aliastype THEN ctptr := ctptr@.realtype ;
				IF ctptr@.form >= power THEN
				  BEGIN
				    error (120) ; ctptr := NIL ;
				  END ;
			        END ;
			    END ELSE error (104) ;
			  globnbpar := globnbpar + 1 ;
			  lp@.proctype := ctptr ;
			  insymbol ;
			END ;
		      END ;                   (* TYPE OF FUNCTION *)
		    lp@.nbparproc := globnbpar ;
		    globnbpar := savenbpar ;
		    lp^.phasdescriptor := globdescriptors ;
		    globdescriptors := savedescriptors ;
		    lp@.segsize := nestproc ; (* LEVEL OF NESTING *)
		    lp@.formals := next ;     (* LIST OF PARAMETERS *)
		    next := savenext ;
		    nestproc := nestproc - 1 ;
		  END ;                       (* CTPTR=NIL *)
	        END ;                           (* NO=1 *)
	    END (* NO IN [44,45] *) ELSE
	    BEGIN
	      IF no = 43 (* VAR *) THEN
	        BEGIN
		itisvar := true ;
		insymbol ;
	        END ELSE itisvar := false ;
	      IF no = 1 THEN
	        BEGIN
		nbpar := 0 ;
		REPEAT                        (* ID1,ID2,... *)
		  srchrec (next) ;
		  IF ctptr # NIL THEN
		    BEGIN
		      IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		      error (101)             (* YET USED *)
		    END
		  ELSE
		    BEGIN
		      nbpar := nbpar + 1 ;
		      create_vars_box (lp, aval) ;
		      WITH lp^ DO
		        BEGIN
			vkind := formal ; varparam := itisvar ; visset := true ;
		        END ;
		      next := lp ;
		      globnbpar := globnbpar + 1 ;
		    END ;                     (* NEW PARAMETER *)
		  insymbol ;
		  IF no = 15 (* , *) THEN
		    BEGIN
		      insymbol ;
		      IF no = 19 (* : *) THEN error (2) ; (* TO DETECT ,: *)
		    END ELSE
		    IF no <> 19 THEN
		      BEGIN error (7) ; insymbol ;
		      END ;
		UNTIL no # 1 ;
		IF no = 19 (* : *) THEN insymbol ;
		IF (no = 42 (* PACKED *)) OR ((no = 38) AND (cl = 1)) THEN
		  BEGIN
		    conformarray (nbofdim) ;
		    globdescriptors := true ;
		    IF nestproc = 0 THEN
		      BEGIN
		        lc := lc + (nbpar * bytesindword) ; lcaux := lc ;
		        lp := next ; lctop := lcaux ;
		        FOR it := 1 TO (nbofdim + nbpar) DO
			BEGIN
			  IF lp <> NIL THEN
			    BEGIN
			      IF lp^.vkind <> arraybound THEN
			        BEGIN
				lcaux := lcaux - bytesindword ;
				lp^.vaddr := lcaux ;
				lp^.vdescaddr := -1 ;
			        END ;
			      lp := lp^.nxtel ;
			    END ;
			END ;
		      END ;
		  END ELSE
		  IF no # 1 THEN error (2) ELSE
		    BEGIN                     (* TYPE IDENTIFIER *)
		      search ;
		      IF ctptr = NIL THEN
		        error (104)
		      ELSE
		        BEGIN
			IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
			IF ctptr^.klass = schema THEN
			  BEGIN
			    IF procptr^.pwantspl1descriptors THEN
			      error (448) ;
			    IF NOT itisvar THEN
			      BEGIN
			        itisvar := true ;
			        error (281) ;
			      END ;
			    create_types_box (lctp, ctptr^.name, records, false) ;
			    WITH lctp^ DO
			      BEGIN
			        father_schema := ctptr ;
			      END ;
			    ctptr := lctp ;
			    globdescriptors := true ;
			  END ;
			IF ctptr@.klass # types THEN error (103) ELSE
			  BEGIN
			    IF ctptr@.form = aliastype THEN ctptr := ctptr@.realtype ;
			    IF (ctptr^.form = files) AND (NOT itisvar) THEN
			      BEGIN
			        error (121) ; (* File must be VAR parameter *)
			        itisvar := true ;
			      END ;
			    IF nestproc = 0 THEN (* NOT DUMMY PARAMETERS *)
			      BEGIN
			        lc := lc + nbpar * bytesindword ; lcaux := lc ;
			        lp := next ;
			      END           (* NOT DUMMY PARAMETER *)
			    ELSE BEGIN
			        IF ctptr^.father_schema <> NIL THEN
				IF ctptr^.actual_parameter_list = NIL THEN (* BUILD ACTUAL PARAMETER LIST FOR SCHEMA *)
				  WITH ctptr^ DO
				    BEGIN
				      lctp1 := father_schema^.formal_parameter_list ;
				      WHILE lctp1 <> NIL DO
				        BEGIN
					create_vars_box (lctp2, lctp1^.name) ;
					lctp2^.vtype := lctp1^.vtype ;
					lctp2^.vkind := arraybound ;
					lctp2^.visset := true ;
					lctp2^.visreadonly := true ;
					IF actual_parameter_list = NIL THEN
					  actual_parameter_list := lctp2
					ELSE
					  lctp3^.nxtel := lctp2 ;
					lctp1 := lctp1^.nxtel ;
					lctp3 := lctp2 ;
				        END ;
				    END ;
			      END ;
			    FOR it := nbpar DOWNTO 1 DO
			      BEGIN
			        IF nestproc = 0 THEN
				BEGIN
				  lcaux := lcaux - bytesindword ;
				  lp@.vaddr := lcaux ;
				END ;
			        lp^.varparam := itisvar ;
			        IF ctptr^.father_schema <> NIL THEN
				IF ctptr^.actual_parameter_list = NIL THEN lp^.vdescaddr := -1 ; (* needs descriptor *)
			        lp := lp@.nxtel ;
			      END ;         (* FOR *)
			    lp := next ;
			    FOR it := nbpar DOWNTO 1 DO
			      BEGIN
			        lp@.vtype := ctptr ;
			        lp := lp@.nxtel ;
			      END ;
			  END ;             (* CORRECT TYPE *)
		        END ;
		    END ;
		insymbol ;
	        END ELSE
	        BEGIN                           (* FIRST ITEM OF THE'LIST IS ILLEGAL *)
		error (2) ;
		skip (10) ;                   (* ) *)
	        END ;
	    END ;                               (* NEITHER PROCEDURE NOR FUNCTION *)
	  IF no = 16 (* ; *) THEN
	    BEGIN
	      insymbol ;
	      IF no = 10 THEN
	        BEGIN
		rep := false ; error (2) ;    (*  TO DETECT  ;) *)
	        END ELSE rep := true ;
	    END ELSE rep := no IN [1, 43, 44, 45] ; (* ID,VAR,FUNC,PROC *)
	UNTIL NOT rep ;
	IF no # 10 THEN
	  BEGIN
	    error (4) ; skip (10) ;
	    IF no IN [37, 40, 41, 43, 44, 45] (* TYPE,LABEL,CONST,VAR,PROC,FUNC *) THEN
	      GOTO 1 ; (* LEAVES FORMPARM *)    (*  1 DEFINED IN  BODY *)
	  END ;                                 (* NO # 10 *)
                                                  (* LINKAGE REVERSE *)
	lp := next ; next := NIL ;
	WHILE lp # NIL DO
	  BEGIN
	    lp1 := lp ; lp := lp@.nxtel ;
	    lp1@.nxtel := next ; next := lp1 ;
	  END ;
                                                  (* La chaine est dans le bon ordre. Termine le remplissage *)
	lp := next ; locended := false ; loccounter := 0 ;
	locad := 0 ;
	WHILE NOT locended DO
	  IF lp = NIL THEN
	    locended := true ELSE
	    WITH lp^ DO
	      BEGIN
	        schema_parameter_count := 0 ;
	        IF klass = proc THEN
		BEGIN
		  IF prockind <> formal THEN
		    locended := true ELSE
		    BEGIN
		      loccounter := loccounter + 1 ;
$OPTIONS compile = trace $
		      printrec (lp) ;
$OPTIONS compile = true $
		      lp := nxtel ;
		    END (* FORMAL *) ;
		END (* PROC *) ELSE
		IF klass <> vars THEN
		  locended := true ELSE
		  BEGIN
		    IF vkind = formal THEN
		      BEGIN
		        locad := vaddr ; loccounter := loccounter + 1 ;
		        IF vdescaddr = -1 THEN
			BEGIN
			  vdescaddr := vaddr + (globnbpar * bytesindword) ;
			  IF vtype <> NIL THEN
			    WITH vtype^ DO
			      IF (father_schema <> NIL) AND (actual_parameter_list = NIL) THEN
                                                  (* THIS VARIABLE HAS A SCHEMA FOR TYPE.
                                                     BUILD ACTUAL PARAMETER LIST FOR THIS TYPE (PASSED IN DESCRIPTOR) *)
			        BEGIN
				lctp1 := father_schema^.formal_parameter_list ;
				WHILE lctp1 <> NIL DO
				  BEGIN
				    create_vars_box (lctp2, lctp1^.name) ;
				    lctp2^.vtype := lctp1^.vtype ;
				    lctp2^.vkind := arraybound ;
				    lctp2^.vaddr := locad + (globnbpar * bytesindword) ;
				    lctp2^.visset := true ;
				    lctp2^.visreadonly := true ;
				    lctp2^.vdispl := 8 (* TWO WORDS FOR MULTICS EXTENDED ARG DESC HEADER *)
				    + 4   (* ONE WORD FOR ACTUAL SIZE OF PASSED SCHEMA *)
				    + 4 * schema_parameter_count ; (* ONE WORD PER SCHEMA PARAMETER *) ;
				    schema_parameter_count := schema_parameter_count + 1 ;
				    IF actual_parameter_list = NIL THEN
				      actual_parameter_list := lctp2
				    ELSE
				      lctp3^.nxtel := lctp2 ;
				    lctp1 := lctp1^.nxtel ;
				    lctp3 := lctp2 ;
				  END ;
			        END ;
			END ;
$OPTIONS compile = trace $
		        printrec (lp) ;
$OPTIONS compile = true $
		        lp := nxtel ;
		      END (* FORMAL *) ELSE
		      IF vkind = arraybound THEN
		        BEGIN
			vaddr := locad + (globnbpar * bytesindword) ;
$OPTIONS compile = trace $
			printrec (lp) ;
$OPTIONS compile = true $
			lp := nxtel ;
		        END (* ARRAYBOUND *) ELSE
		        locended := true ;
		  END (* VARS *) ;
	        IF loccounter > globnbpar THEN
		locended := true ;            (* Security *)
	      END ;                             (* with LP^, LP <> nil, while not ENDED *)
	IF nestproc = 0 THEN
	  IF globdescriptors THEN
	    lc := lc + globnbpar * bytesindword ;
$OPTIONS compile = trace $
	IF decltrace > low THEN
	  BEGIN
	    write (mpcogout, ' @@@ FIN FORMPARM @@@ WITH    NESTPROC,LC,NEXT ', nestproc, lc,
	      ord (next)) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
        END (* FORMPARM *) ;


(* ***********************************  IMPORTPARTDECL < BODY    ************** *)

      PROCEDURE importpartdecl ;

(* C . Before call, $IMPORT has been read.
   . Caution:
   This declaration is allowed only for globals, in mode not standard.
   . The name of origin is not used.
   C *)

(* E Errors detected
   2 Identifier expected
   7 ":" expected
   19 String expected
   20 "," expected
   37   Invalid Multics string for imported item
   76 "$" expected
   77 $IMPORT must appear at global level after the program header
   78 $IMPORT ( EXPORT) not standard features
   100 Duplicate external name
   E *)

        LABEL
	10 ;                                    (* Procedure exit *)

        VAR
	wkexternpt : ptexternalitem ;
	errorfound : boolean ;
	locsegname,
	locentryname,
	locgenerator : alfaid ;
	locwantdescs : boolean ;
	loconlyone : boolean ;
	locerrfound : boolean ;
	locsamestring : integer ;

(* **************************** DECODESTRING < IMPORTPARTDECL    ***** *)

        PROCEDURE decodestring (VAR fsegname, fentryname, fgenerator : alfaid ;
	VAR fwantdescs, fonlyone, ferrfound : boolean) ;

(* Given the output of INSYMBOL  : BUFVAL filled on LONGCHAINE chars,
   this procedure try to find
   - A segment name followed if any by
   - "$" entryname
   - in all cases "(" genrator_name ")"

   If there is an entryname, only one element in the following list

   One exception : The word  'external_static'
   Obsolete 'external_statics' still supported..

   In all cases, for each entity, all caracters are allowed
   except  <    >    ?   $     *   (   )
   C *)

	VAR
	  index : integer ;
	  iderr : boolean ;
	  locerr : boolean ;
	  currch : char ;
	  locdescs : alfaid ;
	  stopch : char ;

	PROCEDURE getamulticsid (VAR fid : alfaid ; VAR fstopch : char ; low : boolean ; VAR ferr : boolean) ;

	  VAR
	    ended : boolean ;
	    locerr : boolean ;
	    loci : integer ;
	    locid : alfaid ;
	    it : integer ;

	  BEGIN
	    fstopch := chr (000) ;              (* Means ended *)
                                                  (* Skip leading spaces   *)

	    locerr := false ;
	    ended := NOT (currch IN [' ', chr (9) (* TAB *)]) ;
	    WHILE NOT ended DO
	      BEGIN
	        index := index + 1 ;
	        IF index > longchaine THEN
		BEGIN
		  locerr := true ; ended := true ; currch := chr (000) ;
		END ELSE
		BEGIN
		  currch := bufval [index] ;
		  ended := NOT (currch IN [' ', chr (9) (* TAB *)]) ;
		END ;
	      END ;

	    IF currch IN ['(', '$'] THEN
	      BEGIN
	        index := index + 1 ;
	        IF index > longchaine THEN
		locerr := true ELSE
		currch := bufval [index] ;
	      END ;
	    loci := 0 ; locid := '     ' ;
	    ended := NOT (currch IN [' ', chr (9) (* TAB *)]) ;
	    WHILE NOT ended DO
	      BEGIN
	        index := index + 1 ;
	        IF index > longchaine THEN
		BEGIN
		  locerr := true ; ended := true ;
		END ELSE
		BEGIN
		  currch := bufval [index] ;
		  ended := NOT (currch IN [' ', chr (9) (* TAB *)]) ;
		END ;
	      END ;

(* Now first char of identifier multics expected *)



	    ended := currch IN ['(', ')', '$', '<', '>', '*', '?'] ;

	    IF NOT locerr THEN
	      WHILE NOT ended DO
	        BEGIN

		loci := loci + 1 ;
		IF loci > maxident THEN
		  BEGIN
		    ended := true ; locerr := true ;
		  END ELSE
		  BEGIN
		    locid [loci] := currch ;
		    index := index + 1 ;
		    IF index > longchaine THEN
		      BEGIN
		        locerr := true ; currch := chr (0) ;
		      END ELSE
		      currch := bufval [index] ;
		    ended := currch IN ['(', ')', '$', '<', '>', '*', '?', ' ', chr (9) (* TAB *), chr (000)] ;
		  END ;
	        END (* while *) ;

(* Here stops on end caracter or end strings *)
	    IF currch IN [' ', chr (9) (* TAB *)] THEN
	      BEGIN
                                                  (* Skip until a good end caracter *)
	        REPEAT
		index := index + 1 ;
		IF index > longchaine THEN
		  currch := chr (000) ELSE
		  currch := bufval [index] ;
	        UNTIL NOT (currch IN [' ', chr (9) (* TAB *)]) ;
	      END (* Skip *) ;

	    fstopch := currch ;
	    fid := locid ;
	    ferr := locerr ;

	    IF low THEN
	      FOR it := 1 TO maxident DO
	        fid [it] := chr (majmin [ord (fid [it])]) ;

$OPTIONS compile = trace $
	    IF decltrace = high THEN
	      BEGIN
	        write (mpcogout, ' Fin de GET_A_MULTICS_ID avec FERR, FSTOPCH, INDEX:',
		ferr : 7, '%', fstopch, '%', index : 5,
		' et LOCI ', loci : 4) ;
	        nextline ;
	      END ;
$OPTIONS compile = true $



	  END (* GET_A_MULTICS_ID *) ;



	BEGIN                                   (* DECODESTRING *)

	  locdescs := blank ;
	  fsegname := blank ; fgenerator := blank ; fentryname := blank ;
	  fonlyone := false ; ferrfound := false ;
	  index := 0 ; currch := ' ' ; iderr := false ; locerr := false ;
	  fwantdescs := false ;

	  getamulticsid (fsegname, stopch, false, locerr) ;
	  IF locerr THEN
	    iderr := true ;
	  IF stopch = chr (000) THEN
	    BEGIN
	      IF (fsegname <> 'external_statics') AND (fsegname <> 'external_static') THEN
	        ferrfound := true
	      ELSE
	        iderr := false ;
	    END ELSE
	    BEGIN
	      IF stopch = '$' THEN
	        BEGIN
		fonlyone := true ;
		getamulticsid (fentryname, stopch, false, locerr) ;
		IF locerr THEN
		  iderr := true ;
		IF stopch <> '(' THEN
		  ferrfound := true ;
	        END ;

	      IF stopch = '(' THEN
	        BEGIN
		getamulticsid (fgenerator, stopch, true, locerr) ;
		IF locerr THEN
		  iderr := true ;
		IF stopch IN ['d', 'D'] THEN
		  BEGIN
		    getamulticsid (locdescs, stopch, true, locerr) ;
		    IF locdescs = 'descriptors' THEN
		      fwantdescs := true
		    ELSE ferrfound := true ;
		  END ;
		IF stopch <> ')' THEN
		  ferrfound := true ;
	        END ;

	      IF fgenerator = blank THEN
	        ferrfound := true ;

	    END ;                               (* STOPCH <> chr(000) *)

	  ferrfound := ferrfound OR iderr ;

$OPTIONS compile = trace $
	  IF decltrace = high THEN
	    BEGIN
	      write (mpcogout, ' Fin de DECODESTRING avec IDERR,FERRFOUND :', iderr : 7,
	        ferrfound : 7) ; nextline ;
	      write (mpcogout, '  ""       ""       avec FONLYONE =', fonlyone : 7) ;
	      nextline ;
	    END ;
$OPTIONS compile = true $


	END (* DECODESTRING *) ;


        BEGIN                                     (* IMPORTPARTDECL *)
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, ' @@@ Debut de IMPORTPARTDECL @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
	IF level <> 0 THEN
	  BEGIN
	    error (77) ; skiptochapter ; GOTO 10 ; (* Exit proc *)
	  END ;
	IF envstandard = stdpure THEN
	  error (78) ;

	insymbol ;
	IF (no <> 2) OR (cl <> 3) THEN
	  BEGIN
	    error (19) ; skipextd ([16, 55]) ;
	  END ;
	WHILE (no = 2) AND (cl = 3) DO          (* String *)
	  BEGIN

	    decodestring (locsegname, locentryname, locgenerator, locwantdescs,
	      loconlyone, locerrfound) ;
	    IF locerrfound THEN
	      error (37) ;

	    locsamestring := 0 ;

	    insymbol ;
	    IF no <> 19 (* : *) THEN
	      BEGIN
	        error (7) ; skipextd ([1, 16, 55]) ;
	      END ELSE
	      insymbol ;

	    IF no <> 1 (* Identifier *) THEN
	      BEGIN
	        error (2) ; skipextd ([16, 55, 2]) ;
	      END ;

	    WHILE no = 1 DO
	      BEGIN
                                                  (* Check if it is a new external identifier *)
	        errorfound := false ;
	        checkexternalitem (aval, wkexternpt) ;
	        IF wkexternpt <> NIL THEN
		BEGIN
                                                  (* External box found may be for a REMANENT file *)
		  wkexternpt^.extrfile2 := symbolfile ; wkexternpt^.extrline2 := symbolline ;
		  IF wkexternpt^.extkind = actual THEN
		    wkexternpt^.extkind := imported ELSE
		    BEGIN
		      error (100) ; wkexternpt := NIL ;
		    END ;
		END ELSE
		createexternalbox (aval, extnotresolved, imported, wkexternpt) ;
	        IF wkexternpt <> NIL THEN
		BEGIN

		  wkexternpt^.extsegname := locsegname ;
		  locsamestring := locsamestring + 1 ;
		  wkexternpt^.extgenerator := locgenerator ;
		  wkexternpt^.extwantdescs := locwantdescs ;
		  IF locentryname <> blank THEN
		    wkexternpt^.extentryname := locentryname ELSE
		    wkexternpt^.extentryname := aval ;
		END (* Create a box for a new external *) ;

	        insymbol ;
	        IF no = 15 (* , *) THEN
		BEGIN insymbol ;
		  IF no <> 1 THEN
		    BEGIN error (2) ; skipextd ([2, 16, 55]) ;
		    END
		END ELSE
		BEGIN
		  IF NOT (no IN [16, 55]) THEN
		    BEGIN error (20) ; errorfound := true ;
		    END ;
		END ;
	      END (* while NO=1 *) ;


(*            IF loconlyone THEN
   IF locsamestring <> 1 THEN
   error (37) ;     *)
	    IF no = 16 (* ; *) THEN
	      insymbol ELSE
	      IF no <> 55 (* $ *) THEN
	        BEGIN
		IF NOT errorfound THEN error (76) ;
		skipextd ([2, 55]) ;
		IF no = 16 (* ; *) THEN insymbol ;
	        END ;

	  END (* While NO=2, CL=3 *) ;

	IF no <> 55 THEN
	  BEGIN
	    error (76) ; skiptochapter ;
	  END ELSE
	  insymbol ;

$OPTIONS compile = trace $
	IF decltrace = high THEN
	  BEGIN
	    write (mpcogout, ' ** Boxes created in IMPORTPARTDECL are the following') ;
	    nextline ; wkexternpt := externallistheader ;
	    WHILE wkexternpt <> NIL DO
	      BEGIN
	        printexternalbox (wkexternpt) ;
	        wkexternpt := wkexternpt^.extnext ;
	      END ;
	  END ;
$OPTIONS compile = true $

10 :                                              (* Procedure exit *)

$OPTIONS compile = trace $
	IF decltrace = high THEN
	  BEGIN
	    write (mpcogout, ' @@@ Fin de IMPORTPARTDECL @@@ with NO,CL',
	      no : 4, cl : 4) ; nextline ;
	  END ;
$OPTIONS compile = true $

        END (* IMPORTPARTDECL *) ;

(* *******************************************    EXPORTPARTDECL(BODY) *)

      PROCEDURE exportpartdecl ;

(* C   Before call $EXPORT has been read
   C *)

(* E
   2  IDENTIFIER expected
   20  ',' expected
   76  $ expected
   78  $IMPORT et $EXPORT not allowed in STANDARD
   79  $EXPORT only in global part
   80 EXPORTED ITEM CANNOT HAVE SAME NAME THAN PROGRAM.
   100  duplicate external name
   447  externbox not nil for a box found
   E *)

        LABEL
	10 ;                                    (* exit procedure *)

        VAR
$OPTIONS compile = trace $
	currextpt : ptexternalitem ;
$OPTIONS compile = true $
	wkexternpt : ptexternalitem ;

        BEGIN                                     (* EXPORTPARTDECL *)
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, '@@@ begin of EXPORTPARTDECL @@@ with EXTERNALHEADER at ^',
	      ord (externallistheader)) ; nextline ;
	  END ;
	currextpt := externallistheader ;
$OPTIONS compile = true $

	IF level <> 0 THEN
	  BEGIN
	    error (79) ; skiptochapter ; GOTO 10 (* exit proc *) ;
	  END ;
	IF envstandard = stdpure THEN
	  error (78) ;
	insymbol ;

	init_fsb_trap_flag := true ;

	IF NOT (no IN [1, 55]) THEN             (* Ident, $  *)
	  BEGIN error (2) ; skipextd ([1]) ;
	  END ;
	WHILE no = 1 DO
	  BEGIN
	    checkexternalitem (aval, wkexternpt) ;
	    IF wkexternpt <> NIL THEN
	      BEGIN
                                                  (* External box found may be for a REMANENT file *)
	        wkexternpt^.extrfile2 := symbolfile ; wkexternpt^.extrline2 := symbolline ;
	        IF wkexternpt^.extkind = actual THEN
		wkexternpt^.extkind := exportable ELSE
		error (100) ;
	      END ELSE
	      BEGIN                             (* new external *)
	        createexternalbox (aval, extnotresolved, exportable, wkexternpt) ;
	      END (* new external *) ;
	    IF aval = progname THEN error (80) ;
	    insymbol ;
	    IF no = 15 (* , *) THEN
	      BEGIN
	        insymbol ;
	        IF no <> 1 THEN
		BEGIN error (2) ;
		  skipextd ([1]) ;
		END ;
	      END ELSE
	      IF no <> 55 THEN
	        BEGIN
		error (20) ;
		IF no <> 1 THEN skipextd ([1]) ;
	        END ;
	  END (* while NO=1 *) ;
	IF no <> 55 (*  $   *) THEN
	  BEGIN
	    error (76) ; skiptochapter ;
	  END ELSE
	  insymbol ;
$OPTIONS compile = trace $
	IF decltrace = high THEN
	  BEGIN
	    write (mpcogout, '* boxes created in EXPORTPARTDECL are the following:') ;
	    nextline ;
	    wkexternpt := externallistheader ;
	    WHILE wkexternpt <> currextpt DO
	      BEGIN
	        printexternalbox (wkexternpt) ;
	        wkexternpt := wkexternpt^.extnext ;
	      END ;
	  END ;
$OPTIONS compile = true $
10 :                                              (* exit proc *)
$OPTIONS compile = trace $
	IF decltrace = high THEN
	  BEGIN
	    write (mpcogout, '@@@ end of EXPORTPARTDECL @@@ with NO,CL:',
	      no : 4, cl : 4) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
        END (* EXPORTPARTDECL *) ;

(* *************************************  LABELPARTDECL < BODY ************** *)

      PROCEDURE labelpartdecl ;

(* C Compilation of   LABEL   lab1, lab2 ..... ;
   Called if the key-word LABEL (NO=40) was encountered.
   C *)

(* E  ERRORS DETECTED
   15  integer EXPECTED
   20  ',' EXPECTED
   166  MULTIDECLARED LABELS
   267  TOO MANY LABELS  (MAXLABS)
   306  LABEL MUST HAVE AT MOST 4 DIGITS
   E *)

        LABEL
	2 ;                                     (* Skip here if bideclared label *)
        VAR
	i : integer ;
	currlabbox : labelblockptr ;
        BEGIN                                     (* LABELPARTDECL *)
	insymbol ;
	WHILE (no = 2) AND (cl = 1) (* CSTE integer *) DO
	  BEGIN
                                                  (* SEARCH  for UNIQUE DECLARATION AT THIS LEVEL *)
	    FOR i := fstix TO clabix DO
	      IF labtab [i].labval = ival THEN
	        BEGIN
		error (166) ; GOTO 2 ;
	        END ;
                                                  (* CHECK  if AT MOST 4 DIGITS *)
	    IF ival > 9999 THEN error (306) ;
                                                  (* ALL OK  ENTER IT  IN LABTAB *)
	    IF clabix = maxlabs THEN
	      error (267) ELSE
	      BEGIN
	        clabix := clabix + 1 ;
	        WITH labtab [clabix] DO
		BEGIN
		  labval := ival ; lablev := level ;
		  labdef := 0 ; labexit := 0 ; labch1 := 0 ;
		  labbox := NIL ;
		  new (labbox) ;
		  IF labbox = NIL THEN heaperror ;
		  WITH labbox^ DO
		    BEGIN
		      number := ival ;
		      next := NIL ;
		      ref_allowed.ic_from := 0 ;
		      ref_allowed.ic_to := maxint ;
		      next_in_block := NIL ;
		      brother := currentnode ^.firstlabel ;
		      currentnode^.firstlabel := labbox ;
		      procnode := currentnode ;
		      dclfile := symbolfile ;
		      dclline := symbolline ;
		      deffile := 0 ; defline := 0 ;
		      new (references) ;
		      IF references = NIL THEN heaperror ;
		      WITH references^ DO
		        BEGIN refnbr := 0 ; nextref := NIL END ;
		      BEGIN
		        next := firstlabbox ^.next ;
		        firstlabbox^.next := labbox ;
		        currlabbox := firstlabbox ;
		        WHILE (next^.number < ival) DO
			BEGIN
			  currlabbox^.next := next ;
			  currlabbox := next ;
			  next := next^.next ;
			  currlabbox^.next := labbox ;
			END ;
		      END ;
		    END ;
		END ;
	      END ;
2 :                                               (* SKIP HERE if BIDECLARED *)
	    insymbol ;
	    IF no = 15 (* , *) THEN
	      BEGIN
	        insymbol ;
	        IF (no <> 2) OR (cl <> 1) THEN error (15) ;
	      END ELSE
	      IF no <> 16 THEN error (20) ;
	  END ;                                 (* while integer CSTE *)
	IF no = 16 (* ;     *) THEN
	  insymbol ;
$OPTIONS compile = trace $
	IF decltrace = high THEN
	  BEGIN
	    write (mpcogout, ' @ BODY.END LABEL PART @@@CLABIX,FSTIX ARE', clabix : 4, fstix : 4) ;
	    nextline ;
	  END ;
$OPTIONS compile = true $
        END (* LABELPARTDECL *) ;

(* ******************************  CONSTPARTDECL < BODY *********************** *)

      PROCEDURE constpartdecl ;

(* C
   Compile CONST CONSTID = constante; CONSTID= ....... ;
   C *)


(* E  ERRORS DETECTED
   HEAPERROR
   16  '=' EXPECTED
   101  Identifier declared twice
   130 Nil not allowed in standard
   226 : THIS IDENTIFIER HAS BEEN PREVIOUSLY REFERENCED AT SAME LEVEL
   E *)

        VAR
	constid : alfaid ;
	typcste, lp : ctp ;
	codcste : integer ;
	tnp : alfalistptr ;
	oldfile, oldline : integer ;

        BEGIN                                     (* CONSTPARTDECL *)
	forbidden_id_list := first_forbidden_id ;
	insymbol ;
	WHILE no = 1 (* ID *) DO                (*   LOOP  ON  < CST_ID = CSTE ; >  *)
	  BEGIN
	    srchrec (next) ;
	    IF ctptr # NIL THEN
	      BEGIN
	        IF listyes THEN nameisref (ctptr, symbolfile, symbolline) ;
	        error (101)
	      END ;
	    constid := aval ;
	    oldfile := symbolfile ; oldline := symbolline ;
	    tnp := forbidden_id_list ;
	    WHILE tnp <> first_forbidden_id DO
	      IF tnp^.name = constid THEN
	        BEGIN
		error (226) ;
		tnp := first_forbidden_id
	        END
	      ELSE tnp := tnp^.previous ;
	    insymbol ;
	    IF (no = 8) AND (cl = 6) (* = *) THEN
	      BEGIN
	        forbidden_id := constid ; check_id := true ;
	        insymbol
	      END ELSE error (16) ;
	    IF no = 36 (* NIL *) THEN
	      BEGIN
	        IF envstandard <> stdextend THEN
		error (130) ;
	        create_konst_box (lp, constid, wordconst) ;
	        WITH lp^ DO
		BEGIN
		  contype := nilptr ;
		  IF listyes THEN nameisref (nilptr, symbolfile, symbolline) ;
		END ;
	        insymbol ;
	      END (* NIL *) ELSE
	      BEGIN
	        inconst (codcste, typcste, next, true) ;
	        CASE codcste OF
		1 (* integer *), 4 (* CHAR *), 5 (* SCALAR *), 0 (* ERR *) :
		  BEGIN
		    create_konst_box (lp, constid, wordconst) ;
		    WITH lp^ DO
		      BEGIN
		        values := conint ;
		      END ;
		  END ;
		2 (* REAL *) :
		  BEGIN
		    create_konst_box (lp, constid, dwordconst) ;
		    WITH lp^ DO
		      BEGIN
		        valreel := conreel ;
		      END ;
		  END ;
		3 (* ALFA *) :
		  BEGIN
		    create_konst_box (lp, constid, alfaconst) ;
		    WITH lp^ DO
		      BEGIN
		        succ := lp ;          (* Means Not Used *)
		      END ;
		    crealfabox (lp) ;         (* Init ALFALONG ALFADEB *)
		  END (* ALFA *) ;
	        END (* CASE CODCSTE *) ;
	        lp^.contype := typcste ;
	      END (* not NIL *) ;
	    check_id := false ;
	    WITH lp^ DO
	      BEGIN
	        deffile := oldfile ; defline := oldline ;
	      END ;
$OPTIONS compile = trace $
	    printrec (lp) ;
$OPTIONS compile = true $
	    next := lp ;
	    findsemicolon ;
	  END ;                                 (* while NO=1 *)
$OPTIONS compile = trace $
	IF decltrace = high THEN
	  BEGIN
	    write (mpcogout, ' @ BODY.END CONST PART @@@') ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* CONSTPARTDECL *) ;

(* ********************************  TYPEPARTDECL < BODY ******************** *)

      PROCEDURE typepartdecl ;

(* C
   TYPE ( NO=37 ) has been read and tested before call
   C *)

(* E Errors DETECTED
   16 = expected
   93 Non resolved forward declared type identifier
   101 Identifier declared twice
   108 File not allowed here
   226 : THIS IDENTIFIER HAS BEEN PREVIOUSLY REFERENCED AT SAME LEVEL
   E *)

        VAR
	typid : alfaid ;
	lp, retpt : ctp ;
	oldfile, oldline : integer ;
	tl : integer ;
	i, j : integer ;
	tnp : alfalistptr ;

        BEGIN                                     (* TYPEPARTDECL   *)
	forbidden_id_list := first_forbidden_id ;
	insymbol ;
	WHILE no = 1 (* ID *) DO                (*    LOOP ON TYPE DECLARATION   TYPID = TYPE ; *)
	  BEGIN
	    srchrec (next) ;
	    IF ctptr <> NIL THEN
	      BEGIN
	        IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
	        error (101) ;
	      END ;
	    oldfile := symbolfile ; oldline := symbolline ;
	    typid := aval ;
	    tnp := forbidden_id_list ;
	    WHILE tnp <> first_forbidden_id DO
	      IF tnp^.name = typid THEN
	        BEGIN
		error (226) ;
		tnp := first_forbidden_id
	        END
	      ELSE tnp := tnp^.previous ;
	    insymbol ;
	    IF (no = 8) AND (cl = 6) (* = *) THEN
	      BEGIN
	        check_id := true ; forbidden_id := typid ;
	        insymbol
	      END ELSE error (16) ;

	    structispack := false ; err := false ; cadre := 0 ;
	    typedecl (tl, retpt) ;
	    check_id := false ;
	    IF (NOT err) AND (retpt <> NIL) THEN
	      IF retpt^.name <> blank THEN
	        BEGIN                           (* SYNONYMY *)
		create_types_box (lp, typid, aliastype, false) ;
		WITH lp^ DO
		  BEGIN
		    realtype := retpt ;
		  END ;
$OPTIONS compile = trace $
		printrec (lp) ;
$OPTIONS compile = true $
		next := lp ;
	        END (* ALIAS *) ELSE
	        BEGIN                           (* NEW TYPE *)
		WITH retpt^ DO
		  BEGIN
		    name := typid ; nxtel := next ;
		    deffile := oldfile ; defline := oldline ; alfathread := NIL ;
		    new (references) ; IF references = NIL THEN heaperror ;
		    WITH references^ DO
		      BEGIN
		        refnbr := 0 ; nextref := NIL ;
		      END ;
		  END (* with RETPT *) ;
		next := retpt ;
	        END (* NEW TYPE *) ;
$OPTIONS compile = trace $
	    IF decltrace = high THEN
	      BEGIN
	        write (mpcogout, ' ON TYPE DEFINED AT ', ord (retpt), ' NAME AND NXTEL ARE ',
		typid : 9, ord (next)) ;
	        nextline ;
	      END ;
$OPTIONS compile = true $
                                                  (* WAS THIS TYPE  ALREADY *)
                                                  (* IN PTLIST  ( @TYPID)  *)
	    FOR i := ptx - 1 DOWNTO 0 DO
	      WITH ptlist [i] DO
	        IF (hname = typid) AND (retpt <> NIL) THEN
		IF retpt^.form = files THEN
		  error (108) ELSE
		  BEGIN
		    pptr^.eltype := retpt ; ptx := ptx - 1 ; pptr^.domain := pptr ;
                                                  (* NOW  FREES TOP OF  ARRAY  PTLIST *)
		    hname := ptlist [ptx].hname ; pptr := ptlist [ptx].pptr ;
		    IF listyes THEN nameisref (next, rfil, rlin) ;
		  END ;                       (* WITH,FOR *)
	    findsemicolon ;                     (* SEARCH  ;  and READ NEXT  SYMBOL *)

	  END (* while NO=1 *) ;
	IF ptx > 0 THEN
	  FOR j := ptx - 1 DOWNTO 0 DO
	    WITH ptlist [j] DO
	      BEGIN
	        aval := hname ; search ;
	        IF ctptr <> NIL THEN
		BEGIN
		  IF ctptr^.klass = types THEN
		    IF ctptr^.form = aliastype THEN ctptr := ctptr^.realtype ;
		  WITH ctptr^ DO
		    IF (klass = types) AND (form <= records) THEN
		      BEGIN
		        pptr^.eltype := ctptr ; ptx := ptx - 1 ; pptr^.domain := pptr ;
		        hname := ptlist [ptx].hname ; pptr := ptlist [ptx].pptr ;
		      END
		    ELSE
		      BEGIN
		        error (96) ;
		        nextline ;
		        write (mpcogout, ' ****** ITEM POINTED BY TYPE ', pptr^.name, ' IS OF ILLEGAL TYPE.') ;
		        writeln (mpcogerr, ' ****** ITEM POINTED BY TYPE ', pptr^.name, ' IS OF ILLEGAL TYPE.') ;
		        nextline
		      END
		END
	      END ;
	IF ptx > 0 THEN
	  BEGIN
	    error (93) ;
	    FOR j := ptx - 1 DOWNTO 0 DO
	      BEGIN
	        nextline ;
	        write (mpcogout, ' ****** IDENTIFIER PENDING :', ptlist [j].hname) ;
	        writeln (mpcogerr, ' ****** IDENTIFIER PENDING :', ptlist [j].hname) ;
	        nextline ;
	      END ;
	    ptx := 0 ;
	  END (* PTX>0 *) ;
$OPTIONS compile = trace $
	IF decltrace = high THEN
	  BEGIN
	    write (mpcogout, ' ^ BODY.END TYPE PART ^^^') ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* TYPEPARTDECL *) ;

(* ******************************** VARPARTDECL < BODY************************* *)

      PROCEDURE varpartdecl ;

(* C .COMPILES  ALL VARIABLES DECLARATION PART FOR A GIVEN LEVEL
   .CREATES   'VARS'  BOXES
   ACTUAL   EXPORTABLE   or  IMPORTED
   .ENTER   FILES  IN  FILPTS
   C *)
(* E  ERRORS DETECTED
   2:  IDENTIFIER EXPECTED
   7:  ':' EXPECTED
   101 : Identifier declared twice
   258:  TOO  MANY FILES
   262:  STARTING POINT FOR VARIABLE TOO BIG IN SEGMENT
   264:  PLT DISP TOO HIGH
   E *)
        LABEL
	10 ;                                    (*  FIND SEMICOLON *)
        VAR
	varsize : integer ;
	recvarsize : integer ;
	locdata : integer ;
	lextpt : ptexternalitem ;
	liactual, liimport, liexport : integer ;
	locerr : boolean ;
	lp, vardeb, varpoint, vartype : ctp ;
        BEGIN                                     (* VARPARTDECL *)
$OPTIONS compile = trace $
	IF decltrace > none THEN
	  BEGIN
	    write (mpcogout, ' @@@ DEBUT VARPARTDECL @@@ with NEXT,    LC', ord (next), lc) ; nextline ;
	  END ;
$OPTIONS compile = true $
	locdata := 0 ;
	insymbol ;
	WHILE no = 1 (* ID. *) DO               (* LOOP  Ident_list: type [ ; Ident_list:type]* *)
	  BEGIN
	    liactual := 0 ; liexport := 0 ; liimport := 0 ;
                                                  (* Counters for actual, exportable, imported variables of same type *)
	    vardeb := next ;
	    REPEAT                              (* SECONDARY  LOOP  ON   A,B,C...  *)
	      locerr := false ;
	      srchrec (next) ;
	      IF ctptr <> NIL THEN
	        BEGIN
		IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		error (101) ; locerr := true ;
	        END ;
	      create_vars_box (lp, aval) ;
	      WITH lp^ DO
	        BEGIN
		vaddr := -1 ;
		lextpt := NIL ;
		IF (level = 0) AND NOT locerr THEN
		  BEGIN
		    checkexternalitem (aval, lextpt) ;
		    IF lextpt = NIL THEN
		      vkind := actual ELSE
		      BEGIN
		        IF symbolmap THEN
			BEGIN
			  nameisref (lp, lextpt^.extrfile1, lextpt^.extrline1) ;
			  IF lextpt^.extrline2 <> 0 THEN
			    nameisref (lp, lextpt^.extrfile2, lextpt^.extrline2) ;
			END ;
		        IF lextpt^.extitemtype = remanentfile THEN
			BEGIN
			  vfilelocation := permanentfile ;
			  lextpt^.extdecl := lp ;
			END ;
		        vkind := lextpt^.extkind ;
		      END ;
		  END ELSE
		  vkind := actual ;
		vptextitem := lextpt ;
	        END ;

	      IF lp^.vkind = actual THEN liactual := liactual + 1 ELSE
	        BEGIN
		lextpt^.extdecl := lp ;
		IF lp^.vkind = exportable THEN
		  BEGIN
		    liexport := liexport + 1 ;
		    lextpt^.extitemtype := exportvar ;
		  END ELSE
		  BEGIN
		    liimport := liimport + 1 ;
		    lextpt^.extitemtype := importvar ;
		    lp^.visset := true ;
		  END ;
	        END ;
	      next := lp ;
	      insymbol ;                        (* EXPECT , or : *)
	      IF no = 15 (* , *) THEN
	        BEGIN
		insymbol ;
		IF no <> 1 (* ID. *) THEN
		  BEGIN
		    error (2) ; skip (1) ;
		  END ;
	        END ELSE
	        IF no <> 19 (* : *) THEN
		BEGIN
		  error (7) ;
		END ;
	    UNTIL (no <> 1) ;
	    varpoint := next ;

(* NOW  COMES TYPE FOR THESE VARIABLES *)

	    IF no = 19 (* : *) THEN
	      insymbol ELSE error (7) ;
	    err := false ; cadre := 0 ; structispack := false ;
	    typedecl (varsize, vartype) ;
	    IF err OR (vartype = NIL) THEN
	      GOTO 10 ;


(* ADJUST SIZE  FUNCTION OF CADRE *)
(* A VARIABLE STARTS AT LESS ON A WORD BOUNDARY *)
	    cadre := sup (vartype^.cadrage, bytesinword) ;
	    recvarsize := recadre (vartype^.size, cadre) ;
                                                  (* Adjust BOUNDARIES *)
	    IF liactual > 0 THEN
	      BEGIN
	        lc := recadre (lc, cadre) + liactual * recvarsize ;
	        locdata := lc ;
	      END ;
	    IF locdata - recvarsize > twoto17 - 1 THEN
	      BEGIN
	        error (260) ;
	        recvarsize := bytesindword ;
	        lc := 0 ;
	        locdata := liactual * bytesindword ;
	      END ;

	    IF varpoint <> vardeb THEN
	      REPEAT
	        WITH varpoint^ DO
		BEGIN
		  vtype := vartype ;
		  CASE vkind OF
		    actual :
		      BEGIN
		        locdata := locdata - recvarsize ;
		        vaddr := locdata ;
		      END ;
		    imported :
		      BEGIN
		        vptextitem^.extlong := vartype^.size ;
		      END ;
		    exportable :
		      BEGIN
		        vptextitem^.extlong := vartype^.size ;
		      END ;
		  END (* case VKIND *) ;
		  IF existfileintype (vtype) THEN
		    BEGIN
		      IF filtop = fillimit THEN
		        error (258) ELSE
		        BEGIN
			filtop := filtop + 1 ;
			filpts [filtop] := varpoint ;
			IF level = 0 THEN
			  BEGIN
			    IF varpoint^.vfilelocation <> permanentfile THEN
			      varpoint^.vfilelocation := workfile ;
			  END ELSE
			  varpoint^.vfilelocation := localfile ;
		        END ;
		    END ;
$OPTIONS compile = trace $
		  printrec (varpoint) ;
$OPTIONS compile = true $
		  varpoint := nxtel ;
		END (* WITH VARPOINT *) ;
	      UNTIL varpoint = vardeb ;
10 :
	    findsemicolon ;
	  END ;                                 (* while NO=1   MAIN LOOP *)
$OPTIONS compile = trace $
	IF decltrace > low THEN
	  BEGIN
	    write (mpcogout, ' @@@ FIN VARPARTDECL  @@@ with LC', lc) ; nextline ;
	  END ;
$OPTIONS compile = true $
        END (* VARPARTDECL *) ;





(* ************************************************  MAIN de BODY    ********** *)
      BEGIN                                       (* BODY *)
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT BODY @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        environt := data ; saved_level := level ;
        declarationpart := true ;
        fstix := clabix + 1 ;
                                                  (* LABELS DECLARED AT THIS LEVEL ARE FROM FSTIX to CLABIX *)

        currentnode^.nextproc := lastproc ;
        lastproc := currentnode ;
        currentnode ^.codebegin := statnbr * 2 ;
        IF level = 1 (* Procedure globale *) THEN
	IF surrptr <> NIL THEN
	  exportablecode := exportscode ;


1 :                                               (* BEGINNING OF DECLARATION PART *)
        level := saved_level ;                    (* FOR SECURITY, IN CASE OF ERROR *)
$OPTIONS compile = trace $
        IF decltrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ LABEL 1 IN BODY @@@') ; nextline ;
	END ;
$OPTIONS compile = true $

        push_lab_pdl ;

        IF no = 52 (* $IMPORT *) THEN
	importpartdecl ;

        IF no = 53 (* $EXPORT *) THEN
	exportpartdecl ;

        IF no = 40 (* LABEL *) THEN
	labelpartdecl ;

        IF no = 41 (* CONST *) THEN
	constpartdecl ;

        pendingtypeallowed := true ;
        IF no = 37 (* TYPE *) THEN
	typepartdecl ;
        pendingtypeallowed := false ;

        IF level <> 0 THEN
	filev [level] := filtop + 1 ;

(*  THE FILES DECLARED AT LEVEL N  ARE IN ARRAY FILPTS FROM
   FILEV[N]  to  FILTOP .    *)

        IF no = 43 (* VAR *) THEN
	varpartdecl ;
        IF level = 0 THEN
	BEGIN                                   (* GLOBAL LEVEL *)
	  workextp := externallistheader ;
	  WHILE workextp <> NIL DO
	    BEGIN
	      IF workextp^.extdecl = NIL THEN
	        IF workextp^.extitemtype IN [extnotresolved, remanentfile] THEN
		exportscode := true ;
	      workextp := workextp^.extnext ;
	    END ;
	  IF lc > maxglobsize THEN error (214) ELSE
	    valuedecl ;
	END ELSE
	BEGIN                                   (* NOT GLOBAL LEVEL *)
	  IF no = 54 (* VALUE *) THEN
	    BEGIN
	      error (65) ;
	      REPEAT
	        skip (46) ;                     (* NOT ASSIGNED *)
	      UNTIL (no # 16) ;                 (* ; *)
	    END ;
	END ;
$OPTIONS compile = trace $
        IF decltrace = high THEN
	BEGIN
	  write (mpcogout, ' @ BODY.END VAR PART @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
        IF no IN [44, 45] (* FUNCTION,PROCEDURE *) THEN
	BEGIN
	  REPEAT
	    IF mapswitch THEN
	      BEGIN
	        hdrfile := symbolfile ;
	        hdrindex := symbolindex ;
	        hdrline := symbolline ;
	      END ;
	    lno := no ; oldlev := level ;
	    lextpt := NIL ;
	    IF level < maxlevel THEN
	      level := level + 1 ELSE
	      error (251) ;
	    insymbol ;
	    IF no # 1 THEN                      (* NOT ID. *)
	      BEGIN
	        error (2) ; level := oldlev ;
	        GOTO 1 ;                        (* BEGINNING OF BODY  *)
	      END ;
	    locerr := false ;
	    srchrec (next) ;
	    IF ctptr # NIL (* ID. FOUND *) THEN
	      BEGIN
	        IF ctptr@.klass # proc (* FORWARDS ? *) THEN
		BEGIN
		  IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		  error (101) ; ctptr := NIL ; locerr := true ;
		END ;
	      END ;
	    IF ctptr = NIL THEN                 (* UNDECLARED  PROC OR FUNCT.  *)
	      BEGIN
	        create_proc_box (procptr, aval) ;
	        WITH procptr^ DO
		BEGIN
		  proctype := procptr ;       (* Default means not a fucntion *)
		  IF (oldlev = 0) AND NOT locerr THEN
		    BEGIN
		      checkexternalitem (aval, lextpt) ;
		      IF lextpt = NIL THEN
		        lprockind := actual ELSE
		        BEGIN
			lprockind := lextpt^.extkind ;
		        END ;
		    END ELSE
		    lprockind := actual ;
		  IF lextpt <> NIL THEN
		    BEGIN
		      IF symbolmap THEN
		        BEGIN
			nameisref (procptr, lextpt^.extrfile1, lextpt^.extrline1) ;
			IF lextpt^.extrline2 <> 0 THEN
			  nameisref (procptr, lextpt^.extrfile2, lextpt^.extrline2) ;
		        END ;
		      lextpt^.extdecl := procptr ;
		      IF lprockind = imported THEN
		        WITH lextpt^ DO
			BEGIN
			  pwantdescs := extwantdescs ;
			  IF (extgenerator = 'pl1') OR
			    (extgenerator = 'pl/i') OR
			    (extgenerator = 'pl/1') THEN extgenerator := 'PL/1'
			  ELSE IF (extgenerator = 'fortran') THEN extgenerator := 'FORTRAN'
			    ELSE IF (extgenerator = 'pascal') THEN extgenerator := 'Pascal'
			      ELSE IF (extgenerator = 'cobol') THEN extgenerator := 'COBOL'
			        ELSE IF (extgenerator = 'alm') THEN extgenerator := 'ALM'
				ELSE extgenerator := 'Unknown' ;
			  pwantspl1descriptors :=
			    (extgenerator = 'PL/1') OR (extgenerator = 'FORTRAN') OR (extgenerator = 'COBOL') ;
			  lextpt^.extitemtype := importproc END ELSE
		        lextpt^.extitemtype := exportproc ;
		    END ;

		  prockind := lprockind ;
		  proclevel := oldlev ;       (* LEVEL -1 *)
		  procextitem := lextpt ;
		END ;
                                                  (*  NOW  BEGINS  NEW  LEVEL *)
	        display [top].fname := procptr ;
	        next := NIL ;
	        insymbol ;                      (*   PARAMETER LIST BEGINS, IF ANY *)
	        oldlc := lc ;
	        lc := pascdebstacklocal ; longparam := 0 ;
	        IF lno = 44 (* FUNCTION *) THEN
		BEGIN
		  globnbpar := 1 ;            (* ONE FOR FUNCTION RESULT *)
		  IF no = 9 (* ( *) THEN
		    BEGIN
		      insymbol ; nestproc := 0 ; formparm ;
		      IF no = 10 (* ) *) THEN
		        insymbol ;
		    END ;
		  lc := lc + bytesindword ;   (* FUNCTION RESULT "ITS" *)
		  longparam := lc - pascdebstacklocal ;
                                                  (* TYPE OF FUNCTION *)
		  IF no = 19 (* : *) THEN
		    insymbol ELSE error (7) ;
                                                  (* MUST BE A TYPE IDENTIFIER < POWER *)
		  IF no # 1 (* ID *) THEN
		    BEGIN
		      error (123) ; procptr@.proctype := NIL ; skip (46) ;
		    END ELSE
		    BEGIN
		      search ;
		      IF ctptr # NIL THEN
		        BEGIN
			IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
			IF ctptr@.klass # types THEN
			  BEGIN error (103) ; ctptr := NIL ;
			  END ELSE
			  BEGIN
			    IF ctptr@.form = aliastype THEN ctptr := ctptr@.realtype ;
			    IF ctptr@.form >= power THEN
			      BEGIN error (120) ; ctptr := NIL ;
			      END ;
			  END ;
		        END ELSE error (104) ;
		      procptr@.proctype := ctptr ;
		      insymbol ;
		    END (* TYPID RESULT FUNCTION *) ;
		END (* LNO=44 FUNCTION *) ELSE
		BEGIN                         (* PROCEDURE *)
		  globnbpar := 0 ;
		  IF no = 9 (* ( *) THEN
		    BEGIN
		      insymbol ; nestproc := 0 ; formparm ;
		      longparam := lc - pascdebstacklocal ;
		      IF no = 10 (* ) *) THEN
		        insymbol ;
		    END (* NO=9 *) ;
		END ;                         (* PROCEDURE *)
	        procptr@.segsize := longparam ;
	        hdrlength := symbolindex - hdrindex ;
	        IF no = 16 (* ; *) THEN
		insymbol ELSE
		BEGIN
		  error (14) ; skip (16) ;    (* ; *)
		END ;
	        procptr@.formals := next ;      (* NIL  OR FIRST PARAM *)
	        typofproc := standdef ;
	        procptr@.nbparproc := globnbpar ;
	        procptr^.phasdescriptor := globdescriptors ;
	        IF no = 1 (* ID *) THEN
		BEGIN
		  IF aval = usednames [4] THEN
		    BEGIN
		      typofproc := forwdef ;
		      WITH procptr@ DO
		        BEGIN
			nameisref (procptr, deffile, defline) ;
			deffile := 0 ; defline := 0 ;
		        END ;
		    END ELSE
		    IF aval = usednames [5] THEN
		      typofproc := extdef ELSE
		      BEGIN
		        error (88) ; typofproc := extdef ;
		      END ;
		  next := procptr ;
		  IF NOT (typofproc IN [standdef, forwdef]) THEN
		    IF procptr@.prockind # imported THEN
		      BEGIN error (87) ; procptr@.prockind := imported ; END ;
		  procptr@.procdef := typofproc ; procptr@.procinscope := false ;
		  insymbol ;
		  IF no <> 16 THEN
		    BEGIN
		      error (14) ; skip (16) ;
		    END ;
		END ;
	        WITH procptr@ DO
		BEGIN
		  procaddr := lkc ;
		  lkc := lkc + bytesindword ;
		  IF pwantdescs THEN
		    BEGIN
		      lkc := lkc + bytesindword ; (* PLACE FOR LINK TO INFO FOR TRAP PROC *)
		      oldlc := recadre (oldlc, bytesindword) ;
		      pdescsaddrplace := oldlc ; (* PLACE FOR VECTOR OF PTRS TO ARG DESCRIPTORS *)
		      oldlc := oldlc + bytesindword * nbparproc ;
		      IF extcalltrapplace = 0 THEN
		        BEGIN
			extcalltrapplace := lkc ;
			lkc := lkc + bytesindword ;
			genentrypoint (0, extcalltrapplace, 2, 'pascal_ext_call_trap_proc_',
			  'pascal_ext_call_trap_proc_', false, entrylength, locreturncode) ;
			IF locreturncode <> 0 THEN error (505) ;
		        END ;
		    END ;
		END ;
$OPTIONS compile = trace $
	        printrec (procptr) ;
$OPTIONS compile = true $
	        IF procptr@.procdef = standdef THEN
		BEGIN
                                                  (* COMPILE   BODY  OF  THIS  PROC *)
		  top := level + 1 ;
		  WITH display [top] DO
		    BEGIN
		      fname := next ; occur := block ;
		    END ;
		  create_dummyclass_box (lfirstentry, blank) ;
		  new (np, procblock) ;
		  WITH np^ DO
		    BEGIN
		      father := currentnode ;
		      brother := currentnode^.son ;
		      currentnode^.son := np ;
		      son := NIL ;
		      nextproc := NIL ;
		      blockbox := procptr ;
		      procptr^.procisactive := true ;
		      codebegin := 0 ;
		      codeend := 0 ;
		      structureplace := 0 ;
		      first := NIL ;
		      firstlabel := NIL ;
		      blocktp := procblock ;
		      hdrlin := hdrline ;
		      hdrfil := hdrfile ;
		      hdrind := hdrindex ;
		      hdrlen := hdrlength ;
		    END ;
		  currentnode := np ;
                                                  (* ***************************** *)
		  body (procptr, lfirstentry) ;
                                                  (* ************************** *)
		  currentnode^.codeend := statnbr * 2 ;
		  procptr^.procisactive := false ;
		  currentnode := currentnode^.father ;
		END                           (* COMPILE  BODY OF A NEW PROC *)
	      END (* THIS WAS A NEW PROC *) ELSE
	      BEGIN                             (* ALREADY DECLARED *)
	        WITH ctptr@ DO
		IF procdef <> forwdef THEN
		  BEGIN
		    IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		    error (101) ;
		  END
		ELSE
		  BEGIN
		    IF ((lno = 45) (* PROC *) AND
		      (ctptr^.proctype <> ctptr)) OR
		      ((lno = 44) AND (ctptr^.proctype = ctptr)) THEN
		      error (116) ;
		    deffile := symbolfile ; defline := symbolline
		  END ;
	        insymbol ;
	        IF no = 9 (* ( *) THEN          (* IGNORE  PARMLIST *)
		BEGIN
		  error (119) ;
		  REPEAT
		    skip (10) ;
		    IF no IN [16, 40, 37, 41, 43, 44, 45] THEN insymbol ;
                                                  (* ;LABEL CONST TYPE VAR FUNC PROC *)
		  UNTIL NOT (no IN [1, 16, 40, 37, 41, 43, 44, 45]) ;
		  IF no = 10 THEN
		    insymbol ELSE error (4) ;
		  hdrlength := symbolindex - hdrindex ;
		END (* IGNORE  PARMLIST *) ;
	        IF no = 15 (* , *) THEN skip (16) ;
	        IF no = 16 THEN
		insymbol ELSE error (14) ;
	        IF (no = 1) THEN
		BEGIN insymbol ; error (88) ; findsemicolon ; END ELSE
		BEGIN                         (* COMPILE BODY OF AN OLD DEFINED PROC *)
		  procptr := ctptr ;
		  WITH procptr@ DO
		    BEGIN
		      lc := segsize ; procdef := standdef ; (* NO MORE FORWARD *)
		      lc := lc + pascdebstacklocal ; procinscope := true ;
		      next := formals ;
		    END ;
		  top := level + 1 ;
		  WITH display [top] DO
		    BEGIN
		      fname := next ; occur := block ;
		    END ;
$OPTIONS compile = trace $
		  printrec (procptr) ;
$OPTIONS compile = true $
		  create_dummyclass_box (lfirstentry, blank) ;
		  new (np, procblock) ;
		  WITH np^ DO
		    BEGIN
		      father := currentnode ;
		      brother := currentnode^.son ;
		      currentnode^.son := np ;
		      son := NIL ;
		      nextproc := NIL ;
		      blockbox := procptr ;
		      procptr^.procisactive := true ;
		      codebegin := 0 ;
		      codeend := 0 ;
		      structureplace := 0 ;
		      first := NIL ;
		      firstlabel := NIL ;
		      blocktp := procblock ;
		      hdrlin := hdrline ;
		      hdrfil := hdrfile ;
		      hdrind := hdrindex ;
		      hdrlen := hdrlength ;
		    END ;
		  currentnode := np ;
                                                  (* **************************** *)
		  body (procptr, lfirstentry) ;
                                                  (* ************************ *)
		  currentnode^.codeend := statnbr * 2 ;
		  procptr^.procisactive := false ;
		  currentnode := currentnode^.father ;
		END (* BODY OF AN OLD PROC *) ;
	      END (* ALREADY DECLARED *) ;
	    lc := oldlc ;
	    level := oldlev ;
	    findsemicolon ;
	  UNTIL NOT (no IN [44, 45]) ;          (* FUNCT , PROC *)
$OPTIONS compile = trace $
	  IF decltrace = high THEN
	    BEGIN
	      write (mpcogout, ' @ BODY.END PROC/FUNC,PART @@@') ; nextline ;
	    END ;
$OPTIONS compile = true $
	END (* FUNCTION OR PROCEDURE *) ;
        display [top].fname := next ;
        IF level = 0 THEN
	staticswordcount := (lc + bytesinword - 1) DIV bytesinword ;

        IF NOT (no = 21) THEN
	BEGIN                                   (*  BEGIN EXPECTED AND NOT FOUND *)
	  error (17) ; skip (46) ;
	  WHILE no IN [16, 22] DO               (* ;  END *)
	    BEGIN
	      insymbol ; skip (46) ;
	    END ;
	  IF no IN [37, 40, 41, 43, 44, 45] THEN GOTO 1 (* BODY  BEGINNING *)
	END (*  BEGIN NOT FOUND *) ;
                                                  (* STATEMENT PART *)
        declarationpart := false ;
        enterbody ;
        compstat ;
        leavebody ;
        declarationpart := true ;
        currentnode^.first := display [top].fname ;
        IF surrptr # NIL THEN
	BEGIN
	  surrptr@.segsize := lc ;
	  create_dummyclass_box (lp, blank) ;
	  IF lp > maxctp THEN maxctp := lp ;
	  firstentry := NIL ;
	  top := level ;
	  next := display [top].fname ;
	END (* NOT NIL *) ;

        pop_lab_pdl ;

$OPTIONS compile = trace $
        IF decltrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ END BODY   @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* BODY *) ;

(* END OF THE DECLARE MODULE ********************************************** *) BEGIN
    END.
