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


(* ******************************************************************************************
   *                                                                                          *
   *                            MULTICS  PASCAL  COMPILER                                     *
   *                            _________________________                                     *
   *                                                                                          *
   *  This compiler is the result of a team work .                                            *
   *  Three people  Jean.Michel Athane   ( C.I.C.G   ),  Jean.Pierre Fauche (CRISS-IREP)      *
   *                Bernard Huc    (C.S.L./C.T.G. )                                           *
   *     during one year,(some nights and week-ends ...) worked together  for this            *
   *     result. If you are not happy with this compiler,you can (must) see one of these      *
   *     three people. For this reason, their addresses are not given here.                   *
   *  We hope anyway ,you don't have any trouble with this fine,sophisticated compiler.       *
   *                                                                                          *
   *  Some of data structures used in some places are derived from the original CDC compiler  *
   *     issued in Zurich in 1972 by N. Wirth and his team.                                   *
   *  The experience of CRISS on Pascal compilers is also included here, as well as the ideas *
   *     taken (judiciously chosen) in the SFER Pascal compiler.                              *
   *  It is obvious that the authors have developped here their personal ideas.            *
   *  Sorry for this....                                                                   *
   *                                                                                          *
   *          at GRENOBLE (FRANCE)  on August,28th  1980.                                     *
   *            sincerely yours,                                                              *
   *                           the authors                                                    *
   *                                                                                          *
   ****************************************************************************************** *)
$OPTIONS page $

$OPTIONS switch trace := true ; switch security := true ; t - $
  PROGRAM racine (mpcogerr, mpcogin, mpcogout) ;
    $IMPORT
                                                  (* LIST  OF IMPORTED  PROCEDURES *)
      'UNIQUE (pascal)' :
        displaysymbols,
        heaperror,
        initclasse,
        initialise,
        progdecl,
        prterrmeans,
        statistiques ;
      'DECLARE (pascal)' :
        body ;
      'CONTEXTTABLE (pascal)' :
        create_dummyclass_box,
        create_vars_box ;
      'STATE (pascal)' :
        freeallregisters ;
      'EXPR (pascal)' :
        expression ;
      'MODATTR (pascal)' :
        initattrvarbl ;
      'GENERE (pascal)' :
        genlongprofileref,
        genprofileref,
        inser ;
      'optimized_procedures (alm)' :
        search,
        srchrec ;
      'pascal (pl1)' :
        listhead ;
                                                  (* LIST OF IMPORTED VARIABLES    *)
      'STATE (pascal)' :
        asscheck,
        divcheck,
        errorctp,
        gattr,
        inputctp,
        inxcheck,
        outputctp,
        stattrace ;
      'DECLARE (pascal)' :
        building_from_schema,
        decltrace,
        externallistheader,
        filpts,
        filtop,
        forbidden_id_list,
        hdrfile,
        hdrindex,
        hdrlength,
        hdrline,
        lc,
        lkc,
        symbtabl ;
      'GENERE (pascal)' :
        fichinter,
        genetrace,
        ic,
        illegal_generation,
        outcode,
        writecode ;
                                                  (* FROM PL/1 *)
      'pascal_build_object$pascal_build_object (pl1)' : buildobject ;
      'pascal_sources_management_$init_source (pl1)' : initsource ;
      'pascal_sources_management_$begin_source (pl1)' : beginsource ;
      'pascal_sources_management_$end_source (pl1)' : endsource ;
      'pascal_sources_management_$display_sources (pl1)' : displaysources ;
      'pascal_statement_map_$return_map_ptr (pl1)' : getmapptr ;
      'pascal_statement_map_$return_prof_ptr (pl1)' : getprofptr ;
      'pascal_convert_real$pascal_convert_real (pl1)' : convertreal ;
      'pascal_gen_io_ref_ (pl1)' : geninput, genoutput, genentree, gensortie, generror, generreur
      $

    $EXPORT

      alfaptr,
      anytrace,
      aval,
      boolptr,
      boxheader,
      bufval,
      ch8flag,
      charptr,
      chnix,
      cl,
      codelist,
      conint,
      conreel,
      crealfabox,
      ctptr,
      currentnode,
      declarationpart,
      display,
      disx,
      environt,
      envstandard,
      errcl,
      error,
      errorflag,
      errorsfound,
      errtotal,
      exportablecode,
      extcalltrapplace,
      check_id,
      fastoperator,
      firstcond,
      forbidden_id,
      init_fsb_trap_flag,
      init_fsb_trap_info_place,
      init_fsb_trap_links_place,
      init_fsb_trap_number_of_files,
      generrorlink,
      geninputlink,
      genoutputlink,
      inconst,
      initracine,
      inputflag,
      inserundlab,
      insymbol,
      interactive,
      intptr,
      iowarnings,
      ival,
      lamptr,
      lastproc,
      level,
      liglues,
      linkswordcount,
      listyes,
      longchaine,
      longprofile,
      longstring,
      majmin,
      mapswitch,
      maxstring_ptr,
      mpcogerr,
      mpcogin,
      mpcogout,
      nameisref,
      next,
      nextline,
      nextpage,
      nilptr,
      no,
      no_compilation_warnings,
      outputflag,
      pageserrors,
      pascalfrench,
      pnumptr,
      poweroftwo,
      profilewordcount,
      profptr,
      progname,
      programnode,
      realptr,
      recadre,
      returnstop,
      rval,
      selectivetable,
      skip,
      skipextd,
      skiptochapter,
      sourceindex,
      sourcenbr,
      startic,
      statement_begins,
      statement_ends,
      staticswordcount,
      statnbr,
      string_ptr,
      sttfile,
      sttindex,
      sttline,
      sup,
      symbolfile,
      symbolindex,
      symbolline,
      symbolmap,
      textfilectp,
      top,
      undecptr,
      undlab,
      usednames,
      version,
      warning,
      xc,
      xrefneed $



    LABEL 100 ;                                   (* END OF THE COMPILATION *)





$OPTIONS page $

$INCLUDE 'CONSTTYPE' $



$OPTIONS page $

    VAR
                                                  (* REDEFINE IMPORTED VARIABLES  *)
                                                  (* FROM STATE *)
      asscheck : boolean ;
      divcheck : boolean ;
      errorctp : ctp ;
      gattr : attr ;
      inputctp : ctp ;
      inxcheck : boolean ;
      outputctp : ctp ;
      stattrace : levtrace ;
                                                  (* FROM DECLARE *)
      building_from_schema : schema_status ;
      decltrace : levtrace ;
      externallistheader : ptexternalitem ;
      forbidden_id_list : alfalistptr ;
      hdrfile : integer ;
      hdrindex : integer ;
      hdrlength : integer ;
      hdrline : integer ;
      filpts : ARRAY [0..fillimit] OF ctp ;
      filtop : integer ;
      lc : integer ;
      lkc : integer ;
      symbtabl : boolean ;
                                                  (* FROM GENERE  *)
      fichinter : ^binartype ;
      genetrace : levtrace ;
      ic : integer ;
      illegal_generation : boolean ;
      outcode : boolean ;
      writecode : boolean ;

(* DEFINE  EXPORTABLE  VARIABLES *)
      alfaptr : ctp ;                             (* CHAR'S STRINGS TYPE POINTER *)
      anytrace : levtrace ;
      aval : alfaid ;                             (* OUTPUT OF  INSYM BOL *)
      boolptr : ctp ;                             (* BOOLEAN TYPE POINTER *)
      boxheader : PACKED ARRAY [1..120] OF char ; (* USED TO PRINT *)
                                                  (* BOXES IN TRACE ENVIRONEMENT   *)
      bufval : ARRAY [1..maxval] OF char ;        (* OUTPUT OF INSYMBOL *)
      ch8flag : boolean ;
      check_id : boolean ;
      charptr : ctp ;                             (* CHAR TYPE POINTER *)
      chnix : integer ;                           (* POINTS THE HEAD OF *)
                                                  (* FREE LIST IN UNDLAB *)
      cl : integer ;                              (* OUTPUT OF INSYMBOL *)
      codelist : boolean ;                        (* TRUE IF "-list" OPTION *)
      conint : integer ;                          (* OUTPUT OF INSYMBOL *)
      conreel : real ;                            (*   "      "    "   *)
      ctptr : ctp ;                               (* OUTPUT  OF   SRCHREC  AND  SEARCH  *)
      currentnode : blocknodeptr ;                (* PTR TO CURRENT PROC NODE *)
      declarationpart : boolean ;
      display : ARRAY [0..displimit] OF recidscope ;

(*  EACH ENTRY (0..TOP)   IS   THE  BEGINNING  OF A LIST  OF
   IDENTIFIERS  IN CONTEXTTABLE.
   EACH LIST  CORRESPONDS
   EITHER   AT   A LEVEL   ( PROC  NESTED)
   EITHER   AT   A SCOPE DUE TO A WITH
   THE  ORDER  OF SCANNING  GIVES   THE  PASCAL  SCOPE  *)
      disx : integer ;                            (* FIRST FREE ENTRY IN DISPLAY *)
                                                  (* TO  DECIDE   BETWEEN   DIGIT.. *)
                                                  (* OR  DIGIT.DIGIT  IN INSYMBOL *)
      environt : contexte ;
                                                  (*  DATA, CODE  , AND SO ON..  *)
      envstandard : stdkind ;
      errcl : ARRAY [norange] OF typofsymb ;
                                                  (* ERROR RECOVERY IN  PASCAL PROGRAM *)
                                                  (* (NOT IN TYPE PART)  *)
      errorflag : ptexternalitem ;
      errorsfound : ARRAY [0..maxerpg] OF SET OF 0..maxset ;
                                                  (* SUMMARY OF ENCOUNTERED  ERRORS *)
                                                  (* DURING THETOTAL  COMPILATION *)
      errtotal : integer ;
                                                  (*  GIVES TOTAL NUMBER OF *)
                                                  (* ENCOUNTERED ERRORS *)
      exportablecode : boolean ;
      extcalltrapplace : integer ;                (* IF NON NULL BYTE DISP IN LINK OF LINK TO EXT CALL TRAP PROC *)
      fastoperator : boolean ;                    (* INIT IN CARTEEXEC. *)
                                                  (* FORCES FAST OPERATORS TO BE CALLED *)
                                                  (* USED IN GENERE    *)
      firstcond : condaddr ;                      (* PRT TO FIRST CONDITIONNAL VARIABLE BOX *)
      forbidden_id : alfaid ;                     (* IDENTIFIER FORBIDDEN IF CHECK_ID IS TRUE *)
      init_fsb_trap_flag : boolean ;              (* TRUE IF FSB INITIALIZED BY F.REF. TRAP *)
      init_fsb_trap_info_place,                   (* BYTE DISP IN TEXT OF OINFO FOR THIS TRAP *)
      init_fsb_trap_links_place,                  (* BYTE DISP IN LINK OF LINKS FOR THIS TRAP *)
      init_fsb_trap_number_of_files : integer ;   (* NBR OF FILES INIT. BY THIS TRAP *)
      inputflag : ptexternalitem ;                (* #0 IF INPUT IS IN PRG. PARAM. *)
      interactive : boolean ;                     (* TRUE IF INTERACTIVE MODE *)
      intptr : ctp ;                              (* INTEGER TYPE POINTER *)
      iowarnings : boolean ;                      (* TRUE IF IO WARNINGS WANTED (DEFAULT) *)
      ival : integer ;                            (* OUTPUT OF  INSYMBOL *)
      lamptr : ctp ;                              (*  POINTS   EMPTY SET TYPE *)
      lastproc : blocknodeptr ;                   (* PTR TO NODE FOR LAST GENERATED PROC *)
      level : levrange ;
      liglues : integer ;                         (* TOTAL  READ LINES COUNTER *)

      linkswordcount : integer ;                  (* WORD COUNT FOR LINKS GENERATION *)
      listyes : boolean ;                         (*  TRUE IF LISTING REQUE STED *)
      longchaine : integer ;                      (* LGTH USED IN BUFVAL *)
      longprofile : boolean ;                     (* TRUE IF LONG_PROFILE OPTION *)
      longstring : integer ;                      (*  LENGTH OF STRING IN CONALFA  *)
      majmin : ARRAY [0..127] OF integer ;
      mapswitch : boolean ;                       (* TRUE IF STATEMEMNT MAP NEEDED *)
      maxstring_ptr : ctp ;                       (* PTR TO MAXSTRING PREDEFINED CONSTANT *)
      mpcogerr, mpcogin, mpcogout : text ;
      next : ctp ;                                (*  LAST ITEM IN CONTTEXTTABLE *)
                                                  (* (NOT ALLWAYS) *)
      nilptr : ctp ;                              (*  NIL POINTER TYPE POINTER *)
      no : integer ;                              (* OUTPUT OF  IN SYMBOL  *)
      no_compilation_warnings : boolean ;
      outputflag : ptexternalitem ;               (* #0 IF OUTPUT IS IN PRG. PARAM. *)
      pageserrors : ARRAY [0..maxerpg] OF SET OF 0..maxset ; (* TO KEEP PAGES  *)
      pascalfrench : boolean ;
      pnumptr : ctp ; (* NUM. SET  TYPE POINTER *) (* WHERE ARE ERRS *)
      profilewordcount : integer ;                (* TOTAL PROFILE COUNTERS WORD COUNT *)
      profptr : profareaptr ;                     (* PTR TO PROFILE COUNTERS GENERATION AREA *)
      progname : alfaid ;                         (* NAME OF PRG.; FILLED IN PROGDECL *)
      programnode : blocknodeptr ;                (* PTR TO FIRST NODE OF PROGRAM *)
      realptr : ctp ;                             (* REAL TYPE POINTER *)
      rval : real ;                               (* OUTPUT  OF INSYMBOL *)
      selectivetable : boolean ;                  (* TRUE IF SOME SYMBOL TABLES REQUIRED *)
      sourceindex : integer ;                     (* INDEX IN SOURCE STRING *)
      sourcenbr : integer ;                       (* CURRENT SOURCE NO *)
      startic : integer ;                         (* INDEX OF FIRST NON PROFILE INSTR OF CURR STTMT *)
      staticswordcount : integer ;                (* TOTAL STATICS WORD COUNT *)
      statnbr : integer ;                         (* TOTAL NBR OF STATEMENTS IN STT MAP *)
      string_ptr : ctp ;                          (* PTR TO STANDARD STRING FORMAT *)
      sttfile : integer ;                         (* FILE NO OF CURR STTMT *)
      sttindex : integer ;                        (* INDEX IN SOURCE OF CURR STTMT *)
      sttline : integer ;                         (* LINE NO OF CURR STTMT *)
      sttplace : integer ;                        (* LOC FOR CURRENT STATEMENT *)
      symbolfile : integer ;                      (* SOURCE FILE OF CURRENT SYMBOL *)
      symbolindex : integer ;                     (* INDEX IN SOURCE OF CURR SYMBOL *)
      symbolline : integer ;                      (* SOURCE LINE OF CURRENT SYMBOL *)
      symbolmap : boolean ;                       (* TRUE IF SYMBOLS MAP REQUESTED *)
      textfilectp : ctp ;                         (* TEXT FILE TYPE POINTER *)
      top : integer ;                             (* LAST USED ENTRY IN DISPLAY *)
      undecptr : ctp ;                            (* FOR UNDEFINED VARS *)
      undlab : ARRAY [1..undmax] OF occurence ;
                                                  (* USED TO KEEP SEVERAL LISTS *)
                                                  (* OF UNRESOLVED REFERENCES *)
      usednames : typusednames ;
      version : integer ;                         (* CURRENT RELEASE OF THE COMPILER *)
      xc : integer ;                              (*  COUNTER  FOR GLOBALS  *)
      xrefneed : boolean ;                        (*  TRUE IF CROSS REFERENCES USED *)

(*  DEFINE INTERNALLY USED  VARIABLES *)
      adrligic : integer ;
      adrliglc : integer ;
                                                  (* USED IN ORDER TO PRINT COUNTERS *)
                                                  (* AT BEGINNING OF EACH LINE *)
      beginline : boolean ;                       (* TRUE IF READING BEGINNING OF SOURCE LINE *)
      brieftable : boolean ;                      (* TRUE IF BRIEF TABLE NEEDED *)
      bufold, bufnew : PACKED ARRAY [1..maxsliceline] OF char ;
      ch : char ;                                 (* OUTPUT OF NEXTCH, INPUT OF INSYMBOL *)
      chcnt : integer ;                           (* COLUMN NUMBER IN A SOURCE LINE *)
      checks : boolean ;                          (* INIT IN CARTEEXEC OR IN MAIN *)
      column : integer ;                          (* CURRENT COLUMN IN SOURCE LINE *)
      compencours : boolean ;
      currdate : alfa ;                           (* CURRENT DATE  DD**MM**YY   *)
      cursttmap : sttmapptr ;                     (* PTR TO CURRENT STT MAP STRUCTURE *)
      digits : SET OF char ;                      (* 0..9 *)
      dpoint : boolean ;
      end_statement : boolean ;                   (* TRUE IF STTMAP HAS BEEN GENERATED FOR CURR STT *)
      erredited : ARRAY [0..maxerpg] OF SET OF 0..maxset ;
      errinx : integer ;
      errlist : ARRAY [1..maxerrline] OF
      RECORD
        pos, nmr : integer ;                      (* TO KEEP ERR NUMBERS AND POSITIONS *)
      END ;
      err257 : boolean ;
                                                  (*  FLAG  FOR A LINE TOO LONG *)
      err149 : boolean ;                          (* FLAG *)
      filetoprint : integer ;                     (* FILE NBR TO PRINT AT THE BEGINNING OF A SOURCE LINE *)
      iligne : integer ;                          (* COUNTER OF LINES ON A PAGE *)
      instring : boolean ;                        (* TRUE IF IN A STRING *)
                                                  (* IN INSYMBOL AND NEXTCH *)
      incomment : boolean ;                       (* TRUE IF IN A COMMENT *)
      lastfile : integer ;                        (* FILE OF LAST EDITED LINE ON ERROR FILE *)
      lastlig : integer ;                         (* LAST EDITED LINE ON ERROR FILE *)
      letters : SET OF char ;
      linetoprint : integer ;                     (* LINE NBR TO PRINT AT THE BEGINNING OF A SOURCE LINE *)
      longpad : integer ;                         (*   ''      "     IN BUFVAL  *)
      mapptr : sttmapptr ;                        (* PTR TO STATEMENT MAP GENERATION AREA *)
      nbccond : integer ;                         (*  NESTED  CONDITIONAL COMP   *)
      oldfile : integer ;                         (* FILE OF PREVIOUS STATEMENT *)
      oldic : integer ;                           (* IC OF PREVIOUS STATEMENT *)
      oldindex : integer ;                        (* INDEX OF OLD STATEMENT MAP *)
      oldline : integer ;                         (* LINE OF PREVIOUS STATEMENT *)
      pagelength : integer ;                      (* INIT BY MAXPAGELINE OR CARTEEXEC *)
      pageno : integer ;                          (* NUMBER OF CURRENT PAGE *)
      pos1 : integer ;                            (* LAST ERROR'S POSITION IN LINE *)
      prevfile : integer ;                        (* FILE OF PREVIOUS LINE *)
      prevlig : integer ;                         (* LIEN NO OF PREVIOUS LINE *)
      profile : boolean ;                         (* TRUE IF PROFILE OPTION *)
      pt : ctp ;                                  (*  WORK  POINTER *)
      skipcode : boolean ;                        (* IF TRUE THEN DONT COMPILE SOURCE *)
      skippage : boolean ;                        (* TRUE ALTER $PAGE *)

      sourcectx : char ;                          (* '*' if line begins in a comment, ' 'otherwise *)
      sttinline : integer ;                       (* NBR OF STATEMENT IN LINE *)
      symbline : PACKED ARRAY [0..maxlinepascal] OF char ; (* CHARS OF A SOURCE LINE *)
      symbol_listing : boolean ;                  (* TRUE IF CROSS REFERENCE OF SYMBOLS ON LISTING *)
      symcl : ARRAY [0..127] OF integer ;         (*  CL  FOR  EACH  PASCAL  0..127 *)
      symno : ARRAY [0..127] OF integer ;         (*  NO   "     "     "       "  *)
      rversion : integer ;                        (* VERSION OF RACINE *)
      tsetinargs : boolean ;                      (* TRUE IF T OPTION SET AT COMMAND LEVEL *)
      usednamesa,
      usednamesf : typusednames ;
      wcl,
      wcla,
      wclf : ARRAY [0..maxnbofkeywords] OF integer ;
      wd,
      wda,
      wdf : ARRAY [0..maxnbofkeywords] OF alfaid ;
      wkextpt : ptexternalitem ;
      wl1,
      wl1a,
      wl1f : ARRAY [1..maxkeylength] OF integer ;
      wl2,
      wl2a,
      wl2f : ARRAY [1..maxkeylength] OF integer ;
      wno,
      wnoa,
      wnof : ARRAY [0..maxnbofkeywords] OF integer ;
      wnoset : setofno ;
      wdsetinargs : boolean ;                     (* TRUE IF WD OPTION SET AT COMMAND LEVEL *)
      wgsetinargs : boolean ;                     (* TRUE IF WG OPTION SET AT COMMAND LEVEL *)
      wssetinargs : boolean ;                     (* TRUE IF WS OPTION SET AT COMMAND LEVEL *)

(*  KEY-WORD XXX IS  IN WD AT   ENTRY  "N"  OF  LENGTH   I
   WNO[N],WCL[N]   ARE  ASSOCIATED NO  AND CL
   ALL KEY WORDS OF  LENGTH I   ARE IN WD   BETWEEN  ENTRIES
   WL1[I] .. WL2[I]
   *)


$OPTIONS page $

    $VALUE
      errcl = (16 * irrelsy,
        endsy,                                    (* 16  ;       *)
        4 * irrelsy,
        begsy,                                    (* 21  BEGIN   *)
        endsy,                                    (* 22  END     *)
        begsy,                                    (* 23  IF      *)
        irrelsy,                                  (* THEN *)
        endsy,                                    (* 25  ELSE    *)
        begsy,                                    (* 26  CASE    *)
        irrelsy,                                  (* OF  *)
        begsy,                                    (* 28  REPEAT  *)
        endsy,                                    (* 29 until *)
        begsy,                                    (* 30  WHILE   *)
        irrelsy,                                  (* DO   *)
        begsy,                                    (* 32  FOR     *)
        2 * irrelsy,                              (* TO DOWNTO *)
        begsy,                                    (* 35  GOTO    *)
        irrelsy,                                  (* 36  nil    *)
        endsy,                                    (* 37  TYPE    *)
        irrelsy,                                  (* 38 array record file set *)
        irrelsy,                                  (* 39 ..   *)
        2 * endsy,                                (* 40  LABEL  41  CONST *)
        irrelsy,                                  (* PACKED *)
        3 * endsy,                                (* 43  VAR  44  FUNCTION  45  PROCEDURE *)
        2 * irrelsy,
        begsy,                                    (* 48  WITH *)
        irrelsy,
        endsy,                                    (* 50  PROGRAM  *)
        7 * endsy) (* 51 $RENAME  52  $IMPORT  53  $EXPORT  54  $VALUE  55  $  *) ;
      majmin = (0, 1, 2, 3, 4, 5, 6, 7, 8, 32 (* SPACE *), 10, 11, 12, 13, 14, 15,
        16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
                                                  (* MAJ TO MIN *)
        97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
        107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
        118, 119, 120, 121, 122,
                                                  (* now same order *)
        91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
        103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
        113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127) ;
      symcl = (35 * 0, 5, 6 * 0, 1, 1, 0, 2, 0, 2, 12 * 0, 1, 6, 4, 65 * 0) ;
      symno = (35 * 0, 8, 4 * 0, 9, 10, 6, 7, 15, 7, 17, 6, 10 * 0,
        19, 16, 3 * 8, 0, 18, 26 * 0, 11, 0, 12, 18,
        33 * 0) ;
      usednamesa = ('input', 'output', 'error', 'forward', 'external', 'otherwise'
        ) ;
      usednamesf = ('entree', 'sortie', 'erreur', 'plusloin', 'externe', 'autrement'
        ) ;
      wcla = (0,
        0, 0, 1, 0, 7, 3,
        0, 0, 0, 4, 5, 0,
        3, 1, 4,
        0, 0, 0, 0, 0, 0,
        3,
        0, 0, 0, 1,
        0, 0,
        0, 2, 2, 0, 0,
        0, 0, 0, 0,
        0, 0, 0,
        0) ;
      wclf = (
        0,
        0, 3, 3, 0,
        2, 0, 4, 0, 5, 0, 1, 0,
        0, 7, 1, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0,
        2, 3, 0, 1, 0, 0, 0,
        4, 0, 0, 0, 0, 0,
        0, 0, 0
        ) ;
      wda = ('$       ',
        'if      ', 'do      ', 'to      ', 'of      ', 'in      ', 'or      ',
        'end     ', 'nil     ', 'for     ', 'div     ', 'mod     ', 'var     ',
        'and     ', 'not     ', 'set     ',
        'then    ', 'else    ', 'goto    ', 'case    ', 'with    ', 'type    ',
        'file    ',
        'begin   ', 'until   ', 'while   ', 'array   ',
        'const   ', 'label   ',
        'repeat  ', 'downto  ', 'record  ', 'packed  ', '$value  ',
        'program ', '$rename ', '$import ', '$export',
        'function', '$include', '$options',
        'procedure') ;
      wdf = (
        '$ ',
        'de', 'et', 'ou', 'si',
        'bas', 'cas', 'div', 'fin', 'mod', 'nil', 'non', 'var',
        'avec', 'dans', 'haut', 'pour', 'type',
        'alors', 'const', 'debut', 'faire', 'sinon',
        'allera', 'jusque', 'paquet',
        'article', 'fichier', 'repeter', 'tableau', 'tantque', '$valeur', '$rename',
        'ensemble', 'fonction', '$exporte', '$importe', '$include', '$options',
        'etiquette', 'procedure', 'programme'
        ) ;
      wl1a = (0, 1, 7, 16, 23, 29, 34, 38, 41) ;
      wl1f = (0, 1, 5, 13, 18, 23, 26, 33, 39) ;
      wl2a = (0, 6, 15, 22, 28, 33, 37, 40, 41) ;
      wl2f = (0, 4, 12, 17, 22, 25, 32, 38, 41) ;
      wnoa = (55,
        23, 31, 33, 27, 8, 7,
        22, 36, 32, 6, 6, 43,
        6, 5, 38,
        24, 25, 35, 26, 48, 37,
        38,
        21, 29, 30, 38,
        41, 40,
        28, 33, 38, 42, 54,
        50, 51, 52, 53,
        44, 56, 57,
        45) ;
      wnof = (
        55,
        27, 6, 7, 23,
        33, 26, 6, 22, 6, 36, 5, 43,
        48, 8, 33, 32, 37,
        24, 41, 21, 31, 25,
        35, 29, 42,
        38, 38, 28, 38, 30, 54, 51,
        38, 44, 53, 52, 56, 57,
        40, 45, 50
        ) $


$OPTIONS page $

(* IMPORTED   FROM   "UNIQUE"  *)
    PROCEDURE initialise ; EXTERNAL ;
    PROCEDURE progdecl ; EXTERNAL ;
    PROCEDURE initclasse ; EXTERNAL ;
    PROCEDURE heaperror ; EXTERNAL ;
    PROCEDURE prterrmeans (VAR ff : text ; numerr : integer) ; EXTERNAL ;
    PROCEDURE statistiques ; EXTERNAL ;
    PROCEDURE displaysymbols ; EXTERNAL ;

(* IMPORTED  FROM "DECLARE" *)
    PROCEDURE body (surrptr, firstentry : ctp) ; EXTERNAL ;

(* IMPORTED FROM CONTEXTTABLE *)

    PROCEDURE create_vars_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;
    PROCEDURE create_dummyclass_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;


(* IMPORTED FORM "GENERE"      *)
    PROCEDURE genprofileref ; EXTERNAL ;
    PROCEDURE genlongprofileref ; EXTERNAL ;
    PROCEDURE inser (fcb, fplace : integer) ; EXTERNAL ;
    PROCEDURE listhead ; EXTERNAL ;


(* IMPORTED FROM STATE *)
    PROCEDURE freeallregisters ; EXTERNAL ;

(* IMPORTED FROM EXPR *)
    PROCEDURE expression ; EXTERNAL ;

(* IMPORTED FROM MODATTR *)
    PROCEDURE initattrvarbl (VAR fattr : attr) ; EXTERNAL ;

(* IMPORTED FROM PL1 *)

    PROCEDURE buildobject ; EXTERNAL ;
    PROCEDURE initsource ; EXTERNAL ;
    PROCEDURE beginsource
        (filename : externid ; stringdeb : alfaid ; ldeb : integer ; stringfin : alfaid ; lfin : integer) ; EXTERNAL ;
    PROCEDURE endsource ; EXTERNAL ;
    PROCEDURE displaysources ; EXTERNAL ;
                                                  (* FROM PL1   *)
    PROCEDURE geninput (pr4disp : integer ; VAR fret : integer) ; EXTERNAL ;
    PROCEDURE genoutput (pr4disp : integer ; VAR fret : integer) ; EXTERNAL ;
    PROCEDURE generror (pr4disp : integer ; VAR fret : integer) ; EXTERNAL ;
    PROCEDURE genentree (pr4disp : integer ; VAR fret : integer) ; EXTERNAL ;
    PROCEDURE gensortie (pr4disp : integer ; VAR fret : integer) ; EXTERNAL ;
    PROCEDURE generreur (pr4disp : integer ; VAR fret : integer) ; EXTERNAL ;

    PROCEDURE getmapptr (VAR mapptr : sttmapptr) ; EXTERNAL ;
    PROCEDURE getprofptr (VAR profptr : profareaptr) ; EXTERNAL ;
    PROCEDURE convertreal (string : numberstring ; exp : integer ; VAR reel : real) ; EXTERNAL ;


    PROCEDURE geninputlink (pr4disp : integer ; VAR fret : integer) ;

      BEGIN
        IF pascalfrench THEN
	genentree (pr4disp, fret) ELSE
	geninput (pr4disp, fret) ;
      END ;


    PROCEDURE genoutputlink (pr4disp : integer ; VAR fret : integer) ;

      BEGIN
        IF pascalfrench THEN
	gensortie (pr4disp, fret) ELSE
	genoutput (pr4disp, fret) ;
      END ;


    PROCEDURE generrorlink (pr4disp : integer ; VAR fret : integer) ;

      BEGIN
        IF pascalfrench THEN
	generreur (pr4disp, fret) ELSE
	generror (pr4disp, fret) ;
      END ;


(* ********************************************************** NEXTPAGE  ******* *)

    PROCEDURE nextpage ; FORWARD ;


$OPTIONS page $

(* *************************************   INITRACINE   *********************** *)

    PROCEDURE initracine ;

(* C     THIS PROCEDURE IS USED TO INITIALIZE THE GLOBALS OF RACINE AND IS
   CALLED IN THE MODULE UNIQUE (PROCEDURE INITILALISE)                  C *)
      VAR
        it : integer ;
      BEGIN                                       (* INITRACINE *)
        adrligic := 0 ; adrliglc := 0 ;
        anytrace := none ;
        beginline := false ;
        bufold := '  ' ; bufnew := '   ' ;
        rewrite (mpcogerr) ;
        reset (mpcogin) ;
        initsource ;
        brieftable := false ;
        ch := ' ' ;
        chcnt := 0 ;
        check_id := false ;
        chnix := 1 ;
        codelist := false ;
        column := 0 ;
        compencours := true ;
        cursttmap := NIL ;
        date (currdate) ;
        declarationpart := true ;
        digits := ['0'..'9'] ;
        FOR it := 0 TO displimit DO
	display [it].fname := NIL ;             (* FOR SECURITY *)
        dpoint := false ;
        end_statement := true ;
        environt := data ;
        errinx := 0 ; fastoperator := false ;
        errorflag := NIL ;
        exportablecode := false ;
        extcalltrapplace := 0 ;
        FOR it := 0 TO maxerpg DO
	BEGIN
	  pageserrors [it] := [] ;
	  errorsfound [it] := [] ;
	  erredited [it] := [] ;
	END ;
        errtotal := 0 ;
        err257 := false ;
        filetoprint := 0 ;
                                                  (* firstcond IS SET IN pascal COMMAND BEFORE CALL TO racine *)
        iligne := 0 ;
        init_fsb_trap_flag := false ;
        init_fsb_trap_info_place := 0 ;
        init_fsb_trap_links_place := 0 ;
        init_fsb_trap_number_of_files := 0 ;
        inputflag := NIL ;
        interactive := false ;
        iowarnings := true ;
        envstandard := stdpure ;
        instring := false ;
        lastfile := 0 ;
        lastlig := 0 ;
        lastproc := NIL ;
        letters := ['a'..'z', 'A'..'Z'] ;
        incomment := false ;
        liglues := 1 ;
        linetoprint := 1 ;
        next := NIL ;
        level := 0 ;
        longpad := maxval ;
        longprofile := false ;
        mapptr := NIL ;
        mapswitch := false ;
        maxstring_ptr := NIL ;
        nbccond := 0 ;
        oldfile := -1 ;
        oldindex := 0 ;
        oldline := 0 ;
        outputflag := NIL ;
        pageno := 0 ; pagelength := maxpageline ; (* DEFAULT *)
        pascalfrench := false ;
        pos1 := 0 ;
        prevfile := 0 ;
        prevlig := 0 ;
        profile := false ;
        profilewordcount := 0 ;
        profptr := NIL ;
        progname := blank ;
        programnode := NIL ;
        rversion := 0 ;
        selectivetable := false ;
        string_ptr := NIL ;
        sourceindex := -1 ;
        version := rversion ;
        skipcode := false ; skippage := false ;
        sourcectx := ' ' ;
        sourcenbr := 0 ;
        startic := -1 ;
        statnbr := 0 ;
        symbol_listing := false ;
        symbolmap := false ;
        top := 0 ;
        FOR it := 1 TO undmax - 1 DO
	undlab [it].succ := it + 1 ;
        undlab [undmax].succ := 0 ;
        wnoset := [5, 6, 7, 8, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 40, 41, 42, 43, 44, 45, 48, 50] ;
        wdsetinargs := false ;
        wgsetinargs := false ;
        wssetinargs := false ;
        xc := firstglobal * bytesinword ;
        xrefneed := false ;
        FOR it := 1 TO 120 DO boxheader [it] := '*' ;
      END (* INITRACINE *) ;


$OPTIONS page $

(* *************************************NEXTLINE******************************* *)

    PROCEDURE nextline ;

(* C   PRINTS THE CURRENT LINE (WRITELN)
   BUFFER OF OUTPUT  MUST BE FILLED BEFORE                                  C *)
      BEGIN
        IF listyes THEN writeln (mpcogout) ;
        iligne := iligne + 1 ;                    (* NUMBER OF LINES  IN  CURRENT  PAGE *)
        IF skippage OR (iligne >= pagelength) THEN nextpage ;
      END (* NEXTLINE *) ;




$OPTIONS page $

(* *******************************************************  RETURNSTOP   ****** *)

    PROCEDURE returnstop ;

(* C  THIS PROCEDURE IS CALLED BY HEAPERROR (IN UNIQUE) IN ORDER TO STOP
   THE COMPILATION                                                        C *)
      BEGIN
        GOTO 100 ;                                (* END OF COMPILATION. HEAP IS FULL *)
      END (* RETURNSTOP *) ;


$OPTIONS page $

(* *********************************************************ERROR************** *)

    PROCEDURE error (errno : integer) ;

(* C  ENTERS  .NEW ERROR IN ERRLIST (FOR EACH LINE)
   .NEW ERROR IN ERRORSFOUND (FOR END OF COMPILATION MEANINGS)
   .LISTING'S PAGE NUMBER IN PAGEERRORS                            C *)
(* E  ERRORS DETECTED
   255: TOO MANY ERRORS ON THIS LINE
   381: ERROR NUMBER EXCEED HIGH BOUND
   382: PAGE  NUMBER   "      "    "                                      E *)
      BEGIN
$OPTIONS compile = trace $
        IF anytrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT ERROR WITH ERRNO ', errno : 5) ; nextline ;
	  IF anytrace = high THEN
	    BEGIN
	      write (mpcogout, ' ERRINX,POS1,COLUMN ', errinx, pos1, column) ; nextline ;
	    END
	END ;
$OPTIONS compile = true $
        IF errinx = (maxerrline - 1) THEN
	errno := 255 ;                          (* TOO MANY ERRORS *)
        IF errinx < maxerrline THEN
	BEGIN
	  IF column > pos1 THEN pos1 := column ;
	  errinx := errinx + 1 ;
	  WITH errlist [errinx] DO
	    BEGIN pos := pos1 ; nmr := errno ;
	    END ;
	  pos1 := pos1 + 1 ;
$OPTIONS compile = security $
	  IF errno > maxerrnum THEN error (381) ELSE
$OPTIONS compile = true $
	    errorsfound [errno DIV setrange] := errorsfound [errno DIV setrange] +
	    [errno MOD setrange] ;
$OPTIONS compile = security $
	  IF pageno > maxpage THEN error (382) ELSE
$OPTIONS compile = true $
	    pageserrors [pageno DIV setrange] := pageserrors [pageno DIV setrange] +
	    [pageno MOD setrange] ;
	  errtotal := errtotal + 1 ;
	END ;
$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  IF anytrace = high THEN
	    BEGIN
	      write (mpcogout, ' NOW  ERRINX, POS1 ARE ', errinx, pos1) ; nextline ;
	    END ;
	  write (mpcogout, ' @@@ FIN  ERROR @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* ERROR *) ;


$OPTIONS page $

(* *******************************************  WARNING   ************** *)

    PROCEDURE warning (errno : integer) ;

(* C  ENTERS  .NEW WARNING IN ERRLIST (FOR EACH LINE)
   .NEW WARNING IN ERRORSFOUND (FOR END OF COMPILATION MEANINGS)
   .LISTING'S PAGE NUMBER IN PAGEERRORS

   EXACTLY THE CONTENTS OF the procedure ERROR , EXCEPT
   ERRTOTAL' INCREMENT      .
   C *)
(* E  ERRORS DETECTED
   255: TOO MANY ERRORS ON THIS LINE
   381: ERROR NUMBER EXCEED HIGH BOUND
   382: PAGE  NUMBER   "      "    "                                      E *)
      BEGIN
$OPTIONS compile = trace $
        IF anytrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT WARNING WITH ERRNO ', errno : 5) ; nextline ;
	  IF anytrace = high THEN
	    BEGIN
	      write (mpcogout, ' ERRINX,POS1,COLUMN ', errinx, pos1, column) ; nextline ;
	    END
	END ;
$OPTIONS compile = true $

        IF NOT no_compilation_warnings THEN
	BEGIN
	  IF errinx = (maxerrline - 1) THEN
	    errno := 255 ;                      (* TOO MANY ERRORS *)
	  IF errinx < maxerrline THEN
	    BEGIN
	      IF column > pos1 THEN pos1 := column ;
	      errinx := errinx + 1 ;
	      WITH errlist [errinx] DO
	        BEGIN pos := pos1 ; nmr := errno ;
	        END ;
	      pos1 := pos1 + 1 ;
$OPTIONS compile = trace $
	      IF errno > maxerrnum THEN error (381) ELSE
$OPTIONS compile = true $
	        errorsfound [errno DIV setrange] := errorsfound [errno DIV setrange] +
	        [errno MOD setrange] ;
$OPTIONS compile = trace $
	      IF pageno > maxpage THEN error (382) ELSE
$OPTIONS compile = true $
	        pageserrors [pageno DIV setrange] := pageserrors [pageno DIV setrange] +
	        [pageno MOD setrange] ;
                                                  (*         NO INCREMENT OF ERRTOTAL      *)
	    END ;
	END ;
$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  IF anytrace = high THEN
	    BEGIN
	      write (mpcogout, ' NOW  ERRINX, POS1 ARE ', errinx, pos1) ; nextline ;
	    END ;
	  write (mpcogout, ' @@@ FIN  WARNING @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* WARNING *) ;


$OPTIONS page $

(* ***********************************************NEXTPAGE********************* *)

    PROCEDURE nextpage ;

(* C NEXTLINE ON OUTPUT BEGINS AT BEGINNING OF A NEW PAGE  ON LISTING
   INCREMENTS    PAGENO;
   PRINTS        PAGE  NUMBER
   C *)
(* E  ERRORS DETECTED
   383:  MAX NUMBER OF LISTING'S PAGES EXCEEDED                             E *)
      BEGIN
        skippage := false ;
        pageno := pageno + 1 ;
$OPTIONS compile = trace $
        IF pageno > maxpage THEN error (383) ;
$OPTIONS compile = true $
        IF listyes THEN page (mpcogout) ;         (* NEXT LINE ON A NEW PAGE(MPCOGOUT) *)
                                                  (* WRITE    PAGE NUMBER *)
        IF listyes THEN
	write (mpcogout, '*** MULTICS PASCAL COMPILER - V8.0', version : 1,
	  '  **** PROGRAM ', progname : 32,
	  ' *** ON ', currdate, ' *** ', ' ' : 11,
	  'PAGE ', pageno : 5) ;
        iligne := 0 ;
        nextline ;
                                                  (* DUMMY SPACE LINE *)
        nextline ;
                                                  (* INIT  COUNTER  FOR ALLOWED LINES *)
                                                  (* ON A PAGE *)
      END (* NEXTPAGE *) ;


$OPTIONS page $

(* ***********************************************(FCT) SUP******************** *)

    FUNCTION sup (fval1, fval2 : integer) : integer ;

(* C SUP IS THE GREATEST  VALUE BETWEEN  FVAL1 AND FVAL2                      C *)
      BEGIN
        IF fval1 > fval2 THEN
	sup := fval1 ELSE
	sup := fval2 ;
      END (* SUP *) ;


$OPTIONS page $

(* ************************************ FCT. POWEROFTWO *********************** *)
    FUNCTION poweroftwo (fval : integer) : integer ;

(* C  RETURNS   N  IF  FVAL=2**N
   -1  IF  FVAL <=0
   0  IF  FVAL= 1
   C *)
      LABEL
        10 ;                                      (* EXIT LOOP *)
      VAR
        lvalu, it : integer ;
      BEGIN                                       (* POWEROFTWO *)
        IF fval <= 0 THEN
                                                  (* <==== *) lvalu := -1 ELSE
	FOR it := 0 TO bitsinword - 2 DO
	  IF fval = 1 THEN
	    BEGIN
	      (* <==== *) lvalu := it ; GOTO 10 ; (* EXITLOOP *)
	    END (* FVAL=1 *) ELSE
	    IF odd (fval) THEN
	      BEGIN
	        (* <==== *) lvalu := -1 ; GOTO 10 ; (* EXIT LOOP *)
	      END (* ODD *) ELSE
	      fval := fval DIV 2 ;
10 :                                              (* EXIT LOOP *)
        poweroftwo := lvalu ;
$OPTIONS compile = trace $
        IF anytrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT-FIN POWEROFTWO @@@ WITH FVAL, COMPUTED VALUE',
	    fval, lvalu) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* POWEROFTWO *) ;


$OPTIONS page $

(* ************************************ INSERUNDLAB *************************** *)

    PROCEDURE inserundlab (fcb, fdebchain : integer) ;

(* C   "FCB"   IS A  BYTES DISPLACEMENT IN THE CODE FOR THE ACTUAL PROCEDURE.
   "FDEBCHAIN"  IS  THE  BEGINNING  IN UNDLAB   OF  A LIST  OF UNRESOLVED
   REFERENCES  USING  THIS VALUE  OF "FCB".
   EACH ITEM  OF  THE  LIST  IS
   .THE PLACE   IN CODE( IN FICHINTER)  OF  INCOMPLETE INSTRUCTION
   .THE POINTER ON THE NEXT LIST'S  ITEM
   C *)
(* E ERRORS DETECTED
   407   FDEBCHAIN MUST NOT BE  0  (EMPTY LIST)
   E *)
      LABEL 1 ;                                   (* EXIT IF COMPILER'S ERROR *)
      VAR
        it : integer ;
      BEGIN                                       (* INSERUNDLAB *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT INSERUNDLAB @@@ WITH FCB,FDEBCHAIN', fcb, fdebchain : 6) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        IF fdebchain = 0 THEN
	BEGIN
	  IF errtotal = 0 THEN error (407) ;
	  GOTO 1 ;
	END ;
        it := fdebchain ;
        WHILE undlab [it].succ # 0 DO
	BEGIN
	  inser (fcb, undlab [it].place) ;
	  it := undlab [it].succ ;
	END ;
                                                  (*  NOW THE LAST *)
        inser (fcb, undlab [it].place) ;
                                                  (*  NOW  GIVE THIS  RESOLVED LIST *)
                                                  (* AT FREE LIST *)
        undlab [it].succ := chnix ;
        chnix := fdebchain ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN INSERUNDLAB @@@ WITH CHNIX:', chnix : 6) ; nextline ;
	END ;
$OPTIONS compile = true $
1 :                                               (* COMES HERE IF ERROR(407) *)
      END (* INSERUNDLAB *) ;



$OPTIONS page $

(* *********************************************************(FCT) RECADRE****** *)

    FUNCTION recadre (fnumber, fmod : integer) : integer ;

(* C  RETURNS THE FIRST FMOD-MULTIPLE OF FNUMBER                              C *)
(* E  ERRORS DETECTED
   350 :  RECADRE CALLED WITH  FMOD <=0
   E *)
      VAR
        lmod : integer ;
      BEGIN
$OPTIONS compile = security $
        IF fmod <= 0 THEN
	error (350) ELSE
	BEGIN
$OPTIONS compile = true $
	  lmod := fnumber MOD fmod ;
	  IF lmod = 0 THEN
	    recadre := fnumber ELSE
	    recadre := fnumber + fmod - lmod ;
$OPTIONS compile = security $
	END ;
$OPTIONS compile = true $
      END (* RECADRE *) ;



$OPTIONS page $

(* ***********************************************SRCHREC********************** *)

    PROCEDURE srchrec (fbegsearch : ctp) ; EXTERNAL ; (* THIS PROCEDURE HAS BEEN OPTIMIZED *)

{
  OOPROCEDURE DEF SRCHREC (  FBEGSEARCH:CTP );

  CC(*C  SEARCHS A BOX WITH NAME= AVAL .  RETURNS   CTPTR = NIL  OR FOUND BOX
  SEARCH     BEGINS IN CONTEXTTABLE  AT  FBEGSEARCH   AND STOPS AT NIL
  C*)
  BBLABEL
  1 ; (* EXIT WHILE  FOR EFFICIENCY*)
  GGBEGIN
  CTPTR := FBEGSEARCH;
  WHILE  CTPTR # NIL DO
  IF  CTPTR@.NAME = AVAL THEN
  GOTO 1   ELSE
  CTPTR := CTPTR@.NXTEL;
  111: (* CTPTR HERE  NIL OR OK *)
  DDEND (*SRCHREC*) ;
}


$OPTIONS page $

(* ***********************************************SEARCH*********************** *)

    PROCEDURE search ; EXTERNAL ;                 (* THIS PROCEDURE HAS BEEN OPTIMIZED *)

{
  OOPROCEDURE DEF SEARCH;

  CC(*C   THE ARRAY 'DISPLAY'  FROM 0 TO TOP   CONTAINS  EACH  LEVEL'S LIST'S
  BEGINNING.
  THIS PROC  SEARCHS  A BOX WITH NAME 'AVAL'
  RETURNS   CTPTR = NIL  OR  FOUND BOX
  DISX  = INDEX  IN DISPLAY  WHERE  BOX  WAS  FOUND
  CAN BE 0  =>  PREDEF  OR  NOT FOUND
  C*)
  BBLABEL
  1; (* EXIT LOOP  FOR  EFFICIENCY  *)
  RRVAR
  I:INTEGER ;
  GGBEGIN
  FOR I:= TOP DOWNTO 0 DO
  BEGIN
  CTPTR := DISPLAY[I].FNAME; (* BEGINNING OF LIST *)
  WHILE CTPTR # NIL DO
  IF CTPTR@.NAME = AVAL THEN
  BEGIN
  DISX :=I; GOTO 1;
  END   ELSE
  CTPTR := CTPTR@.NXTEL;
  END;(*FOR I *)
  DISX := 0;
  111:  (* HERE   CTPTR AND DISX OK FOR CALLER *)
  DDEND (*SEARCH *);
}


$OPTIONS page $

(* *************************************CREALFABOX***************************** *)

    PROCEDURE crealfabox (VAR fkonstbox : ctp) ;

(* C .BUFVAL IS LOADED  FOR 1  TO LONGSTRING  WITH THE  STRING VALUE
   .THIS PROC   CREATES   THE  BOXES ASSOCIATED WITH THIS VALUE
   AND ASSIGNS  FKONSTBOX@.ALFADEB  AND  FKONSTBOX@.ALFALONG
   C *)
(* E ERRORS   DETECTED
   HEAPERROR
   E *)
      VAR
        localfpt, nxtal : alfapt ;
        nboxes, it, j, longlast, debbuf : integer ;


(* ***********************************************PRINTALFABOX < CREALFABOX**** *)

$OPTIONS compile = trace $
      PROCEDURE printalfabox (ptalfabox : alfapt) ;

(* C   USED IN CONDITIONAL  COMPILATION TO PRINT THE CONTENT OF
   AN ALFABOX (TYPE=ALFAVALUE). PTALFABOX POINTS THE BOX                  C *)
        VAR
	it : integer ;
        BEGIN
	nextline ; write (mpcogout, boxheader) ; nextline ;
	IF ptalfabox = NIL THEN
	  BEGIN
	    write (mpcogout, '* ALFABOX REQUESTED IS NIL. TRACE STOPS') ; nextline ;
	  END ELSE
	  BEGIN
	    write (mpcogout, '* ALFABOX FOLLOWING IS AT @', ord (ptalfabox)) ; nextline ;
	    WITH ptalfabox@ DO
	      BEGIN
	        write (mpcogout,
		'*   NEXTVAL IS : ', ord (nextval), ' USED SIZE IS ', longfill : 4) ;
	        nextline ;
	        write (mpcogout, '*   ALFAVAL IS : @') ;
	        FOR it := 1 TO longfill DO write (mpcogout, alfaval [it]) ;
	        write (mpcogout, '@') ;
	        nextline ;
	      END ;                             (* WITH PTALFABOX@ *)
	  END ;                                 (*  PTALFABOX # NIL *)
	write (mpcogout, boxheader) ; nextline ; nextline ;
        END (* PRINTALFABOX *) ;
$OPTIONS compile = true $


      BEGIN                                       (* CREALFABOX *)
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT CREALFABOX @@@ ', 'V. FKONSTBOX ', ord (fkonstbox)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        nboxes := longstring DIV longalfbox ;     (* NB. OF FULL BOXES *)
        longlast := longstring MOD longalfbox ;   (* LENGTH OF LAST BOX OR ZERO *)
        nxtal := NIL ;
        debbuf := 0 ;
        FOR it := 0 TO nboxes - 1 DO              (* FOR FULL  BOXES  *)
	BEGIN
	  new (localfpt) ; IF localfpt = NIL THEN heaperror ; (* EXIT COMP *)
	  IF nxtal = NIL THEN
	    fkonstbox@.alfadeb := localfpt ELSE nxtal@.nextval := localfpt ;
	  nxtal := localfpt ;
	  WITH localfpt@ DO
	    BEGIN nextval := NIL ;
	      FOR j := 1 TO longalfbox DO
	        alfaval [j] := bufval [debbuf + j] ;
	      debbuf := debbuf + longalfbox ;
	      longfill := longalfbox ;
	    END ;
$OPTIONS compile = trace $
	  IF decltrace > none THEN
	    BEGIN
	      write (mpcogout, ' ALFA BOX CREATED AT ', ord (localfpt)) ; nextline ;
	      IF decltrace = high THEN
	        printalfabox (localfpt) ;
	    END ;
$OPTIONS compile = true $
	END ;                                   (* FOR IT *)
        fkonstbox@.alfalong := longstring ;
        IF longlast # 0 THEN                      (* FILL   LAST BOX  *)
	BEGIN
	  new (localfpt) ; IF localfpt = NIL THEN heaperror ; (* EXIT COMP *)
	  IF nxtal = NIL THEN
	    fkonstbox@.alfadeb := localfpt ELSE nxtal@.nextval := localfpt ;
	  WITH localfpt@ DO
	    BEGIN
	      nextval := NIL ; longfill := longlast ;
	      FOR j := 1 TO longlast DO
	        alfaval [j] := bufval [debbuf + j] ;
	    END (* WITH *) ;
$OPTIONS compile = trace $
	  IF decltrace > none THEN
	    BEGIN
	      write (mpcogout, ' ALFA BOX CREATED AT ', ord (localfpt)) ; nextline ;
	      IF decltrace = high THEN
	        printalfabox (localfpt) ;
	    END ;
$OPTIONS compile = true $
	END ;                                   (* LONGLAST #0 *)
$OPTIONS compile = trace $
        IF decltrace > low THEN
	BEGIN
	  IF decltrace = high THEN
	    BEGIN
	      write (mpcogout, ' STRING TO BE GENERATED  ON ', longstring : 3, ' CHARS WAS') ;
	      nextline ;
	      FOR it := 1 TO longstring DO write (mpcogout, bufval [it]) ; nextline ;
	    END ;
	  write (mpcogout, ' @@@ FIN  CREALFABOX @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* CREALFABOX *) ;


$OPTIONS page $

(* *******************************************************************PRINTERR* *)

    PROCEDURE printerr ;

(* C   AFTER  COMPILATION OF EACH PASCAL SOURCE LINE ,THIS PROCEDURE IS CALLED
   IN ORDER TO FLAG THE COLUMN(S) WHERE IS(ARE) ERROR(S)
   ERRINX POINTS THE LAST ENTRY USED IN ERRLIST WHERE NUMBER AND POSITION OF
   EACH ERROR IS KEPT                                                     C *)
      VAR
        it, errdeb, errmax, errptr, errnumb : integer ;
      BEGIN                                       (* PRINTERR *)
        errptr := 1 ;                             (* POINTS THE NEXT ERROR *)
                                                  (* TO BE PROCESSED *)
        errmax := 0 ;                             (* POINTS THE LAST COLUMN *)
                                                  (* REACHED ON A LINE *)
                                                  (* WRITES TWO LAST LINES *)
        IF (lastlig <> prevlig) OR (lastfile <> prevfile) THEN
	IF prevfile = 0 THEN writeln (mpcogerr, '    ', prevlig : 5, ' ', bufold)
	ELSE writeln (mpcogerr, prevfile : 3, ' ', prevlig : 5, ' ', bufold) ;
        IF filetoprint = 0 THEN writeln (mpcogerr, '    ', linetoprint : 5, ' ', bufnew)
        ELSE writeln (mpcogerr, filetoprint : 3, ' ', linetoprint : 5, ' ', bufnew) ;
        lastfile := filetoprint ; lastlig := linetoprint ;
        WHILE errptr <= errinx DO
	BEGIN
	  errmax := errmax + lgprint ;
	  IF errlist [errptr].pos <= errmax THEN
	    BEGIN
	      IF chcnt <= lgprint THEN
	        BEGIN write (mpcogout, '*********') ;
		write (mpcogerr, '*********') ;
	        END ELSE
	        BEGIN write (mpcogout, '***', (errmax DIV lgprint) : 3, '***') ;
		write (mpcogerr, '***', (errmax DIV lgprint) : 3, '***') ;
	        END ;
	      errdeb := errptr ;                (* FIRST ERROR ON THE LINE PRINTED *)
	      FOR it := errmax - lgprint TO errmax DO
	        IF errptr <= errinx THEN
		BEGIN
		  IF errlist [errptr].pos = it THEN
		    BEGIN
		      write (mpcogout, '"') ; write (mpcogerr, '"') ;
		      errptr := errptr + 1 ;
		    END ELSE
		    BEGIN write (mpcogout, ' ') ; write (mpcogerr, ' ') ;
		    END ;
		END ELSE
		BEGIN write (mpcogout, ' ') ; write (mpcogerr, ' ') ;
		END ;
	      nextline ; writeln (mpcogerr) ;
	    END ;                               (* ERROR(S) ON THE LINE *)
	END ;                                   (* LOOP ON THE LINES *)
        write (mpcogout, '  ERROR(S) NR :') ;
        write (mpcogerr, '  ERROR(S) NR :') ;
        FOR it := 1 TO errinx DO
	BEGIN
	  write (mpcogout, errlist [it].nmr : 4) ;
	  write (mpcogerr, errlist [it].nmr : 4)
	END ;
        nextline ; writeln (mpcogerr) ;
        FOR it := 1 TO errinx DO

	BEGIN
	  errnumb := errlist [it].nmr ;
	  IF NOT (errnumb MOD maxset IN erredited [errnumb DIV maxset]) THEN
	    BEGIN
	      prterrmeans (mpcogerr, errnumb) ; writeln (mpcogerr) ;
	      erredited [errnumb DIV maxset] := erredited [errnumb DIV maxset] +
	      [errnumb MOD maxset] ;
	    END
	END ;
        writeln (mpcogerr) ;
        errinx := 0 ;
        pos1 := 0 ;
      END (* PRINTERR *) ;


$OPTIONS page $

(* ***********************************************NEXTCH*********************** *)

    PROCEDURE nextch ;

(* C .GIVES TO INSYMBOL THE NEXT RELEVANT  CHARACTER OF SOURCE.
   .AT EOLN   PRINTS  LAST LINE
   .AT EOF    EXITS COMPIL.
   C *)
(* E  ERRORS DETECTED
   18: ' EXPECTED
   22: EOF ON FILE INPUT =SOURCE
   257: SOURCE LINE  IS TOO LONG
   E *)
      LABEL
        1, 2 ;                                    (*  EXIT OF LOOP FOR CH#' '  *)
      VAR
        caract : char ;
        it, startit, chprint, index, ll : integer ;
        listingline : PACKED ARRAY [1..maxsliceline] OF char ;
        ch1 : char ;
        liststatus : boolean ;
      BEGIN                                       (* NEXTCH *)
2 :
        IF beginline THEN BEGIN
	  beginline := false ;
	  IF incomment OR skipcode THEN sourcectx := '*' ELSE sourcectx := ' '
	END ;
        IF eoln (mpcogin) THEN                    (* END OF CURRENT LINE *)
	IF NOT eof (mpcogin) THEN
	  BEGIN
	    IF listyes OR (errinx > 0) THEN     (* L+ OR ERROR(S) ON LINE *)
	      BEGIN                             (* PRINTS THIS LINE *)
	        liststatus := listyes ; listyes := true ;
	        IF instring AND (envstandard <> stdextend) THEN
		BEGIN error (18) ; instring := false ; END ;
                                                  (* PRINTS FILE NO, LINE NO *)
	        IF filetoprint = 0 THEN ll := swrite (listingline, 1, '    ', linetoprint : 5, sourcectx)
	        ELSE ll := swrite (listingline, 1, filetoprint : 3, ' ', linetoprint : 5, sourcectx) ;
                                                  (* NOW PRINTS SOURCE . *)
                                                  (* 'LGPRINT' CHARS ON A LINE, *)
                                                  (* SEVERAL LINES ALLOWED *)
	        startit := 2 ; it := 1 ; chprint := 0 ;
	        WHILE it <= chcnt DO
		BEGIN
		  caract := symbline [it] ;
		  IF caract = '	' THEN    (* TAB *)
		    chprint := ((chprint + 10) DIV 10) * 10
		  ELSE
		    chprint := chprint + 1 ;
		  IF chprint >= lgprint THEN
		    BEGIN
		      ll := swrite (listingline, ll, symbline : 2 + it - startit : startit) ;
		      startit := it + 2 ;
		      write (mpcogout, listingline : ll - 1) ;
		      nextline ;
		      ll := swrite (listingline, 1, '   ') ;
		      chprint := 0 ;
		    END ;
		  it := it + 1 ;
		END ;
	        IF chprint <> 0 THEN
		ll := swrite (listingline, ll, symbline : 1 + it - startit : startit) ;
	        write (mpcogout, listingline : ll - 1) ;
	        nextline ;
	        IF errinx > 0 THEN printerr ;
	        listyes := liststatus ;
	      END (* LISTING *) ;
	    bufold := bufnew ; bufnew := '   ' ;
	    chcnt := -1 ;                       (* SYMBLINE[0] = SPACE  DUMMY *)
                                                  (* DUE TO EOLN *)
	    beginline := true ;
	    prevfile := filetoprint ;
	    prevlig := linetoprint ;
	    column := -1 ;
	    liglues := liglues + 1 ;            (* LINES' COUNTER *)
                                                  (* AT BEGINNING OF NEXT PRINTED LINE *)
	    err257 := false ;                   (* TO AVOID SEVERAL ERROR(257) *)
                                                  (* ON THE SAME LINE *)
	    filetoprint := sourcenbr ;
	    linetoprint := liglues ;
	  END (* EOLN *) ;
        IF eof (mpcogin) THEN
	IF sourcenbr = 0 THEN
	  BEGIN
	    IF compencours THEN error (22) ;
	    GOTO 100 ;                          (* GOTO    END OF COMPILER *)
	  END
	ELSE
	  BEGIN
	    endsource ;                         (* END OF INCLUDE FILE *)
	    GOTO 2 ;
	  END ;
                                                  (* HERE VITAL PART *)
                                                  (* ==> ASSIGNS CH FOR INSYMBOL *)
        REPEAT
	ch1 := ch ;
	read (mpcogin, ch) ;                    (* SPACE RETURNED  IF EOLN(MPCOGIN)   *)
	sourceindex := sourceindex + 1 ;
	IF chcnt < maxlinepascal THEN
	  BEGIN
	    chcnt := chcnt + 1 ;
	    symbline [chcnt] := ch ; IF chcnt < maxsliceline THEN bufnew [chcnt] := ch ;
	    IF ch = '       ' THEN              (* TAB *)
	      BEGIN
	        IF column = -1 THEN column := 0 ;
	        column := ((column + 10) DIV 10) * 10
	      END
	    ELSE column := column + 1 ;
	  END ELSE
	  IF NOT err257 THEN
	    BEGIN error (257) ;
	      err257 := true ;
	    END ;
	IF (ch # ' ') OR (ch1 # ' ') THEN GOTO 1 ; (* EXIT REPEAT *)
        UNTIL eoln (mpcogin) OR instring OR eof (mpcogin) ;
1 :
        IF NOT instring THEN ch := chr (majmin [ord (ch)]) ;
      END (* NEXTCH *) ;


$OPTIONS page $

(* ***********************************************TRACELEVEL******************* *)

    PROCEDURE tracelevel (VAR whichtrace : levtrace ; charfound : char) ;

(* C   A TRACE COMMAND  WAS FOUND;  THE  CHAR GIVING WANTED LEVEL ALSO.
   C *)
(* E   ERRORS DETECTED
   25:  INVALID  TRACE  OPTIONS   IN  COMPILER  PARMLIST
   E *)
      BEGIN
        whichtrace := none ;                      (* DEFAULT *)
        IF charfound = '0' THEN whichtrace := none ELSE
	IF charfound = '1' THEN whichtrace := low ELSE
	  IF charfound = '2' THEN whichtrace := medium ELSE
	    IF charfound = '3' THEN whichtrace := high ELSE
	      error (25) ;
        anytrace := decltrace ;
        IF anytrace < stattrace THEN anytrace := stattrace ;
        IF anytrace < genetrace THEN anytrace := genetrace ;
      END (* TRACELEVEL *) ;


$OPTIONS page $

    PROCEDURE traiteinclude ; FORWARD ;
    PROCEDURE traiteoptions ; FORWARD ;

(* *******************************************************************INSYMBOL* *)

    PROCEDURE insymbol ;

(* C  .ASSIGNS A CODE  (NO,CL)  TO EACH ITEM
   .RETURNS VALUE   IN  IVAL,RVAL,BUFVAL
   .SKIPS  COMMENT   AND  COMPIL. COND.
   .DECODE   OPTIONS

   INSYMBOL'S  OUTPUT   SUMMARY

   *NO*CL*****ITEM***SYNONYMS*******OUTPUTS*******||||**NO**CL***ITEM************

   .    1 LG       ID.                AVAL                     21        BEGIN
   .    2  1       CST. INT           IVAL                     22        END
   .       2          . REAL          RVAL                     23        IF
   .       3          . ALFA          BUFVAL  LONGCHAINE       24        THEN
   .       4          . CHAR          IVAL                     25        ELSE
   .                                                        26        CASE
   .    5  1       NOT                                         27        OF
   28        REPEAT
   .    6  1         *                                         29        UNTIL
   .       2         /                                         30        WHILE
   .       3       AND                                         31        DO
   .       4       DIV                                         32        FOR
   .       5       MOD                                         33 1      TO
   .                                                           2      DOWNTO
   .    7  1         +
   .       2         -                                         35        GOTO
   .       3        OR                                         36        NIL
   37        TYPE
   .    8  1        <                                          38 1      ARRAY
   .       2        <=                                            2      RECORD
   .       3        >=                                            3      FILE
   .       4        >                                             4      CLASS
   .       5        <>    #                                       5      SET
   .       6        =                                          39     ...SEE  CHAR..
   .       7        IN                                         40        LABEL
   .                                                        41        CONST
   .                                                        42        PACKED
   .    9           (                                          43        VAR
   .   10           )                                          44        FUNCTION
   .   11           [        (.                                45        PROCEDURE
   .   12           ]        .)                                46   .... CF SKIP ...
   .   55           $                                          47        VALUE
   .   56          $include
   .   15           ,                                          48        WITH
   .   16          ;                                           49     ...SEE CHAR..
   .   17           .                                          50        PROGRAM
   .   18           @               ^
   .   19           :
   .   20           :=
   .   39           ..
   .   49           ->
   C *)
(* E
   32  OCTAL NUMBER IS NOT STANDARD
   33  HEXADECIMAL,BINARY NUMBER IS NOT STANDARD
   70 OBSOLETE CONDITIONNAL COMPILATION MECHANISM
   200  CHARACTER NOT ALLOWED IN PASCAL TEXT
   201  ERROR IN REAL CONSTANT DIGIT EXPECTED
   202  ERROR IN EXPONENT OF REAL CONSTANT
   203  INTEGER CONSTANT OUT OF RANGE
   204  ILLEGAL DIGIT IN OCTAL CONSTANT
   205  EXPONENT OUT OF RANGE
   206  DECIMAL CONSTANT IS TOO LONG
   207  OCTAL CONSTANT IS TOO LONG
   208  ILLEGAL NESTING OF ( /  AND  / )
   209  CHARACTERS' STRING IS TOO LONG
   210  HEXADECIMAL VALUE IS TOO LONG
   211  ILLEGAL DIGIT IN HEXADECIMAL CONSTANT
   212  ERROR IN COMPILATION'S  OPTIONS
   215 Too many digits
   216 Only 0 ou 1 allowed
   217 REAL > MAXREAL
   218 REAL < MINREAL
   219 TOO MANY PRECISION DIGITS FOR A REAL
   220 Empty string not allowed
   222
   223  Invalid number separator
   224 REFERENCE TO THIS IDENTIFIER IS NOT ALLOWED HERE.
   E *)
      LABEL 1,                                    (* BEGINNING OF INSYMBOL *)
        3,                                        (* EXIT WHEN KEY-WORD IS FOUND *)
        4,
        5 ;                                       (* COMMENT *)
      VAR
        it, k, scale, exp, valhex : integer ;
        sign, combraces, option, fin : boolean ;
        locvalue : integer ;
        locsomme : integer ;
        ch1 : char ;
        nbrstring : numberstring ;

      BEGIN                                       (* INSYMBOL *)
        IF building_from_schema.on THEN
	WITH building_from_schema DO
	  BEGIN
	    WITH current_token^ DO
	      CASE kind OF
	        symbol_token : BEGIN no := tno ; cl := tcl END ;
	        name_token : BEGIN aval := taval ; no := 1 END ;
	        int_const_token : BEGIN no := 2 ; cl := 1 ; ival := t_int_value END ;
	        real_const_token : BEGIN no := 2 ; cl := 2 ; rval := t_real_value END ;
	        char_const_token : BEGIN no := 2 ; cl := 4 ; ival := ord (t_char_value) END ;
	      END ;
	    current_token := current_token^.next ;
	    IF current_token = NIL THEN on := false ;
	  END
        ELSE
	BEGIN
1 :	  IF dpoint THEN (* INTEGER.. AT LAST CALL *) (* .. *)
	    BEGIN
	      dpoint := false ; no := 39 ;
	      nextch ;
	    END ELSE
	    BEGIN                               (* NOT DPOINT *)
4 :
	      WHILE ch = ' ' DO nextch ;        (* CH IS CHECKED BY NEXTCH *)
	      symbolindex := sourceindex ;
	      symbolline := liglues ;
	      symbolfile := sourcenbr ;
	      IF ch IN ['a'..'z', '$'] THEN
	        BEGIN
		IF ch = '$' THEN
		  IF envstandard = stdpure THEN
		    BEGIN
		      error (200) ;
		      nextch ;
		      IF NOT (ch IN ['a'..'z']) THEN GOTO 4
		    END ;
		k := 0 ; aval := blank ;
		IF envstandard <> stdextend THEN
		  REPEAT
		    IF k < maxident THEN
		      BEGIN
		        k := k + 1 ; aval [k] := ch ;
		      END ;
		    nextch ;
		  UNTIL NOT (ch IN ['a'..'z', '0'..'9'])
		ELSE
		  REPEAT
		    IF k < maxident THEN
		      BEGIN
		        k := k + 1 ; aval [k] := ch ;
		      END ;
		    nextch ;
		  UNTIL NOT (ch IN ['a'..'z', '0'..'9', '_']) ; (* UNDERSCORE IS ALLOWED IN NO STANDARD *)
                                                  (* KEY-WORDS *)
		IF k <= maxkeylength THEN
		  FOR it := wl1 [k] TO wl2 [k] DO (* KEY-WORD *)
		    IF aval = wd [it] THEN
		      BEGIN
		        no := wno [it] ; cl := wcl [it] ; GOTO 3 ; (* EXIT LOOP ,KEY-WORD FOUND *)
		      END ;
		no := 1 ; cl := k ;
		IF check_id THEN
		  IF aval = forbidden_id THEN error (224)
		  ELSE
		    BEGIN
		      IF forbidden_id_list^.next = NIL THEN
		        BEGIN
			new (forbidden_id_list^.next) ;
			WITH forbidden_id_list^.next^ DO
			  BEGIN
			    previous := forbidden_id_list ;
			    next := NIL
			  END
		        END ;
		      forbidden_id_list := forbidden_id_list^.next ;
		      forbidden_id_list^.name := aval ;
		    END ;
                                                  (* if aval[1] = '$' then error(200) ; *)
3 :
		IF no = 56 THEN               (* $include founded *)
		  BEGIN
		    traiteinclude ;
		    GOTO 1
		  END ;
		IF no = 57 THEN

		  IF NOT skipcode THEN
		    BEGIN
		      traiteoptions ;
		      GOTO 1 ;
		    END ;
	        END (* letter *) ELSE
	        IF ch IN digits THEN
		BEGIN                         (* NUMBER *)
		  no := 2 ; cl := 1 ;
		  it := 1 ; ival := 0 ; nbrstring := '+0000000000000000000' ;
		  WHILE ch = '0' DO nextch ;  (* SKIP LEADING ZEROES *)
		  WHILE ch IN digits DO
		    BEGIN
		      it := it + 1 ;
		      IF it <= maxdigitsinteger THEN
		        nbrstring [it] := ch ;
		      nextch
		    END ;
		  IF ch IN letters THEN
		    IF ch <> 'E' THEN
		      IF ch <> 'e' THEN
		        error (223) ;
		  IF (it > maxdigitsinteger) OR
		    ((it = maxdigitsinteger) AND (nbrstring > maxintegerstring))
		  THEN
		    BEGIN
		      error (203) ;
		      it := 1
		    END
		  ELSE
		    FOR k := 2 TO it DO
		      ival := (ival * 10) + (ord (nbrstring [k]) - ord ('0')) ;
		  exp := it - 1 ;

		  IF ch = '.' THEN
		    BEGIN
		      nextch ;
		      IF ch = '.' THEN dpoint := true ELSE
		        IF ch = ')' THEN ch := ']' ELSE
			BEGIN
			  rval := ival ; cl := 2 ; (* REAL *)
			  IF NOT (ch IN digits) THEN error (201) ELSE
			    BEGIN
			      IF it = 1 THEN
			        WHILE ch = '0' DO
				BEGIN
				  exp := exp - 1 ;
				  nextch
				END ;
			      WHILE ch IN digits DO
			        BEGIN
				it := it + 1 ;
				IF it <= maxdigitsreal THEN nbrstring [it] := ch ;
				nextch
			        END ;
			      IF it > maxdigitsreal THEN warning (219)
			    END
			END
		    END (* ch = '.' *) ;

		  IF ch = 'e' THEN
		    BEGIN
		      nextch ;
		      rval := ival ; cl := 2 ; scale := exp ; exp := 0 ; (* REAL *)
		      sign := false ;
		      IF ch = '+' THEN nextch
		      ELSE
		        IF ch = '-' THEN
			BEGIN
			  nextch ; sign := true ;
			END ;
		      IF NOT (ch IN digits) THEN error (201)
		      ELSE
		        REPEAT
			IF exp < maxexpon THEN
			  exp := (exp * 10) + (ord (ch) - ord ('0')) ;
			nextch
		        UNTIL NOT (ch IN digits) ;
		      IF sign THEN exp := scale - exp
		      ELSE exp := scale + exp ;
		    END (* CH = 'E' *) ;

		  IF cl = 2 THEN              (* CHECK BOUNDS AND CONVERT REAL *)
		    IF it = 1 THEN rval := 0  (* MANTISSA IS ZERO *)
		    ELSE
		      IF (exp > maxexp)
		        OR ((exp = maxexp) AND (nbrstring > maxrealstring)) THEN error (217)
		      ELSE
		        IF (exp < minexp)
			OR ((exp = minexp) AND (nbrstring < minrealstring)) THEN error (218)
		        ELSE
			BEGIN
			  exp := exp - 19 ;
			  convertreal (nbrstring, exp, rval)
			END ;

		END (* CH IN DIGITS *) ELSE
		BEGIN                         (* SPECIAL CHARACTER *)
		  IF ch = '''' THEN           (* ALFA  OR CHAR *)
		    BEGIN
		      no := 2 ; longchaine := 0 ; instring := true ;
		      REPEAT
		        IF eoln (mpcogin) THEN
			BEGIN
			  IF envstandard <> stdextend THEN error (231) ;
			  nextch ; ch := chr (10) ; (* ASCII NEW-LINE *)
			END ELSE
			nextch ;
		        IF ch = '''' THEN     (* ' *)
			BEGIN
			  instring := false ; (* TO OBTAIN  A PASCAL CHAR *)
			  nextch ;
			  instring := ch = '''' ;
			END ELSE
			IF ch = chr (92) THEN
			  IF envstandard = stdsol THEN
			    BEGIN
			      nextch ;
			      IF ch IN ['N', 'n', 'Z', 'z', 'T', 't', 'R', 'r', 'V', 'A'..'F', 'a'..'f',
			        '0'..'9'] THEN
			        BEGIN
				CASE ch OF
				  'N', 'n' : ch := chr (10) ; (* ASCII NEWLINE *)
				  'Z', 'z' : ch := chr (0) ;
				  'T', 't' : ch := chr (9) ; (* HORIZONTAL TABULATION *)
				  'R', 'r' : ch := chr (13) ; (* CARRIAGE RETURN *)
				  'V' : ch := chr (92) ; (* ASCII ANTISLASH *)
				  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
				  'A', 'B', 'C', 'D', 'E', 'F',
				  'a', 'b', 'c', 'd', 'e', 'f' : BEGIN (* HEXADECIMAL DIGIT *)
				      locvalue := 0 ;
				      IF ch IN ['0'..'9'] THEN
				        locvalue := ord (ch) - ord ('0') ELSE
				        IF ch IN ['A'..'F'] THEN
					locvalue := ord (ch) - ord ('A') + 10 ELSE
					locvalue := ord (ch) - ord ('a') + 10 ;
				      locsomme := locvalue * 16 ; (* FIRST DIGIT HEXA *)
				      nextch ;
				      IF ch IN ['0'..'9'] THEN
				        locvalue := ord (ch) - ord ('0') ELSE
				        IF ch IN ['A'..'F'] THEN
					locvalue := ord (ch) - ord ('A') + 10 ELSE
					locvalue := ord (ch) - ord ('a') + 10 ;
				      locsomme := locsomme + locvalue ;

				      IF locsomme <= maxchar THEN
				        ch := chr (locsomme) ELSE
				        error (303) ;

				    END (* HEXADECIMAL DIGIT *) ;
				END (* case CH *) ;
			        END ;
			    END (* chr (92 *) ;
		        IF instring THEN
			BEGIN
			  longchaine := longchaine + 1 ;
			  IF longchaine <= maxval THEN bufval [longchaine] := ch ;
			END ;
		      UNTIL NOT instring ;
		      IF envstandard <> stdextend THEN
		        IF longchaine = 0 THEN
			error (220) ;
		      IF ch = 'x' THEN (* HEXA *) (* HEXADECIMAL *)
		        BEGIN
			IF envstandard = stdpure THEN
			  error (33) ;
			nextch ;
			cl := 1 ;           (* CODE FOR INTEGER *)
			ival := 0 ;
			IF longchaine > maxhexdi THEN error (210) ELSE
			  FOR it := 1 TO longchaine DO
			    BEGIN
			      IF bufval [it] IN digits
			      THEN valhex := ord (bufval [it]) - ord ('0')
			      ELSE

			        IF bufval [it] IN ['A'..'F'] THEN
				valhex := ord (bufval [it]) - ord ('A') + 10 ELSE
				IF bufval [it] IN ['a'..'f'] THEN
				  valhex := ord (bufval [it]) - ord ('a') + 10 ELSE
				  BEGIN
				    error (211) ; valhex := 0 ;
				  END ;
			      append_ (ival, 4, valhex) ;
			    END ;
		        END (* HEXA *) ELSE

		        IF ch = 'o' THEN      (* octal number *)
			BEGIN
			  IF envstandard = stdpure THEN
			    error (32) ;
			  nextch ; cl := 1 ; (* integer *) ival := 0 ; no := 2 ;
			  IF longchaine > maxdig + 1 THEN error (207) ELSE
			    FOR it := 1 TO longchaine DO
			      BEGIN
			        valhex := ord (bufval [it]) - ord ('0') ;
			        IF NOT (valhex IN [0..7]) THEN
				BEGIN error (204) ; valhex := 0 ;
				END ;
			        append_ (ival, 3, valhex) ;
			      END (* for it *) ;
			END (* octal number *) ELSE
			IF ch = 'b' THEN    (* binary *)
			  BEGIN
			    nextch ; cl := 1 ; (* integer *) ; ival := 0 ; no := 2 ;
			    IF envstandard = stdpure THEN
			      error (33) ;
			    IF longchaine > bitsinword THEN error (215) ELSE
			      FOR it := 1 TO longchaine DO
			        BEGIN
				valhex := ord (bufval [it]) - ord ('0') ;
				IF NOT (valhex IN [0..1]) THEN
				  BEGIN error (216) ; valhex := 0 ;
				  END ;
				append_ (ival, 1, valhex) ;
			        END ;       (* FOR IT *)
			  END ELSE
			  IF longchaine = 1 THEN (* CHAR *) (* CHAR *)
			    BEGIN
			      cl := 4 ; ival := ord (bufval [1]) ;
			    END (* CHAR *) ELSE (* STRING *)
			    BEGIN           (* ALFA *)
			      cl := 3 ;
			      IF longchaine > maxval THEN
			        BEGIN
				error (209) ; longchaine := maxval ;
			        END ;
			    END ;           (* ALFA *)
		      IF longchaine >= longpad THEN longpad := longchaine ELSE
		        REPEAT                (* PADDING WITH SPACES *)
			bufval [longpad] := ' ' ; longpad := longpad - 1 ;
		        UNTIL longpad = longchaine ;
		    END (* ALFA OR CHAR *) ELSE
		    BEGIN                     (* OTHER CHARS *)
		      no := symno [ord (ch)] ; (* SINGLE CHAR *)
		      cl := symcl [ord (ch)] ;
		      ival := 0 ;
		      IF NOT (eof (mpcogin) AND (sourcenbr = 0)) THEN
		        BEGIN
			ch1 := ch ;
			nextch ;
                                                  (* TEST FOR DOUBLE CHARS *)
			IF ch1 = ':' THEN
			  BEGIN
			    IF ch = '=' THEN (* := *)
			      BEGIN
			        no := 20 ; nextch ;
			      END ;
			  END ELSE
			  IF ch1 = '.' THEN
			    BEGIN
			      IF ch = '.' THEN (* .. *)
			        BEGIN
				no := 39 ; nextch ;
			        END ELSE
			        IF ch = ')' THEN (* .) *)
				BEGIN
				  no := 12 ; nextch ;
				END ;
			    END ELSE
			    IF ch1 = '-' THEN
			      BEGIN
			        IF envstandard = stdextend THEN
				IF ch = '>' THEN (* CRISS EXTENSION *)
				  BEGIN
				    no := 49 ; cl := 0 ; nextch ;
				  END ;
			      END ELSE
			      IF ch1 = '<' THEN
			        BEGIN
				IF ch = '=' THEN (* <= *)
				  BEGIN   (* NO=8 *)
				    cl := 2 ; nextch ;
				  END ELSE
				  IF ch = '>' THEN (* <> *)
				    BEGIN
				      cl := 5 ; nextch ;
				    END ;
			        END ELSE
			        IF ch1 = '>' THEN
				BEGIN
				  IF ch = '=' THEN (* >= *)
				    BEGIN (* NO=8 *)
				      cl := 3 ; nextch ;
				    END ;
				END ELSE
				IF ch1 = '/' THEN
				  BEGIN
				    IF (ch = ')') THEN (* NS *) (* / ) *)
				      BEGIN
				        error (70) ;
				        nextch ;
				        GOTO 1 ; (* FOLLOWING MECHANISM IS OBSOLETE AND SKIPPED *)
				        IF envstandard <> stdextend THEN error (70) ;
				        nextch ;
				        IF nbccond = 0 THEN error (208)
				        ELSE nbccond := nbccond - 1 ;
				        GOTO 1 ; (* BEGINNING OF INSYMBOL *)
				      END ;
				  END ELSE
				  IF ch1 = '(' THEN
				    BEGIN
				      IF ch = '.' THEN
				        BEGIN (* (. *)
					no := 11 ; nextch ;
				        END ELSE
				        IF ch = '*' THEN
					BEGIN (* (* *)
					  nextch ;
					  combraces := false ;
					  incomment := true ;
					END ;
				    END ELSE
				    IF ch1 = '{' THEN (* COMMENT WITH BRACE *)
				      BEGIN
				        combraces := true ;
                                                  (* NEXTCH HAS BEEN DONE *)
5 :				        incomment := true ;
				      END ; (* COMMENT *)
		        END ;                 (* OTHER CHARS *)
		    END                       (* NOT EOF(MPCOGIN) *)
		END ;                         (* SPECIAL CHARS *)
	    END ;                               (* NOT DPOINT *)

	  IF incomment THEN
	    BEGIN
	      IF envstandard = stdpure THEN
	        REPEAT
		WHILE NOT (ch IN ['}', '*']) DO nextch ;
		fin := ch = '}' ;
		IF NOT fin THEN
		  BEGIN
		    nextch ; fin := ch = ')'
		  END
	        UNTIL fin
	      ELSE
	        IF combraces THEN               (* COMMENT WITH BRACES *)
		WHILE ch <> '}' DO nextch
	        ELSE
		REPEAT
		  WHILE ch <> '*' DO nextch ;
		  nextch ; fin := ch = ')' ;
		UNTIL fin ;
	      incomment := false ;
	      nextch ;
	      GOTO 1 ;                          (* RESTART INSYMBOL *)
	    END ;
	END ;

$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ RETOUR INSYMBOL @@@ WITH NO,CL', no : 4, cl : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* INSYMBOL *) ;


$OPTIONS page $

    PROCEDURE skip (nosymb : integer) ; FORWARD ;
    PROCEDURE skipextd (nosymb : setofno) ; FORWARD ;

(* *************************************************** TRAITEINCLUDE ******************** *)

    PROCEDURE traiteinclude ;

(* c CALLED BY INSYMBOL WHEN $INCLUDE DIRECTIVE HAS BEEN ENCOUNTERED      C *)

(* E
   35 : MAX LENGTH FOR EXTERNAL IS 168 CHARS
   38 : ',' OR '$' EXPECTED
   39 : STRING OR '*' EXPECTED
   40 : '$' EXPECTED
   41 : THIS STRING CANNOT BE > 32 CHARS                                        E *)

      LABEL
        10 ;                                      (* EXIT ON ERROR *)

      VAR
        filename : externid ;                     (* NAME OF INCLUDE FILE *)
        stringdeb, stringfin : alfaid ;           (* OPTIONAL BEGIN AND END STRINGS *)
        it, ldeb, lfin : integer ;

      BEGIN                                       (* TRAITEINCLUDE *)
$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT TRAITEINCLUDE @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        insymbol ;                                (* FILENAME STRING *)
        IF NOT ((no = 2) AND (cl = 3)) THEN
	BEGIN
	  error (19) ;
	  skip (55) ;
	  GOTO 10
	END ;
        IF longchaine > maxexternname THEN
	BEGIN
	  error (35) ;
	  longchaine := maxexternname
	END ;
        filename := '  ' ;
        FOR it := 1 TO longchaine DO
	filename [it] := bufval [it] ;
        stringdeb := '  ' ;
        stringfin := '  ' ;
                                                  (* CHECK FOR OPTIONNAL STRINGS *)
        WHILE ch = ' ' DO nextch ;
        IF ch = '$' THEN
	BEGIN
	  stringdeb := '* ' ; ldeb := 1 ;
	  stringfin := '* ' ; lfin := 1
	END
        ELSE
	BEGIN
	  insymbol ;
	  IF no <> 15 THEN
	    BEGIN
	      error (38) ;
	      skip (55) ;
	      GOTO 10
	    END ;
	  insymbol ;                            (* '*' OR STRING EXPECTED *)
	  IF (no = 6) AND (cl = 1) THEN         (* '*' *)
	    BEGIN
	      stringdeb := '* ' ;
	      ldeb := 1
	    END
	  ELSE
	    BEGIN
	      IF NOT ((no = 2) AND (cl = 3)) THEN
	        BEGIN
		error (39) ;
		skip (55) ;
		GOTO 10
	        END ;
	      IF longchaine > maxident THEN
	        BEGIN
		error (41) ;
		longchaine := maxident
	        END ;
	      FOR it := 1 TO longchaine DO
	        stringdeb [it] := bufval [it] ;
	      ldeb := longchaine ;
	    END ;
	  WHILE ch = ' ' DO nextch ;
	  IF ch = '$' THEN
	    BEGIN
	      stringfin := '* ' ;
	      lfin := 1
	    END
	  ELSE
	    BEGIN
	      insymbol ;
	      IF no <> 15 THEN
	        BEGIN
		error (38) ;
		skip (55) ;
		GOTO 10
	        END ;
	      insymbol ;                        (* '*' OR STRING EXPECTED *)
	      IF (no = 6) AND (cl = 1) THEN     (* '*' *)
	        BEGIN
		stringfin := '* ' ;
		lfin := 1
	        END
	      ELSE
	        BEGIN
		IF NOT ((no = 2) AND (cl = 3)) THEN
		  BEGIN
		    error (39) ;
		    skip (55) ;
		    GOTO 10
		  END ;
		IF longchaine > maxident THEN
		  BEGIN
		    error (41) ;
		    longchaine := maxident
		  END ;
		FOR it := 1 TO longchaine DO
		  stringfin [it] := bufval [it] ;
		lfin := longchaine ;
	        END ;
	      WHILE ch = ' ' DO nextch ;
	      IF ch <> '$' THEN
	        BEGIN
		error (40) ;
		skip (55) ;
		GOTO 10
	        END
	    END
	END ;
        beginsource (filename, stringdeb, ldeb, stringfin, lfin) ; (* BEGIN INCLUDE FILE *)
        reset (mpcogin) ;
        nextch ;
10 :
$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN TRAITEINCLUDE @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* TRAITEINCLUDE *) ;


$OPTIONS page $

(* ***************************************** TRAITEOPTIONS ************************************** *)

    PROCEDURE traiteoptions ;

(* C CALLED BY INSYMBOL WHEN "$OPTIONS" DIRECTIVE IS ENCOUNTERED       C *)

(* E ERRORS DETECTED ARE :

   47 : OPTION IDENTIFIER EXPECTED
   16 : "=" EXPECTED
   50 : "$" OR ";" EXPECTED
   49 : "+" OR "-" EXPECTED
   34 : CONDITION IDENTIFIER EXPECTED
   35 : "," OR ";" OR "$" EXPECTED
   48 : UNKNOWN OPTION
   15 : INTEGER EXPECTED

   E *)

      LABEL
        1, 3, 4, 5, 10 ;

      VAR
        ch : char ;
        flag : boolean ;
        work : condaddr ;

(* ************************************************* SKIPOPTION < TRAITEOPTIONS ************************* *)

      PROCEDURE skipoption (errno : integer) ;

        BEGIN                                     (* SKIPOPTION *)
	error (errno) ;
	IF no = 55 THEN GOTO 10 ;
	IF no = 16 THEN GOTO 1 ;
	skipextd ([16, 55]) ;
	IF no = 55 THEN GOTO 10
	ELSE GOTO 1 ;
        END (* SKIPOPTION *) ;

(* ************************************************ CHECKPLUSMINUS < TRAITEOPTIONS ********************** *)

      FUNCTION checkplusminus : boolean ;

        BEGIN                                     (* CHECKPLUSMINUS *)
	insymbol ;
	checkplusminus := false ;
	IF (no = 7) AND (cl = 1) THEN checkplusminus := true
	ELSE IF NOT ((no = 7) AND (cl = 2)) THEN skipoption (49) ;
        END (* CHECKPLUSMINUS *) ;

(* ******************************************* FINDCOND < TRAITEOPTIONS ******************** *)

      PROCEDURE findcond ;

        LABEL 5 ;

        BEGIN
	work := firstcond ;
5 :
	IF work <> NIL THEN
	  IF work^.condname <> aval THEN
	    BEGIN
	      work := work^.nextcond ;
	      GOTO 5 ;
	    END ;
        END ;                                     (* findcond *)

(* ****************************** CHECKVALUE < TRAITEOPTIONS ******************* *)

      FUNCTION checkvalue : boolean ;

        VAR
	invert : boolean ;
        BEGIN
	insymbol ;
	IF no <> 1 THEN
	  IF NOT ((no = 5) AND (cl = 1)) THEN skipoption (314)
	  ELSE
	    BEGIN
	      invert := true ;
	      insymbol ;
	      IF no <> 1 THEN skipoption (316)
	    END
	ELSE invert := false ;
	IF aval = 'true' THEN checkvalue := true
	ELSE IF aval = 'false' THEN checkvalue := false
	  ELSE BEGIN
	      findcond ;
	      IF work = NIL THEN skipoption (315) ;
	      checkvalue := work^.active ;
	    END ;
	IF invert THEN checkvalue := NOT checkvalue ;
        END (* CHECKVALUE *) ;

(* ******************************* CREATECOND < TRAITEOPTIONS ***************************** *)

      PROCEDURE createcond ;

        BEGIN
	new (work) ;
	IF work = NIL THEN heaperror ;
	WITH work^ DO
	  BEGIN
	    nextcond := firstcond ;
	    condname := aval ;
	    setinargs := false ;
	    active := false ;
	    activated := false ;
	  END ;
	firstcond := work ;

        END (* CREATECOND *) ;

      BEGIN                                       (* TRAITEOPTION *)

$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT TRAITEOPTIONS @@@') ; nextline ;
	END ;
$OPTIONS compile = true $

1 :
        insymbol ;
        IF no <> 1 THEN skipoption (47) ;
        ch := aval [1] ;
        IF NOT (ch IN ['w', 'l', 't', 'e', 'c', 'p', 'd', 's']) THEN skipoption (48) ;
        CASE ch OF
	'w' : BEGIN
	    IF cl <> 3 THEN skipoption (48) ;
	    IF NOT (aval [2] IN ['d', 's', 'g']) THEN skipoption (48) ;
	    IF NOT (aval [3] IN ['0'..'3']) THEN skipoption (48) ;
	    CASE aval [2] OF
	      'd' : IF (NOT wdsetinargs) THEN tracelevel (decltrace, aval [3]) ;
	      's' : IF (NOT wssetinargs) THEN tracelevel (stattrace, aval [3]) ;
	      'g' : IF (NOT wgsetinargs) THEN BEGIN
		  tracelevel (genetrace, aval [3]) ;
		  outcode := writecode OR (genetrace > none) ;
		END ;
	    END (* CASE *) ;
	    insymbol ;
	  END ;
	'l' : IF aval = 'l ' THEN
	    BEGIN
	      flag := checkplusminus ;
	      IF NOT skipcode THEN
	        BEGIN
		listyes := flag ;
		writecode := writecode AND listyes ;
		outcode := writecode OR (genetrace > none) ;
	        END ;
	      insymbol ;
	    END
	  ELSE
	    IF aval = 'll' THEN
	      BEGIN
	        insymbol ;
	        IF NOT ((no = 8) AND (cl = 6)) THEN skipoption (16) ;
	        insymbol ;
	        IF NOT ((no = 2) AND (cl = 1)) THEN skipoption (15) ;
                                                  (* WARNING : LL INEFFECTIVE *)
	        warning (222) ;
	        insymbol ;
	      END
	    ELSE IF aval = 'listing' THEN
	        BEGIN
		insymbol ;
		IF NOT ((no = 8) AND (cl = 6)) THEN skipoption (16) ;
		listyes := checkvalue ;
		writecode := writecode AND listyes ;
		outcode := writecode OR (genetrace > none) ;
		insymbol
	        END
	      ELSE skipoption (48) ;
	't' : IF aval = 't ' THEN
	    BEGIN
	      flag := checkplusminus ;
	      IF (NOT skipcode) AND (NOT tsetinargs) THEN
	        BEGIN
		divcheck := flag ;
		asscheck := divcheck ;
		inxcheck := divcheck ;
	        END ;
	      insymbol ;
	    END
	  ELSE skipoption (48) ;
	'p' : IF aval = 'page' THEN
	    BEGIN
	      IF NOT skipcode THEN skippage := true ;
	      insymbol ;
	    END
	  ELSE IF aval = 'p ' THEN
	      BEGIN
	        flag := checkplusminus ;
                                                  (* WARNING : P INEFFECTIVE *)
	        warning (222) ;
	        insymbol ;
	      END
	    ELSE skipoption (48) ;
	'e' : IF aval = 'ec' THEN
	    BEGIN
	      flag := checkplusminus ;
                                                  (* WARNING : EC INEFFECTIVE *)
	      warning (222) ;
	      insymbol ;
	    END
	  ELSE skipoption (48) ;
	'c' : IF aval = 'cond' THEN
	    BEGIN
	      insymbol ;
	      IF NOT ((no = 8) AND (cl = 6)) THEN skipoption (16) ;
3 :
	      insymbol ;
	      IF no <> 1 THEN
	        IF NOT (no IN wnoset) THEN skipoption (34)
	        ELSE
		IF ((no = 6) AND (NOT (cl IN [3, 4, 5])))
		  OR ((no = 7) AND (cl <> 3))
		  OR ((no = 8) AND (cl <> 7)) THEN skipoption (34) ;
	      findcond ;
	      IF work = NIL THEN
	        createcond ;
	      flag := checkplusminus ;
	      WITH work^ DO
	        IF NOT setinargs THEN active := flag ;
	      insymbol ;
	      IF NOT (no IN [15, 16, 55]) THEN skipoption (35) ;
	      IF no = 15 THEN GOTO 3 ;
	    END
	  ELSE IF aval = 'cc' THEN
	      BEGIN
	        insymbol ;
	        IF NOT ((no = 8) AND (cl = 6)) THEN skipoption (16) ;
4 :
	        insymbol ;
	        IF no <> 1 THEN
		IF NOT (no IN wnoset) THEN skipoption (34)
		ELSE
		  IF ((no = 6) AND (NOT (cl IN [3, 4, 5])))
		    OR ((no = 7) AND (cl <> 3))
		    OR ((no = 8) AND (cl <> 7)) THEN skipoption (34) ;
	        findcond ;
	        IF work = NIL THEN
		createcond ;
	        work^.activated := checkplusminus ;
	        insymbol ;
	        IF NOT (no IN [15, 16, 55]) THEN skipoption (35) ;
	        IF no = 15 THEN GOTO 4 ;
	        work := firstcond ;
	        skipcode := false ;
	        WHILE work <> NIL DO
		BEGIN
		  skipcode := skipcode OR ((NOT work^.active) AND work^.activated) ;
		  work := work^.nextcond ;
		END ;
	      END
	    ELSE IF aval = 'compile' THEN
	        BEGIN
		insymbol ;
		IF NOT ((no = 8) AND (cl = 6)) THEN skipoption (16) ;
		skipcode := NOT checkvalue ;
		insymbol ;
	        END
	      ELSE skipoption (48) ;
	's' : IF aval = 'switch' THEN
	    BEGIN
5 :
	      insymbol ;
	      IF no <> 1 THEN skipoption (316) ;
	      findcond ;
	      IF work = NIL THEN createcond ;
	      insymbol ;
	      IF no = 20 THEN                   (* := *)
	        BEGIN
		flag := checkvalue ;
		insymbol ;
	        END
	      ELSE flag := false ;              (* default *)
	      WITH work^ DO
	        IF NOT setinargs THEN active := flag ;
	      IF NOT (no IN [15, 16, 55]) THEN skipoption (36) ;
	      IF no = 15 THEN GOTO 5 ;
	    END
	  ELSE skipoption (48) ;
	'd' : IF aval = 'debug' THEN
	    BEGIN
	      insymbol ;                        (* = *)
	      IF NOT ((no = 8) AND (cl = 6)) THEN skipoption (16) ;
	      flag := checkvalue ;
	      IF (NOT tsetinargs) THEN
	        BEGIN
		divcheck := flag ;
		asscheck := divcheck ;
		inxcheck := divcheck ;
	        END ;

	      insymbol ;
	    END
	  ELSE skipoption (48) ;
        END (* CASE *) ;
        IF no = 16 THEN GOTO 1 ;
        IF no <> 55 THEN skipoption (50) ;
10 :
        IF skipcode THEN
	BEGIN
	  REPEAT
	    insymbol ;
	  UNTIL (no = 57) ;
	  GOTO 1 ;
	END ;

$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN TRAITEOPTIONS @@@') ; nextline ;
	END ;
$OPTIONS compile = true $

      END (* TRAITEOPTION *) ;


$OPTIONS page $

(* **************************** STATEMENT BEGINS **************************** *)

    PROCEDURE statement_begins (genp : boolean) ;

      BEGIN
        IF ic <> startic THEN
	BEGIN
	  sttplace := ic ;
	  IF genp THEN
	    IF profile THEN genprofileref
	    ELSE IF longprofile THEN genlongprofileref ;
	END ;
        sttindex := symbolindex ;
        sttline := symbolline ;
        sttfile := symbolfile ;
        startic := ic ;
        end_statement := false ;
      END (* STATEMENT BEGINS *) ;

$OPTIONS page $

(* ****************************** STATEMENT ENDS **************************** *)

    PROCEDURE statement_ends (sttlength : integer) ;

      VAR
        locic : integer ;

      BEGIN
        IF NOT end_statement THEN
	IF ic <> startic THEN
	  BEGIN
	    statnbr := statnbr + 1 ;
	    WITH mapptr^[statnbr] DO
	      BEGIN
	        IF (oldline = sttline)
		AND (oldic <> ic)
		AND (oldfile = sttfile)
		AND (oldindex <> sttindex) THEN
		sttinline := sttinline + 1
	        ELSE
		BEGIN
		  sttinline := 1 ;
		  oldfile := sttfile ;
		  oldline := sttline ;
		END ;
	        oldic := ic ;
	        oldindex := sttindex ;
	        word1 := (sttfile * twoto10) + (sttline DIV twoto4) ;
	        locic := sttplace DIV bytesinword ;
	        insert_ (locic, 18, word1) ;
	        word2 := (sttinline * twoto27) + (sttindex * twoto9) + (sttlength MOD 256) ;
	        insert_ ((sttline MOD twoto4), 32, word2) ;
	      END ;
	    end_statement := true ;
	  END ;
      END (* STATEMENT ENDS *) ;
$OPTIONS page $

(* *************************************************** NAMEISREF ************************ *)

    PROCEDURE nameisref (box : ctp ; fil, lin : integer) ;

(* C FILLS REF STRUCTURE WHEN NAME IS REFERENCED                      C *)

      VAR
        refbox : refptr ;

      BEGIN
        IF NOT building_from_schema.on THEN
	WITH box^ DO
	  BEGIN
	    IF klass = vars THEN visrefincode := (NOT declarationpart)
	    ELSE IF klass = proc THEN pisrefincode := (NOT declarationpart) AND (NOT procisactive) ;
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
		    filen := fil ;
		    linen := lin ;
		    IF (environt = code) AND (NOT declarationpart) THEN sttmapind := statnbr * 2
		    ELSE IF lin < 0 THEN sttmapind := -1
		      ELSE sttmapind := -2 ;
		  END ;
	      END ;
	  END ;

      END (* NAMEISREF *) ;

$OPTIONS page $

(* ***********************************************SKIP************************* *)

    PROCEDURE skip ;

(* C   THIS PROCEDURE IS USED  FOR ERROR'S  RECOVERY  MECHANISM.
   SKIPS ALL IRRELEVANT SYMBOLS DEFINED IN ERRCL
   STOPS ON  BEGSYMBOL,  ENDSYMBOL   OR  SPECIFIED 'NOSYMB'
   C *)
      BEGIN
$OPTIONS compile = trace $
        IF anytrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT SKIP @@@  WITH  NOSYMB= ', nosymb : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
        WHILE (errcl [no] = irrelsy) AND (nosymb # no) AND NOT eof (mpcogin) DO
	IF (no = 38) AND (cl = 2) (* RECORD *) THEN
	  BEGIN
	    REPEAT
	      insymbol ; skip (46) ;            (* NON ASSIGNED VALUE *)
	    UNTIL NOT (no IN [16, 26]) ;        (* ; CASE *)
	    IF no = 22 (* END *) THEN
	      insymbol ;
	  END ELSE
	  insymbol ;
$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN SKIP  @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* SKIP *) ;

$OPTIONS page $

(* ***********************************************      SKIPEXTD       *)

    PROCEDURE skipextd ;

(* C   THIS PROCEDURE IS USED  FOR ERROR'S  RECOVERY  MECHANISM.
   SKIPS ALL IRRELEVANT SYMBOLS DEFINED IN ERRCL
   STOPS ON  BEGSYMBOL,  ENDSYMBOL   OR  SPECIFIED 'NOSYMB'S
   C *)
      VAR
        it : integer ;

      BEGIN                                       (* SKIPEXTD *)
$OPTIONS compile = trace $
        IF anytrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT SKIPEXTD @@@  WITH  NOSYMB= ') ;
	  FOR it := minno TO maxno DO
	    IF it IN nosymb THEN
	      write (mpcogout, it : 4) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        WHILE (errcl [no] = irrelsy) AND NOT (no IN nosymb) DO
	IF (no = 38) AND (cl = 2) (* RECORD *) THEN
	  BEGIN
	    REPEAT
	      insymbol ; skip (46) ;            (* NON ASSIGNED VALUE *)
	    UNTIL NOT (no IN [16, 26]) ;        (* ; CASE *)
	    IF no = 22 (* END *) THEN
	      insymbol ;
	  END ELSE
	  insymbol ;
$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN SKIPEXTD  @@@ with NO', no : 4) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* SKIPEXTD *) ;

$OPTIONS page $

(* ***********************************************      SKIPTOCHAPTER       *** *)

    PROCEDURE skiptochapter ;

(* C   THIS PROCEDURE IS USED  FOR ERROR'S  RECOVERY  MECHANISM.
   SKIPS ALL IRRELEVANT SYMBOLS
   STOPS ON PROGRAM $RENAME $IMPORT $EXPORT LABEL CONST
   TYPE VAR $VALUE PROCEDURE FUNCTION BEGIN
   C *)
      BEGIN                                       (* SKIPTOCHAPTER *)
$OPTIONS compile = trace $
        IF anytrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT SKIPTOCHAPTER @@@  ') ; nextline ;
	END ;
$OPTIONS compile = true $
        WHILE NOT (no IN [50, 51, 52, 53, 40, 41, 37, 43, 54, 44, 45, 21]) DO
	insymbol ;
$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN SKIPTOCHAPTER  @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* SKIPTOCHAPTER *) ;


$OPTIONS page $

(* ***********************************************INCONST********************** *)

    PROCEDURE inconst (VAR code : integer ; VAR restype : ctp ; fnxt : ctp ; expression_allowed : boolean) ;

(* C  THIS PROCEDURE IS CALLED IN ORDER  TO ANALYSE  A CONSTANTE
   .CODE       IS A CODE  FOR THE  CONSTANTE
   1    VALUE IN "CONINT"                        5:SCALAR
   2    VALUE  IN "CONREEL"                      0:ERROR
   3    VALUE IN "BUFVAL" WITH LENGTH "LONGSTRING"
   4    VALUE  IN "CONINT"  (CHARPTR)
   .RESTYPE    TYPE OF CONSTANTE
   .FNXT     IS  CTP BEGINNING OF SEARCH  IN  CONTEXTTABLE              C *)
(* E    50  ERROR IN CONSTANT
   60  OR NOT ALLOWED AS MONADIC OPERATOR
   103  IDENTIFIER IS NOT OF APPROPRIATE CLASS
   104  IDENTIFIER NOT DECLARED
   144 : ILLEGAL TYPE OF EXPRESSION
   225 : THIS EXPRESSION CANNOT BE EVALUATED HERE : IT NEEDS CODE GENERATION
   105  SIGN NOT ALLOWED                                                  E *)
      VAR
        sign, isno7 : boolean ;
        savectptr : ctp ;
        it, jt : integer ;
        curbox : alfapt ;
        whattrace : levtrace ;
      BEGIN
$OPTIONS compile = trace $
        IF decltrace > stattrace THEN whattrace := decltrace ELSE
	whattrace := stattrace ;
        IF whattrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT INCONST @@@  FNXT IS ', ord (fnxt)) ; nextline ;
	  IF whattrace = high THEN
	    BEGIN
	      write (mpcogout, ' GLOBALS NO,CL,IVAL,RVAL ARE ', no : 4, cl : 4, ival, rval) ;
	      nextline ;
	    END ;
	END ;
$OPTIONS compile = true $
        restype := NIL ;                          (* DEFAULT = ERROR *)
        IF expression_allowed AND (envstandard = stdextend) THEN
	BEGIN
	  illegal_generation := false ;
	  initattrvarbl (gattr) ; freeallregisters ;
	  expression ;
	  IF illegal_generation THEN error (225) ;
	  WITH gattr DO
	    IF (NOT (kind IN [sval, chain])) OR (typtr = NIL) THEN
	      BEGIN
	        IF (NOT illegal_generation) AND (typtr <> NIL) THEN error (225) ;
	        restype := NIL ; conint := 0
	      END
	    ELSE
	      IF kind = sval THEN
	        BEGIN
		IF (typtr = intptr) OR
		  (typtr^.form = scalar) OR
		  (typtr = charptr) THEN
		  BEGIN
		    restype := typtr ; conint := val
		  END ELSE
		  IF typtr = realptr THEN
		    BEGIN
		      restype := realptr ; conreel := rsval
		    END ELSE
		    BEGIN
		      error (144) ;
		      restype := NIL ; conint := 0
		    END
	        END ELSE
	        BEGIN
		restype := alfaptr ;
		longstring := 0 ;
		IF alfactp <> NIL THEN
		  WITH alfactp^ DO
		    BEGIN
		      curbox := alfadeb ; longstring := 0 ;
		      FOR it := 1 TO alfalong DIV longalfbox DO
		        BEGIN
			FOR jt := 1 TO longalfbox DO
			  BEGIN
			    bufval [longstring + jt] := curbox^.alfaval [jt] ;
			  END ;
			longstring := longstring + longalfbox ;
			curbox := curbox^.nextval ;
		        END ;
		      FOR it := 1 TO alfalong MOD longalfbox DO
		        BEGIN
			longstring := longstring + 1 ;
			bufval [longstring] := curbox^.alfaval [it] ;
		        END ;
		    END
	        END
	END
        ELSE
	BEGIN                                   (* NOT EXPRESSION *)
	  IF no = 7 (* + - OR *) THEN
	    BEGIN
	      sign := cl = 2 ;
	      IF cl = 3 THEN error (60) ;
	      isno7 := true ;
	      insymbol ;
	    END ELSE
	    BEGIN
	      sign := false ;
	      isno7 := false ;
	    END ;
	  IF no = 2 THEN                        (* EXPLICIT CONST *)
	    BEGIN
	      IF (cl > 2) AND isno7 THEN error (105) ;
	      CASE cl OF
	        1 : BEGIN
		  restype := intptr ;
		  IF sign THEN conint := -ival ELSE conint := ival ;
		END ;
	        2 : BEGIN
		  restype := realptr ;
		  IF sign THEN conreel := -rval ELSE conreel := rval ;
		END ;
	        3 : BEGIN
		  restype := alfaptr ;
		  longstring := longchaine ;
		END ;
	        4 : BEGIN
		  restype := charptr ;
		  conint := ival ;
		END ;
	      END (* CASE *) ;
	      insymbol ;
	    END (* NO=2 *) ELSE
	    IF no = 1 (* CONSTANT IDENTIFIER *) THEN
	      BEGIN
	        savectptr := ctptr ;
	        srchrec (fnxt) ;
	        IF ctptr = NIL THEN search ;
	        IF ctptr = NIL THEN error (104) ELSE
		BEGIN
		  IF symbolmap THEN nameisref (ctptr, symbolfile, symbolline) ;
		  WITH ctptr@ DO
		    BEGIN                     (* IDENTIFIER FOUND IN CONTEXTABLE *)
		      IF klass # konst THEN error (103) ELSE
		        BEGIN
			restype := contype ;
			IF restype = intptr THEN
			  IF sign THEN conint := -values ELSE conint := values
			ELSE
			  IF restype = realptr THEN
			    IF sign THEN conreel := -valreel ELSE conreel := valreel
			  ELSE
			    BEGIN           (* CHAR,SCALAR OR ALFA CONST *)
			      IF isno7 THEN error (105) ;
			      IF restype = alfaptr THEN
			        BEGIN
				curbox := alfadeb ; longstring := 0 ;
				FOR it := 1 TO alfalong DIV longalfbox DO
				  BEGIN
				    FOR jt := 1 TO longalfbox DO
				      BEGIN
				        bufval [longstring + jt] := curbox@.alfaval [jt] ;
				      END ;
				    longstring := longstring + longalfbox ;
				    curbox := curbox@.nextval ;
				  END ;
				FOR it := 1 TO alfalong MOD longalfbox DO
				  BEGIN
				    longstring := longstring + 1 ;
				    bufval [longstring] := curbox@.alfaval [it] ;
				  END ;
			        END (* ALFA *) ELSE (* CHAR OR SCALAR *)
			        conint := values ;
			    END ;           (* CHAR,SCALAR OR ALFA *)
		        END ;                 (* KONST *)
		    END ;                     (* WITH CTPTR *)
		END ;
	        ctptr := savectptr ;            (* RESTORE CTPTR *)
	        insymbol ;
	      END (* IDENTIFIER *) ELSE
	      error (50) ;
	END (* NOT EXPRESSION *) ;
        IF restype = NIL THEN code := 0 ELSE
	IF restype = intptr THEN code := 1 ELSE
	  IF restype = realptr THEN code := 2 ELSE
	    IF restype = alfaptr THEN code := 3 ELSE
	      IF restype = charptr THEN code := 4 ELSE
	        code := 5 ;
$OPTIONS compile = trace $
        IF whattrace > low THEN
	BEGIN
	  IF whattrace = high THEN
	    BEGIN
	      write (mpcogout, 'GLOBALS  CONINT,CONREEL ,LONGSTRING  ARE', conint, conreel,
	        longstring) ; nextline ;
	    END ;
	  write (mpcogout, ' @@@ FIN INCONST @@@ WITH  V.CODE,V.RESTYPE = ', code : 3,
	    ord (restype)) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* INCONST *) ;





$OPTIONS page $

(* ***********************************************CARTEEXEC ******************* *)

    PROCEDURE carteexec ;

(* C  MUST BE CALLED  AFTER INITIALISE ;  BEFORE  FIRST INSYMBOL;
   SCANS  $PARM   IN ORDER   TO
   ASSIGN   TRACE'S VARIABLES
   STANDARD
   NOTRACE
   SYMBTABL
   XREFNEED                                                   C *)
      CONST
        count = 100 ;
      VAR
        lfound : boolean ;
        lch : char ;
        lastread : integer ;
        parmlist : PACKED ARRAY [1..count] OF char ;


(* *************************************SCANPARM < CARTEEXEC ****************** *)

      PROCEDURE scanparm (fstring : alfa ; flong : integer ; VAR strisfound : boolean ;
        VAR nextchar : char) ;

(* C  SCANS IN PARMLIST ON 'FSTRING'  ON'FLONG'  CHARS;
   IF  FOUND   RETURNS 'STRISFOUND ' TRUE    AND   THE  NEXTCHAR
   IF  NOT     RETURNS               FALSE
   C *)
        LABEL
	1 ;                                     (* EXIT  LOOP FOR *)
        VAR
	i, j : integer ;
	lalf : alfa ;
        BEGIN
                                                  (*  DEFAULT  VALUES *)
	strisfound := false ; nextchar := ' ' ;
	lalf := blank ;
	FOR i := 0 TO count - flong DO
	  BEGIN
	    FOR j := 1 TO flong DO
	      lalf [j] := parmlist [i + j - 1] ;
	    IF lalf = fstring THEN
	      BEGIN
	        strisfound := true ;
	        IF i <= (count - flong) THEN
		BEGIN lastread := i + flong ;
		  nextchar := parmlist [lastread] ;
		END ;
	        GOTO 1 ;                        (* EXIT  LOOP(S) *)
	      END ;
	  END ;
1 :                                               (* EXIT  LOOP  FOR *)
        END (* SCANPARM *) ;


      BEGIN                                       (* CARTEEXEC *)
        checks := true ;
        argv (1, parmlist) ;
        scanparm ('FAST    ', 4, lfound, lch) ;
        IF lfound THEN fastoperator := true ;
        scanparm ('PRCODE  ', 6, lfound, lch) ;
        IF lfound THEN codelist := true ;
        scanparm ('REFS    ', 4, lfound, lch) ;
        IF lfound THEN symbolmap := true ;
        scanparm ('LIST    ', 4, lfound, lch) ;
        IF lfound THEN
	BEGIN
	  listyes := true ;
	  symbolmap := true ;
	  symbol_listing := true ;
	  mapswitch := true
	END ;
        scanparm ('SKIPCODE', 8, lfound, lch) ;
        IF lfound THEN
	skipcode := true ;                      (* CONDITIONAL  COMPILATION  *)
        scanparm ('NOCHECKS', 8, lfound, lch) ;
        IF lfound THEN
	BEGIN
	  checks := false ;
	  tsetinargs := true ;
	END ;
        scanparm ('NOSTAND ', 7, lfound, lch) ;
        IF lfound THEN
	envstandard := stdextend ;
        scanparm ('STDSOL', 6, lfound, lch) ;
        IF lfound THEN
	BEGIN
	  envstandard := stdsol ;
	END ;
        scanparm ('STRACE= ', 7, lfound, lch) ;
        IF lfound THEN
	BEGIN
	  tracelevel (stattrace, lch) ;
	  wssetinargs := true ;
	END ;
        scanparm ('DTRACE= ', 7, lfound, lch) ;
        IF lfound THEN
	BEGIN
	  tracelevel (decltrace, lch) ;
	  wdsetinargs := true ;
	END ;
        scanparm ('INTER', 5, lfound, lch) ;
        IF lfound THEN
	interactive := true ;
        scanparm ('NOIOW', 5, lfound, lch) ;
        IF lfound THEN
	iowarnings := false ;
        scanparm ('GTRACE= ', 7, lfound, lch) ;
        IF lfound THEN
	BEGIN
	  tracelevel (genetrace, lch) ;
	  wgsetinargs := true ;
	END ;
        scanparm ('FRENC', 5, lfound, lch) ;
        IF lfound THEN
	BEGIN
	  pascalfrench := true ;
	  wd := wdf ; wno := wnof ; wcl := wclf ; wl1 := wl1f ; wl2 := wl2f ;
	  usednames := usednamesf ;
	END ELSE
	BEGIN
	  pascalfrench := false ;
	  wd := wda ; wno := wnoa ; wcl := wcla ; wl1 := wl1a ; wl2 := wl2a ;
	  usednames := usednamesa ;
	END ;
        scanparm ('TABLE   ', 5, lfound, lch) ;
        IF lfound THEN
	BEGIN
	  symbtabl := true ;
	  mapswitch := true
	END ;
        scanparm ('BRIEFTB ', 7, lfound, ch) ;
        IF lfound THEN
	BEGIN
	  brieftable := true ;
	  mapswitch := true
	END ;
        scanparm ('LONGPROF', 8, lfound, ch) ;
        IF lfound THEN
	BEGIN
	  longprofile := true ;
	  mapswitch := true
	END ;
        scanparm ('PROFILE ', 7, lfound, ch) ;
        IF lfound THEN
	BEGIN
	  profile := true ;
	  mapswitch := true
	END ;
        scanparm ('BRIEFMAP', 8, lfound, ch) ;
        IF lfound THEN
	BEGIN
	  listyes := true ;
	  mapswitch := true
	END ;
        scanparm ('LP      ', 2, lfound, lch) ;
        IF lfound THEN
	BEGIN                                   (* 2 DIGITS COMING NOW *)
	  pagelength := (ord (lch) - ord ('0')) * 10 ;
	  pagelength := pagelength + ord (parmlist [lastread + 1]) - ord ('0') ;
	END ;
        scanparm ('XREF    ', 4, lfound, lch) ;
        IF lfound THEN
	xrefneed := true ;
        outcode := writecode OR (genetrace > none) ;
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, 'SKIPCODE,STATTRACE,DECLTRACE,GENETRACE,SYMBTABL,XREF',
	    skipcode, ord (stattrace), ord (decltrace), ord (genetrace),
	    symbtabl, xrefneed) ;
	  nextline
	END ;
$OPTIONS compile = true $
      END (* CARTEEXEC *) ;

$OPTIONS page $

(* ********************************************************** VERIFCOHERENCE    ** *)

$OPTIONS compile = security $
    PROCEDURE verifcoherence ;

(* C
   On verifie que les relations qui doivent exister entre les constantes
   se maintiennent de VERSION en VERSION.
   C *)

(* E ERRORS DETECTED
   439 Premier groupe
   440 Second   "
   441 Troisieme "
   E *)
      BEGIN
        IF (confdimsize <> confdimw * bytesinword) OR
	(eofb <> eofw * bytesinword) OR
	(eolnb <> eolnw * bytesinword) OR
	(lgparm <> lgparm1 + 1) THEN error (439) ;
        IF (maxset + 1 <> setrange) OR
	(maxchar > maxset) OR
	(maxerrnum <> 3 * setrange - 1) OR
	(maxpage <> maxerrnum) OR
	(maxerpg + 1 < (maxpage DIV setrange)) THEN error (440) ;
        IF (bitsforset <> bytesforset * bitsinbyte) OR
	(bytesforset <> wordsforset * bytesinword) OR
	(wordsforset <> bornesupset + 1) THEN error (441) ;

      END ;

$OPTIONS compile = true $

$OPTIONS page $

(* *************************************COMPILER'S  MAIN*********************** *)

    BEGIN                                         (* MAIN *)
      listyes := false ;
      new (fichinter) ; IF fichinter = NIL THEN heaperror ;
      rewrite (mpcogout) ;
      initialise ;
      carteexec ;
      IF mapswitch THEN BEGIN
	getmapptr (mapptr) ;
	getprofptr (profptr) ;
	IF profile THEN profilewordcount := phl ;
	IF longprofile THEN profilewordcount := lphl ;
        END ;
                                                  (* IF symbtabl THEN *) lkc := lkc + (2 * bytesinword) ;
      IF listyes THEN
        BEGIN
	listhead ;
	pageno := pageno + 1 ;
	iligne := 9 ;
	nextline ;
        END ;
      asscheck := checks ; divcheck := checks ; inxcheck := checks ;
      IF eof (mpcogin) THEN
        BEGIN
	error (22) ; GOTO 100 ;
        END ELSE nextch ;
$OPTIONS compile = security $
      verifcoherence ;
$OPTIONS compile = true $
      progdecl ;                                  (* BEFORE CALL OF INITCLASS *)
      initclasse ;
                                                  (* display[0].fname := next; In INITCLASSE *)

      lc := xc ;
      WITH display [1] DO
        BEGIN
	fname := NIL ; occur := block ;
        END ;
      top := 1 ;
      level := 0 ;
      create_dummyclass_box (pt, blank) ;
      next := NIL ;
      IF inputflag # NIL THEN
        BEGIN
	create_vars_box (inputctp, usednames [1]) ;
	WITH inputctp^ DO
	  BEGIN
	    vtype := textfilectp ; vkind := imported ; vlevel := 0 ;
	    vaddr := -1 ; vptextitem := inputflag ;
	    visused := true ; vfilelocation := standardfile ; visset := true ;
	    deffile := inputflag^.extrfile1 ; defline := inputflag^.extrline1 ;
	    IF symbolmap THEN
	      IF inputflag^.extrline2 <> 0 THEN
	        nameisref (inputctp, inputflag^.extrfile2, inputflag^.extrline2) ;
	  END ;
	next := inputctp ; filtop := filtop + 1 ;
	inputflag^.extdecl := inputctp ;
	filpts [filtop] := inputctp ;
        END (* INPUTFLAG *) ;
      IF outputflag # NIL THEN
        BEGIN
	create_vars_box (outputctp, usednames [2]) ;
	WITH outputctp^ DO
	  BEGIN
	    vtype := textfilectp ; vkind := imported ; vlevel := 0 ;
	    vaddr := -1 ; vptextitem := outputflag ;
	    visused := true ; vfilelocation := standardfile ;
	    deffile := outputflag^.extrfile1 ; defline := outputflag^.extrline1 ;
	    IF symbolmap THEN
	      IF outputflag^.extrline2 <> 0 THEN
	        nameisref (outputctp, outputflag^.extrfile2, outputflag^.extrline2) ;
	  END ;
	next := outputctp ; filtop := filtop + 1 ;
	outputflag^.extdecl := outputctp ;
	filpts [filtop] := outputctp ;
        END (* OUTPUTFLAG *) ;
      IF errorflag # NIL THEN
        BEGIN
	create_vars_box (errorctp, usednames [3]) ;
	WITH errorctp^ DO
	  BEGIN
	    vtype := textfilectp ; vkind := imported ; vlevel := 0 ;
	    vaddr := -1 ; vptextitem := errorflag ;
	    visused := true ; vfilelocation := standardfile ;
	    deffile := errorflag^.extrfile1 ; defline := errorflag^.extrline1 ;
	    IF symbolmap THEN
	      IF errorflag^.extrline2 <> 0 THEN
	        nameisref (errorctp, errorflag^.extrfile2, errorflag^.extrline2) ;
	  END ;
	next := errorctp ; filtop := filtop + 1 ;
	errorflag^.extdecl := errorctp ;
	filpts [filtop] := errorctp ;
        END (* ERRORFLAG *) ;



      new (programnode, procblock) ;              (* ROOTNODE *)
      currentnode := programnode ;
      WITH programnode^ DO BEGIN
	father := NIL ;
	brother := NIL ;
	son := NIL ;
	nextproc := NIL ;
	blockbox := NIL ;
	codebegin := 0 ;
	codeend := 0 ;
	structureplace := 0 ;
	first := NIL ;
	firstlabel := NIL ;
	blocktp := procblock ;
	hdrlin := hdrline ;
	hdrfil := hdrfile ;
	hdrlen := hdrlength ;
	hdrind := hdrindex ;
        END ;

(* *********************************
   *                                *)

      body (NIL, pt) ;
      IF no # 17 THEN error (24) ;

(*                               *
   ********************************* *)

      compencours := false ;
      REPEAT
        nextch ;
      UNTIL compencours ;                         (* ARTIFICIAL EXIT VIA *)
                                                  (* GOTO 100 IN NEXTCH *)
100 :                                             (* END OF COMPILATION *)
      IF mapswitch THEN
        statement_ends (0) ;
      wkextpt := externallistheader ;
      err149 := false ;
      WHILE wkextpt <> NIL DO
        BEGIN

	IF wkextpt^.extdecl = NIL THEN
	  IF wkextpt^.extitemtype IN [extnotresolved, remanentfile] THEN
	    BEGIN
	      IF NOT err149 THEN
	        BEGIN
		err149 := true ;
		error (149) ; printerr ;
	        END ;
	      IF listyes THEN write (mpcogout, '  NOT REDEFINED EXTERNAL NAME(S) :',
		wkextpt^.extname : maxident + 1) ; nextline ;
	      writeln (mpcogerr, '  NOT REDEFINED EXTERNAL NAME(S) :',
	        wkextpt^.extname : maxident + 1) ;
	    END ;
	wkextpt := wkextpt^.extnext ;
        END ;
      IF programnode <> NIL THEN
        programnode^.codeend := statnbr * 2 ;
      IF nbccond # 0 THEN error (208) ;
      IF errinx > 0 THEN
        printerr ;
      statistiques ;
      linkswordcount := lkc DIV bytesinword ;
      IF mapswitch THEN BEGIN                     (* END STATEMENT MAP *)
	statnbr := statnbr + 1 ;
	WITH mapptr^[statnbr] DO
	  BEGIN
	    word1 := twoto18 - 1 ;
	    insert_ (ic DIV bytesinword, 18, word1) ;
	    word2 := 0 ;
	    insert_ (-1, 27, word2) ;
	  END ;
	IF profile THEN BEGIN
	    insert_ ((statnbr - 1) * 2, 18, profptr^[profilewordcount]) ;
	    profilewordcount := profilewordcount + pclength
	  END ;
        END ;
      IF errtotal = 0 THEN buildobject ;
      IF errtotal <> 0 THEN
        BEGIN
	IF mapswitch THEN
	  displaysources ;
	IF symbol_listing THEN
	  IF programnode <> NIL THEN displaysymbols ;
        END ;
      reset (fichinter) ;
      stop (errtotal) ;                           (* RETURN CODE  #0 IF COMP ERRORS *)
    END (* END OF MAIN PROGRAM FOR PASCAL COMPILER ********************* *).
