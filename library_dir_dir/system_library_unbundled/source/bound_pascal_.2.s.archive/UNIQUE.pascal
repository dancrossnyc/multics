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
  PROGRAM unique ;

    $IMPORT
                                                  (* IMPORTED CONSTANTS *)
      'pascal_constants_$max_real (alm)' : maxreal ;
      'pascal_constants_$min_real_pos (alm)' : minreal ;
                                                  (* IMPORTED PROCEDURES *)
      'RACINE (pascal)' :
        error,
        initracine,
        insymbol,
        nextline,
        nextpage,
        recadre,
        returnstop,
        skip,
        statement_begins ;
      'DECLARE (pascal)' :
        checkexternalitem,
        createexternalbox,
        initdeclare ;
      'GENERE (pascal)' :
        initgen,
        longint ;
      'STATE (pascal)' :
        initstate ;
      'CONTEXTTABLE (pascal)' :
        add_schema_token,
        boundary,
        bytesneeded,
        create_konst_box,
        create_proc_box,
        create_vars_box,
        create_schema_box,
        create_types_box ;
                                                  (* IMPORTED VARIABLES *)
      'RACINE (pascal)' :
        alfaptr,
        aval,
        boolptr,
        ch8flag,
        charptr,
        display,
        errorflag,
        errorsfound,
        errtotal,
        inputflag,
        intptr,
        lamptr,
        listyes,
        mapswitch,
        maxstring_ptr,
        mpcogout,
        next,
        nilptr,
        no,
        outputflag,
        pageserrors,
        pascalfrench,
        pnumptr,
        progname,
        programnode,
        realptr,
        string_ptr,
        symbolfile,
        symbolindex,
        symbolline,
        textfilectp,
        top,
        undecptr,
        usednames,
        version ;
      'DECLARE (pascal)' :
        analyzing_schema,
        decltrace,
        hdrfile,
        hdrindex,
        hdrlength,
        hdrline,
        firstlabbox $

    $EXPORT
      displaysymbols,
      heaperror,
      initclasse,
      initialise,
      progdecl,
      prterrmeans,
      statistiques $





$OPTIONS page $

$INCLUDE 'CONSTTYPE' $



$OPTIONS page $

    VAR
                                                  (* IMPORTED CONSTANTS *)
      minreal, maxreal : real ;
                                                  (* IMPORTED FROM RACINE *)
      alfaptr : ctp ;
      aval : alfaid ;
      boolptr : ctp ;
      ch8flag : boolean ;
      charptr : ctp ;
      display : ARRAY [0..displimit] OF recidscope ;
      errorflag : ptexternalitem ;
      errorsfound : ARRAY [0..maxerpg] OF SET OF 0..maxset ;
      errtotal : integer ;
      inputflag : ptexternalitem ;
      intptr : ctp ;
      lamptr : ctp ;
      listyes : boolean ;
      mapswitch : boolean ;
      maxstring_ptr : ctp ;
      mpcogout : text ;
      next : ctp ;
      nilptr : ctp ;
      no : integer ;
      outputflag : ptexternalitem ;
      pageserrors : ARRAY [0..maxerpg] OF SET OF 0..maxset ;
      pascalfrench : boolean ;
      pnumptr : ctp ;
      progname : alfaid ;
      programnode : blocknodeptr ;
      realptr : ctp ;
      string_ptr : ctp ;
      symbolfile : integer ;
      symbolindex : integer ;
      symbolline : integer ;
      textfilectp : ctp ;
      top : integer ;
      undecptr : ctp ;
      usednames : typusednames ;
      version : integer ;
                                                  (* IMPORTED FROM DECLARE *)
      analyzing_schema : schema_status ;
      decltrace : levtrace ;
      firstlabbox : labelblockptr ;
      hdrfile : integer ;
      hdrindex : integer ;
      hdrlength : integer ;
      hdrline : integer ;


(* EXPORTABLE VARIABLES *)
(* NONE *)


(* LOCAL VARIABLES *)
      currentnode : blocknodeptr ;
      firstalfa : ctp ;
      stdcompilernames : ARRAY [1..2] OF alfaid ;
      stdextendnames : ARRAY [1..23] OF alfaid ;
      stdnames,
      stdnamesa,
      stdnamesf : ARRAY [1..38] OF alfaid ;
      stdsolnames,
      stdsolnamesa,
      stdsolnamesf : ARRAY [1..30] OF alfaid ;
      uversion : integer ;                        (* VERSION OF UNIQUE *)


$OPTIONS page $

    $VALUE
      stdcompilernames = ('insert_', 'append_') ;
      stdextendnames = (
        'maxchar', 3 * '          ',
        'date', 'time', 'mvc', 'alloc', '        ',
        'clock', 'cvptrint', 'ccsubarr', 2 * '        ',
        'log10', 'string', 'maxstring', 'length', 'maxlength', 'position', 'substr', 'insert', 'delete'
        ) ;
      stdnamesa = (
        'real', 'integer', 'maxint', 'boolean', 'false', 'true', 'char', 'text',
        'get', 'put', 'reset', 'rewrite', 'new', 'dispose', 'read', 'readln', 'write',
        'writeln', 'page', 'pack', 'unpack',
        'odd', 'ord', 'chr', 'eof', 'eoln', 'abs', 'trunc', 'round', 'pred', 'succ',
        'sqr',
        'sin', 'cos', 'ln', 'exp', 'sqrt', 'arctan'
        ) ;
      stdnamesf = (
        'reel', 'entier', 'entmax', 'booleen', 'faux', 'vrai', 'car', 'texte',
        'prendre', 'mettre', 'relire', 'recrire', 'creer', 'liberer', 'lire',
        'lireln', 'ecrire', 'ecrireln', 'page', 'tasser', 'detasser',
        'impair', 'ord', 'carac', 'fdf', 'fdln', 'abs', 'tronc', 'arrondi', 'pred',
        'succ', 'carre', 'sin', 'cos', 'ln', 'exp', 'rac2', 'arctan'
        ) ;
      stdsolnamesa = (
        'maxreal', 'minreal', 'setmax', 2 * '    ',
        'fconnect', 'fupdate', 'fget', 'fput', 'fclose', 'fappend', 'freopen',
        'flush', 'argv', 'stop',
        4 * '     ',
        'fsize', 'fpos', 'fllength', 'fstatus', 'sread', 'swrite', 'argc', 4 * '  '
        ) ;
      stdsolnamesf = (
        'reelmax', 'precision', 'ensmax', '      ', '      ',
        'connecter', 'fupdate', 'fprendre', 'fmettre', 'fermer', 'allonger',
        'reouvrir', 'vider', 'arg', 'stop',
        '     ', '     ', '     ', '        ',
        'taille', 'poscour', 'maxligne', 'etat', 'lirech', 'ecrirech', 'nbarg',
        4 * '     '
        ) $


$OPTIONS page $

(* HEADERS OF THE IMPORTED PROCEDURES *)
(* FROM RACINE *)
    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    FUNCTION recadre (fnumber, fmod : integer) : integer ; EXTERNAL ;
    PROCEDURE nextpage ; EXTERNAL ;
    PROCEDURE skip (nosymb : integer) ; EXTERNAL ;
    PROCEDURE insymbol ; EXTERNAL ;
    PROCEDURE returnstop ; EXTERNAL ;
    PROCEDURE initracine ; EXTERNAL ;
    PROCEDURE statement_begins (genp : boolean) ; EXTERNAL ;
                                                  (* FROM DECLARE *)
    PROCEDURE initdeclare ; EXTERNAL ;
    PROCEDURE checkexternalitem (ai : alfaid ; VAR fpt : ptexternalitem) ; EXTERNAL ;
    PROCEDURE createexternalbox (ai : alfaid ; ei : externalitemtype ; id : idkinds ;
      VAR fpt : ptexternalitem) ; EXTERNAL ;
                                                  (* FROM GENERE *)
    PROCEDURE initgen ; EXTERNAL ;
    FUNCTION longint (i : integer) : integer ; EXTERNAL ;
                                                  (* FROM STATE *)
    PROCEDURE initstate ; EXTERNAL ;

(* FROM CONTEXTTABLE *)
    FUNCTION bytesneeded (objform : typform ; highest : integer ; ispack : boolean) : integer ; EXTERNAL ;
    PROCEDURE add_schema_token (kind : schema_token_kind) ; EXTERNAL ;
    FUNCTION boundary (objform : typform ; ispack : boolean ; pcksize : integer) : integer ; EXTERNAL ;
    PROCEDURE create_vars_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;

    PROCEDURE create_proc_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;
    PROCEDURE create_types_box (VAR fvbox : ctp ; fname : alfaid ; fform : typform ; fbool : boolean) ; EXTERNAL ;
    PROCEDURE create_schema_box (VAR fvbox : ctp ; fname : alfaid) ; EXTERNAL ;
    PROCEDURE create_konst_box (VAR fvbox : ctp ; fname : alfaid ; ftypofconst : consttype) ; EXTERNAL ;

$OPTIONS page $

(* *********************************************************HEAPERROR********** *)

    PROCEDURE heaperror ;

(* C  FUNCTIONS OF THIS  PROCEDURE
   . EMITS AN ERROR   HEAP IS FULL
   . EMITS EXPLICIT MSG  ON  LISTING
   . STOPS  COMPILATION
   C *)
(* E  ERRORS DETECTED
   252 :  COMPILER'S HEAP FULL.COMPILATION STOPS
   E *)
      BEGIN
        error (252) ;
        nextline ;
        write (mpcogout, ' ********  COMPILER''S HEAP IS FULL. COMPILATION STOPS') ;
        nextline ;
        returnstop ;                              (* GOTO 100 IN MODULE RACINE *)
                                                  (* TO STOP COMPILATION *)
      END (* HEAPERROR *) ;


$OPTIONS page $

(* ***********************************************INITIALISE******************* *)

    PROCEDURE initialise ;

(* C INITIALIZES ALL GLOBALS USED IN COMPILER WHICH MUST BE INITIALIZED       C *)
      BEGIN
        uversion := 3 ;
        initracine ;
        initdeclare ;
        initgen ;
        initstate ;
        IF uversion > version THEN version := uversion ;
      END (* INITIALISE *) ;


$OPTIONS page $

(* *************************************************  INITSTDPURE  ****** *)

    PROCEDURE initstdpure ;
      VAR
        locpt, lp : ctp ;
        it : integer ;
      BEGIN                                       (* INITSTDPURE *)

(* TYPE OF NIL *)
        create_types_box (nilptr, blank, pointer, false) ;
        WITH nilptr ^ DO
	BEGIN
	  size := bytesneeded (pointer, 0, false) ;
	  cadrage := boundary (pointer, false, 0) ;
	  pack := false ;
	END ;

(* TYPE REAL *)
        create_types_box (realptr, stdnames [1], reel, false) ;
        WITH realptr ^ DO
	BEGIN
	  deffile := 0 ; defline := 0 ; next := realptr ;
	  size := bytesneeded (reel, 0, false) ;
	  cadrage := boundary (reel, false, 0) ;
	  pack := false ;
	END ;

(* TYPE INTEGER *)
        create_types_box (intptr, stdnames [2], numeric, false) ;
        WITH intptr ^ DO
	BEGIN
	  deffile := 0 ; defline := 0 ; next := intptr ;
	  size := bytesneeded (numeric, maxint, false) ;
	  cadrage := boundary (numeric, false, 0) ;
	  pack := false ;
	  npksize := size ;
	  nmax := maxint ; nmin := -maxint ;
	END ;

(* NUMERIC SUBRANGE SIMULATION OF NUMERIC SETS *)
        create_types_box (locpt, blank, numeric, false) ;
        WITH locpt ^ DO
	BEGIN
	  size := bytesneeded (numeric, maxset, false) ;
	  cadrage := boundary (numeric, false, 0) ;
	  pack := false ;
	  npksize := bytesneeded (numeric, maxset, true) ;
	  nmax := maxset ;
	END ;

(* TYPE OF NUMERIC SETS  *)
        create_types_box (pnumptr, blank, power, false) ;
        WITH pnumptr ^ DO
	BEGIN
	  size := bytesneeded (power, maxset, false) ;
	  cadrage := boundary (power, false, 0) ;
	  pack := false ;
	  ppksize := bytesneeded (power, setmax, true) ;
	  setlength := setmax + 1 ;
	  elset := locpt ;
	END ;

(* CONSTANT MAXINT *)
        create_konst_box (locpt, stdnames [3], wordconst) ;
        WITH locpt^ DO
	BEGIN
	  next := locpt ;
	  deffile := 0 ; defline := 0 ;
	  contype := intptr ; values := maxint ;
	END ;

(* TYPE BOOLEAN *)
        create_types_box (boolptr, stdnames [4], scalar, false) ;
        WITH boolptr^ DO
	BEGIN
	  next := boolptr ;
	  deffile := 0 ; defline := 0 ;
	  size := bytesneeded (scalar, 1, false) ;
	  cadrage := boundary (scalar, false, 0) ;
	  pack := false ;
	  spksize := bytesneeded (scalar, 1, true) ;
	END ;

(* CONSTANTS FALSE TRUE *)
        lp := NIL ;
        FOR it := 0 TO 1 DO
	BEGIN
	  create_konst_box (locpt, stdnames [5 + it], wordconst) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      contype := boolptr ; values := it ; succ := lp ;
	    END ;
	  lp := locpt ;
	END ;
        boolptr^.fconst := locpt ;

(* TYPE OF PREDEFINED SET OF BOOLEAN *)
        create_types_box (locpt, blank, power, false) ;
        WITH locpt ^ DO
	BEGIN
	  deffile := 0 ; defline := 0 ;
	  size := bytesneeded (power, 1, false) ;
	  cadrage := boundary (power, false, 0) ;
	  pack := false ;
	  ppksize := bytesneeded (power, 1, true) ;
	  setlength := 2 ;
	  elset := boolptr ;
	END ;
        boolptr^.sptcstepw := locpt ;

(* TYPE CHAR *)
        create_types_box (charptr, stdnames [7], scalar, false) ;
        WITH charptr^ DO
	BEGIN
	  next := charptr ;
	  deffile := 0 ; defline := 0 ;
	  size := bytesneeded (scalar, maxchar, false) ;
	  cadrage := boundary (scalar, false, 0) ;
	  pack := false ;
	  spksize := bytesneeded (scalar, maxchar, true) ;
	END ;

(* LAST CONSTANT OF TYPE CHAR *)
        create_konst_box (locpt, blank, wordconst) ;
        WITH locpt ^ DO
	BEGIN
	  contype := charptr ;
	  IF ch8flag THEN values := maxchar8
	  ELSE values := maxchar ;
	END ;
        charptr ^.fconst := locpt ;

(* TYPE OF PREDEFINED SET OF CHAR *)
        create_types_box (locpt, blank, power, false) ;
        WITH locpt ^ DO
	BEGIN
	  deffile := 0 ; defline := 0 ;
	  size := bytesneeded (power, maxchar, false) ;
	  cadrage := boundary (power, false, 0) ;
	  pack := false ;
	  ppksize := bytesneeded (power, maxchar, true) ;
	  setlength := maxchar + 1 ;
	  elset := charptr ;
	END ;
        charptr^.sptcstepw := locpt ;

(* TYPE OF EMPTY SET *)
        create_types_box (lamptr, blank, power, false) ;
        WITH lamptr ^ DO
	BEGIN
	  deffile := 0 ; defline := 0 ;
	  size := bytesneeded (power, maxset, false) ;
	  cadrage := boundary (power, false, 0) ;
	  pack := false ;
	  ppksize := bytesneeded (power, maxset, true) ;
	  setlength := maxset + 1 ;
	END ;

(* TYPE OF ALFA CONSTANTS   *)
        create_types_box (alfaptr, blank, arrays, false) ;
        WITH alfaptr ^ DO
	BEGIN
	  deffile := 0 ; defline := 0 ;
	  size := 0 ;
	  cadrage := 0 ;
	  pack := true ;
	  aeltype := charptr ; inxtype := intptr ;
	  subsize := bytesneeded (scalar, maxchar, true) ;
	END ;

(* TYPE TEXT *)
        create_types_box (textfilectp, stdnames [8], files, false) ;
        WITH textfilectp^ DO
	BEGIN
	  next := textfilectp ;
	  deffile := 0 ; defline := 0 ;
	  size := fsbpointersize ;
	  cadrage := bytesindword ;
	  pack := false ;
	  feltype := charptr ;
	END ;

(* PREDEFINED PROCEDURES:
   get put reset rewrite new dispose read readln write writeln
   page pack unpack       *)
        FOR it := 0 TO 12 DO
	BEGIN
	  create_proc_box (locpt, stdnames [9 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := locpt ; proclevel := 0 ; formals := NIL ;
	      segsize := it ;
	      procinscope := false ;
	      predefproc := true ; ploc := instdpure ;
	    END ;
	END ;

(* PREDEFINED FUNCTIONS *)
(* odd ord chr eof eoln abs trunc round pred succ sqr   *)
        FOR it := 0 TO 10 DO
	BEGIN
	  create_proc_box (locpt, stdnames [22 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := nilptr ; proclevel := 0 ; formals := NIL ;
	      segsize := it ;
	      procinscope := false ;
	      predefproc := true ; ploc := instdpure ;
	    END ;
	END ;

(* PREDEFINED SCIENTIFIC FUNCTIONS:
   sin cos ln exp sqrt arctan    *)
        FOR it := 0 TO 5 DO
	BEGIN
	  create_proc_box (locpt, stdnames [33 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := realptr ; proclevel := 0 ; formals := NIL ;
	      segsize := it ;
	      procinscope := false ;
	      predefproc := true ; ploc := instdpure ;
	    END ;
	END ;

(* UNDECLARED VARIABLE associated to  undeclared identifiers *)
        create_vars_box (undecptr, blank) ;
        WITH undecptr^ DO
	BEGIN
	  visused := true ; visset := true ;
	END ;

$OPTIONS compile = trace $
        IF decltrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ Fin de INITSTDPURE @@@ with NEXT, UNDECPTR at^',
	    ord (next), ord (undecptr)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* INITSTDPURE *) ;

$OPTIONS page $

(* ***********************************************  INITSTDSOL  ************** *)

    PROCEDURE initstdsol ;

      VAR
        it : integer ;
        locpt : ctp ;

      BEGIN                                       (* INITSTDSOL *)

(* Constantes MAXREAL and MINREAL     *)
        FOR it := 1 TO 2 DO
	BEGIN
	  create_konst_box (locpt, stdsolnames [it], dwordconst) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      contype := realptr ;
	      IF it = 1 THEN valreel := maxreal ELSE valreel := minreal ;
	    END ;
	END ;

(* Constante SOL SETMAX    *)
        create_konst_box (locpt, stdsolnames [3], wordconst) ;
        WITH locpt^ DO
	BEGIN
	  next := locpt ;
	  deffile := 0 ; defline := 0 ;
	  contype := intptr ; values := setmax ;
	END ;

(* SOL procedures
   fconnect,fupdate,fget,fput,fclose,fappend,freopen,flush,argv,
   stop     *)
        FOR it := 0 TO 9 DO
	BEGIN
	  create_proc_box (locpt, stdsolnames [6 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := locpt ; proclevel := 0 ; formals := NIL ;
	      segsize := it ;
	      procinscope := false ;
	      predefproc := true ; ploc := instdsol ;
	    END ;
	END ;

(* SOL functions
   fsize, fpos, fllength, fstatus, sread, swrite, argc   *)
        FOR it := 0 TO 6 DO
	BEGIN
	  create_proc_box (locpt, stdsolnames [20 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := nilptr ; proclevel := 0 ; formals := NIL ;
	      segsize := it ;
	      procinscope := false ;
	      predefproc := true ; ploc := instdsol ;
	    END ;
	END ;
      END (* INITSTDSOL *) ;


$OPTIONS page $

(* *****************************************  INITSTDCOMPILER   ************** *)

    PROCEDURE initstdcompiler ;

      VAR
        it : integer ;
        locpt : ctp ;

      BEGIN                                       (* INITSTDCOMPILER *)
        FOR it := 0 TO 1 DO
	BEGIN
	  create_proc_box (locpt, stdcompilernames [1 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := locpt ; proclevel := 0 ; formals := NIL ;
	      segsize := it ;
	      procinscope := false ;
	      predefproc := true ; ploc := instdcompiler ;
	    END ;
	END ;
      END (* INITSTDCOMPILER *) ;

$OPTIONS page $

(* *****************************************  INITSTDEXTEND    *************** *)

    PROCEDURE initstdextend ;
      VAR
        it : integer ;
        locpt : ctp ;
      BEGIN                                       (* INITSTDEXTEND *)


(* CONSTANT MAXCHAR *)
        new (locpt, konst, wordconst) ; IF locpt = NIL THEN heaperror ;
        WITH locpt^ DO
	BEGIN
	  klass := konst ; typofconst := wordconst ;
	  name := stdextendnames [1] ; nxtel := next ; next := locpt ;
	  alfathread := NIL ; deffile := 0 ; defline := 0 ;
	  new (references) ; IF references = NIL THEN heaperror ; (* Exit compil *)
	  WITH references^ DO
	    BEGIN
	      refnbr := 0 ; nextref := NIL ;
	    END ;
	  contype := intptr ; values := maxchar ; succ := NIL ;
	END ;


(* EXTEND PROCEDURES
   date time    mvc alloc
   *)
        FOR it := 0 TO 3 DO
	BEGIN
	  create_proc_box (locpt, stdextendnames [5 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := locpt ; proclevel := 0 ; formals := NIL ;
	      segsize := it ;
	      procinscope := false ;
	      predefproc := true ; ploc := instdextend ;
	    END ;
	END ;

(* EXTEND FUNCTIONS
   clock
   cvptrint  ccsubarr     *)
        FOR it := 0 TO 2 DO
	BEGIN
	  create_proc_box (locpt, stdextendnames [10 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := nilptr ; proclevel := 0 ; formals := nilptr ;
	      segsize := it ;
	      procinscope := false ;
	      predefproc := true ; ploc := instdextend ;
	    END ;
	END ;

(* Predefined scientific function LOG10         *)

        create_proc_box (locpt, stdextendnames [15]) ;
        WITH locpt^ DO
	BEGIN
	  next := locpt ;
	  deffile := 0 ; defline := 0 ;
	  proctype := realptr ; proclevel := 0 ;
	  segsize := log10switch ;
	  procinscope := false ;
	  predefproc := true ; ploc := instdextend ;
	END ;


(* PREDEFINED MAXSTRING CONSTANT *)

        create_konst_box (maxstring_ptr, stdextendnames [17], wordconst) ;
        WITH maxstring_ptr^ DO
	BEGIN
	  deffile := 0 ; defline := 0 ;
	  contype := intptr ; values := (wordsinsegment - 1) * bytesinword ;
	END ;
        next := maxstring_ptr ;

(* BOX FOR STRING LENGTH RANGE *)

        create_types_box (locpt, blank, numeric, false) ;
        WITH locpt^ DO
	BEGIN
	  size := bytesneeded (numeric, maxstring_ptr^.values, false) ;
	  cadrage := boundary (numeric, false, 0) ;
	  npksize := size ;
	  nmin := 0 ; nmax := maxstring_ptr^.values ;
	END ;

(* BOX FOR STRING FORMAT *)

        create_schema_box (string_ptr, stdextendnames [16]) ;
        next := string_ptr ;
        WITH string_ptr^ DO
	BEGIN
	  deffile := 0 ; defline := 0 ;
	  parameter_count := 1 ;
	  create_vars_box (formal_parameter_list, 'maxlength') ;
	  WITH formal_parameter_list^ DO
	    BEGIN
	      vtype := locpt ;
	      vkind := formal ;
	      nxtel := NIL ;
	    END ;
	END ;
        WITH analyzing_schema DO
	BEGIN
	  on := true ;
	  schema_ptr := string_ptr ;
	  current_token := NIL ;
	  add_schema_token (symbol_token) ;
	  WITH current_token^ DO
	    BEGIN
	      tno := 38 ; tcl := 2              (* "RECORD" *)
	    END ;
	  add_schema_token (name_token) ;
	  current_token^.taval := '(length)' ;
	  add_schema_token (symbol_token) ;
	  WITH current_token^ DO
	    current_token^.tno := 19 ;          (* ":" *)
	  add_schema_token (int_const_token) ;  (* "0" *)
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 39 ;            (* ".." *)
	  add_schema_token (name_token) ;
	  current_token^.taval := 'maxlength' ;
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 16 ;            (* ";" *)
	  add_schema_token (name_token) ;
	  current_token^.taval := '(character string)' ;
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 19 ;            (* : *)
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 42 ;            (* "PACKED" *)
	  add_schema_token (symbol_token) ;
	  WITH current_token^ DO
	    BEGIN
	      tno := 38 ; tcl := 1              (* "ARRAY" *)
	    END ;
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 11 ;            (* "[" *)
	  add_schema_token (int_const_token) ;  (* "1" *)
	  current_token^.t_int_value := 1 ;
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 39 ;            (* ".." *)
	  add_schema_token (name_token) ;
	  current_token^.taval := 'maxlength' ;
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 12 ;            (* "]" *)
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 27 ;            (* "OF" *)
	  add_schema_token (name_token) ;
	  current_token^.taval := charptr^.name ;
	  add_schema_token (symbol_token) ;
	  current_token^.tno := 22 ;            (* " END" *)
	END ;

(* PREDEFINED STRING FUNCTIONS *)

        FOR it := 0 TO 3 DO
	BEGIN
	  create_proc_box (locpt, stdextendnames [18 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := nilptr ; proclevel := 0 ; formals := nilptr ;
	      segsize := it + 3 ;               (* FROM 3 TO 6 *)
	      procinscope := false ;
	      predefproc := true ; ploc := instdextend ;
	    END ;
	END ;

(* PREDEFINED STRING PROCEDURES *)

        FOR it := 0 TO 1 DO
	BEGIN
	  create_proc_box (locpt, stdextendnames [22 + it]) ;
	  WITH locpt^ DO
	    BEGIN
	      next := locpt ;
	      deffile := 0 ; defline := 0 ;
	      proctype := locpt ; proclevel := 0 ; formals := NIL ;
	      segsize := it + 3 ;               (* FROM 3 TO 4 *)
	      procinscope := false ;
	      predefproc := true ; ploc := instdextend ;
	    END ;
	END ;

      END (* INITSTDEXTEND *) ;




$OPTIONS page $

(* *************************************** INITCLASSE ************************* *)

    PROCEDURE initclasse ;

(* C  By successive calls of
   INITSTDPURE, INITSTDCOMPILER
   INITSTDSOL
   INITSTDEXTEND
   all suitable predefined items are created
   As output of this procedure, we have
   INTPTR, REALPTR, and so on .......
   NEXT     last created name
   DISPLAY [ 0 ]  is initialized
   C *)

      BEGIN                                       (* Initclasse *)
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ Debut de INITCLASSE @@@ ') ;
	  nextline ;
	END ;
$OPTIONS compile = true $

        next := NIL ;
        IF pascalfrench THEN
	BEGIN
	  stdnames := stdnamesf ; stdsolnames := stdsolnamesf ;
	END ELSE
	BEGIN
	  stdnames := stdnamesa ; stdsolnames := stdsolnamesa ;
	END ;

        initstdpure ;
        initstdcompiler ;
        initstdsol ;
        initstdextend ;

        WITH display [0] DO
	BEGIN
	  fname := next ; occur := block ;
	END ;

$OPTIONS compile = trace $
        IF decltrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ Fin de INITCLASSE @@@ with NEXT at^',
	    ord (next)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* Initclasse *) ;

$OPTIONS page $

(* *************************************PROGDECL******************************* *)

    PROCEDURE progdecl ;

(* C CALLED IN ORDER  TO ANALYZE   PROGRAM  HEADER.
   * MAIN PROGRAM
   * PROGRAM   NAME
   * EXTERNAL  LIST
   C *)
(* E ERRORS DETECTED
   2: ID. EXPECTED
   3: 'PROGRAM' EXPECTED
   4: ')' EXPECTED
   14: ';' EXPECTED
   20: ',' EXPECTED
   100 : External id declared twice
   E *)
      VAR
        wkextpt : ptexternalitem ;
      BEGIN                                       (* PROGDECL *)
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT PROGDECL @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
        insymbol ;
        IF mapswitch THEN
	BEGIN
	  hdrline := symbolline ;
	  hdrfile := symbolfile ;
	  hdrindex := symbolindex ;
	END ;
        IF no # 50 (* PROGRAM *) THEN
	BEGIN
	  error (3) ; skip (46) ;
	END ELSE
	BEGIN
	  insymbol ;
	  IF (no # 1) (* ID *) THEN
	    BEGIN
	      error (2) ; skip (9) ;            (* SEARCH ( *)
	    END ELSE
	    BEGIN
	      progname := aval ;
	      insymbol ;
	    END ;
	  IF no = 9 (* ( *) THEN
	    BEGIN                               (* EXTERNAL LIST *)
	      insymbol ;
	      IF no # 1 THEN
	        BEGIN
		error (2) ; skip (1) ;        (* SEARCHS NEXT ID *)
	        END ;
	      WHILE no = 1 (* ID *) DO
	        BEGIN
		checkexternalitem (aval, wkextpt) ;
		IF wkextpt <> NIL THEN error (100) ELSE
		  BEGIN
		    IF aval = usednames [1] THEN
		      createexternalbox (aval, requiredfile, imported, inputflag) ELSE
		      IF aval = usednames [3] THEN
		        createexternalbox (aval, requiredfile, imported, errorflag) ELSE
		        IF aval = usednames [2] THEN
			createexternalbox (aval, requiredfile, imported, outputflag) ELSE
			createexternalbox (aval, remanentfile, actual, wkextpt) ;
		  END ;
		insymbol ;
		IF no = 15 (* , *) THEN
		  BEGIN
		    insymbol ;
		    IF no <> 1 THEN
		      BEGIN
		        error (2) ; skip (1)
		      END
		  END ELSE
		  IF no # 10 (* ) *) THEN
		    error (20) ;
	        END (* WHILE NO=1 *) ;
	      IF no = 10 THEN                   (* ) *)
	        insymbol ELSE
	        BEGIN
		error (4) ; skip (46) ;       (* SEARCHS ; *)
	        END ;
	    END (* NO=9 *) ;
	  hdrlength := symbolindex - hdrindex ;
	  IF no # 16 (* ; *) THEN
	    BEGIN
	      error (14) ; skip (16) ;
	    END ;
	END (* NO=50 PROGRAM *) ;
        IF no = 16 THEN insymbol ;
$OPTIONS compile = trace $
        IF decltrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN PROGDECL @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* PROGDECL *) ;


$OPTIONS page $

(* *********************************************************PRTERRMEANS******** *)


    PROCEDURE prterrmeans (VAR filetowr : text ; errornum : integer) ;

      VAR j, i : integer ;

(* ***********************************************PR00 < PRTERRMEANS*********** *)

      PROCEDURE pr00 (errnumod : integer) ;

(* C   ERRORS 0 TO 49   NUMBER IS ERRNUMOD                                    C *)
        BEGIN
	CASE errnumod OF
	  1 :                                   (*   1 *)
	    write (filetowr, 'SCALAR OR NUMERIC EXPECTED') ;
	  2 :                                   (*   2 *)
	    write (filetowr, 'IDENTIFIER EXPECTED') ;
	  3 :                                   (*   3 *)
	    IF pascalfrench THEN
	      write (filetowr, '''PROGRAMME'' EXPECTED')
	    ELSE
	      write (filetowr, '''PROGRAM'' EXPECTED') ;
	  4 :                                   (*   4 *)
	    write (filetowr, ''')'' EXPECTED') ;
	  5 :                                   (*   5 *)
	    write (filetowr, '''..'' EXPECTED') ;
	  6 :                                   (*    6 *)
	    write (filetowr, 'BOOLEAN EXPRESSION EXPECTED') ;
	  7 :                                   (*   7 *)
	    write (filetowr, ''':'' EXPECTED') ;
	  8 :                                   (*   8 *)
	    IF pascalfrench THEN
	      write (filetowr, '''DE'' EXPECTED')
	    ELSE
	      write (filetowr, '''OF'' EXPECTED') ;
	  9 :                                   (*   9 *)
	    write (filetowr, '''('' EXPECTED') ;
	  10 :                                  (*  10 *)
	    write (filetowr, 'ERROR IN TYPE DECLARATION') ;
	  11 :                                  (*  11 *)
	    write (filetowr, '''['' EXPECTED') ;
	  12 :                                  (*  12 *)
	    write (filetowr, ''']'' EXPECTED') ;
	  13 :                                  (*  13 *)
	    IF pascalfrench THEN
	      write (filetowr, '''FIN'' EXPECTED')
	    ELSE
	      write (filetowr, '''END'' EXPECTED') ;
	  14 :                                  (*  14 *)
	    write (filetowr, ''';'' EXPECTED') ;
	  15 :                                  (*  15 *)
	    write (filetowr, 'INTEGER EXPECTED') ;
	  16 :                                  (*  16 *)
	    write (filetowr, '''='' EXPECTED') ;
	  17 :                                  (*  17 *)
	    IF pascalfrench THEN
	      write (filetowr, '''DEBUT'' EXPECTED')
	    ELSE
	      write (filetowr, '''BEGIN'' EXPECTED') ;
	  18 :                                  (*  18 *)
	    write (filetowr, ''' EXPECTED') ;
	  19 :                                  (*  19 *)
	    write (filetowr, '"PACKED ARRAY OF CHAR" CHARACTER STRING EXPECTED') ;
	  20 :                                  (*  20 *)
	    write (filetowr, ''','' EXPECTED') ;
	  21 :                                  (*  21 *)
	    write (filetowr, 'ILLEGAL SHIFT COUNT') ;
	  22 :                                  (*  22 *)
	    write (filetowr, 'END_OF_FILE ON INPUT FILE') ;
	  23 :                                  (*  23 *)
	    write (filetowr, '"CASE LABEL" EXPECTED') ;
	  24 :                                  (*  24 *)
	    write (filetowr, '''.'' EXPECTED') ;
	  25 :                                  (*  25 *)
	    write (filetowr, 'INVALID TRACE OPTION IN PARAMETERS'' LIST') ;
	  26 :                                  (*  26 *)
	    write (filetowr, 'PACKED ITEM NOT ALLOWED HERE') ;
	  27 :                                  (* 27 *)
	    write (filetowr, 'TYPE IDENTIFIER ENCOUNTERED IN TYPE DECLARATION') ;
	  28 :                                  (* 28 *)
	    write (filetowr, 'PREDEFINED PROC OR FUNCT NOT ALLOWED HERE ') ;
	  29 :                                  (* 29 *)
	    write (filetowr, 'SAME LENGTH STRINGS EXPECTED HERE') ;
	  30 :                                  (* 30 *)
	    write (filetowr, 'AT LEAST A DUMMY BLOC EXPECTED ') ;
	  31 :                                  (* 31 *)
	    write (filetowr, 'MAIN NOT ALLOWED IN SEPARATE PROGRAM ') ;
	  32 :                                  (*   32 *)
	    write (filetowr, 'OCTAL NUMBER NOT ALLOWED IN STANDARD') ;
	  33 :                                  (*  33 *)
	    write (filetowr, 'HEXADECIMAL,BINARY  NUMBER NOT ALLOWED IN STANDARD ') ;
	  34 :                                  (* 34 *)
	    write (filetowr, 'CONDITION IDENTIFIER EXPECTED') ;
	  35 :                                  (* 35 *)
	    write (filetowr, ''','' OR '';'' OR ''$'' EXPECTED') ;
	  36 :                                  (* 36 *)
	    write (filetowr, ''','', '':='', '';'' OR ''$'' EXPECTED') ;
	  37 :
	    write (filetowr, 'SUPPLIED MULTICS IDENTIFICATION STRING ERRONEOUS') ; (*    37    *)
	  38 :                                  (* 38 *)
	    write (filetowr, ''','' or ''$'' EXPECTED') ;
	  39 :                                  (* 39 *)
	    write (filetowr, 'STRING OR ''*'' EXPECTED') ;
	  40 :                                  (* 40 *)
	    write (filetowr, '''$'' EXPECTED') ;
	  41 :                                  (* 41 *)
	    write (filetowr, 'THIS STRING CANNOT BE > 32 CHARS') ;
	  42 :
	    write (filetowr, 'SOL PROCEDURE IS NOT STANDARD ') ;
	  44 :
	    write (filetowr, 'SOL PROCEDURE IS NOT YET IMPLEMENTED ') ;
	  45 :
	    write (filetowr, 'EXTENDED PASCAL IS NOT STANDARD ') ;
	  46 :
	    write (filetowr, 'ARRAY OF FILE NOT YET READY     ') ;
	  47 :                                  (* 47 *)
	    write (filetowr, 'OPTION IDENTIFIER EXPECTED') ;
	  48 :                                  (* 48 *)
	    write (filetowr, 'UNKNOWN OPTION') ;
	  49 :                                  (* 49 *)
	    write (filetowr, '''+'' OR ''-'' EXTECTED') ;
	END ;                                   (* CASE *)
        END (* PR00 *) ;

(* ***********************************************PR01 < PRTERRMEANS*********** *)

      PROCEDURE pr01 (errnumod : integer) ;

(* C   ERRORS  50 TO 99  NUMBER IS 50+ERRNUMOD                                C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (*  50 *)
	    write (filetowr, 'ERROR IN CONSTANT') ;
	  1 :                                   (*   51 *)
	    write (filetowr, ''':='' EXPECTED') ;
	  2 :                                   (*   52 *)
	    IF pascalfrench THEN
	      write (filetowr, '''ALORS'' EXPECTED')
	    ELSE
	      write (filetowr, '''THEN'' EXPECTED') ;
	  3 :                                   (*   53 *)
	    IF pascalfrench THEN
	      write (filetowr, '''JUSQUE'' EXPECTED')
	    ELSE
	      write (filetowr, '''UNTIL'' EXPECTED') ;
	  4 :                                   (*   54 *)
	    IF pascalfrench THEN
	      write (filetowr, '''FAIRE'' EXPECTED')
	    ELSE
	      write (filetowr, '''DO'' EXPECTED') ;
	  5 :                                   (*   55 *)
	    IF pascalfrench THEN
	      write (filetowr, '''HAUT/BAS'' EXPECTED')
	    ELSE
	      write (filetowr, '''TO/DOWNTO'' EXPECTED') ;
	  6 :                                   (* 56 *)
	    write (filetowr, ' TYPE IDENTIFIER OR CONFORMANT ARRAY SCHEMA EXPECTED') ;
	  7 :                                   (* 57 *)
	    write (filetowr, ' CONFORMANT ARRAY SCHEMA EXPECTED ') ;
	  8 :                                   (*   58 *)
	    write (filetowr, 'ILLEGAL BEGINNING SYMBOL FOR A FACTOR') ;
	  9 :                                   (* 59 *)
	    write (filetowr, 'AN IDENTIFIER CANNOT BE MORE THAN 32 CHARS LONG.') ;
	  10 :                                  (*  60 *)
	    IF pascalfrench THEN
	      write (filetowr, '''OU'' NOT ALLOWED AS MONADIC OPERATOR')
	    ELSE
	      write (filetowr, '''OR'' NOT ALLOWED AS MONADIC OPERATOR') ;
	  11 :                                  (*  61 *)
	    write (filetowr, 'ILLEGAL FIRST SYMBOL IN A STATEMENT') ;
	  12 :                                  (* 62 *)
	    write (filetowr, 'POINTED TYPE NOT DEFINED ') ;
	  14 :                                  (*  64 *)
	    write (filetowr, ''','' OR '')'' EXPECTED IN VALUE LIST') ;
	  15 :                                  (*  65 *)
	    write (filetowr, 'VALUE PART ALLOWED AT GLOBAL LEVEL ONLY') ;
	  16 :                                  (*  66 *)
	    write (filetowr, 'ILLEGAL OPERATION FOR THIS TYPE OF FILE') ;
	  17 :                                  (* 67 *)
	    write (filetowr, '''$'' OR '';'' EXPECTED.') ;
	  18 :                                  (*  68 *)
	    write (filetowr, 'RESET POINTER NOT ALLOWED IN STANDARD MODE') ;
	  19 :                                  (*  69 *)
	    write (filetowr, 'VALUE PART NOT ALLOWED (STANDARD)') ;
	  20 :                                  (*  70 *)
	    write (filetowr, 'THIS CONDITIONAL COMPILATION MECHANISM IS OBSOLETE') ;
	  21 :                                  (* 71 *)
	    write (filetowr, 'PACK ATTRIBUTE ALLOWED ONLY FOR LAST DIMENSION(S)') ;
	  23 :                                  (* 73 *)
	    write (filetowr, 'EXTENSION USED IS NOT SOL AND NOT STANDARD') ;
	  24 :                                  (* 74 *)
	    write (filetowr, 'STRING OR PACKED ARRAY OF 8 CHARS EXPECTED ') ;
	  25 :                                  (* 75 *)
	    write (filetowr, 'EXTENSION USED IS SOL BUT NOT STANDARD') ;
	  26 :                                  (* 76 *)
	    write (filetowr, '$ EXPECTED ') ;
	  27 :                                  (* 77 *)
	    IF pascalfrench THEN
	      write (filetowr, '$IMPORTE MUST BE AT GLOBAL LEVEL AFTER PROGRAM HEADER')
	    ELSE
	      write (filetowr, '$IMPORT MUST BE AT GLOBAL LEVEL AFTER PROGRAM HEADER') ;
	  28 :                                  (* 78 *)
	    IF pascalfrench THEN
	      write (filetowr, '$IMPORTE AND $EXPORTE ONLY SOL FEATURES')
	    ELSE
	      write (filetowr, '$IMPORT AND $EXPORT ONLY SOL FEATURES') ;
	  29 :                                  (* 79 *)
	    IF pascalfrench THEN
	      write (filetowr, '$EXPORTE ONLY ALLOWED AT MAIN LEVEL.') ELSE
	      write (filetowr, '$EXPORT ONLY ALLOWED AT MAIN LEVEL.') ;
	  30 :                                  (* 80 *)
	    write (filetowr, 'EXPORTED ITEM CANNOT HAVE SAME NAME THAN PROGRAM.') ;
	  36 :                                  (* 86 *)
	    write (filetowr, 'FUNCTION CANNOT BE ASSIGNED HERE ') ;
	  37 :                                  (*  87 *)
	    write (filetowr, 'THIS PROCEDURE MUST OCCUR  IN EXTERNAL LIST') ;
	  38 :                                  (* 88 *)
	    write (filetowr, 'INVALID DIRECTIVE') ;
	  43 :                                  (* 93 *)
	    write (filetowr, 'UNRESOLVED FORWARD TYPE DEFINITION') ;
	  46 :                                  (*  96 *)
	    write (filetowr, 'ILLEGAL POINTED ITEM') ;
	  47 :                                  (*  97 *)
	    write (filetowr, 'POINTER ON A VARIABLE MUST POINT A CLASS') ;
	  48 :                                  (*  98 *)
	    IF NOT pascalfrench THEN
	      write (filetowr, '''PACKED'' NOT ALLOWED HERE')
	    ELSE
	      write (filetowr, '''PAQUET'' NOT ALLOWED HERE') ;
	  49 :                                  (*  99 *)
	    write (filetowr, 'ILLEGAL FIRST ITEM FOR A SIMPLE TYPE') ;
	END ;                                   (* CASE *)
        END (* PR01 *) ;

(* ***********************************************PR02 < PRTERRMEANS*********** *)

      PROCEDURE pr02 (errnumod : integer) ;

(* C   ERRORS 100 TO 149   NUMBER IS 100+ERRNUMOD                             C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (* 100 *)
	    write (filetowr, 'EXTERNAL ITEM HAS YET BEEN USED') ;
	  1 :                                   (* 101 *)
	    write (filetowr, 'IDENTIFIER DECLARED TWICE') ;
	  2 :                                   (* 102 *)
	    write (filetowr, 'HIGH BOUND MUST NOT BE LOWER THAN LOW BOUND') ;
	  3 :                                   (* 103 *)
	    write (filetowr, 'IDENTIFIER IS NOT OF APPROPRIATE CLASS') ;
	  4 :                                   (* 104 *)
	    write (filetowr, 'IDENTIFIER NOT DECLARED') ;
	  5 :                                   (* 105 *)
	    write (filetowr, 'SIGN NOT ALLOWED HERE') ;
	  6 :                                   (* 106 *)
	    write (filetowr, 'INTEGER TYPE NOT ALLOWED HERE') ;
	  7 :                                   (*  107 *)
	    write (filetowr, 'ERROR IN THE SELECTOR OF A RECORD') ;
	  8 :                                   (* 108 *)
	    write (filetowr, 'FILE  NOT ALLOWED HERE') ;
	  9 :                                   (*  109 *)
	    write (filetowr, 'TYPE MUST NOT BE REAL') ;
	  10 :                                  (* 110 *)
	    write (filetowr, 'ERROR IN THE TYPE IDENTIFIER OF A TAG FIELD') ;
	  11 :                                  (* 111 *)
	    write (filetowr, 'TYPE INCOMPATIBLE WITH THE TYPE OF THE TAG FIELD') ;
	  12 :                                  (* 112 *)
	    write (filetowr, 'TOO LARGE ARRAY .MAX SIZE IS ONE SEGMENT') ;
	  13 :                                  (* 113 *)
	    write (filetowr, 'INDEX TYPE MUST BE SCALAR OR NUMERIC') ;
	  14 :                                  (* 114 *)
	    write (filetowr, 'SUBRANGE TYPE MUST BE SCALAR OR NUMERIC') ;
	  15 :                                  (* 115 *)
	    write (filetowr, 'BASE TYPE OF A SET MUST BE SCALAR OR NUMERIC') ;
	  16 :                                  (* 116 *)
	    write (filetowr, 'CONFLICT BETWEEN FIRST DECLARATION AND REDECLARATION FORWARD') ;
	  17 :                                  (* 117 *)
	    write (filetowr, 'UNDEFINED FORWARD DECLARED PROCEDURE') ;
	  19 :                                  (* *)
	    write (filetowr, 'REPETITION OF PARAMETERS'' LIST NOT ALLOWED (FORWARD DECLARATION)') ;
	  20 :                                  (* *)
	    write (filetowr, 'FUNCTION RESULT TYPE MUST BE SCALAR,REAL,SUBRANGE OR POINTER') ;
	  21 :                                  (* 119,120,121 *)
	    write (filetowr, 'FILE OR CLASS PARAMETERS MUST BE VAR PARAMETERS') ;
	  23 :                                  (* 123 *)
	    write (filetowr, 'MISSING RESULT''S TYPE IN FUNCTION DECLARATION') ;
	  24 :                                  (* 124 *)
	    write (filetowr, 'CONFORMANT ARRAY PARAMETERS MUST BE VAR PARAMETERS') ;
	  25 :                                  (*  125 *)
	    write (filetowr, 'ERROR IN TYPE OF STANDARD FUNCTION OR PROCEDURE PARAM.') ;
	  26 :                                  (*  126 *)
	    write (filetowr, 'NUMBER OF PARAMETERS DOES NOT AGREE WITH DECLARATION') ;
	  27 :                                  (*  127 *)
	    write (filetowr, 'ILLEGAL PARAMETER SUBSTITUTION ') ;
	  28 :                                  (*  128 *)
	    write (filetowr, 'PARAMETER CONFLICT  FOR FORMAL PROCEDURE ') ;
	  29 :                                  (*  129 *)
	    write (filetowr, 'OPERAND TYPE CONFLICT') ;
	  30 :                                  (* 130 *)
	    write (filetowr, 'NIL NO MORE ALLOWED IN CONSTANT PART (STANDARD)') ;
	  31 :                                  (*  131 *)
	    write (filetowr, 'STRINGS LENGTH  CONFLICT ') ;
	  33 :                                  (*  133 *)
	    write (filetowr, 'ILLEGAL CONFORMANT ARRAY  SUBSTITUTION') ;
	  34 :                                  (*  134 *)
	    write (filetowr, 'ILLEGAL TYPE OF OPERAND') ;
	  35 :                                  (*  135 *)
	    write (filetowr, 'TYPE OF OPERAND MUST BE BOOLEAN') ;
	  38 :                                  (* 138 *)
	    write (filetowr, 'TYPE OF THIS VARIABLE IS NOT ARRAY OR RECORD') ;
	  39 :                                  (*  139 *)
	    write (filetowr, 'INDEX TYPE IS NOT COMPATIBLE WITH ITS DECLARATION') ;
	  40 :                                  (*  140 *)
	    write (filetowr, 'TYPE OF THIS VARIABLE MUST BE RECORD') ;
	  41 :                                  (*  141 *)
	    write (filetowr, 'TYPE OF THIS VARIABLE MUST BE FILE OR POINTER') ;
	  42 :                                  (*  142 *)
	    write (filetowr, 'TYPE OF THIS VARIABLE MUST BE ARRAY') ;
	  43 :                                  (* 143 *)
	    write (filetowr, 'ELEMENT TYPE ALLOWED IS SCALAR, NUMERIC OR POINTER') ;
	  44 :                                  (*  144 *)
	    write (filetowr, 'ILLEGAL TYPE OF EXPRESSION') ;
	  45 :                                  (* 145 *)
	    write (filetowr, 'TYPE CONFLICT') ;
	  46 :                                  (*  146 *)
	    write (filetowr, 'ASSIGNEMENT TO FILE OR CLASS NOT ALLOWED') ;
	  47 :                                  (* 147 *)
	    write (filetowr, 'TYPE CONFLICT WITH THE CASE SELECTOR') ;
	  48 :                                  (* 148 *)
	    write (filetowr, 'CASE VECTOR TRANSFER TOO LARGE FOR THIS PROCEDURE') ;
	  49 :                                  (* 149 *)
	    write (filetowr, 'EXTERNAL IDENT NOT REDEFINED   ') ;
	END ;                                   (* CASE *)
        END (* PR02 *) ;

(* ***********************************************PR03 < PRTERRMEANS*********** *)

      PROCEDURE pr03 (errnumod : integer) ;

(* C   ERRORS  150 TO 199  NUMBER IS  150+ERRNUMOD                            C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (*  150 *)
	    write (filetowr, 'ASSIGNEMENT TO STANDARD FUNCTION NOT ALLOWED') ;
	  2 :                                   (*  152 *)
	    write (filetowr, 'NO SUCH FIELD IN THIS RECORD') ;
	  3 :                                   (* 153 *)
	    write (filetowr, 'ILLEGAL TYPE FOR ITEM READ') ;
	  5 :                                   (* 155 *)
	    write (filetowr, 'FUNCTION IDENTIFIER HAS NOT BEEN ASSIGNED') ;
	  6 :                                   (* 156 *)
	    write (filetowr, 'DUPLICATE CASE LABEL') ;
	  8 :                                   (*  158 *)
	    write (filetowr, 'VARIANT SELECTOR DOES NOT MATCH WITH DECLARATION') ;
	  9 :                                   (*  159 *)
	    write (filetowr, 'UNPACKED ARRAY EXPECTED') ;
	  10 :                                  (*  160 *)
	    write (filetowr, 'PACKED ARRAY EXPECTED') ;
	  11 :                                  (*  161 *)
	    write (filetowr, 'CONFORMANT ARRAY NOT READY ( Restriction temporary ) FOR PACK AND UNPACK') ;
	  12 :                                  (*  162 *)
	    write (filetowr, 'ORIGIN AND TARGET NOT COMPATIBLE') ;
	  13 :                                  (*  163 *)
	    write (filetowr, 'ELEMENT TOO LARGE') ;
	  15 :                                  (*  165 *)
	    write (filetowr, 'MULTIDEFINED LABEL') ;
	  16 :                                  (* 166 *)
	    write (filetowr, 'MULTIDECLARED LABEL') ;
	  17 :                                  (*  167 *)
	    write (filetowr, 'UNDECLARED LABEL') ;
	  18 :                                  (* 168 *)
	    write (filetowr, 'UNDEFINED LABEL(S).SEE MESSAGES LATER') ;
	  19 :                                  (* 169 *)
	    write (filetowr, 'ERROR IN BASE TYPE OF A SET') ;
	  25 :                                  (*  175 *)
	    IF pascalfrench THEN
	      write (filetowr, 'ENTREE USED AND NOT DECLARED')
	    ELSE
	      write (filetowr, 'INPUT USED AND NOT DECLARED') ;
	  26 :                                  (*  176 *)
	    IF pascalfrench THEN
	      write (filetowr, 'SORTIE USED AND NOT DECLARED')
	    ELSE
	      write (filetowr, 'OUTPUT USED AND NOT DECLARED') ;
	  28 :                                  (* 178 *)
	    write (filetowr, 'ALPHANUMERIC STRING IS TOO LONG') ;
	  29 :                                  (* 179 *)
	    write (filetowr, 'INITIALIZATION LIST IS TOO LONG') ;
	  30 :                                  (* 180 *)
	    write (filetowr, 'INITIALIZATION OF IMPORTED VARIABLE NOT ALLOWED') ;
	  31 :                                  (* 181 *)
	    write (filetowr, 'THIS VARIABLE MUST BE AN ARRAY OR A RECORD') ;
	  32 :                                  (* 182 *)
	    write (filetowr, 'PACKED VARIABLE NOT ALLOWED HERE') ;
	  33 :                                  (* 183 *)
	    write (filetowr, 'ILLEGAL VARIABLE TYPE IN VALUE PART') ;
	  34 :                                  (* 184 *)
	    write (filetowr, 'IDENTIFIER MUST BE A VARIABLE (VALUE PART)') ;
	  35 :                                  (* 185 *)
	    write (filetowr, 'VARIABLES MUST BE INITIALIZED IN THEIR DECLARATION ORDER') ;
	  37 :                                  (*  187 *)
	    write (filetowr, 'PROCEDURE USED AS A FUNCTION') ;
	  40 :                                  (* 190 *)
	    write (filetowr, 'TEXT FILE EXPECTED HERE') ;
	  41 :                                  (*  191 *)
	    write (filetowr, 'SCALING FACTOR ALLOWED ONLY FOR REAL') ;
	  44 :                                  (*  194 *)
	    write (filetowr, 'CONTROL VARIABLE MUST BE DECLARED AND USED AT SAME LEV.') ;
	  45 :                                  (*  195 *)
	    write (filetowr, 'CONTROL VARIABLE MUST BE SCALAR OR NUMERIC') ;
	  46 :                                  (*  196 *)
	    write (filetowr, 'THIS VARIABLE MUST NOT BE ASSIGNED') ;
	  47 :                                  (*  197 *)
	    write (filetowr, 'TRUNCATION OF STRING NOT ALLOWED') ;
	  48 :                                  (*  198 *)
	    write (filetowr, 'OPERATION ALLOWED ONLY ON TEXT FILE') ;
	  49 :                                  (* 199 *)
	    write (filetowr, 'CONTROL VARIABLE MUST NOT BE FORMAL OR EXTERNAL') ;
	END ;                                   (* CASE *)
        END (* PR03 *) ;

(* ***********************************************PR04 < PRTERRMEANS*********** *)

      PROCEDURE pr04 (errnumod : integer) ;

(* C   ERRORS  200 TO 249   NUMBER IS  ERRNUMOD+200                           C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (* 200 *)
	    write (filetowr, 'CHARACTER NOT ALLOWED IN PASCAL TEXT') ;
	  1 :                                   (* 201 *)
	    write (filetowr, 'ERROR IN A REAL CONSTANT. DIGIT EXPECTED') ;
	  2 :                                   (* 202 *)
	    write (filetowr, 'ERROR IN THE EXPONENT OF A REAL CONSTANT') ;
	  3 :                                   (* 203 *)
	    write (filetowr, 'INTEGER CONSTANT OUT OF RANGE') ;
	  4 :                                   (* 204 *)
	    write (filetowr, 'ILLEGAL DIGIT IN AN OCTAL CONSTANT') ;
	  5 :                                   (* 205 *)
	    write (filetowr, 'EXPONENT OF A REAL CONSTANT OUT OF RANGE') ;
	  6 :                                   (* 206 *)
	    write (filetowr, 'DECIMAL CONSTANT IS TOO LONG') ;
	  7 :                                   (* 207 *)
	    write (filetowr, 'OCTAL CONSTANT IS TOO LONG') ;
	  8 :                                   (* 208 *)
	    write (filetowr, 'ILLEGAL NESTING OF (/ AND /)') ;
	  9 :                                   (* 209 *)
	    write (filetowr, 'CHARACTERS'' STRING IS TOO LONG') ;
	  10 :                                  (* 210 *)
	    write (filetowr, 'HEXADECIMAL STRING IS TOO LONG') ;
	  11 :                                  (* 211 *)
	    write (filetowr, 'ILLEGAL CHARACTER IN A HEXADECIMAL STRING') ;
	  12 :                                  (* 212 *)
	    write (filetowr, 'ERROR IN COMPILATION OPTIONS') ;
	  13 :                                  (*  213 *)
	    write (filetowr, 'STACK FRAME MUST NOT EXCEED 60000 WORDS') ;
	  14 :                                  (* 214 *)
	    write (filetowr, 'SIZE ALLOWED FOR GLOBALS EXCEEDED') ;
	  15 :                                  (* 215 *)
	    write (filetowr, 'TOO MANY BINARY DIGITS.MAX IS 36 ') ;
	  16 :                                  (* 216 *)
	    write (filetowr, 'INVALID BINARY DIGIT. 0 OR 1 EXPECTED ') ;
	  17 :                                  (* 217 *)
	    write (filetowr, 'REAL CONSTANT CANNOT BE > 1.701411834604692317E+38') ;
	  18 :                                  (* 218 *)
	    write (filetowr, 'NON NULL REAL CONSTANT CANNOT BE < 1.469367938527859385E-39') ;
	  19 :                                  (* 218 *)
	    write (filetowr, 'WARNING : MAXIMUM PRECISION FOR A REAL IS 19 DIGITS') ;
	  20 :                                  (* 220 *)
	    write (filetowr, 'EMPTY STRING NOT ALLOWED    ') ;
	  21 :                                  (* 221 *)
	    IF pascalfrench THEN
	      write (filetowr, '''SINON'' ALREADY USED IN THIS CASE STATEMENT') ELSE
	      write (filetowr, '''ELSE'' ALREADY USED IN THIS CASE STATEMENT') ;
	  22 :                                  (* 222 *)
	    write (filetowr, 'WARNING : OPTION ACCEPTED BUT INEFFECTIVE.') ;
	  23 :                                  (* 223 *)
	    write (filetowr, 'ILLEGAL SEPARATOR AFTER NUMBER READ ') ;
	  24 :                                  (* 224 *)
	    write (filetowr, 'REFERENCE TO THIS IDENTIFIER IS NOT ALLOWED HERE') ;
	  25 :                                  (* 225 *)
	    write (filetowr, 'THIS EXPRESSION CANNOT BE EVALUATED HERE : IT NEEDS CODE GENERATION') ;
	  26 :                                  (* 226 *)
	    write (filetowr, 'THIS IDENTIFIER HAS BEEN PREVIOUSLY REFERENCED AT SAME LEVEL') ;
	  27 :                                  (* 227 *)
	    write (filetowr, 'SOME LABELS DECLARED IN THIS PROCEDURE ARE ILLEGALLY REFERENCED') ;
	  28 :                                  (* 228 *)
	    write (filetowr, 'INTEGER OVERFLOW IN EXPRESSION') ;
	  29 :                                  (* 229 *)
	    write (filetowr, 'INTEGER UNDERFLOW IN EXPRESSION') ;
	  30 :                                  (* 230 *)
	    write (filetowr, 'EFFECTIVE PARAMETER PASSED BY VALUE CANNOT BE A CONFORMANT ARRAY') ;
	  31 :                                  (* 231 *)
	    write (filetowr, 'CONSTANT CHAIN CANNOT CONTAIN A NEW-LINE') ;
	END ;                                   (* CASE *)
        END (* PR04 *) ;

(* ***********************************************PR05 < PRTERRMEANS*********** *)

      PROCEDURE pr05 (errnumod : integer) ;

(* C   ERRORS  250 TO 299   NUMBER IS 250+ERRNUMOD                            C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (*  250 *)
	    write (filetowr, 'TOO MANY NESTED SCOPES OF IDENTIFIERS') ;
	  1 :                                   (* 251 *)
	    write (filetowr, 'TOO MANY NESTED PROCEDURES AND(OR) FUNCTIONS') ;
	  2 :                                   (* 252 *)
	    write (filetowr, 'COMPILER''S HEAP IS FULL. INCREASE REGION') ;
	  3 :                                   (* 253 *)
	    write (filetowr, 'CODE FOR THIS PROCEDURE ( OR VALUE ) IS TOO LONG') ;
	  4 :                                   (* 254 *)
	    write (filetowr, 'EXPRESSION TOO COMPLICATED') ;
	  5 :                                   (* 255 *)
	    write (filetowr, 'TOO MANY ERRORS ON THIS LINE') ;
	  6 :                                   (* 256 *)
	    write (filetowr, 'FCONNECT IS ONLY ALLOWED ON PERMANENT FILES') ;
	  7 :                                   (* 257 *)
	    write (filetowr, 'SOURCE LINE IS TOO LONG') ;
	  8 :                                   (* 258 *)
	    write (filetowr, 'TOO MANY FILES') ;
	  10 :                                  (* 260 *)
	    write (filetowr, 'STARTING POINT FOR THIS VARIABLE EXCEED IMPLEMENTATION LIMIT') ;
	  11 :                                  (*  261 *)
	    write (filetowr, 'TOO MANY UNRESOLVED REFERENCES (UNDLAB)') ;
	  17 :                                  (* 267 *)
	    write (filetowr, 'TOO MANY LABELS') ;
	  18 :                                  (* 268 *)
	    write (filetowr, 'TOO MANY FORWARD DEFINED POINTERS') ;
	  19 :                                  (* 269 *)
	    write (filetowr, 'TOO MANY CLASSES') ;
	  20 :                                  (*    270 *)
	    write (filetowr, 'NOT YET IMPLEMENTED') ;
	  21 :                                  (* 271 *)
	    write (filetowr, 'ACTUAL SCHEMA PARAMETER IS OF ILLEGAL TYPE') ;
	  22 :                                  (* 272 *)
	    write (filetowr, 'ACTUAL SCHEMA PARAMETER IS OUT OF BOUNDS') ;
	  23 :                                  (* 273 *)
	    write (filetowr, 'TARGET STRING IS TOO SHORT') ;
	  24 :                                  (* 274 *)
	    write (filetowr, 'STRING EXPRESSION EXPECTED') ;
	  25 :                                  (* 275 *)
	    write (filetowr, 'STRING VARIABLE REFERENCE EXPECTED') ;
	  26 :                                  (* 276 *)
	    write (filetowr, 'ERROR IN DELETE : SUBSTRING TO DELETE IS OUT OF STRING BOUNDS.') ;
	  27 :                                  (* 277 *)
	    write (filetowr, 'ERROR IN DELETE : SUBSTRING TO DELETE HAS NEGATIVE LENGTH') ;
	  28 :                                  (* 278 *)
	    write (filetowr, 'ERROR IN SUBSTRING : SUBSTRING IS OUT OF STRING BOUNDS') ;
	  29 :                                  (* 279 *)
	    write (filetowr, 'ERROR IN SUBSTRING : SUBSTRING HAS NEGATIVE LENGTH') ;
	  30 :                                  (* 280 *)
	    write (filetowr, 'INTEGER EXPRESSION EXPECTED') ;
	  31 :                                  (* 281 *)
	    write (filetowr, 'THIS PARAMETER MUST BE PASSED BY ADDRESS') ;
	END ;                                   (* CASE *)
        END (* PR05 *) ;

(* ***********************************************PR06 < PRTERRMEANS*********** *)

      PROCEDURE pr06 (errnumod : integer) ;

(* C   ERRORS 300 TO 349   NUMBER IS 300+ERRNUMOD                             C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (*  300 *)
	    write (filetowr, 'ZERO DIVIDE  CAN BE NOT SUITABLE ') ;
	  1 :                                   (* 301 *)
	    write (filetowr, 'CASE VARIANT OUT OF BOUNDS') ;
	  2 :                                   (* 302 *)
	    write (filetowr, 'INDEX OUT OF BOUNDS') ;
	  3 :                                   (* 303 *)
	    write (filetowr, 'VALUE ASSIGNED OUT OF BOUNDS') ;
	  4 :                                   (* 304 *)
	    write (filetowr, 'CASE LABEL OUT OF BOUNDS') ;
	  5 :                                   (* 305 *)
	    write (filetowr, 'VALUE IN A SET OUT OF BOUNDS') ;
	  6 :                                   (* 306 *)
	    write (filetowr, 'LABEL MUST HAVE AT MOST 4 DIGITS') ;
	  7 :                                   (*  307 *)
	    write (filetowr, 'ITEMS COMPARED TOO LONG ') ;
	  8 :                                   (* 308 *)
	    write (filetowr, 'RIGHT ARGUMENT OF DIV IS NULL') ;
	  9 :                                   (* 309 *)
	    write (filetowr, 'RIGHT ARGUMENT OF MOD IS NEGATIVE OR NULL') ;
	  10 :                                  (* 310 *)
	    write (filetowr, 'VALUE ALREADY USED IN CASE SELECTOR ') ;
	  11 :                                  (* 311 *)
	    write (filetowr, 'ALL POSSIBLE CASE VALUES ARE NOT MENTIONED') ;
	  12 :                                  (* 312 *)
	    writeln (filetowr, 'IMPLEMENTATION RESTRICTION: MAX NUMBER OF POSSIBLE CASE VALUES IS 288.') ;
	  13 :                                  (* 313 *)
	    writeln (filetowr, 'WARNING : ALL POSSIBLE CASE VALUES ARE NOT MENTIONNED') ;
	  14 :                                  (* 314 *)
	    write (filetowr, '''TRUE'', ''FALSE'', ''NOT'' OR CONDITIONNAL COMPILATION SWITCH NAME EXPECTED') ;
	  15 :                                  (* 315 *)
	    write (filetowr, 'CONDITIONNAL COMPILATION SWITCH NOT DEFINED') ;
	  16 :                                  (* 316 *)
	    write (filetowr, '''TRUE'', ''FALSE'' OR CONDITIONNAL COMPILATION SWITCH NAME EXPECTED') ;
	  17 :                                  (* 317 *)
	    write (filetowr, ''','' OR '':'' EXPECTED') ;
	  18 :                                  (* 318 *)
	    write (filetowr, 'PARAMETER PROCEDURE PASSED TO AN EXTERNAL PROCEDURE MUST BE EXPORTABLE') ;
	  44 :                                  (* 344 *)
	    write (filetowr, 'EXTENDED DISPOSE NOT ALLOWED') ;
	  45 :                                  (* 345 *)
	    write (filetowr, 'NEW IS LIMITED TO 261094 WORDS') ;
	END ;                                   (* CASE *)
        END (* PR06 *) ;

(* ***********************************************PR07  < PRTERRMEANS********** *)

      PROCEDURE pr07 (errnumod : integer) ;

(* C   ERRORS 350 TO 399   NUMBER IS 350+ERRNUMOD                             C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (* 350 *)
	    write (filetowr, '(RECADRE) BAD ARGUMENTS') ;
	  1 :                                   (* 351 *)
	    write (filetowr, '(BYTESNEEDED) OBJFORM=ALIASTYPE') ;
	  2 :                                   (* 352 *)
	    write (filetowr, '(BYTESNEEDED) BAD ARGUMENT') ;
	  3 :                                   (* 353 *)
	    write (filetowr, '(BOUNDARY) OBJFORM=ALIASTYPE') ;
	  4 :                                   (* 354 *)
	    write (filetowr, '(BOUNDARY) BAD ARGUMENT') ;
	  5 :                                   (* 355 *)
	    write (filetowr, '(GENSTAND) ILLEGAL SHIFT COUNT') ;
	  6 :                                   (* 356 *)
	    write (filetowr, '(GENSTAND) ILLEGAL OP. CODE WITHOUT POINTER REGISTER') ;
	  7 :                                   (* 357 *)
	    write (filetowr, '(GENSTAND) TAG FIELD INCOMPATIBLE WITH OP. CODE') ;
	  8 :                                   (* 358 *)
	    write (filetowr, '(GENWITHPR) ILLEGAL ADDRESS WITHOUT POINTER REGISTER') ;
	  9 :                                   (* 359 *)
	    write (filetowr, 'TEMPORARY RESTRICTION: GLOBALS MUST BE < 16384 WORDS ') ;
	  10 :                                  (* 360 *)
	    write (filetowr, '(GENSTOBC) ILLEGAL BYTES'' POSITION FIELD') ;
	  11 :                                  (* 361 *)
	    write (filetowr, '(GENREPT) ILLEGAL TALLY') ;
	  12 :                                  (* 362 *)
	    write (filetowr, '(GENREPT) ILLEGAL TERMINATION CONDITION') ;
	  13 :                                  (* 363 *)
	    write (filetowr, '(GENREPT) ILLEGAL DELTA') ;
	  14 :                                  (* 364 *)
	    write (filetowr, '(GENREPT) BITS 8,9,10 INCOMPATIBLE WITH OP. CODE') ;
	  15 :                                  (* 365 *)
	    write (filetowr, '(GENIPAIR) ILLEGAL SEGMENT NUMBER') ;
	  16 :                                  (* 366 *)
	    write (filetowr, '(GENIPAIR) ILLEGAL SECOND WORD IN AN ITP OR ITS PAIR') ;
	  17 :                                  (* 367 *)
	    write (filetowr, '(GENEISM)  ILLEGAL TAG IN AN EIS MODIFICATION FIELD') ;
	  18 :                                  (* 368 *)
	    write (filetowr, '(GENEISM) BITS 0,9,10 INCOMPATIBLE WITH OP. CODE') ;
	  19 :                                  (* 369 *)
	    write (filetowr, '(GENEISM) ILLEGAL FIELD 0-8') ;
	  20 :                                  (* 370 *)
	    write (filetowr, '(GENINDW) ILLEGAL TAG IN AN INDIRECT WORD') ;
	  21 :                                  (* 371 *)
	    write (filetowr, '(GENINDW) USE OF PREG NOT ALLOWED IN AN INDIRECT WORD') ;
	  22 :                                  (* 372 *)
	    write (filetowr, '(LENGTHCTRL) ILLEGAL EIS OPERAND LENGTH') ;
	  23 :                                  (* 373 *)
	    write (filetowr, '(GENDESCA_B_N) ILLEGAL CHARACTERS'' COUNT') ;
	  24 :                                  (* 374 *)
	    write (filetowr, '(LENGTHCTRL) ILLEGAL MODIFIER') ;
	  25 :                                  (* 375 *)
	    write (filetowr, '(GENDESCB) ILLEGAL BITS'' COUNT') ;
	  26 :                                  (* 376 *)
	    write (filetowr, '(GENDESCN) ILLEGAL SCALING FACTOR') ;
	  27 :                                  (* 377 *)
	    write (filetowr, '(GENINDIT) ILLEGAL TALLY OR TAG') ;
	  29 :                                  (* 379 *)
	    write (filetowr, '(PACKEDSIZE) ILLEGAL ITEM') ;
	  31 :                                  (* 381 *)
	    write (filetowr, '(ERROR) ERROR NUMBER IS TOO HIGH') ;
	  32 :                                  (* 382 *)
	    write (filetowr, '(ERROR) PAGE NUMBER IS TOO HIGH') ;
	  33 :                                  (* 383 *)
	    write (filetowr, '(NEXTPAGE) PAGE NUMBER BECOMES TOO HIGH') ;
	  34 :                                  (* 384 *)
	    write (filetowr, '(CHECKMINMAX) FCTP=NIL') ;
	  35 :                                  (* 385 *)
	    write (filetowr, '(CHECKMINMAX) FCTP@.FORM IS BAD') ;
	  36 :                                  (* 386 *)
	    write (filetowr, '(CHECKMINMAX) FCONST=NIL') ;
	  40 :                                  (* 390 *)
	    write (filetowr, 'LOCAL (STACK) STORAGE OVERFLOW : CANNOT BE > 16384 WORDS') ;
	  41 :                                  (* 391 *)
	    write (filetowr, 'ILLEGAL OFFSET IN INSTRUCTION GENERATION. CONTACT MAINTENANCE.') ;
	END ;                                   (* CASE *)
        END (* PR07 *) ;

(* ***********************************************PR08 < PRTERRMEANS*********** *)

      PROCEDURE pr08 (errnumod : integer) ;

(* C   ERRORS  400 TO 449    NUMBER IS 400+ERRNUMOD                           C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (*  400 *)
	    write (filetowr, 'LDREGBLOC IS NIL(TRANSFER OUT)') ;
	  1 :                                   (*  401 *)
	    write (filetowr, 'LCOND IS SAVED(TRANSFER IN)') ;
	  2 :                                   (*  402 *)
	    write (filetowr, 'FORM # NUMERIC(CONVREAL)') ;
	  3 :                                   (*  403 *)
	    write (filetowr, 'BLOC NOT FOUND(SAUVEREG)') ;
	  4 :                                   (*  404 *)
	    write (filetowr, 'REGISTER ALREADY SAVED(SAUVEREG)') ;
	  5 :                                   (*  405 *)
	    write (filetowr, 'FATTR IS NOT CHAIN OR VARBL(LOADADR)') ;
	  6 :                                   (*  406 *)
	    write (filetowr, 'FMIN > FMAX (INBOUNDS)') ;
	  7 :                                   (*  407 *)
	    write (filetowr, 'EMPTY STRING(INSERUNDLAB)') ;
	  8 :                                   (*  408 *)
	    write (filetowr, 'FPLACE OUT OF RANGE(INSER)') ;
	  9 :                                   (*  409 *)
	    write (filetowr, 'OFFSET TOO LARGE(INSER)') ;
	  10 :                                  (*  410 *)
	    write (filetowr, 'INSER ON HALF-WORD # 0 (INSER)') ;
	  11 :                                  (*  411 *)
	    write (filetowr, 'TYPTR = NIL(CONVREAL)') ;
	  12 :                                  (*  412 *)
	    write (filetowr, 'TYPTR = NIL(CALCVARIANT)') ;
	  13 :                                  (*  413 *)
	    write (filetowr, 'KIND = LVALNOT SAVED(CALCVARIANT)') ;
	  14 :                                  (*  414 *)
	    write (filetowr, 'KIND = CHAIN/LCOND (CALCVARANT)') ;
	  16 :                                  (*  416 *)
	    write (filetowr, 'LVAL SAVED (TRANSFER IN)') ;
	  17 :                                  (*  417 *)
	    write (filetowr, 'FREEBLOC CALLED WITH DUMMYBLOC') ;
	  18 :                                  (*  418 *)
	    write (filetowr, 'INCORRECT SOURCE (TRANSFER IN)') ;
	  19 :                                  (*  419 *)
	    write (filetowr, 'TYPSEQ=0 (GENOPMULT) ') ;
	  20 :                                  (*  420 *)
	    write (filetowr, 'FATTR.KIND # VARBL (TRANSFER OUT)') ;
	  21 :                                  (*  421 *)
	    write (filetowr, 'GATTR.KIND # LVAL (TRANSFER OUT)') ;
	  22 :                                  (*  422 *)
	    write (filetowr, 'GATTR.KIND CHAIN IN CHOICERARQ') ;
	  23 :                                  (*  423 *)
	    write (filetowr, 'FCTP = NIL (FINDMINMAX)') ;
	  24 :                                  (*  424 *)
	    write (filetowr, 'FCTP@.KLASS # TYPES (FINDMINMAX)') ;
	  25 :                                  (*  425 *)
	    write (filetowr, 'FATTR.KIND # LVAL (LVALVARBL)') ;
	  26 :                                  (*  426 *)
	    write (filetowr, 'NO BLOC ASSOCIATED TO THE REGISTER (LVALVARBL)') ;
	  27 :                                  (*  427 *)
	    write (filetowr, 'OLDBLOC = NIL (REGENERE)') ;
	  28 :                                  (*  428 *)
	    write (filetowr, 'REGISTER NOT SAVED AND NOT LOAD (REGENERE)') ;
	  29 :                                  (*  429 *)
	    write (filetowr, 'SOME REGISTER BOX NOT FREED (FREEALLREGISTER)') ;
	  30 :                                  (*  430 *)
	    write (filetowr, 'TYPTR = NIL (EASYVAR)') ;
	  31 :                                  (*  431 *)
	    write (filetowr, 'KIND # VARBL (EASYVAR)') ;
	  32 :                                  (*  432 *)
	    write (filetowr, 'TYPSEQ = 0 (GENOPADD)') ;
	  33 :                                  (*  433 *)
	    write (filetowr, 'TYPSEQ = 0 (GENOPSUB)') ;
	  34 :                                  (*  434 *)
	    write (filetowr, 'TYPSEQ=0 (GENCOMPARE)') ;
	  35 :                                  (*  435 *)
	    write (filetowr, 'REGISTER NOT SAVED AND NOT LOAD(FREEBLOC)') ;
	  36 :                                  (*  436 *)
	    write (filetowr, 'PROCKIND = FORMAL OR IMPORTED(GENPRCEXIT)') ;
	  37 :                                  (*  437 *)
	    write (filetowr, 'FORM NOT NUMERIC OR SCALAR (FINDMINMAX)') ;
	  38 :                                  (*  438 *)
	    write (filetowr, 'FCTP = NIL (ADDRESSVAR)') ;
	  39 :                                  (* 439 *)
	    write (filetowr, 'VERIF COHERENCE ERREUR PREMIER GROUPE ') ;
	  40 :                                  (* 440 *)
	    write (filetowr, 'VERIF COHERENCE ERREUR DEUXIEME GROUPE ') ;
	  41 :                                  (* 441 *)
	    write (filetowr, 'VERIF COHERENCE ERREUR TROISIEME GROUPE') ;
	  42 :                                  (* 442 *)
	    write (filetowr, 'GENBINAREA FAILED. CONTACT MAINTENANCE') ;
	  46 :                                  (* 446 *)
	    write (filetowr, ' (CHECKEXTERNALITEM) COMPILER ERROR ') ;
	  47 :                                  (* 447 *)
	    write (filetowr, 'EXPORTPARTDECL ERROR ') ;
	  48 :                                  (* 448 *)
	    write (filetowr, 'EXTERNAL DESCRIPTOR CANNOT BE GENERATED FOR SUCH A PARAMETER') ;
	END ;                                   (* CASE *)
        END (* PR08 *) ;

(* ***********************************************PR09 < PRTERRMEANS*********** *)
      PROCEDURE pr09 (errnumod : integer) ;

(* C   ERRORS  450 TO 499     NUMBER IS 450+ERRNUMOD                          C *)
        BEGIN
	CASE errnumod OF
	  0 : (* DUMMY *) ;
	END ;                                   (* CASE *)
        END (* PR09 *) ;

(* ***********************************************PR10 < PRTERRMEANS*********** *)

      PROCEDURE pr10 (errnumod : integer) ;

(* C   ERRORS 500 TO 549      NUMBER IS 500+ERRNUMOD                          C *)
        BEGIN
	CASE errnumod OF
	  0 :                                   (* 500 *)
	    write (filetowr, 'INTERNAL ERROR (Genentrypoint   . Exitlabel.) CONTACT MAINTENANCE') ;
	  1 :                                   (* 501 *)
	    write (filetowr, 'INTERNAL ERROR (Genentrypoint   . Genprolog main.) CONTACT MAINTENANCE') ;
	  2 :                                   (* 502 *)
	    write (filetowr, 'INTERNAL ERROR (Genentrypoint   . Link to main  .) CONTACT MAINTENANCE') ;
	  3 :                                   (* 503 *)
	    write (filetowr, 'INTERNAL ERROR (Genentrypoint   . Genprocentry  .) CONTACT MAINTENANCE') ;
	  4 :                                   (* 504 *)
	    write (filetowr, 'INTERNAL ERROR (Genbinarea      . Writout       .) CONTACT MAINTENANCE') ;
	  5 :                                   (* 505 *)
	    write (filetowr, 'INTERNAL ERROR (Genentrypoint   . Imported procedure) CONTACT MAINTENANCE') ;
	  6 :                                   (* 506 *)
	    write (filetowr, 'INTERNAL ERROR (Link pour export non init Valuedecl ) CONTACT MAINTENANCE') ;
	  7 :                                   (* 507 *)
	    write (filetowr, 'INTERNAL ERROR (Genbinarea      Valuedecl           ) CONTACT MAINTENANCE') ;
	  8 :                                   (* 508 *)
	    write (filetowr, 'INTERNAL ERROR (Genextvariable   Exportable   Init  ) CONTACT MAINTENANCE') ;
	  9 :                                   (* 509 *)
	    write (filetowr, 'INTERNAL ERROR (Genexportfile    Valuedecl          ) CONTACT MAINTENANCE') ;
	  10 :                                  (* 510 *)
	    write (filetowr, 'INTERNAL ERROR (Genentrypoint    LINKTOEND          ) CONTACT MAINTENANCE') ;
	  11 :                                  (* 511 *)
	    write (filetowr, 'ALREADY BUILDING TYPE FROM SHEMA (INTERNAL ERROR. PLEASE CONTACT MAINTENANCE)') ;
	END ;                                   (* CASE *)
        END (* PR10 *) ;

(* ***********************************************PR11 < PRTERRMEANS*********** *)

      PROCEDURE pr11 (errnumod : integer) ;

(* C   ERRORS 550 TO 599     NUMBER IS  550+ERRNUMOD                          C *)
        BEGIN
	CASE errnumod OF
	  0 : (* DUMMY *) ;
	END ;                                   (* CASE *)
        END (* PR11 *) ;

(* ***********************************************PR12 < PRTERRMEANS*********** *)

      PROCEDURE pr12 (errnumod : integer) ;

(* C   ERRORS 600 TO 639    NUMBER IS   600+ERRNUMOD                          C *)
        BEGIN
	CASE errnumod OF
	  0 : (* DUMMY *) ;
	  40, 41, 42, 43, 44, 45, 46, 47, 48, 49 :
	    write (filetowr, '*** (PR12) ERRNUMOD > 39 ***') ;
	END ;                                   (* CASE *)
        END (* PR12 *) ;

      BEGIN                                       (* PRTERRMEANS *)
        write (filetowr, ' ', errornum : 4, ' : ') ;
        i := errornum DIV 50 ; j := errornum MOD 50 ;
        CASE i OF
	0 : pr00 (j) ;
	1 : pr01 (j) ;
	2 : pr02 (j) ;
	3 : pr03 (j) ;
	4 : pr04 (j) ;
	5 : pr05 (j) ;
	6 : pr06 (j) ;
	7 : pr07 (j) ;
	8 : pr08 (j) ;
	9 : pr09 (j) ;
	10 : pr10 (j) ;
	11 : pr11 (j) ;
	12 : pr12 (j) ;
        END (* CASE I *) ;                        (* NEXTLINE MADE IN 'STATISTIQUES' *)
      END (* PRTERRMEANS *) ;


$OPTIONS page $

(* ************************************************* DISPLAYSYMBOLS **************************************** *)

    PROCEDURE displaysymbols ;

(* C CALLED IF LISTYES BY RACINE AT THE END OF COMPILATION
   PRINTS SYMBOL MAP ON LISTING OUTPUT                                C *)

      CONST
        llmax = 126 ;
      TYPE
        alfalistrange = 0..26 ;
      VAR
        i : integer ;
        tittle : boolean ;
        tittlestring : PACKED ARRAY [1..50] OF char ;
        p1, p2, refbox : refptr ;
        currlabbox : labelblockptr ;
        checkunused : boolean ;
        lastbox, cctp : ctp ;                     (* CURRENT SYMBOL BOX *)
        n, ll, it, lastit : integer ;
        alfalist : ARRAY [alfalistrange] OF RECORD
	firstname, lastname : ctp ;
        END ;
        output_string : PACKED ARRAY [1..200] OF char ;

(* ********************************************** PRINTOCT < DISPLAYSYMBOLS ************************ *)

      PROCEDURE printoct (nb : integer) ;

        VAR
	tab : ARRAY [1..7] OF integer ;
	j, k : integer ;

        BEGIN
	FOR j := 7 DOWNTO 1 DO
	  BEGIN
	    tab [j] := nb MOD 8 ;
	    nb := nb DIV 8 ;
	  END ;
	k := 1 ;
	WHILE (tab [k] = 0) AND (k < 7) DO
	  k := k + 1 ;
	FOR j := k TO 7 DO
	  ll := swrite (output_string, ll, chr (ord ('0') + tab [j])) ;
        END (* PRINTOCT *) ;


(* ************************************************* SPLIT < DISPLAYSYMBOLS ********************************* *)

      PROCEDURE split ;

        BEGIN
	writeln (mpcogout, output_string : ll - 1) ;
	IF checkunused THEN ll := maxident + 9 ELSE ll := maxident + 7 ;
	ll := ll + 2 ;                          (* INDENT *)
	ll := swrite (output_string, 1, '  ' : ll) ;
        END (* SPLIT *) ;


(* ************************************************* PRINTREFS < DISPLAYSYMBOLS ************************ *)

      PROCEDURE printrefs ;

        VAR
	p1, p2, refbox : refptr ;
	i, n : integer ;
	newl : boolean ;

        BEGIN
	IF ll >= llmax THEN split ;
	newl := false ;
	WITH cctp^ DO
	  BEGIN
	    IF defline <> 0 THEN BEGIN
	        ll := swrite (output_string, ll, ' ; DEF: ') ;
	        IF deffile <> 0 THEN
		ll := swrite (output_string, ll, deffile : 1, '-') ;
	        ll := swrite (output_string, ll, defline : 1) ;
	      END ;
	    IF references^.refnbr <> 0 THEN BEGIN
	        IF ll >= llmax THEN split ;
	        ll := swrite (output_string, ll, ' ; REF: ') ;
	        refbox := references ;
	        p1 := NIL ;
	        WHILE refbox^.nextref <> NIL DO BEGIN
		  p2 := refbox^.nextref ;
		  refbox^.nextref := p1 ;
		  p1 := refbox ;
		  refbox := p2 ;
		END ;
	        refbox^.nextref := p1 ;
	        REPEAT
		WITH refbox^ DO
		  FOR i := 1 TO refnbr DO
		    WITH refs [i] DO
		      BEGIN
		        IF ll >= llmax THEN split ;
		        IF filen <> 0 THEN
			ll := swrite (output_string, ll, filen : 1, '-') ;
		        IF linen < 0 THEN
			ll := swrite (output_string, ll, -linen : 1, '* ')
		        ELSE
			ll := swrite (output_string, ll, linen : 1, ' ') ;
		      END ;
		refbox := refbox^.nextref ;
	        UNTIL refbox = NIL ;
	      END ;
	    writeln (mpcogout, output_string : ll - 1) ;
	  END
        END (* PRINTREFS *) ;

(* *********************************************** PRINTTYPE < DISPLAYSYMBOLS **************************** *)

      PROCEDURE printtype (cctp : ctp) ;

        VAR
	m, i : integer ;

        BEGIN
	IF ll >= llmax THEN split ;
	WITH cctp^ DO BEGIN
	    IF (defline = 0) AND (name <> blank) THEN (* PREDEFINED *)
	      BEGIN
	        i := 1 ;
	        WHILE name [i] <> ' ' DO BEGIN
		  ll := swrite (output_string, ll, name [i]) ;
		  i := i + 1 ;
		END ;
	      END
	    ELSE
	      IF cctp^.father_schema <> NIL THEN
	        BEGIN
		IF cctp^.father_schema <> NIL THEN
		  WITH cctp^.father_schema^ DO
		    BEGIN
		      i := 1 ;
		      REPEAT
		        IF name [i] <> ' ' THEN
			ll := swrite (output_string, ll, name [i])
		        ELSE
			i := maxident ;
		        i := i + 1
		      UNTIL i > maxident ;
		    END ;
	        END
	      ELSE
	        CASE form OF
		reel :
		  ll := swrite (output_string, ll, 'real') ;
		numeric :
		  ll := swrite (output_string, ll, 'numeric ', nmin : 1, '..', nmax : 1) ;
		scalar :
		  BEGIN
		    ll := swrite (output_string, ll, 'scalar') ;
		    IF subrng THEN
		      ll := swrite (output_string, ll, ' subrange') ;
		  END ;
		pointer :
		  ll := swrite (output_string, ll, 'pointer') ;
		power :
		  BEGIN
		    ll := swrite (output_string, ll, 'set of (') ;
		    IF cctp^.elset <> NIL THEN printtype (cctp^.elset) ;
		    ll := swrite (output_string, ll, ')') ;
		  END ;
		arrays :
		  BEGIN
		    IF conformant THEN
		      ll := swrite (output_string, ll, 'conformant ') ;
		    ll := swrite (output_string, ll, 'array of (') ;
		    IF cctp^.aeltype <> NIL THEN printtype (cctp^.aeltype) ;
		    ll := swrite (output_string, ll, ')') ;
		  END ;
		records :
		  ll := swrite (output_string, ll, 'record') ;
		files :
		  ll := swrite (output_string, ll, 'file') ;
		aliastype :
		  IF cctp^.realtype <> NIL THEN printtype (cctp^.realtype) ;
	        END ;
	  END
        END (* PRINTTYPE *) ;

(* *********************************************************** PRINTSYMBOL < DISPLAYSYMBOLS ******************* *)

      PROCEDURE printsymbol ;

        VAR
	dw, bc : integer ;
	i : integer ;
	bp : blocknodeptr ;
	lctp : ctp ;

        BEGIN
	IF NOT tittle THEN
	  BEGIN
	    writeln (mpcogout, '        ', tittlestring) ;
	    writeln (mpcogout) ;
	    tittle := true ;
	  END ;
	WITH cctp^ DO
	  BEGIN
	    CASE klass OF
	      schema :
	        BEGIN
		IF checkunused THEN
		  ll := swrite (output_string, 1, name, ' * schem ') ELSE
		  ll := swrite (output_string, 1, name, ' schem ') ;
		printrefs ;
	        END ;
	      types :
	        BEGIN
		IF checkunused THEN
		  ll := swrite (output_string, 1, name, ' * type  ') ELSE
		  ll := swrite (output_string, 1, name, ' type  ') ;
		IF cctp^.pack THEN
		  IF cctp^.defline <> 0 THEN
		    ll := swrite (output_string, ll, 'packed ') ;
		printtype (cctp) ;
		printrefs ;
	        END ;
	      vars :
	        BEGIN
		IF checkunused THEN
		  ll := swrite (output_string, 1, name, ' * var   ') ELSE
		  ll := swrite (output_string, 1, name, ' var   ') ;
		CASE vkind OF
		  actual :
		    BEGIN
		      IF vlevel = 0 THEN
		        ll := swrite (output_string, ll, 'global')
		      ELSE BEGIN
			ll := swrite (output_string, ll, 'local to ') ;
			IF nxtel <> NIL THEN
			  WITH nxtel^ DO
			    BEGIN
			      i := 1 ;
			      REPEAT
			        IF name [i] <> ' ' THEN
				ll := swrite (output_string, ll, name [i])
			        ELSE
				i := maxident ;
			        i := i + 1
			      UNTIL i > maxident ;
			    END ;
		        END ;
		      ll := swrite (output_string, ll, ', loc:') ;
		      dw := vaddr DIV bytesinword ;
		      bc := (vaddr MOD bytesinword) * bitsinbyte ;
		      printoct (dw) ;
		      IF bc <> 0 THEN
		        ll := swrite (output_string, ll, '(', bc : 1, ')') ;
		    END ;
		  formal, arraybound :
		    BEGIN
		      IF varparam THEN
		        ll := swrite (output_string, ll, 'var ') ;
		      ll := swrite (output_string, ll, 'parameter of ') ;
		      IF nxtel <> NIL THEN
		        WITH nxtel^ DO
			BEGIN
			  i := 1 ;
			  REPEAT
			    IF name [i] <> ' ' THEN
			      ll := swrite (output_string, ll, name [i])
			    ELSE
			      i := maxident ;
			    i := i + 1
			  UNTIL i > maxident ;
			END ;
		    END ;
		  exportable :
		    ll := swrite (output_string, ll, 'global exportable') ;
		  imported :
		    ll := swrite (output_string, ll, 'global imported') ;
		END ;
		IF vtype <> NIL THEN BEGIN
		    IF vtype^.form <> files THEN
		      BEGIN
		        ll := swrite (output_string, ll, ', size:') ;
		        printoct (vtype^.size) ;
		      END ;
		    ll := swrite (output_string, ll, ' ; ') ;
		    IF vtype^.pack THEN
		      IF vtype^.defline <> 0 THEN
		        ll := swrite (output_string, ll, 'packed ') ;
		    printtype (vtype) ;
		  END ;
		printrefs ;
	        END ;
	      field :
	        BEGIN
		IF checkunused THEN
		  ll := swrite (output_string, 1, name, '   field disp:') ELSE
		  ll := swrite (output_string, 1, name, ' field disp:') ;
		printoct (fldaddr) ;
		ll := swrite (output_string, ll, ', size:') ;
		printoct (bytwidth) ;
		IF fldtype <> NIL THEN BEGIN
		    ll := swrite (output_string, ll, ' ; ') ;
		    IF fldtype^.pack THEN
		      IF fldtype^.defline <> 0 THEN
		        ll := swrite (output_string, ll, 'packed ') ;
		    printtype (fldtype) ;
		  END ;
		printrefs ;
	        END ;
	      konst :
	        BEGIN
		IF checkunused THEN
		  IF (typofconst = wordconst) AND (contype <> NIL) THEN
		    IF contype^.form IN [numeric, pointer, reel] THEN
		      ll := swrite (output_string, 1, name, ' * const ')
		    ELSE ll := swrite (output_string, 1, name, '   const ')
		  ELSE ll := swrite (output_string, 1, name, '   const ')
		ELSE
		  ll := swrite (output_string, 1, name, ' const ') ;
		CASE typofconst OF
		  wordconst :
		    IF contype <> NIL THEN
		      CASE contype^.form OF
		        numeric :
			ll := swrite (output_string, ll, 'numeric') ;
		        scalar :
			ll := swrite (output_string, ll, 'scalar, ord=', values : 1) ;
		        pointer :
			ll := swrite (output_string, ll, 'nil pointer') ;
		      END ;
		  dwordconst :
		    ll := swrite (output_string, ll, 'real') ;
		  alfaconst :
		    ll := swrite (output_string, ll, 'alphanumeric, ', alfalong : 1, ' char(s)') ;
		END ;
		printrefs ;
	        END ;
	      proc :
	        BEGIN
		IF checkunused THEN
		  IF proctype = cctp THEN
		    ll := swrite (output_string, 1, name, ' * proc  ')
		  ELSE ll := swrite (output_string, 1, name, ' * funct ')
		ELSE
		  IF proctype = cctp THEN
		    ll := swrite (output_string, 1, name, ' proc  ')
		  ELSE ll := swrite (output_string, 1, name, ' funct ') ;
		lctp := nxtel ;
		CASE prockind OF
		  actual :
		    IF lctp = NIL THEN
		      ll := swrite (output_string, ll, 'level 0')
		    ELSE
		      WITH lctp^ DO
		        BEGIN
			ll := swrite (output_string, ll, 'of ') ;
			i := 1 ;
			REPEAT
			  IF name [i] <> ' ' THEN
			    ll := swrite (output_string, ll, name [i])
			  ELSE
			    i := maxident ;
			  i := i + 1
			UNTIL i > maxident ;
		        END ;
		  formal :
		    BEGIN
		      ll := swrite (output_string, ll, 'parameter of ') ;
		      IF lctp <> NIL THEN
		        WITH lctp^ DO
			BEGIN
			  i := 1 ;
			  REPEAT
			    IF name [i] <> ' ' THEN
			      ll := swrite (output_string, ll, name [i])
			    ELSE
			      i := maxident ;
			    i := i + 1
			  UNTIL i > maxident ;
			END ;
		    END ;
		  exportable :
		    ll := swrite (output_string, ll, 'level 0, exportable') ;
		  imported :
		    ll := swrite (output_string, ll, 'level 0, imported') ;
		END ;
		IF proctype <> cctp THEN
		  IF proctype <> NIL THEN BEGIN
		      ll := swrite (output_string, ll, ' ; ') ;
		      IF proctype = nilptr THEN
		        ll := swrite (output_string, ll, '(standard)')
		      ELSE
		        printtype (proctype)
		    END ;
		printrefs ;
	        END ;
	      tagfield, dummyclass :
	    END ;
	    IF lastbox = NIL THEN firstalfa := alfathread
	    ELSE lastbox^.alfathread := alfathread ;
	  END ;
        END (* PRINTSYMBOL *) ;

(* ******************************************** SEARCHINTYPE < DISPLAYSYMBOLS ********************************* *)

      PROCEDURE sortlevel (cctp : ctp) ; FORWARD ;

      PROCEDURE searchintype (cctp : ctp) ;

        BEGIN
	WITH cctp^ DO
	  CASE form OF
	    records :
	      sortlevel (fstfld) ;
	    arrays :
	      IF aeltype <> NIL THEN
	        IF aeltype^.name = blank THEN searchintype (aeltype) ;
	    pointer :
	      IF eltype <> NIL THEN
	        IF eltype^.name = blank THEN searchintype (eltype) ;
	    reel, numeric, scalar, power, files, aliastype : ;
	  END                                   (* CASE *)
        END (* SEARCHINTYPE *) ;

(* ****************************************** SORTLEVEL < DISPLAYSYMBOLS ************************************* *)

      PROCEDURE sortlevel ;

        LABEL
	100, 200 ;

        VAR
	sctp : ctp ;
	index : integer ;
	previous, next : ctp ;

        BEGIN                                     (* SORTLEVEL *)
	WHILE cctp <> NIL DO
	  BEGIN
	    WITH cctp^ DO
	      BEGIN
	        IF (name <> blank) AND (references <> NIL) THEN
		BEGIN
		  IF name [1] = '$' THEN index := 26 ELSE index := ord (name [1]) - ord ('a') ;
		  IF index IN [0..26] THEN
		    WITH alfalist [index] DO
		      IF firstname = NIL THEN
		        BEGIN
			firstname := cctp ;
			alfathread := NIL ;
			lastname := cctp
		        END
		      ELSE
		        BEGIN
			previous := NIL ;
			next := firstname ;
100 :
			IF next = cctp THEN GOTO 200 ; (* TO AVOID LOOP IN SYMBOLS THREAD... *)
			IF next^.name <= name THEN
			  IF next^.alfathread = NIL THEN
			    BEGIN
			      next^.alfathread := cctp ;
			      cctp^.alfathread := NIL ;
			      lastname := cctp
			    END
			  ELSE
			    BEGIN
			      previous := next ;
			      next := next^.alfathread ;
			      GOTO 100
			    END
			ELSE
			  BEGIN
			    cctp^.alfathread := next ;
			    IF previous = NIL THEN
			      firstname := cctp
			    ELSE
			      previous^.alfathread := cctp
			  END
		        END
		END ;
	        CASE klass OF
		types :
		  searchintype (cctp) ;
		vars :
		  IF vtype <> NIL THEN
		    IF vtype^.name = blank THEN searchintype (vtype) ;
		field :
		  IF fldtype <> NIL THEN
		    IF fldtype^.name = blank THEN searchintype (fldtype) ;
		proc :
		  IF proctype <> cctp THEN
		    IF proctype <> NIL THEN
		      IF proctype^.name = blank THEN searchintype (proctype) ;
		schema, konst, tagfield, dummyclass :
	        END ;
	      END ;
	    sctp := cctp^.nxtel ;
	    cctp^.nxtel := currentnode^.blockbox ;
	    cctp := sctp ;
	  END ;
200 :
        END (* SORTLEVEL *) ;

(* ************************************** SORTALFA<DISPLAYSYMBOLS ************************************* *)

      PROCEDURE sortalfa ;

        BEGIN                                     (* SORTALFA *)
	IF currentnode^.blocktp = procblock THEN
	  BEGIN
	    sortlevel (currentnode^.first) ;
	    IF currentnode^.son <> NIL THEN
	      BEGIN
	        currentnode := currentnode^.son ;
	        sortalfa ;
	        currentnode := currentnode^.father ;
	      END ;
	  END ;
	IF currentnode^.brother <> NIL THEN
	  BEGIN
	    currentnode := currentnode^.brother ;
	    sortalfa ;
	  END ;
        END (* SORTALFA *) ;

      BEGIN                                       (* DISPLAYSYMBOLS *)
                                                  (* SORT ALL SYMBOLS *)
        checkunused := false ;
        currentnode := programnode ;
        FOR it := 0 TO 26 DO
	alfalist [it].firstname := NIL ;
        sortalfa ;
        it := 0 ;
        WHILE (alfalist [it].firstname = NIL) AND (it <> 26) DO
	it := it + 1 ;
        firstalfa := alfalist [it].firstname ;
        lastit := it ;
        it := it + 1 ;
        WHILE it <> 27 DO
	BEGIN
	  IF alfalist [it].firstname <> NIL THEN
	    BEGIN
	      alfalist [lastit].lastname^.alfathread := alfalist [it].firstname ;
	      lastit := it ;
	    END ;
	  it := it + 1 ;
	END ;
                                                  (* EDITION *)
        tittle := false ;
        tittlestring := 'NAMES DECLARED AND REFERENCED' ;
        writeln (mpcogout) ;
        cctp := firstalfa ;
        lastbox := NIL ;
        WHILE cctp <> NIL DO
	BEGIN
	  WITH cctp^ DO
	    IF klass = vars THEN IF visrefincode OR (vkind = arraybound) THEN printsymbol ELSE lastbox := cctp
	    ELSE IF klass = proc THEN IF pisrefincode THEN printsymbol ELSE lastbox := cctp
	      ELSE IF references^.refnbr <> 0 THEN printsymbol ELSE lastbox := cctp ;
	  cctp := cctp^.alfathread ;
	END ;
        IF NOT tittle THEN
	writeln (mpcogout, '        NO ', tittlestring) ;

        tittle := false ;
        tittlestring := 'NAMES DECLARED AND NEVER REFERENCED' ;
        writeln (mpcogout) ;
        cctp := firstalfa ;
        checkunused := true ;
        lastbox := NIL ;
        WHILE cctp <> NIL DO
	BEGIN
	  printsymbol ;
	  cctp := cctp^.alfathread ;
	END ;
        checkunused := false ;
        IF NOT tittle THEN
	writeln (mpcogout, '        NO ', tittlestring) ;

        tittle := false ;
        tittlestring := 'NAMES DECLARED BY DEFAULT' ;
        writeln (mpcogout) ;
        FOR it := 0 TO 26 DO
	alfalist [it].firstname := NIL ;
        sortlevel (display [0].fname) ;
        it := 0 ;
        WHILE (alfalist [it].firstname = NIL) AND (it <> 26) DO
	it := it + 1 ;
        firstalfa := alfalist [it].firstname ;
        lastit := it ;
        it := it + 1 ;
        WHILE it <> 27 DO
	BEGIN
	  IF alfalist [it].firstname <> NIL THEN
	    BEGIN
	      alfalist [lastit].lastname^.alfathread := alfalist [it].firstname ;
	      lastit := it ;
	    END ;
	  it := it + 1 ;
	END ;
        cctp := firstalfa ;
        WHILE cctp <> NIL DO
	BEGIN
	  IF (cctp^.references^.refnbr <> 0) THEN printsymbol ;
	  cctp := cctp^.alfathread ;
	END ;
        IF NOT tittle THEN
	writeln (mpcogout, '        NO ', tittlestring) ;
        writeln (mpcogout) ;
        IF firstlabbox^.next^.next = NIL THEN
	writeln (mpcogout, '        NO LABELS')
        ELSE
	BEGIN
	  writeln (mpcogout, '        LABELS') ;
	  writeln (mpcogout) ;
	  writeln (mpcogout, '      BLOCK NAME') ;
	  currlabbox := firstlabbox^.next ;
	  REPEAT
	    WITH currlabbox^ DO
	      BEGIN
	        write (mpcogout, number : 4) ;
	        IF references^.refnbr = 0 THEN write (mpcogout, '* ')
	        ELSE write (mpcogout, '  ') ;
	        IF procnode = programnode THEN write (mpcogout, '(main)                          ')
	        ELSE write (mpcogout, procnode^.blockbox^.name : 32) ;
	        write (mpcogout, ' DCL : ') ;
	        ll := 39 + 6 ;
	        IF dclfile <> 0 THEN
		BEGIN
		  n := longint (dclfile) ;
		  write (mpcogout, dclfile : n, '-') ;
		  ll := ll + n + 1 ;
		END ;
	        n := longint (dclline) ;
	        write (mpcogout, dclline : n, ' ; DEF: ') ;
	        ll := ll + n + 8 ;
	        IF deffile <> 0 THEN
		BEGIN
		  n := longint (deffile) ;
		  write (mpcogout, deffile : n, '-') ;
		  ll := ll + n + 1 ;
		END ;
	        n := longint (defline) ;
	        write (mpcogout, defline : n) ;
	        ll := ll + n ;
	        IF references^.refnbr <> 0 THEN BEGIN
		  write (mpcogout, ' ; REF: ') ;
		  ll := ll + 8 ;
		  refbox := references ;
		  p1 := NIL ;
		  WHILE refbox^.nextref <> NIL DO BEGIN
		      p2 := refbox^.nextref ;
		      refbox^.nextref := p1 ;
		      p1 := refbox ;
		      refbox := p2 ;
		    END ;
		  refbox^.nextref := p1 ;
		  REPEAT
		    WITH refbox^ DO
		      FOR i := 1 TO refnbr DO
		        WITH refs [i] DO
			BEGIN
			  IF ll >= llmax THEN
			    BEGIN
			      writeln (mpcogout) ;
			      write (mpcogout, '  ' : 41) ;
			      ll := 40 ;
			    END ;
			  IF filen <> 0 THEN BEGIN
			      n := longint (filen) ;
			      write (mpcogout, filen : n, '-') ;
			      ll := ll + n + 1 ;
			    END ;
			  IF linen < 0 THEN
			    BEGIN
			      n := longint (-linen) ;
			      write (mpcogout, -linen : n, '* ')
			    END ELSE
			    BEGIN
			      n := longint (linen) ;
			      write (mpcogout, linen : n, ' ') ;
			    END ;
			  ll := ll + n + 1 ;
			END ;
		    refbox := refbox^.nextref ;
		  UNTIL refbox = NIL ;
		END ;
	      END ;
	    writeln (mpcogout) ;
	    currlabbox := currlabbox^.next ;
	  UNTIL currlabbox^.next = NIL ;
	END ;

      END (* DISPLAYSYMBOLS *) ;

$OPTIONS page $

(* ***********************************************STATISTIQUES***************** *)

    PROCEDURE statistiques ;

(* C  CALLED AT END OF COMPILATION
   . PRINTS   ERROR'S MEANING
   . PRINTS   PAGENUMBER  WHERE  ERRORS   WERE  FOUND
   . PRINTS   ERRTOTAL
   . ASSIGNS    $COND                                                    C *)
      VAR
        i, j, pageocc : integer ;
      BEGIN
        IF mapswitch THEN
	IF errtotal # 0 THEN
	  BEGIN
	    nextpage ;
	    write (mpcogout, errtotal : 5, ' COMPILATION ERROR(S) DETECTED') ; nextline ;
	    nextline ;
	    pageocc := -1 ;                     (* FLAG  FIRST LINE  OF  PAGE NUMBERS *)
	    FOR i := 0 TO maxerpg DO            (* LOOP  ON  ENTRIES *)
	      FOR j := 0 TO maxset DO           (* LOOP  ON ELEMENT IN  AN ENTRY *)
	        IF j IN pageserrors [i] THEN
		BEGIN
		  IF pageocc = -1 THEN
		    BEGIN write (mpcogout, 'ERROR(S) DETECTED IN PAGE(S) :') ; pageocc := 1 ;
		    END ELSE
		    IF pageocc = 1 THEN
		      write (mpcogout, '                             :') ;
		  write (mpcogout, i * setrange + j : 5) ; pageocc := pageocc + 1 ;
		  IF pageocc = 19 THEN
		    BEGIN
		      nextline ; pageocc := 1 ;
		    END ;
		END (* FOR I,J,IF *) ;
	    IF pageocc > 1 THEN                 (*  LINE  NOT EMPTY *)
	      nextline ;
                                                  (* NOW   PRINTS  ERROR'S MEANING *)
	    nextline ;
	    FOR i := 0 TO maxerpg DO
	      FOR j := 0 TO maxset DO
	        IF j IN errorsfound [i] THEN
		BEGIN
		  prterrmeans (mpcogout, i * setrange + j) ;
		  nextline ;
		END ;
	  END (* ERRORS *) ELSE
	  BEGIN
	    nextpage ;
	    IF listyes THEN write (mpcogout, '        NO COMPILATION ERROR ') ; nextline ;
	  END ;
        IF listyes THEN
	nextline ;
      END (* STATISTIQUES *) ;

(* END OF UNIQUE MODULE ******************************************************* *) BEGIN
    END.

