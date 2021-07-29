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


PROGRAM contexttable ;

$OPTIONS switch trace := true ; switch security := true ; t + $


    $IMPORT
      'RACINE (pascal)' :
        alfaptr,
        anytrace,
        boxheader,
        charptr,
        intptr,
        lamptr,
        level,
        mpcogout,
        next,
        nilptr,
        pnumptr,
        realptr,
        symbolfile,
        symbolline,
        top ;
      'RACINE (pascal) ' :
        error,
        nextline,
        warning ;
      'DECLARE (pascal) ' :
        analyzing_schema,
        decltrace,
        tabform,
        tabkinds,
        tabklass,
        tabkonst,
        tabpdef ;
      'UNIQUE (pascal)' :
        heaperror ;
      'STATE (pascal)' :
        stattrace ;
    $


    $EXPORT
      add_schema_token,
      areconformeq,
      boundary,
      bytesneeded,
      checkminmax,
      compatbin,
      conformantdim,
      create_vars_box,
      create_types_box,
      create_proc_box,
      create_field_box,
      create_konst_box,
      create_schema_box,
      create_tagfield_box,
      create_dummyclass_box,
      existfileintype,
      findminmax,
      legalconfarrsubstitution,
      packedsize,
      packedcadre,
      printrec,
      warningminmax

    $
$INCLUDE 'CONSTTYPE' $

$OPTIONS page $

    VAR

(* REDEFINE IMPORTED VARIABLES FROM         "RACINE"        *)

      alfaptr : ctp ;
      anytrace : levtrace ;
      boxheader : PACKED ARRAY [1..120] OF char ;
      charptr : ctp ;
      intptr : ctp ;
      lamptr : ctp ;
      level : levrange ;
      mpcogout : text ;
      next : ctp ;
      nilptr : ctp ;
      pnumptr : ctp ;
      realptr : ctp ;
      symbolfile : integer ;
      symbolline : integer ;
      top : integer ;


(* REDEFINE IMPORTED VARIABLES FROM       "DECLARE"        *)


      analyzing_schema : schema_status ;
      decltrace : levtrace ;
      tabform : ARRAY [typform] OF alfa ;
      tabkinds : ARRAY [idkinds] OF alfa ;
      tabklass : ARRAY [idklass] OF alfa ;
      tabkonst : ARRAY [consttype] OF alfa ;
      tabpdef : ARRAY [idprocdef] OF alfa ;

(* REDEFINE IMPORTED VARIABLES FROM  "STATE"        *)


      stattrace : levtrace ;


(* REDEFINE IMPORTED PROCEDURES FROM        "UNIQUE"     *)

    PROCEDURE heaperror ; EXTERNAL ;


(* REDEFINE IMPORTED PROCEDURES FROM               "RACINE"       *)

    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    PROCEDURE warning (ferrnum : integer) ; EXTERNAL ;




(* ******************************************* ADD_FORMAT_TOKEN ****************************** *)

    PROCEDURE add_schema_token (kind : schema_token_kind) ;

      VAR
        localftp : ftp ;
      BEGIN
        new (localftp) ;
        IF localftp = NIL THEN heaperror ;
        localftp^.kind := kind ;
        WITH localftp^ DO
	CASE kind OF
	  symbol_token :
	    BEGIN
	      tno := 0 ; tcl := 0
	    END ;
	  name_token :
	    taval := blank ;
	  int_const_token :
	    t_int_value := 0 ;
	  char_const_token :
	    t_char_value := ' ' ;
	  real_const_token :
	    t_real_value := 0 ;
	END ;
        localftp^.next := NIL ;
        IF analyzing_schema.schema_ptr^.token_list = NIL THEN
	analyzing_schema.schema_ptr^.token_list := localftp
        ELSE analyzing_schema.current_token^.next := localftp ;
        analyzing_schema.current_token := localftp ;
      END ;

$OPTIONS page $
$OPTIONS page $
    PROCEDURE initcommonpart (fvbox : ctp ; fname : alfaid) ;

(* C Cette procedure initialise les champs communs a toutes les boites
   CONTEXTTABLE.
   C *)

      BEGIN                                       (* INITCOMMONPART *)

        WITH fvbox^ DO
	BEGIN
	  name := fname ;
	  alfathread := NIL ;
	  symbolplace := packednil ;
	  symbtablerefs := 0 ;


	  IF name = blank THEN
	    BEGIN
	      nxtel := NIL ; deffile := 0 ; defline := 0 ; references := NIL ;
	    END (* NAME = BLANK *) ELSE
	    BEGIN
	      nxtel := next ;                   (* Common default *)
	      deffile := symbolfile ; defline := symbolline ;
	      new (references) ; IF references = NIL THEN heaperror ;
	      WITH references^ DO
	        BEGIN
		refnbr := 0 ; nextref := NIL ;
	        END ;
	    END ;
	END (* with FVBOX *) ;
      END (* INITCOMMONPART *) ;


$OPTIONS page $

(* ********************************* CREATE_VARS_BOX ********* *)

    PROCEDURE create_vars_box (VAR fvbox : ctp ; fname : alfaid) ;

(* C Cette procedure est la seule autorisee a creer un enregistrement
   de CONTEXTTABLE correspondant a la classe VARS.
   En sortie ,elle renvoie le pointeur FVBOX .
   En cas de saturation du tas, on appelle HEAPERROR, qui arrete la
   compilation.
   C *)

      BEGIN                                       (* CREATE_VARS_BOX *)

        new (fvbox, vars) ;
        IF fvbox = NIL THEN heaperror ;
        WITH fvbox ^ DO
	BEGIN
	  initcommonpart (fvbox, fname) ;
	  klass := vars ;
	  vtype := NIL ;
	  vkind := actual ;
	  vfilelocation := notafile ;
	  vaddr := 0 ;
	  vdispl := 0 ; vdescaddr := 0 ;
	  vlevel := level ;
	  visused := false ;
	  visset := false ;
	  visreadonly := false ;
	  visrefincode := false ;
	  varparam := false ;
	  vptextitem := NIL ;
	END ;

      END (* CREATE_VARS_BOX *) ;

$OPTIONS page $
                                                  (* ********************************* CREATE_SCHEMA_BOX ********* *)

    PROCEDURE create_schema_box (VAR fvbox : ctp ; fname : alfaid) ;

(* C Cette procedure est la seule autorisee a creer un enregistrement
   de CONTEXTTABLE correspondant a la classe SCHEMA.
   En sortie ,elle renvoie le pointeur FVBOX .
   En cas de saturation du tas, on appelle HEAPERROR, qui arrete la
   compilation.
   C *)

      BEGIN                                       (* CREATE_SCHEMA_BOX *)

        new (fvbox, schema) ;
        IF fvbox = NIL THEN heaperror ;
        WITH fvbox ^ DO
	BEGIN
	  initcommonpart (fvbox, fname) ;
	  klass := schema ;
	  top_for_schema := top ;
	  next_for_schema := next ;
	  formal_parameter_list := NIL ;
	  parameter_count := 0 ;
	  token_list := NIL
	END ;

      END (* CREATE_SCHEMA_BOX *) ;

$OPTIONS page $


(* ********************************* CREATE_TYPES_BOX ********* *)

    PROCEDURE create_types_box (VAR fvbox : ctp ; fname : alfaid ;
      fform : typform ; fbool : boolean) ;

(* C Cette procedure est la seule autorisee a creer un enregistrement
   de CONTEXTTABLE correspondant a la classe TYPES.
   En sortie ,elle renvoie le pointeur FVBOX .
   En cas de saturation du tas, on appelle HEAPERROR, qui arrete la
   compilation.
   FFORM identifie le sous-type.
   FBOOL  n'est utilise que pour SCALAR, ARRAYS.
   C *)

      BEGIN                                       (* CREATE_TYPES_BOX *)

        new (fvbox, types) ;
        IF fvbox = NIL THEN heaperror ;
        WITH fvbox ^ DO
	BEGIN
	  initcommonpart (fvbox, fname) ;
	  klass := types ;
	  size := 0 ;
	  cadrage := 0 ;
	  pack := false ;
	  tlevel := level ;
	  form := fform ;
	  father_schema := NIL ;
	  actual_parameter_list := NIL ;

	  CASE form OF
	    reel : BEGIN
	      END ;
	    numeric : BEGIN
	        npksize := 0 ;
	        nmin := 0 ;
	        nmax := 0 ;
	      END ;
	    scalar : BEGIN
	        spksize := 0 ;
	        subrng := fbool ;
	        CASE subrng OF
		false : BEGIN
		    fconst := NIL ;
		    sptcstepw := NIL ;
		  END ;
		true : BEGIN
		    smin := 0 ;
		    smax := 0 ;
		    typset := NIL ;
		  END ;
	        END ;
	      END ;
	    pointer : BEGIN
	        ptpksize := 0 ;
	        domain := NIL ;
	        eltype := NIL ;
	      END ;
	    power : BEGIN
	        ppksize := 0 ;
	        setlength := 0 ;
	        elset := NIL ;
	      END ;
	    arrays : BEGIN
	        aeltype := NIL ;
	        inxtype := NIL ;
	        conformant := fbool ;
	        CASE conformant OF
		false : BEGIN
		    lo := 0 ;
		    hi := 0 ;
		    opt2 := 0 ;
		    subsize := 0 ;
		  END ;
		true : BEGIN
		    ptlow := NIL ;
		    father_schema := NIL ;
		    actual_parameter_list := NIL ;
		    desc_vector_references := -1 ;
		  END ;
	        END (* case CONFORMANT *) ;
	      END ;
	    records : BEGIN
	        recvar := NIL ;
	        fstfld := NIL ;
	      END ;
	    files : BEGIN
	        feltype := NIL ;
	      END ;
	    aliastype : BEGIN
	        realtype := NIL ;
	      END ;
	  END (* case FORM *) ;
	END ;

      END (* CREATE_TYPES_BOX *) ;

$OPTIONS page $

(* ********************************* CREATE_PROC_BOX ********* *)

    PROCEDURE create_proc_box (VAR fvbox : ctp ; fname : alfaid) ;

(* C Cette procedure est la seule autorisee a creer un enregistrement
   de CONTEXTTABLE correspondant a la classe PROC.
   En sortie ,elle renvoie le pointeur FVBOX .
   En cas de saturation du tas, on appelle HEAPERROR, qui arrete la
   compilation.
   C *)

      BEGIN                                       (* CREATE_PROC_BOX *)

        new (fvbox, proc) ;
        IF fvbox = NIL THEN heaperror ;
        WITH fvbox ^ DO
	BEGIN
	  initcommonpart (fvbox, fname) ;
	  klass := proc ;
	  proctype := NIL ;
	  formals := NIL ;
	  prockind := actual ;
	  proclevel := level ;
	  procaddr := 0 ;
	  segsize := 0 ;
	  nbparproc := 0 ;
	  locincode := 0 ;
	  procisassigned := false ;
	  predefproc := false ;
	  procinscope := true ;
	  phasdescriptor := false ;
	  ploc := notpredef ;
	  procextitem := NIL ;
	  procdef := standdef ;
	  ptypesymbolplace := packednil ;
	  pisrefincode := false ;
	  procisactive := false ;
	  pwantdescs := false ;
	  pdescsaddrplace := 0 ;
	  pextcalltrapinfoplace := 0 ;
	  pwantspl1descriptors := false ;
	END ;

      END (* CREATE_PROC_BOX *) ;

$OPTIONS page $

(* ********************************* CREATE_FIELD_BOX ********* *)

    PROCEDURE create_field_box (VAR fvbox : ctp ; fname : alfaid) ;

(* C Cette procedure est la seule autorisee a creer un enregistrement
   de CONTEXTTABLE correspondant a la classe FIELD.
   En sortie ,elle renvoie le pointeur FVBOX .
   En cas de saturation du tas, on appelle HEAPERROR, qui arrete la
   compilation.
   C *)

      BEGIN                                       (* CREATE_FIELD_BOX *)

        new (fvbox, field) ;
        IF fvbox = NIL THEN heaperror ;
        WITH fvbox ^ DO
	BEGIN
	  initcommonpart (fvbox, fname) ;
	  klass := field ;
	  fldtype := NIL ;
	  fldaddr := 0 ;
	  bytwidth := 0 ;
	END ;

      END (* CREATE_FIELD_BOX *) ;

$OPTIONS page $

(* ********************************* CREATE_KONST_BOX ********* *)

    PROCEDURE create_konst_box (VAR fvbox : ctp ; fname : alfaid ;
      ftypofconst : consttype) ;

(* C Cette procedure est la seule autorisee a creer un enregistrement
   de CONTEXTTABLE correspondant a la classe KONST.
   En sortie ,elle renvoie le pointeur FVBOX .
   En cas de saturation du tas, on appelle HEAPERROR, qui arrete la
   compilation.
   FTYPOFCONST identifie la sous-classe de constante.
   C *)

      BEGIN                                       (* CREATE_KONST_BOX *)

        new (fvbox, konst) ;
        IF fvbox = NIL THEN heaperror ;
        WITH fvbox ^ DO
	BEGIN
	  initcommonpart (fvbox, fname) ;
	  klass := konst ;
	  succ := NIL ;
	  contype := NIL ;
	  typofconst := ftypofconst ;

	  CASE typofconst OF
	    wordconst : BEGIN
	        values := 0 ;
	      END ;
	    dwordconst : BEGIN
	        valreel := 0 ;
	      END ;
	    alfaconst : BEGIN
	        alfadeb := NIL ;
	        alfalong := 0 ;
	        alfalevel := level ;
	        unddeb := 0 ;
	      END ;
	  END (* case TYPOFCONST *) ;
	END ;

      END (* CREATE_KONST_BOX *) ;

$OPTIONS page $

(* ********************************* CREATE_TAGFIELD_BOX ********* *)

    PROCEDURE create_tagfield_box (VAR fvbox : ctp ; fname : alfaid ; ftagval : boolean) ;

(* C Cette procedure est la seule autorisee a creer un enregistrement
   de CONTEXTTABLE correspondant a la classe TAGFIELD.
   En sortie ,elle renvoie le pointeur FVBOX .
   En cas de saturation du tas, on appelle HEAPERROR, qui arrete la
   compilation.
   FTAGVAL permet la discrimination de champs.
   C *)

      BEGIN                                       (* CREATE_TAGFIELD_BOX *)

        new (fvbox, tagfield) ;
        IF fvbox = NIL THEN heaperror ;
        WITH fvbox ^ DO
	BEGIN
	  initcommonpart (fvbox, fname) ;
	  klass := tagfield ;
	  casesize := 0 ;
	  variants := NIL ;
	  tagval := ftagval ;

	  CASE tagval OF
	    false : BEGIN
	        casetype := NIL ;
	        selectorfield := NIL ;
	      END ;
	    true : BEGIN
	        caseval := 0 ;
	        firstfield := NIL ;
	      END ;
	  END (* case TAGVAL *) ;

	END (* with *) ;

      END (* CREATE_TAGFIELD_BOX *) ;

$OPTIONS page $

(* ********************************* CREATE_DUMMYCLASS_BOX ********* *)

    PROCEDURE create_dummyclass_box (VAR fvbox : ctp ; fname : alfaid) ;

(* C Cette procedure est la seule autorisee a creer un enregistrement
   de CONTEXTTABLE correspondant a la classe DUMMYCLASS.
   En sortie ,elle renvoie le pointeur FVBOX .
   En cas de saturation du tas, on appelle HEAPERROR, qui arrete la
   compilation.
   C *)

      BEGIN                                       (* CREATE_DUMMYCLASS_BOX *)

        new (fvbox, dummyclass) ;
        IF fvbox = NIL THEN heaperror ;
        WITH fvbox ^ DO
	BEGIN
	  initcommonpart (fvbox, fname) ;
	  klass := dummyclass ;
	END ;

      END (* CREATE_DUMMYCLASS_BOX *) ;

$OPTIONS page $

(* *************************************PRINTREC******************************* *)

    PROCEDURE printrec (ptbox : ctp) ;

(* C .CALLED IN ORDER TO WRITE ON LISTING THE CONTENT OF THE BOX  POINTED  BY
   "PTBOX".
   .THE VALUE OF  DECLTRACE  GIVES  THE  LEVEL  OF INFORMATIONS  TO BE
   WRITTEN
   C *)


(* ***********************************************CRACHEPROC < PRINTREC******** *)

      PROCEDURE cracheproc ;

        BEGIN nextline ;
	WITH ptbox@ DO
	  IF decltrace = high THEN
	    BEGIN
	      write (mpcogout, '*  PROCTYPE, FORMALS  AT  @ ', ord (proctype), ord (formals),
	        ' PROCKIND  IS ', tabkinds [prockind], ' PROCLEVEL IS', proclevel : 4) ;
	      nextline ;
	      write (mpcogout, '* PROCADDR,SEGSIZE ARE ', procaddr : 5, segsize, ' PROCDEF IS ',
	        tabpdef [procdef], ' POCISASSIGNED IS ', procisassigned : 5,
	        ' PROCINSCOPE IS ', procinscope) ;
	      nextline ;
	      write (mpcogout, '* NBPARPROC,PREDEFPROC ARE : ', nbparproc : 5, predefproc : 5) ;
	      write (mpcogout, '  PROCEXTITEM is at^', ord (procextitem)) ;
	      write (mpcogout, ' PISREFINCODE is:', pisrefincode) ;
	      write (mpcogout, ' PHASDESCRIPTOR = ', phasdescriptor) ;
	      nextline ;
	    END ;
        END (* CRACHEPROC *) ;


(* *************************************CRACHEFIELD    < PRINTREC  *********** *)

      PROCEDURE crachefield ;
        BEGIN nextline ;
	WITH ptbox@ DO
	  IF decltrace = high THEN
	    BEGIN
	      write (mpcogout, '*  FLDTYPE IS AT @ ', ord (fldtype), ' FLDADDR,BYTWIDTH ARE',
	        fldaddr : 5, bytwidth : 5) ;
	      nextline ;
	    END ;
        END ;                                     (* CRACHEFIELD *)


(* *************************************CRACHEVARS    <  PRINTREC  *********** *)

      PROCEDURE crachevars ;
        BEGIN
	nextline ;
	WITH ptbox@ DO
	  IF decltrace = high THEN
	    BEGIN
	      write (mpcogout, '*  VTYPE IS AT @ ', ord (vtype), ' VKIND IS ', tabkinds [vkind],
	        ' VADDR,VLEVEL,VPTEXTITEM ARE : ', vaddr, vlevel : 4, ord (vptextitem)) ;
	      nextline ;
	      write (mpcogout, '* ord(VFILELOCATION) is:', ord (vfilelocation),
	        ' VISREFINCODE is :', visrefincode) ;
	      write (mpcogout, ' VDISPL and VDESCADDR are :', vdispl : 8, vdescaddr : 8) ;
	      nextline ;
	      write (mpcogout, '* VISUSED,VISSET,VISREADONLY ARE :', visused : 5, visset : 5,
	        visreadonly : 5, ' VARPARAM IS : ', varparam : 5) ;
	      nextline ;
	    END ;
        END ;                                     (* CRACHEVARS *)


(* *************************************CRACHEKONST  <   PRINTREC  *********** *)

      PROCEDURE crachekonst ;

        BEGIN
	WITH ptbox@ DO
	  IF decltrace = medium THEN
	    BEGIN
	      write (mpcogout, ' TYPOFCONST IS ', tabkonst [typofconst]) ; nextline ;
	    END ELSE
	    BEGIN
	      nextline ;
	      write (mpcogout,
	        '*  SUCC ,CONTYPE ARE AT@ ', ord (succ), ord (contype), ' TYPOFCONST IS',
	        tabkonst [typofconst] : 9) ;
	      nextline ;
	      IF typofconst = wordconst THEN
	        write (mpcogout, '*  VALUES IS: ', values) ELSE
	        IF typofconst = dwordconst THEN
		write (mpcogout, '*  VALREEL IS: ', valreel) ELSE
		write (mpcogout,
		  '*  ALFADEB IS AT @ ', ord (alfadeb), ' ALFALONG,ALFALEVEL,UNDDEB ',
		  alfalong : 4, alfalevel : 4, unddeb : 4) ;
	      nextline ;
	    END ;
        END ;                                     (* CRACHEKONST *)


(* *************************************CRACHETAGFIELD < PRINTREC  *********** *)
      PROCEDURE crachetagfield ;

        BEGIN
	WITH ptbox@ DO
	  IF decltrace = medium THEN
	    BEGIN
	      write (mpcogout, '*  TAGVAL  IS: ', tagval : 5) ; nextline ;
	    END ELSE
	    BEGIN
	      nextline ;
	      write (mpcogout,
	        '*  CASESIZE IS:', casesize : 5, ' VARIANTS IS AT@ ', ord (variants),
	        ' TAGVAL IS: ', tagval : 5) ; nextline ;
	      IF tagval THEN
	        write (mpcogout, '* CASEVAL IS:', caseval) ELSE
	        write (mpcogout, '* CASETYPE IS AT @', ord (casetype)) ;
	      nextline ;
	    END ;
        END ;                                     (* CRACHETAGFIELD *)


(* *************************************CRACHETYPES   <  PRINTREC  *********** *)
      PROCEDURE crachetypes ;

        BEGIN
	WITH ptbox@ DO
	  IF decltrace = medium THEN
	    BEGIN
	      write (mpcogout, ' FORM IS : ', tabform [form]) ;
	      IF form = scalar THEN
	        write (mpcogout, ' SUBRNG IS ', subrng : 5) ELSE
	        IF form = arrays THEN
		write (mpcogout, ' CONFORMANT IS ', conformant : 5) ;
	      nextline ;
	    END ELSE
	    BEGIN nextline ;
	      write (mpcogout,
	        '*  SIZE,CADRAGE ARE : ', size, cadrage : 4, ' PACK IS ', pack : 5,
	        ' FORM IS : ', tabform [form]) ;
	      nextline ;
	      CASE form OF
	        reel : ;
	        numeric : BEGIN
		  write (mpcogout, '* NPKSIZE,NMIN AND NMAX ARE: ', npksize, nmin, nmax) ;
		  nextline ;
		END ;
	        scalar : BEGIN
		  write (mpcogout, '* SPKSIZE IS: ', spksize, ' SUBRNG IS: ', subrng : 5) ;
		  nextline ;
		  IF subrng THEN
		    write (mpcogout,
		      '* SMIN,SMAX ARE :', smin, smax, ' TYPSET IS AT @', ord (typset))
		  ELSE
		    write (mpcogout,
		      '* FCONST,SPTCSTEPW ARE AT @', ord (fconst), ord (sptcstepw)) ;
		  nextline ;
		END ;
	        pointer : BEGIN
		  write (mpcogout,
		    '* PTPKSIZE IS:', ptpksize : 4,
		    ' DOMAIN,ELTYPE ARE AT @', ord (domain), ord (eltype)) ;
		  nextline ;
		END ;
	        power : BEGIN
		  write (mpcogout,
		    '* PPKSIZE IS: ', ppksize : 4, ' ELSET IS AT @', ord (elset)) ;
		  nextline ;
		END ;
	        arrays : BEGIN
		  write (mpcogout, '* AELTYPE,INXTYPE ARE AT @', ord (aeltype), ord (inxtype),
		    ' CONFORMANT IS :', conformant : 5) ;
		  nextline ;
		  IF conformant THEN
		    BEGIN

		    END ELSE
		    write (mpcogout, '* LO,HI,OPT2,SUBSIZE ARE :', lo, hi, opt2, subsize) ;
		  nextline ;
		END ;
	        records : BEGIN
		  write (mpcogout, '*RECVAR,FSTFLD ARE AT@', ord (recvar), ord (fstfld)) ;
		  nextline ;
		END ;
	        files : BEGIN
		  write (mpcogout,
		    '* FELTYPE IS AT @', ord (feltype)) ;
		  nextline ;
		END ;
	        aliastype : BEGIN
		  write (mpcogout, '* REALTYPE IS AT @', ord (realtype)) ; nextline ;
		END ;
	      END (* CASE FORM *) ;
	    END (* DECLTRACE=HIGH *) ;
        END (* CRACHETYPE *) ;


      BEGIN                                       (* PRINTREC  *)
        IF decltrace > low THEN
	BEGIN
	  nextline ; write (mpcogout, boxheader) ; nextline ;
	  IF ptbox = NIL THEN
	    BEGIN
	      write (mpcogout, '* BOX REQUESTED IS NIL . TRACE STOPS ') ; nextline ;
	    END ELSE
	    WITH ptbox@ DO
	      BEGIN
	        write (mpcogout, '* BOX FOLLOWING HERE IS AT @', ord (ptbox)) ; nextline ;
	        write (mpcogout, '*   NAME IS  : ', name, '   NXTEL IS   AT @', ord (nxtel),
		'   KLASS IS  : ', tabklass [klass]) ;
	        CASE klass OF
		types : crachetypes ;
		konst : crachekonst ;
		proc : cracheproc ;
		vars : crachevars ;
		field : crachefield ;
		tagfield : crachetagfield ;
		dummyclass : nextline ;
	        END (* CASE KLASS *) ;
	      END ;
	  write (mpcogout, boxheader) ; nextline ;
	  nextline ;
	END (* DECLTRACE > LOW *) ;
      END ;                                       (* PRINTREC *)


$OPTIONS page $

(* ********************************************    FCT. EXISTFILEINTYPE   *)

    FUNCTION existfileintype (ptontype : ctp) : boolean ;

(* C   returns TRUE if the type pointed by PTONTYPE (may be a complex type)
   is a file type or a type containing a file as element,
   returns FALSE otherwise
   C *)

      VAR
        locexist : boolean ;

      BEGIN                                       (* EXISTFILEINTYPE *)
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ Debut de EXISTFILEINTYPE @@@ avec ^',
	    ord (ptontype)) ; nextline ;
	END ;
$OPTIONS compile = true $

(* THIS IS A VERY POOR SIMULATION OF THE DEFINITIVE FUNCTION *)

        locexist := ptontype^.form = files ;
        existfileintype := locexist ;
$OPTIONS compile = trace $
        IF decltrace = high THEN
	BEGIN
	  write (mpcogout, ' @@@ Fin de EXISTFILEINTYPE @@@ avec valeur=',
	    locexist) ; nextline ;
	END ;
$OPTIONS compile = true $

      END (* EXISTFILEINTYPE *) ;

$OPTIONS page $

(* *********************************************************FCT. BOUNDARY****** *)

    FUNCTION boundary (objform : typform ; ispack : boolean ; pcksize : integer) : integer ;

(* C GIVES FOR AN OBJECT ITS BOUNDARY IN MEMORY (IN BYTES)                    C *)
(* E   ERRORS DETECTED
   353 COMPILER'S CONTROL (BOUNDARY) (OBJFORM=ALIASTYPE)
   354 COMPILER'S CONTROL (BOUNDARY) (BAD ARGUMENT)                       E *)
      VAR
        lbound : integer ;
      BEGIN
        lbound := bytesinword ;                   (* DEFAULT AND MOST COMMON VALUE *)
        IF ispack THEN
                                                  (* PACKED OBJECT *)
	CASE objform OF
	  reel : lbound := bytesindword ;
	  numeric, scalar : lbound := pcksize ;
	  pointer : (* LBOUND := BYTESINWORD *) ;
	  power : IF pcksize <= bytesindword THEN lbound := pcksize ELSE
	      lbound := bytesindword ;
	  arrays, records, files : error (354) ; (* COMPILER'S FAULT *)
	  aliastype : error (353) ;             (* COMPILER'S FAULT *)
	END                                     (* CASE,PACKED *)
        ELSE
                                                  (* UNPACKED OBJECT *)
	CASE objform OF
	  reel, pointer : lbound := bytesindword ;
	  numeric, scalar : (* LBOUND:=BYTESINWORD *) ;
	  power : lbound := bytesindword ;
	  arrays, records : (* LBOUND:=BYTESINWORD *) ;
	  files : lbound := bytesindword ;
	  aliastype : error (353) ;             (* COMPILER'S FAULT *)
	END (* CASE,UNPACKED *) ;
        boundary := lbound ;
      END (* BOUNDARY *) ;


$OPTIONS page $

(* *************************************FCT.BYTESNEEDED************************ *)

    FUNCTION bytesneeded (objform : typform ; highest : integer ;
      ispack : boolean) : integer ;

(* C FOR EACH TYPE  THIS  FUNCTION  RETURNS  THE SIZE NEEDED IN BYTES
   THIS VALUE DEPENDS ON THE BOOLEAN ISPACK.
   WHEN THIS BOOLEAN IS TRUE, HIGHEST GIVES  THE  MAXIMUM VALUE  OF THE OBJECT
   THEN IT IS POSSIBLE  TO  FIND  THE OPTIMAL SIZE                       C *)
(* E   ERRORS DETECTED
   351  COMPILER'S CONTROL (BYTESNEEDED) (OBSFORM=ALIASTYPE)
   352  COMPILER'S CONTROL (BYTESNEEDED) (BAD ARGUMENT )                  E *)
      VAR i : integer ;
      BEGIN i := bytesinword ;                    (* DEFAULT VALUE  *)
        IF NOT ispack THEN
                                                  (* NOT PACKED ENVIRONMENT *)
	CASE objform OF
	  reel, pointer : i := bytesindword ;
	  numeric, scalar : (* DEFAULT *) ;
	  power : IF highest <= bitsindword - 1 THEN i := bytesindword ELSE
	      i := bytesforset ;
	  arrays, records : i := 0 ;            (* PRELIMINARY SIZE *)
	  files : i := fsbpointersize ;
	  aliastype : error (351) ;             (* COMPILER'S CONTROL *)
	END                                     (* CASE , NOT ISPACK *)
        ELSE
                                                  (*  PACKED  ENVIRONMENT  *)
	CASE objform OF
	  reel : i := bytesindword ;
	  numeric : IF highest <= ntwotobyte THEN i := 1 (* ONE BYTE *) ELSE
	      IF highest <= ntwotohword THEN i := bytesinhword ELSE
	        i := bytesinword ;
	  scalar : IF highest <= stwotobyte THEN i := 1 (* ONE BYTE *) ELSE
	      i := bytesinhword ;
	  pointer : i := bytesinword ;
	  power : IF highest <= bitsinbyte - 1 THEN i := 1 (* ONE BYTE *) ELSE
	      IF highest <= bitsinhword - 1 THEN i := bytesinhword ELSE
	        IF highest <= bitsinword - 1 THEN i := bytesinword ELSE
		IF highest <= bitsindword - 1 THEN i := bytesindword ELSE
		  i := bytesforset ;
	  arrays, records, files : error (352) ; (* NO MEANINGS IN PACKED *)
	  aliastype : error (351) ;             (*  COMPILER'S  CONTROL *)
	END (* CASE , PACKED ENV *) ;
        bytesneeded := i ;
      END (* BYTESNEEDED *) ;


$OPTIONS page $

(* *********************************************************PACKEDSIZE********* *)

    FUNCTION packedsize (ftype : ctp) : integer ;

(* C   GIVES THE PACKED SIZE FOR A GIVEN TYPE FTYPE                           C *)
(* E   379  COMPILER'S CONTROL (PACKEDSIZE)                                   E *)
      VAR
        lsize : integer ;
      BEGIN
        lsize := 0 ;
$OPTIONS compile = security $
        IF ftype = NIL THEN error (379) ELSE
	IF ftype@.klass # types THEN error (379) ELSE
$OPTIONS compile = true $
	  WITH ftype@ DO
	    IF pack THEN lsize := size ELSE
	      CASE form OF
	        numeric : lsize := npksize ;
	        pointer : lsize := ptpksize ;
	        power : lsize := ppksize ;
	        scalar : lsize := spksize ;
	        aliastype, arrays, files, records, reel : lsize := size ;
	      END (* CASE *) ;
        packedsize := lsize ;
      END (* PACKEDSIZE *) ;

$OPTIONS page $

(* *********************************************************PACKEDCADRE********* *)

    FUNCTION packedcadre (ftype : ctp) : integer ;

(* C   GIVES THE PACKED CADRE FOR A GIVEN TYPE FTYPE                           C *)
(* E   379  COMPILER'S CONTROL (PACKEDCADRE)                                   E *)
      VAR
        lcadre : integer ;
      BEGIN
        lcadre := 0 ;
$OPTIONS compile = security $
        IF ftype = NIL THEN error (379) ELSE
	IF ftype@.klass # types THEN error (379) ELSE
$OPTIONS compile = true $
	  WITH ftype@ DO
	    IF pack THEN lcadre := cadrage ELSE
	      CASE form OF
	        numeric : BEGIN
		  lcadre := npksize ;
		END ;
	        pointer : lcadre := ptpksize ;
	        power : lcadre := ppksize ;
	        scalar : lcadre := spksize ;
	        aliastype, arrays, files, records, reel : lcadre := cadrage ;
	      END (* CASE *) ;
        packedcadre := lcadre ;
$OPTIONS compile = trace $
        IF decltrace = high THEN
	BEGIN
	  write (mpcogout, '@@@ Fin   de PACKED CADRE @@@ sur FTYPE^',
	    ord (ftype), ' valeur retournee =', lcadre : 6) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* PACKEDCADRE *) ;

$OPTIONS page $

(* ************************************ COMPATBIN ***************************** *)

    PROCEDURE compatbin (typleft, typright : ctp ; VAR fgeneric : ctp) ;

(* C  GIVEN TWO CTP (TYPES)  ,THIS PROCEDURE  RETURNS NIL IF  POINTER ARE NOT
   COMPATIBLE.
   IF THEY  ARE  COMPATIBLES  RETURNS  GENERIC TYPE
   C *)
      VAR
        locgen : ctp ;
      BEGIN                                       (* COMPATBIN *)
$OPTIONS compile = trace $
        IF anytrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT COMPATBIN @@@ WITH TYPLEFT,TYPRIGHT AT @', ord (typleft),
	    ord (typright)) ; nextline ;
	END ;
$OPTIONS compile = true $
        fgeneric := NIL ;                         (* DEFAULT  OVERRIDEN *)
                                                  (* ONLY IF COMPATIBLE  TYPES *)
        IF typleft # NIL THEN                     (* LEFT NIL *)
	IF typright # NIL THEN                  (* RIGHT NIL *)
	  IF typleft = typright THEN            (* SAME  TYPE *)
	    fgeneric := typleft ELSE
	    CASE typleft@.form OF
	      reel : IF typright@.form = numeric THEN fgeneric := realptr ;
	      numeric :
	        IF typright = realptr THEN fgeneric := realptr ELSE
		IF typright@.form = numeric THEN fgeneric := intptr ;
	      scalar :
	        IF typright@.form = scalar THEN
		IF NOT typleft@.subrng THEN
		  BEGIN
		    IF typright@.subrng THEN
		      BEGIN
		        IF typright@.typset = typleft THEN fgeneric := typleft ;
		      END
		  END (* LEFT NOT SUBRNG *) ELSE
		  BEGIN                       (* SUBRNG *)
		    IF typright@.subrng THEN
		      BEGIN
		        IF typright@.typset = typleft@.typset
		        THEN fgeneric := typleft@.typset ;
		      END ELSE
		      IF typleft@.typset = typright THEN fgeneric := typright ;
		  END (* LEFT SUBRNG *) ;
	      pointer : IF typright@.form = pointer THEN
		IF typleft = nilptr THEN
		  fgeneric := typright ELSE
		  IF typright = nilptr THEN
		    fgeneric := typleft ;
	      power : IF typright@.form = power THEN
		IF typleft = lamptr THEN fgeneric := typright ELSE
		  IF typright = lamptr THEN fgeneric := typleft ELSE
		    BEGIN
		      compatbin (typleft@.elset, typright@.elset, locgen) ;
		      IF locgen # NIL THEN
		        IF locgen@.form = numeric THEN
			fgeneric := pnumptr ELSE
			IF locgen@.subrng THEN
			  fgeneric := locgen@.typset@.sptcstepw ELSE
			  fgeneric := locgen@.sptcstepw ;
		    END ;
	      arrays : IF typright@.form = arrays THEN
		IF typleft@.pack THEN
		  IF typright@.pack THEN
		    IF typleft@.aeltype = charptr THEN
		      IF typright@.aeltype = charptr THEN
		        BEGIN
			IF typright = alfaptr THEN
			  BEGIN
			    IF typleft@.lo = 1 THEN
			      IF typleft@.inxtype@.form = numeric THEN
			        fgeneric := typleft
			  END ELSE
			  IF typleft = alfaptr THEN
			    BEGIN
			      IF typright@.lo = 1 THEN
			        IF typright@.inxtype@.form = numeric THEN
				fgeneric := typright
			    END ELSE
			    BEGIN
			      IF typright@.inxtype = typleft@.inxtype THEN
			        IF typleft@.inxtype@.form = numeric THEN
				IF typleft@.size = typright@.size THEN
				  fgeneric := typleft ;
			    END ;
		        END (* 2 PACKED ARRAYS OF CHARS *) ;
	      records, files : ;
	    END (* CASE TYPLEFT@.FORM *) ;
$OPTIONS compile = trace $
        IF anytrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN COMPATBIN @@@ WITH GENERIC AT @', ord (fgeneric)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* COMPATBIN *) ;

$OPTIONS page $

(* *************************************WARNINGMINMAX**************************** *)

    PROCEDURE warningminmax (fvalue : integer ; fctp : ctp ; ferrnum : integer) ;

(* C CALLED EACH  TIME  THE COMPILER IS ABLE TO FIND  IF  'FVALUE'  IS A CONSTANT
   COMPATIBLE   WITH THE DECLARED   BOUNDS   OF 'FCTP'                   C *)
(* E Errors detected
   COMPILER'S CONTROL
   384 :   FCTP IS NIL
   385 :   TYPES  NOT OF A GOOD FORM
   386 :   FCONST IS NIL                                                E *)
      VAR
        lerr : boolean ;
      BEGIN
$OPTIONS compile = security $
        IF fctp = NIL THEN error (384) ELSE
	IF fctp@.klass # types THEN error (385) ELSE
	  IF NOT (fctp@.form IN [numeric, scalar]) THEN error (385) ELSE
$OPTIONS compile = true $
	    WITH fctp@ DO
	      BEGIN
                                                  (* NUMERIC                   *)
	        IF form = numeric THEN lerr := (fvalue > nmax) OR (fvalue < nmin) ELSE
                                                  (* SCALAR                    *)
		IF subrng THEN lerr := (fvalue > smax) OR (fvalue < smin) ELSE
		  BEGIN
$OPTIONS compile = security $
		    IF fconst = NIL THEN
		      BEGIN
		        error (386) ; lerr := false ;
		      END ELSE
$OPTIONS compile = true $
		      lerr := (fvalue > fconst@.values) OR (fvalue < 0) ;
		  END ;
	        IF lerr THEN
		warning (ferrnum) ;
	      END (* WITH *) ;
      END (* WARNINGMINMAX *) ;

$OPTIONS page $

(* *************************************CHECKMINMAX**************************** *)

    PROCEDURE checkminmax (fvalue : integer ; fctp : ctp ; ferrnum : integer) ;

(* C CALLED EACH  TIME  THE COMPILER IS ABLE TO FIND  IF  'FVALUE'  IS A CONSTANT
   COMPATIBLE   WITH THE DECLARED   BOUNDS   OF 'FCTP'                   C *)
(* E ERRORS DETECTED
   VIA FERRNUM
   301  :  CASE  VARIANT   OUT OF  BOUNDS
   302  :  INDEX  OUT  OF BOUNDS
   303  :  VALUE  ASSIGNED   OUT OF BOUNDS
   304  :  CASE  LABEL   OUT OF  BOUNDS
   305  :  VALUE  IN A SET OUT OF BOUNDS
   COMPILER'S CONTROL
   384 :   FCTP IS NIL
   385 :   TYPES  NOT OF A GOOD FORM
   386 :   FCONST IS NIL                                                E *)
      VAR
        lerr : boolean ;
      BEGIN
$OPTIONS compile = trace $
        IF decltrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT CHEKMINMAX @@@', ' FVALUE,FCTP,FERRNUM :', fvalue,
	    ord (fctp), ferrnum) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
$OPTIONS compile = security $
        IF fctp = NIL THEN error (384) ELSE
	IF fctp@.klass # types THEN error (385) ELSE
	  IF NOT (fctp@.form IN [numeric, scalar]) THEN error (385) ELSE
$OPTIONS compile = true $
	    WITH fctp@ DO
	      BEGIN
                                                  (* NUMERIC                   *)
	        IF form = numeric THEN lerr := (fvalue > nmax) OR (fvalue < nmin) ELSE
                                                  (* SCALAR                    *)
		IF subrng THEN lerr := (fvalue > smax) OR (fvalue < smin) ELSE
		  BEGIN
$OPTIONS compile = security $
		    IF fconst = NIL THEN
		      BEGIN
		        error (386) ; lerr := false ;
		      END ELSE
$OPTIONS compile = true $
		      lerr := (fvalue > fconst@.values) OR (fvalue < 0) ;
		  END ;
	        IF lerr THEN
		error (ferrnum) ;
	      END (* WITH *) ;
$OPTIONS compile = trace $
        IF decltrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN CHECKMINMAX') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* CHECKMINMAX *) ;

$OPTIONS page $

(* ************************************ FINDMINMAX **************************** *)

    PROCEDURE findminmax (fctp : ctp ; VAR fmin, fmax : integer) ;

(* C  GIVEN A POINTER (FCTP) NOT NIL  ON A SCALAR OR NUMERIC TYPE,THIS PROC.
   RETURNS  THE BOUNDS ALLOWED  IN "FMIN" AND "FMAX
   C *)
(* E ERRORS DETECTED
   423  FCTP NIL
   424  KLASS # TYPES
   437  FORM # [NUMERIC,SCALAR]
   E *)
      BEGIN                                       (* FINDMINMAX *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT FINDMINMAX @@@ FOR CTP AT @', ord (fctp)) ; nextline ;
	END ;
$OPTIONS compile = true $
        fmin := 0 ; fmax := 0 ;                   (* IF ERROR(S) *)
$OPTIONS compile = security $
        IF fctp = NIL THEN error (423) ELSE
	IF fctp@.klass # types THEN error (424) ELSE
	  IF NOT (fctp@.form IN [numeric, scalar]) THEN error (437) ELSE
$OPTIONS compile = true $
	    WITH fctp@ DO
	      IF form = numeric THEN
	        BEGIN
		fmin := nmin ; fmax := nmax ;
	        END ELSE
                                                  (* SCALAR *)
	        IF subrng THEN
		BEGIN
		  fmin := smin ; fmax := smax ;
		END ELSE
		BEGIN
		  fmin := 0 ; fmax := fconst@.values ;
		END ;
$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN  FINDMINMAX @@@ WITH FMIN,FMAX', fmin, fmax) ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* FINDMINMAX *) ;

$OPTIONS page $

(* ******************************  ARECONFORMEQ   ******************* *)

    FUNCTION areconformeq (fp1, fp2 : ctp) : boolean ;

(* C
   Tool of PASSPARAMS
   if two types denoted by a pointer on their descriptive box are not
   identical, perhaps are they congruent conformant arrays types
   C *)

      VAR
        locbool : boolean ;

      BEGIN                                       (* ARECONFORMEQ *)
$OPTIONS compile = trace $
        IF stattrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ Debut de ARECONFORMEQ @@@ avec ',
	    ' FP1 en ^', ord (fp1), ' FP2 en ^', ord (fp2)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        locbool := false ;
        IF fp1^.form = arrays THEN
	IF fp1^.conformant THEN
	  IF fp2^.form = arrays THEN
	    IF fp2^.conformant THEN
	      IF fp1^.inxtype = fp2^.inxtype THEN (* ISO 7185 6.6.3.6 (3) *)
	        IF fp1^.pack = fp2^.pack THEN
		IF fp1^.aeltype = fp2^.aeltype THEN
		  locbool := true ELSE
		  IF (fp1^.aeltype <> NIL) AND (fp2^.aeltype <> NIL) THEN
		    locbool := areconformeq (fp1^.aeltype, fp2^.aeltype) ;

        areconformeq := locbool ;

$OPTIONS compile = trace $
        IF stattrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ Fin   de ARECONFORMEQ @@@ avec valeur=',
	    locbool) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* ARECONFORMEQ *) ;

$OPTIONS page $

(* ********************************   LEGALCONFARRSUBSTITUTION ************* *)

    FUNCTION legalconfarrsubstitution (ffound, fdecl : ctp) : boolean ;
      VAR
        locbool : boolean ;
        lmin, lmax : integer ;
        generic : ctp ;

      BEGIN                                       (* LEGALCONFARRSUBSTITUTION *)
        locbool := false ;

        IF (ffound <> NIL) AND (fdecl <> NIL) THEN
	IF (ffound^.klass = types) AND (fdecl^.klass = types) THEN
	  IF (ffound^.form = arrays) AND (fdecl^.form = arrays) THEN
	    IF ffound^.pack = fdecl^.pack THEN
	      BEGIN
	        compatbin (ffound^.inxtype, fdecl^.inxtype, generic) ;
	        IF generic <> NIL THEN
		BEGIN
		  findminmax (fdecl^.inxtype, lmin, lmax) ;
		  IF ffound^.conformant THEN
		    BEGIN
		      locbool := areconformeq (ffound, fdecl) ;
		    END (* FOUND CONFORMANT *) ELSE
		    BEGIN
		      IF ffound^.lo >= lmin THEN
		        IF ffound^.hi <= lmax THEN
			IF ffound^.aeltype = fdecl^.aeltype THEN
			  locbool := true ELSE
			  BEGIN
			    IF ffound^.aeltype^.form = arrays THEN
			      IF fdecl^.aeltype^.form = arrays THEN
			        locbool := legalconfarrsubstitution (ffound^.aeltype, fdecl^.aeltype) ;
			  END ;

		    END (* FOUND NOT CONFORMANT *) ;
		END (* GENERIC <> nil *) ;
	      END (* Can be equivalent *) ;
        legalconfarrsubstitution := locbool ;

$OPTIONS compile = trace $
        IF stattrace = high THEN
	BEGIN
	  write (mpcogout, '@@@ Fin de LEGALCONFARRSUBSTITUTION avec valeur retournee=', locbool) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* LEGALCONFARRSUBSTITUTION *) ;

$OPTIONS page $

(* **************************** CONFORMANTDIM     ************************ *)

    FUNCTION conformantdim (ffound : ctp) : boolean ;

      VAR
        locbool : boolean ;

      BEGIN                                       (* CONFORMANTDIM *)
        locbool := false ;

        IF ffound <> NIL THEN
	IF ffound^.klass = types THEN
	  IF ffound^.father_schema <> NIL THEN
	    locbool := ffound^.actual_parameter_list = NIL
	  ELSE IF ffound^.form = arrays THEN
	      IF ffound^.conformant THEN
	        locbool := true ;
        conformantdim := locbool ;

$OPTIONS compile = trace $
        IF stattrace = high THEN
	BEGIN
	  write (mpcogout, '@@@ Fin de CONFORMANTDIM avec valeur retournee=', locbool) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
      END (* CONFORMANTDIM *) ;


    BEGIN
    END.
