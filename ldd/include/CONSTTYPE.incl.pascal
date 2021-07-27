(* BEGIN INCLUDE FILE CONSTTYPE.incl.pascal *)
  
(* HISTORY COMMENTS:
  1) change(86-09-11,JMAthane), approve(86-09-11,MCR7521),
     audit(86-09-15,JPFauche), install(86-11-12,MR12.0-1212):
     Release 8.03 for MR12
                                                   END HISTORY COMMENTS *)

CONST                                           (*    VERSION 7.05 *)
                                                  (*  ARRAY'S   BOUNDS *)

  minno = 0 ;                                     (* MIN VALUE FOR NO  ;   SEE  TYPE NORANGE *)
  maxno = 63 ; (* MAX VALUE FORNO  ;    "     "    " *) { Modified for SimOne }
  ptlimit = 20 ;                                  (* MAX FOR   PTLIST     0 ..PTLIMIT *)
  displimit = 30 ;                                (*  "   "    DISPLAY       ..DISPLIMIT *)
  maxlabs = 50 ;                                  (*  "   "    LABTAB        ..MAXLABS *)
  fillimit = 50 ;                                 (*  "   "    FILPTS        .. FILLIMIT *)
  maxlevel = 20 ;                                 (*  SEE   LEVRANGE TYPE *)
  maxerrline = 16 ;                               (*  MAX  FOR   ERRINX *)
  maxerpg = 2 ;                                   (*  MAX  FOR  ERRORSFOUND, PAGESERRORS   0.. *)
  undmax = 1000 ;                                 (*  "    "   UNDLAB         1.. *)
  longalfbox = 16 ;                               (*  SEE  ALFAVALUE  TYPE *)
  lgparm = 100 ;                                  (*    PARMLIST  IN  $PARM *)
  lgparm1 = lgparm - 1 ;                          (*       LGPARM - 1 *)
  maxpredef = 99 ;                                (*   INITNAME         0 .. MAXPREDEF *)
  maxident = 32 ;                                 (*  LENGTH  MAX  FOR AN ID. *)
                                                  (* DONT MODIFY WITHOUT MODIFY maxident *)
                                                  (* IN optimized_procedures.alm *)
  maxnbofkeywords = 49 ;                          { Modified for SimOne }

  maxkeylength = 12 ; (* LENGTH MAX FOR A KEY-WORD *) { Modified for SimOne }
  maxexternname = 168 ;                           (* length max for an external name *)
  maxval = 256 ;                                  (*  LENGTH  MAX FOR  A STRING *)
  maxstring = 256 ;                               (*  MAX LENGTH FOR A STRING (STAND) <= MAXVAL *)
  alfaleng = 8 ;                                  (* SIZE  OF  ALFA TYPE *)
  maxfich = 50000 ;                               (*  FOR FICHINTER *)
  longboxlist = 20 ;                              (* MAX. NB. OF REG. BOXES *)
  maxref = 25 ;
  maxfield = 25 ;


  maxnewsize = 261094 ;                           (* MAX WORD SIZE FOR NEW *)

(*  POWERS    OF  2 *)
  twoto4 = 2 * 2 * 2 * 2 ;
  twoto6 = twoto4 * 2 * 2 ;
  twoto8 = twoto6 * 2 * 2 ;
  twoto8m1 = twoto8 - 1 ;
  twoto9 = twoto8 * 2 ;
  twoto10 = twoto9 * 2 ;
  twoto12 = twoto10 * 2 * 2 ;
  twoto14 = twoto6 * twoto8 ;
  twoto15 = twoto14 * 2 ;
  twoto16 = twoto15 * 2 ;
  twoto17 = twoto16 * 2 ;
  twoto17m1 = twoto17 - 1 ;
  twoto18 = twoto17 * 2 ;
  twoto18m1 = twoto18 - 1 ;
  twoto27 = twoto18 * twoto9 ;

(* COMPUTER'S  DESCRIPTION *)

  bitsinbyte = 9 ;
  bytesinword = 4 ;
  bytesinhword = bytesinword DIV 2 ;
  bytesindword = bytesinword * 2 ;

  wordsinpage = twoto10 ;
  pagesinsegment = twoto8 - 1 ;
  wordsinsegment = wordsinpage * pagesinsegment ;
  maxglobsize = twoto18 * bytesinword ;           (* MAX SIZE FOR GLOBALS IN BYTES *)
  maxstacksize = twoto15 * bytesinword ;          (* MAX SIZE FOR A STACK FRAME IN BYTES *)
  maxwseg = wordsinsegment ;                      (* MAX NBR OF WORDS IN SEGMENT = 255 * 1024 *)

  nilleft = '1FFFC0023'x ;                        (* VALUE FOR LEFT WORD OF THE "NIL" ITS *)
  nilright = '40000'x ;                           (* VALUE FOR RIGHT WORD OF THE "NIL" ITS *)
  packednil = '007777000001'o ;                   (* MULTICS PACKED NIL PTR *)
  bitsinword = bitsinbyte * bytesinword ;
  bitsinhword = bitsinbyte * bytesinhword ;
  bitsindword = 2 * bitsinword ;
  byteinbyte = 1 ;
  byteshift = 512 ;                               (* USED TO SHIFT AN INTEGER BY MULTIPLICATION *)
                                                  (* AND SUBSEQUENT VALUES *)
  maxint = 34359738367 ;                          (*  MAX. INTEGER *)
                                                  (* maxreal = 1.701411834604692317e38 ; *)
                                                  (* minreal = 1.469367938527859386e-39 ; *)
  racmaxint = 131072 ;                            (* USED TO AVOID OVERFLOW *)
                                                  (* IN INTEGER MULTIPLICATION *)
  ntwotobyte = twoto8 - 1 ;                       (* MAX. NUM. VALUE IN A BYTE *)
  ntwotohword = twoto17 - 1 ;                     (* MAX. NUM. VALUE IN A HALF. WORD *)
  stwotobyte = twoto9 - 1 ;                       (* MAX. SCAL. VALUE IN A BYTE *)
  setrange = 8 * bitsinword ;                     (*  SIZE OF STANDARD SETS : 8 WORDS MAX *)
  maxset = setrange - 1 ;                         (*  SETRANGE -1 *)
  maxerrnum = 3 * setrange - 1 ;                  (*   3*SETRANGE  -1 *)
  maxpage = maxerrnum ;
  maxchar = 127 ;                                 (*   < MAXSET *)

(*     IN  LISTING *)
  maxpageline = 59 ;                              (*  MAX NUMBER OF LINES ON A LISTING'S PAGE *)
  maxlinepascal = 400 ;
  maxsliceline = 136 ;
  lgprint = maxsliceline - 10 ;                   (*    SLICE  OF  PRINTED LINES *)

(*  COMPILATION'S  RETURN  CODE *)

  errorcond = 8 ;
  noerrorcond = 0 ;

  wordsforset = 8 ;
  bytesforset = wordsforset * bytesinword ;       (* MAX. NUM. OF BYTES IN A SET *)
  bitsforset = wordsforset * bitsinword ;         (* MAX. NUM. OF BITS  IN A SET *)
  bornesupset = wordsforset - 1 ;
  psrinbytes = 32 ;                               (* SIZE OF PSR REG. IN BYTES *)
                                                  (* AND   EXTERNAL  CONSTRAINTS *)
  maxdig = bitsinword DIV 3 ;                     (* MAX. NUMBER OF DIGITS IN OCTAL *)
  maxexpon = 1000 ;                               (* TO AVOID OVERFLOW WITH EXPONENTS *)
  maxhexdi = bitsinword DIV 4 ;                   (* MAX. NUMBER OF HEXA CHARS *)
  max10 = 3435973835 ;                            (* TO AVOID OVERFLOW IN INTEGER CST *)
  maxexp = 39 ;                                   (* MAX. SCALING FACTOR ALLOWED *)
  minexp = -38 ;                                  (* MIN. SCALING FACTOR ALLOWED *)
  maxdigitsinteger = 12 ;
  maxdigitsreal = 20 ;
  maxintegerstring = '+3435973836700000000' ;
  maxrealstring = '+1701411834604692317' ;
  minrealstring = '+1469367938527859386' ;

(*  GENERATION   CONSTRAINTS  AND CONSTANTS *)
  stackboundary = 16 * bytesinword ;              (*  BOUNDARY  FOR  A STACK  FRAME *)
  maxrel = 132 ;                                  (*   LENGTH OF  PHYSICAL LINE *)
  pascdebstacklocal = 384 ;                       (* BYTES DISP. OF FIRST FREE ST. IN PASCAL FRAME *)
  simdebstacklocal = 448 ; (* BYTES DISP. OF FIRST FREE ST. IN  SIMONE FRAME *) { Modified for SimOne }
  procparmsize = 24 ;                             (* SIZE FOR FORMAL PROCEDURE PARAMETERS *)
  mathworksizew = 32 ;                            (* SIZE OF WORK AREA FOR MATH OPS *)
  modulinitsize = 48 ;                            { Inserted for SimOne }
  monitorinitsize = 88 ;                          { Inserted for SimOne }

(* USEFUL   TOOLS *)
  blank = '        ' ;
  longblank = '                                ' ;

(*   ALM   GENERATION CONSTANTS *)
  bit29 = 64 ;                                    (* USE OF POINTER REGISTER BIT (BIT 29) *)
  inhibit = 128 ;                                 (* INTERRUPT INHIBITION BIT (BIT28) *)
  o41 = 33 ;                                      (*  ITS *)
  o43 = 35 ;                                      (*  ITP *)


(* ENTRY POINTS OF OPERATORS *)

  log10switch = 6 ;
                                                  (* Used in UNIQUE for predefinition of log10 *)
                                                  (* and in EXPR for scientific subroutines    *)

  mainentryplace = 79 ;
  extentryplace = 88 ;
  checkbeforeeolnplace = 66 ;
  checkbeforeeofplace = 67 ;
  checkbeforetextreferenceplace = 68 ;
  gotoexitextplace = 3 ;
  intentryplace = 89 ;
  scientplace = 80 ;
  resetplace = 11 ;
  rewriteplace = 12 ;
  closeplace = 13 ;
  readtextplace = 14 ;
  readseqplace = 15 ;
  readlnplace = 16 ;
  writetextplace = 17 ;
  writeseqplace = 18 ;
  writelnplace = 19 ;
  pageplace = 20 ;
  puttextplace = 21 ;
  putseqplace = 22 ;
  gettextplace = 23 ;
  getseqplace = 24 ;
  gotoexitplace = 25 ;
  newplace = 26 ;
  disposeplace = 27 ;
  resetheapplace = 28 ;
  truncplace = 63 ;
  roundplace = 64 ;
  rafltplace = 31 ;
  rqfltplace = 32 ;
  putdirplace = 33 ;
  getdirplace = 34 ;
  fupdtplace = 35 ;
  fcloseplace = 37 ;
  connectplace = 36 ;
  exceptcodeplace = 38 ;
  intreturnplace = 39 ;
  extreturnplace = 40 ;
  returnzeroplace = 87 ;
  intcallplace = 42 ;
  extcallplace = 43 ;
  initfsballocplace = 72 ;
  dateopplace = 47 ;
  timeopplace = 48 ;
  clockopplace = 49 ;
  longprofileplace = 50 ;
  flushplace = 65 ;
  fappendplace = 82 ;
  freopenplace = 81 ;
  argcshortplace = 69 ;
  argcplace = 70 ;
  argcextplace = 71 ;
  argvshortplace = 73 ;
  argvplace = 74 ;
  argvextplace = 75 ;
  stopshortplace = 76 ;
  stopplace = 77 ;
  stopextplace = 78 ;
  sreadcharplace = 90 ;
  sreadintegerplace = 91 ;
  sreadrealplace = 92 ;
  swritecharplace = 93 ;
  swritestringplace = 94 ;
  swritesubstringplace = 95 ;
  swritebooleanplace = 96 ;
  swriteintegerplace = 97 ;
  swriterealeplace = 98 ;
  swriterealdplace = 99 ;
  swriteenumplace = 100 ;
  parmproccheckplace = 101 ;
  functionvaluesetplace = 102 ;
  functionvaluecheckplace = 103 ;
  extend_stack_op_place = 104 ;
  reset_stack_end_op_place = 105 ;

(* CONST USED WITH OPERATORS *)
  transoptvptr = 40 ;                             (* DISP / PR7 OF TRANSFER VECTOR *)
  lotptrdep = 22 ;                                (* DISP / PR7  LOT-PTR *)
  pascoperatorsdep = 8 ;                          (* PASCAL OPERATORS *)
                                                  (* EXECUTION BITS FOR STACK FRAME *)
  mainbit = 131072 ;                              (* bit 18 *)
  fastbit = 65536 ;                               (* bit 19 *)
  checkbit = 32768 ;                              (* bit 20 *)
  interactivebit = 16384 ;                        (* bit 21 *)
  iowarningsbit = 8192 ;                          (* bit 22 *)
  solstandardbit = 32 ;                           (* bit 30 *)
  french_bit = 4 ;                                (* bit 31 *)

(* PASCAL ERRORS CODES *)

  inxerrcode = 1 ;                                (* INDEX *)
  chrerrcode = 2 ;                                (* FCT CHR *)
  prderrcode = 3 ;                                (* FCT PRED *)
  sucerrcode = 4 ;                                (* FCT SUCC *)
  forerricode = 5 ;                               (* FOR INF *)
  forerrscode = 6 ;                               (* FOR SUP *)
  asserrcode = 7 ;                                (* ASSIGN *)
  diverrcode = 8 ;                                (* DIV BY 0 *)
  parerrcode = 9 ;                                (* PARAM. *)
  caserrcode = 10 ;                               (* CASE *)
  pckerrcode = 11 ;                               (* PACK/UNPK *)
  seterrcode = 12 ;                               (* SET EXP *)
  mlterrcode = 13 ;                               (* INTEGER MULT OVERFLOW *)
  eofeolnerrcode = 14 ;                           (* Is eof or eoln meaningsfull *)
  randinterrcode = 15 ;                           (* BAD BOUNDS FOR RANDINT *)
  stringlength_range_error = 16 ;
  stringlength_assignment_error = 17 ;
  substring_offset_error = 18 ;
  substring_negative_length_error = 19 ;
  substring_too_long_error = 20 ;
  delete_offset_error = 21 ;
  delete_negative_length_error = 22 ;
  delete_too_long_error = 23 ;
  insert_overflow_error = 24 ;
  bad_string_index = 25 ;
  bad_date_time_parameter = 26 ;

(* ** FOR CONFORMANT ARRAY *)

  confdimw = 4 ;                                  (*  FOUR ITS IN PARAMETERS LIST *)
  dopevectorsize = 12 ;
                                                  (* LO HI SIZE SUBSIZE *)
  confdimsize = confdimw * bytesinword ;          (*  SAME IN BYTES *)

(* STACK FRAME  DISPL. *)

  argptw = 26 ;                                   (* SAVE ARG POINTER ENTRY *)
  next_sp_place = 18 ;                            (* MULTIC STACK_FRAME NEXT_SP *)
  pr4depw = 36 ;                                  (* SAVE PR4 HERE *)
  psrdepw = 56 ;                                  (* PSEUDO REGISTER FOR SET *)
  psrdepb = 224 ;
  dlkdepw = 32 ;                                  (* DYNAMIC LINK *)
  fctdeplw = 34 ;                                 (* RETURNED VALUE FOR A FUNCTION *)
  fctdepl = 136 ;
  evareaw = 38 ;                                  (* WORKING EVEN STORAGE *)
  fsbadrw = 68 ;                                  (* FSB STORED HERE FOR I/0 OPERATORS *)
  valplacew = 70 ;                                (* VALUE TO BE WRITTEN OR POINTER ON IT *)
  longplacew = 73 ;                               (* REQUESTED LENGTH *)
  scaleplacew = 74 ;                              (* FOR REAL SCALING FACTOR *)
  longstplacew = 74 ;                             (* REAL LENGTH FOR STRINGS *)

(* INIT ZONE FOR LINKAGE SECTION *)
(* 8 WORDS HEADER *)

  firstglobal = 8 ;                               (* WORD OFFSET OF FIRST GLOBAL (EVEN) *)

(* FSB DISPLACEMENTS *)

  lgfilename = 32 ;
  iotextbuffersize = 400 ;
  fdescsize = 152 ;
  fsbpointersize = 8 ;
  fstatusw = 7 ;
  fposw = 21 ;
  fsizew = 22 ;
  fllengthw = 23 ;
  fstatusb = 28 ;
  fposb = 84 ;
  fsizeb = 88 ;
  fllengthb = 92 ;
  eofw = 4 ;                                      (* WORD BOOLEAN EOF *)
  eofb = 16 ;
  eolnw = 27 ;                                    (* WORD BOOLEAN EOLN *)
  eolnb = 108 ;

(* PROFILE *)

  pclength = 2 ;                                  (* PROFILE COUNTER LENGTH IN WORDS *)
  lpclength = 4 ;                                 (* LONG PROFILE COUNTER LENGTH IN WORDS *)
  phl = 0 ;                                       (* PROFILE HEADER LENGTH *)
  lphl = 13 ;                                     (* LONG PROFILE HEADER LENGTH IN WORDS *)

(* RELOCATION CODES *)

  link18 = '900000000'x ;                         (* "10010"b *)
  link15 = 'A00000000'x ;                         (* "10100"b *)
  int18 = 'C00000000'x ;                          (* "11000"b *)
  self_rel = '880000000'x ;                       (* "10001"b *)
  int15 = 'C80000000'x ;                          (* "11001"b *)
  prof = 'D00000000'x ;                           (* "11010"b *)
  absl = '000000000'x ;                           (* "0"b *)
  symb = 'B00000000'x ;                           (* "10110"b *)

  (*  CONSTANTES USED BY SIMONE COMPILER *)       { Inserted for SimOne }
  maxchar8 = 255 ;
  mofatherdisp = 1 ; (* DIFFERENCE BETWEEN ADDR OF FATHER WHOSE TYPE IS MONORMOD *) { Inserted for SimOne }
  procfatherdisp = 0 ; (* OR WHOSE TYPE IS PROC *) { Inserted for SimOne }
  procmodlkw = 104 ;                              { Inserted for SimOne }
  modynlk = 0 ;                                   { Inserted for SimOne }
  condcounterplace = 16 ; (* BYTES DIP. OF CONDITION COUNTER *) { Inserted for SimOne }
  moarglistw = 6 ; (* DEPLACEMENT OF ARGLIST IN MONITOR OR MODULE PSEUDO_STACK *) { Inserted for SimOne }
  baseprocessdplmt = 100 ;                        { Inserted for SimOne }
  executionmodplmt = 106 ;                        { Inserted for SimOne }
  maindplmt = 0 ;
  processlocaldplmt = 96 ;
  (* ENTRY POINTS OF OPERATORS *)                 { Inserted for SimOne }
                                                  { Inserted for SimOne }
  simainentryplace = -1 ;                         { Inserted for SimOne }
  simintentryplace = -2 ;                         { Inserted for SimOne }
  simintreturnplace = -3 ;                        { Inserted for SimOne }
  processentryplace = -4 ;                        { Inserted for SimOne }
  settinginactivequeueplace = -5 ;                { Inserted for SimOne }
  waitsonsplace = -6 ;                            { Inserted for SimOne }
  processreturnplace = -7 ;                       { Inserted for SimOne }
  holdplace = -8 ;                                { Inserted for SimOne }
  savearglistplace = -9 ;                         { Inserted for SimOne }
  modulentryplace = -10 ;                         { Inserted for SimOne }
  monitorentryplace = -11 ;                       { Inserted for SimOne }
  initcondplace = -12 ;                           { Inserted for SimOne }
  askforexclusionplace = -13 ;                    { Inserted for SimOne }
  freeexclusionplace = -14 ;                      { Inserted for SimOne }
  signalplace = -15 ;                             { Inserted for SimOne }
  waitplace = -16 ;                               { Inserted for SimOne }
  emptyplace = -17 ;                              { Inserted for SimOne }
  lengthplace = -18 ;                             { Inserted for SimOne }
  priorityplace = -19 ;                           { Inserted for SimOne }
  vtimeplace = -20 ;                              { Inserted for SimOne }
  terminateplace = -21 ;                          { Inserted for SimOne }
  mowaitsonsplace = -22 ;                         { Inserted for SimOne }
  uniformplace = -23 ;                            { Inserted for SimOne }
  normalplace = -24 ;                             { Inserted for SimOne }
  negexpplace = -25 ;                             { Inserted for SimOne }
  randintplace = -26 ;                            { Inserted for SimOne }
  pureentryplace = -27 ;
  simextentryplace = -28 ;
  exitplace = -29 ;
  restorprevmoplace = -30 ;
                                                  { Inserted for SimOne }
  lcprocess = 96 ; (* BYTES SIZE OF THE LOCAL VARIABLES IN THE STACK OF A PROCESS *) { Inserted for SimOne }
                                                  { Inserted for SimOne }
                                                  { Inserted for SimOne }
  emptyindex = 4 ;                                { Inserted for SimOne }
                                                  { Inserted for SimOne }
                                                  (* DEFAULT  LENGTH  FOR EDITION *)
  deflreal = 24 ;
  deflnum = 12 ;
  deflbool = 4 ;
  deflchar = 1 ;



(*$PAGE *)
TYPE
  numberstring = PACKED ARRAY [1..maxdigitsreal] OF char ;
  alfa = PACKED ARRAY [1..alfaleng] OF char ;
  alfaid = PACKED ARRAY [1..maxident] OF char ;
  externid = PACKED ARRAY [1..maxexternname] OF char ;
  alfalistptr = ^alfalist ;
  alfalist = RECORD
    previous, next : alfalistptr ;
    name : alfaid
  END ;
  idkinds = (actual, formal, arraybound, exportable, imported) ;

(*  ACTUAL  MEANS STANDARD  PASCAL  PROC/VARS
   FORMAL  USED  FOR VAR  PARAMETERS
   EXPORTABLE     'DEF'  PROC/VARS
   IMPORTED     APPEARS  IN EXTERNAL  LIST.  MUST BE REDEFINED *)
  typform = (reel, numeric, scalar, pointer, power, arrays, records, monormod, { Modified for SimOne }
    condition, files, aliastype) ;                { Modified for SimOne }
                                                  { Modified for SimOne }
  idklass = (schema, types, konst, proc, vars, field, tagfield, dummyclass) ;
  consttype = (wordconst, dwordconst, alfaconst) ;
  idprocdef = (standdef, forwdef, extdef, initdef, finitdef) ; { Modified for SimOne }
  levrange = 0..maxlevel ;
  alfapt = @alfavalue ;
  alfavalue = RECORD
    nextval : alfapt ;                            (* NEXT VALUE BOX FOR  SAME STRING *)
    alfaval : PACKED ARRAY [1..longalfbox] OF char ;
    longfill : integer ;                          (* USED PART OF  ALFAVAL IN  THIS BOX *)
  END ;
  refptr = @reflist ;
  reflist = RECORD
    nextref : refptr ;
    refnbr : integer ;
    refs : ARRAY [1..maxref] OF RECORD
      filen, linen, sttmapind, place : integer ;
    END ;
  END ;
  ctp = @contexttable ;
  setofno = SET OF minno..maxno ;
  stdkind = (stdpure, stdcompiler, stdsol, stdextend, stdcomputer) ;
  typusednames = ARRAY [1..6] OF alfaid ;
  filelocation = (notafile, permanentfile, workfile, localfile, standardfile) ;

  proclocation = (notpredef, instdpure, instdcompiler, instdsol,
    instdextend, instdcomputer, instdsimone) ;    { Modified for SimOne }
  externalitemtype = (extnotresolved, externalarea, exportvar, importvar,
    exportproc, importproc, localproc, mainprogram, remanentfile,
    requiredfile, runtimeentry) ;
  ptexternalitem = ^ externalitem ;
  externalitem = RECORD
    extname : alfaid ;
    extsegname, extgenerator, extentryname : alfaid ;
    extnext : ptexternalitem ;
    extrfile1, extrline1, extrfile2, extrline2 : integer ;
    extdecl : ctp ;
    extitemtype : externalitemtype ;
    extkind : idkinds ;
    extpltdisp : integer ;
    extareadisp : integer ;
    extlong : integer ;
    extwantdescs : boolean ;
  END ;
  (* TYPES USED BY SIMONE COMPILER *)             { Inserted for SimOne }
  motypes = (module, monitor) ;                   { Inserted for SimOne }
  objaccessibles = (modul, monit, condit) ;       { Inserted for SimOne }
  ensaccessible = SET OF objaccessibles ;         { Inserted for SimOne }
  incbloc = (monitormodule, subroutine) ;         { Inserted for SimOne }
                                                  { Inserted for SimOne }
  nodeptr = @node ;                               { Inserted for SimOne }
                                                  { Inserted for SimOne }
  node = RECORD { USED FOR SPACE COMPUTATION }    { Inserted for SimOne }
    inthelist, compiled, recursive : boolean ;    { Inserted for SimOne }
    sizemax : integer ;                           { Inserted for SimOne }
    proce : ctp ;                                 { Inserted for SimOne }
  END ;                                           { Inserted for SimOne }
  nodelistptr = @nodelistelem ;                   { Inserted for SimOne }
                                                  { Inserted for SimOne }
  nodelistelem = RECORD                           { Inserted for SimOne }
    previousnode, nextnode : nodelistptr ;        { Inserted for SimOne }
    elem : nodeptr ;                              { Inserted for SimOne }
  END ;                                           { Inserted for SimOne }
                                                  { Inserted for SimOne }
  exitptr = @exitelem ;                           { Inserted for SimOne }
  exitelem = RECORD (* ONE FOR EACH EXIT STATEMENT *) { Inserted for SimOne }
    nextexitelem : exitptr ;                      { Inserted for SimOne }
    exitdplmt : integer ;                         { Inserted for SimOne }
  END ;                                           { Inserted for SimOne }

  ftp = ^schema_token ;
  schema_status = RECORD
    on : boolean ;
    schema_ptr : ctp ;
    current_token : ftp ;
    current_parameter : ctp ;
  END ;
  schema_token_kind = (symbol_token, name_token, int_const_token, char_const_token, real_const_token) ;
  schema_token = RECORD
    next : ftp ;
    CASE kind : schema_token_kind OF
    symbol_token : (tno, tcl : integer) ;
    name_token : (taval : alfaid) ;
    int_const_token : (t_int_value : integer) ;
    real_const_token : (t_real_value : real) ;
    char_const_token : (t_char_value : char) ;
  END ;

(* DONT MODIFY CONTEXTABLE DECLARATION
   WITHOUT CHECKING :
   nameplaceincontextable
   AND nxtelplaceincontextable
   IN optimized_procedures.alm *)
  contexttable = RECORD
    name : alfaid ;
    nxtel : ctp ;
    alfathread : ctp ;
    deffile, defline : integer ;
    references : refptr ;
    symbolplace : integer ;                       (* PCK PTR TO SYMBOL NODE IN SYMB TB *)
    symbtablerefs : integer ;                     (* BACKWARD THREAD OF REFS IN TEXT TO SYMBOL TABLE *)
    CASE klass : idklass OF
    schema : (
      top_for_schema : integer ;                  (* CONTEXT AT SHEMA DECLARATION TIME *)
      next_for_schema : ctp ;
      formal_parameter_list : ctp ;
      parameter_count : integer ;
      token_list : ftp ;
      type_description : ctp) ;
    types : (
      size, cadrage : integer ;
      pack : boolean ;
      tlevel : levrange ;
      objaccedes : ensaccessible ; { TO ALLOW VARIABLES ' DECLARATIONS } { Inserted for SimOne }
                                                  (* *** BEGIN SCHEMA INFO *** *)
      father_schema : ctp ;
      actual_parameter_list : ctp ;
      desc_vector_references : integer ;
                                                  (* *** END SCHEMA INFO *** *)
      CASE form : typform OF
      reel : () ;
      numeric : (npksize, nmin, nmax : integer) ;
      scalar : (spksize : integer ;
        CASE subrng : boolean OF
        false : (fconst, sptcstepw : ctp) ;
        true : (smin, smax : integer ;
	typset : ctp) ;) ;
      pointer : (ptpksize : integer ;
        domain, eltype : ctp) ;
      power : (ppksize : integer ;
        elset : ctp ;
        setlength : integer) ;
      arrays : (aeltype, inxtype : ctp ;

        CASE conformant : boolean OF
        false : (lo, hi, opt2, subsize : integer) ;
        true : (
	pthigh, ptlow : ctp ;
	) ;
        ) ;
      records : (recvar, fstfld : ctp) ;
      files : (feltype : ctp ;
        ) ;
      monormod : (motype : motypes ;              { Inserted for SimOne }
        niveau : levrange ;                       { Inserted for SimOne }
        moaddr, nbparmo : integer ;               { Inserted for SimOne }
        ptpar, ptvarloc, ptentr : ctp ;           { Inserted for SimOne }
        initmoproc, finitmoproc, blocenglob : ctp) ; { Inserted for SimOne }
      condition : () ;                            { Inserted for SimOne }
      aliastype : (realtype : ctp) ;) ;
    konst : (
      succ, contype : ctp ;
      CASE typofconst : consttype OF
      wordconst : (values : integer) ;
      dwordconst : (valreel : real) ;
      alfaconst : (alfadeb : alfapt ;
        alfalong, alfalevel, unddeb : integer) ;) ;
    proc : (proctype, formals : ctp ;
      prockind : idkinds ;
      proclevel : levrange ;
      procaddr, segsize, nbparproc, locincode : integer ;
      procisassigned, predefproc, procinscope, pisrefincode : boolean ;
      phasdescriptor : boolean ;                  (* TRUE IF HAS CONF ARRAY PARAMETERS *)
      ploc : proclocation ;
      procextitem : ptexternalitem ;
      ptypesymbolplace : integer ;                (* PACKED PTR TO TYPE BOX IN SYMBOL TB *)
      recur : integer ;                           { Inserted for SimOne }
      procaccesslevel : levrange ;                { Inserted for SimOne }
      processus, pure : boolean ;                 { Inserted for SimOne }
      chaineentree : ctp ;                        { Inserted for SimOne }
      procnode : nodeptr ;                        { Inserted for SimOne }
      procincbloc : incbloc ; (* BLOC CONTAINING THIS PROC *) { Inserted for SimOne }
      procfirstexit : exitptr ; (* TO CHAIN EXIT STATEMENT *) { Inserted for SimOne }
      procfirsttofinit : ctp ; (* FIRST VAR TO BE FINALIZED IN THIS PROCESS, ALWAYS NIL FOR A PROCEDURE *) { Inserted for SimOne }
      procstackinitsize : integer ;               { Inserted for SimOne }
      procwasforwarddef : boolean ;               { Inserted for SimOne }
      procdef : idprocdef ;
      pwantdescs : boolean ;                      (* TRUE IF PROC WAS DECLARED EXT DESCRIPTORS *)
      pdescsaddrplace : integer ;                 (* PLACE IN STATICS OF PTR TO DESCRS VECTOR - IF PREV. TRUE *)
      procisactive : boolean ;                    (* TRUE IF COMPILER IS ANALYZING BODY OF THIS PROC *)
      pextcalltrapinfoplace : integer ;           (* WORD OFFSET OF TRAP INFO FOR EXT CALL - ONLY IF DESCS *)
      pwantspl1descriptors : boolean) ;           (* TRUE IF PL1 DESCRIPTORS NEEDED *)
    vars : (vtype : ctp ;
      vkind : idkinds ;
      vfilelocation : filelocation ;
      vaddr : integer ;
      vdispl, vdescaddr : integer ;
      vlevel : levrange ;
      vlink_is_generated : boolean ;
      visused, visset, visreadonly, varparam, visrefincode : boolean ;
      vnexttofinit : ctp ; (* TO CHAIN VAR TO BE FINIT *) { Inserted for SimOne }
      varmo : boolean ; (* TRUE IF VAR IS DECLARE IN A MONITOR OR A MODULE *) { Inserted for SimOne }
      vfather : ctp ; (* PTR ON THE MONITOR OR THE MODULE CONTAINING VAR *) { Inserted for SimOne }
      vptextitem : ptexternalitem) ;
    field : (fldtype : ctp ;
      fldaddr, bytwidth : integer) ;
    tagfield : (casesize : integer ;
      variants : ctp ;
      CASE tagval : boolean OF
      false : (casetype : ctp ; selectorfield : ctp) ;
      true : (caseval : integer ; firstfield : ctp) ;) ;
    dummyclass : () ;
  END ;                                           (* RECORD CONTEXTTABLE *)
                                                  (* NOW  OTHER TYPES *)

  shrtint = -twoto17..twoto17m1 ;
  bytint = -twoto8..twoto8m1 ;
  norange = minno..maxno ;
  where = (block, cwith, vwith) ;                 (* USED TO DESCRIBE AN IDENTIFIER *)
  contexte = (data, code, linkage, definition, deccode) ; { Modified for SimOne }
  levtrace = (none, low, medium, high) ;
                                                  (* DONT MODIFY RECIDSCOPE DECLARATION *)
  withreflist = RECORD
    nbr : 0..maxfield ;
    symbolp : ARRAY [1..maxfield] OF ctp
  END ;
                                                  (* WITHOUT CHECK : *)
                                                  (* recidscopelength IN optimized_procedures.alm *)
  recidscope = RECORD                             (* ELEMENT OF  DISPLAY *)
    fname : ctp ;
    CASE occur : where OF
    block : () ;
    cwith : (creflist : withreflist ; clevel : levrange ; cdspl : integer) ;
    vwith : (vreflist : withreflist ; vdspl : integer ; vpack : boolean) ;
  END (* REC-ID-SCOPE *) ;
  blocknodeptr = @blocknode ;
  labelblockptr = ^labelblock ;                   (* BOXES FOR LABELS *)
  labelblock = RECORD
    number : integer ;                            (* LABEL ID *)
    locinbytes : integer ;                        (* LOCATION IN TEXT SECTION (BYTES) *)
    next : labelblockptr ;                        (* NEXT IN LABELS LIST *)
    brother : labelblockptr ;                     (* THREAD FOR LABELS OF SAME BLOCK *)
    procnode : blocknodeptr ;                     (* PTR TO PROCEDURE NODE *)
    dclfile, dclline : integer ;                  (* FILE, LINE FOR DECLARATION *)
    deffile, defline : integer ;                  (* FILE, LINE OF LOCATION *)
    references : refptr ;                         (* PTR TO REFERENCES BOX(ES) *)
    ref_allowed : RECORD
      ic_from, ic_to : integer
    END ;
    next_in_block : labelblockptr ;
  END ;
  label_pdl_element = RECORD
    previous, next : ^label_pdl_element ;
    first : labelblockptr ;
  END ;
  labdescr = RECORD                               (* ELEMENT  OF  LABTAB *)
    labval, lablev, labexit, labch1, labdef : integer ;
    labbox : labelblockptr ;
  END (* REC *) ;
  lab_pdl_ptr = ^lab_pdl_element ;
  lab_pdl_element = RECORD
    previous, next : lab_pdl_ptr ;
    first_in_block : labelblockptr ;
    start : integer
  END ;
  typofsymb = (irrelsy, begsy, endsy) ;
  occurence = RECORD                              (* ELEMENT OF UNDLAB *)
    succ, place : shrtint ;
  END (* REC *) ;
                                                  (* TYPES  USED IN CODE GENERATION *)

(*  ALM INSTRUCTIONS , I+MNEMONIC. INSTRUCTIONS ARE GROUPED BY FUNCTIONS. IN
   EACH FUNCTION, THE ORDER IS THE ONE OF THE AL39 MANUAL EXCEPT FOR EIS
   MULTIWORD. IN THIS LAST GROUP, THE ORDER MAKES EASIER THE CODE GENERATION *)
(* FIXED-POINT STANDARD INSTRUCTIONS ****** *)
  instword = (ieaa, ieaq, ieax0, ieax1, ieax2, ieax3, ieax4, ieax5,
    ieax6, ieax7, ilca, ilcaq, ilcq, ilcx0, ilcx1, ilcx2,
    ilcx3, ilcx4, ilcx5, ilcx6, ilcx7, ilda, ildac, ildaq,
    ildi, ildq, ildqc, ildx0, ildx1, ildx2, ildx3, ildx4,
    ildx5, ildx6, ildx7, ilreg, ilxl0, ilxl1, ilxl2, ilxl3,
    ilxl4, ilxl5, ilxl6, ilxl7, isreg, ista, istac, istacq,
    istaq, istc1, istc2, istcd, isti, istq, istt, istx0,
    istx1, istx2, istx3, istx4, istx5, istx6, istx7, istz,
    isxl0, isxl1, isxl2, isxl3, isxl4, isxl5, isxl6, isxl7,
    ialr, ials, iarl, iars, illr, ills, ilrl, ilrs,
    iqlr, iqls, iqrl, iqrs, iada, iadaq, iadl, iadla,
    iadlaq, iadlq, iadlx0, iadlx1, iadlx2, iadlx3, iadlx4, iadlx5,
    iadlx6, iadlx7, iadq, iadx0, iadx1, iadx2, iadx3, iadx4,
    iadx5, iadx6, iadx7, iaos, iasa, iasq, iasx0, iasx1,
    iasx2, iasx3, iasx4, iasx5, iasx6, iasx7, iawca, iawcq,
    isba, isbaq, isbla, isblaq, isblq, isblx0, isblx1, isblx2,
    isblx3, isblx4, isblx5, isblx6, isblx7, isbq, isbx0, isbx1,
    isbx2, isbx3, isbx4, isbx5, isbx6, isbx7, issa, issq,
    issx0, issx1, issx2, issx3, issx4, issx5, issx6, issx7,
    iswca, iswcq, impf, impy, idiv, idvf, ineg, inegl,
    icmg, icmk, icmpa, icmpaq, icmpq, icmpx0, icmpx1, icmpx2,
    icmpx3, icmpx4, icmpx5, icmpx6, icmpx7, icwl, iszn, isznc,
                                                  (* FLOATING-POINT INSTRUCTIONS ************* *)
    idfld, ifld, idfst, idfstr, ifst, ifstr, idfad, idufa,
    ifad, iufa, idfsb, idufs, ifsb, iufs, idfmp, idufm,
    ifmp, iufm, idfdi, idfdv, ifdi, ifdv, ifneg, ifno,
    idfrd, ifrd, idfcmg, idfcmp, ifcmg, ifcmp, iade, ifszn,
    ilde, iste,
                                                  (* BOOLEAN INSTRUCTIONS ******************** *)
    iana, ianaq, ianq, iansa, iansq, iansx0, iansx1, iansx2,
    iansx3, iansx4, iansx5, iansx6, iansx7, ianx0, ianx1, ianx2,
    ianx3, ianx4, ianx5, ianx6, ianx7, iora, ioraq, iorq,
    iorsa, iorsq, iorsx0, iorsx1, iorsx2, iorsx3, iorsx4, iorsx5,
    iorsx6, iorsx7, iorx0, iorx1, iorx2, iorx3, iorx4, iorx5,
    iorx6, iorx7, iera, ieraq, ierq, iersa, iersq, iersx0,
    iersx1, iersx2, iersx3, iersx4, iersx5, iersx6, iersx7, ierx0,
    ierx1, ierx2, ierx3, ierx4, ierx5, ierx6, ierx7, icana,
    icanaq, icanq, icanx0, icanx1, icanx2, icanx3, icanx4, icanx5,
    icanx6, icanx7, icnaa, icnaaq, icnaq, icnax0, icnax1, icnax2,
    icnax3, icnax4, icnax5, icnax6, icnax7,
                                                  (* POINTER REGISTERS INSTRUCTIONS ********** *)
    ieasp0, ieasp1, ieasp2, ieasp3, ieasp4, ieasp5, ieasp6, ieasp7,
    ieawp0, ieawp1, ieawp2, ieawp3, ieawp4, ieawp5, ieawp6, ieawp7,
    iepbp0, iepbp1, iepbp2, iepbp3, iepbp4, iepbp5, iepbp6, iepbp7,
    iepp0, iepp1, iepp2, iepp3, iepp4, iepp5, iepp6, iepp7,
    ilpri, ilprp0, ilprp1, ilprp2, ilprp3, ilprp4, ilprp5, ilprp6,
    ilprp7, ispbp0, ispbp1, ispbp2, ispbp3, ispbp4, ispbp5, ispbp6,
    ispbp7, ispri, ispri0, ispri1, ispri2, ispri3, ispri4, ispri5,
    ispri6, ispri7, isprp0, isprp1, isprp2, isprp3, isprp4, isprp5,
    isprp6, isprp7, iadwp0, iadwp1, iadwp2, iadwp3, iadwp4, iadwp5,
    iadwp6, iadwp7, iepaq,
                                                  (* TRANSFER INSTRUCTIONS ******************* *)
    icall6, iret, irtcd, iteo, iteu, itmi, itmoz, itnc,
    itnz, itov, itpl, itpnz, itra, itrc, itrtf, itrtn,
    itsp0, itsp1, itsp2, itsp3, itsp4, itsp5, itsp6, itsp7,
    itss, itsx0, itsx1, itsx2, itsx3, itsx4, itsx5, itsx6,
    itsx7, ittf, ittn, itze,
                                                  (* MISCELLANEOUS INSTRUCTIONS ************** *)
    irccl, idrl, ixec, ixed, imme, imme2, imme3, imme4,
    inop, ipuls1, ipuls2, isra, isbar, ibcd, igtb,
                                                  (* PRIVILEGED INSTRUCTIONS ***************** *)
    ilbar, ilcpr, ildbr, ildt, ilptp, ilptr, ilra, ilsdp,
    ilsdr, ircu, iscpr, iscu, isdbr, isptp, isptr, issdp,
    issdr, icamp, icams, irmcm, irscr, irsw, icioc, ismcm,
    ismic, isscr, iabsa, idis,
                                                  (* SINGLE WORD EIS INSTRUCTIONS ************ *)
    iaar0, iaar1, iaar2, iaar3, iaar4, iaar5, iaar6, iaar7,
    ilar0, ilar1, ilar2, ilar3, ilar4, ilar5, ilar6, ilar7,
    ilareg, ilpl, inar0, inar1, inar2, inar3, inar4, inar5,
    inar6, inar7, iara0, iara1, iara2, iara3, iara4, iara5,
    iara6, iara7, iarn0, iarn1, iarn2, iarn3, iarn4, iarn5,
    iarn6, iarn7, isar0, isar1, isar2, isar3, isar4, isar5,
    isar6, isar7, isareg, ispl, ia4bd, ia6bd, ia9bd, iabd,
    iawd, is4bd, is6bd, is9bd, isbd, iswd,
                                                  (* MULTI-WORDS EIS INSTRUCTIONS ************ *)
    itct, itctr, icmpc, iscm, iscmr, imlr, imrl, imvt,
    icsl, icsr, isztl, isztr, iscd, iscdr, icmpn, imvn,
    icmpb, ibtd, idtb, iad2d, isb2d, imp2d, idv2d, imve,
    imvne, iad3d, isb3d, imp3d, idv3d,
                                                  (* REPEAT INSTRUCTIONS ********************* *)
    irpd, irpl, irpt,
                                                  (* STBA, STBQ ,STCA AND STCB INSTRUCTIONS ** *)
    istba, istbq, istca, istcq) ;
                                                  (* ADDRESS MODIFICATIONS  , *)
                                                  (* T+MNEMONIC  AND Y USED FOR *  , *)
                                                  (* TZ = ILLEGAL MODIFIER *)
                                                  (* BINARY CODE IS OBTAINED WITH THE ORD FUNCTION *)
  tag = (tn, tau, tqu, tdu, tic, tal, tql, tdl,   (* R  MOD *)
    tx0, tx1, tx2, tx3, tx4, tx5, tx6, tx7,
    tny, tauy, tquy, tz23, ticy, taly, tqly, tz27, (* RI MOD *)
    tx0y, tx1y, tx2y, tx3y, tx4y, tx5y, tx6y, tx7y,
    tf1, titp, tz42, tits, tsd, tscr, tf2, tf3,   (* IT MOD *)
    tci, ti, tsc, tad, tdi, tdic, tid, tidc,
    tz60, tyau, tyqu, tydu, tyic, tyal, tyql, tydl, (* IR MOD *)
    tyx0, tyx1, tyx2, tyx3, tyx4, tyx5, tyx6, tyx7) ;
  register = (nreg, pr1, pr2, pr5, pr7, pr3, pr0, prstatic, prlink, pr6,
    nxreg, x0, x1, x2, x3, x4, x5, x6, x7,
    xbidon, ra, rq, raq, reaq, psr, re, ri) ;     (* USED REGISTERS *)
  preg = nreg..pr6 ;                              (* POINTER REGISTERS - SUBRANGE OF REGISTER *)
  mreg = tn..tx7 ;                                (* ADDRESS MODIFICATIONS WITHOUT INDIRECTION *)
                                                  (* - SUBRANGE OF TAG *)
                                                  (* SUBRANGES OF INSTWORD USED IN CODE GENERATION *)
  istand = ieaa..iswd ;                           (* STANDARD INSTRUCTIONS *)
  ieism = icmpc..idv3d ;                          (* EIS MULTIWORD INSTRUCTIONS *)
  irept = irpd..irpt ;                            (* REPEAT INSTRUCTIONS *)
  istobc = istba..istcq ;                         (* STORE BYTES OR CHARACTERS INSTRUCTIONS *)
  lgcar = (l4, l6, l9) ;                          (* CHARACTER DATA TYPE - NUMBER = LENGTH IN BITS *)
  typsig = (flls, fxls, fxts, fxns) ;             (* SIGN AND DECIMAL TYPES,FL=FLOATING-POINT, *)
                                                  (* FX=FIXED-POINT,LS= LEADING SIGN *)
                                                  (* TS= TRAILING SIGN *)
                                                  (* NS= NO SIGN *)
                                                  (* BIT VALUES FOR CODE GENERATION *)
  zptr = (p0t0r0, p0t0r1, p0t1r0, p0t1r1, p1t0r0, p1t0r1, p1t1r0, p1t1r1) ; (* BITS 0,9 *)
                                                  (* AND 10 OF EIS MULTIWORD INSTRUCTIONS *)
                                                  (* P=SIGN OR FILL BIT , T=TRUNCATION BIT , *)
                                                  (* R=ROUNDING FLAG BIT *)
  zabc = (a0b0c0, a0b0c1, a1b0c0, a1b0c1, a0b1c0, a0b1c1, a1b1c0, a1b1c1) ; (* BITS 8,9 *)
                                                  (* AND 10 OF REPEAT INSTRUCTIONS *)
                                                  (* A AND B = USE OF DELTA FIELD , *)
                                                  (* C = USE OF X0 REGISTER *)
  zari = (a0r0i0, a0r0i1, a0r1i0, a0r1i1, a1r0i0, a1r0i1, a1r1i0, a1r1i1) ; (* BITS 0,1 *)
                                                  (* AND 2  OF EIS MODIFICATION FIELDS *)
                                                  (* A=USE OF ADDRESS REGISTER , *)
                                                  (* R = OPERAND LENGTH FIELD GIVES A REGISTER *)
                                                  (* I= USE OF INDIRECTION FOR *)
                                                  (* THE OPERAND DESCRIPTOR *)

(* USED ONLY IN CONDITIONNAL COMPILATION FOR COMPILER'S CONTROLS *)
  forset = (s0, s1, s2, s3, s4, s5) ;             (* USED TO GIVE FORBIDDEN TAGS/PTR FOR ISTAND/ *)
                                                  (* IEISM *)
  halfword = integer ;                            (* USED FOR FICHINTER *)
  binartype = PACKED ARRAY [1..maxfich] OF halfword ; (* FICHINTER *)
  binartypeptr = ^binartype ;
  attrkind = (varbl, lcond, lval, chain, sval) ;
  attraccess = (direct, pointee, pointable, encode) ;
  destination = (inacc, inq, inaq, inpsr, inpr, out) ;
  setarray = ARRAY [0..bornesupset] OF integer ;
  regpt = @regbox ;
  regbox = RECORD                                 (* BOX DESCRIBING A LOADED REGISTER *)
                                                  (* AND MEMORIZING SAVING INFORMATIONS *)
    sregister : register ;                        (* LOADED REGISTER *)
    saveplace : integer ;                         (* BYTES DISP. /PR6 OF SAVING STORAGE *)
    nextbloc : regpt ;                            (* POINTS PREVIOUS BLOC.(NOT NEXT.) *)
    predbloc : regpt ;                            (* POINTS NEXT BLOC. (NOT PRED.) *)
  END ;                                           (* REGBOX *)
  wcstpt = @iunresolv ;
  rcstpt = @runresolv ;
  lcstpt = @liunresolv ;
  llcstpt = @lliunresolv ;
  iunresolv = RECORD
    valu : integer ;
    cstplace : integer ;                          (* ENTRY IN UNDLAB *)
    cstnext : wcstpt ;                            (* LINKAGE OF WORD CSTES *)
  END ;
  runresolv = RECORD
    rvalu : real ;
    rplace : integer ;                            (* ENTRY IN UNDLAB *)
    rnext : rcstpt ;                              (* LINKAGE OF REAL CSTES *)
  END ;
  liunresolv = RECORD
    lvalu : setarray ;
    lplace : integer ;                            (* ENTRY IN UNDLAB *)
    lnext : lcstpt ;                              (* LINKAGE OF LONG CSTES *)
  END ;
  lliunresolv = RECORD
    llvalu : setarray ;                           (* SET CSTE *)
    llplace : integer ;                           (* ENTRY ON UNDLAB *)
    llnext : llcstpt ;                            (* LINKAGE OF SET CSTES *)
  END ;

  attr = RECORD

    typtr : ctp ;                                 (* TYPE OF DESCRIBED ITEM *)
    CASE kind : attrkind OF

    varbl : (                                     (*  ITEM IS ANYWHERE IN STORAGE *)
      vlev : levrange ;                           (* DEFINITION LEVEL *)
      basereg : preg ;                            (* BASIS TO ACCES ITEM, *)
                                                  (* OR ITS TO ITEM IF POINTABLE *)
      basebloc : regpt ;
                                                  (* POINTS THE BOX DESCRIBING BASEREG; *)
                                                  (* NIL FOR PR4,PR6 *)
      dplmt : integer ;                           (* BYTES DISPLACEMENT TO ADD AT FINAL ADDRESS *)
      inxreg : register ;                         (* MODIFICATION REGISTER CONTAINING WORDS DISP *)
                                                  (* RA  ==>  A 18..35  USED *)
                                                  (* RQ  ==>  Q 18..35  USED *)
                                                  (* XI  ==>  INDEX REGISTER *)
                                                  (* NXREG   NO MODIFIER *)
      inxbloc : regpt ;                           (* POINTS BOX DESCRIBING INXREG *)
      inxmem : integer ;                          (* BYTES DISPLACEMENT/PR6 OF A STORAGE WORD *)
                                                  (* CONT. IN BITS 18..35 A WORD OFFSET SIGNED *)
      inxmemrw : boolean ;                        (* TRUE IF INXMEM CAN BE WRITTEN *)
      access : attraccess ;

(* DIRECT ITEM=[[BASIS]+[INDEX]+DPLMT+[INXMEM]] BASIS=PR4/PR6
   POINTEE   SAME BUT BASIS= ANY POINTER REGISTER
   POINTABLE ITEM=[[[BASIS]+ITSDPLMT]+[INDEX]+DPLMT+[INXMEM]]
   ENCODE    ITEM GENERATEA IN CODE *)
      itsdplmt : integer ;                        (* BYTES DISP OF AN ITS EVEN BOUNDARY *)
      pckd : boolean ;                            (* TRUE IF CONTAINING STRUCTURE WAS PACKED *)
      nameaddr : ctp ;                            (* PTR TO NAME OF CONCERNED VAR OR FIELD *)
      descreg : preg ;                            (* PTR TO DESCRIPTOR *)
      descbloc : regpt ;                          (* BOX FOR THIS POINTER *)
      vmo : boolean ; (* TRUE IF INTERNAL VAR OF MONITOR OR MODULE ACCESS BY PR5 *) { Inserted for SimOne }
      temporary : boolean ;                       (* TRUE IF THIS VARIABLE IS THE RESULT OF AN EXPRESSION,
                                                     ALLOCATED IN STACK (USED FOR RESULT OF STRING EXPRESSIONS *)
      ) ;                                         (* END VARBL *)

    lval : (                                      (* ITEM IS LOADED IN A *)
                                                  (* REGISTER RA,RQ,RAQ,REAQ,PSR *)
      ldreg : register ;                          (* LOADED REGISTER *)
      ldregbloc : regpt ;                         (* POINTS THE BOX DESCRIBING LDREG *)
      psrsize : integer ;                         (* SIZE MEANINGSFULL IN BYTES OF PSR IF LOADED *)
      ) ;                                         (* END LVAL *)

    chain : (                                     (* ITEM DESCRIBED IS A CHARACTER STRING CONSTANT *)
                                                  (* GENERATED AT THE END OF CODE *)
                                                  (* (WORD BOUNDARY ALIGNED) *)
      alfactp : ctp ;                             (* POINTS THE (KONST,ALFACONST) BOX ASSOCIATED *)
      ) ;                                         (* END CHAIN *)

    sval : (                                      (* ITEM DESCRIBED IS A COMPUTABLE VALUE *)
                                                  (* BY THE COMPILER *)
      val : integer ;                             (* WORD CONSTANT *)
      rsval : real ;                              (* FLOAT CONSTANT *)
      valpw : setarray ;                          (* SET CONSTANT *)
      longv : integer ;                           (* BYTES LENGTH MEANINGSFULL IN VALPW *)
      ) ;                                         (* END SVAL *)

    lcond : (                                     (* ITED DESCRIBED IS A BOOLEAN EITHER LOADED OR *)
                                                  (* KNOWN ONLY BY THE SETTING OF INDICATORD *)
      accbloc : regpt ;                           (* POINTS BOX DESCRIBING RA IF USED *)
      accbool : boolean ;                         (* TRUE  <==> RA IS USED TO KEEP LCOND *)
      transf : integer ;                          (* GIVES THE SUITABLE INDICATORS TESTING *)
      ) ;                                         (* END LCOND *)

  END ;                                           (* RECORD ATTR *)

  typepr = (epp, spri, lprp) ;                    (*  FOR  PR. INST *)
  typix = (adlx, adx, sxl, lxl) ;                 (*  FOR  XI  INST *)
  typeofop = (load, sub, shiftl, add, neg, cmp, stor) ; (*  FOR  A,Q,AQ,EAQ INST *)
  statearray = ARRAY [register] OF boolean ;      (* STATE OF REGISTERS *)

(* STATEMENT MAP *)

  sttmapptr = @sttmap ;
  sttmap = ARRAY [1..30000] OF
  RECORD
    word1 : integer ;
    word2 : integer
  END ;

(* PROFILE COUNTERS AREA *)

  profareaptr = @profarea ;
  profarea = ARRAY [0..60000] OF integer ;

(* BLOCK NODE *)

  blocktype = (procblock, withblock) ;
  vararea = (statics, locals) ;
  blocknode = RECORD
    father, brother, son : blocknodeptr ;         (* BLOCKS TREE *)
    codebegin, codeend : integer ;                (* OFFSETS IN STATEMENT MAP *)
    first : ctp ;                                 (* PTR TO FIRST DECLARED SYMBOL *)
    CASE blocktp : blocktype OF
    procblock : (
      blockbox : ctp ;                            (* SYMBOL BLOCK FOR PROCEDURE *)
      structureplace : integer ;                  (* OFFSET OF ENTRY STRUCTURE *)
      firstlabel : labelblockptr ;                (* PTR TO FIRST LABEL *)
      nextproc : blocknodeptr ;                   (* NEXT BLOCK *)
      hdrfil, hdrind, hdrlen, hdrlin : integer ;  (* SOURCE OF HEADER *)
      ) ;
    withblock : (
      wbase : vararea ;
      wdispl : integer ;                          (* OFFSET *)
      windirect : boolean ;
      wstrfile, wstrindex, wstrlength : integer ;
      recordptr : ctp ;
      ) ;
  END ;

(* CONDITIONNAL COMPILATION MECHANISM *)

  condaddr = ^condbox ;
  condbox = RECORD
    condname : alfaid ;
    nextcond : condaddr ;
    active, activated, setinargs : boolean
  END ;


(* ARRAY OF POINTERS TO USED NAMES - FOR "-list" OPTION *)

  usednamesarray = PACKED ARRAY [0..maxwseg] OF ctp ;
  usednamesptr = @usednamesarray ;


(* END INCLUDE FILE CONSTTYPE.incl.pascal *)
