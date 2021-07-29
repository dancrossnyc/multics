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
  PROGRAM genere ;
    $IMPORT
                                                  (* IMPORTED PROCEDURES *)
      'RACINE (pascal)' :
        crealfabox,
        error,
        nextline,
        recadre,
        statement_begins,
        statement_ends ;
      'STATE (pascal)' :
        enterundlab,
        gencstecode ;
      'CONTEXTTABLE (pascal)' :
        create_konst_box ;

(* FROM PL1     *)
      'pascal_gen_entry_point (pl1)' : genentrypoint ;
      'pascal_gen_bin_area (pl1)' : genbinarea ;
      'pascal_gen_rel_$text (pl1)' : genreltext ;
                                                  (* IMPORTED VARIABLES *)
      'RACINE (pascal)' :
        alfaptr,
        bufval,
        codelist,
        currentnode,
        declarationpart,
        environt,
        envstandard,
        errtotal,
        fastoperator,
        init_fsb_trap_flag,
        init_fsb_trap_info_place,
        init_fsb_trap_links_place,
        init_fsb_trap_number_of_files,
        interactive,
        iowarnings,
        level,
        longprofile,
        longstring,
        mapswitch,
        mpcogout,
        profilewordcount,
        profptr,
        progname,
        selectivetable,
        statnbr,
        textfilectp,
        version ;
      'STATE (pascal)' :
        asscheck,
        errorctp,
        inputctp,
        linktomain,
        linktomainplace,
        outputctp,
        tmax ;
      'DECLARE (pascal)' :
        lkc,
        nextalf,
        symbtabl ;
      'pascal_context_ (alm)' :
        asciiformataddr,
        nilformataddr,
        octalformataddr,
        realformataddr,
        usednamesaddr$

    $EXPORT
      cb,
      closefile,
      codesymb,
      enterreftosymbol,
      exitlabel,
      fichinter,
      genalfa,
      genc,
      gencodfonct,
      gendesca,
      gendescb,
      geneism,
      genetrace,
      geninsertion,
      genlongprofileref,
      genmulticsnil,
      genpgexit,
      genprcentry,
      genprcexit,
      genprofileref,
      genprolog,
      genr,
      genstand,
      genstring,
      gen_init_fsb_trap_structures,
      ic,
      illegal_generation,
      indfich,
      infich,
      initgen,
      initiozone,
      inser,
      longint,
      mainloc,
      mfari1,
      mfari2,
      mfari3,
      mfreg1,
      mfreg2,
      mfreg3,
      outcode,
      tagsymb,
      usednameaddr,
      writecode,
      writout $




$OPTIONS page $

$INCLUDE 'CONSTTYPE' $



$OPTIONS page $

    VAR
                                                  (* IMPORTED VARIABLES *)

(* FROM RACINE *)
      alfaptr : ctp ;
      bufval : ARRAY [1..maxval] OF char ;
      codelist : boolean ;
      currentnode : blocknodeptr ;
      declarationpart : boolean ;
      environt : contexte ;
      envstandard : stdkind ;
      errtotal : integer ;
      fastoperator : boolean ;
      init_fsb_trap_flag : boolean ;
      init_fsb_trap_info_place,
      init_fsb_trap_links_place,
      init_fsb_trap_number_of_files : integer ;

      interactive : boolean ;
      iowarnings : boolean ;
      level : levrange ; mpcogout : text ;
      longprofile : boolean ;
      longstring : integer ;
      mapswitch : boolean ;
      profilewordcount : integer ;
      profptr : profareaptr ;
      progname : alfaid ;
      selectivetable : boolean ;
      statnbr : integer ;
      textfilectp : ctp ;
      version : integer ;

(* FROM STATE *)
      asscheck : boolean ;
      errorctp : ctp ;
      inputctp : ctp ;
      linktomain : boolean ;
      linktomainplace : integer ;
      outputctp : ctp ;
      tmax : integer ;

(* FROM DECLARE *)
      lkc : integer ;
      nextalf : ctp ;
      symbtabl : boolean ;

(* FROM ALM *)
      realformataddr,
      nilformataddr,
      asciiformataddr,
      octalformataddr : ctp ;
      usednamesaddr : usednamesptr ;

(* EXPORTABLE VARIABLES *)

      cb : integer ;                              (* GIVES THE RELATIVE ADDRESS  *)
                                                  (* IN THE CURRENT PROCEDURE *)
      codesymb : ARRAY [instword] OF alfa ;       (* MNEMONICS OF ALM INSTRUCTIONS *)
      fichinter : ^binartype ;
                                                  (* CONTAINS THE CODE AND DATA GENERATED *)
      genetrace : levtrace ;                      (* TO KNOW IF A TRACE IS DONE ON GENERATION *)
      ic : integer ;                              (* GIVES THE ABSOLUTE ADDRESS IN TEXT SECTION *)
      illegal_generation : boolean ;              (* TRUE IF ILLEGAL INSTRUCTION GENERATION CALLED *)
      indfich : integer ;                         (* GIVES THE FIRST FREE ENTRY IN FICHINTER *)
      mainloc : integer ;                         (* LOCATION (WORDS) OF FIRST INSTRUCTION OF MAIN *)
      mfari1, mfari2, mfari3 : zari ;             (* BITS 0,1,2 IN MF'S EIS *)
      mfreg1, mfreg2, mfreg3 : mreg ;             (* REG. MOD. IN MF'S EIS *)
      outcode : boolean ;                         (* TO KNOW IF ALM GENERATED CODE MUST BE PRINTED *)
      tagsymb : ARRAY [tag] OF PACKED ARRAY [1..4] OF char ; (* MNEM. FOR TAGS *)
      usednameaddr : ctp ;                        (* PTR TO USED NAME IF ANY (FOR "-list" OPTION) *)
      writecode : boolean ;                       (* TO KNOW IF OPTION 'C' IS '+' *)

(* LOCAL VARIABLES *)

      gversion : integer ;                        (* VERSION OF GENERE *)
$OPTIONS compile = security $
      forbiset : ARRAY [instword] OF forset ;     (* GIVES FORBIDDEN PTR FIELD FOR *)
                                                  (* IEISM, FORBIDDEN TAG FOR ISTAND  *)
$OPTIONS compile = true $

(* BITS MASKS FOR INSTRUCTION CODING. ALL MASKS ARE ON HALF-WORD. *)
      codebin : ARRAY [instword] OF integer ;     (* OPERATION CODE *)
      valari : ARRAY [zari] OF integer ;          (* ARI FIELD FOR EIS MF'S *)
      valcar : ARRAY [lgcar] OF integer ;         (* CHARACTER DATA TYPE *)
      valpos : ARRAY [lgcar] OF integer ;         (* GIVES THE MULTIPLICATOR TO CODE *)
                                                  (* THE CN FIELD IN ALPHANUMERIC AND NUMERIC *)
                                                  (* OPERAND  DESCRIPTOR *)
      valptr : ARRAY [zptr] OF integer ;          (* PTR FIELDS FOR EIS MULTIWORD *)
      valreg : ARRAY [preg] OF integer ;          (* POINTER REGISTER *)

{
  AAVALSIG   : ARRAY[TYPSIG]     OF INTEGER; (* SIGN AND DECIMAL TYPE *)
  AAVALABC   : ARRAY[ZABC]       OF INTEGER; (* ABC FIELD FOR REPEAT INST. *)


  }
      prsymb : ARRAY [preg] OF PACKED ARRAY [1..4] OF char ; (* MNEMONICS FOR P. REG. *)
      charsize : ARRAY [lgcar] OF PACKED ARRAY [1..4] OF char ;
                                                  (* USED TO CODE MNEMONICS OF ALPHANUMERIC *)
                                                  (* OPERAND DESCRIPTORS *)
$OPTIONS compile = security $
      forbitag : ARRAY [forset] OF SET OF tag ;   (* FORBIDDEN TAGS FOR ISTAND : *)
                                                  (* S0  -> NONE *)
                                                  (* S1  -> DU,DL *)
                                                  (* S2  -> CI,SC,SCR *)
                                                  (* S3  -> DU,DL,CI,SC,SCR *)
                                                  (* S4  -> ALL EXCEPT AU,QU,AL,QL,X0..X7 *)
                                                  (* S5  -> ALL *)
      forbiptr : ARRAY [forset] OF SET OF zptr ;  (* FORBIDDEN PTR FIELDS (EISM) : *)
                                                  (* S0 -> ALL *)
                                                  (* S1 -> T AND R MUST BE ZERO *)
                                                  (* S2 -> P AND R MUST BE ZERO *)
                                                  (* S3 -> R MUST BE ZERO *)
                                                  (* S4 -> NONE *)
$OPTIONS compile = true $


$OPTIONS page $

    $VALUE

      codesymb = (
        'eaa     ', 'eaq     ', 'eax0    ', 'eax1    ', 'eax2    ', 'eax3    ', 'eax4    ',
        'eax5    ', 'eax6    ', 'eax7    ', 'lca     ', 'lcaq    ', 'lcq     ', 'lcx0    ',
        'lcx1    ', 'lcx2    ', 'lcx3    ', 'lcx4    ', 'lcx5    ', 'lcx6    ', 'lcx7    ',
        'lda     ', 'ldac    ', 'ldaq    ', 'ldi     ', 'ldq     ', 'ldqc    ', 'ldx0    ',
        'ldx1    ', 'ldx2    ', 'ldx3    ', 'ldx4    ', 'ldx5    ', 'ldx6    ', 'ldx7    ',
        'lreg    ', 'lxl0    ', 'lxl1    ', 'lxl2    ', 'lxl3    ', 'lxl4    ', 'lxl5    ',
        'lxl6    ', 'lxl7    ', 'sreg    ', 'sta     ', 'stac    ', 'stacq   ', 'staq    ',
        'stc1    ', 'stc2    ', 'stcd    ', 'sti     ', 'stq     ', 'stt     ', 'stx0    ',
        'stx1    ', 'stx2    ', 'stx3    ', 'stx4    ', 'stx5    ', 'stx6    ', 'stx7    ',
        'stz     ', 'sxl0    ', 'sxl1    ', 'sxl2    ', 'sxl3    ', 'sxl4    ', 'sxl5    ',
        'sxl6    ', 'sxl7    ', 'alr     ', 'als     ', 'arl     ', 'ars     ', 'llr     ',
        'lls     ', 'lrl     ', 'lrs     ', 'qlr     ', 'qls     ', 'qrl     ', 'qrs     ',
        'ada     ', 'adaq    ', 'adl     ', 'adla    ', 'adlaq   ', 'adlq    ', 'adlx0   ',
        'adlx1   ', 'adlx2   ', 'adlx3   ', 'adlx4   ', 'adlx5   ', 'adlx6   ', 'adlx7   ',
        'adq     ', 'adx0    ', 'adx1    ', 'adx2    ', 'adx3    ', 'adx4    ', 'adx5    ',
        'adx6    ', 'adx7    ', 'aos     ', 'asa     ', 'asq     ', 'asx0    ', 'asx1    ',
        'asx2    ', 'asx3    ', 'asx4    ', 'asx5    ', 'asx6    ', 'asx7    ', 'awca    ',
        'awcq    ', 'sba     ', 'sbaq    ', 'sbla    ', 'sblaq   ', 'sblq    ', 'sblx0   ',
        'sblx1   ', 'sblx2   ', 'sblx3   ', 'sblx4   ', 'sblx5   ', 'sblx6   ', 'sblx7   ',
        'sbq     ', 'sbx0    ', 'sbx1    ', 'sbx2    ', 'sbx3    ', 'sbx4    ', 'sbx5    ',
        'sbx6    ', 'sbx7    ', 'ssa     ', 'ssq     ', 'ssx0    ', 'ssx1    ', 'ssx2    ',
        'ssx3    ', 'ssx4    ', 'ssx5    ', 'ssx6    ', 'ssx7    ', 'swca    ', 'swcq    ',
        'mpf     ', 'mpy     ', 'div     ', 'divf    ', 'neg     ', 'negl    ', 'cmg     ',
        'cmk     ', 'cmpa    ', 'cmpaq   ', 'cmpq    ', 'cmpx0   ', 'cmpx1   ', 'cmpx2   ',
        'cmpx3   ', 'cmpx4   ', 'cmpx5   ', 'cmpx6   ', 'cmpx7   ', 'cwl     ', 'szn     ',
        'sznc    ',
                                                  (* ********************************** *)
        'dfld    ', 'fld     ', 'dfst    ', 'dfstr   ', 'fst     ', 'fstr    ', 'dfad    ',
        'dufa    ', 'fad     ', 'ufa     ', 'dfsb    ', 'dufs    ', 'fsb     ', 'ufs     ',
        'dfmp    ', 'dufm    ', 'fmp     ', 'ufm     ', 'dfdi    ', 'dfdv    ', 'fdi     ',
        'fdv     ', 'fneg    ', 'fno     ', 'dfrd    ', 'frd     ', 'dfcmg   ', 'dfcmp   ',
        'fcmg    ', 'fcmp    ', 'ade     ', 'fszn    ', 'lde     ', 'ste     ',
                                                  (* ********************************** *)
        'ana     ', 'anaq    ', 'anq     ', 'ansa    ', 'ansq    ', 'ansx0   ', 'ansx1   ',
        'ansx2   ', 'ansx3   ', 'ansx4   ', 'ansx5   ', 'ansx6   ', 'ansx7   ', 'anx0    ',
        'anx1    ', 'anx2    ', 'anx3    ', 'anx4    ', 'anx5    ', 'anx6    ', 'anx7    ',
        'ora     ', 'oraq    ', 'orq     ', 'orsa    ', 'orsq    ', 'orsx0   ', 'orsx1   ',
        'orsx2   ', 'orsx3   ', 'orsx4   ', 'orsx5   ', 'orsx6   ', 'orsx7   ', 'orx0    ',
        'orx1    ', 'orx2    ', 'orx3    ', 'orx4    ', 'orx5    ', 'orx6    ', 'orx7    ',
        'era     ', 'eraq    ', 'erq     ', 'ersa    ', 'ersq    ', 'ersx0   ', 'ersx1   ',
        'ersx2   ', 'ersx3   ', 'ersx4   ', 'ersx5   ', 'ersx6   ', 'ersx7   ', 'erx0    ',
        'erx1    ', 'erx2    ', 'erx3    ', 'erx4    ', 'erx5    ', 'erx6    ', 'erx7    ',
        'cana    ', 'canaq   ', 'canq    ', 'canx0   ', 'canx1   ', 'canx2   ', 'canx3   ',
        'canx4   ', 'canx5   ', 'canx6   ', 'canx7   ', 'cnaa    ', 'cnaaq   ', 'cnaq    ',
        'cnax0   ', 'cnax1   ', 'cnax2   ', 'cnax3   ', 'cnax4   ', 'cnax5   ', 'cnax6   ',
        'cnax7   ',
                                                  (* ********************************** *)
        'easp0   ', 'easp1   ', 'easp2   ', 'easp3   ', 'easp4   ', 'easp5   ', 'easp6   ',
        'easp7   ', 'eawp0   ', 'eawp1   ', 'eawp2   ', 'eawp3   ', 'eawp4   ', 'eawp5   ',
        'eawp6   ', 'eawp7   ', 'epbp0   ', 'epbp1   ', 'epbp2   ', 'epbp3   ', 'epbp4   ',
        'epbp5   ', 'epbp6   ', 'epbp7   ', 'epp0    ', 'epp1    ', 'epp2    ', 'epp3    ',
        'epp4    ', 'epp5    ', 'epp6    ', 'epp7    ', 'lpri    ', 'lprp0   ', 'lprp1   ',
        'lprp2   ', 'lprp3   ', 'lprp4   ', 'lprp5   ', 'lprp6   ', 'lprp7   ', 'spbp0   ',
        'spbp1   ', 'spbp2   ', 'spbp3   ', 'spbp4   ', 'spbp5   ', 'spbp6   ', 'spbp7   ',
        'spri    ', 'spri0   ', 'spri1   ', 'spri2   ', 'spri3   ', 'spri4   ', 'spri5   ',
        'spri6   ', 'spri7   ', 'sprp0   ', 'sprp1   ', 'sprp2   ', 'sprp3   ', 'sprp4   ',
        'sprp5   ', 'sprp6   ', 'sprp7   ', 'adwp0   ', 'adwp1   ', 'adwp2   ', 'adwp3   ',
        'adwp4   ', 'adwp5   ', 'adwp6   ', 'adwp7   ', 'epaq    ',
                                                  (* ********************************** *)
        'call6   ', 'ret     ', 'rtcd    ', 'teo     ', 'teu     ', 'tmi     ', 'tmoz    ',
        'tnc     ', 'tnz     ', 'tov     ', 'tpl     ', 'tpnz    ', 'tra     ', 'trc     ',
        'trtf    ', 'trtn    ', 'tsp0    ', 'tsp1    ', 'tsp2    ', 'tsp3    ', 'tsp4    ',
        'tsp5    ', 'tsp6    ', 'tsp7    ', 'tss     ', 'tsx0    ', 'tsx1    ', 'tsx2    ',
        'tsx3    ', 'tsx4    ', 'tsx5    ', 'tsx6    ', 'tsx7    ', 'ttf     ', 'ttn     ',
        'tze     ',
                                                  (* ********************************** *)
        'rccl    ', 'drl     ', 'xec     ', 'xed     ', 'mme     ', 'mme2    ', 'mme3    ',
        'mme4    ', 'nop     ', 'puls1   ', 'puls2   ', 'sra     ', 'sbar    ', 'bcd     ',
        'gtb     ',
                                                  (* ********************************** *)
        'lbar    ', 'lcpr    ', 'ldbr    ', 'ldt     ', 'lptp    ', 'lptr    ', 'lra     ',
        'lsdp    ', 'lsdr    ', 'rcu     ', 'scpr    ', 'scu     ', 'sdbr    ', 'sptp    ',
        'sptr    ', 'ssdp    ', 'ssdr    ', 'camp    ', 'cams    ', 'rmcm    ', 'rscr    ',
        'rsw     ', 'cioc    ', 'smcm    ', 'smic    ', 'sscr    ', 'absa    ', 'dis     ',
                                                  (* ********************************** *)
        'aar0    ', 'aar1    ', 'aar2    ', 'aar3    ', 'aar4    ', 'aar5    ', 'aar6    ',
        'aar7    ', 'lar0    ', 'lar1    ', 'lar2    ', 'lar3    ', 'lar4    ', 'lar5    ',
        'lar6    ', 'lar7    ', 'lareg   ', 'lpl     ', 'nar0    ', 'nar1    ', 'nar2    ',
        'nar3    ', 'nar4    ', 'nar5    ', 'nar6    ', 'nar7    ', 'ara0    ', 'ara1    ',
        'ara2    ', 'ara3    ', 'ara4    ', 'ara5    ', 'ara6    ', 'ara7    ', 'arn0    ',
        'arn1    ', 'arn2    ', 'arn3    ', 'arn4    ', 'arn5    ', 'arn6    ', 'arn7    ',
        'sar0    ', 'sar1    ', 'sar2    ', 'sar3    ', 'sar4    ', 'sar5    ', 'sar6    ',
        'sar7    ', 'sareg   ', 'spl     ', 'a4bd    ', 'a6bd    ', 'a9bd    ', 'abd     ',
        'awd     ', 's4bd    ', 's6bd    ', 's9bd    ', 'sbd     ', 'swd     ',
                                                  (* ********************************** *)
        'tct     ', 'tctr    ', 'cmpc    ', 'scm     ', 'scmr    ', 'mlr     ', 'mrl     ',
        'mvt     ', 'csl     ', 'csr     ', 'sztl    ', 'sztr    ', 'scd     ', 'scdr    ',
        'cmpn    ', 'mvn     ', 'cmpb    ', 'btd     ', 'dtb     ', 'ad2d    ', 'sb2d    ',
        'mp2d    ', 'dv2d    ', 'mve     ', 'mvne    ', 'ad3d    ', 'sb3d    ', 'mp3d    ',
        'dv3d    ',
                                                  (* ********************************** *)
        'rpd     ', 'rpl     ', 'rpt     ',
                                                  (* ********************************** *)
        'stba    ', 'stbq    ', 'stca    ', 'stcq    ') ;
      tagsymb = ('n   ', 'au  ', 'qu  ', 'du  ', 'ic  ', 'al  ', 'ql  ', 'dl  ',
        'x0  ', 'x1  ', 'x2  ', 'x3  ', 'x4  ', 'x5  ', 'x6  ', 'x7  ',
        'n*  ', 'au* ', 'qu* ', 'z23 ', 'ic* ', 'al* ', 'ql* ', 'z27 ',
        'x0* ', 'x1* ', 'x2* ', 'x3* ', 'x4* ', 'x5* ', 'x6* ', 'x7* ',
        'f1  ', 'itp ', 'z42 ', 'its ', 'sd  ', 'scr ', 'f2  ', 'f3  ',
        'ci  ', 'i   ', 'sc  ', 'ad  ', 'di  ', 'dic ', 'id  ', 'idc ',
        'z60 ', '*au ', '*qu ', '*du ', '*ic ', '*al ', '*ql ', '*dl ',
        '*x0 ', '*x1 ', '*x2 ', '*x3 ', '*x4 ', '*x5 ', '*x6 ', '*x7 ') ;
$OPTIONS compile = security $
      forbiset = (10 * s1, s0, s3, s0, 8 * s2, s0, 2 * s3, s2, s0, s3, 8 * s2, s3, 8 * s2, s3, s1,
        7 * s3, s1, 9 * s3, s1, 20 * s3, s0, s3, s2, s0, s3, s0, 8 * s2, s0, 8 * s2,
        11 * s3, 3 * s0, s3, s0, s3, (* FIXED *) s0, 8 * s2, s0, 8 * s2, 10 * s3,
        2 * s0, 2 * s2, 7 * s0, s3, s0, 8 * s2, 2 * s0, s3, (* FLOAT *) s3, s2,
        6 * s3, 2 * s2, 2 * s3, 2 * s2, 2 * s3, 2 * s2, 2 * s3, 2 * s2, 4 * s0,
        2 * s3, 5 * s2, s3, s0, s3, s0, 10 * s3, 8 * s2, s0, s3, s0, 10 * s3, 8 * s2,
        s0, s3, s0, 10 * s3, 8 * s2, s0, s3, (* BOOLE *) s0, 8 * s2, s0, s3, s0,
        8 * s2, (* POINT *) 75 * s3, (* TRANS *) 36 * s3, (* MISCE *) s3, s0, 2 * s3,
        7 * s0, 2 * s3, s2, s5, (* PRIVI *) s2, s5, s3, s2, 6 * s3, s5, 10 * s3,
        s0, 5 * s3, s0, (* EISSW *) 52 * s3, 10 * s4, (* EISMW *) 5 * s0, 3 * s2, 4 * s3,
        3 * s0, s4, s3, s1, s0, 4 * s4, 2 * s0, 4 * s4) ;
$OPTIONS compile = true $
      codebin = (
        '33A00'x, '33C00'x, '32000'x, '32200'x, '32400'x, '32600'x, '32800'x, '32A00'x,
        '32C00'x, '32E00'x, '1BA00'x, '1BE00'x, '1BC00'x, '1A000'x, '1A200'x, '1A400'x,
        '1A600'x, '1A800'x, '1AA00'x, '1AC00'x, '1AE00'x, '13A00'x, '03800'x, '13E00'x,
        '33800'x, '13C00'x, '03400'x, '12000'x, '12200'x, '12400'x, '12600'x, '12800'x,
        '12A00'x, '12C00'x, '12E00'x, '07600'x, '3A000'x, '3A200'x, '3A400'x, '3A600'x,
        '3A800'x, '3AA00'x, '3AC00'x, '3AE00'x, '3D600'x, '3DA00'x, '1D800'x, '35800'x,
        '3DE00'x, '2D800'x, '3D000'x, '1DE00'x, '3D800'x, '3DC00'x, '25800'x, '3C000'x,
        '3C200'x, '3C400'x, '3C600'x, '3C800'x, '3CA00'x, '3CC00'x, '3CE00'x, '25000'x,
        '24000'x, '24200'x, '24400'x, '24600'x, '24800'x, '24A00'x, '24C00'x, '24E00'x,
        '3FA00'x, '3BA00'x, '3F200'x, '3B200'x, '3FE00'x, '3BE00'x, '3F600'x, '3B600'x,
        '3FC00'x, '3BC00'x, '3F400'x, '3B400'x, '07A00'x, '07E00'x, '03600'x, '03A00'x,
        '03E00'x, '03C00'x, '02000'x, '02200'x, '02400'x, '02600'x, '02800'x, '02A00'x,
        '02C00'x, '02E00'x, '07C00'x, '06000'x, '06200'x, '06400'x, '06600'x, '06800'x,
        '06A00'x, '06C00'x, '06E00'x, '05800'x, '05A00'x, '05C00'x, '04000'x, '04200'x,
        '04400'x, '04600'x, '04800'x, '04A00'x, '04C00'x, '04E00'x, '07200'x, '07400'x,
        '0FA00'x, '0FE00'x, '0BA00'x, '0BE00'x, '0BC00'x, '0A000'x, '0A200'x, '0A400'x,
        '0A600'x, '0A800'x, '0AA00'x, '0AC00'x, '0AE00'x, '0FC00'x, '0E000'x, '0E200'x,
        '0E400'x, '0E600'x, '0E800'x, '0EA00'x, '0EC00'x, '0EE00'x, '0DA00'x, '0DC00'x,
        '0C000'x, '0C200'x, '0C400'x, '0C600'x, '0C800'x, '0CA00'x, '0CC00'x, '0CE00'x,
        '0F200'x, '0F400'x, '20200'x, '20400'x, '28C00'x, '28E00'x, '2B200'x, '2B600'x,
        '20A00'x, '11200'x, '09A00'x, '09E00'x, '09C00'x, '08000'x, '08200'x, '08400'x,
        '08600'x, '08800'x, '08A00'x, '08C00'x, '08E00'x, '09200'x, '13800'x, '11800'x,
        '23600'x, '23200'x, '25E00'x, '27400'x, '25A00'x, '27000'x, '27E00'x, '23E00'x,
        '27A00'x, '23A00'x, '2FE00'x, '2BE00'x, '2FA00'x, '2BA00'x, '26600'x, '22600'x,
        '26200'x, '22200'x, '2AE00'x, '2EE00'x, '2AA00'x, '2EA00'x, '29600'x, '2F600'x,
        '27600'x, '27200'x, '22E00'x, '29E00'x, '22A00'x, '29A00'x, '21A00'x, '23000'x,
        '21200'x, '25C00'x, '1FA00'x, '1FE00'x, '1FC00'x, '1DA00'x, '1DC00'x, '1C000'x,
        '1C200'x, '1C400'x, '1C600'x, '1C800'x, '1CA00'x, '1CC00'x, '1CE00'x, '1E000'x,
        '1E200'x, '1E400'x, '1E600'x, '1E800'x, '1EA00'x, '1EC00'x, '1EE00'x, '17A00'x,
        '17E00'x, '17C00'x, '15A00'x, '15C00'x, '14000'x, '14200'x, '14400'x, '14600'x,
        '14800'x, '14A00'x, '14C00'x, '14E00'x, '16000'x, '16200'x, '16400'x, '16600'x,
        '16800'x, '16A00'x, '16C00'x, '16E00'x, '37A00'x, '37E00'x, '37C00'x, '35A00'x,
        '35C00'x, '34000'x, '34200'x, '34400'x, '34600'x, '34800'x, '34A00'x, '34C00'x,
        '34E00'x, '36000'x, '36200'x, '36400'x, '36600'x, '36800'x, '36A00'x, '36C00'x,
        '36E00'x, '19A00'x, '19E00'x, '19C00'x, '18000'x, '18200'x, '18400'x, '18600'x,
        '18800'x, '18A00'x, '18C00'x, '18E00'x, '11A00'x, '11E00'x, '11C00'x, '10000'x,
        '10200'x, '10400'x, '10600'x, '10800'x, '10A00'x, '10C00'x, '10E00'x, '19200'x,
        '19100'x, '19600'x, '19500'x, '1B200'x, '1B100'x, '1B600'x, '1B500'x, '19000'x,
        '19300'x, '19400'x, '19700'x, '1B000'x, '1B300'x, '1B400'x, '1B700'x, '1D100'x,
        '1D200'x, '1D500'x, '1D600'x, '1F100'x, '1F200'x, '1F500'x, '1F600'x, '1D000'x,
        '1D300'x, '1D400'x, '1D700'x, '1F000'x, '1F300'x, '1F400'x, '1F700'x, '0F600'x,
        '3E000'x, '3E200'x, '3E400'x, '3E600'x, '3E800'x, '3EA00'x, '3EC00'x, '3EE00'x,
        '15100'x, '15200'x, '15500'x, '15600'x, '35100'x, '35200'x, '35500'x, '35600'x,
        '15800'x, '15000'x, '15300'x, '15400'x, '15700'x, '35000'x, '35300'x, '35400'x,
        '35700'x, '2C000'x, '2C200'x, '2C400'x, '2C600'x, '2C800'x, '2CA00'x, '2CC00'x,
        '2CE00'x, '05000'x, '05200'x, '05400'x, '05600'x, '0D000'x, '0D200'x, '0D400'x,
        '0D600'x, '11600'x, '39600'x, '33000'x, '31000'x, '31800'x, '31A00'x, '30800'x,
        '30900'x, '30400'x, '30200'x, '31E00'x, '30A00'x, '30B00'x, '39000'x, '30600'x,
        '30300'x, '30100'x, '17000'x, '17200'x, '17400'x, '17600'x, '37000'x, '37200'x,
        '37400'x, '37600'x, '39A00'x, '38000'x, '38200'x, '38400'x, '38600'x, '38800'x,
        '38A00'x, '38C00'x, '38E00'x, '30E00'x, '30D00'x, '30000'x, '33600'x, '00400'x,
        '39C00'x, '39E00'x, '00200'x, '00800'x, '00A00'x, '00E00'x, '01200'x, '01400'x,
        '01600'x, '3D900'x, '2D000'x, '28A00'x, '3F800'x, '13000'x, '37800'x, '13400'x,
        '33E00'x, '15F00'x, '0F700'x, '3F900'x, '15E00'x, '13500'x, '31600'x, '25400'x,
        '35E00'x, '0D800'x, '2DF00'x, '0D900'x, '2DE00'x, '15900'x, '2B500'x, '2B400'x,
        '13600'x, '21600'x, '13200'x, '01A00'x, '2D600'x, '25200'x, '05E00'x, '11400'x,
        '31C00'x, '2E100'x, '2E300'x, '2E500'x, '2E700'x, '2E900'x, '2EB00'x, '2ED00'x,
        '2EF00'x, '3E100'x, '3E300'x, '3E500'x, '3E700'x, '3E900'x, '3EB00'x, '3ED00'x,
        '3EF00'x, '26700'x, '26F00'x, '36100'x, '36300'x, '36500'x, '36700'x, '36900'x,
        '36B00'x, '36D00'x, '36F00'x, '2C100'x, '2C300'x, '2C500'x, '2C700'x, '2C900'x,
        '2CB00'x, '2CD00'x, '2CF00'x, '34100'x, '34300'x, '34500'x, '34700'x, '34900'x,
        '34B00'x, '34D00'x, '34F00'x, '3C100'x, '3C300'x, '3C500'x, '3C700'x, '3C900'x,
        '3CB00'x, '3CD00'x, '3CF00'x, '24700'x, '24F00'x, '28500'x, '28300'x, '28100'x,
        '28700'x, '28F00'x, '2A500'x, '2A300'x, '2A100'x, '2A700'x, '2AF00'x, '0E900'x,
        '0EB00'x, '08D00'x, '0A900'x, '0AB00'x, '08100'x, '08300'x, '0E100'x, '06100'x,
        '06300'x, '06900'x, '06B00'x, '0A100'x, '0A300'x, '18700'x, '18100'x, '06D00'x,
        '18300'x, '18B00'x, '10500'x, '10700'x, '10D00'x, '10F00'x, '02100'x, '02900'x,
        '12500'x, '12700'x, '12D00'x, '12F00'x, '2E000'x, '28000'x, '2A000'x, '2D200'x,
        '2D400'x, '3D200'x, '3D400'x) ;
      valari = (000, 016, 032, 048, 064, 080, 096, 112) ;
      valcar = (16384, 08192, 00000) ;
      valpos = (32768, 32768, 65536) ;
      valptr = (000000, 000128, 000256, 000384,
        131072, 131200, 131328, 131456) ;
      valreg = (000000, 032768, 065536, 163840, 229376,
        098304, 000000, 131072, 131072, 196608) ;

{
  VALSIG = (  00000 , 04096 , 08192 , 12288 ) ;
  VALABC = ( 0000 , 0128 , 0512 , 0640 , 0256 , 0384 , 0768 , 0896 );


  }
      prsymb = ('    ', 'pr1|', 'pr2|', 'pr5|', 'pr7|', 'pr3|', 'pr0|', 'pr4|', 'pr4|', 'pr6|') ;
      charsize = ('4a  ', '6a  ', '9a  ') $


$OPTIONS page $

(* IMPORTED PROCEDURES FROM RACINE *)

    PROCEDURE crealfabox (VAR fkonstbox : ctp) ; EXTERNAL ;
    PROCEDURE error (errno : integer) ; EXTERNAL ;
    PROCEDURE nextline ; EXTERNAL ;
    FUNCTION recadre (fval, fmod : integer) : integer ; EXTERNAL ;
    PROCEDURE statement_begins (genp : boolean) ; EXTERNAL ;
    PROCEDURE statement_ends (sttlength : integer) ; EXTERNAL ;

(* IMPORTED PROCEDURES  FROM STATE *)
    PROCEDURE enterundlab (VAR fundix : integer) ; EXTERNAL ;
    PROCEDURE gencstecode (farg : integer ; finst : istand) ; EXTERNAL ;
                                                  (* IMPORTED PROCEDURES FROM CONTEXTTABLE *)

    PROCEDURE create_konst_box (VAR fvbox : ctp ; fname : alfaid ; ftypofconst : consttype) ; EXTERNAL ;

(* IMPORTED FROM PL1 *)

    PROCEDURE genreltext (relcode : integer ; halfwordcount : integer) ; EXTERNAL ;


$OPTIONS page $


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


(* **************************************************** INITGEN  ************** *)

    PROCEDURE initgen ;

(* C   INITIALIZES GENERATION DEPENDANT VARIABLES                             C *)
      BEGIN
$OPTIONS compile = security $
        forbitag [s0] := [] ;
        forbitag [s1] := [tdu, tdl] ;
        forbitag [s2] := [tci, tsc, tscr] ;
        forbitag [s3] := [tdu, tdl, tci, tsc, tscr] ;
        forbitag [s4] := [tdu, tic, tdl, tny..tyx7] ;
        forbitag [s5] := [tau..tyx7] ;
        forbiptr [s0] := [p0t0r1..p1t1r1] ;
        forbiptr [s1] := [p0t0r1, p0t1r0, p0t1r1, p1t0r1, p1t1r0, p1t1r1] ;
        forbiptr [s2] := [p0t0r1, p0t1r1..p1t1r1] ;
        forbiptr [s3] := [p0t0r1, p0t1r1, p1t0r1, p1t1r1] ;
        forbiptr [s4] := [] ;
        forbiptr [s5] := [] ;
$OPTIONS compile = true $
        gversion := 00 ;
        IF gversion > version THEN version := gversion ;
        indfich := 1 ;
        ic := 0 ;
        genetrace := none ;
        mfreg1 := tn ; mfreg2 := tn ; mfreg3 := tn ; (* MOST COMMON USED VALUES *)
        usednameaddr := NIL ;
        writecode := false ;                      (* TRUE IF 'C' IS '+' *)
        outcode := false ;                        (* OUTPUT OF SYMBOLIQUE ALM CODE *)
      END (* INITGEN *) ;


$OPTIONS page $

(* *********************************************************FCT LONGINT******** *)

    FUNCTION longint (fint : integer) : integer ;

(* C GIVES THE NUMBER OF DIGITS OF AN INTEGER                                 C *)
      VAR
        it : integer ;
      BEGIN
        IF fint < 0 THEN
	BEGIN
	  fint := -fint ; it := 1 ;
	END ELSE
	it := 0 ;
        IF fint < 10 THEN it := it + 1 ELSE
	BEGIN                                   (* MORE THAN ONE DIGIT *)
	  WHILE fint # 0 DO
	    BEGIN
	      fint := fint DIV 10 ;
	      it := it + 1 ;
	    END ;
	END ;
        longint := it ;
      END (* LONGINT *) ;



$OPTIONS page $

(* ***********************************************WRITEOCTAL ********* *)

    PROCEDURE writeoctal (fint : integer) ;

(* C   WRITES FINT IN OCTAL ON 6 CHARACTERS                                   C *)
      VAR
        bufoct : PACKED ARRAY [1..6] OF char ;
        it : integer ;
      BEGIN
        FOR it := 6 DOWNTO 1 DO
	BEGIN
	  bufoct [it] := chr (fint MOD 8 + ord ('0')) ;
	  fint := fint DIV 8 ;
	END ;
        write (mpcogout, bufoct) ;
      END (* WRITEOCTAL *) ;


$OPTIONS page $

(* *********************************************************GENHALF************ *)

    PROCEDURE genhalf (fval : integer) ;


      BEGIN                                       (* GENHALF *)
        IF fval < 0 THEN
	fval := fval + twoto18 ;                (* TWO'S COMPLEMENT *)
        IF environt = code THEN
	BEGIN
	  IF outcode THEN
	    BEGIN
	      IF (ic MOD bytesinword) = 0 THEN
	        BEGIN
		write (mpcogout, ' ' : 55) ;
		writeoctal (ic DIV bytesinword) ;
		write (mpcogout, '   ') ;
	        END ;                           (* FIRST HALF WORD *)
	      writeoctal (fval) ;
	    END ;                               (* OUTCODE *)
	  IF codelist THEN
	    IF (ic MOD bytesinword) = 0 THEN
	      usednamesaddr@ [ic DIV bytesinword] := usednameaddr ;
	  usednameaddr := NIL ;
	  ic := ic + 2 ; cb := cb + 2 ;
	END ;                                   (* CODE *)
        IF indfich > maxfich THEN
	BEGIN
	  indfich := 1 ; error (253) ;
	END ;
        fichinter^[indfich] := fval ;
        indfich := indfich + 1 ;
      END (* GENHALF *) ;


$OPTIONS page $

(* ***********************************************INFICH ********************** *)

    PROCEDURE infich (fval : integer) ;

(* C   THIS PROCEDURE IS USED TO ADD AN HALF WORD TO FICHINTER VIA GENHALF.
   IT MUST BE USED TO GENERE EVERYTHING OTHERWISE BY THE ALM GENERATION
   PROCEDURES                                                             C *)
      BEGIN
        genhalf (fval) ;
        IF outcode THEN
	IF environt = code THEN
	  IF (ic MOD bytesinword) = 0 THEN nextline ;
      END (* INFICH *) ;


$OPTIONS page $

(* *********************************************************GENC*************** *)

    PROCEDURE genc (fval : integer) ;

(* C  USED TO GENERE A WORD CSTE.                                             C *)

      VAR
        word : PACKED RECORD
	CASE boolean OF
	true : (int : integer) ;
	false : (high, low : shrtint) ;
        END ;

      BEGIN
        word.int := fval ;
        infich (word.high) ;
        infich (word.low) ;
      END (* GENC *) ;


$OPTIONS page $

(* *********************************************************GENR*************** *)

    PROCEDURE genr (frval : real) ;

      VAR
        convrec : RECORD
	CASE boolean OF
	false : (reel : real) ;
	true : (left, right : integer) ;
        END ;
      BEGIN                                       (* GENR *)
        WITH convrec DO
	BEGIN
	  reel := frval ;
	  usednameaddr := realformataddr ;
	  genc (left) ; genc (right) ;
	END ;
      END (* GENR *) ;


$OPTIONS page $

(* ****************************************************************** ENTERREFTOSYMBOL ************************* *)

    FUNCTION enterreftosymbol (ctplace : ctp) : integer ;

(* C
   THIS FUNCTION BUILDS THE BACKWARD THREAD OF REFERENCES IN TEXT
   TO SYMBOL TABLE OF ITEM POINTED BY CTPLACE
   THESE REFERENCES WILL BE CORRECTLY FILLED IN PASCAL_CREATE_TABLES
   IF SUCH REFERENCES EXIST, SYMBOL TABLE IS GENERATED
   IF THE OPTION "-TABLE" HAS NOT BEEN GIVEN, SYMBOL TABLE WILL ONLY CONTAIN INFORMATION
   CONCERNING SYMBOLS REFERENCED IN TEXT.                   C *)

      BEGIN
        WITH ctplace^ DO
	BEGIN
	  enterreftosymbol := symbtablerefs ;
	  symbtablerefs := ic DIV bytesinword ;
	END ;
        selectivetable := true ;
      END (* ENTERREFTOSYMBOL *) ;

$OPTIONS page $

(* ****************************    GENMULTICSNIL    ******************* *)

    PROCEDURE genmulticsnil ;

      BEGIN                                       (* GENMULTICSNIL *)
        usednameaddr := nilformataddr ;
        genc ('077777000043'o) ;
        genc ('000001000000'o) ;
      END (* GENMULTICSNIL *) ;

$OPTIONS page $

(* *********************************************************GENSTRING********** *)

    PROCEDURE genstring (falfapt : ctp) ;

(* C   FALFAPT POINTS A BOX (KONST,ALFACONST)                                 C *)
      VAR
        curalf : alfapt ;
        h1, it : integer ;
      BEGIN                                       (* GENSTRING *)
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT-FIN DE GENSTRING @@@') ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        curalf := falfapt@.alfadeb ;
        WHILE curalf # NIL DO
	WITH curalf@ DO
	  BEGIN
	    it := 1 ;
	    WHILE it < longfill DO
	      BEGIN
	        h1 := ord (alfaval [it]) * byteshift + ord (alfaval [it + 1]) ;
	        usednameaddr := asciiformataddr ;
	        infich (h1) ;
	        it := it + 2 ;
	      END (* LOOP ON THE BOX *) ;
	    IF it = longfill THEN               (* ONE MORE CHAR TO GENERATE *)
	      BEGIN
	        usednameaddr := asciiformataddr ;
	        infich (ord (alfaval [it]) * byteshift) ;
	      END ;
	    curalf := nextval ;
	  END ;                                 (* LOOP ON THE BOXES *)
      END (* GENSTRING *) ;


$OPTIONS page $

(* ***********************************************GENALFA********************** *)

    PROCEDURE genalfa ;

(* C  GENERATION OF AN ALFA STRING IN  FICHINTER ;  ALFA STRING  IS  IN
   BUFVAL on LONGSTRING chars.
   If LONGSTRING > MAXVAL it is an error due to padding allowed in
   VALUEDECL.
   C *)
(* E ERRORS DETECTED
   209 Too long string
   E *)

      VAR
        stringpt : integer ;
      BEGIN
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ DEBUT GENALFA @@@ WITH   LONGSTRING ', longstring) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        IF longstring > maxval THEN
	BEGIN
	  error (209) ; longstring := maxval ;
	END ;
        stringpt := 1 ;
        WHILE stringpt < longstring DO
	BEGIN
	  usednameaddr := asciiformataddr ;
	  infich (ord (bufval [stringpt]) * byteshift + ord (bufval [stringpt + 1])) ;
	  stringpt := stringpt + 2 ;
	END ;
        IF stringpt = longstring THEN
                                                  (* ONE MORE CHAR ALONE... *)
	BEGIN
	  usednameaddr := asciiformataddr ;
	  infich (ord (bufval [stringpt]) * byteshift) ;
	END ;
        IF NOT odd (indfich) THEN infich (0) ;    (* PADDING *)
$OPTIONS compile = trace $
        IF genetrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ FIN GENALFA @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* GENALFA *) ;


$OPTIONS page $

(* ********************************************************   BOUNDSCTRL   **** *)

$OPTIONS compile = security $
    PROCEDURE boundsctrl (VAR sfield : integer ; flow, fhigh, fnoerr : integer) ;

(* C   CONTROLS THAT SFIELD IS IN FLOW..FHIGH
   IF NO, SFIELD BECOMES ZERO AND ERROR(FNOERR) IS CALLED.   *)
      BEGIN
        IF (sfield > fhigh) OR (sfield < flow) THEN
	BEGIN
	  sfield := 0 ; error (fnoerr) ;
	END ;
      END (* BOUNDSCTRL *) ;
$OPTIONS compile = true $


$OPTIONS page $

(* ******************************************************   LENGTHCTRL    ***** *)

$OPTIONS compile = security $
    PROCEDURE lengthctrl (VAR flength : integer ; fmax : integer ; freg : mreg) ;

(* C   VERIFICATION OF OPERAND LENGTH IN OPERAND DESCRIPTOR *)
(* E   372  ILLEGAL OPERAND LENGTH(LENGTHCTRL)
   374  ILLEGAL MODIFIER (LENGTHCTRL)        *)
      BEGIN
        IF freg # tn THEN
	BEGIN
	  fmax := 0 ;
	  IF freg = tdl THEN error (374) ;
	END ;
        boundsctrl (flength, 0, fmax, 372) ;
      END (* LENGTHCTRL *) ;
$OPTIONS compile = true $


$OPTIONS page $

(* ********************************************************    GENWITHPR   **** *)

    PROCEDURE genwithpr (fpr : preg ; fadr : integer ; VAR sbit29 : integer) ;

(* C   HALF-WORD GENERATION :
   EITHER  ADDRESS 0-17                RETURNS SBIT29 = 000O
   OR      PREG 0-2  AND ADDRESS 3-17  RETURNS SBIT29 = 100O *)
(* E   358  ILLEGAL ADDRESS FIELD WITHOUT PREG (GENWITHPR)
   359  ILLEGAL ADDRESS FIELD WITH PREG (GENWITHPR)     *)
      BEGIN
        IF fpr = nreg THEN                        (* NO POINTER REGISTER *)
	BEGIN
$OPTIONS compile = security $
	  boundsctrl (fadr, -twoto17, twoto18 - 1, 358) ;
$OPTIONS compile = true $
	  IF fadr < 0 THEN fadr := twoto18 + fadr ; (* TWO'S COMPLEMENT *)
	  genhalf (fadr) ;
	  sbit29 := 0 ;                         (* BIT 29 OFF *)
	END ELSE                                (* USE OF POINTER REGISTER *)
	BEGIN
$OPTIONS compile = security $
	  IF fpr IN [prstatic, prlink] THEN
	    boundsctrl (fadr, -twoto14, twoto14 - 1, 359) ELSE
	    IF fpr = pr6 THEN
	      boundsctrl (fadr, -twoto14, twoto14 - 1, 390) ELSE
	      boundsctrl (fadr, -twoto14, twoto14 - 1, 391) ;
$OPTIONS compile = true $
	  IF fadr < 0 THEN fadr := twoto15 + fadr ; (* TWO'S COMPLEMENT *)
	  genhalf (valreg [fpr] + fadr) ;
	  sbit29 := bit29 ;                     (* BIT 29 ON *)
	END ;
      END (* GENWITHPR *) ;


$OPTIONS page $

(* **********************************************************   GENSTAND    *** *)

    PROCEDURE genstand (fpr : preg ; fadr : integer ; fcode : istand ; ftg : tag) ;

(* C   GENERATION OF A STANDARD INSTRUCTION (NOT EIS MULTIWORD, STORE BYTES AND
   CHARACTERS, REPETITION)   *)
(* E   355 :  ILLEGAL SHIFT COUNT (GENSTAND)
   356 :  ILLEGAL INSTRUCTION CODE WITHOUT POINTER REGISTER (GENSTAND)
   357 :  TAG FIELD IS INCOMPATIBLE WITH INSTRUCTION CODE (GENSTAND)   *)
      LABEL
        1 ;                                       (* IF ILLEGAL CALL *)
      VAR
        lbit29 : integer ;
      BEGIN
$OPTIONS compile = security $
        IF fpr = nreg THEN                        (* NO PR *)
	BEGIN
	  IF (fcode >= iarl) AND (fcode <= iqrs) AND (ftg = tn) THEN
	    boundsctrl (fadr, 0, 127, 355) ;    (* SHIFT WITH COUNT IN FADR *)
	  IF fcode >= ia4bd THEN error (356) ;  (* OPERATION ON AREG WITHOUT AREG *)
	END ;
$OPTIONS compile = true $
        IF declarationpart THEN
	BEGIN
	  illegal_generation := true ;
	  GOTO 1
	END ;
        IF ftg IN [tdu, tdl] THEN BEGIN           (* FIRST HALF WORD *)
	  genhalf (fadr) ;
	  lbit29 := 0
	END
        ELSE genwithpr (fpr, fadr, lbit29) ;
$OPTIONS compile = security $
        IF ftg IN forbitag [forbiset [fcode]] THEN error (357) ; (* ILLEGAL TAG *)
$OPTIONS compile = true $
        genhalf (codebin [fcode] + lbit29 + ord (ftg)) ; (* SECOND HALF-WORD *)
        IF outcode THEN
	BEGIN
	  IF fpr = nreg THEN
	    write (mpcogout, codesymb [fcode] : 12, fadr : longint (fadr))
	  ELSE
	    write (mpcogout, codesymb [fcode] : 12, prsymb [fpr], fadr : longint (fadr)) ;
	  IF ftg # tn THEN write (mpcogout, ',', tagsymb [ftg]) ;
	  nextline ;
	END ;
        IF fpr = prstatic THEN BEGIN
	  genreltext (int15, 1) ;
	  genreltext (absl, 1)
	END
        ELSE IF fpr = prlink THEN BEGIN
	    genreltext (link15, 1) ;
	    genreltext (absl, 1)
	  END
	ELSE genreltext (absl, 2) ;
1 :
      END (* GENSTAND *) ;


$OPTIONS page $

(* **********************************************************  GENEISM    ***** *)

    PROCEDURE geneism (fcode : ieism ; ffield : integer ; fbits : zptr) ;

(* C   GENERATION OF AN EIS MULTIWORD INSTRUCTION. THE MF FIELDS ARE IN THE
   GLOBAL VARIABLES : MFARI1,2,3 AND MFREG1,2,3   *)
(* E   367  TAG FORBIDDEN IN EIS MULTIWORD MF (GENEISM)
   368  BITS 0,9 OR 10 ILLEGAL IN EIS (GENEISM)
   369  ILLEGAL FIELD 0-8 (GENEISM)               *)
      LABEL
        1 ;                                       (* IF ILLEGAL CALL *)
      VAR
        lhalf, o1, o2, o3
$OPTIONS compile = security $, lhigh
$OPTIONS compile = true $
        : integer ;


(* *********************************************************OUTMF < GENEISM**** *)

      PROCEDURE outmf (fari : zari ; freg : mreg) ;

(* C   OUTPUT OF A MODIFICATOR FIELD IN AN EIS MULTIWORD                      C *)
        VAR
	chx : char ;
        BEGIN
	chx := '(' ;
	IF fari >= a1r0i0 THEN
	  BEGIN
	    write (mpcogout, chx, 'pr') ; chx := ',' ;
	  END ;
	IF fari IN [a0r1i0, a0r1i1, a1r1i0, a1r1i1] THEN
	  BEGIN
	    write (mpcogout, chx, 'rl') ; chx := ',' ;
	  END ;
	IF fari IN [a0r0i1, a0r1i1, a1r0i1, a1r1i1] THEN
	  BEGIN
	    write (mpcogout, chx, 'id') ; chx := ',' ;
	  END ;
	IF freg # tn THEN
	  BEGIN
	    write (mpcogout, chx, tagsymb [freg, 1], tagsymb [freg, 2]) ; chx := ',' ;
	    IF tagsymb [freg, 3] # ' ' THEN write (mpcogout, tagsymb [freg, 3]) ;
	  END ;
	IF chx = '(' THEN write (mpcogout, '()') ELSE write (mpcogout, ')') ;
        END (* OUTMF *) ;


      BEGIN                                       (* GENEISM *)
$OPTIONS compile = security $
        IF mfreg1 IN [tdu, tdl] THEN error (367) ;
$OPTIONS compile = true $
        IF fcode <= itctr THEN lhalf := 0 ELSE
	BEGIN                                   (* MORE THAN ONE MF *)
	  IF declarationpart THEN
	    BEGIN
	      illegal_generation := true ;
	      GOTO 1 ;
	    END ;
$OPTIONS compile = security $
	  IF mfreg2 IN [tdu, tdl] THEN error (367) ;
	  IF fbits IN forbiptr [forbiset [fcode]] THEN
	    BEGIN error (368) ; fbits := p0t0r0 END ; (* TO AVOID OVERFLOW *)
$OPTIONS compile = true $
	  lhalf := valari [mfari2] + ord (mfreg2) + valptr [fbits] ;
	  IF fcode < imve THEN
	    BEGIN                               (* TWO MF'S *)
$OPTIONS compile = security $
	      IF fcode <= imvt THEN lhigh := twoto9 - 1 ELSE
	        IF fcode <= isztr THEN lhigh := 15 ELSE lhigh := 0 ;
	      boundsctrl (ffield, 0, lhigh, 369) ;
$OPTIONS compile = true $
	      lhalf := lhalf + ffield * twoto9 ;
	    END (* TWO MF'S *) ELSE
	    BEGIN                               (* THREE MF'S *)
$OPTIONS compile = security $
	      IF mfreg3 IN [tdu, tdl] THEN error (367) ;
$OPTIONS compile = true $
	      lhalf := lhalf + (valari [mfari3] + ord (mfreg3)) * twoto9 ;
	    END (* THREE MF'S *) ;
	END (* MORE THAN ONE MF *) ;
        genhalf (lhalf) ; genhalf (codebin [fcode] + valari [mfari1] + ord (mfreg1)) ;
        IF outcode THEN
	BEGIN
	  write (mpcogout, codesymb [fcode] : 12) ;
	  outmf (mfari1, mfreg1) ;
	  IF lhalf # 0 THEN
	    BEGIN
	      write (mpcogout, ',') ; outmf (mfari2, mfreg2) ;
	      IF fcode >= imvne THEN
	        BEGIN
		write (mpcogout, ',') ; outmf (mfari3, mfreg3) ;
	        END ;
	      IF (fcode < icsl) OR (fcode > itctr) THEN
	        IF fcode # icmpb THEN
		BEGIN
		  IF (fcode = iscm) OR (fcode = iscmr) THEN write (mpcogout, ',MASK(') ELSE
		    IF fcode < icsl THEN write (mpcogout, ',fill(') ELSE
		      write (mpcogout, ',bool(') ;
		  o1 := ffield DIV 64 ;
		  o2 := (ffield - o1 * 64) DIV 8 ;
		  o3 := ffield MOD 8 ;
		  write (mpcogout, o1 : 1, o2 : 1, o3 : 1, ')') ;
		END ;
	      IF fbits IN [p0t1r0, p0t1r1, p1t1r0, p1t1r1] THEN
	        write (mpcogout, ',enablefault') ;
	      IF fbits IN [p0t0r1, p0t1r1, p1t0r1, p1t1r1] THEN write (mpcogout, ',round') ;
	      IF (fcode = icmpb) OR ((fcode <= isztr) AND (fcode >= icsl)) THEN
	        IF fbits >= p1t0r0 THEN write (mpcogout, ',fill(1)')
	        ELSE write (mpcogout, ',fill(0)') ;
	    END ;
	  nextline ;
	END ;
        mfreg1 := tn ; mfreg2 := tn ; mfreg3 := tn ; (* MOST COMMON VALUES *)
        genreltext (absl, 2) ;
1 :
      END (* GENEISM *) ;


$OPTIONS page $

(* *************************************************************   GENREPT  *** *)


{ PROCEDURE GENREPT(FTALLY:INTEGER;FABC:ZABC;FCODE:IREPT;
  FTERCOND,FDELTA : INTEGER);

  CC(*C   GENERATION OF A REPEAT INSTRUCTION *)
  EE(*E   361  ILLEGAL TALLY (GENREPT)
  362  ILLEGAL TERMINATION CONDITION (GENREPT)
  363  ILLEGAL DELTA (GENREPT)
  364  BITS 8,9,10 INCOMPATIBLE WITH INSTRUCTION CODE (GENREPT)  *)
  GGBEGIN
  (/ BOUNDSCTRL(FTALLY,0,255,361); BOUNDSCTRL(FTERCOND,0,127,362) ;
  IF FCODE=IRPL THEN
  BEGIN
  IF (FABC>A0B0C1) OR (FDELTA#0) THEN ERROR(364);
  END    ELSE
  BEGIN
  BOUNDSCTRL(FDELTA,0,63,363);
  IF (FCODE=IRPT) AND (FABC>A1B0C1) THEN ERROR(364);
  END ; /)
  GENHALF(FTALLY*TWOTO10+VALABC[FABC]+FTERCOND);
  GENHALF(CODEBIN[FCODE]+INHIBIT+FDELTA) ;
  IF OUTCODE THEN NEXTLINE;
  DDEND (* GENREPT *) ; }


$OPTIONS page $

(* ***********************************************************  GENSTOBC    *** *)

{ PROCEDURE GENSTOBC(FPR:PREG;FADR:INTEGER;FCODE:ISTOBC;FPOS:INTEGER);

  CC(*C   GENERATION OF A STORE BYTES OR CHARACTERS INSTRUCTION
  FPOS GIVES THE BYTES OR CHARACTERS TO BE STORED       *)
  EE(*E   360  ILLEGAL TAG FIELD FOR BYTES'POSITION (GENSTOBC) *)
  RRVAR
  LBIT29 : INTEGER ;
  GGBEGIN
  GENWITHPR(FPR,FADR,LBIT29);
  (/ BOUNDSCTRL(FPOS,0,63,360);
  IF (FCODE < ISTCA) AND ((FPOS MOD 4) # 0) THEN ERROR(360); /)
  GENHALF(CODEBIN[FCODE]+LBIT29+FPOS);
  IF OUTCODE THEN
  BEGIN
  WRITE(MPCOGOUT,CODESYMB[FCODE]:12,PRSYMB[FPR],FADR:LONGINT(FADR),',O');
  WRITE(MPCOGOUT,(FPOS DIV 8):1,(FPOS MOD 8):1);
  NEXTLINE;
  END;
  DDEND (* GENSTOBC *); }


$OPTIONS page $

(* ************************************************************  GENDESCA   *** *)

    PROCEDURE gendesca (fareg : preg ; fadr, fcn : integer ; fta : lgcar ; fn : integer ;
      frlgth : mreg) ;

(* C   GENERATION OF AN ALPHANUMERIC OPERAND DESCRIPTOR   *)
(* E   373  ILLEGAL CHARACTERS COUNT (GENDESC) *)
      VAR
        ldummy
$OPTIONS compile = security $, lhigh
$OPTIONS compile = true $
        : integer ;
      BEGIN
        genwithpr (fareg, fadr, ldummy) ;
$OPTIONS compile = security $
        lengthctrl (fn, twoto12 - 1, frlgth) ;    (* OPERAND LENGTH *)
        CASE fta OF
	l9 : lhigh := 3 ;
	l6 : lhigh := 5 ;
	l4 : lhigh := 7 ;
        END (* CASE *) ; boundsctrl (fcn, 0, lhigh, 373) ;
$OPTIONS compile = true $
        genhalf (fcn * valpos [fta] + valcar [fta] + fn + ord (frlgth)) ;
        IF outcode THEN
	BEGIN
	  IF fareg = nreg THEN
	    write (mpcogout, 'desc' : 8, charsize [fta], fadr : longint (fadr))
	  ELSE
	    write (mpcogout, 'desc' : 8, charsize [fta], prsymb [fareg], fadr : longint (fadr)) ;
	  IF fcn # 0 THEN write (mpcogout, '(', fcn : longint (fcn), ')') ;
	  IF frlgth = tn THEN write (mpcogout, ',', fn : longint (fn)) ELSE
	    write (mpcogout, ',', tagsymb [frlgth]) ;
	  nextline ;
	END ;
        IF fareg = prstatic THEN BEGIN
	  genreltext (int15, 1) ;
	  genreltext (absl, 1)
	END
        ELSE IF fareg = prlink THEN BEGIN
	    genreltext (link15, 1) ;
	    genreltext (absl, 1)
	  END
	ELSE genreltext (absl, 2) ;
      END (* GENDESCA *) ;


$OPTIONS page $

(* ***********************************************************  GENDESCN    *** *)

{ PROCEDURE GENDESCN(FAREG:PREG;FADR,FCN:INTEGER;FTN:LGCAR;FS:TYPSIG;
  FSF,FN:INTEGER;FRLGTH:MREG);

  CC(*C   GENERATION OF A NUMERIC OPERAND DESCRIPTOR *)
  EE(*E   373  ILLEGAL CHARACTERS COUNT (GENDESC)
  376  ILLEGAL SCALING FACTOR(GENDESCN)    *)
  RRVAR
  LDUMMY (/,LHIGH/) : INTEGER;
  GGBEGIN
  GENWITHPR(FAREG,FADR,LDUMMY);
  (/ LENGTHCTRL(FN,63,FRLGTH); (* OPERAND LENGTH *)
  BOUNDSCTRL(FSF,-32,31,376);
  CASE FTN OF
  L9 : LHIGH:=3 ;
  L4 : LHIGH:=7 ;
  L6 : BEGIN
  LHIGH := -1; (* TO FORCE AN ERROR *)
  FTN := L9; (* TO HAVE A RELEVANT FIELD => NO OVERFLOW *)
  END ; (* L6 *)
  END (* CASE *);
  BOUNDSCTRL(FCN,0,LHIGH,373); /)
  IF FSF<0 THEN FSF:=64+FSF; (* TWO'S COMPLEMENT *)
  GENHALF(FCN*VALPOS[FTN]+VALCAR[FTN]+VALSIG[FS]+FSF*TWOTO6+FN+ORD(FRLGTH));
  IF OUTCODE THEN NEXTLINE;
  DDEND (* GENDESCN *) ; }


$OPTIONS page $

(* ************************************************************   GENDESCB  *** *)

    PROCEDURE gendescb (fareg : preg ; fadr, fc, fb, fn : integer ; frlgth : mreg) ;

(* C   GENERATION OF A BITS STRING OPERAND DESCRIPTOR  *)
(* E   373  ILLEGAL CHARACTERS COUNT(GENDESC)
   375  ILLEGAL BITS COUNT(GENDESCB)        *)
      VAR
        ldummy : integer ;
      BEGIN
        genwithpr (fareg, fadr, ldummy) ;
$OPTIONS compile = security $
        lengthctrl (fn, twoto12 - 1, frlgth) ;    (* OPERAND LENGTH *)
        boundsctrl (fc, 0, 63, 373) ; boundsctrl (fb, 0, 8, 375) ;
$OPTIONS compile = true $
        genhalf (fc * twoto16 + fb * twoto12 + fn + ord (frlgth)) ;
        IF outcode THEN
	BEGIN
	  IF fareg = nreg THEN
	    write (mpcogout, '    descb   ', fadr : longint (fadr))
	  ELSE
	    write (mpcogout, '    descb   ', prsymb [fareg], fadr : longint (fadr)) ;
	  ldummy := 3 * fc + fb ;
	  IF ldummy # 0 THEN write (mpcogout, '(', ldummy : longint (ldummy), ')') ;
	  IF frlgth = tn THEN write (mpcogout, ',', fn : longint (fn)) ELSE
	    write (mpcogout, ',', tagsymb [frlgth]) ;
	  nextline ;
	END ;
        IF fareg = prstatic THEN BEGIN
	  genreltext (int15, 1) ;
	  genreltext (absl, 1)
	END
        ELSE IF fareg = prlink THEN BEGIN
	    genreltext (link15, 1) ;
	    genreltext (absl, 1)
	  END
	ELSE genreltext (absl, 2) ;
      END (* GENDESCB *) ;


$OPTIONS page $

(* *************************************************************  GENINDW  **** *)

{ PROCEDURE GENINDW(FPR:PREG;FADR:INTEGER;FTG:TAG;EIS:BOOLEAN);

  CC(*C GENERATION OF AN INDIRECT WORD(TO DESCRIPTOR IF EIS=TRUE) *)
  EE(*E   370   ILLEGAL TAG IN AN INDIRECT WORD (GENINDW)
  371   USE OF PREG NOT ALLOWED IN AN INDIRECT WORD (GENINDW) *)
  RRVAR
  LBIT29 : INTEGER;
  GGBEGIN
  GENWITHPR(FPR,FADR,LBIT29);
  (/ IF EIS THEN
  BEGIN
  IF NOT (FTG IN [TN..TQU,TIC..TQL,TX0..TX7]) THEN ERROR(370);
  END  ELSE
  BEGIN (* NOT EIS *)
  IF FTG IN [TITP,TITS] THEN ERROR(370);
  IF LBIT29 # 0 THEN ERROR(371); (* PREG NOT ALLOWED *)
  END;  /)
  GENHALF(ORD(FTG)+LBIT29);
  IF OUTCODE THEN
  BEGIN
  WRITE(MPCOGOUT,'    VFD     ');
  IF FPR#NREG THEN WRITE(MPCOGOUT,'3/',ORD(FPR):1,',15/') ELSE
  WRITE(MPCOGOUT,'18/',FADR:LONGINT(FADR),',11/0,1/');
  IF FPR#NREG THEN WRITE(MPCOGOUT,'1') ELSE WRITE(MPCOGOUT,'0');
  WRITE(MPCOGOUT,',O6/',(ORD(FTG) DIV 8):1,(ORD(FTG) MOD 8):1);
  NEXTLINE;
  END;
  DDEND (* GENINDW *) ; }


$OPTIONS page $

(* ***********************************************************   GENINDIT  **** *)

{ PROCEDURE GENINDIT(FADR,FTALLY,FTG : INTEGER);

  CC(*C   GENERATION OF AN INDIRECT WORD WITH THREE BINARY FIELDS
  0-17=FADR ; 18-29=FTALLY;30-35=FTG        *)
  EE(*E   377  ILLEGAL TALLY OR TAG (GENINDIT)     *)
  RRVAR
  LDUMMY : INTEGER ;
  GGBEGIN
  GENWITHPR(NREG,FADR,LDUMMY);
  (/ BOUNDSCTRL(FTALLY,0,TWOTO12-1,377);
  BOUNDSCTRL(FTG,0,63,377); /)
  GENHALF( FTALLY*TWOTO6+FTG);
  IF OUTCODE THEN NEXTLINE;
  DDEND (* GENINDIT *); }


$OPTIONS page $

(* ************************************************************  GENIPAIR ***** *)

{ PROCEDURE GENIPAIR(FPR:PREG;FSNO,FWNO,FBNO:INTEGER;FTG:TAG);

  CC(*C   GENERATION OF AN ITS OR ITP WORDS PAIR *)
  EE(*E   365  ILLEGAL SEGMENT NUMBER (GENIPAIR)
  366  ILLEGAL SECOND WORD IN AN ITP OR ITS (GENIPAIR)
  $$(*$PAGE*)
  E*)
  RRVAR
  LTAG,LDUMMY : INTEGER;
  GGBEGIN
  IF FPR # NREG THEN
  BEGIN (* ITP *)
  GENHALF(VALREG[FPR]);LTAG:=O41;
  END   ELSE
  BEGIN (* ITS *)
  (/ BOUNDSCTRL(FSNO,0,TWOTO15-1,365); /)
  GENHALF(FSNO);LTAG:=O43;
  END;
  GENHALF(LTAG);
  IF OUTCODE THEN
  BEGIN
  IF FPR#NREG THEN WRITE(MPCOGOUT,'    ITP     ',PRSYMB[FPR]) ELSE
  WRITE(MPCOGOUT,'    ITS     ',FSNO:LONGINT(FSNO),',');
  WRITE(MPCOGOUT,FWNO:LONGINT(FWNO),',',TAGSYMB[FTG]);
  NEXTLINE;
  END;
  GENWITHPR(NREG,FWNO,LDUMMY);
  (/ BOUNDSCTRL(FBNO,0,35,366); IF FTG IN[TITP,TITS] THEN ERROR(366); /)
  GENHALF(FBNO*TWOTO9+ORD(FTG));
  IF OUTCODE THEN NEXTLINE ;
  DDEND (* GENIPAIR *); }


$OPTIONS page $

(* ************************************ INSER ********************************* *)

    PROCEDURE inser (fcb, fplace : integer) ;

(* C  "FPLACE"  IS  AN INDEX ON FICHINTER   OF INCOMPLETE  INSTRUCTION.
   FICHINTER : ARRAY[ 1..   ] OF SHRTINT;
   CB   CODE COUNTER    0 FOR THE FIRST INST IN THE PROC
   C *)
(* E ERRORS DETECTED
   408: GIVEN FPLACE OUT OF RANGE
   409: COMPUTED DISP OUT OF RANGE
   410: NON-ZERO DISPLACEMENT PART.
   E *)
      LABEL
        1 ;                                       (* SKIP IF ILLEGAL CALL *)

      VAR
        coddep : integer ;
        lerr : boolean ;
      BEGIN
        IF declarationpart THEN
	BEGIN
	  illegal_generation := true ;
	  GOTO 1
	END ;
        lerr := false ;
                                                  (* A CODE DISP MUST BE EXPRESSED IN WORDS *)
        coddep := (fcb (* BYTES FROM 0 *) - 2 * (fplace - 1)) DIV bytesinword ;
$OPTIONS compile = security $
                                                  (* AUTO-CONTROLE *)
        lerr := true ;
        IF (fplace < 1) OR (fplace > maxfich) THEN error (408) ELSE
	IF (coddep < -twoto17) OR (coddep > twoto17 - 1) THEN error (409) ELSE
	  IF fichinter^[fplace] # 0 THEN error (410) ELSE lerr := false ;
$OPTIONS compile = true $
        IF outcode THEN
	BEGIN
	  write (mpcogout, '@@@', '*' : 80, 'INSER ', coddep : 6, ' AT ') ;
	  writeoctal ((ic - (cb - fplace * 2)) DIV bytesinword) ;
	  nextline ;
	END ;
        IF NOT lerr THEN fichinter^[fplace] := coddep ;
1 :
      END (* INSER *) ;


$OPTIONS page $

(* ************************************ GENINSERTION ************************** *)

    PROCEDURE geninsertion (fplace : integer ; fptproc : ctp) ;

(* C .IN ENTRY SEQUENCE AN INCOMPLETE INSTRUCTION HAS BEEN
   GENERATED FOR THE CURRENT STACK FRAME SIZE;
   .FPLACE IS THE FICHINTER INDEX OF THIS INSTRUCTION.
   .AT END OF PROC, TMAX IS THE LARGEST DISPLACEMENT IN CURRENT STACKFRAME
   AND MUST BE INSERTED IN INCOMPLETE INSTR.
   C *)
(* E ERRORS DETECTED
   214 : STACK FRAME MUST NOT EXCCEED MAXSTACKSIZE BYTES
   E *)
      VAR
        coddep : integer ;
      BEGIN
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT-FIN GENINSERTION @@@ WITH FPLACE, TMAX', fplace, tmax) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        tmax := recadre (tmax, stackboundary) ;
        IF tmax >= maxstacksize THEN
	error (213) ELSE
	BEGIN
	  coddep := tmax DIV bytesinword ;
	  IF outcode THEN
	    BEGIN
	      write (mpcogout, '@@@', '*' : 80, 'INSER ', coddep : 6, ' AT ') ;
	      writeoctal ((ic - (cb - fplace * 2)) DIV bytesinword) ;
	      nextline ;
	    END ;
	  fichinter^[fplace] := coddep ;
	END ;
      END (* GENINSERTION *) ;


$OPTIONS page $

(* ************************************ GENCODFONCT *************************** *)

    PROCEDURE gencodfonct (fptproc : ctp) ;

(* C .CALLED AT END OF GENERATED CODE FOR A FUNCTION.
   .LOADS*  A   (SCALAR,NUMERIC *
   * AQ   (POINTER)       *  WITH THE VALUE STORED IN PR6|FCTDEPLW
   *EAQ   (REAL)          *
   .STORES ALSO THIS VALUE IN STORAGE POINTED BY THE LAST "ITS" OF THE
   ARGUMENT LIST  (MOVED AT DEBSTACKLOCAL BY GENPRCENTRY)
   .THE RETURN-OPERATOR DOES NOT ALTER EAQ
   .FPTPROC IS NOT NIL (TESTED BEFORE CALL) AND POINTS THE BOX "PROC"
   C *)
      VAR
        lload, lstor : istand ;
        fctitsw : integer ;
      BEGIN                                       (* GENCODFONCT *)
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT GENCODFONCT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
        WITH fptproc@ DO
	IF proctype # NIL THEN
	  BEGIN                                 (* NO TYPE ERROR *)
	    IF proctype@.form = reel THEN
	      BEGIN lload := idfld ; lstor := idfst ;
	      END ELSE
	      IF proctype@.form = pointer THEN
	        BEGIN lload := ildaq ; lstor := istaq ;
	        END ELSE
	        BEGIN lload := ilda ; lstor := ista ;
	        END ;
	    fctitsw := (pascdebstacklocal + (nbparproc - 1) * bytesindword) DIV bytesinword ;
                                                  (* LOAD  REG *)
	    usednameaddr := fptproc ;
	    genstand (pr6, fctdeplw, lload, tn) ;
                                                  (* STORE VALUE *)
	    genstand (pr6, fctitsw, lstor, tny) ;
	  END (* #NIL, WITH *) ;
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ FIN GENCODFONCT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* GENCODFONCT *) ;


$OPTIONS page $

(* ********************************************* LONGPROFILEENTRYSEQUENCE ***************************** *)

    PROCEDURE longprofileentrysequence ;

      VAR
        lbit29 : integer ;

      BEGIN
        genwithpr (pr0, longprofileplace, lbit29) ;
        genhalf (codebin [itsp3] + lbit29 + ord (tn)) ;
        IF outcode THEN
	BEGIN
	  write (mpcogout, codesymb [itsp3] : 12, prsymb [pr0], longprofileplace : longint (longprofileplace)) ;
	  nextline
	END ;
        usednameaddr := octalformataddr ;
        genhalf (0) ;
        genhalf (5) ;
        IF outcode THEN nextline ;
        genreltext (absl, 2) ;
        genreltext (prof, 1) ;
        genreltext (absl, 1) ;
        genwithpr (pr0, longprofileplace, lbit29) ;
        genhalf (codebin [itsp3] + lbit29 + ord (tn)) ;
        IF outcode THEN
	BEGIN
	  write (mpcogout, codesymb [itsp3] : 12, prsymb [pr0], longprofileplace : longint (longprofileplace)) ;
	  nextline
	END ;
        usednameaddr := octalformataddr ;
        genhalf (0) ;
        genhalf (5) ;
        IF outcode THEN nextline ;
        genreltext (absl, 2) ;
        genreltext (prof, 1) ;
        genreltext (absl, 1) ;
        genwithpr (pr0, longprofileplace, lbit29) ;
        genhalf (codebin [itsp3] + lbit29 + ord (tn)) ;
        IF outcode THEN
	BEGIN
	  write (mpcogout, codesymb [itsp3] : 12, prsymb [pr0], longprofileplace : longint (longprofileplace)) ;
	  nextline
	END ;
        usednameaddr := octalformataddr ;
        genhalf (0) ;
        genhalf (9) ;
        IF outcode THEN nextline ;
        genreltext (absl, 2) ;
        genreltext (prof, 1) ;
        genreltext (absl, 1) ;
        genwithpr (pr0, longprofileplace, lbit29) ;
        genhalf (codebin [itsp3] + lbit29 + ord (tn)) ;
        IF outcode THEN
	BEGIN
	  write (mpcogout, codesymb [itsp3] : 12, prsymb [pr0], longprofileplace : longint (longprofileplace)) ;
	  nextline
	END ;
        usednameaddr := octalformataddr ;
        genhalf (0) ;
        genhalf (9) ;
        IF outcode THEN nextline ;
        genreltext (absl, 2) ;
        genreltext (prof, 1) ;
        genreltext (absl, 1) ;
        genwithpr (pr0, longprofileplace, lbit29) ;
        genhalf (codebin [itsp3] + lbit29 + ord (tn)) ;
        IF outcode THEN
	BEGIN
	  write (mpcogout, codesymb [itsp3] : 12, prsymb [pr0], longprofileplace : longint (longprofileplace)) ;
	  nextline
	END ;
        usednameaddr := octalformataddr ;
        genhalf (0) ;
        genhalf (5) ;
        IF outcode THEN nextline ;
        genreltext (absl, 2) ;
        genreltext (prof, 1) ;
        genreltext (absl, 1) ;

      END ;

$OPTIONS page $

(* ********************************************** GENPROFILEREF ************************************ *)

    PROCEDURE genprofileref ;

(* C GENERATES INSTRUCTION AOS 4|N TO INCREMENT PROFILE COUNTER                              C *)

      VAR
        lbit29 : integer ;
        counterplace : integer ;

      BEGIN
        counterplace := profilewordcount + 1 ;
        genwithpr (prstatic, counterplace, lbit29) ;
        genhalf (codebin [iaos] + lbit29 + ord (tn)) ;
        IF outcode THEN BEGIN
	  write (mpcogout, codesymb [iaos] : 12, prsymb [prstatic], counterplace : longint (counterplace)) ;
	  nextline
	END ;
        insert_ (statnbr * 2, 18, profptr^[profilewordcount]) ;
        profilewordcount := profilewordcount + pclength ;
        genreltext (prof, 1) ;
        genreltext (absl, 1) ;
      END ;

$OPTIONS page $

(* *************************************************** GENLONGPROFILEREF *************************** *)

    PROCEDURE genlongprofileref ;

(* C GENERATES CALL TO LONG_PROFILE OPERATOR TO INCREMENT LONG_PROFILE COUNTERS                   C *)

      VAR
        lbit29 : integer ;

      BEGIN
        genwithpr (pr0, longprofileplace, lbit29) ;
        genhalf (codebin [itsp3] + lbit29 + ord (tn)) ;
        IF outcode THEN
	BEGIN
	  write (mpcogout, codesymb [itsp3] : 12, prsymb [pr0], longprofileplace : longint (longprofileplace)) ;
	  nextline
	END ;
        usednameaddr := octalformataddr ;
        genhalf (0) ;
        genhalf (profilewordcount) ;
        insert_ (statnbr * 2, 18, profptr^[profilewordcount]) ;
        profilewordcount := profilewordcount + lpclength ;
        IF outcode THEN nextline ;
        genreltext (absl, 2) ;
        genreltext (prof, 1) ;
        genreltext (absl, 1) ;
      END ;

$OPTIONS page $

(* ********************************* GENENTRYSTRUCTURE ************************************************************** *)

    PROCEDURE genentrystructure ;

(* C GENERATES A TWO WORDS STRUCTURE AFTER THE CALL TO THE ENTRY OPERATOR
   THIS STRUCTURE CONTAINS OFFSET TO LINK TO SYMBOL TABLE,
   AND OFFSET IN SYMBOL SECTION OF SYMBOL BLOCK OF PROCEDURE.
   THIS STRUCTURE IS FILLED LATER BY pascal_create_tables_                  C *)

      BEGIN
        currentnode^.structureplace := ic DIV bytesinword ;
        usednameaddr := octalformataddr ;
        genc (0) ;
        usednameaddr := octalformataddr ;
        genc (0) ;
        genreltext (absl, 2) ;
        IF symbtabl THEN
	BEGIN
	  genreltext (link18, 1) ;
	  genreltext (symb, 1) ;
	END
        ELSE
	genreltext (absl, 2) ;
      END (* GENENTRYSTRUCTURE *) ;

$OPTIONS page $

(* ************************************ EXITLABEL ***************************** *)

    PROCEDURE exitlabel (flabinx : integer ; flabplace : integer) ;

(* C  FLABINX IS BYTES DISPLACEMENT IN LINKAGE SECTION OF AN ITS,WHICH MUST
   POINT AT EXECUTION TIME ON THE INSTRUCTION AT DISPLACEMENT
   FLABPLACE IN TEXT SECTION
   C *)
      VAR
        locreturncode, entrylength : integer ;
        functionflag : boolean ;
      BEGIN                                       (* EXITLABEL *)
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout,
	    '@@@ DEBUT EXITLABEL @@@ WITH FLABINX,FLABPLACE', flabinx, flabplace) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        IF errtotal = 0 THEN
	BEGIN
	  functionflag := false ;
	  genentrypoint (flabplace, flabinx,
	    4,                                  (* EXIT LABEL *)
	    blank, blank,
	    functionflag,
	    entrylength,
	    locreturncode) ;
	  IF locreturncode <> 0 THEN
	    error (500) ;
	END ;
$OPTIONS compile = trace $
        IF genetrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN EXITLABEL @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* EXITLABEL *) ;


$OPTIONS page $

(* ************************************ GENPROLOG ***************************** *)

    PROCEDURE genprolog (VAR unres : integer ; VAR fdebic : integer) ;

(* C .CALLED  TO GENERATE THE CALL OF " MAIN-ENTRY-OPERATOR"
   .CONTEXT IS
   PR6 =FRAME CALLER       PR7 STACK HEADER
   .AFTER MAIN ENTRY
   PR6  FRAME MAIN
   PR0  PASCAL OPERATOR
   PR4  LINKAGE SECTION OF PROGRAM
   .CALL  RESET ,REWRITE  FOR INPUT,OUTPUT
   C *)
      VAR
        functionflag : boolean ;
        it, execflags, locreturncode, entrylength : integer ;
      BEGIN                                       (* GENPROLOG *)
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT GENPROLOG @@@ WITH FDEBIC', fdebic) ; nextline ;
	END ;
$OPTIONS compile = true $
                                                  (* *)
                                                  (* FIRST CALL PL1 PROCEDURE  *)
                                                  (*  TO GENERATE ENTRY POINT SEQUENCE *)
                                                  (* *)
        functionflag := false ;
        entrylength := 0 ;
        IF errtotal = 0 THEN
	BEGIN
	  genentrypoint (fdebic, 0,             (* NO MEANING *)
	    3,                                  (* MAIN ENTRY POINT *)
	    blank,                              (* For segname      *)
	    progname,                           (* For entryname    *)
	    functionflag,
	    entrylength,
	    locreturncode) ;
	  IF locreturncode <> 0 THEN
	    error (501) ;
	END ;
                                                  (* INCR IC *)
        fdebic := fdebic + entrylength ;
        IF codelist THEN
	FOR it := ic TO (ic + entrylength - 1) DO
	  IF (it MOD bytesinword) = 0 THEN
	    BEGIN
	      usednamesaddr@ [it DIV bytesinword] := octalformataddr ;
	      usednameaddr := NIL ;
	    END ;
        ic := ic + entrylength ;
        mainloc := ic DIV bytesinword ;
        IF linktomain THEN
	IF errtotal = 0 THEN
	  BEGIN
	    genentrypoint (ic, linktomainplace, 0, blank, blank, functionflag, entrylength, locreturncode) ;
	    IF locreturncode <> 0 THEN
	      error (502) ;
	  END ;
        genstand (nreg, 0, iepp5, tic) ;          (*  OFFSET OF 1RST INSTR OF MAIN *)
        genstand (pr7, transoptvptr, iepp2, tny) ; (* PTR ON OP SEG'S TRANSFER VECTORS *)
        genstand (pr2, pascoperatorsdep, iepp2, tny) ; (* PASCAL OPERATORS SEGMENT *)
        execflags := mainbit ;
        IF fastoperator THEN
	execflags := execflags + fastbit ;
        IF asscheck THEN execflags := execflags + checkbit ;
        IF interactive THEN execflags := execflags + interactivebit ;
        IF envstandard = stdsol THEN execflags := execflags + solstandardbit ;
        IF iowarnings THEN execflags := execflags + iowarningsbit ;
        genstand (nreg, execflags, ildq, tdl) ;
        unres := indfich ; genstand (nreg, 0, ieax7, tn) ; (* FILLED LATER *)
        genstand (pr2, mainentryplace, itsp3, tn) ;
        genentrystructure ;
        IF mapswitch THEN BEGIN
	  IF longprofile THEN longprofileentrysequence ;
	  statement_ends (currentnode^.hdrlen) ;
	  statement_begins (true) ;
	END ;
        IF inputctp # NIL THEN
	BEGIN
	  usednameaddr := inputctp ;
	  genstand (prlink, inputctp@.vaddr DIV bytesinword, iepp3, tny) ;
	  genstand (pr6, fsbadrw, ispri3, tn) ;
	  genstand (pr0, resetplace, itsp3, tn) ;
	END ;
        IF outputctp # NIL THEN
	BEGIN
	  usednameaddr := outputctp ;
	  genstand (prlink, outputctp@.vaddr DIV bytesinword, iepp3, tny) ;
	  genstand (pr6, fsbadrw, ispri3, tn) ;
	  genstand (pr0, rewriteplace, itsp3, tn) ;
	END ;
        IF errorctp <> NIL THEN
	BEGIN
	  usednameaddr := errorctp ;
	  genstand (prlink, errorctp^.vaddr DIV bytesinword, iepp3, tny) ;
	  genstand (pr6, fsbadrw, ispri3, tn) ;
	  genstand (pr0, rewriteplace, itsp3, tn) ;
	END ;

$OPTIONS compile = trace $
        IF genetrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN GENPROLOG @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* GENPROLOG *) ;


$OPTIONS page $

(* ************************************ GENPGEXIT ***************************** *)

    PROCEDURE genpgexit ;

(* C GENERATES THE CALL OF MAIN-RETURN-OPERATOR
   C *)
      BEGIN                                       (* GENPGEXIT *)
        genstand (prstatic, 8 (* HEADER LENGTH *), iepp2, tn) ;
        genstand (pr0, extreturnplace, itra, tn) ;
      END (* GENPGEXIT *) ;


$OPTIONS page $

(* ************************************* GENPRCEXIT *************************** *)

    PROCEDURE genprcexit (fptproc : ctp) ;

(* C  THIS PROCEDURE CALL THE RIGHT RETURN-OPERATOR WHOSE FUNCTIONS ARE
   . EXT-RETURN
   *RESET PR7 = STACK-HEADER
   *CHANGE PR6 = AND PR7|STACK-END-PTR
   *RESET = OPERATOR SEGMENT
   *RESET INDICATORS
   *RETURN IN CALLER
   . INT-RETURN
   *CHANGE PR6 AND PR7|STACK-END-PTR
   *RETURN IN CALLER
   C *)
(* E ERRORS DETECTED
   436  ILLEGAL PROCKIND
   E *)
      VAR
        opplace : integer ;
      BEGIN                                       (* GENPRCEXIT *)
        IF fptproc # NIL THEN
	WITH fptproc@ DO
	  BEGIN                                 (* POINTS  A "PROC" BOX *)
	    IF prockind = actual THEN
                                                  (* LOCAL  PASCAL PROCEDURE ===> SHORT RETURN *)
	      opplace := intreturnplace ELSE
$OPTIONS compile = security $
	      IF prockind # exportable THEN
	        BEGIN opplace := 0 ; error (436) ;
	        END ELSE
$OPTIONS compile = true $
	        opplace := extreturnplace ;
	    genstand (pr0, opplace, itra, tn) ;
	  END (* WITH FPTPROC,FPTPROC #NIL *) ;
      END (* GENPRCEXIT *) ;


$OPTIONS page $

(* ************************************ GENPRCENTRY *************************** *)

    PROCEDURE genprcentry (VAR unres : integer ; fptproc : ctp ; VAR fic : integer) ;

(* C .THIS PROC GENERATES CALLING SEQUENCE OF ENTRY-OPERATOR FOR THE
   PROCEDURE  DESCRIBED BY "FPTPROC".
   .FIC IS THE  ADDRESS OF STANDARD ENTRY SEQUENCE TO BE GENERATE
   .UNRES IS  THE PLACE IN FICHINTER OF UNENDED INSTRUCTION USED TO KNOW
   THE FINAL  FRAME SIZE.
   THIS INSTR IS RESOLVED IN LEAVEBODY WITH THE CALL OF GENINSERTION.
   . AN EXTERNAL  PL/I PROCEDURE IS CALLED IN ORDER TO
   . GENERATE  STANDARD ENTRY SEQUENCE
   . GENERATE ITS IN LINKAGE SECTION
   .   GENERATE ALL OTHER ASSOCIATED STRUCTURES.
   C *)
      VAR
        locsegname, locentryname : alfaid ;
        it, lcode, longlist, execflags, locreturncode, entrylength : integer ;
        functionflag : boolean ;
      BEGIN                                       (* GENPRCENTRY *)
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT GENPRCENTRY @@@ WITH FIC:', fic) ; nextline ;
	END ;
$OPTIONS compile = true $
        IF fptproc # NIL THEN
	WITH fptproc@ DO
	  BEGIN

(* CALL  EXTERNAL PROCEDURE FOR ITS, ENTRY SEQUENCE, ...       *)
(* 1RST. PARAM = BYTES OFFSET IN TEXT SECTION                  *)
(* 2D.   PARAM = BYTES OFFSET WANTED IN PR4                    *)
(* 3D.   PARAM = CODE FOR PROCEDURE *0 INTERNAL                *)
(*                                  *1 EXPORTABLE              *)
(*                        *3 MAIN   *2 IMPORTED                *)
(* 4D. SEGNAME BLANK when exported
   5D  ENTRYNAME  Procedure name
   6D  Response 0 means OK    *)
	    locsegname := blank ;
	    locentryname := name ;
	    IF prockind = actual THEN lcode := 0 ELSE
	      lcode := 1 ;
	    entrylength := 0 ;
	    functionflag := (proctype <> fptproc) ;
	    IF errtotal = 0 THEN
	      BEGIN
	        genentrypoint (fic,
		procaddr,
		lcode,                        (* Type of entry *)
		locsegname,
		locentryname,
		functionflag,
		entrylength,
		locreturncode) ;
	        IF locreturncode <> 0 THEN
		error (503) ;
	      END ;
                                                  (* NOW SELECT THE RIGHT  *)
                                                  (*  OPERATOR SHORT FOR ACTUAL *)
	    IF lcode = 0 (* ACTUAL *) THEN
	      BEGIN
	        locincode := ic ;
	        genstand (nreg, 0, iepp5, tic) ; (* PR5 = FIRST INSTR OF THIS PROC *)
                                                  (* <=== *)
	        unres := indfich ; genstand (nreg, 0, ieax7, tn) ; (* FRAME SIZE *)
	        genstand (pr0, intentryplace, itsp3, tn) ;
	        genentrystructure ;
	      END (* ACTUAL *) ELSE
	      BEGIN                             (* EXPORTABLE *)
	        IF codelist THEN
		FOR it := ic TO (ic + entrylength - 1) DO
		  IF (it MOD bytesinword) = 0 THEN
		    BEGIN
		      usednamesaddr@ [it DIV bytesinword] := octalformataddr ;
		      usednameaddr := NIL ;
		    END ;
	        ic := ic + entrylength ;
	        locincode := ic ;
	        fic := fic + entrylength ;
	        genstand (nreg, 0, iepp5, tic) ; (* OFFSET OF FIRST INSTR. *)
                                                  (* LOAD PR2 WITH PASCAL OPERATOR SEGMENT *)
	        genstand (pr7, transoptvptr, iepp2, tny) ;
	        genstand (pr2, pascoperatorsdep, iepp2, tny) ;
	        IF fastoperator THEN
		execflags := fastbit
	        ELSE
		execflags := 0 ;
	        IF asscheck THEN execflags := execflags + checkbit ;
	        IF interactive THEN execflags := execflags + interactivebit ;
	        IF envstandard = stdsol THEN execflags := execflags + solstandardbit ;
	        IF iowarnings THEN execflags := execflags + iowarningsbit ;
	        genstand (nreg, execflags, ildq, tdl) ;
	        unres := indfich ; genstand (nreg, 0, ieax7, tn) ; (* FRAME SIZE *)
                                                  (* NOW CALL OPERATOR *)
	        genstand (pr2, extentryplace, itsp3, tn) ;
	        genentrystructure ;
	        IF mapswitch THEN BEGIN
		  IF longprofile THEN longprofileentrysequence ;
		END ;
	      END (* EXPORTABLE *) ;
	    IF (formals # NIL) OR (proctype # fptproc) THEN
	      BEGIN
                                                  (* MOVE ARGUMENT LIST IN CURRENT *)
                                                  (*  FRAME TO OPTIMIZE ACCESS *)
	        longlist := nbparproc * bytesindword ; (* EACH ITEM IS AN ITS *)
	        IF phasdescriptor THEN
		longlist := longlist * 2 ;
	        genstand (pr6, argptw, iepp3, tny) ; (* SAVING PLACE OF ARG POINTER *)
	        mfari1 := a1r0i0 ; mfari2 := a1r0i0 ;
	        geneism (imlr, 0, p0t0r0) ;
	        gendesca (pr3, 2, 0, l9, longlist, tn) ;
	        gendesca (pr6, pascdebstacklocal DIV bytesinword, 0, l9, longlist, tn) ;
	      END ;
	  END (* WITH FPTPROC *) ELSE
	BEGIN unres := 0 ; fic := 0 ;
	END ;
        IF mapswitch THEN
	BEGIN
	  statement_ends (currentnode^.hdrlen) ;
	  statement_begins (true) ;
	END ;
$OPTIONS compile = trace $
        IF genetrace > low THEN
	BEGIN
	  write (mpcogout, '@@@  FIN GENPRCENTRY @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* GENPRCENTRY *) ;


$OPTIONS page $

(* ************************************* CLOSEFILE **************************** *)

    PROCEDURE closefile (filept : ctp) ;

(* C   CALLED IN ORDER TO GENERATE CODE TO CLOSE FILE
   OR TO CALL THE APPROPRIATE RUN-TIME
   IF FILEPT IS NIL THEN DECLARATION ERROR ==> NO-OP
   FILEPT POINTS THE VARS BOX         C *)
      VAR
        lbase : preg ;
      BEGIN
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT-FIN CLOSEFILE @@@ WITH FILEPT AT @', ord (filept)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        IF filept # NIL THEN
	BEGIN
                                                  (* CALL OPERATOR CLOSE-FILE *)
                                                  (* LOAD PR3 WITH  F S B  ADDRESS *)
                                                  (*   AND STORE IT IN FRAME *)
	  IF level = 0 THEN
	    lbase := prstatic ELSE lbase := pr6 ;
	  IF filept^.vkind <> actual THEN
	    lbase := prlink ;
	  usednameaddr := filept ;
	  genstand (lbase, filept@.vaddr DIV bytesinword, iepp3, tny) ;
	  genstand (pr6, fsbadrw, ispri3, tn) ;
	  genstand (pr0, closeplace, itsp3, tn) ;
	END ;
      END (* CLOSEFILE *) ;


$OPTIONS page $

(* ***********************************   INITIOZONE   ************************* *)

    PROCEDURE initiozone (filept : ctp) ;

(* C
   This procedure prepares the code to call INIT_FSB_ALLOC
   Standard files input, output et error  are excluded from the logic
   of this procedure.
   Parameter list description:
   ------------------------------
   ADDRESS OF POINTER on address
   File identification code
   1    Permanent        3  Workfile        5 Localfile    <-- Record
   2         ''          4    ''            6   ''         <-- Text
   Record_Size
   Number of files
   Pointer on name or array of names

   C *)

      VAR
        filecode : integer ;
        lp : ctp ;
        it : integer ;
        lbase : register ;
        locsize : integer ;
        charcount : integer ;

      BEGIN                                       (* INITIOZONE *)
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, ' @@@ Debut de INITIOZONE @@@ pour FILEPT',
	    ord (filept)) ; nextline ;
	END ;
$OPTIONS compile = true $
        WITH filept^ DO
	BEGIN
	  filecode := -1 ;                      (* Means at end required standard file *)
	  IF vfilelocation = permanentfile THEN filecode := 1 ELSE
	    IF vfilelocation = workfile THEN filecode := 3 ELSE
	      IF vfilelocation = localfile THEN filecode := 5 ;
	  IF filecode >= 0 (* Not required *) THEN
	    IF vtype = textfilectp THEN
	      filecode := filecode + 1 ;

(* At this stage FILECODE ready for call of INITFSB    *)
	  IF filecode >= 0 THEN
	    BEGIN
	      IF level = 0 THEN lbase := prstatic ELSE
	        lbase := pr6 ;
	      IF vkind <> actual THEN
	        lbase := prlink ;
                                                  (* Compute address of pointer on fsb and store it *)
	      usednameaddr := filept ;
	      genstand (lbase, vaddr DIV bytesinword, iepp2, tn) ;
	      genstand (pr6, fsbadrw, ispri2, tn) ;

(* Load RA with FILECODE *)
	      genstand (nreg, filecode, ilda, tdl) ;

	      IF NOT odd (filecode) THEN
	        locsize := iotextbuffersize (* File text *) ELSE
	        locsize := vtype^.feltype^.size (* File sequential *) ;
	      gencstecode (locsize, ildq) ;

(* Now load X1 with the numbers of files associated with
   this FSB pointer   *)
	      genstand (nreg, 1, ieax1, tn) ;

(* Now load PR2 with a pointer on the name(s)        *)
	      create_konst_box (lp, blank, alfaconst) ;
	      WITH lp^ DO
	        BEGIN
		contype := alfaptr ; succ := nextalf ;
	        END ;
	      FOR it := 1 TO maxval DO bufval [it] := ' ' ;
	      charcount := 1 ;
	      WHILE (name [charcount] <> ' ') AND (charcount <= lgfilename) DO
	        charcount := charcount + 1 ;
	      longstring := charcount ;
	      bufval [1] := chr (charcount - 1) ;
	      FOR it := 2 TO charcount DO
	        bufval [it] := name [it - 1] ;
	      nextalf := lp ; crealfabox (lp) ;
	      enterundlab (lp^.unddeb) ;
	      genstand (nreg, 0, iepp2, tic) ;

	      genstand (pr0, initfsballocplace, itsp3, tn) ;

	    END (* FILECODE >= 0      else NOCALL *) ;
	END (* With FILEPT *) ;

$OPTIONS compile = trace $
        IF genetrace > low THEN
	BEGIN
	  write (mpcogout, ' @@@ Fin de INITIOZONE @@@ ') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* INITIOZONE *) ;


$OPTIONS page $

(* ******************************** GEN_INIT_FSB_TRAP_STRUCTURES ********************* *)

    PROCEDURE gen_init_fsb_trap_structures (filpt : ctp) ;

      VAR
        it : integer ;
        locreturncode : integer ;
        entrylength : integer ;
        lp : ctp ;
        charcount : integer ;
      BEGIN
        WITH filpt^ DO
	IF (vfilelocation IN [permanentfile, workfile]) AND (NOT (vkind = imported)) THEN
	  BEGIN
	    IF init_fsb_trap_flag THEN
	      BEGIN
	        init_fsb_trap_flag := false ;
	        init_fsb_trap_number_of_files := 1 ;
	        init_fsb_trap_info_place := ic ;
	        lkc := recadre (lkc, bytesindword) ;
	        init_fsb_trap_links_place := lkc ;
	        lkc := lkc + 2 * bytesindword ;
	        genentrypoint (0, init_fsb_trap_links_place, 2, 'pascal_io_',
		'pascal_init_fsb_trap_proc_', false, entrylength, locreturncode) ;
	        IF locreturncode <> 0 THEN error (505) ;
	        genentrypoint (ic, init_fsb_trap_links_place + bytesindword,
		4, blank, blank, false, entrylength, locreturncode) ;
	        IF locreturncode <> 0 THEN error (505) ;
	        usednameaddr := octalformataddr ;
	        infich (2) ;                    (* VERSION NUMBER FOR TRAP_INFO STRUCTURE *)
	        infich (0) ;                    (* NUMBER OF FILES. FILLED LATER IN PASCAL_BUILD_OBJECT *)
	        genreltext (absl, 2) ;
	      END ELSE
	      init_fsb_trap_number_of_files := init_fsb_trap_number_of_files + 1 ;
	    usednameaddr := octalformataddr ;
	    infich (vaddr DIV bytesinword) ;
	    IF vkind = exportable THEN genreltext (link15, 1)
	    ELSE genreltext (int15, 1) ;
	    infich ('350100'o) (* FOR epp0 0|vaddr INSTRUCTION, RELOCATED BY BINDER *) ;
	    genreltext (absl, 1) ;
	    create_konst_box (lp, blank, alfaconst) ;
	    WITH lp^ DO
	      BEGIN
	        contype := alfaptr ; succ := nextalf ;
	      END ;
	    FOR it := 1 TO maxval DO bufval [it] := ' ' ;
	    charcount := 1 ;
	    WHILE (name [charcount] <> ' ') AND (charcount <= lgfilename) DO
	      charcount := charcount + 1 ;
	    longstring := charcount ;
	    bufval [1] := chr (charcount - 1) ;
	    FOR it := 2 TO charcount DO
	      bufval [it] := name [it - 1] ;
	    nextalf := lp ; crealfabox (lp) ;
	    enterundlab (lp^.unddeb) ;
	    usednameaddr := octalformataddr ;
	    infich (0) ; genreltext (self_rel, 1) ;
	    infich ((1 + 2 * ord (vfilelocation = workfile) + ord (vtype = textfilectp)) * twoto14 + 1) ;
	    genreltext (absl, 1) ;
	    usednameaddr := octalformataddr ;
	    IF vtype = textfilectp THEN
	      genc (iotextbuffersize) (* File text *) ELSE
	      genc (vtype^.feltype^.size) (* File sequential *) ;
	    genreltext (absl, 2) ;
	  END ;
      END (* INIT_FSB_TRAP_STRUCTURES *) ;

$OPTIONS page$

(* ***********************************************WRITOUT********************** *)

    PROCEDURE writout (zonedisp, endcode : integer) ;

(* C     . MUST BE CALLED ONLY IF ENVIRONT = DATA  (ACTUAL GLOBAL INIT.)
   ENVIRONT = TEXT  (ALM CODE FOR A BODY)
   . ZONEDISP IS THE BYTE ADDRESS OF THE FIRST ITEM TO BE GENERATE
   C *)
(* E ERRORS DETECTED
   504   Auto-controle de GENBINAREA
   E *)

      VAR
        areacode : integer ;
        locreturncode : integer ;
      BEGIN                                       (* WRITOUT *)
$OPTIONS compile = trace $
        IF genetrace > none THEN
	BEGIN
	  write (mpcogout, '@@@ DEBUT WRITOUT @@@ WITH ZONEDISP,INDFICH,ENVIRONT :',
	    zonedisp, indfich, ord (environt)) ;
	  nextline ;
	END ;
$OPTIONS compile = true $
        IF errtotal = 0 THEN BEGIN
	  IF environt = data THEN areacode := 3
	  ELSE BEGIN
	      areacode := 1 ;
	      genreltext (absl, (indfich - 1) - endcode) ;
	    END ;
	  genbinarea (zonedisp, areacode, indfich - 1, endcode, fichinter^, locreturncode) ;
	  IF locreturncode <> 0 THEN
	    BEGIN
	      error (504) ;
                                                  (* Sequence filled later if necessary *)
	    END ;
	END ;
        indfich := 1 ;
$OPTIONS compile = trace $
        IF genetrace > low THEN
	BEGIN
	  write (mpcogout, '@@@ FIN WRITOUT @@@') ; nextline ;
	END ;
$OPTIONS compile = true $
      END (* WRITOUT *) ;

(* END OF THE MODULE GENERE  *************** *) BEGIN
    END.
