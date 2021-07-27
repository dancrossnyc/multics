/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */

/**** format: ind3,ll80,initcol6,indattr,^inddcls,dclind4,idind16	       */
/**** format: struclvlind2,^ifthenstmt,^ifthendo,^ifthen,^indnoniterdo       */
/**** format: ^inditerdo,^indnoniterend,^indthenelse,case,^indproc,^indend   */
/**** format: ^delnl,^insnl,comcol41,^indcom,^indblkcom,linecom,^indcomtxt   */

/*   *   *   *   *	 *   *   *   *   *	 *   *   *   *   *	 *   *   *   *   */
/*							       */
/* Name:	time_info_					       */
/*							       */
/*     Table of values used in converting date/time character strings to or  */
/*  from Multics standard clock values.  Use time_names.incl.pl1 to	       */
/*  reference data values.					       */
/*							       */
/*							       */
/* Entry:  time_info_$version					       */
/*							       */
/* Version number of the structures in the time_info_.		       */
/*							       */
/*							       */
/* Entry:  time_info_$language_names				       */
/*							       */
/* Names of languages in which day names, month names and time zones can be  */
/*  expressed.  Each language is present in each language.		       */
/*							       */
/*							       */
/* Entry:	 time_info_$zone_names				       */
/*							       */
/* Table of time zones in each of the languages.			       */
/*							       */
/*							       */
/* Entry:  time_info_$month_names				       */
/*							       */
/* Table of month names in each of the languages.			       */
/*							       */
/*							       */
/* Entry:  time_info_$day_names				       */
/*							       */
/* Table of day names in each of the languages.			       */
/*							       */
/*							       */
/* Note							       */
/*							       */
/*      A Multics standard clock value is a number of microseconds relative  */
/*  to January 1, 1901 0000.0 GMT.				       */

/* Status							       */
/* 0) Created:	1983-02-09  JFalksen-			       */
/* 1) Updated:	1984-11-18 jaf				       */
/*	Added Australian zones, filled in Spanish.		       */


/* HISTORY COMMENTS:
  1) change(86-08-14,GDixon), approve(86-09-04,MCR7532),
     audit(86-09-05,Martinson), install(86-09-16,MR12.0-1159):
     Rename zone AHST (Alaska-Hawaii Standard Time) to HST (Hawaiian Standard
      Time, GMT-10), according to ANSI Standard X3.51-1975.  Add HDT
      (Hawaiian Daylight Time, GMT-9) and YDT (Yukon Daylight Time, GMT-8)
      according to this standard. (phx18898)
     Add NDT (Newfoundland Daylight Time, GMT-2.5). (phx19658)
     Correct spelling, capitalization and accents (by removing them) in French
      language words. (Compliments of Bruno Mannoni, phx20440)
     Replace zone NZT by NZST (New Zealand Standard Time, GMT+12) and add
      zone NZDT (New Zealand Daylight Time, GMT+13). (phx18881)
  2) change(87-04-09,Lippard), approve(86-11-24,MCR7576),
     audit(87-05-18,Dickson), install(87-07-17,MR12.1-1043):
     Delete inclusion of time_zones_.
                                                   END HISTORY COMMENTS */



/* Modified 07/09/86 by Bruno Mannoni to correct the French names            */
/* ************************************************************************* */
/* ************************************************************************* */
/* **							    ** */
/* ** Debugging trace: While working on a table, it may be helpful to see ** */
/* ** what is going on.  For this purpose there is a debugging switch     ** */
/* ** to be set before running a COMPILED version of this procedure,      ** */
/* **   i.e. execution via cds doesn't hack it.			    ** */
/* ** execute like this:     time_info_$dbn;time_info_;time_info_$dbf	    ** */
/* **							    ** */
/* ************************************************************************* */
/* ************************************************************************* */%page;
time_info_: ti_: proc;

dcl (the_language_count init (4),	/* how many languages in the table   */
    the_zone_count  init (48),	/* how many zones in the table       */
    the_keyword_count init (16),	/* how many keywords in the table    */
				/* includes 3 generated in setup     */

    english	init (1),		/* define a set of named languages,  */
    french	init (2),		/* ..beginning at 1		       */
    german	init (3),
    spanish	init (4),

    Default_Language init (1),	/* site default language	       */

    Fill_From	init (1)		/* which language supplies defaults  */
				/*  for unspecified zones.	       */
    )		int static options (constant);

   call setup;

/**** The 3 keywords "date_time", "date", and "time" cannot be specified     */
/**** here.  They are dynamic quantities, while the ones here are static.    */
/**** These end up in read-only memory.				       */

   call set_format (		/* everything but the kitchen sink   */
      "all",   "^9999yc-^my-^dm  ^Hd:^MH:^99.(6)9UM^zd ^za ^da ^fi"
      || "^(6)9fw ^ma dy^dy dc^dc Uc^Uc");
   call set_format (
      "calendar_clock",     "^9999yc-^my-^dm__^Hd:^MH:^99.(6)9UM_^za_^da");
   call set_format (
      "clock",              "^9999yc-^my-^dm  ^Hd:^MH:^99.(6)9UM ^za ^da");
   call set_format (
      "iso_date",           "^9999yc-^my-^dm");
   call set_format (
      "iso_date_time",      "^9999yc-^my-^dm ^Hd:^MH:^SM ^za");
   call set_format (
      "iso_long_date",      "^9999yc-^my-^dm ^da");
   call set_format (
      "iso_long_date_time", "^9999yc-^my-^dm ^Hd:^MH:^99.(6)9UM ^za");
   call set_format (
      "iso_long_time",      "^Hd:^MH:^99.(6)9UM");
   call set_format (
      "iso_time",           "^Hd:^MH:^SM");
   call set_format (
      "system_date",        "^my/^dm/^yc");
   call set_format (
      "system_date_time",   "^my/^dm/^yc  ^Hd^99v.9MH ^xxxxza^xxxda");
   call set_format (
      "system_time",        "^Hd:^MH");
   call set_format (
      "request_id",         "^yc^my^dm^Hd^MH^99.(6)9UM");

/*  +-+ +-+ +-+ +-+ +-+ +-+ english language values +-+ +-+ +-+ +-+ +-+ +-+  */

   call set_language (english, english, "english");
   call set_language (english, french, "french");
   call set_language (english, german, "german");
   call set_language (english, spanish, "spanish");


   call set_month_name (english, Jan, "Jan", "January");
   call set_month_name (english, Feb, "Feb", "February");
   call set_month_name (english, Mar, "Mar", "March");
   call set_month_name (english, Apr, "Apr", "April");
   call set_month_name (english, May, "May", "May");
   call set_month_name (english, Jun, "Jun", "June");
   call set_month_name (english, Jul, "Jul", "July");
   call set_month_name (english, Aug, "Aug", "August");
   call set_month_name (english, Sep, "Sep", "September");
   call set_month_name (english, Oct, "Oct", "October");
   call set_month_name (english, Nov, "Nov", "November");
   call set_month_name (english, Dec, "Dec", "December");

   call set_day_name (english, Mon, "Mon", "Monday");
   call set_day_name (english, Tue, "Tue", "Tuesday");
   call set_day_name (english, Wed, "Wed", "Wednesday");
   call set_day_name (english, Thu, "Thu", "Thursday");
   call set_day_name (english, Fri, "Fri", "Friday");
   call set_day_name (english, Sat, "Sat", "Saturday");
   call set_day_name (english, Sun, "Sun", "Sunday");

   call set_offset (english, Year, "yr", "years", "year", "this");
   call set_offset (english, Month, "mo", "months", "month", "this");
   call set_offset (english, Week, "wk", "weeks", "week", "this");
   call set_offset (english, Day, "da", "days", "day", "this");
   call set_offset (english, Hour, "hr", "hours", "hour", "this");
   call set_offset (english, Minute, "min", "minutes", "minute", "this");
   call set_offset (english, Second, "sec", "seconds", "second", "this");
   call set_offset (english, Microsecond, "usec", "microseconds",
      "microsecond", "this");

   call set_word (english, Before, "before", "?");
   call set_word (english, On, "on", "?");
   call set_word (english, After, "after", "?");
   call set_word (english, Or, "or", "?");
   call set_word (english, Noon, "noon", "n");
   call set_word (english, Midnight, "midnight", "m");
   call set_word (english, Now, "now", "?");
   call set_word (english, Today, "today", "?");
   call set_word (english, Yesterday, "yesterday", "?");
   call set_word (english, Tomorrow, "tomorrow", "?");
   call set_word (english, FiscalWeek, "FW", "?");

   call set_zone (english, "ut  ", "ut  ", 0, "Universal Time");
   call set_zone (english, "z   ", "z   ", 0, "Universal Time");
   call set_zone (english, "gmt ", "gmt ", 0, "Greenwich Mean Time");
   call set_zone (english, "wat ", "wat ", -1, "West Africa Time");
   call set_zone (english, "at  ", "at  ", -2, "Azores Time");
   call set_zone (english, "gst ", "gst ", -3, "Greenland  Standard Time");
   call set_zone (english, "adt ", "adt ", -3, "Atlantic Daylight Time");
   call set_zone (english, "nst ", "nst ", -3.5,
				        "Newfoundland Standard Time");
   call set_zone (english, "ndt ", "ndt ", -2.5,
				        "Newfoundland Daylight Time");
   call set_zone (english, "ast ", "ast ", -4, "Atlantic Standard Time");
   call set_zone (english, "edt ", "edt ", -4, "Eastern Daylight Time");
   call set_zone (english, "est ", "est ", -5, "Eastern Standard Time");
   call set_zone (english, "cdt ", "cdt ", -5, "Central Daylight Time");
   call set_zone (english, "cst ", "cst ", -6, "Central Standard Time");
   call set_zone (english, "mdt ", "mdt ", -6, "Mountain Daylight Time");
   call set_zone (english, "mst ", "mst ", -7, "Mountain Standard Time");
   call set_zone (english, "pdt ", "pdt ", -7, "Pacific Daylight Time");
   call set_zone (english, "pst ", "pst ", -8, "Pacific Standard Time");
   call set_zone (english, "ydt ", "ydt ", -8, "Yukon Daylight Time");
   call set_zone (english, "yst ", "yst ", -9, "Yukon Standard Time");
   call set_zone (english, "hdt ", "hdt ", -9, "Hawaiian Daylight Time");
   call set_zone (english, "hst ", "hst ",-10, "Hawaiian Standard Time");
/**** l set_zone (english, "bst ", "bst ",-11, "Bering Standard Time");      */
   call set_zone (english, "nt  ", "nt  ",-11, "Nome Time");
   call set_zone (english, "cet ", "cet ", +1, "Central European Time");
   call set_zone (english, "met ", "met ", +1, "Middle Europe Time");
   call set_zone (english, "mewt", "mewt", +1, "Middle Europe Winter Time");
   call set_zone (english, "bst ", "bst ", +1, "British Summer Time");
   call set_zone (english, "swt ", "swt ", +1, "Swedish Winter Time");
   call set_zone (english, "fwt ", "fwt ", +1, "French Winter Time");
   call set_zone (english, "mest", "mest", +2, "Middle Europe Summer Time");
   call set_zone (english, "eet ", "eet ", +2, "Eastern European Time");
   call set_zone (english, "sst ", "sst ", +2, "Swedish Summer Time");
   call set_zone (english, "fst ", "fst ", +2, "French Summer Time");
   call set_zone (english, "bt  ", "bt  ", +3, "Baghdad Time");
   call set_zone (english, "ist ", "ist ", +5.5, "Indian Standard Time");
/**** l set_zone (english, "sst ", "sst ", +7, "South Sumatra Time");	       */
   call set_zone (english, "wast", "wast", +7,
				   "West Australian Standard Time");
   call set_zone (english, "jt  ", "jt  ", +7.5, "Java Time");
   call set_zone (english, "wadt", "wadt", +8,
				   "West Australian Daylight Time");
   call set_zone (english, "cct ", "cct ", +8, "China Coast Time");
   call set_zone (english, "jst ", "jst ", +9, "Japan Standard Time");
   call set_zone (english, "cast", "cast", +9.5,
				   "Central Australian Standard Time");
   call set_zone (english, "sast", "sast", +9.5,
				   "South Australian Standard Time");
   call set_zone (english, "cadt", "cadt", +10.5,
				   "Central Australian Daylight Time");
   call set_zone (english, "sadt", "sadt", +10.5,
				   "South Australian Daylight Time");
   call set_zone (english, "east", "east", +10,
				   "East Australian Standard Time");
   call set_zone (english, "eadt", "eadt", +11,
				   "East Australian Daylight Time");
   call set_zone (english, "nzst", "nzst", +12, "New Zealand Standard Time");
   call set_zone (english, "nzdt", "nzdt", +13, "New Zealand Daylight Time");

/*   +-+ +-+ +-+ +-+ +-+ +-+ french language values +-+ +-+ +-+ +-+ +-+ +-+  */
/*							       */
/*  The national character usage from here on down is taken from	       */
/*	  REFERENCE CHART ISO CODE AND ASSOCIATED RELATIONSHIPS	       */
/*	   The Honeywell Computer Journal, 1971, Vol. 5, No. 3	       */

   call set_language (french, english, "anglais");
   call set_language (french, french, "francais");
   call set_language (french, german, "allemand");
   call set_language (french, spanish, "espagnol");

   call set_month_name (french, Jan, "jan", "Janvier");
   call set_month_name (french, Feb, "fev", "Fevrier");
   call set_month_name (french, Mar, "mars", "Mars");
   call set_month_name (french, Apr, "avr", "Avril");
   call set_month_name (french, May, "mai", "Mai");
   call set_month_name (french, Jun, "juin", "Juin");
   call set_month_name (french, Jul, "jul", "Juillet");
   call set_month_name (french, Aug, "aout", "Aout");
   call set_month_name (french, Sep, "sep", "Septembre");
   call set_month_name (french, Oct, "oct", "Octobre");
   call set_month_name (french, Nov, "nov", "Novembre");
   call set_month_name (french, Dec, "dec", "Decembre");

   call set_day_name (french, Mon, "lun", "Lundi");
   call set_day_name (french, Tue, "mar", "Mardi");
   call set_day_name (french, Wed, "mer", "Mercredi");
   call set_day_name (french, Thu, "jeu", "Jeudi");
   call set_day_name (french, Fri, "ven", "Vendredi");
   call set_day_name (french, Sat, "sam", "Samedi");
   call set_day_name (french, Sun, "dim", "Dimanche");

   call set_offset (french, Year,   "an",  "annees",   "annee",   "cette");
   call set_offset (french, Month,  "m",   "mois",     "mois",    "ce");
   call set_offset (french, Week,   "sem", "semaines", "semaine", "cette");
   call set_offset (french, Day,    "j",   "jours",    "jour",    "ce");
   call set_offset (french, Hour,   "hr",  "heures",   "heure",   "cette");
   call set_offset (french, Minute, "min", "minutes",  "minute",  "cette");
   call set_offset (french, Second, "sec", "secondes", "seconde", "cette");
   call set_offset (french, Microsecond, "usec", "microsecondes",
      "microseconde", "cette");

   call set_word (french, Before, "avant", "?");
   call set_word (french, On, "sur", "?");
   call set_word (french, After, "apres", "?");
   call set_word (french, Or, "ou", "?");
   call set_word (french, Noon, "midi", "?");
   call set_word (french, Midnight, "minuit", "?");
   call set_word (french, Now, "maintenant", "?");
   call set_word (french, Today, "aujourd'hui", "?");
   call set_word (french, Yesterday, "hier", "?");
   call set_word (french, Tomorrow, "demain", "?");
   call set_word (french, FiscalWeek, "SF", "?");

   call set_zone (french, "ut  ", "tu  ", 0, "Temps Universel");
   call set_zone (french, "fwt ", "hfh ", +1, "Heure Francaise d'Hiver");
   call set_zone (french, "fst ", "hfe ", +2, "Heure Francaise d'Ete");


/*   +-+ +-+ +-+ +-+ +-+ +-+ german language values +-+ +-+ +-+ +-+ +-+ +-+  */

   call set_language (german, english, "englisch");
   call set_language (german, french, "franzosisch");
   call set_language (german, german, "deutsch");
   call set_language (german, spanish, "spanisch");


   call set_month_name (german, Jan, "Jan", "Januar");
   call set_month_name (german, Feb, "Feb", "Februar");
   call set_month_name (german, Mar, "Mrz", "M{rz");
   call set_month_name (german, Apr, "Apr", "April");
   call set_month_name (german, May, "Mai", "Mai");
   call set_month_name (german, Jun, "Jun", "Juni");
   call set_month_name (german, Jul, "Jul", "Juli");
   call set_month_name (german, Aug, "Aug", "August");
   call set_month_name (german, Sep, "Sep", "September");
   call set_month_name (german, Oct, "Okt", "Oktober");
   call set_month_name (german, Nov, "Nov", "November");
   call set_month_name (german, Dec, "Dez", "Dezember");

   call set_day_name (german, Mon, "Mo", "Montag");
   call set_day_name (german, Tue, "Di", "Dienstag");
   call set_day_name (german, Wed, "Mi", "Mittwoch");
   call set_day_name (german, Thu, "Do", "Donnerstag");
   call set_day_name (german, Fri, "Fr", "Freitag");
   call set_day_name (german, Sat, "Sa", "Samstag");
   call set_day_name (german, Sun, "So", "Sonntag");

   call set_offset (german, Year,  "J",   "Jahre",   "Jahr",   "dieses");
   call set_offset (german, Month, "Mt",  "Monaten", "Monat",  "dieser");
   call set_offset (german, Week,  "Wo",  "Wochen",  "Woche",  "diese");
   call set_offset (german, Day,   "T",   "Tage",    "Tag",    "dieser");
   call set_offset (german, Hour,  "St",  "Stunden", "Stunde", "diese");
   call set_offset (german, Minute,"Min", "Minuten", "Minute", "diese");
   call set_offset (german, Second,"Sek", "Sekunden","Sekunde","diese");
   call set_offset (german, Microsecond, "Usek", "Mikrosekunden",
      "Mikrosekunde", "diese");

   call set_word (german, Before, "vor", "?");
   call set_word (german, Or, "oder", "?");
   call set_word (german, After, "nach", "?");
   call set_word (german, On, "am", "?");
   call set_word (german, Noon, "mittag", "?");
   call set_word (german, Midnight, "mitternacht", "?");
   call set_word (german, Now, "jetzt", "?");
   call set_word (german, Today, "heute", "?");
   call set_word (german, Yesterday, "gestern", "?");
   call set_word (german, Tomorrow, "morgen", "?");
   call set_word (german, FiscalWeek, "FW", "?");

   call set_zone (german, "nst ", "nst ", -3.5,  "Neufundlandzeit");
   call set_zone (german, "gst ", "gst ", -3,  "Groenlandzeit");
   call set_zone (german, "cet ", "cet ", +1,  "Central European Time");
   call set_zone (german, "met ", "mez ", +1,  "Mitteleuropaeische Zeit");
   call set_zone (german, "mewt", "mewz", +1,  "Mitteleuropaeische Winterzeit");
   call set_zone (german, "swt ", "swt ", +1,  "Schwedische Winterzeit");
   call set_zone (german, "fwt ", "fwt ", +1,  "Franzoesische Winterzeit");
   call set_zone (german, "mest", "mesz", +2,  "Mitteleuropaeische Sommerzeit");
   call set_zone (german, "eet ", "eet ", +2,  "Osteuropaeische Zeit");
   call set_zone (german, "sst ", "sst ", +2,  "Schwedische Sommerzeit");
   call set_zone (german, "fst ", "fst ", +2,  "Franzoesische Sommerzeit");
   call set_zone (german, "ist ", "ist ",+5.5, "Indische Standardzeit");
   call set_zone (german, "jt  ", "jt  ",+7.5,"Javazeit");
   call set_zone (german, "cct ", "cct ", +8, "Chinsische Kuerstenzeit");
   call set_zone (german, "jst ", "jst ", +9, "Japanische Standardzeit");
   call set_zone (german, "sast", "sast", +9.5,
      				      "Suedaustralische Standardzeit");
   call set_zone (german, "nzst", "nzst", +12,"Neuzeeland Standardzeit");
   

/*  +-+ +-+ +-+ +-+ +-+ +-+ spanish language values +-+ +-+ +-+ +-+ +-+ +-+  */

   call set_language (spanish, english, "ingles");
   call set_language (spanish, french, "frances");
   call set_language (spanish, german, "aleman");
   call set_language (spanish, spanish, "espa|ol");


   call set_month_name (spanish, Jan, "Ene", "Enero");
   call set_month_name (spanish, Feb, "Feb", "Febrero");
   call set_month_name (spanish, Mar, "Marzo", "Marzo");
   call set_month_name (spanish, Apr, "Abr", "Abril");
   call set_month_name (spanish, May, "May", "Mayo");
   call set_month_name (spanish, Jun, "Jun", "Junio");
   call set_month_name (spanish, Jul, "Jul", "Julio");
   call set_month_name (spanish, Aug, "Ago", "Agosto");
   call set_month_name (spanish, Sep, "Sep", "Septiembre");
   call set_month_name (spanish, Oct, "Oct", "Octubre");
   call set_month_name (spanish, Nov, "Nov", "Noviembre");
   call set_month_name (spanish, Dec, "Dic", "Diciembre");

   call set_day_name (spanish, Mon, "Lun", "Lunes");
   call set_day_name (spanish, Tue, "Mar", "Martes");
   call set_day_name (spanish, Wed, "Mie", "Mi{rcoles");
   call set_day_name (spanish, Thu, "Jue", "Jueves");
   call set_day_name (spanish, Fri, "Vie", "Viernes");
   call set_day_name (spanish, Sat, "Sab", "Sabado");
   call set_day_name (spanish, Sun, "Dom", "Domingo");

   call set_offset (spanish, Year, "a}o", "a}oa", "a}o", "este");
   call set_offset (spanish, Month, "mes", "meses", "mes", "este");
   call set_offset (spanish, Week, "semana", "semanas", "sem", "esta");
   call set_offset (spanish, Day, "dia", "dias", "dia",  "este");
   call set_offset (spanish, Hour, "hora", "horas", "hr", "esta");
   call set_offset (spanish, Minute, "minuto", "minutos", "min", "este");
   call set_offset (spanish, Second, "segundo", "segundos", "seg", "este");
   call set_offset (spanish, Microsecond, "microsegundo",
				"microsegundos", "useg", "este");

   call set_word (spanish, Before,    "antes", "?");
   call set_word (spanish, On,        "en", "?");
   call set_word (spanish, After,     "despues", "?");
   call set_word (spanish, Or,        "oh", "?");
   call set_word (spanish, Noon,      "mediodia", "?");
   call set_word (spanish, Midnight,  "medianoche", "?");
   call set_word (spanish, Now,       "ahorta", "?");
   call set_word (spanish, Today,     "hoy", "?");
   call set_word (spanish, Yesterday, "ayer", "?");
   call set_word (spanish, Tomorrow,  "ma}ana", "?");
   call set_word (spanish, FiscalWeek,"SF", "?");

   call build;
   return;

setup_error:
      call com_err_ (0, me, """call setup()"" must be done first.");
exit:
      return;%page;
/*****		     Internal Support Procedures		    ****/

setup: proc;


/* compose: off */
dcl get_temp_segments_ entry (char(*), (*) ptr, fixed bin(35));
dcl hcs_$make_entry entry (ptr, char(*), char(*), entry, fixed bin(35));
dcl i		fixed bin;

      if db_sw
      then call ioa_ ("setup:");
      call hcs_$make_entry (null(), "date_time_", "valid_format",
         valid_format, code);
      if (code ^= 0)
      then call hcs_$make_entry (null(), "new_date_time_", "valid_format",
	  valid_format, code);
      if (code ^= 0)
      then do;
         call com_err_ (code, me, "
The known version of date_time_ does not contain the entrypoint
$valid_format.  The  new version of  date_time_ being installed
must exist in  the  working  directory  with the  added name of
new_date_time_ before time_info_ can be generated.
");
         goto exit;
      end;
      call get_temp_segments_ (me, temp_p, code);
      if (code ^= 0)
      then do;
         call com_err_ (code, me, "getting tempsegs");
         goto exit;
      end;
      seg_p = temp_p (1);
      ZONE_p = addr (HOLD.z);
      string (zone_def) = ""b;
      HOLD.next_list_p = addr (HOLD.begin_list);
/**** Make room for a few entries which may need to be generated.	       */
      zones_filled = the_zone_count + 12;
      ZONE.zone_id (*) = "";
      zones_filled = 0;
      token_ct = 0;
      keywords_filled = 3;

/**** Begin building the table image				       */
      time_info.version = Vtime_info_2;
      time_info.gmt_zone_index = -1;	/* set undefined		       */
      time_info.default_language_index = Default_Language;

      time_info.date_time_keywords.number_kwd = the_keyword_count;
      time_info.date_time_keywords.pad = 0;
      time_info.date_time_keywords.name (site_date) = "";
      time_info.date_time_keywords.name (site_date_time) = "";
      time_info.date_time_keywords.name (site_time) = "";

      time_info.language_names.number_lang = the_language_count;
      time_info.language_names.pad = 0;

      time_info.month_names.number_lang = the_language_count;
      time_info.month_names.pad = 0;

      time_info.day_names.number_lang = the_language_count;
      time_info.day_names.pad = 0;

      time_info.offset_names.number_lang = the_language_count;
      time_info.offset_names.number_offset = the_offset_count;

      time_info.word_names.number_lang = the_language_count;
      time_info.word_names.number_word = the_word_count;

      call set_format ("multics_date",	   "^my/^dm/^yc");
      call set_format ("multics_date_time", "^my/^dm/^yc  ^Hd^99v.9MH ^xxxxza^xxxda");
      call set_format ("multics_time",	   "^Hd:^MH");

      do i = 1 to the_language_count;
         call set_word (i, AM, "AM", "A");
         call set_word (i, PM, "PM", "P");
      end;
				/* compose: on */
   end setup;
/**** This variable is set by setup for use by set_format.		       */
dcl valid_format	automatic
		entry (char(*), fixed bin, fixed bin(35));

%page;
set_format: proc (kwd, fmt);

dcl kwd		char (*),		/* keyword to set		       */
    fmt		char (*);		/* format to associate with it       */

      if (kwd = "date") | (kwd = "time") | (kwd = "date_time")
      then do;
         call ioa_ ("ERROR: Process default keyword may not be defined. ^a",
	  kwd);
         goto err_return;
      end;
      if (length (fmt) > 128)
      then do;
         call ioa_ ("ERROR: format string >128 characters. ^a", fmt);
         goto err_return;
      end;
      errloc = 0;
      if (index (fmt, "^") = 0)	/* You must ask for SOMETHING!       */
      then code = error_table_$dt_no_format_selector;
      else call valid_format ((fmt), errloc, code);
      if (code ^= 0)
      then do;
         call com_err_ (code, me, "set_format ^a^[
Format is: ""^va""
 ERROR at: ^vx^^^]", kwd,
	  (errloc > 0), length (fmt), fmt, errloc);
         return;
      end;
      if (kwd = "system_date")
      then do;
         time_info.date_time_keywords.name (site_date) = kwd;
         time_info.date_time_keywords.str (site_date) = fmt;
         return;
      end;
      if (kwd = "system_date_time")
      then do;
         time_info.date_time_keywords.name (site_date_time) = kwd;
         time_info.date_time_keywords.str (site_date_time) = fmt;
         return;
      end;
      if (kwd = "system_time")
      then do;
         time_info.date_time_keywords.name (site_time) = kwd;
         time_info.date_time_keywords.str (site_time) = fmt;
         return;
      end;
      if (keywords_filled = the_keyword_count)
      then do;
         call ioa_ ("Too many keywords supplied, the_keyword_count is only = ^i",
	  the_keyword_count);
         goto err_return;
      end;
/**** if (keywords_filled = 3) then key_pos = 4; else */
      key_pos = 3;
      if (keywords_filled > 3)
      then
         do key_pos = keywords_filled to 4 by -1;
         if (time_info.date_time_keywords.name (key_pos) = kwd)
         then do;
	  call ioa_ ("ERROR: Duplicate keyword. ^a", kwd);
	  goto err_return;
         end;
         if (time_info.date_time_keywords.name (key_pos) < kwd)
         then goto insert;
         time_info.date_time_keywords.name (key_pos+1) = time_info.date_time_keywords.name (key_pos);
         time_info.date_time_keywords.str (key_pos+1) = time_info.date_time_keywords.str (key_pos);
      end;
insert:
      keywords_filled = keywords_filled + 1;
      time_info.date_time_keywords.name (key_pos+1) = kwd;
      time_info.date_time_keywords.str (key_pos+1) = fmt;
      return;

err_return:
      err_sw = "1"b;
      return;

dcl errloc	fixed bin;
dcl error_table_$dt_no_format_selector fixed bin(35) ext static;
dcl key_pos	fixed bin;

end set_format;%page;
set_language: proc (lang, element, value);

dcl lang		fixed bin,	/* which language to set	       */
    element	fixed bin,	/* which element to set	       */
    value		char (*);		/* value to put there	       */

      if db_sw
      then call ioa_ ("lang(^i,^i) '^a'", lang, element, value);
				/* compose: off */
      if (seg_p = null ())
      then goto setup_error;

      if (value = "")
      then call no_value_msg ("language", lang, char (element));
      if (length (time_info.language_names.name (lang, element)) ^= 0)
      then call reset_msg ("Language", lang, ltrim (char (element)),
	    time_info.language_names.name (lang, element), value);
      time_info.language_names.name (lang, element) = value;
				/* compose: on */
      return;

   end set_language;%skip(2);
set_month_name: proc (lang, element, short, long);

dcl lang		fixed bin,	/* which language to set	       */
    element	fixed bin,	/* which element to set	       */
    (short, long)	char (*);		/* values to put there	       */

      if db_sw
      then call ioa_ ("month(^i,^i) '^a' '^a'", lang, element, short, long);
				/* compose: off */
      if (seg_p = null ())
      then goto setup_error;

      if (short = "")
      then call no_value_msg ("short month", lang, mo_name (element));
      if (length (time_info.month_names.short (lang, element)) ^= 0)
      then call reset_msg ("Short month", lang, mo_name (element),
	    time_info.month_names.short (lang, element), short);
      time_info.month_names.short (lang, element) = short;

      if (long = "")
      then call no_value_msg ("long month", lang, mo_name (element));
      if (length (time_info.month_names.long (lang, element)) ^= 0)
      then call reset_msg ("Long month", lang, mo_name (element),
	    time_info.month_names.long (lang, element), long);
      time_info.month_names.long (lang, element) = long;
				/* compose: on */

      return;

   end set_month_name;%page;
set_day_name: proc (lang, element, short, long);

dcl lang		fixed bin,	/* which language to set	       */
    element	fixed bin,	/* which element to set	       */
    (short, long)	char (*);		/* values to put there	       */

      if db_sw
      then call ioa_ ("day(^i,^i) '^a' '^a'", lang, element, short, long);
				/* compose: off */
      if (seg_p = null ())
      then goto setup_error;

      if (short = "")
      then call no_value_msg ("short day", lang, da_name (element));
      if (length (time_info.day_names.short (lang, element)) ^= 0)
      then call reset_msg ("Short day", lang, da_name (element),
	    time_info.day_names.short (lang, element), short);
      time_info.day_names.short (lang, element) = short;

      if (long = "")
      then call no_value_msg ("Long day", lang, da_name (element));
      if (length (time_info.day_names.long (lang, element)) ^= 0)
      then call reset_msg ("Long day", lang, da_name (element),
	    time_info.day_names.long (lang, element), long);
      time_info.day_names.long (lang, element) = long;
				/* compose: on */

      return;

   end set_day_name;%page;
set_offset: proc (lang, element, short, plural, singular, this);

dcl lang		fixed bin,	/* which language< to set	       */
    element	fixed bin,	/* which element to set	       */
    (short, plural, singular, this)
		char (*);		/* values to put there	       */

      if db_sw
      then call ioa_ ("offset(^i,^i) '^a' '^a' '^a'",
         lang, element, short, plural, singular);
				/* compose: off */
      if (seg_p = null ())
      then goto setup_error;

      if (short = "")
      then call no_value_msg ("Short offset", lang, of_name (element));
      if (length (time_info.offset_names.short (lang, element)) ^= 0)
      then call reset_msg ("Short offset", lang, of_name (element),
	    time_info.offset_names.short (lang, element), (short));
	   time_info.offset_names.short (lang, element) = short;

      if (plural = "")
      then call no_value_msg ("Plural offset", lang, of_name (element));
      if (length (time_info.offset_names.plural (lang, element)) ^= 0)
      then call reset_msg ("Plural offset", lang, of_name (element),
	    time_info.offset_names.plural (lang, element), plural);
      time_info.offset_names.plural (lang, element) = plural;

      if (singular = "")
      then call no_value_msg ("singular offset", lang, of_name (element));
      if (length (time_info.offset_names.singular (lang, element)) ^= 0)
      then call reset_msg ("Singular offset", lang, of_name (element),
	    time_info.offset_names.singular (lang, element), singular);
      time_info.offset_names.singular (lang, element) = singular;

      if (this = "")
      then call no_value_msg ("This-word", lang, of_name (element));
      if (length (time_info.offset_names.this (lang, element)) ^= 0)
      then call reset_msg ("This-word", lang, of_name (element),
	    time_info.offset_names.this (lang, element), (this));
      time_info.offset_names.this (lang, element) = this;
				/* compose: on */
      return;

   end set_offset;%page;
set_word: proc (lang, element, value, v);

dcl lang		fixed bin,	/* which language to set	       */
    element	fixed bin,	/* which element to set	       */
    (value, v)	char (*);		/* long&short value to put there     */

      if db_sw
      then call ioa_ ("word(^i,^i) '^a'^a'", lang, element, value, v);
				/* compose: off */
      if (seg_p = null ())
      then goto setup_error;

      if (value = "")
      then call no_value_msg ("word", lang, wo_name (element));
      if (length (time_info.word_names.word (lang, element)) ^= 0)
      then call reset_msg ("Word", lang, wo_name (element),
	    time_info.word_names.word (lang, element), value);
      time_info.word_names.word (lang, element) = value;

      if (v = "")
      then call no_value_msg ("word", lang, wo_name (element));
      if (length (time_info.word_names.short (lang, element)) ^= 0)
      then call reset_msg ("Word", lang, wo_name (element),
	    time_info.word_names.short (lang, element), v);
      time_info.word_names.short (lang, element) = v;
      return;
				/* compose: on */

   end set_word;%page;
set_zone: proc (lang, id, brief, interval, long);

dcl lang		fixed bin,	/* which language to set	       */
    id		char (*),		/* which element to set	       */
    (brief, long)	char (*),		/* values to put there	       */
    interval	fixed dec (12, 8);	/* hour_value+GMT gives this zone    */

dcl element	fixed bin;
dcl range_check	bit (1) init (""b);
dcl ferr_sw	bit (1) init ("1"b);
				/* compose: off */
      if db_sw
      then call ioa_ ("zone(^2i,^4a) ^7f '^a' '^a'", 
         lang, id, interval, brief, long);

      if (seg_p = null ())
      then goto setup_error;

      if (id = "")
      then call no_value_msg ("zone id", lang, brief);
      range_check = "1"b;

add_zone: entry (lang, id, brief, interval, long);

      if (zones_filled = 0)
      then zones_filled = 1;
      else do;
         do element = 1 to zones_filled;
	  if (ZONE.zone_id (element) = id)
	  then goto found_id;
         end;
         if (zones_filled >= the_zone_count) & range_check
         then do;
	  if ferr_sw
	  then do;
	     ferr_sw = ""b;
	     call ioa_ ("ERROR: at ^a (^a,^a)", id, brief, interval);
	  end;
	  call ioa_ ("Too many zones supplied, the_zone_count is only = ^i",
	     the_zone_count);
	  err_sw = "1"b;
	  return;
         end;
         zones_filled = zones_filled + 1;
      end;
      element = zones_filled;
      ZONE.zone_id (element) = id;
found_id:
      if (length (ZONE.zone.short (element, lang)) ^= 0)
      then call reset_msg ("Brief zone", lang, ZONE.zone_id (element),
	    ZONE.zone.short (element, lang), brief);
      ZONE.zone.short (element, lang) = rtrim (brief);

      if (length (ZONE.zone.long (element, lang)) ^= 0)
      then call reset_msg ("Long zone", lang, ZONE.zone_id (element),
	    ZONE.zone.long (element, lang), long);
      ZONE.zone.long (element, lang) = long;

      if (interval < -11) | (interval > 13)
      then do;
         if ferr_sw
         then do;
	  ferr_sw = ""b;
	  call ioa_ ("ERROR: at ^a (^a,^a)", id, brief, interval);
         end;
         call ioa_ ("zone interval(^f) must be in the range -11<x<13.",
	  interval);
         err_sw = "1"b;
      end;
      else do;
         ZONE.zone.delta (element, lang) = -interval;
         if (interval = floor (interval))
         then zone_def (interval) = "1"b;
      end;
      if (time_info.gmt_zone_index < 0) & (interval = 0)
      then time_info.gmt_zone_index  = element;

      return;
				/* compose: on */

   end set_zone;%page;
build: proc;
				/* compose: off */
/**** Things to do in this routine:				       */
/****	generate any needed "zmX" and "zpX" zones		       */
/****	make sure gmt_zone_index got set			       */
/****	check for missing values in Fill_From language		       */
/****	fill in values from Fill_From language			       */
/****	check for missing values on everybody			       */
/****	check for zone ids having different values in different languages  */
/****	sort zones into order by interval			       */
/****	prepare binary search list for CBTB to use		       */
/****	check ambiguity (1) same string, different meaning, different lang */
/****	i.e. you can't tell the meaning w/o help of some other word	       */
/****	check ambiguity (2) same string,   same    meaning, different lang */
/****	i.e. you can't tell the language w/o help of some other word       */


      if (seg_p = null ())
      then goto setup_error;

/****	check for proper number of keywords set			       */
      if (keywords_filled < the_keyword_count)
      then do;
         call ioa_ ("ERROR: the_keyword_count is ^i, but only ^i keywords are set.",
	  the_keyword_count, keywords_filled);
         err_sw = "1"b;
      end;

/****	check for proper number of zones set			       */
      if (zones_filled < the_zone_count)
      then do;
         call ioa_ ("ERROR: the_zone_count is ^i, but only ^i zones are set.",
	  the_zone_count, zones_filled);
         err_sw = "1"b;
      end;

/****	make sure gmt_zone_index got set			       */
       if (time_info.gmt_zone_index < 0)
       then do;
	call ioa_ ("ERROR: No GMT zone has been set.");
	err_sw = "1"b;
       end;

/****     check for missing world zones.			       */
       first_sw = "1"b;
       done = ""b;
       do while (^done);
	done = "1"b;
	element = index (string (zone_def), "0"b);
	if (element > 0)
	then do;
	   done = ""b;
	   element = element - 12;
/**** Some of this may look strange, but historically the Multics offsets    */
/**** are of the opposite sign from those mentioned in the standards.  So    */
/**** we have to be sure to change sign when going from ext->int or int->ext */
	   ch4 = ltrim (char (abs (element)));
	   ch15 = "GMT +00 hours.";
	   substr (ch15, 6, 2) = ch4;
	   if (element < 0)
	   then do;
	      ch4 = "zm" || ch4;
	      substr (ch15, 5, 1) = "-";
	   end;
	   else ch4 = "zp" || ch4;
	   call add_zone (Fill_From, (ch4), (ch4), (element), (ch15));
	   if first_sw
	   then do;
	      first_sw = ""b;
	      call ioa_ ("
This procedure checks for the presence of at least 1 name for
each hourly time zone.  If any zone has no names, a name is
generated for that slot.  This is not an error.");
	   end;
	   call ioa_ ("  Generated zone: ^a", ch4);
	end;
       end;

/****	check for missing values in Fill_From language		       */
      do element = 1 to zones_filled;
         if (length (ZONE.zone.short (element, Fill_From)) = 0)
         then do;
	  call ioa_ (
	     "ERROR: Zone ^a in the Fill_From language (^a) isn't set.",
	     ZONE.zone_id (element), get_lang (Fill_From));
	  err_sw = "1"b;
         end;
      end;

/*	fill in values from Fill_From language			       */
      do lang = 1 to the_language_count;
         do element = 1 to zones_filled;
	  if (length (ZONE.zone.short (element, lang)) = 0)
	  then do;
	     ZONE.zone.short (element, lang)
	        = ZONE.zone.short (element, Fill_From);
	     ZONE.zone.long (element, lang)
	        = ZONE.zone.long (element, Fill_From);
	     ZONE.zone.delta (element, lang)
	        = ZONE.zone.delta (element, Fill_From);
	  end;
         end;
      end;

/**** Build the token/item tables, checking for missing values.	       */
      do lang = 1 to the_language_count;
         do element = 1 to the_language_count;
	  if (length (time_info.language_names.name (lang, element)) = 0)
	  then do;
	     call ioa_ ("ERROR: Language (^a, ^a) has not been specified.",
	        get_lang (lang), get_lang (element));
	     err_sw = "1"b;
	  end;
	  if ^err_sw
	  then do;
	     call add_token (time_info.language_names.name (lang, element),
	        Language_table, lang, element);
	  end;
         end;

         do element = 1 to zones_filled;
	  if (length (ZONE.zone.short (element, lang)) = 0)
	  then do;
	     call ioa_ ("ERROR: Zone ""^a"" is not specified in ^a.",
	        ZONE.zone_id (element), get_lang (lang));
	     err_sw = "1"b;
	  end;
         end;

         do element = 1 to 12;
	  if (length (time_info.month_names.short (lang, element)) = 0)
	  then do;
	     call ioa_ ("ERROR: Month ""^a"" is not specified in ^a.",
	        mo_name (element), get_lang (lang));
	     err_sw = "1"b;
	  end;
	  if ^err_sw
	  then do;
	     call add_token (time_info.month_names.short (lang, element),
	        Month_table, lang, element);
	     call add_token (time_info.month_names.long (lang, element),
	        Month_table, lang, element);
	  end;
         end;

         do element = 1 to 7;
	  if (length (time_info.day_names.short (lang, element)) = 0)
	  then do;
	     call ioa_ ("ERROR: Day ""^a"" is not specified in ^a.",
	        da_name (element), get_lang (lang));
	     err_sw = "1"b;
	  end;
	  if ^err_sw
	  then do;
	     call add_token (time_info.day_names.short (lang, element),
	        Day_table, lang, element);
	     call add_token (time_info.day_names.long (lang, element),
	        Day_table, lang, element);
	  end;
         end;

         do element = 1 to the_offset_count;
	  if (length (time_info.offset_names.short (lang, element)) = 0)
	  then do;
	     call ioa_ ("ERROR: Offset ""^a"" is not specified in ^a.",
	        of_name (element), get_lang (lang));
	     err_sw = "1"b;
	  end;
	  if ^err_sw
	  then do;
	     call add_token (time_info.offset_names.short (lang, element),
	        Offset_table, lang, element);
	     call add_token (time_info.offset_names.plural (lang, element),
	        Offset_table, lang, element);
	     call add_token (time_info.offset_names.singular (lang, element),
	        Offset_table, lang, element);
	     call add_token (time_info.offset_names.this (lang, element),
	        This_table, lang, element);
	  end;
         end;

         do element = 1 to the_word_count;
	  if (length (time_info.word (lang, element)) = 0)
	  then do;
	     call ioa_ ("ERROR: Word ""^a"" is not specified in ^a.",
	        wo_name (element), get_lang (lang));
	     err_sw = "1"b;
	  end;
	  if ^err_sw
	  then do;
	     call add_token (time_info.word_names.word (lang, element),
	        Word_table, lang, element);
	     call add_token (time_info.word_names.short (lang, element),
	        Word_table, lang, element);
	  end;
         end;
      end;

/*	check for zone ids having different values in different languages  */
      do element = 1 to zones_filled;
         do lang = 1 to Fill_From - 1, Fill_From + 1 to the_language_count;
	  if (ZONE.zone.delta (element, Fill_From))
	     ^= (ZONE.zone.delta (element, lang))
	  then do;
	     call ioa_ (
	        "ERROR: Zone offset ""^a"" is not the same in ^a and ^a.",
	        ZONE.zone_id (element), get_lang (lang),
	        get_lang (Fill_From));
	     err_sw = "1"b;
	  end;
         end;
      end;

/*	sort zones into order by interval			       */
      do i = 1 to zones_filled;	/* The methodology used here is very */
         ZONE.zone_order (i) = i;	/*  simple instead of being fast.    */
      end;			/*  This procedure will seldom be    */
				/*  used, thus it is not worth       */
      done = ""b;			/*  wringing CPU time out of it.     */
      do while (^done);
         done = "1"b;
         do i = 2 to zones_filled;
	  t1 = ZONE.zone_order (i - 1);
	  t2 = ZONE.zone_order (i);
	  if (ZONE.zone.delta (t1, 1) < ZONE.zone.delta (t2, 1))
	  then do;
	     done = ""b;
	     ZONE.zone_order (i - 1) = t2;
	     ZONE.zone_order (i) = t1;
	  end;
         end;
      end;
      time_info.gmt_zone_index = ZONE.zone_order (time_info.gmt_zone_index);

/*	fill in the ordered zone table			       */
      time_info.zone_names.number_lang = the_language_count;
      time_info.zone_names.number_zone = zones_filled;

      do t1 = 1 to zones_filled;
         element = ZONE.zone_order (t1);
         do lang = 1 to the_language_count;
	  time_info.zone_names (lang, t1).delta
	     = ZONE.zone (element, lang).delta * microseconds_per_hour;
	  time_info.zone_names (lang, t1).long
	     = ZONE.zone (element, lang).long;
	  time_info.zone_names (lang, t1).short
	     = ZONE.zone (element, lang).short;
	  call add_token (time_info.zone_names.short (lang, t1),
	     Zone_table, lang, t1);
         end;
      end;

/*	prepare binary search list for CDTB to use		       */
      time_info.tokens.count = token_ct;
      time_info.tokens.ambig = ""b;
      item_p = addr (time_info.tokens.item_space);
      item_size = 0;
      first_sw, msg_sw = "1"b;
      do element = 1 to token_ct;
         if db_sw
         then call ioa_ ("token(^i)=""^a""", element, HOLD.symbol (element));
         time_info.tokens.symbol (element) = HOLD.symbol (element);
         time_info.tokens.list_r (element) = rel (item_p);
         item.count = 0;
         do an_item_p = HOLD.list_p (element) repeat (an_item.next)
	  while (an_item_p ^= null ());
	  do ii = 1 to item.count;
	     if (item.table (ii) = (an_item.table))
	        & (item.element (ii) = an_item.elem)
	     then do;
	        item.ambig (ii) = item.ambig (ii) | an_item.ambig;
	        item.in_lang (ii)
		 = item.in_lang (ii) | an_item.lang;
	        goto end_an_item;
	     end;
	  end;
	  item.count = item.count + 1;
	  item.ambig (item.count) = an_item.ambig;
	  item.table (item.count) = an_item.table;
	  item.element (item.count) = an_item.elem;
	  item.in_lang (item.count) = an_item.lang;
end_an_item:
         end;
         if (item.count > 1)
         then do;
	  lang_check, lang_use = ""b;
	  do ii = 1 to item.count;
	     if (item.table (ii) = This_table)
	     then;		/* The "this" is used on all offsets */
				/* ..and may have the same token     */
				/* ..on several of them.  This is    */
				/* ..not a ambiguity. Forget it.     */
	     else do;
	        lang_check = item.in_lang (ii) & lang_use;
	        lang_use = item.in_lang (ii) | lang_use;
	     end;
	  end;
	  if (lang_check ^= ""b)
	  then do;
	     if msg_sw
	     then do;
	        call ioa_ ("
When a token is not unique within a language, some date/time
strings which use this token may be ambiguous and therefore
unparsable.  The date/time software is prepared to handle
such ambiguities.  This is not an error.");
	        msg_sw = ""b;
	     end;
	     call ioa_ ("  Non-unique token ""^a""", HOLD.symbol (element));
	  end;

	  if (item.table (1) ^= This_table)
	  then do;
	     if first_sw
	     then do;
	        call ioa_ ("
When a token does not have the same meaning in all languages,
some date/time strings which use this token may be ambiguous
and therefore unparsable.  The date/time software is prepared
to handle such ambiguities.  This is not an error.");
	        first_sw = ""b;
	     end;
	     call ioa_ ("  Ambiguous token: ^a", HOLD.symbol (element));
	  end;
         end;
         item_size = item_size + (item.count + 2);
         item_p = addrel (item_p, (item.count + 2));
      end;

/**** Fill in arg struc for create_data_segment_.			       */

      if err_sw
      then do;
         call com_err_ (0, "time_info_",
	  "An error has occurred. No new table generated.");
         return;
      end;
      cdsa.sections (1).p = seg_p;
      cdsa.sections (1).len = size (time_info);
      cdsa.sections (1).struct_name = "time_info";
      
      cdsa.sections (2).p = null();
      cdsa.sections (2).len = 0;
      cdsa.sections (2).struct_name = "";
      
      cdsa.seg_name = "time_info_";

      cdsa.num_exclude_names = 0;
      cdsa.exclude_array_ptr = null();
      cdsa.defs_in_link = ""b;
      cdsa.separate_static = ""b;
      cdsa.have_text = "1"b;
      cdsa.have_static = ""b;
      cdsa.pad = ""b;

      call create_data_segment_ (addr (cdsa), code);
      if code ^= 0
      then call com_err_ (code, "time_info_");

      call release_temp_segments_ (me, temp_p, code);
      if (code ^= 0)
      then do;
         call com_err_ (code, me, "releasing tempsegs");
         goto exit;
      end;

dcl release_temp_segments_ entry (char(*), (*) ptr, fixed bin(35));
dcl lang_check	bit (18)aligned;
dcl lang_use	bit (18)aligned;
dcl lang		fixed bin;
dcl element	fixed bin;
dcl done		bit (1);
dcl i		fixed bin;
dcl ii		fixed bin;
dcl msg_sw	bit (1);
dcl first_sw	bit (1);
dcl ch4		char (4)var;
dcl ch15		char (16);
dcl (t1, t2)	fixed bin;
				/* compose: on */

   end build;

/* compose: off */
add_token: proc (tok, tab_id, lang_id, elem_id);
dcl (tok		char (*) var,
    tab_id	fixed bin,	/* which table is it in	       */
    lang_id	fixed bin,	/* which language is it in	       */
    elem_id	fixed bin		/* which element in table is it      */
    )		parm;

dcl symb		char (32) var;

      if (tok = "?")
      then return;
      symb = translate (tok,
         "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
      if (token_ct = 0)
      then cur_token = 1;
      else do;
         lb = 1;
         hb = token_ct;
         do while (lb <= hb);
	  cur_token = divide (lb + hb, 2, 17, 0);
	  if (HOLD.symbol (cur_token) = symb)
	  then goto found_token;
	  if (HOLD.symbol (cur_token) < symb)
	  then do;
	     lb = cur_token + 1;
	     insert_point = cur_token + 1;
	  end;
	  else do;
	     hb = cur_token - 1;
	     insert_point = cur_token;
	  end;
         end;
         do cur_token = token_ct to insert_point by -1;
	  HOLD.token_list (cur_token + 1) = HOLD.token_list (cur_token);
         end;
         cur_token = insert_point;
      end;

      if db_sw
      then call ioa_ ("----new ^a|^a", symb, tok);
      token_ct = token_ct + 1;	/* insert a new token entry	       */
      HOLD.item_ct (cur_token) = 0;
      HOLD.symbol (cur_token) = symb;
      HOLD.list_p (cur_token) = null ();
      goto new_item;

found_token:
      if db_sw
      then call ioa_ ("--found ^a|^a", symb, tok);
      do an_item_p = HOLD.list_p (cur_token)
         repeat (an_item.next)
         while (an_item_p ^= null ());
         if (an_item.table = tab_id) & (an_item.elem = elem_id)
         then goto more_lang;
      end;

new_item:
      if db_sw
      then call ioa_ ("==item ^i(^i)",
         tab_id, elem_id);
      HOLD.item_ct (cur_token) = HOLD.item_ct (cur_token) + 1;
      an_item_p = HOLD.next_list_p;
      HOLD.next_list_p = addrel (HOLD.next_list_p, size (an_item));
      an_item.next = HOLD.list_p (cur_token);
      HOLD.list_p (cur_token) = an_item_p;
      an_item.ambig, an_item.lang = ""b;
      an_item.table = tab_id;
      an_item.elem = elem_id;
more_lang:
      if (an_item.lang ^= ""b) & (substr (an_item.lang, lang_id, 1) = "1"b)
      then an_item.ambig = "1"b;
      substr (an_item.lang, lang_id, 1) = "1"b;
      if db_sw
      then call ioa_ ("lang-^.3b", an_item.lang);
      return;

dcl (lb, hb)	fixed bin;
dcl cur_token	fixed bin;
dcl insert_point	fixed bin;

   end add_token;

get_lang: proc (lang) returns (char (32) var);

dcl lang		fixed bin;

dcl result	char (32) var;

      if (time_info.language_names.name (lang, Fill_From) ^= "")
      then result = time_info.language_names.name (lang, Fill_From);
      else result = ltrim (char (lang));
      return (result);

   end get_lang;

no_value_msg: proc (v1, v2, v3);

dcl v2		fixed bin,
    (v1, v3)	char (*);

      call ioa_ ("ERROR: Value not supplied when setting ^a (^a, ^a).",
         v1, get_lang (v2), v3);
      err_sw = "1"b;

   end no_value_msg;

reset_msg: proc (v1, v2, v3, v4, v5);

dcl v1		char (*),		/* what is being set	       */
    v2		fixed bin,	/* which language it's in	       */
    v3		char (*),		/* which element of table	       */
    v4		char (*) var,	/* prior value		       */
    v5		char (*);		/* new value		       */

      call ioa_ (
         "ERROR: ^a(^a,^a), with the value ""^a"", is being reset to ""^a"".",
         v1, get_lang (v2), v3, v4, v5);
      err_sw = "1"b;

   end reset_msg;

/****		  +-+ +-+automatic variables+-+ +-+ 		    ****/

dcl me		char (15) int static options (constant)
		init ("time_info_.cds");
dcl err_sw	bit (1) init ("0"b);
dcl sys_date_sw	bit (1) init ("0"b);
dcl sys_time_sw	bit (1) init ("0"b);
dcl sys_date_time_sw bit (1) init ("0"b);
dcl temp_p	(2)ptr;

/**** in this hold area, all uninitialized fields will be recognized by      */
/****  having a length of zero (this is what the temp_seg initially	       */
/****  contains).						       */
dcl ZONE_p	ptr;
dcl 1 ZONE	(zones_filled) like HOLD.z based (ZONE_p);
dcl 1 HOLD	based (temp_p(2)),
      2 token_list	(512),
        3 symbol	char (32) var,	/* lower-case form of token	       */
        3 item_ct	fixed bin,	/* # items for this token	       */
        3 list_p	ptr unal,		/* -> list of items for token	       */
      2 z		(the_zone_count),
        3 zone_id	char (16),
        3 zone_order fixed bin,
        3 zone	(the_language_count),
          4 long	char (64) var,
          4 short	char (4) var,
          4 delta	fixed dec (12, 8),
      2 zz	(12) like HOLD.z,	/* place for any generated zones     */
      2 next_list_p ptr,		/* -> next place to put an item      */
      2 begin_list	ptr;		/* 1st item based here	       */

dcl an_item_p	ptr;
dcl 1 an_item	based (an_item_p),
      2 next	ptr,
      2 table	fixed bin,	/* which table this item for	       */
      2 elem	fixed bin,	/* which element in table	       */
      2 lang	bit (18) aligned,	/* languages involved	       */
      2 ambig	bit (1) aligned;



/**** automatic arg struc for create_data_segment_.			 */
dcl 1 cdsa	aligned like cds_args;

dcl code		fixed bin (35);	/* status code.		       */
dcl zone_def	(-11:12) bit(1) unal;

dcl (abs, addr, addrel, char, divide, floor, index, length, ltrim, null, rel,
     rtrim, substr, size, string, translate
    )		builtin;

dcl com_err_	entry options (variable);
dcl ioa_		entry () options (variable);
				/* compose: on */
dcl create_data_segment_
		entry (ptr, fixed bin (35));

dcl microseconds_per_hour init (3600000000)
		fixed bin (71) int static options (constant);

/* format: off */
dcl (Jan	init (1),		Mon	init (1),
     Feb	init (2),		Tue	init (2),
     Mar	init (3),		Wed	init (3),
     Apr	init (4),		Thu	init (4),
     May	init (5),		Fri	init (5),
     Jun	init (6),		Sat	init (6),
     Jul	init (7),		Sun	init (7),
     Aug	init (8),
     Sep	init (9),		     Before	init (1),
     Oct	init (10),	     Or		init (2),
     Nov	init (11),	     After	init (3),
     Dec	init (12),	     On		init (4),
			     Noon		init (5),
     Year		init (1),	     Midnight	init (6),
     Month	init (2),      Now		init (7),
     Week		init (3),	     Yesterday	init (8),
     Day		init (4),	     Today	init (9),
     Hour		init (5),	     Tomorrow	init (10),
     Minute	init (6),	     FiscalWeek	init (11),
     Second	init (7),	     AM		init (12),
     Microsecond	init (8),	     PM		init (13)
    )		fixed bin int static options (constant);
/* format: on */
%include cds_args;

%include time_info_cds;
dcl keywords_filled	fixed bin;
dcl zones_filled	fixed bin;

%include time_info_search;
%include time_names;%skip(5);
dcl db_sw		bit (1) int static init (""b);
dbn: entry; db_sw = "1"b; return;
dbf: entry; db_sw = "0"b; return;

   end time_info_;
