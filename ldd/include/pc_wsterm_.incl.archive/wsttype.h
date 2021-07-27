/****	BEGIN INCLUDE FILE wsttype.h				       */

/* HISTORY COMMENTS:
  1) change(87-03-13,Wallman), approve(87-03-13,MCR7586),
     audit(87-07-16,Flegel), install(87-08-07,MR12.1-1072):
     First release.
  2) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
     These defines moved from "wstdefs.h" to this separate include
     resolve define conflict with ctype.h macros.
                                                   END HISTORY COMMENTS */

#ifndef WSTTYPE

/* These macros "borrowed" from ctype.h and is here so we can use them without
   the rest of the ctype.h macros overiding the function definitions for
   various other is* constructs. */

#define	U	1		/* upper case flag */
#define	L	2		/* lower case flag */
#define	N	4		/* number flag */
#define	S	8		/* space flag */
#define	P	16		/* punctuation flag */
#define	C	32		/* control character flag */
#define	B	64		/* blank flag */
#define	X	128		/* hexadecimal flag */

extern	char	_ctype[];		/* character type table */

#define	isprint(c)	(_ctype[(c)+1]&(P|U|L|N|B))

#define WSTTYPE
#endif WSTTYPE

/****	END INCLUDE FILE wsttype.h				       */
