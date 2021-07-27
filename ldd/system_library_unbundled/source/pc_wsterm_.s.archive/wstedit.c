/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1987 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(87-05-04,Wallman), approve(87-05-04,MCR7586),
     audit(87-08-10,Flegel), install(87-08-07,MR12.1-1072):
     First release.
  2) change(87-09-02,Wallman), approve(87-09-02,MCR7586),
     audit(87-08-17,Flegel), install(87-09-10,MR12.1-1103):
     PBF to improve robustness of string handling by avoiding all str*
     functions.
  3) change(88-02-24,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed debugging code and unused variables; code re-formatting.
  4) change(88-04-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Null-terminated temporary string "trash" in edit_HT to fix bug.
  5) change(88-05-18,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Deleted hide_sw reference to function replay().
  6) change(88-05-31,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added file and printer auditing support.
  7) change(88-07-12,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Replace calls to redundant move_abs() with calls to cursor_move().
  8) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
  9) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
 10) change(88-08-30,Lee), approve(88-09-12,MCR7986), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed non-edit async mode line editing routines and references
     to those routines.
                                                   END HISTORY COMMENTS */

/*  WSTEDIT - Local edit module for WSTERM                 */

/* Perform local edit action for async mode */

#include    <stdio.h>
#include    <dos.h>
#include    "wstdefs.h"
#include    "wstglob.h"
#include    <wsmincap.h>

/*
**********************************************************************

  Routine:            REPLAY 

  Function:
      This routine redisplays the input line in non-edit async mode. 

  Parameters:
     (input)          coli - specifies the column movement increment 

  Returns:            NONE 

**********************************************************************/


replay (coli)
int coli;       /* Column movement increment */
{   
    int cndx,       /* Scan index for kb.klin */
    dcol,       /* Local column position */
    dend,       /* Final column position */
    dlin,       /* Local line counter */
    dstrt,      /* Local starting column value */
    space_left,     /* Space left in current line */
    spill,      /* Line wrap spill character count */
    tab_space;  /* Tabbing space needed */

    register int    ilin;        /* Working index */

    /* Set up boundary conditions for the display */

    cndx = kb.endx;         /* Starting character */
    dstrt = ds.ccol;            /* Starting column */
    dcol = ds.ccol;
    dend = dcol + coli;     /* Ending column */
    dlin = ds.lndx;         /* Starting line index */

    if (dend > screen.maxcol)       /* Adjust ending column */
        dend %= screen.maxcol;

    if (dstrt < strlen (ds.dlin))   /* Clear needed stuff */
        putscr (EL, strlen (EL));

    ds.dlin [dstrt] = NUL;
    space_left = 0;
    spill = 0;

    /* Scan kb.klin from cndx to end */

    while (cndx < strlen (kb.klin)) {

        /* Loop thru display line */

        while (dcol < screen.maxcol && cndx < strlen (kb.klin)) { 
            kb.pos [cndx] = dcol;

            if (kb.klin [cndx] == HT)   /* Tabbing */ { 
                tab_space = next_tab (dcol);
                space_left = screen.maxcol - dcol;
                if ((space_left < tab_space) & (space_left >
                    0))
                    spill = tab_space - space_left;
                catstr (ds.dlin, spaces, tab_space,
                     "ds.dlin", sizeof (ds.dlin));

                if (spill > 0) { 
                    ds.spill [dlin] = ON;
                    if (wrap_line (dlin, dcol))   /* Non-zero if line wont fit */
                        return (ON);

                    catstr (ds.dlin, spaces,
                         spill, "ds.dlin", sizeof (ds.dlin));
                    dcol = ds.splct [dlin] = spill;
                    dstrt = 0;
                }
                else 
                    dcol = tab_space;

            } /* Control characters */

            else if (iscntrl (kb.klin [cndx])) { 
                sprintf (kb.dstr, "%c%03o", lnc, (int) kb.klin [cndx]);
                space_left = screen.maxcol - dcol;
                if ((space_left < 4) & (space_left > 0))
                    spill = (4 - space_left) % 4;

                if (space_left >= 4)        /* It fits on the line */ { 
                    catstr (ds.dlin, kb.dstr,
                         strlen (kb.dstr), "ds.dlin", sizeof (ds.dlin));

                    dcol += 4;
                }

                else /* It doesn't fit */        { 
                    if (space_left > 0)
                        catstr (ds.dlin,
                             kb.dstr, space_left, "ds.dlin",
                                    sizeof (ds.dlin));

                    putscr (ds.dlin [dstrt],
                         space_left);

                    ds.spill [dlin] = ON;
                    if (wrap_line (dlin, dcol))   /* Error if line wont fit */
                        return (ON);

                    dlin++; 
                    dstrt = 0;

                    strcpy (ds.dlin, kb.dstr [space_left]);
                    dcol = ds.splct [dlin] = spill;
                }
            }               /* End of control chars */

            else {    /* Printing graphics */
                putscr (&kb.klin [cndx], 1);

                ds.dlin [dcol++] = kb.klin [cndx];
                ds.dlin [dcol] = NUL;

            }

            cndx++;
        }               /* End of ds.lin loop */

        if (cndx < strlen (kb.klin))    /* Next line if there's more */ { 
            putscr (&ds.dlin [dstrt], strlen (&ds.dlin [dstrt]));

            if (wrap_line (dlin, dcol))   /* Error if line wont fit */
                return (ON);
            dstrt = 0;
            dcol = 0;
            setmem (ds.dlin, sizeof (ds.dlin), NUL);
            dlin++;
        }

        else { /* Copy current line to ds map */
            strcpy (&ds.map [dlin][0], ds.dlin);

        }
    }               /* End of kb.klin loop */

    if ((ilin = dlin + 1) <= ds.lct)    /* Erase leftovers */ { 
        for (; ilin <= ds.lct; ilin++)
            ;
         { 
            cursor_move (screen.curlin + ilin, 0);
            putscr (EL, strlen (EL));
        }

        ds.lct = dlin;
        cursor_move (screen.curlin + dlin, dstrt);
    }

    putscr (EL, strlen (EL));

    if (dcol != dend)
        cursor_move (screen.curlin, dend + (dlin == 0) * ds.pstrl);
    ds.lndx = dlin;
    ds.ccol = dend;

    return (0);
}               /* End of replay */


/* End of WSTEDIT */
