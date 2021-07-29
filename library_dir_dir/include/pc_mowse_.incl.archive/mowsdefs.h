/* BEGIN INCLUDE FILE: mowsedefs.h */

/* HISTORY COMMENTS:
  1) change(86-01-01,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(88-01-26,Flegel), approve(88-02-29,MCR7853),
     audit(88-03-10,Nakaska):
     Moved in option definitions from MOWSE.C
  3) change(88-06-17,Lee), approve(88-07-18,MCR7936), audit(88-08-10,Flegel):
     Added definitions for Mark and Space parity values
                                                   END HISTORY COMMENTS */

/* FUNCTION

Define the necessary initialization and miscellaneous values required by MOWSE.
*/

#define ACK          6
#define NAK          21

/* Baud rates */

#define B110         0
#define B150         1
#define B300         2
#define B600         3
#define B1200        4
#define B2400        5
#define B4800        6
#define B9600        7

/* Parity */

#define NO_PARITY    0
#define ODD_PARITY   1
#define EVEN_PARITY  3
#define MARK_PARITY  5
#define SPACE_PARITY 7
#define NO_PAR       0
#define ODD_PAR      1
#define EVEN_PAR     3
#define MARK_PAR     5
#define SPAC_PAR     7

/* Stop Bits */

#define STOP1        0
#define STOP2        1

/* Data Bits */      

#define DATA7        2
#define DATA8        3

/* Default communications setup: <B9600,even_parity,stop1,data7> */

#define DEFAULT_COMM ((B9600<<5)|(EVEN_PARITY<<3)|(STOP1<<2)|(DATA7))
#define DEF_COMM     ((B9600<<5)|(EVEN_PAR<<3)|(STOP1<<2)|(DATA7))


/* Common constant values */

#define T            1
#define TRUE         1
#define F            0
#define FALSE        0
#define ADD_CONTROL  32

/* Buffer constants */

#define BUFSIZE      4096              /* General buffer size */
#define TBUFSIZE     4096              /* Terminal buffer size */
#define PBUFSIZE     520               /* Packet buffer size */
#define DATA_SIZE    121               /* Packet data size */

/* ASCII Codes */

#define BS           8
#define TAB          9
#define LF           10
#define CR           13
#define ESC          27
#define CTL_RSB      29

/* Startup option parameters */

#define OPTION_B            0x001       /* B parameter option */
#define OPTION_C            0x002       /* C   */
#define OPTION_D            0x004       /* D   */
#define OPTION_I            0x018       /* I   */
#define OPTION_P            0x010       /* P   */
#define OPTION_S            0x020       /* S   */
#define OPTION_GX           0x040       /* G X */
#define OPTION_GR           0x080       /* G R */
#define OPTION_GP           0x100       /* G P */
#define OPTION_H            0x200       /* H   */

/* END INCLUDE FILE mowsedefs.h */
