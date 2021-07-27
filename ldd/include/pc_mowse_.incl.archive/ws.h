/* BEGIN INCLUDE FILE: WS.H */

/* HISTORY COMMENTS:
  1) change(86-06-01,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(88-02-02,Flegel):
     Changed version number to 1.1
  3) change(88-06-17,Lee), approve(88-07-18,MCR7936), audit(88-08-10,Flegel):
     Changed version number to 1.2
                                                   END HISTORY COMMENTS */

/* : FUNCTION:

Defines constants used by mowse routines.  Equivalent include file ws.mac
*/

#define VERSION                 1       /* Version Number               */
#define SUBVERSION              2

#define FG                      1       /* defines foreground channel   */
#define BG                      0       /* defines background channel   */
#define WSMAJCAP                32      /* Mowse's major capability     */
#define WSIBMPC                 33      /* IBMPC system ID              */
#define WSMULTICS               32      /* Multics system ID            */
#define WSLOCAL                 0       /* Local system                 */
#define WSREMOTE                1       /* Remote system                */
#define WSMINBUF                128     /* Minimum buffer size          */
#define WSMAXBUF                4096    /* Maximum buffer size          */
#define WSPAKSIZ                118     /* Packet size(data portion)    */
#define MIN_CAPABILITY_NUMBER   33      /* Min capability number        */
#define MAX_CAPABILITY_NUMBER   65      /* Max capability number        */
#define NUMBER_OF_CAT_ENTRIES   32      /* Max number of CAT entries    */
#define CAPABILITY_NAME_LENGTH  32      /* Capability name length       */
#define WSCAPLEN                32
#define MIN_SYSTEM_ID           32      /* Minimum system id            */
#define MAX_SYSTEM_ID           64      /* Maximum system id            */

#define WSINFO                  37      /* defines info background msg  */
#define WSQUERY                 38      /* defines query background msg */
#define WSACCEPT                32      /* accept connect request       */
#define WSREJECT                33      /* reject connect request       */
#define WSRQSTATUS              1       /* ask MOWSE to request status  */
#define WSRPSTATUS              2       /* ask MOWSE for status reply   */
#define WSSDSTATUS              3       /* ask MOWSE to send status     */

/* END INCLUDE FILE: WS.H */
