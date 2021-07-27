/* BEGIN INCLUDE FILE: wsmincap.h */

/* HISTORY COMMENTS:
  1) change(86-05-31,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-09-02,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added MOWSE_DETACHED, MOWSE_ATTACHED minor.
  3) change(86-11-20,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Combined with ws_caps to bring all predefined
     minor capability numbers under one roof.
  4) change(86-11-21,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added SET_SUSPEND/RESET_SUSPEND minors.
                                                   END HISTORY COMMENTS */

/* FUNCTION:

Defines the dedicated minor capability numbers used by MOWSE.  These minor 
capability numbers should be supported by all MOWSE applications.  Equivalent 
include file wsmincap.mac
*/

#define EXECUTE_COMMAND_REPLY   32      /* Execute command reply */
#define WSCOMREP                32
#define FAIL_CAPABILITY         33      /* Execute capability failed */
#define WSFAILCP                33
#define WS_EXECUTE_COMMAND      34      /* Execute command */
#define WS_ADD_TO_RAT           35      /* Add to RAT */
#define WS_DELETE_FROM_RAT      36      /* Delete from RAT */
#define SUSPEND_APPLICATION     37      /* Suspend BG application */
#define WSSUSAPP                37
#define RESUME_APPLICATION      38      /* Resume BG application */
#define WSRSMAPP                38
#define TERMINATE_APPLICATION   39      /* Terminate BG application */
#define WSTRMAPP                39
#define RESET_APPLICATION       40      /* Reset BG application */
#define WSRSTAPP                40
#define RESET_REPLY             41      /* BG application reset */
#define WSRSTREP                41
#define WAKE_UP                 42      /* Wake up BG application */
#define WSWAKEUP                42
#define GET_STATUS              43      /* Status request */
#define WSSTATUS                43
#define MESSAGE_TOO_LONG        44      /* Buffer overflow */
#define WSOVRFLW                44
#define SYSTEM_ERROR            45      /* System error occurred */
#define WSSYSERR                45
#define QUERY_REPLY             46      /* Query reply */
#define WSQRYREP                46
#define RESPONSE_CONNECT        47      /* Connect response */
#define WSRESPCN                47
#define RESPONSE_DISCONNECT     48      /* Disconnect response */
#define WSRESPDS                48
#define REQUEST_CONNECT         49      /* Connect request */
#define WSRQSTCN                49
#define REQUEST_DISCONNECT      50      /* Disconnect request */
#define WSRQSTDS                50
#define WS_SET_SLEEP_FLAG       53      /* Set sleep flag */
#define WS_RESET_SLEEP_FLAG     54      /* Reset sleep flag */
#define SET_SUSPEND             55      /* Set suspend on remote cat */
#define RESET_SUSPEND           56      /* Reset suspend on remote cat */
#define STATUS_REPLY            57      /* reply to get_status          */
#define STATREPL                57

/* Special internal MOWSE minor caps for PAD */

#define PARTIAL_MESSAGE         51      /* Message fragment */
#define CONTINUE_MESSAGE        52      /* Request for message */

/* The following minor capability numbers should be supported by foreground 
   MOWSE applications. */

#define FG_CONTROL_MESSAGE      33      /* Control message for WSTERM   */
#define FGCONTRL                33
#define FG_BREAK                34      /* Foreground break             */
#define FGBREAK                 34
#define FG_TERMINAL_DATA        35      /* foreground terminal data     */
#define FGDATA                  35
#define FG_MORE_DATA            36      /* more data to follow          */
#define FGMORDAT                36
#define BG_MESSAGE              37      /* background message           */
#define BG_QUERY                38      /* background query             */
#define MOWSE_DETACHED          39      /* Multics MOWSE not attached   */
#define DETACHED                39
#define MOWSE_ATTACHED          40      /* Multics MOWSE attached        */
#define ATTACHED                40

/* END INCLUDE FILE: wsmincap.h */
