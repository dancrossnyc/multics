
/* BEGIN INCLUDE FILE ws_error.h */

/* HISTORY COMMENTS:
  1) change(86-06-01,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-09-05,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added WSINVTIM.
                                                   END HISTORY COMMENTS */


/* : FUNCTION
Defines PC error codes, equivalent include file ws_error.mac

Error codes between -1000 and -1200 are general
                    -1201 and -1300 are PC only
                    -1301 and -1400 are Multics only
*/

#define WSNOERR                 0       /* No error                     */
#define WSACTIVE                -1000   /* MOWSE is currently active    */
#define WSNOTACT                -1001   /* Mowse not active             */
#define WSINVSYS                -1002   /* Invalid system number        */
#define WSINVMCB                -1003   /* Invalid MCB pointer          */
#define WSINVNUM                -1005   /* Invalid capability number    */
#define WSSUSPND                -1006   /* Suspended                    */
#define WSERROR                 -1007   /* Some kind of error           */
#define WSINVMIN                -1008   /* Invalid minor capability     */
#define WSBUFOVR                -1009   /* Buffer Overflow              */
#define WSDISPEN                -1010   /* Disconnect pending           */
#define WSCNTCRE                -1011   /* couldn't create instance     */
#define WSINVNAM                -1012   /* Invalid capability name      */
#define WSINVTIM                -1013   /* Invalid sleep interval       */
#define WSNOSPND                -1014   /* Not suspended                */
#define WSSLPING                -1015   /* Already sleeping             */
#define WSNOTRES                -1016   /* MOWSE is not resident        */
#define WSNOMESS                -1201   /* No background message        */
#define WSINVBUF                -1202   /* Invalid buffer size          */
#define WSINVCAT                -1203   /* Invalid CAT entry            */
#define WSINVENM                -1301   /* Invalid entry name           */
#define WSINVCON                -1302   /* Invalid connect status       */

/* END INCLUDE FILE  ws_error.h */

