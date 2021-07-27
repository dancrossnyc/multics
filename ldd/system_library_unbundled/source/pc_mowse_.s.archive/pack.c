/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-05,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-06-10,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Removed from capabil.c
  3) change(86-11-14,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Check for WSMAJCAP as a capability number.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (c_pack):

Pack system_id and major capability number into one integer
*/

/* : RETURNS:

     0         - if pack successful
     WSINVNUM  - if major_number outside of the range MIN_CAPABILITY_NUMBER and
                 MAX_CAPABILITY_NUMBER
     WSINVSYS  - if system_id is outside of the range MIN_SYSTEM_ID and 
                 MAX_SYSTEM_ID
*/

/* : NOTES

Both system_id and major number are considered to be 8 bit unsigned integers. 
The system_id is moved to the high order half of major capability is moved 
to the low order half.
*/

#include <dos.h>
#include <ws.h>
#include <cat.h>
#include <ws_error.h>

#define LOW_8_BITS  0x00FF
#define BYTE_SHIFT  8

c_pack (system_id, major_number, major_capability)
int system_id;           /* system id */
int major_number;        /* Major capability number */
int *major_capability;   /* Resultant capability number (packed) */
{

/* : if major_number not between 32 and 64 inclusive then
     - set code to invalid major number */

   if (major_number < WSMAJCAP || major_number > MAX_CAPABILITY_NUMBER)
      return(WSINVNUM);

/* : if system id not between 32 and 64 and not WSLOCAL and WSREMOTE then
     - set code to invalid system id */

   if ((system_id != WSLOCAL) && (system_id != WSREMOTE) &&
       ((system_id < MIN_SYSTEM_ID) || (system_id > MAX_SYSTEM_ID)))
   {
      return(WSINVSYS);
   }

/* : return major capability number, where system id occupies high 8 bits, 
     major_number low 8 bits */

   *major_capability = (system_id << BYTE_SHIFT) | major_number;
   return(0);
}

/* : PROCEDURE FUNCTION (c_unpack):

Unpack system_id and major capability number from one integer into two 
separate integers.
*/

/* : RETURNS:

     0, if unpack successful
     WSINVNUM - if major_number outside of the range
                MIN_CAPABILITY_NUMBER and MAX_CAPABILITY_NUMBER
*/

/* : NOTES

Both system_id and major number are considered to be 8 bit unsigned integers. 
The high order half of major_capability is moved to the low order byte in
system_id, while the low order half of major_capability is moved to the low 
order half of major_number.
*/

c_unpack (system_id, major_number, major_capability)
int *system_id;        /* Pointer to system id (resultant) */
int *major_number;     /* Pointer to major number (resultant) */
int major_capability;  /* Capability number to be unpacked */
{

/* : - system id is high 8 bits */

   *system_id = (major_capability >> BYTE_SHIFT) & LOW_8_BITS;

/* : - major number is low 8 bits */

   *major_number = major_capability & LOW_8_BITS;

/* : - if major number unpacked is invalid, return error code */

   if (*major_number < WSMAJCAP || *major_number > MAX_CAPABILITY_NUMBER)
      return(WSINVNUM);

/* : if system id not between 32 and 64 and is not WSREMOTE and WSLOCAL then
     -  return invalid system id error code */

   if ((*system_id != WSLOCAL) && (*system_id != WSREMOTE) &&
       ((*system_id < MIN_SYSTEM_ID) || (*system_id > MAX_SYSTEM_ID)))
   {
      return(WSINVSYS);
   }

   return(0);
}
