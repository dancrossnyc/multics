/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-01-27,Hoover), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-09-15,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Changed check-sum from character addition to
     CRC-6.
  3) change(86-09-24,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Changed sndpkt to allow only DisConnect packets
     to be sent when a disconnect is in progress.
  4) change(86-10-15,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added FastDis for fast (non-confirmed)
     disconnects.
  5) change(86-12-25,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Moved setting of the packetize_flag from
     get_inbuff to prsrst to ensure that it is set only when entire protocol is
     set.
                                                   END HISTORY COMMENTS */

/* : PROGRAM FUNCTION

MIO is the PC module of MOWSE which controls packet communications between
Multics and the PC in order to guarantee error free communications.
*/

/* : NOTES

The following is a description of a packet:

    --------------------------------------------------
   | SOP | TYPE | ... {data} ... | LENGTH | CRC | EOP |
    --------------------------------------------------

Supervisory packets (those which do not carry data) all are of length 5 and
carry no data (ACK, NAK, RST, BRK, and there associated types)
*/

#include <ws_error.h>

/* Special constants */
#define TIME_OUT       1               /* Disconnect timed out */
#define DISCONNECT_OUT 2               /* Disconnect completed */

/* logical Constants */
#define False   0
#define True    1

/* ASCII Control Characters */
#define NULL         0
#define CR           13
#define ESC          27
#define LF           10
#define SI           15
#define SO           14
#define SOH          1
#define MIN_ASCII    32
#define REV_VIDEO    0200
#define Null_Convert 20
#define MASK_ALL     0377
#define MASK_SIX     077

/* Protocol Bit-field Constants */
#define ChnCnt  2                      /* Number of channels */
#define SeqFld  2                      /* Number of bits in sequence field */
#define SeqCnt  4                      /* 2**SeqFld */
#define SeqMsk  3                      /* SeqCnt - 1 */

/* Protocol Byte-field Constants */
#define SOPLen          1              /* Characters in SOP field */
#define TypLen          1              /* Characters in TYPE field */
#define MaxDatLen       124            /* Maximum number of data chars */
#define ChkLen          1              /* Characters in CHECKSUM field */
#define EOPLen          1              /* Characters in EOP field */
#define LenLen          1              /* Characters in Length field */
#define MinPktLen (SOPLen+TypLen+LenLen+ChkLen+EOPLen) /* Minimum chars in Packet */
#define MaxPktLen (MinPktLen+MaxDatLen) /* Maximum chars in Packet */

/* Protocol Packet Constants */
#define BG      0                      /* Background Channel */
#define FG      1                      /* Foreground Channel */
#define RstOff  ' '                    /* <SPACE> */
#define Request 0                      /* Packet request */
#define Confirm 1                      /* Packet confirmation */
#define RstCnt  (Confirm+1)            /* Number of types of reset packets */
#define BrkOff  (RstOff+RstCnt)        /* Break packet base */
#define BrkCnt  (ChnCnt*(Confirm+1))   /* Number of types of break packet */
#define DisCon  (BrkOff+(2*BG))        /* Disconnect packet */
#define FGBrk   (BrkOff+(2*FG))        /* Foreground break packet */
#define DatOff  (BrkOff+BrkCnt)        /* Data packet base */
#define DatCnt  (ChnCnt*SeqCnt*SeqCnt) /* Number of types of data packets */
#define AckOff  (38+DatCnt)            /* Ack packet base */
#define AckCnt  (ChnCnt*SeqCnt)        /* Number of types of Ack packets */
#define NakOff  (AckOff+AckCnt)        /* Nak packet base */
#define NakCnt  (ChnCnt*SeqCnt)        /* Number of types of Nak packets */
#define FastDis (NakOff+NakCnt)        /* Fast disconnect packet */

/* Protocol Parameters */
#define Debug           True
#define RQS             2              /* 'rcvchr's receive queue size */
#define RWS             3              /* Receiver's window size */
#define SWS             3              /* Sender's windo size */
#define LimRTmr         7              /* Limit for receiver timer */
#define LimSTmr         15             /* Limit for sender timer */
#define LimPTmr         30             /* Limit for pending timer */
#define InitDis         1              /* PC initiated disconnection */
#define RespDis         2              /* Multics initiated disconnection */
#define InitRst         1              /* PC initiated reset */
#define RespRst         2              /* Multics initiated reset */
#define REVPOLY         051            /* bit N is coeff of x**(5-N) of CRC generator */
#define INIT_CRC        63             /* Initial value for seed in calculating CRC */
#define Reset_Timer_X   0              /* pending_timer index for reset resends */
#define Dis_Timer_X     1              /* pending_timer index for disconnect resends */
#define Break_Timer_X   2              /* pending_timer index for break resends */

/* Debugging Switches */
#if Debug
   int  dbgpkts = False;               /* show packets */
   int  dbgrejs = False;               /* diagnose rejects */
   int  dbgxchr = False;               /* show extraneous received chars */
#endif

/* Task Control Variables */
int  apv_locked = False;               /* Approving a packet */
int  ds_pending = 0;                   /* Process of disconnecting */
int  ds_timeout = 0;                   /* PC initiation of disconnect timeout */
int  rs_pending = 0;                   /* Process of resetting */
int  br_pending = 0;                   /* Process of a FG break */
int  pending_timer[3] = {0,0,0};       /* Timer on reset/disconnect/break */

/* Receiver Global Variables */
char r_EOP          = LF;              /* End Of Packet char */
char r_ESC[3]       = {ESC, SI, SO};   /* Receiver's 3 ESCape chars */
int  r_ESC_count    = 0;               /* Number of ESCape chars read in */
char r_SOP          = SOH;             /* Start Of Packet char */
int  r_asn[ChnCnt]  = {0, 0};          /* Acked sequence numbers */
char r_esckey       = '\000';          /* Key to decoding 2nd char of esc */
int  r_ignoring[ChnCnt] = {False, False}; /* Ignoring data pending resync */
int  r_pktin        = 0;               /* Next free slot in receive Q */
int  r_pktout       = 0;               /* Head of receive Q */
int  r_psn[ChnCnt]  = {0, 0};          /* Receive packet sequence number */
int  r_timer[ChnCnt] = {0, 0};         /* Time since last ack */
char r_dat[ChnCnt][SeqCnt][1+MaxDatLen];  /* receive data Q */
char r_pkt[RQS+1][1+MaxPktLen];        /* Receive packet */

/* Sender Global Variables */
char s_EOP          = LF;              /* End Of Packet Char */
char s_ESC[3]       = {ESC, SI, SO};   /* Sender's 3 esc chars */
char s_SOP          = SOH;             /* Start Of Packet Char */
int  s_lasn[ChnCnt] = {0, 0};          /* Last Acked sequence number */
int  s_nasn[ChnCnt] = {0, 0};          /* Next Ack to send */
int  s_psn[ChnCnt]  = {0, 0};          /* Packet sequence number */
int  s_timer[ChnCnt] = {0, 0};         /* Time since last ack */
char s_escreq[256]  = {0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,
                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                       1,1,1,1};       /* Chars to be esc'ed when sent */
char s_dat[ChnCnt][SeqCnt][1+MaxDatLen]; /* Senders data Q */

/* Current execution modes */
extern int packet_mode;                /* Indicates that packet mode is on */
extern int packetize_flag;             /* Indicates to TE to accumulate packets */
int mowse_terminating = 0;             /* Indicates that MOWSE is terminating */

/**/

/* : PROCEDURE FUNCTION (advtmr)

Advance various timers.  If a timer reaches its limit, it is reset and the
associated time-out action is performed.
*/

advtmr()
{
int  chn;                              /* Channel number */
int  i;
char type;                             /* Type field of packet */

/* : For each channel
     - If receive time limit exceeded then send an ack */

   for (chn = 0; chn < ChnCnt; chn++)
   {  r_timer[chn]++;
      if (r_timer[chn] > LimRTmr)
      {  r_timer[chn] = 0;
         if (s_nasn[chn] != s_lasn[chn])
            sndack(chn);
      }

/* : - If send time limit exceeded then send an ack */

      s_timer[chn]++;
      if (s_timer[chn] > LimSTmr)
      {  s_timer[chn] = 0;
         resend(chn);
      }
   }

/* : Increment the pending timer */

   for (i = 0; i < 3; pending_timer[i++]++);

/* : If limit exceeded on reset packet, then resend */

   if ((rs_pending & InitRst) && (pending_timer[Reset_Timer_X] > LimPTmr))
   {  pending_timer[Reset_Timer_X] = 0;
      type = RstOff + Request;
      sndpkt(type,"");
   }
   if ((rs_pending & RespRst) && (pending_timer[Reset_Timer_X] > LimPTmr))
   {  pending_timer[Reset_Timer_X] = 0;
      type = RstOff + Confirm;
      sndpkt(type,"");
   }

/* : If limit exceeded on disconnect packet
     - set timeout flag if PC initiated, else resend reply */

   if ((ds_pending & InitDis) && (pending_timer[Dis_Timer_X] > LimPTmr))
   {  ds_pending &= ~InitDis;
      ds_timeout = TIME_OUT;
   }

   if ((ds_pending & RespDis) && (pending_timer[Dis_Timer_X] > LimPTmr))
   {  pending_timer[Dis_Timer_X] = 0;
      type = DisCon + Confirm;
      sndpkt(type,"");
   }

/* : If limit exceeded on break packet, then resend */

   if ((br_pending) && (pending_timer[Break_Timer_X] > LimPTmr))
   {  pending_timer[Break_Timer_X] = 0;
      type = FGBrk + Request;
      sndpkt(type,"");
   }
}

/**/

/* : PROCEDURE FUNCTION (apvpkt)

Approve packet.  If packet appears valid, dispatch it to the appropriate
routine; otherwise, reject it.
*/

apvpkt (pkt)

char pkt[];     /* The packet to be approved */
{
int  chkidx;    /* Check index for checksum character */
int  lenidx;    /* Length index for length character */
int  pktl;      /* Packet length */
int  type;      /* Packet type */
int  i;


/* : If packet is too short or too long, reject it. */

   pktl = pkt[0] & MASK_ALL;
   if (pktl < MinPktLen || pktl > MaxPktLen)
   {
#if Debug
      if (dbgrejs)
      {  if (dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pktl);
         sdbgmsg("RejPkt/length");
      }
#endif
      return;
   }

/* : If check-length is incorrect, reject packet */

   lenidx = pktl - EOPLen - ChkLen - LenLen + 1;
   if (check_len (pktl + r_ESC_count) != pkt[lenidx])
   {
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pktl);
         sdbgmsg("RejPkt/chklen");
      }
#endif
      return;
   }

/* : If check-sum is incorrect, reject packet. */

   chkidx = lenidx + LenLen;
   if (chkcrc(&pkt[1], chkidx-1) != pkt[chkidx])
   {
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pktl);
         sdbgmsg("RejPkt/chksum");
      }
#endif
      return;
   }

/* : If packet type is valid, dispatch accordingly; else reject packet. */

   type = pkt[SOPLen + 1];
   if ((DatOff <= type && type < DatOff+DatCnt) && !rs_pending)
      prsdat(pkt);
   else if ((AckOff <= type && type < AckOff+AckCnt) && !rs_pending)
      prsack(pkt);
   else if ((NakOff <= type && type < NakOff+NakCnt) && !rs_pending)
      prsnak(pkt);
   else if ((BrkOff <= type && type < BrkOff+BrkCnt) && !rs_pending)
      prsbrk(pkt);
   else if (RstOff <= type && type < RstOff+RstCnt)
      prsrst(pkt);
   else if (FastDis == type)
      disconnect_confirm (True);
   else
   {
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pktl);
         sdbgmsg("RejPkt/type");
      }
#endif
      return;
   }
}

/**/

/* : PROCEDURE FUNCTION (check_len)

Calculate the checklength character based on the length of the entire packet
strip the upper bits, and  add 32 to make it printable.
*/

/* : RETURNS:

The calculated check length chartacter.
*/

int check_len(length)

int  length;    /* Length to be converted */
{

   return ((length & MASK_SIX) + MIN_ASCII);
}

/**/

/* : PROCEDURE FUNCTION

Calculate a 6-bit CRC for a character based on a generator polynomial of

           x**6 + x**5 + x**2 + x**0
*/

/* : RETURNS

A value in range of 0-63 which is the desired CRC.
*/

/* : NOTES

The result of this function is the remainder produced by synthetic division
modulo 2 of a 7-bit integer (whose bits are the coefficients of the generator
polynomial) into a 14-bit integer (whose top 8 bits are those of the character,
in reverse order, and whose low 6 bits are the low 6 bits of the seed).

The CRC for a string of characters is calculated by calling 'crc_char' once for
each character inthe block, from first character to last.  The seed for the
first character is the 0 and the seed for each remaining character is the CRC
produced for the previous character.  The CRC produced for the last character
is the CRC for the whole string.
*/

int crc_char (p_chr, p_seed)

int  p_seed;   /* Value in range of 0-63 which is the seed for the CRC calc */
char p_chr;    /* Char for which CRC is to be calced */
{
int  b;        /* Bit counter */
int  crc;      /* accumulator for CRC */
int  q;        /* next quotient bit of the division */
int  schr;     /* rank of 'chr' shifted right 'b' times */

   crc = p_seed;
   schr = p_chr;

/* : For each of the 8 bits in the character
     - q = low bit, if q set then right shift crc
     - right shift schr */

   for (b = 0; b < 8; b++)
   {  q = (crc + schr) & 1;
      crc >>= 1;
      if (q)
         crc ^= REVPOLY;
      schr >>= 1;
   }
   return (crc);
}

/**/

/* : PROCEDURE FUNCTION (chkcrc)

Calculate the printable ascii value of the CRC for the packet provided.
*/

/* : RETURNS

The desired CRC.
*/

int chkcrc(str, strl)

char str[];     /* Address of string */
int  strl;      /* Length of string */
{
int  i;
int  crc;       /* Accumulating CRC */

/* : CRC the ranks of the characters in the string. */

   crc = INIT_CRC;
   for (i = 0; i < strl; i++)
   {  crc = crc_char (str[i], crc);
   }

/* : Make it ASCII character between ' ' and '\177'. */

     return (crc + MIN_ASCII);
}

/**/

/* : PROCEDURE FUNCTION (disconnect_confirm)

Set up all necessary flags to indicate that disconnection has been accepted and
send a confirm to the confirm if one is needed.
*/

disconnect_confirm(p_fast)

int  p_fast;    /* Set to true for a fast (non-confirmed) disconnect */
{
char type;      /* Packet type to send */

/* : If PC initiated disconnection, then send confirm to confirm */

   if ((ds_pending & InitDis) && (!p_fast))
   {  ds_timeout = DISCONNECT_OUT;
      ds_pending = 0;
      type = DisCon + Confirm;
      sndpkt (type,"");
      return (0);
   }

/* : Set up the terminating flags accordingly */

   mowse_terminating = True;
   ds_pending = 0;
   packet_mode = False;
   packetize_flag = False;
   reset();
}

/**/

/* : PROCEDURE FUNCTION (getdat)

Get any available received data.
*/

/* : RETURNS

The number of characters stored in the buffer (between 0 and MaxDatLen,
inclusive).
*/

int getdat(chn, data)

int  chn;       /* Channel */
char *data;     /* Address off buffer to receive data */
{
char *rdat;     /* Data string */
int  i;
int  rdatl;     /* Length of receive data */

/* : If the received data queue is empty, there is nothing to do. */

   if (r_psn[chn] == s_nasn[chn])
      return 0;

/* : Extract the data in the head element of the queue. */

   rdat = &r_dat[chn][s_nasn[chn]][1];
   rdatl = rdat[-1] & MASK_ALL;
   for (i = 0; i < rdatl; i++)
   {  data[i] = rdat[i];
   }
   s_nasn[chn] = (s_nasn[chn] + 1) & SeqMsk;

/* : If the receive window is nearly full, send an Ack to keep it open. */

   if (((s_nasn[chn] - s_lasn[chn]) & SeqMsk) >= RWS-1)
      sndack(chn);

   return(rdatl);
}

/**/

/* : PROCEDURE FUNCTION (prsack)

Parse an Ack-packet.  If the packet is valid, flow control information is
updated accordingly.
*/

prsack(pkt)

char pkt[];     /* The ack-packet to be parsed */
{
int  asn;       /* Ack sequence number */
int  asn_valid; /* Valid Ack sequence number */
int  chn;       /* Channel of ack */
int  fields;    /* Fields of packet */

/* : If packet length incorrect, reject packet. */

   if (pkt[0] != MinPktLen)
   {
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pkt[0] & MASK_ALL);
         sdbgmsg("RejAck/length");
      }
#endif
      return;
   }

/* : Extract the acknowledgement sequence number and channel number. */

   fields = pkt[SOPLen+1] - AckOff;
   asn = fields & SeqMsk;
   chn = fields >> SeqFld;

/* : If the acknowledgement sequence number is invalid, reject packet. */

   if (r_asn[chn] <= s_psn[chn])
      asn_valid = (r_asn[chn] <= asn && asn <= s_psn[chn]);
   else
      asn_valid = (r_asn[chn] <= asn || asn <= s_psn[chn]);
   if (!asn_valid)
   {
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pkt[0] & MASK_ALL);
         sdbgmsg("RejAck/asn");
      }
#endif
      return;
   }

/* : Save the new acknowledgement sequence number. */

   r_asn[chn] = asn;
}

/**/

/* : PROCEDURE FUNCTION (prsbrk)

Parse a Break-packet.
*/

/* : NOTES

There are two types of break packets - one is the foreground break which serves
to interrupt a foreground activity (not possible on the PC) and the other is
the background break which provides a mechanism for disabling MOWSE on Multics
and to leave the communications open to non-MOWSE Multics
*/

prsbrk (pkt)

char pkt[];     /* The packet to be parsed */
{
char type;      /* Type field of packet */

/* : If a Disconnect then send a confirmation to Multics */

   if ((pkt[SOPLen+1] == DisCon + Request) && !ds_pending)
   {  ds_pending = ds_pending | RespDis;
      pending_timer[Dis_Timer_X] = 0;
      type = DisCon + Confirm;
      sndpkt (type,"");
   }

/* : If a Disconnect confirmation, then all done and shut off protocol modes */

   else if (pkt[SOPLen + 1] == DisCon + Confirm)
   {  pending_timer[Dis_Timer_X] = 0;
      disconnect_confirm(0);
   }

/* : If a Foreground break request then handle accordingly */

   else if (pkt[SOPLen + 1] == FGBrk + Request)
   {
   }

/* ; If a Foreground break confirm then handle accordingly */
   else if (pkt[SOPLen + 1] == FGBrk + Confirm)
   {  pending_timer[Break_Timer_X] = 0;
      br_pending = False;
      type = FGBrk + Confirm;
      sndpkt(type,"");
   }
}

/**/

/* PROCEDURE FUNCTION (prsdat)

Parse a Data-packet.  If the packet is valid, the data is stored in the receive
data queue.
*/

prsdat(pkt)

char pkt[];     /* The packet to be parsed */
{
char *data;     /* Accepted data buffer */
char *rdat;     /* Received data */
int  asn;       /* Ack sequence number */
int  asn_valid; /* Accept sequence number valid */
int  chn;       /* Channel */
int  datal;     /* Length of data */
int  fields;    /* Type field */
int  i;
int  psn;       /* Packet sequence number */

/* : Extract the acknowledgement sequence number, packet sequence number and
     channel number from the type char. */

   fields = pkt[SOPLen + 1] - DatOff;
   asn = fields & SeqMsk;
   psn = (fields >> SeqFld) & SeqMsk;
   chn = fields >> (SeqFld + SeqFld);

/* : If the acknowledgement sequence number is invalid, reject the packet. */

   if (r_asn[chn] <= s_psn[chn])
      asn_valid = (r_asn[chn] <= asn && asn <= s_psn[chn]);
   else
      asn_valid = (r_asn[chn] <= asn || asn <= s_psn[chn]);
   if (!asn_valid)
   {
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pkt[0] & MASK_ALL);
         sdbgmsg("RejDat/asn");
      }
#endif
      return;
   }

/* : Save the new acknowledgement sequence number. */

   r_asn[chn] = asn;

/* : If ds_pending then ignore all data */

   if (ds_pending)
      return;

/* : If the send sequence number is not the one we expect, send a Nak-packet,
     (unless one has already been sent) and ignore this packet. */

   if (psn != r_psn[chn])
   {  if (!r_ignoring[chn])
      {  sndnak(chn);
         r_ignoring[chn] = True;
      }
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pkt[0] & MASK_ALL);
         sdbgmsg("RejDat/psn");
      }
#endif
      return;
   }

/* : Accept the data. */

   data = &pkt[SOPLen + TypLen + 1];
   datal = (pkt[0] & MASK_ALL) - MinPktLen;
   rdat = &r_dat[chn][psn][1];
   rdat[-1] = datal;
   for (i = 0; i < datal; i++)
   {  rdat[i] = data[i];
   }
   r_ignoring[chn] = False;
   r_psn[chn] = (psn + 1) & SeqMsk;
}

/**/

/* : PROCEDURE FUNCTION (prsnak)

Parse a Nak-packet.  If the packet is valid, any data packets that have been
sent but are not acknowledged by this Nak-packet are resent.
*/

prsnak(pkt)

char pkt[];     /* The Nak-packet to be parsed */
{
int asn;        /* Ack sequence number */
int asn_valid;  /* Valid ack sequence number */
int chn;        /* Channel */
int fields;     /* Type field of packet */

/* : If packet length is incorrect, reject the packet. */

   if (pkt[0] != MinPktLen)
   {
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pkt[0] & MASK_ALL);
         sdbgmsg("RejNak/length");
      }
#endif
      return;
   }

/* : Extract the acknowledgement sequence number and the channel number from
     the type byte. */

   fields = pkt[SOPLen + 1] - NakOff;
   asn = fields & SeqMsk;
   chn = fields >> SeqFld;

/* : If the acknowledgement sequence number is invalid, reject the packet. */

   if (r_asn[chn] <= s_psn[chn])
      asn_valid = (r_asn[chn] <= asn && asn <= s_psn[chn]);
   else
      asn_valid = (r_asn[chn] <= asn || asn <= s_psn[chn]);
   if (!asn_valid)
   {
#if Debug
      if (dbgrejs)
      {  if (!dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pkt[0] & MASK_ALL);
         sdbgmsg("RejNak/asn");
      }
#endif
      return;
   }

/* : Save the new acknowledgement sequence number. */

   r_asn[chn] = asn;

/* : Resend any data in the send data queue. */

   resend(chn);
}

/**/

/* PROCEDURE FUNCTION (prsrst)

Parse a Reset-packet.  When a reset is complete, set the packetize flag to true
which indicates that protocol is set up completely.
*/

prsrst(pkt)

char pkt[];     /* The reset packet to be parsed */
{
char type;      /* Type field of a packet */

/* : If in process of disconnecting then ignore */

   if (ds_pending)
      return (0);

/* : If Multics requested reset: reset and send confirmation and pretend reset
     in packet queue */

   if (pkt[SOPLen+1] == RstOff + Request)
   {  rs_pending = rs_pending | RespRst;
      reset();
      r_pktin = 1;
      type = RstOff + Confirm;
      sndpkt(type, "");
   }

/* : Else if confirmation */

   else if (pkt[SOPLen + 1] == RstOff + Confirm)
   {  if (rs_pending & InitRst)
      {  pending_timer[Reset_Timer_X] = 0;
         type = RstOff + Confirm;
         sndpkt (type, "");
      }
      packetize_flag = True;
      rs_pending = 0;
   }
}

/**/

/* PROCEDURE FUNCTION (rcvchr)

To receive the next character detected by the interrupt handler for input from
the remote.  If the character is valid, it is added to the packet currently
being assembled.
*/

rcvchr(chr)

char chr;       /* The character received by the interrupt handler */
{
static int EndPkt = 1;
static int ExtChr = 2;
static int LngPkt = 3;
static int NoRoom = 4;

int  dbgmsg;    /* Accumulating debug message */
int  nextin;    /* Next packet in */
int  pktl;      /* Packet length */
int  test_ds;   /* Tests if ds_pending is set before processing data */
char *pkt;      /* Accumulating packet */

/* : If character is a null then ignore it (network created it) */

   if (!chr)
      return;

/* : Replace Null_Convert characters with a NULL character */

   if (chr == Null_Convert)
      chr = NULL;

   dbgmsg = 0;

/* : Assemble packet in next slot of received packet queue. */

   pkt = r_pkt[r_pktin];

/* : If the received char is the SOP char, start a new packet. */

   if (chr == r_SOP)
   {  r_esckey = '\000';
      pkt[0] = 1;
      pkt[1] = chr;
   }

/* : Else if we are not assembling a packet, discard the received char. */

   else if (pkt[0] == 0)
   {  dbgmsg = ExtChr;
   }

/* : Else if the received char is the EOP char, append it to the packet.
     If there is room in the received packet queue, make the packet visible (by
     updating the "in" pointer); otherwise flush the packet. */

   else if (chr == r_EOP)
   {  dbgmsg = EndPkt;
      pktl = ++pkt[0] & MASK_ALL;
      pkt[pktl] = chr;

      if (r_pktin < RQS)
         nextin = r_pktin + 1;
      else
         nextin = 0;

      if (nextin != r_pktout)
      {  r_pktin = nextin;
         r_pkt[r_pktin][0] = 0;
      }
      else
      {  dbgmsg = NoRoom;
         pkt[0] = 0;
      }
   }

/* : Else append the character (or what it stands for if it was preceded
     by an escape character) to the buffer, unless it is an escape
     character.  If this fills the packet (in which case there will be
     no room for the EOP character), flush the packet. */

   else
   {  if (r_esckey != '\000')
      {  chr = chr ^ r_esckey;
         r_esckey = '\000';
         r_ESC_count++;
      }
      else if (chr == r_ESC[0])
         r_esckey = '\100';
      else if (chr == r_ESC[1])
         r_esckey = '\200';
      else if (chr == r_ESC[2])
         r_esckey = '\300';

      if (r_esckey == '\000')
      {  pktl = ++pkt[0] & MASK_ALL;
         pkt[pktl] = chr;
         if (pktl >= MaxPktLen)
         {  dbgmsg = LngPkt;
            pkt[0] = 0;
         }
      }
   }

/* : Print any debugging messages. If printable, send as is, otherwise convert
     it to octal, preceed it with \ */
#if Debug
   if (dbgmsg != 0)
   {  if (dbgmsg == ExtChr)
      {  if (dbgxchr)
         {  if (chr >= ' ' && chr <= '~')
               sdbgchr(chr + REV_VIDEO);
            else
            {  sdbgchr('\\' + REV_VIDEO);
               sdbgchr(((chr>>6) & 3) + '0' + REV_VIDEO);
               sdbgchr(((chr>>3) & 7) + '0' + REV_VIDEO);
               sdbgchr((chr & 7) + '0' + REV_VIDEO);
            }
         }
      }
      else
      {  if (dbgpkts)
            sdbgpkt("RcvPkt/", &pkt[1], pktl);
         if (dbgrejs)
         {  if (dbgmsg == LngPkt)
            {  if (!dbgpkts)
                  sdbgpkt("RcvPkt/", &pkt[1], pktl);
               sdbgmsg("RejPkt/LngPkt");
            }
            else if (dbgmsg == NoRoom)
            {  if (!dbgpkts)
                  sdbgpkt("RcvPkt/", &pkt[1], pktl);
               sdbgmsg("RejPkt/NoRoom");
            }
         }
      }
   }
#endif

/* : If we were approving packets when we were interrupted to receive the
     character just processed, we want to simply return so that approving can
     continue.  If we were not approving packets and the received packet queue
     is not empty, then we want to initiate approving.
*/
   if (!apv_locked)
   {  apv_locked = (r_pktin != r_pktout);
      while (apv_locked)
      {  test_ds = ds_pending;
         apvpkt(r_pkt[r_pktout]);
         r_ESC_count = 0;
         if (r_pktout < RQS)
            r_pktout++;
         else
            r_pktout = 0;
         apv_locked = (r_pktin != r_pktout);

         if ((test_ds & ds_pending) & RespDis)
            disconnect_confirm(0);
      }
   }
}

/**/

/* : PROCEDURE FUNCTION (resend)

Resend all unacknowledged data for a channel.
*/

resend(chn)

int  chn;       /* Channel number */
{
int  psn;       /* Packet sequence number */
char type;      /* Packet type */

/* : Resend each packet */

   for (psn = r_asn[chn]; psn != s_psn[chn]; psn = (psn + 1) & SeqMsk)
   {  type = (((chn << SeqFld) + psn) << SeqFld) + s_nasn[chn] + DatOff;
      sndpkt(type, s_dat[chn][psn]);
      s_lasn[chn] = s_nasn[chn];
      s_timer[chn] = 0;
   }
}

/**/

/* PROCEDURE FUNCTION (reset)

To intialize the flow-control related variables of the protocol.
*/

reset()

{
int  chn;       /* Channel number */
int  i;

   apv_locked = False;
   ds_pending = 0;
   br_pending = 0;
   for (i = 0; i < 3; pending_timer[i++] = 0);

   for (i = 0; i <= RQS; i++)
   {  r_pkt[i][0] = 0;
   }
   r_pktin = 0;
   r_pktout = 0;

   for (chn = 0; chn < ChnCnt; chn++)
   {  for (i = 0; i < SeqCnt; i++)
      {  r_dat[chn][i][0] = 0;
      }
      r_ignoring[chn] = False;
      r_asn[chn] = 0;
      r_psn[chn] = 0;
      r_timer[chn] = 0;

      for (i = 0; i < SeqCnt; i++)
      {  s_dat[chn][i][0] = 0;
      }
      s_lasn[chn] = 0;
      s_nasn[chn] = 0;
      s_psn[chn] = 0;
      s_timer[chn] = 0;
   }
}

/**/

#if Debug

/* : PROCEDURE FUNCTION (sdbgmsg)

Send a debugging message.
*/

sdbgmsg(msg)

char msg[];     /* Message to be shown */
{
int  i;

/* : Send CR/LF and a reverse video '('. */

   sdbgchr(CR);
   sdbgchr(LF);
   sdbgchr('(' + REV_VIDEO);

/* : Send the message. */

   for (i = 0; msg[i] != '\000'; i++)
   {  sdbgchr(msg[i]);
   }

/* : Send a reverse-video ')' and CR/LF. */

   sdbgchr(')' + REV_VIDEO);
   sdbgchr(CR);
   sdbgchr(LF);
}
#endif

/**/

#if Debug

/* : PROCEDURE FUNCTION (sdbgpkt)

Send a debugging message that displays a packet.
*/

sdbgpkt(msg, pkt, pktl)

char msg[];     /* Message to be displayed */
char pkt[];     /* Debug packet */
int  pktl;      /* Length of packet */
{
char chr;       /* Temporary char space */
int  i;

/* : Send CR/LF and a reverse-video '('. */

   sdbgchr(CR);
   sdbgchr(LF);
   sdbgchr('(' + 128);

/* : Send the message describing the packet. */

   for (i = 0; msg[i] != '\0'; i++)
   {  sdbgchr(msg[i]);
   }

/* : Send the packet with nonprintable characters replaced by an octal
     escape sequence. */

   for (i = 0; i < pktl; i++)
   {  chr = pkt[i];
      if (chr >= ' ' && chr <= '~')
         sdbgchr(chr);
      else
      {  sdbgchr('`');
         sdbgchr(((chr>>6) & 3) + '0');
         sdbgchr(((chr>>3) & 7) + '0');
         sdbgchr((chr & 7) + '0');
      }
   }

/* : Send a reverse-video ')' and CR/LF. */

   sdbgchr(')' + 128);
   sdbgchr(CR);
   sdbgchr(LF);
}
#endif

/**/

/* PROCEDURE FUNCTION (sndack)

Send an Ack-packet.
*/

sndack(chn)

int  chn;       /* The channel to send the Ack along */
{
char type;      /* Type field */

   type = (chn << SeqFld) + s_nasn[chn] + AckOff;
   sndpkt(type, "");
   s_lasn[chn] = s_nasn[chn];
   r_timer[chn] = 0;
}

/**/

/* PROCEDURE FUNCTION (sndbrk)

Send a Break-packet to signal a break on the foreground channel.
*/

sndbrk()

{
char type;      /* A packet type field */

/* : Set up break processing protocol */

   br_pending = True;
   pending_timer[Break_Timer_X] = 0;

/* : Send the break message */

   type = FGBrk + Request;
   sndpkt (type, "");
}

/**/

/* PROCEDURE FUNCTION (snddat)

To queue and then send data to the remote.
*/

/* : RETURNS

   -1 => sum of lengths of strings to be sent exceeds 'MaxDatLen'.
    0 => data sent.
    1 => couldn't send data because send window was full.
    2 => couldn't send because reset is pending.

*/

int snddat(chn, datac, datap, datal)

int  chn,       /* Channel number */
     datac,     /* Count of number of strings to be sent */
     datal[];   /* Array of lengths of strings to be sent */
char *datap[];  /* Array of pointers to the strings to be sent */
{
static int TooLong = -1;
static int SentDat = 0;
static int WdwFull = 1;
static int RstPnd  = 2;

char *sdat;     /* Send data pointer */
char type;      /* Type field */
int  i;
int  j;
int  l;
int  spsn;      /* Send packet sequence number */

/* : If resetting or not talking yet, then return */

   if ((rs_pending) || (!packetize_flag))
      return RstPnd;

/* : Reject the data if the send data queue is full. */

   spsn = s_psn[chn];
   if (((spsn - r_asn[chn]) & SeqMsk) >= SWS)
      return WdwFull;

/* : Catenate the data to be sent in the next slot of the send data queue. */

   sdat = s_dat[chn][spsn];
   sdat[0] = 0;
   l = 0;
   for (i = 0; i < datac; i++)
   {  for (j = 0; j < datal[i]; j++)
      {  if (l < MaxDatLen)
            sdat[++l] = datap[i][j];
         else
            return TooLong;
      }
   }
   sdat[0] = l;

/* : Return if there is nothing to send. */

   if (l == 0)
      return SentDat;

/* : Make the catenated data visible by updating the packet sequence number. */

   s_psn[chn] = (spsn + 1) & SeqMsk;

/* : Send the data and reset resend timer. */

   type = (((chn << SeqFld) + spsn) << SeqFld) + s_nasn[chn] + DatOff;
   sndpkt(type, sdat);
   s_lasn[chn] = s_nasn[chn];
   r_timer[chn] = 0;
   s_timer[chn] = 0;
   return SentDat;
}

/**/

/* : PROCEDURE FUNCTION (snddis)

Initiate a disconnection from MOWSE on Multics.
*/

snddis()

{
char type;      /* Type field of message */

/* : If in not in packet/protocol mode then reject request */

   if (!packetize_flag || !packet_mode)
      return (WSNOTACT);

/* : If a disconnection is currently in progress, ignore it */

   if (ds_pending)
      return (WSDISPEN);

/* : If timeout has occurred, return timeout error */

   if (ds_timeout & TIME_OUT)
   {  ds_timeout = 0;
      return (TIME_OUT);
   }

/* : If got response, reply that Multics is still active */

   if (ds_timeout & DISCONNECT_OUT)
   {  ds_timeout = 0;
      return (DISCONNECT_OUT);
   }

/* : Initiate disconnection by sending Disconnect request to Multics */

   ds_pending |= InitDis;
   ds_timeout = 0;
   type = DisCon + Request;
   sndpkt (type, "");
   return (WSDISPEN);
}

/**/

/* PROCEDURE FUNCTION (sndnak)

Send a Nak-packet to the remote.
*/

sndnak(chn)

int  chn;       /* The channel on which to send the Nak-packet */
{
char type;      /* Type field of the packet */

   type = (chn << SeqFld) + /*r_psn*/s_nasn[chn] + NakOff;
   sndpkt(type, "");
   return (0);
}

/**/

/* PROCEDURE FUNCTION (sndpkt)

Send a packet to the remote.
*/

/* : NOTES

If MOWSE is not in packet mode state, then packets will not be sent as they
will go to the remote's command processor.

The packet to be sent is built in the local buffer 'pkt' and then sent to the
remote all at once.  The space required for this buffer could be saved if the
packet were sent to the remote a character at a time.  This was not done
because the time to execute a call to the routine that sends data to the remote
is greater than that to add a character to a buffer, and because to send the
packet in pieces would require that interrupts be inhibited for the duration of
the execution of 'sndpkt' in order to prevent it from being re-entered. 

Data outside the range ' ' through '_' for which the corresponding element of
's_escreq' is set is replaced in the packet via a two character escape sequence
consisting of an element of the array "s_ESC" followed by a printable ASCII
character between ' ' and '_'.  These two characters are chosen such that the
exclusive-or of the second with 64 times the ordinal in 's_ESC' of the first
yields the character they represent. 

The replacement of data characters via two character escape sequences has no
effect upon the checksum of the packet:  The checksum is calculated using the
original data characters rather than those actually sent in the data field of
the packet.  This allows the receiver of the packet to revert escape sequences
as soon as they are encountered.
*/

sndpkt(type, data)

char type;      /* Type field of message */
char data[];    /* Data of message */
{
static int EscKey[3]    = {1*64, 2*64, 3*64},
           GrpEscIdx[8] = {0, -1, -1, 0, 2, 1, 1, 2};

int  crc;                              /* CRC character */
int  chr;                              /* Character */
int  datal;                            /* Length of data */
int  escidx;                           /* Escape index */
int  i;
int  group;                            /* Group number of escape character */
int  pktl;                             /* Packet length */
char pkt[MinPktLen + 2*MaxDatLen];     /* Accumulating send packet */

/* : If not in packet mode (remote not MOWSE active), return */

   if (!packet_mode)
      return (0);

/* : If a disconnect is in progress and the type is not one of the disconnect
     packets, return */

   if (ds_pending && ((type != DisCon + Request) && (type != DisCon + Confirm)))
      return(0);

/* : Assemble the packet.  Insert escape sequences as required. */

   pkt[0] = s_SOP;
   pkt[1] = type;
   crc = crc_char (pkt[0], INIT_CRC);
   crc = crc_char (pkt[1], crc);
   pktl = 2;
   datal = data[0] & MASK_ALL;
   for (i = 1; i <= datal; i++)
   {  chr = data[i] & MASK_ALL;
      crc = crc_char ((char)chr, crc);
      group = chr >> 5;                /* Divide char by 32 to get group number */
      escidx = GrpEscIdx[group];
      if (escidx >= 0 && s_escreq[chr])
      {  pkt[pktl] = s_ESC[escidx];
         pktl++;
         chr ^= EscKey[escidx];
      }
      pkt[pktl] = chr;
      pktl++;
   }

/* : Insert the control information */

   pkt[pktl] = check_len (pktl + LenLen + ChkLen + SOPLen);
   crc = crc_char (pkt[pktl], crc);
   pktl++;
   pkt[pktl] = crc + MIN_ASCII;
   pktl++;
   pkt[pktl] = s_EOP;
   pktl++;

/* : Send the packet to the remote. */

   smdmstr(&pkt[0], pktl);

#if Debug
   if (dbgpkts)
   {  sdbgpkt("SndPkt/", &pkt[0], pktl);
   }
#endif
}

/**/

/* : PROCEDURE FUNCTION (sndrst)

Send a Reset-packet.
*/

sndrst()

{
char type;      /* Packet type field */

/* : If currently resetting, ignore */

   if (rs_pending)
      return (0);

/* : Send reset */

   rs_pending = rs_pending | InitRst;
   reset();
   type = RstOff + Request;
   sndpkt(type, "");
}
