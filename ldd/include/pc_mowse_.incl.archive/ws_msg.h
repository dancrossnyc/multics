/* BEGIN INCLUDE FILE: ws_msg.h */

/* HISTORY COMMENTS:
  1) change(86-06-01,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-09-03,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Installed a message structure which contains
     no data.
                                                   END HISTORY COMMENTS */

/* FUNCTION:

Defines formats for the mowse internal minor capabilities.  Equivalent include 
file wsmsg.mac
*/

struct input_msg {
   char    system;         /* destination system id        */
   char    major;          /* destination major capability */
   char    minor;          /* destination minor capability */
   char    source_system;  /* source system id             */
   char    source_major;   /* source major capability      */
   char    msg_data[1];    /* placeholder for data string  */
};

struct more_msg {
   char    system;         /* destination system id        */
   char    major;          /* destination major capability */
   char    more_minor;     /* MORE_DATA minor capability   */
   char    source_system;  /* source system id             */
   char    source_major;   /* source major capability      */
   char    minor;          /* destination minor capability */
   char    msg_data[1];    /* placeholder for data string  */
};

struct execom_msg {        /* execute_command message      */
   char    system;         /* destination system id        */
   char    major;          /* destination major capability */
   char    minor;          /* destination minor capability */
   char    source_system;  /* source system id             */
   char    source_major;   /* source major capability      */
   int     cmd_id;         /* unique id for command        */
   char    command [1];    /* placeholder for data string  */
};

struct exerep_msg {        /* execute_command_reply        */
   char    system;         /* destination system id        */
   char    major;          /* destination major capability */
   char    minor;          /* destination minor capability */
   char    source_system;  /* source system id             */
   char    source_major;   /* source major capability      */
   int     cmd_id;         /* unique id for command        */
   char    status;         /* status of execute attempt    */
};

struct execap_msg {        /* execute capability message   */
   char    system;         /* destination system id        */
   char    major;          /* destination major capability */
   char    minor;          /* destination minor capability */
   char    source_system;  /* source system id             */
   char    source_major;   /* source major capability      */
   char    data_len;       /* length of data string        */
   char    data_buf[1];    /* placeholder for data string  */
};

struct alter_cat_msg {     /* Alter CAT entry              */
   char    system;         /* destination system id        */
   char    major;          /* destination major capability */
   char    minor;          /* destination minor capability */
   char    source_system;  /* source system id             */
   char    source_major;   /* source major capability      */
   char    rat_major;      /* major cap to be inserted     */
   char    major_name[2];  /* placeholder for data string  */
};

struct query_msg {         /* background query/info message*/
   char    minor;          /* destination minor capability */
   char    source_system;  /* source system id             */
   char    source_major;   /* source major capability      */
   char    msg_data[1];    /* placeholder for data string  */
};

struct packet_msg {
   char    system;         /* destination system id        */
   char    major;          /* destination major capability */
   char    minor;          /* destination minor capability */
   char    source_system;  /* source system id             */
                           /* = 0, if foreground is sender */
   char    source_major;   /* source major capability      */
                           /* = 0, if foreground is sender */
   char    msg_data[128];  /* data string                  */
};

struct null_msg {          /* Message with no data         */
   char    system;         /* destination system id        */
   char    major;          /* destination major capability */
   char    minor;          /* destination minor capability */
   char    source_system;  /* source system id             */
   char    source_major;   /* source major capability      */
};

/* END INCLUDE FILE: ws_msg.h */
