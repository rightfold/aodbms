       IDENTIFICATION DIVISION.
       PROGRAM-ID. aodbms.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ws-data-path                 PIC X(256).

       PROCEDURE DIVISION.

      ******************************************************************
      * This section marks the entry point of the DBMS.

       se-entry SECTION.

       pa-entry-initialize.
           MOVE '/var/lib/aodbms/data' TO ws-data-path
           .

       pa-entry-loop.
           PERFORM se-command FOREVER
           .

      ******************************************************************
      * This section receives and executes a single command.

       se-command SECTION.

       pa-command-receive.
           DISPLAY 'RECEIVE'
           .

       pa-command-process.
           CALL STATIC 'C$SLEEP' USING 1
           .
