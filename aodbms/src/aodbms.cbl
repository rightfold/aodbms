       IDENTIFICATION DIVISION.
       PROGRAM-ID. aodbms.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ws-configuration.
           02 ws-data-path             PIC X(256).
           02 ws-listen-address        PIC X(256).

       01 ws-zmq.
           02 ws-zmq-rep               BINARY-LONG SIGNED VALUE 4.
           02 ws-status                BINARY-LONG SIGNED.
           02 ws-context               POINTER.
           02 ws-socket                POINTER.
           02 ws-bind-address          PIC X(257).

       01 ws-crash.
           02 ws-origin                PIC X(20).

       PROCEDURE DIVISION.

      ******************************************************************
      * This section marks the entry point of the DBMS.

       se-entry SECTION.

       pa-entry.
           PERFORM se-initialize
           PERFORM se-command FOREVER
           STOP RUN
           .

      ******************************************************************
      * This section performs initialization of global state.

       se-initialize SECTION.

       pa-initialize-configuration.
           MOVE '/var/lib/aodbms/data' TO ws-data-path
           MOVE 'tcp://127.0.0.1:8000' TO ws-listen-address
           .

       pa-initialize-zmq-context.
           CALL STATIC 'zmq_ctx_new' GIVING ws-context
           IF ws-context IS EQUAL TO NULL THEN
               MOVE 'zmq_ctx_new' TO ws-origin
               GO TO se-crash
           END-IF
           .

       pa-initialize-zmq-socket.
           CALL STATIC 'zmq_socket'
               USING VALUE ws-context, VALUE ws-zmq-rep
               GIVING ws-socket
           IF ws-socket IS EQUAL TO NULL THEN
               MOVE 'zmq_socket' TO ws-origin
               GO TO se-crash
           END-IF
           .

       pa-initialize-zmq-bind.
           STRING ws-listen-address DELIMITED BY SPACES, X'00'
               INTO ws-bind-address
           CALL STATIC 'zmq_bind'
               USING VALUE ws-socket, REFERENCE ws-bind-address
               GIVING ws-status
           IF ws-status IS EQUAL TO -1 THEN
               MOVE 'zmq_bind' TO ws-origin
               GO TO se-crash
           END-IF
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

      ******************************************************************
      * This section is used for crashing.

       se-crash SECTION.

       pa-crash-announce.
           DISPLAY FUNCTION TRIM(ws-origin) UPON SYSERR
           .

       pa-crash-exit.
           MOVE 1 TO RETURN-CODE
           STOP RUN
           .
