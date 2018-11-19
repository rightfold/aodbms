       IDENTIFICATION DIVISION.
       PROGRAM-ID. aodbms.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ******************************************************************
      * These data items contain ZMQ constants.

       01 FILLER.
           02 ws-zmq-rep               BINARY-LONG SIGNED VALUE 4.

      ******************************************************************
      * These data items contain the DBMS configuration.

       01 ws-configuration.
           02 ws-data-path             PIC X(256).
           02 ws-listen-address        PIC X(256).

      ******************************************************************
      * These data items contain ZMQ objects and ephemeral data.

       01 ws-zmq.
           02 ws-context               POINTER.
           02 ws-socket                POINTER.

           02 ws-ephemeral.

      *        ZMQ expects ws-message to be 8-byte aligned, and the
      *        size must be 64 bytes to correspond to the zmq_msg_t
      *        structure found in the zmq.h header.
               03 ws-message               PIC X(64).

      *        NUL-terminated, hence the increased size.
               03 ws-address               PIC X(257).

      *        Return status and flags are 32-bit signed integers.
               03 ws-status                BINARY-LONG SIGNED.
               03 ws-flags                 BINARY-LONG SIGNED.

      ******************************************************************
      * These data items used to communicate with the se-crash section.

       01 ws-crash.
           02 ws-origin                PIC X(20).

       PROCEDURE DIVISION.

      ******************************************************************
      * This section marks the entry point of the DBMS.

       se-entry SECTION.

       pa-entry.
           PERFORM se-initialize
           PERFORM se-command FOREVER
           .

      ******************************************************************
      * This section performs initialization of global state.

       se-initialize SECTION.

       pa-initialize-configuration.
           MOVE '/var/lib/aodbms/data' TO ws-data-path
           MOVE 'tcp://127.0.0.1:8000' TO ws-listen-address
           .

       pa-initialize-zmq-context.
           CALL STATIC 'zmq_ctx_new' GIVING ws-context OF ws-zmq
           IF ws-context OF ws-zmq IS EQUAL TO NULL THEN
               MOVE 'zmq_ctx_new' TO ws-origin OF ws-crash
               GO TO se-crash
           END-IF
           .

       pa-initialize-zmq-socket.
           CALL STATIC 'zmq_socket'
               USING VALUE ws-context OF ws-zmq, VALUE ws-zmq-rep
               GIVING ws-socket OF ws-zmq
           IF ws-socket OF ws-zmq IS EQUAL TO NULL THEN
               MOVE 'zmq_socket' TO ws-origin OF ws-crash
               GO TO se-crash
           END-IF
           .

       pa-initialize-zmq-bind.
           STRING ws-listen-address DELIMITED BY SPACES, X'00'
               INTO ws-address OF ws-zmq
           CALL STATIC 'zmq_bind'
               USING VALUE ws-socket OF ws-zmq
                     REFERENCE ws-address OF ws-zmq
               GIVING ws-status OF ws-zmq
           IF ws-status OF ws-zmq IS EQUAL TO -1 THEN
               MOVE 'zmq_bind' TO ws-origin OF ws-crash
               GO TO se-crash
           END-IF
           .

       pa-initialize-zmq-msg.
           CALL STATIC 'zmq_msg_init'
               USING ws-message OF ws-zmq
               GIVING ws-status OF ws-zmq
           .

      ******************************************************************
      * This section receives and executes a single command.

       se-command SECTION.

       pa-command-receive.
           MOVE 0 TO ws-flags OF ws-zmq
           CALL STATIC 'zmq_msg_recv'
               USING REFERENCE ws-message OF ws-zmq
                     VALUE ws-socket OF ws-zmq
                     VALUE ws-flags OF ws-zmq
               GIVING ws-status OF ws-zmq
           IF ws-status OF ws-zmq IS EQUAL TO -1 THEN
               MOVE 'zmq_msg_recv' TO ws-origin OF ws-crash
               GO TO se-crash
           END-IF
           .

       pa-command-process.
           DISPLAY ws-message OF ws-zmq
           .

       pa-command-respond.
           MOVE 0 TO ws-flags OF ws-zmq
           CALL STATIC 'zmq_msg_send'
               USING REFERENCE ws-message OF ws-zmq
                     VALUE ws-socket OF ws-zmq
                     VALUE ws-flags OF ws-zmq
               GIVING ws-status OF ws-zmq
           IF ws-status OF ws-zmq IS EQUAL TO -1 THEN
               MOVE 'zmq_msg_send' TO ws-origin OF ws-crash
               GO TO se-crash
           END-IF
           .

      ******************************************************************
      * This section is used for crashing.

       se-crash SECTION.

       pa-crash-announce.
           DISPLAY FUNCTION TRIM(ws-origin OF ws-crash) UPON SYSERR
           .

       pa-crash-exit.
           MOVE 1 TO RETURN-CODE
           STOP RUN
           .
