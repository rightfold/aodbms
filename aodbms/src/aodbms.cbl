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
               03 ws-message           PIC X(64).

      *        NUL-terminated, hence the increased size.
               03 ws-address           PIC X(257).

      *        Return status and flags are 32-bit signed integers.
               03 ws-status            BINARY-LONG SIGNED.
               03 ws-flags             BINARY-LONG SIGNED.

      ******************************************************************
      * These data items used to communicate with the se-check section.

       01 ws-check                     PIC X.
           88 ws-zmq-bind              VALUE 'B'.
           88 ws-zmq-ctx-new           VALUE 'C'.
           88 ws-zmq-msg-recv          VALUE 'R'.
           88 ws-zmq-msg-send          VALUE 'S'.
           88 ws-zmq-socket            VALUE 'E'.

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

           SET ws-zmq-ctx-new OF ws-check TO TRUE
           PERFORM se-check
           .

       pa-initialize-zmq-socket.
           CALL STATIC 'zmq_socket'
               USING VALUE ws-context OF ws-zmq, VALUE ws-zmq-rep
               GIVING ws-socket OF ws-zmq

           SET ws-zmq-socket OF ws-check TO TRUE
           PERFORM se-check
           .

       pa-initialize-zmq-bind.
           STRING ws-listen-address DELIMITED BY SPACES, X'00'
               INTO ws-address OF ws-zmq
           CALL STATIC 'zmq_bind'
               USING VALUE ws-socket OF ws-zmq
                     REFERENCE ws-address OF ws-zmq
               GIVING ws-status OF ws-zmq

           SET ws-zmq-bind OF ws-check TO TRUE
           PERFORM se-check
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

           SET ws-zmq-msg-recv OF ws-check TO TRUE
           PERFORM se-check
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

           SET ws-zmq-msg-send OF ws-check TO TRUE
           PERFORM se-check
           .

      ******************************************************************
      * This section is used for checking errors and crashing.

       se-check SECTION.

       pa-check-analyze.
           IF ws-zmq-bind OF ws-check AND
               ws-status OF ws-zmq IS EQUAL TO -1 THEN
               GO TO pa-check-crash
           END-IF

           IF ws-zmq-ctx-new OF ws-check AND
               ws-context OF ws-zmq IS EQUAL TO NULL THEN
               GO TO pa-check-crash
           END-IF

           IF ws-zmq-msg-recv OF ws-check AND
               ws-status OF ws-zmq IS EQUAL TO -1 THEN
               GO TO pa-check-crash
           END-IF

           IF ws-zmq-msg-send OF ws-check AND
               ws-status OF ws-zmq IS EQUAL TO -1 THEN
               GO TO pa-check-crash
           END-IF

           IF ws-zmq-socket OF ws-check AND
               ws-socket OF ws-zmq IS EQUAL TO NULL THEN
               GO TO pa-check-crash
           END-IF

           EXIT SECTION
           .

       pa-check-crash.
           DISPLAY ws-check UPON SYSERR
           MOVE 1 TO RETURN-CODE
           STOP RUN
           .
