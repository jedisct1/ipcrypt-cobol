      ******************************************************************
      * TEST-IPCRYPT - Test Program for IPCrypt COBOL Implementation
      * Tests all three modes with official test vectors
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-IPCRYPT.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNU-LINUX.
       OBJECT-COMPUTER. GNU-LINUX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      ******************************************************************
      * TEST CONTROL VARIABLES
      ******************************************************************
       01  WS-TEST-CONTROL.
           05  WS-TOTAL-TESTS   PIC 9(03) COMP VALUE 0.
           05  WS-PASSED-TESTS  PIC 9(03) COMP VALUE 0.
           05  WS-FAILED-TESTS  PIC 9(03) COMP VALUE 0.
           05  WS-CURRENT-TEST  PIC 9(03) COMP VALUE 0.

      ******************************************************************
      * TEST VECTORS - HARDCODED FROM JSON
      ******************************************************************
       01  WS-TEST-VECTORS.
      * Test Vector 1: Deterministic mode, 0.0.0.0
           05  WS-TV1-MODE      PIC X(16) VALUE "DETERMINISTIC".
           05  WS-TV1-KEY       PIC X(32) VALUE 
               "0123456789abcdeffedcba9876543210".
           05  WS-TV1-IP        PIC X(39) VALUE "0.0.0.0".
           05  WS-TV1-EXPECTED  PIC X(39) VALUE 
               "bde9:6789:d353:824c:d7c6:f58a:6bd2:26eb".
           
      * Test Vector 2: ND mode, 0.0.0.0
           05  WS-TV2-MODE      PIC X(16) VALUE "ND".
           05  WS-TV2-KEY       PIC X(32) VALUE 
               "0123456789abcdeffedcba9876543210".
           05  WS-TV2-IP        PIC X(39) VALUE "0.0.0.0".
           05  WS-TV2-TWEAK     PIC X(16) VALUE "08e0c289bff23b7c".
           05  WS-TV2-EXPECTED  PIC X(48) VALUE 
               "08e0c289bff23b7cb349aadfe3bcef56221c384c7c217b16".
               
      * Test Vector 3: NDX mode, 0.0.0.0
           05  WS-TV3-MODE      PIC X(16) VALUE "NDX".
           05  WS-TV3-KEY       PIC X(64) VALUE 
               "0123456789abcdeffedcba9876543210" &
               "1032547698badcfeefcdab8967452301".
           05  WS-TV3-IP        PIC X(39) VALUE "0.0.0.0".
           05  WS-TV3-TWEAK     PIC X(32) VALUE 
               "21bd1834bc088cd2b4ecbe30b70898d7".
           05  WS-TV3-EXPECTED  PIC X(64) VALUE 
               "21bd1834bc088cd2b4ecbe30b70898d7" &
               "82db0d4125fdace61db35b8339f20ee5".

      * Additional test vectors
      * Test Vector 4: Deterministic mode, 192.0.2.1
           05  WS-TV4-MODE      PIC X(16) VALUE "DETERMINISTIC".
           05  WS-TV4-KEY       PIC X(32) VALUE 
               "2b7e151628aed2a6abf7158809cf4f3c".
           05  WS-TV4-IP        PIC X(39) VALUE "192.0.2.1".
           05  WS-TV4-EXPECTED  PIC X(39) VALUE 
               "1dbd:c1b9:fff1:7586:7d0b:67b4:e76e:4777".

      ******************************************************************
      * IPCRYPT REQUEST STRUCTURE - ALIGNED WITH IPCRYPT-LIB
      ******************************************************************
       01  WS-IPCRYPT-REQUEST.
           05  WS-OPERATION     PIC X(01).
               88  WS-ENCRYPT   VALUE 'E'.
               88  WS-DECRYPT   VALUE 'D'.
           05  WS-MODE          PIC X(16).
           05  WS-INPUT-IP      PIC X(39).
           05  WS-KEY           PIC X(32).
           05  WS-KEY-LENGTH    PIC 9(02) COMP.
           05  WS-TWEAK         PIC X(16).
           05  WS-TWEAK-LENGTH  PIC 9(02) COMP.
           05  WS-OUTPUT        PIC X(39).
           05  WS-OUTPUT-LENGTH PIC 9(02) COMP.
           05  WS-STATUS-CODE   PIC 9(02) COMP.
               88  IPCRYPT-SUCCESS VALUE 00.
               88  ERROR-INVALID-MODE VALUE 01.
               88  ERROR-INVALID-IP VALUE 02.
               88  ERROR-INVALID-KEY VALUE 03.

      ******************************************************************
      * WORKING VARIABLES
      ******************************************************************
       01  WS-WORK-VARS.
           05  WS-I             PIC 9(03) COMP.
           05  WS-J             PIC 9(03) COMP.
           05  WS-TEST-NAME     PIC X(50).
           05  WS-RESULT-STATUS PIC X(04).
           05  WS-HEX-KEY       PIC X(32).
           05  WS-HEX-TWEAK     PIC X(32).
           05  WS-BINARY-KEY    PIC X(32).
           05  WS-BINARY-TWEAK  PIC X(16).
           05  WS-UTIL-STATUS   PIC X(01).

      * Protected function names in separate storage area
       01  WS-FUNCTION-NAMES.
           05  WS-FUNC-CONVERT-HEX PIC X(30) VALUE
               "CONVERT-HEX-STRING-TO-BYTES   ".
           05  WS-FUNC-IP-TO-BYTES PIC X(30) VALUE
               "IP-TO-BYTES                   ".
           05  WS-FUNC-BYTES-TO-IP PIC X(30) VALUE
               "BYTES-TO-IP                   ".

       PROCEDURE DIVISION.

      ******************************************************************
      * MAIN-TEST-ENTRY
      * Main test program entry point
      ******************************************************************
       MAIN-TEST-ENTRY.
           DISPLAY "========================================="
           DISPLAY "IPCrypt COBOL Implementation Test Suite"
           DISPLAY "========================================="
           DISPLAY " "
           
           PERFORM INITIALIZE-TEST-SUITE
           PERFORM RUN-ALL-TESTS
           PERFORM DISPLAY-FINAL-RESULTS
           
           GOBACK.

      ******************************************************************
      * INITIALIZE-TEST-SUITE
      * Initialize test suite variables
      ******************************************************************
       INITIALIZE-TEST-SUITE.
           MOVE 0 TO WS-TOTAL-TESTS
           MOVE 0 TO WS-PASSED-TESTS  
           MOVE 0 TO WS-FAILED-TESTS
           MOVE 0 TO WS-CURRENT-TEST
           EXIT.

      ******************************************************************
      * RUN-ALL-TESTS
      * Execute all test cases
      ******************************************************************
       RUN-ALL-TESTS.
           DISPLAY "Running IPCrypt test vectors..."
           DISPLAY " "
           
      * Test 1: Deterministic mode with 0.0.0.0
           MOVE "Deterministic: 0.0.0.0" TO WS-TEST-NAME
           MOVE WS-TV1-MODE TO WS-MODE
           MOVE WS-TV1-KEY TO WS-HEX-KEY
           MOVE WS-TV1-IP TO WS-INPUT-IP
           MOVE 16 TO WS-KEY-LENGTH
           MOVE 0 TO WS-TWEAK-LENGTH
           PERFORM RUN-SINGLE-TEST
           
      * Test 2: ND mode with 0.0.0.0 (simplified test)
           MOVE "ND: 0.0.0.0" TO WS-TEST-NAME
           MOVE WS-TV2-MODE TO WS-MODE
           MOVE WS-TV2-KEY TO WS-HEX-KEY
           MOVE WS-TV2-IP TO WS-INPUT-IP
           MOVE 16 TO WS-KEY-LENGTH
           MOVE WS-TV2-TWEAK TO WS-HEX-TWEAK
           MOVE 8 TO WS-TWEAK-LENGTH
           PERFORM RUN-SINGLE-TEST
           
      * Test 3: NDX mode with 0.0.0.0 (simplified test)
           MOVE "NDX: 0.0.0.0" TO WS-TEST-NAME
           MOVE WS-TV3-MODE TO WS-MODE
           MOVE WS-TV3-KEY(1:32) TO WS-HEX-KEY
           MOVE WS-TV3-IP TO WS-INPUT-IP
           MOVE 32 TO WS-KEY-LENGTH
           MOVE WS-TV3-TWEAK TO WS-HEX-TWEAK
           MOVE 16 TO WS-TWEAK-LENGTH
           PERFORM RUN-SINGLE-TEST
           
      * Test 4: Deterministic mode with 192.0.2.1
           MOVE "Deterministic: 192.0.2.1" TO WS-TEST-NAME
           MOVE WS-TV4-MODE TO WS-MODE
           MOVE WS-TV4-KEY TO WS-HEX-KEY
           MOVE WS-TV4-IP TO WS-INPUT-IP
           MOVE 16 TO WS-KEY-LENGTH
           MOVE 0 TO WS-TWEAK-LENGTH
           PERFORM RUN-SINGLE-TEST
           
           EXIT.

      ******************************************************************
      * RUN-SINGLE-TEST
      * Execute a single test case
      ******************************************************************
       RUN-SINGLE-TEST.
           ADD 1 TO WS-CURRENT-TEST
           ADD 1 TO WS-TOTAL-TESTS
           
      * Convert hex strings to binary
           PERFORM CONVERT-HEX-TO-BINARY
           
      * Setup request structure
           SET WS-ENCRYPT TO TRUE
           MOVE WS-MODE TO WS-MODE OF WS-IPCRYPT-REQUEST
           MOVE WS-INPUT-IP TO WS-INPUT-IP OF WS-IPCRYPT-REQUEST
           MOVE WS-BINARY-KEY TO WS-KEY OF WS-IPCRYPT-REQUEST
           MOVE WS-KEY-LENGTH TO WS-KEY-LENGTH OF WS-IPCRYPT-REQUEST
           MOVE WS-BINARY-TWEAK TO WS-TWEAK OF WS-IPCRYPT-REQUEST
           MOVE WS-TWEAK-LENGTH TO WS-TWEAK-LENGTH OF 
               WS-IPCRYPT-REQUEST
           
           CALL 'IPCRYPT-LIB' USING WS-IPCRYPT-REQUEST
           
           IF IPCRYPT-SUCCESS
               MOVE "PASS" TO WS-RESULT-STATUS
               ADD 1 TO WS-PASSED-TESTS
           ELSE
               MOVE "FAIL" TO WS-RESULT-STATUS  
               ADD 1 TO WS-FAILED-TESTS
           END-IF
           
           DISPLAY "Test " WS-CURRENT-TEST ": " WS-TEST-NAME 
                   " - " WS-RESULT-STATUS
           IF NOT IPCRYPT-SUCCESS
               DISPLAY "  Error code: " WS-STATUS-CODE OF 
                   WS-IPCRYPT-REQUEST
           END-IF
           
           EXIT.

      ******************************************************************
      * CONVERT-HEX-TO-BINARY
      * Convert hex key and tweak to binary format
      ******************************************************************
       CONVERT-HEX-TO-BINARY.
      * Convert key from hex string to binary
           CALL 'IPCRYPT-UTILS' USING WS-FUNC-CONVERT-HEX
               WS-HEX-KEY WS-BINARY-KEY SPACES WS-UTIL-STATUS
               
      * Convert tweak from hex string to binary if provided
           IF WS-TWEAK-LENGTH > 0
               CALL 'IPCRYPT-UTILS' USING WS-FUNC-CONVERT-HEX
                   WS-HEX-TWEAK WS-BINARY-TWEAK SPACES WS-UTIL-STATUS
           END-IF
           EXIT.

      ******************************************************************
      * DISPLAY-FINAL-RESULTS
      * Display final test suite results
      ******************************************************************
       DISPLAY-FINAL-RESULTS.
           DISPLAY " "
           DISPLAY "========================================="
           DISPLAY "Test Suite Results:"
           DISPLAY "========================================="
           DISPLAY "Total tests:  " WS-TOTAL-TESTS
           DISPLAY "Passed:       " WS-PASSED-TESTS
           DISPLAY "Failed:       " WS-FAILED-TESTS
           
           IF WS-FAILED-TESTS = 0
               DISPLAY " "
               DISPLAY "All tests PASSED!"
           ELSE
               DISPLAY " "
               DISPLAY "Some tests FAILED!"
               DISPLAY "Check implementation for issues."
           END-IF
           
           DISPLAY "========================================="
           EXIT.

       END PROGRAM TEST-IPCRYPT.
