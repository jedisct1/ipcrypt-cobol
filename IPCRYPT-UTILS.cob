      ******************************************************************
      * IPCRYPT-UTILS - Utility Functions for IPCrypt
      * Part of IPCrypt COBOL Implementation
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IPCRYPT-UTILS.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNU-LINUX.
       OBJECT-COMPUTER. GNU-LINUX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      ******************************************************************
      * IP ADDRESS STRUCTURES
      ******************************************************************
       01  WS-IP-WORK-AREA.
           05  WS-IP-STRING     PIC X(39).
           05  WS-DEST-BLOCK    PIC X(16).
           05  WS-I             PIC 9(03) COMP.
           05  WS-J             PIC 9(03) COMP.
           05  WS-K             PIC 9(03) COMP.
           05  WS-IP-PART       PIC 9(03) COMP.
           05  WS-CHAR-POS      PIC 9(03) COMP.
           05  WS-DOT-COUNT     PIC 9(02) COMP.
           05  WS-COLON-COUNT   PIC 9(02) COMP.

      ******************************************************************
      * HEX CONVERSION WORK AREA
      ******************************************************************
       01  WS-HEX-WORK-AREA.
           05  WS-HEX-STRING    PIC X(32).
           05  WS-HEX-DEST      PIC X(16).
           05  WS-INPUT-CHAR    PIC X(01).
           05  WS-TEMP-NUM      PIC 9(03) COMP.
           05  WS-TEMP-VAL      PIC 9(03) COMP.
           05  WS-HEX-DIGIT     PIC 9(02) COMP.
           05  WS-BYTE-VAL      PIC 9(03) COMP.

      ******************************************************************
      * BLOCK MANIPULATION WORK AREA
      ******************************************************************
       01  WS-BLOCK-WORK-AREA.
           05  WS-SOURCE-BLOCK  PIC X(16).
           05  WS-TARGET-BLOCK  PIC X(16).
           05  WS-XOR-BLOCK     PIC X(16).
           05  WS-TWEAK-8       PIC X(08).
           05  WS-TWEAK-16      PIC X(16).

      ******************************************************************
      * PARSING WORK AREAS
      ******************************************************************
       01  WS-PARSE-WORK.
           05  WS-CURRENT-PART  PIC X(04).
           05  WS-PART-LENGTH   PIC 9(02) COMP.
           05  WS-NUMERIC-PART  PIC 9(03) COMP.
           05  WS-START-POS     PIC 9(03) COMP.
           05  WS-END-POS       PIC 9(03) COMP.

      ******************************************************************
      * UTILITY STATUS FLAGS
      ******************************************************************
       01  WS-UTIL-STATUS       PIC X(01) VALUE 'Y'.
           88  UTIL-SUCCESS     VALUE 'Y'.
           88  UTIL-ERROR       VALUE 'N'.
           88  UTIL-INVALID-IP  VALUE 'I'.
           88  UTIL-INVALID-HEX VALUE 'H'.

       01  WS-ERROR-MESSAGE     PIC X(50).

      ******************************************************************
      * XOR OPERATION VARIABLES
      ******************************************************************
       01  WS-XOR-VARS.
           05  WS-TEMP-BYTE-VAL PIC X(01).
           05  WS-RESULT-BYTE   PIC X(01).
           05  WS-XOR-RESULT    PIC X(01).

      ******************************************************************
      * LINKAGE SECTION - For receiving parameters from callers
      ******************************************************************
       LINKAGE SECTION.
       01  LS-FUNCTION-NAME     PIC X(30).
       01  LS-PARAM-1           PIC X(64).
       01  LS-PARAM-2           PIC X(64).
       01  LS-PARAM-3           PIC X(64).
       01  LS-STATUS            PIC X(01).

       PROCEDURE DIVISION USING LS-FUNCTION-NAME
                               LS-PARAM-1
                               LS-PARAM-2
                               LS-PARAM-3
                               LS-STATUS.

      ******************************************************************
      * MAIN-DISPATCHER
      * Routes function calls to appropriate internal routines
      ******************************************************************
       MAIN-DISPATCHER.
           SET UTIL-SUCCESS TO TRUE
      *    IPCRYPT-UTILS called
           
           EVALUATE LS-FUNCTION-NAME
               WHEN 'IP-TO-BYTES'
      *            IP-TO-BYTES function
                   MOVE LS-PARAM-1 TO WS-IP-STRING
                   PERFORM IP-TO-BYTES
                   MOVE WS-DEST-BLOCK TO LS-PARAM-2
                   MOVE WS-UTIL-STATUS TO LS-STATUS
                   
               WHEN 'BYTES-TO-IP'
                   MOVE LS-PARAM-1 TO WS-DEST-BLOCK
                   PERFORM BYTES-TO-IP
                   MOVE WS-IP-STRING TO LS-PARAM-2
                   
               WHEN 'CONVERT-HEX-STRING-TO-BYTES'
                   MOVE LS-PARAM-1 TO WS-HEX-STRING
                   PERFORM CONVERT-HEX-STRING-TO-BYTES
                   MOVE WS-HEX-DEST TO LS-PARAM-2
                   MOVE WS-UTIL-STATUS TO LS-STATUS
                   
               WHEN 'XOR-BLOCKS'
                   MOVE LS-PARAM-1 TO WS-SOURCE-BLOCK
                   MOVE LS-PARAM-2 TO WS-TARGET-BLOCK
                   PERFORM XOR-BLOCKS
                   MOVE WS-XOR-BLOCK TO LS-PARAM-2
                   
               WHEN 'PAD-TWEAK-8TO16'
                   MOVE LS-PARAM-1 TO WS-TWEAK-8
                   PERFORM PAD-TWEAK-8TO16
                   MOVE WS-TWEAK-16 TO LS-PARAM-2
                   
               WHEN 'COPY-BLOCK'
                   MOVE LS-PARAM-1 TO WS-SOURCE-BLOCK
                   PERFORM COPY-BLOCK
                   MOVE WS-TARGET-BLOCK TO LS-PARAM-2
                   
               WHEN OTHER
                   SET UTIL-ERROR TO TRUE
                   STRING "Unknown function: " LS-FUNCTION-NAME
                       INTO WS-ERROR-MESSAGE
                   END-STRING
           END-EVALUATE
           
           GOBACK.

      ******************************************************************
      * IP-TO-BYTES
      * Convert IP address string to 16-byte representation
      ******************************************************************
       IP-TO-BYTES.
           SET UTIL-SUCCESS TO TRUE
           MOVE ALL X"00" TO WS-DEST-BLOCK
           
      * Determine if IPv4 or IPv6 by counting dots and colons
           MOVE 0 TO WS-DOT-COUNT WS-COLON-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > FUNCTION LENGTH(
                       FUNCTION TRIM(WS-IP-STRING))
               IF WS-IP-STRING(WS-I:1) = '.'
                   ADD 1 TO WS-DOT-COUNT
               END-IF
               IF WS-IP-STRING(WS-I:1) = ':'
                   ADD 1 TO WS-COLON-COUNT
               END-IF
           END-PERFORM
           
           IF WS-DOT-COUNT = 3 AND WS-COLON-COUNT = 0
               PERFORM PARSE-IPV4
           ELSE
               IF WS-COLON-COUNT >= 2
                   PERFORM PARSE-IPV6
               ELSE
                   SET UTIL-INVALID-IP TO TRUE
                   MOVE "Invalid IP address format" TO WS-ERROR-MESSAGE
               END-IF
           END-IF
           EXIT.

      ******************************************************************
      * PARSE-IPV4
      * Parse IPv4 and convert to IPv4-mapped IPv6 format
      ******************************************************************
       PARSE-IPV4.
           SET UTIL-SUCCESS TO TRUE
           MOVE ALL X"00" TO WS-DEST-BLOCK
           
      * Set IPv4-mapped IPv6 prefix: first 10 bytes are 0, next 2 are FF
           MOVE X"FFFF" TO WS-DEST-BLOCK(11:2)
           
      * Parse the 4 IPv4 octets
           MOVE 1 TO WS-START-POS WS-K
           PERFORM 4 TIMES
               PERFORM FIND-NEXT-DOT
      *        Found dot in IP address
               IF WS-END-POS > WS-START-POS
                   COMPUTE WS-PART-LENGTH = WS-END-POS - WS-START-POS
                   MOVE WS-IP-STRING(WS-START-POS:WS-PART-LENGTH)
                        TO WS-CURRENT-PART
      *            Parsing octet
                   PERFORM CONVERT-DECIMAL-STRING
                   IF WS-NUMERIC-PART >= 0 AND WS-NUMERIC-PART <= 255
                       COMPUTE WS-I = 12 + WS-K
                       MOVE FUNCTION CHAR(WS-NUMERIC-PART) 
                            TO WS-DEST-BLOCK(WS-I:1)
                       ADD 1 TO WS-K
                       COMPUTE WS-START-POS = WS-END-POS + 1
                   ELSE
                       SET UTIL-INVALID-IP TO TRUE
                       EXIT
                   END-IF
               ELSE
                   SET UTIL-INVALID-IP TO TRUE
                   EXIT
               END-IF
           END-PERFORM
           EXIT.

      ******************************************************************
      * PARSE-IPV6 
      * Parse IPv6 address to 16-byte format
      ******************************************************************
       PARSE-IPV6.
           SET UTIL-SUCCESS TO TRUE
           MOVE ALL X"00" TO WS-DEST-BLOCK
      * Simplified IPv6 parsing - would need full implementation
      * For now, just clear the block (placeholder)
           MOVE "IPv6 parsing not fully implemented" 
               TO WS-ERROR-MESSAGE
           EXIT.

      ******************************************************************
      * FIND-NEXT-DOT
      * Find position of next dot or end of string
      ******************************************************************
       FIND-NEXT-DOT.
           MOVE WS-START-POS TO WS-END-POS
           PERFORM UNTIL WS-END-POS > FUNCTION LENGTH(
                   FUNCTION TRIM(WS-IP-STRING))
                   OR WS-IP-STRING(WS-END-POS:1) = '.'
               ADD 1 TO WS-END-POS
           END-PERFORM
           
           IF WS-IP-STRING(WS-END-POS:1) NOT = '.'
               COMPUTE WS-END-POS =
                   FUNCTION LENGTH(FUNCTION TRIM(WS-IP-STRING)) + 1
           END-IF
           EXIT.

      ******************************************************************
      * CONVERT-DECIMAL-STRING
      * Convert decimal string to numeric value
      ******************************************************************
       CONVERT-DECIMAL-STRING.
           MOVE 0 TO WS-NUMERIC-PART
      *    Testing numval
           IF FUNCTION TEST-NUMVAL(WS-CURRENT-PART) = 0
               COMPUTE WS-NUMERIC-PART =
                   FUNCTION NUMVAL(WS-CURRENT-PART)
      *        Converted to numeric
           ELSE
      *        TEST-NUMVAL failed
               SET UTIL-INVALID-IP TO TRUE
           END-IF
           EXIT.

      ******************************************************************
      * CONVERT-HEX-STRING-TO-BYTES
      * Convert hex string to binary bytes
      ******************************************************************
       CONVERT-HEX-STRING-TO-BYTES.
           SET UTIL-SUCCESS TO TRUE
           MOVE ALL X"00" TO WS-HEX-DEST
           
           COMPUTE WS-J = FUNCTION LENGTH(
               FUNCTION TRIM(WS-HEX-STRING))
           IF WS-J = 0 OR FUNCTION MOD(WS-J, 2) NOT = 0
               SET UTIL-INVALID-HEX TO TRUE
               MOVE "Invalid hex string length" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
           MOVE 1 TO WS-K
           PERFORM VARYING WS-I FROM 1 BY 2 UNTIL WS-I > WS-J
               MOVE WS-HEX-STRING(WS-I:2) TO WS-CURRENT-PART
               PERFORM CONVERT-HEX-PAIR
               IF UTIL-SUCCESS
                   MOVE FUNCTION CHAR(WS-BYTE-VAL) 
                       TO WS-HEX-DEST(WS-K:1)
                   ADD 1 TO WS-K
               ELSE
                   EXIT
               END-IF
           END-PERFORM
           EXIT.

      ******************************************************************
      * CONVERT-HEX-PAIR
      * Convert 2-character hex string to byte value
      ******************************************************************
       CONVERT-HEX-PAIR.
           MOVE 0 TO WS-BYTE-VAL
           
      * Convert first hex digit
           MOVE WS-CURRENT-PART(1:1) TO WS-INPUT-CHAR
           PERFORM CONVERT-HEX-DIGIT
           IF UTIL-SUCCESS
               COMPUTE WS-BYTE-VAL = WS-HEX-DIGIT * 16
           ELSE
               EXIT
           END-IF
           
      * Convert second hex digit
           MOVE WS-CURRENT-PART(2:1) TO WS-INPUT-CHAR
           PERFORM CONVERT-HEX-DIGIT
           IF UTIL-SUCCESS
               ADD WS-HEX-DIGIT TO WS-BYTE-VAL
           END-IF
           EXIT.


      ******************************************************************
      * CONVERT-HEX-DIGIT
      * Convert single hex character to numeric value
      ******************************************************************
       CONVERT-HEX-DIGIT.
           EVALUATE WS-INPUT-CHAR
               WHEN '0' THRU '9'
                   COMPUTE WS-HEX-DIGIT = 
                       FUNCTION ORD(WS-INPUT-CHAR) - 48
               WHEN 'A' THRU 'F'
                   COMPUTE WS-HEX-DIGIT = 
                       FUNCTION ORD(WS-INPUT-CHAR) - 55
               WHEN 'a' THRU 'f'
                   COMPUTE WS-HEX-DIGIT = 
                       FUNCTION ORD(WS-INPUT-CHAR) - 87
               WHEN OTHER
                   SET UTIL-INVALID-HEX TO TRUE
                   MOVE "Invalid hex character" TO WS-ERROR-MESSAGE
           END-EVALUATE
           EXIT.

      ******************************************************************
      * PAD-TWEAK-8TO16
      * Pad 8-byte tweak to 16 bytes for KIASU-BC
      ******************************************************************
       PAD-TWEAK-8TO16.
           SET UTIL-SUCCESS TO TRUE
           MOVE ALL X"00" TO WS-TWEAK-16
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               COMPUTE WS-J = (WS-I - 1) * 2 + 1
               COMPUTE WS-K = (WS-I - 1) * 4 + 1
               MOVE WS-TWEAK-8(WS-J:2) TO WS-TWEAK-16(WS-K:2)
      * Bytes 3 and 4 of each group remain 0x00
           END-PERFORM
           EXIT.

      ******************************************************************
      * COPY-BLOCK
      * Copy one 16-byte block to another
      ******************************************************************
       COPY-BLOCK.
           MOVE WS-SOURCE-BLOCK TO WS-TARGET-BLOCK
           EXIT.

      ******************************************************************
      * XOR-BLOCKS
      * XOR two 16-byte blocks using table lookup
      ******************************************************************
       XOR-BLOCKS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
               MOVE WS-SOURCE-BLOCK(WS-I:1) TO WS-TEMP-BYTE-VAL
               MOVE WS-TARGET-BLOCK(WS-I:1) TO WS-RESULT-BYTE
               PERFORM XOR-SINGLE-BYTE
               MOVE WS-XOR-RESULT TO WS-XOR-BLOCK(WS-I:1)
           END-PERFORM
           EXIT.

      ******************************************************************
      * XOR-SINGLE-BYTE
      * XOR two single bytes (calls IPCRYPT-TABLES)
      ******************************************************************
       XOR-SINGLE-BYTE.
           CALL 'IPCRYPT-TABLES' USING 'XOR-BYTES'
               WS-TEMP-BYTE-VAL WS-RESULT-BYTE WS-XOR-RESULT
           EXIT.

      ******************************************************************
      * CLEAR-BLOCK
      * Clear a 16-byte block with zeros
      ******************************************************************
       CLEAR-BLOCK.
           MOVE ALL X"00" TO WS-TARGET-BLOCK
           EXIT.

      ******************************************************************
      * SECURE-CLEAR-MEMORY
      * Securely clear sensitive memory areas
      ******************************************************************
       SECURE-CLEAR-MEMORY.
           PERFORM 3 TIMES
               MOVE ALL X"FF" TO WS-TARGET-BLOCK
               MOVE ALL X"AA" TO WS-TARGET-BLOCK
               MOVE ALL X"55" TO WS-TARGET-BLOCK
               MOVE ALL X"00" TO WS-TARGET-BLOCK
           END-PERFORM
           EXIT.

      ******************************************************************
      * BYTES-TO-IP
      * Convert 16-byte representation back to IP address string
      ******************************************************************
       BYTES-TO-IP.
           SET UTIL-SUCCESS TO TRUE
           MOVE SPACES TO WS-IP-STRING
           
      * Check for IPv4-mapped IPv6 format
           IF WS-DEST-BLOCK(1:10) = ALL X"00" AND
              WS-DEST-BLOCK(11:2) = X"FFFF"
      * Convert to IPv4 format
               PERFORM CONVERT-TO-IPV4-STRING
           ELSE
      * Convert to IPv6 format (placeholder)
               PERFORM CONVERT-TO-IPV6-STRING
           END-IF
           EXIT.

      ******************************************************************
      * CONVERT-TO-IPV4-STRING
      * Convert IPv4-mapped bytes to dotted decimal string
      ******************************************************************
       CONVERT-TO-IPV4-STRING.
           MOVE SPACES TO WS-IP-STRING
           MOVE 1 TO WS-CHAR-POS
           
           PERFORM VARYING WS-I FROM 13 BY 1 UNTIL WS-I > 16
               COMPUTE WS-NUMERIC-PART = 
                   FUNCTION ORD(WS-DEST-BLOCK(WS-I:1))
               PERFORM APPEND-DECIMAL-TO-STRING
               IF WS-I < 16
                   MOVE '.' TO WS-IP-STRING(WS-CHAR-POS:1)
                   ADD 1 TO WS-CHAR-POS
               END-IF
           END-PERFORM
           EXIT.

      ******************************************************************
      * APPEND-DECIMAL-TO-STRING
      * Append decimal number to IP string
      ******************************************************************
       APPEND-DECIMAL-TO-STRING.
           MOVE FUNCTION TRIM(FUNCTION NUMVAL-C(WS-NUMERIC-PART))
                TO WS-CURRENT-PART
           COMPUTE WS-PART-LENGTH = 
               FUNCTION LENGTH(FUNCTION TRIM(WS-CURRENT-PART))
           MOVE WS-CURRENT-PART(1:WS-PART-LENGTH) 
                TO WS-IP-STRING(WS-CHAR-POS:WS-PART-LENGTH)
           ADD WS-PART-LENGTH TO WS-CHAR-POS
           EXIT.

      ******************************************************************
      * CONVERT-TO-IPV6-STRING
      * Convert IPv6 bytes to colon-separated hex string (placeholder)
      ******************************************************************
       CONVERT-TO-IPV6-STRING.
           MOVE "IPv6 to string conversion not implemented" 
                TO WS-ERROR-MESSAGE
           SET UTIL-ERROR TO TRUE
           EXIT.

      ******************************************************************
      * INITIALIZE-UTILS
      * Initialize utility functions
      ******************************************************************
       INITIALIZE-UTILS.
           SET UTIL-SUCCESS TO TRUE
           MOVE SPACES TO WS-ERROR-MESSAGE
           MOVE ALL X"00" TO WS-IP-WORK-AREA
           MOVE ALL X"00" TO WS-HEX-WORK-AREA
           MOVE ALL X"00" TO WS-BLOCK-WORK-AREA
           EXIT.

       END PROGRAM IPCRYPT-UTILS.