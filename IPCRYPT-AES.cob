      ******************************************************************
      * IPCRYPT-AES - Complete AES-128 Implementation for IPCrypt
      * Part of IPCrypt COBOL Implementation
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IPCRYPT-AES.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNU-LINUX.
       OBJECT-COMPUTER. GNU-LINUX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      ******************************************************************
      * AES STATE STRUCTURES
      ******************************************************************
       01  WS-AES-STATE.
           05  WS-STATE-MATRIX  OCCURS 4 TIMES.
               10  WS-STATE-BYTE OCCURS 4 TIMES PIC X(01).

       01  WS-AES-STATE-FLAT REDEFINES WS-AES-STATE PIC X(16).

       01  WS-TEMP-STATE.
           05  WS-TEMP-MATRIX   OCCURS 4 TIMES.
               10  WS-TEMP-BYTE OCCURS 4 TIMES PIC X(01).

       01  WS-TEMP-STATE-FLAT REDEFINES WS-TEMP-STATE PIC X(16).

      ******************************************************************
      * KEY STRUCTURES
      ******************************************************************
       01  WS-MASTER-KEY        PIC X(16).
       
       01  WS-ROUND-KEYS.
           05  WS-ROUND-KEY     OCCURS 11 TIMES.
               10  WS-KEY-MATRIX OCCURS 4 TIMES.
                   15  WS-KEY-BYTE OCCURS 4 TIMES PIC X(01).

       01  WS-ROUND-KEY-FLAT.
           05  WS-ROUND-KEY-ENTRY OCCURS 11 TIMES PIC X(16).

      ******************************************************************
      * WORKING VARIABLES
      ******************************************************************
       01  WS-WORK-VARS.
           05  WS-I             PIC 9(03) COMP.
           05  WS-J             PIC 9(03) COMP.
           05  WS-K             PIC 9(03) COMP.
           05  WS-ROUND         PIC 9(02) COMP.
           05  WS-BYTE-INDEX    PIC 9(03) COMP.
           05  WS-TEMP-BYTE-VAL PIC X(01).
           05  WS-RESULT-BYTE   PIC X(01).
           05  WS-TEMP-WORD     PIC X(04).
           05  WS-INPUT-BYTE    PIC 9(03) COMP.
           05  WS-OUTPUT-BYTE   PIC X(01).
           05  WS-BYTE-A        PIC X(01).
           05  WS-BYTE-B        PIC X(01).
           05  WS-XOR-BYTE      PIC X(01).
           05  WS-RCON-VAL      PIC X(01).

      ******************************************************************
      * KEY EXPANSION WORKING STORAGE
      ******************************************************************
       01  WS-KEY-WORK.
           05  WS-TEMP-KEY-WORD PIC X(04).
           05  WS-PREV-KEY-WORD PIC X(04).
           05  WS-ROT-WORD      PIC X(04).
           05  WS-SUB-WORD      PIC X(04).

      ******************************************************************
      * KIASU-BC AND AES-XTS VARIABLES
      ******************************************************************
       01  WS-KIASU-BC-VARS.
           05  WS-TWEAK-8       PIC X(08).
           05  WS-TWEAK-16      PIC X(16).
           05  WS-INPUT-BLOCK   PIC X(16).
           05  WS-OUTPUT-BLOCK  PIC X(16).
           05  WS-KEY-128       PIC X(16).
           05  WS-KEY-256       PIC X(32).
           05  WS-ENCRYPTED-TWEAK PIC X(16).

      ******************************************************************
      * AES STATUS FLAGS
      ******************************************************************
       01  WS-AES-STATUS        PIC X(01) VALUE 'Y'.
           88  AES-SUCCESS      VALUE 'Y'.
           88  AES-ERROR        VALUE 'N'.

       01  WS-FUNCTION-NAMES.
           05  WS-FUNC-PAD-TWEAK    PIC X(30) VALUE
               "PAD-TWEAK-8TO16               ".

      ******************************************************************
      * LINKAGE SECTION - For receiving parameters from callers
      ******************************************************************
       LINKAGE SECTION.
       01  LS-FUNCTION-NAME     PIC X(30).
       01  LS-PARAM-1           PIC X(64).
       01  LS-PARAM-2           PIC X(64).
       01  LS-PARAM-3           PIC X(64).
       01  LS-PARAM-4           PIC X(64).

       PROCEDURE DIVISION USING LS-FUNCTION-NAME
                               LS-PARAM-1
                               LS-PARAM-2
                               LS-PARAM-3.

      ******************************************************************
      * MAIN-DISPATCHER
      * Routes function calls to appropriate internal routines
      ******************************************************************
       MAIN-DISPATCHER.
           SET AES-SUCCESS TO TRUE
           
           EVALUATE LS-FUNCTION-NAME
               WHEN 'AES-ENCRYPT-BLOCK'
                   MOVE LS-PARAM-1(1:16) TO WS-AES-STATE-FLAT
                   MOVE LS-PARAM-2(1:16) TO WS-MASTER-KEY
                   PERFORM AES-ENCRYPT-BLOCK-INTERNAL
                   MOVE WS-AES-STATE-FLAT TO LS-PARAM-2(1:16)
                   
               WHEN 'AES-DECRYPT-BLOCK'
                   MOVE LS-PARAM-1(1:16) TO WS-AES-STATE-FLAT
                   MOVE LS-PARAM-2(1:16) TO WS-MASTER-KEY
                   PERFORM AES-DECRYPT-BLOCK-INTERNAL
                   MOVE WS-AES-STATE-FLAT TO LS-PARAM-2(1:16)
                   
               WHEN 'KIASU-BC-ENCRYPT'
                   MOVE LS-PARAM-1(1:16) TO WS-INPUT-BLOCK
                   MOVE LS-PARAM-2(1:16) TO WS-KEY-128
                   MOVE LS-PARAM-3(1:8) TO WS-TWEAK-8
                   PERFORM KIASU-BC-ENCRYPT-INTERNAL
                   MOVE WS-OUTPUT-BLOCK TO LS-PARAM-2(1:16)
                   
               WHEN 'KIASU-BC-DECRYPT'
                   MOVE LS-PARAM-1(1:16) TO WS-INPUT-BLOCK
                   MOVE LS-PARAM-2(1:16) TO WS-KEY-128
                   MOVE LS-PARAM-3(1:8) TO WS-TWEAK-8
                   PERFORM KIASU-BC-DECRYPT-INTERNAL
                   MOVE WS-OUTPUT-BLOCK TO LS-PARAM-2(1:16)
                   
               WHEN 'AES-XTS-ENCRYPT'
                   MOVE LS-PARAM-1(1:16) TO WS-INPUT-BLOCK
                   MOVE LS-PARAM-2(1:32) TO WS-KEY-256
                   MOVE LS-PARAM-3(1:16) TO WS-TWEAK-16
                   PERFORM AES-XTS-ENCRYPT-INTERNAL
                   MOVE WS-OUTPUT-BLOCK TO LS-PARAM-2(1:16)
                   
               WHEN 'AES-XTS-DECRYPT'
                   MOVE LS-PARAM-1(1:16) TO WS-INPUT-BLOCK
                   MOVE LS-PARAM-2(1:32) TO WS-KEY-256
                   MOVE LS-PARAM-3(1:16) TO WS-TWEAK-16
                   PERFORM AES-XTS-DECRYPT-INTERNAL
                   MOVE WS-OUTPUT-BLOCK TO LS-PARAM-2(1:16)
                   
               WHEN OTHER
                   SET AES-ERROR TO TRUE
           END-EVALUATE
           
           GOBACK.

      ******************************************************************
      * INITIALIZE-AES
      * Initialize AES components
      ******************************************************************
       INITIALIZE-AES.
           SET AES-SUCCESS TO TRUE
           MOVE ALL X"00" TO WS-AES-STATE
           MOVE ALL X"00" TO WS-ROUND-KEYS
           CALL 'IPCRYPT-TABLES' USING 'INITIALIZE-TABLES'
           EXIT.

      ******************************************************************
      * AES-KEY-EXPANSION
      * Proper AES-128 key expansion to generate 11 round keys
      ******************************************************************
       AES-KEY-EXPANSION.
           MOVE WS-MASTER-KEY TO WS-ROUND-KEY-ENTRY(1)
           
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > 11
               COMPUTE WS-K = WS-I - 1
               
      * Get last word of previous round key
               MOVE WS-ROUND-KEY-ENTRY(WS-K)(13:4) 
                   TO WS-TEMP-KEY-WORD
               
      * Apply key schedule core (RotWord, SubWord, XOR with Rcon)
               PERFORM ROT-WORD
               PERFORM SUB-WORD
               
      * XOR with RCON for first byte
               COMPUTE WS-BYTE-INDEX = WS-K
               PERFORM GET-RCON-VALUE
               MOVE WS-RCON-VAL TO WS-BYTE-A
               MOVE WS-SUB-WORD(1:1) TO WS-BYTE-B
               PERFORM XOR-BYTES
               MOVE WS-XOR-BYTE TO WS-SUB-WORD(1:1)
               
      * Generate first word of new round key
               MOVE WS-ROUND-KEY-ENTRY(WS-K)(1:4) TO WS-PREV-KEY-WORD
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
                   MOVE WS-PREV-KEY-WORD(WS-J:1) TO WS-BYTE-A
                   MOVE WS-SUB-WORD(WS-J:1) TO WS-BYTE-B
                   PERFORM XOR-BYTES
                   COMPUTE WS-BYTE-INDEX = WS-J
                   MOVE WS-XOR-BYTE TO 
                       WS-ROUND-KEY-ENTRY(WS-I)(WS-BYTE-INDEX:1)
               END-PERFORM
               
      * Generate remaining 3 words by XORing with previous word
               PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J > 4
                   COMPUTE WS-BYTE-INDEX = (WS-J - 1) * 4 + 1
                   MOVE WS-ROUND-KEY-ENTRY(WS-K)(WS-BYTE-INDEX:4)
                       TO WS-PREV-KEY-WORD
                   COMPUTE WS-K = (WS-J - 2) * 4 + 1  
                   MOVE WS-ROUND-KEY-ENTRY(WS-I)(WS-K:4)
                       TO WS-TEMP-KEY-WORD
                   PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 4
                       MOVE WS-PREV-KEY-WORD(WS-K:1) TO WS-BYTE-A
                       MOVE WS-TEMP-KEY-WORD(WS-K:1) TO WS-BYTE-B
                       PERFORM XOR-BYTES
                       COMPUTE WS-BYTE-INDEX = 
                           (WS-J - 1) * 4 + WS-K
                       MOVE WS-XOR-BYTE TO 
                           WS-ROUND-KEY-ENTRY(WS-I)(WS-BYTE-INDEX:1)
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           EXIT.

      ******************************************************************
      * ROT-WORD
      * Rotate 4-byte word left by one position
      ******************************************************************
       ROT-WORD.
           MOVE WS-TEMP-KEY-WORD(2:3) TO WS-ROT-WORD(1:3)
           MOVE WS-TEMP-KEY-WORD(1:1) TO WS-ROT-WORD(4:1)
           MOVE WS-ROT-WORD TO WS-TEMP-KEY-WORD
           EXIT.

      ******************************************************************
      * SUB-WORD
      * Apply S-box substitution to 4-byte word
      ******************************************************************
       SUB-WORD.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               COMPUTE WS-INPUT-BYTE = FUNCTION ORD(
                   WS-TEMP-KEY-WORD(WS-J:1))
               PERFORM GET-SBOX-VALUE
               MOVE WS-OUTPUT-BYTE TO WS-SUB-WORD(WS-J:1)
           END-PERFORM
           MOVE WS-SUB-WORD TO WS-TEMP-KEY-WORD
           EXIT.

      ******************************************************************
      * AES-ENCRYPT-BLOCK
      * Complete AES-128 encryption
      ******************************************************************
       AES-ENCRYPT-BLOCK-INTERNAL.
           SET AES-SUCCESS TO TRUE
           PERFORM AES-KEY-EXPANSION
           
      * Initial round key addition
           MOVE 1 TO WS-ROUND
           PERFORM ADD-ROUND-KEY
           
      * Main rounds (1-9)
           PERFORM VARYING WS-ROUND FROM 2 BY 1 UNTIL WS-ROUND > 10
               PERFORM SUB-BYTES
               PERFORM SHIFT-ROWS
               IF WS-ROUND < 10
                   PERFORM MIX-COLUMNS
               END-IF
               PERFORM ADD-ROUND-KEY
           END-PERFORM
           EXIT.

      ******************************************************************
      * AES-DECRYPT-BLOCK  
      * Complete AES-128 decryption
      ******************************************************************
       AES-DECRYPT-BLOCK-INTERNAL.
           SET AES-SUCCESS TO TRUE
           PERFORM AES-KEY-EXPANSION
           
      * Initial round key addition
           MOVE 11 TO WS-ROUND
           PERFORM ADD-ROUND-KEY
           
      * Main rounds (10-2)
           PERFORM VARYING WS-ROUND FROM 10 BY -1 UNTIL WS-ROUND < 2
               PERFORM INV-SHIFT-ROWS
               PERFORM INV-SUB-BYTES
               PERFORM ADD-ROUND-KEY
               IF WS-ROUND > 1
                   PERFORM INV-MIX-COLUMNS
               END-IF
           END-PERFORM
           EXIT.

      ******************************************************************
      * SUB-BYTES
      * Apply S-Box substitution to state
      ******************************************************************
       SUB-BYTES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
                   COMPUTE WS-INPUT-BYTE = FUNCTION ORD(
                       WS-STATE-BYTE(WS-I, WS-J))
                   PERFORM GET-SBOX-VALUE
                   MOVE WS-OUTPUT-BYTE TO WS-STATE-BYTE(WS-I, WS-J)
               END-PERFORM
           END-PERFORM
           EXIT.

      ******************************************************************
      * INV-SUB-BYTES
      * Apply inverse S-Box substitution to state
      ******************************************************************
       INV-SUB-BYTES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
                   COMPUTE WS-INPUT-BYTE = FUNCTION ORD(
                       WS-STATE-BYTE(WS-I, WS-J))
                   PERFORM GET-INV-SBOX-VALUE
                   MOVE WS-OUTPUT-BYTE TO WS-STATE-BYTE(WS-I, WS-J)
               END-PERFORM
           END-PERFORM
           EXIT.

      ******************************************************************
      * SHIFT-ROWS
      * Shift rows transformation
      ******************************************************************
       SHIFT-ROWS.
           MOVE WS-AES-STATE TO WS-TEMP-STATE
      * Row 0: no shift
      * Row 1: shift left by 1
           MOVE WS-TEMP-BYTE(2,2) TO WS-STATE-BYTE(2,1)
           MOVE WS-TEMP-BYTE(2,3) TO WS-STATE-BYTE(2,2)
           MOVE WS-TEMP-BYTE(2,4) TO WS-STATE-BYTE(2,3)
           MOVE WS-TEMP-BYTE(2,1) TO WS-STATE-BYTE(2,4)
      * Row 2: shift left by 2
           MOVE WS-TEMP-BYTE(3,3) TO WS-STATE-BYTE(3,1)
           MOVE WS-TEMP-BYTE(3,4) TO WS-STATE-BYTE(3,2)
           MOVE WS-TEMP-BYTE(3,1) TO WS-STATE-BYTE(3,3)
           MOVE WS-TEMP-BYTE(3,2) TO WS-STATE-BYTE(3,4)
      * Row 3: shift left by 3
           MOVE WS-TEMP-BYTE(4,4) TO WS-STATE-BYTE(4,1)
           MOVE WS-TEMP-BYTE(4,1) TO WS-STATE-BYTE(4,2)
           MOVE WS-TEMP-BYTE(4,2) TO WS-STATE-BYTE(4,3)
           MOVE WS-TEMP-BYTE(4,3) TO WS-STATE-BYTE(4,4)
           EXIT.

      ******************************************************************
      * INV-SHIFT-ROWS
      * Inverse shift rows transformation
      ******************************************************************
       INV-SHIFT-ROWS.
           MOVE WS-AES-STATE TO WS-TEMP-STATE
      * Row 0: no shift
      * Row 1: shift right by 1 (left by 3)
           MOVE WS-TEMP-BYTE(2,4) TO WS-STATE-BYTE(2,1)
           MOVE WS-TEMP-BYTE(2,1) TO WS-STATE-BYTE(2,2)
           MOVE WS-TEMP-BYTE(2,2) TO WS-STATE-BYTE(2,3)
           MOVE WS-TEMP-BYTE(2,3) TO WS-STATE-BYTE(2,4)
      * Row 2: shift right by 2 
           MOVE WS-TEMP-BYTE(3,3) TO WS-STATE-BYTE(3,1)
           MOVE WS-TEMP-BYTE(3,4) TO WS-STATE-BYTE(3,2)
           MOVE WS-TEMP-BYTE(3,1) TO WS-STATE-BYTE(3,3)
           MOVE WS-TEMP-BYTE(3,2) TO WS-STATE-BYTE(3,4)
      * Row 3: shift right by 3 (left by 1)
           MOVE WS-TEMP-BYTE(4,2) TO WS-STATE-BYTE(4,1)
           MOVE WS-TEMP-BYTE(4,3) TO WS-STATE-BYTE(4,2)
           MOVE WS-TEMP-BYTE(4,4) TO WS-STATE-BYTE(4,3)
           MOVE WS-TEMP-BYTE(4,1) TO WS-STATE-BYTE(4,4)
           EXIT.

      ******************************************************************
      * MIX-COLUMNS
      * Mix columns transformation using Galois field multiplication
      ******************************************************************
       MIX-COLUMNS.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               PERFORM MIX-SINGLE-COLUMN
           END-PERFORM
           EXIT.

      ******************************************************************
      * MIX-SINGLE-COLUMN
      * Mix a single column using GF multiplication
      ******************************************************************
       MIX-SINGLE-COLUMN.
      * Save original column
           MOVE WS-STATE-BYTE(1, WS-J) TO WS-TEMP-BYTE(1, 1)
           MOVE WS-STATE-BYTE(2, WS-J) TO WS-TEMP-BYTE(1, 2)
           MOVE WS-STATE-BYTE(3, WS-J) TO WS-TEMP-BYTE(1, 3)
           MOVE WS-STATE-BYTE(4, WS-J) TO WS-TEMP-BYTE(1, 4)
           
      * Apply MixColumns matrix multiplication
      * First row: 2*s0 + 3*s1 + 1*s2 + 1*s3
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,1))
           PERFORM GET-MUL2-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,2))
           PERFORM GET-MUL3-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           MOVE WS-TEMP-BYTE(1,3) TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           MOVE WS-TEMP-BYTE(1,4) TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-STATE-BYTE(1, WS-J)
           
      * Second row: 1*s0 + 2*s1 + 3*s2 + 1*s3
           MOVE WS-TEMP-BYTE(1,1) TO WS-BYTE-A
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,2))
           PERFORM GET-MUL2-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,3))
           PERFORM GET-MUL3-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           MOVE WS-TEMP-BYTE(1,4) TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-STATE-BYTE(2, WS-J)
           
      * Third row: 1*s0 + 1*s1 + 2*s2 + 3*s3
           MOVE WS-TEMP-BYTE(1,1) TO WS-BYTE-A
           MOVE WS-TEMP-BYTE(1,2) TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,3))
           PERFORM GET-MUL2-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,4))
           PERFORM GET-MUL3-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-STATE-BYTE(3, WS-J)
           
      * Fourth row: 3*s0 + 1*s1 + 1*s2 + 2*s3
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,1))
           PERFORM GET-MUL3-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           MOVE WS-TEMP-BYTE(1,2) TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           MOVE WS-TEMP-BYTE(1,3) TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,4))
           PERFORM GET-MUL2-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-STATE-BYTE(4, WS-J)
           
           EXIT.

      ******************************************************************
      * INV-MIX-COLUMNS
      * Inverse mix columns transformation
      ******************************************************************
       INV-MIX-COLUMNS.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               PERFORM INV-MIX-SINGLE-COLUMN
           END-PERFORM
           EXIT.

      ******************************************************************
      * INV-MIX-SINGLE-COLUMN
      * Inverse mix single column using GF multiplication
      * Matrix: [14 11 13  9]
      *         [ 9 14 11 13]
      *         [13  9 14 11]
      *         [11 13  9 14]
      ******************************************************************
       INV-MIX-SINGLE-COLUMN.
      * Save original column
           MOVE WS-STATE-BYTE(1, WS-J) TO WS-TEMP-BYTE(1, 1)
           MOVE WS-STATE-BYTE(2, WS-J) TO WS-TEMP-BYTE(1, 2)
           MOVE WS-STATE-BYTE(3, WS-J) TO WS-TEMP-BYTE(1, 3)
           MOVE WS-STATE-BYTE(4, WS-J) TO WS-TEMP-BYTE(1, 4)
           
      * First row: 14*s0 + 11*s1 + 13*s2 + 9*s3
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,1))
           PERFORM GET-MUL14-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,2))
           PERFORM GET-MUL11-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,3))
           PERFORM GET-MUL13-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,4))
           PERFORM GET-MUL9-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-STATE-BYTE(1, WS-J)
           
      * Second row: 9*s0 + 14*s1 + 11*s2 + 13*s3
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,1))
           PERFORM GET-MUL9-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,2))
           PERFORM GET-MUL14-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,3))
           PERFORM GET-MUL11-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,4))
           PERFORM GET-MUL13-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-STATE-BYTE(2, WS-J)
           
      * Third row: 13*s0 + 9*s1 + 14*s2 + 11*s3
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,1))
           PERFORM GET-MUL13-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,2))
           PERFORM GET-MUL9-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,3))
           PERFORM GET-MUL14-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,4))
           PERFORM GET-MUL11-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-STATE-BYTE(3, WS-J)
           
      * Fourth row: 11*s0 + 13*s1 + 9*s2 + 14*s3
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,1))
           PERFORM GET-MUL11-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,2))
           PERFORM GET-MUL13-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,3))
           PERFORM GET-MUL9-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           
           COMPUTE WS-INPUT-BYTE = FUNCTION ORD(WS-TEMP-BYTE(1,4))
           PERFORM GET-MUL14-VALUE
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES
           MOVE WS-XOR-BYTE TO WS-STATE-BYTE(4, WS-J)
           
           EXIT.

      ******************************************************************
      * ADD-ROUND-KEY
      * XOR state with round key
      ******************************************************************
       ADD-ROUND-KEY.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
               MOVE WS-AES-STATE-FLAT(WS-I:1) TO WS-BYTE-A
               MOVE WS-ROUND-KEY-ENTRY(WS-ROUND)(WS-I:1) TO WS-BYTE-B
               PERFORM XOR-BYTES
               MOVE WS-XOR-BYTE TO WS-AES-STATE-FLAT(WS-I:1)
           END-PERFORM
           EXIT.

      ******************************************************************
      * HELPER FUNCTIONS - Call IPCRYPT-TABLES functions
      ******************************************************************
       GET-SBOX-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-SBOX-VALUE'
               WS-INPUT-BYTE WS-OUTPUT-BYTE
           EXIT.
           
       GET-INV-SBOX-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-INV-SBOX-VALUE'
               WS-INPUT-BYTE WS-OUTPUT-BYTE
           EXIT.
               
       GET-MUL2-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-MUL2-VALUE'
               WS-INPUT-BYTE WS-OUTPUT-BYTE
           EXIT.
               
       GET-MUL3-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-MUL3-VALUE'
               WS-INPUT-BYTE WS-OUTPUT-BYTE
           EXIT.
               
       GET-MUL9-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-MUL9-VALUE'
               WS-INPUT-BYTE WS-OUTPUT-BYTE
           EXIT.
               
       GET-MUL11-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-MUL11-VALUE'
               WS-INPUT-BYTE WS-OUTPUT-BYTE
           EXIT.
               
       GET-MUL13-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-MUL13-VALUE'
               WS-INPUT-BYTE WS-OUTPUT-BYTE
           EXIT.
               
       GET-MUL14-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-MUL14-VALUE'
               WS-INPUT-BYTE WS-OUTPUT-BYTE
           EXIT.
               
       XOR-BYTES.
           CALL 'IPCRYPT-TABLES' USING 'XOR-BYTES'
               WS-BYTE-A WS-BYTE-B WS-XOR-BYTE
           EXIT.
               
       GET-RCON-VALUE.
           CALL 'IPCRYPT-TABLES' USING 'GET-RCON-VALUE'
               WS-BYTE-INDEX WS-RCON-VAL
           EXIT.

      ******************************************************************
      * SET-MASTER-KEY
      * Set the master key for AES operations
      ******************************************************************
       SET-MASTER-KEY.
      * Key should be passed as parameter
           EXIT.

      ******************************************************************
      * CLEAR-AES-STATE
      * Securely clear AES state and keys
      ******************************************************************
       CLEAR-AES-STATE.
           MOVE ALL X"00" TO WS-AES-STATE
           MOVE ALL X"00" TO WS-ROUND-KEYS
           PERFORM 3 TIMES
               MOVE ALL X"FF" TO WS-AES-STATE
               MOVE ALL X"AA" TO WS-ROUND-KEYS
               MOVE ALL X"00" TO WS-AES-STATE
               MOVE ALL X"00" TO WS-ROUND-KEYS
           END-PERFORM
           EXIT.

      ******************************************************************
      * KIASU-BC-ENCRYPT-INTERNAL
      * KIASU-BC encryption using tweaked AES rounds
      ******************************************************************
       KIASU-BC-ENCRYPT-INTERNAL.
           SET AES-SUCCESS TO TRUE
           
      * Pad 8-byte tweak to 16 bytes
           CALL 'IPCRYPT-UTILS' USING WS-FUNC-PAD-TWEAK
               WS-TWEAK-8 WS-TWEAK-16
               
      * Copy input parameters
           MOVE WS-INPUT-BLOCK TO WS-AES-STATE-FLAT
           MOVE WS-KEY-128 TO WS-MASTER-KEY
           
      * Expand key for 11 round keys
           PERFORM AES-KEY-EXPANSION
           
      * Initial round: AddRoundKey with key XOR tweak
           MOVE 1 TO WS-ROUND
           PERFORM KIASU-BC-ADD-ROUND-KEY
           
      * Main rounds (1-9)
           PERFORM VARYING WS-ROUND FROM 1 BY 1 UNTIL WS-ROUND > 9
               PERFORM SUB-BYTES
               PERFORM SHIFT-ROWS
               PERFORM MIX-COLUMNS
               ADD 1 TO WS-ROUND
               PERFORM KIASU-BC-ADD-ROUND-KEY
               SUBTRACT 1 FROM WS-ROUND
           END-PERFORM
           
      * Final round (10)
           PERFORM SUB-BYTES
           PERFORM SHIFT-ROWS
           MOVE 11 TO WS-ROUND
           PERFORM KIASU-BC-ADD-ROUND-KEY
           
      * Copy result
           MOVE WS-AES-STATE-FLAT TO WS-OUTPUT-BLOCK
           EXIT.

      ******************************************************************
      * KIASU-BC-ADD-ROUND-KEY
      * Add round key with tweak XOR for KIASU-BC
      * Per spec: state = state XOR (round_key XOR padded_tweak)
      ******************************************************************
       KIASU-BC-ADD-ROUND-KEY.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
      * First XOR round key with padded tweak
               MOVE WS-ROUND-KEY-ENTRY(WS-ROUND)(WS-I:1) TO WS-BYTE-A
               MOVE WS-TWEAK-16(WS-I:1) TO WS-BYTE-B
               PERFORM XOR-BYTES
               MOVE WS-XOR-BYTE TO WS-TEMP-BYTE-VAL
      * Then XOR result with state
               MOVE WS-AES-STATE-FLAT(WS-I:1) TO WS-BYTE-A
               MOVE WS-TEMP-BYTE-VAL TO WS-BYTE-B
               PERFORM XOR-BYTES
               MOVE WS-XOR-BYTE TO WS-AES-STATE-FLAT(WS-I:1)
           END-PERFORM
           EXIT.

      ******************************************************************
      * KIASU-BC-DECRYPT-INTERNAL
      * KIASU-BC decryption using tweaked AES rounds
      ******************************************************************
       KIASU-BC-DECRYPT-INTERNAL.
           SET AES-SUCCESS TO TRUE
           
      * Pad 8-byte tweak to 16 bytes
           CALL 'IPCRYPT-UTILS' USING WS-FUNC-PAD-TWEAK
               WS-TWEAK-8 WS-TWEAK-16
               
      * Copy input parameters
           MOVE WS-INPUT-BLOCK TO WS-AES-STATE-FLAT
           MOVE WS-KEY-128 TO WS-MASTER-KEY
           
      * Expand key for 11 round keys
           PERFORM AES-KEY-EXPANSION
           
      * Initial round key addition (reverse of final round)
           MOVE 11 TO WS-ROUND
           PERFORM KIASU-BC-ADD-ROUND-KEY
           PERFORM INV-SHIFT-ROWS
           PERFORM INV-SUB-BYTES
           
      * Inverse main rounds (9-1)
           PERFORM VARYING WS-ROUND FROM 10 BY -1 UNTIL WS-ROUND < 2
               PERFORM KIASU-BC-ADD-ROUND-KEY
               PERFORM INV-MIX-COLUMNS
               PERFORM INV-SHIFT-ROWS
               PERFORM INV-SUB-BYTES
           END-PERFORM
           
      * Final round (0)
           MOVE 1 TO WS-ROUND
           PERFORM KIASU-BC-ADD-ROUND-KEY
           
      * Copy result
           MOVE WS-AES-STATE-FLAT TO WS-OUTPUT-BLOCK
           EXIT.

      ******************************************************************
      * AES-XTS-ENCRYPT-INTERNAL
      * AES-XTS mode encryption
      * Per spec: Uses two 16-byte keys from 32-byte input
      ******************************************************************
       AES-XTS-ENCRYPT-INTERNAL.
           SET AES-SUCCESS TO TRUE
           
      * Split 32-byte key: K1 for data, K2 for tweak
      * K1 = key[0:16], K2 = key[16:32]
           
      * Encrypt the tweak with K2 (second half of key)
           MOVE WS-TWEAK-16 TO WS-AES-STATE-FLAT
           MOVE WS-KEY-256(17:16) TO WS-MASTER-KEY
           PERFORM AES-ENCRYPT-BLOCK-INTERNAL
           MOVE WS-AES-STATE-FLAT TO WS-ENCRYPTED-TWEAK
           
      * XOR input block with encrypted tweak
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
               MOVE WS-INPUT-BLOCK(WS-I:1) TO WS-BYTE-A
               MOVE WS-ENCRYPTED-TWEAK(WS-I:1) TO WS-BYTE-B
               PERFORM XOR-BYTES
               MOVE WS-XOR-BYTE TO WS-AES-STATE-FLAT(WS-I:1)
           END-PERFORM
           
      * Encrypt the XOR-ed block with K1 (first half of key)
           MOVE WS-KEY-256(1:16) TO WS-MASTER-KEY
           PERFORM AES-ENCRYPT-BLOCK-INTERNAL
           
      * XOR result back with encrypted tweak
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
               MOVE WS-AES-STATE-FLAT(WS-I:1) TO WS-BYTE-A
               MOVE WS-ENCRYPTED-TWEAK(WS-I:1) TO WS-BYTE-B
               PERFORM XOR-BYTES
               MOVE WS-XOR-BYTE TO WS-OUTPUT-BLOCK(WS-I:1)
           END-PERFORM
           
           EXIT.

      ******************************************************************
      * AES-XTS-DECRYPT-INTERNAL
      * AES-XTS mode decryption
      * Per spec: Uses two 16-byte keys from 32-byte input
      ******************************************************************
       AES-XTS-DECRYPT-INTERNAL.
           SET AES-SUCCESS TO TRUE
           
      * Split 32-byte key: K1 for data, K2 for tweak
      * K1 = key[0:16], K2 = key[16:32]
           
      * Encrypt the tweak with K2 (second half of key)
           MOVE WS-TWEAK-16 TO WS-AES-STATE-FLAT
           MOVE WS-KEY-256(17:16) TO WS-MASTER-KEY
           PERFORM AES-ENCRYPT-BLOCK-INTERNAL
           MOVE WS-AES-STATE-FLAT TO WS-ENCRYPTED-TWEAK
           
      * XOR input block with encrypted tweak
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
               MOVE WS-INPUT-BLOCK(WS-I:1) TO WS-BYTE-A
               MOVE WS-ENCRYPTED-TWEAK(WS-I:1) TO WS-BYTE-B
               PERFORM XOR-BYTES
               MOVE WS-XOR-BYTE TO WS-AES-STATE-FLAT(WS-I:1)
           END-PERFORM
           
      * Decrypt the XOR-ed block with K1 (first half of key)
           MOVE WS-KEY-256(1:16) TO WS-MASTER-KEY
           PERFORM AES-DECRYPT-BLOCK-INTERNAL
           
      * XOR result back with encrypted tweak
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 16
               MOVE WS-AES-STATE-FLAT(WS-I:1) TO WS-BYTE-A
               MOVE WS-ENCRYPTED-TWEAK(WS-I:1) TO WS-BYTE-B
               PERFORM XOR-BYTES
               MOVE WS-XOR-BYTE TO WS-OUTPUT-BLOCK(WS-I:1)
           END-PERFORM
           
           EXIT.

       END PROGRAM IPCRYPT-AES.

