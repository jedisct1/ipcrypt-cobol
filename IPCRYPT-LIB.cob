      ******************************************************************
      * IPCRYPT-LIB - Main IPCrypt Library Interface
      * Part of IPCrypt COBOL Implementation
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IPCRYPT-LIB.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNU-LINUX.
       OBJECT-COMPUTER. GNU-LINUX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      ******************************************************************
      * CRYPTO MATERIALS
      ******************************************************************
       01  WS-CRYPTO-MATERIALS.
           05  WS-KEY-128       PIC X(16).
           05  WS-KEY-256       PIC X(32).
           05  WS-TWEAK         PIC X(16).
           05  WS-INPUT-BLOCK   PIC X(16).
           05  WS-OUTPUT-BLOCK  PIC X(16).

       01  WS-WORK-VARS.
           05  WS-I             PIC 9(03) COMP.
           05  WS-J             PIC 9(03) COMP.
           05  WS-KEY-LENGTH    PIC 9(02) COMP.
           05  WS-TWEAK-LENGTH  PIC 9(02) COMP.
           05  WS-OUTPUT-STRING PIC X(39).
           05  WS-WORK-I        PIC 9(03) COMP.
           05  WS-RANDOM-BYTE   PIC 9(03) COMP.

       01  WS-UTILITY-STATUS    PIC X(01).
           88  UTIL-SUCCESS     VALUE 'Y'.
           88  UTIL-ERROR       VALUE 'N'.

       01  WS-ERROR-MESSAGE     PIC X(50).

       01  WS-FUNCTION-NAMES.
           05  WS-FUNC-IP-TO-BYTES  PIC X(30) VALUE
               "IP-TO-BYTES                   ".
           05  WS-FUNC-BYTES-TO-IP  PIC X(30) VALUE
               "BYTES-TO-IP                   ".

      * Call parameter wrappers (must be 01 level for CALL)
       01  WS-CALL-FUNCTION     PIC X(30).
       01  WS-CALL-PARAM-1      PIC X(64).
       01  WS-CALL-PARAM-2      PIC X(64).
       01  WS-CALL-PARAM-3      PIC X(64).
       01  WS-CALL-PARAM-4      PIC X(64).
       01  WS-CALL-SPACES       PIC X(64) VALUE SPACES.

       LINKAGE SECTION.
       01  LS-IPCRYPT-REQUEST.
           05  LS-OPERATION     PIC X(01).
               88  LS-ENCRYPT   VALUE 'E'.
               88  LS-DECRYPT   VALUE 'D'.
           05  LS-MODE          PIC X(16).
           05  LS-INPUT-IP      PIC X(39).
           05  LS-KEY           PIC X(32).
           05  LS-KEY-LENGTH    PIC 9(02) COMP.
           05  LS-TWEAK         PIC X(16).
           05  LS-TWEAK-LENGTH  PIC 9(02) COMP.
           05  LS-OUTPUT        PIC X(39).
           05  LS-OUTPUT-LENGTH PIC 9(02) COMP.
           05  LS-STATUS-CODE   PIC 9(02) COMP.
               88  IPCRYPT-SUCCESS VALUE 00.
               88  ERROR-INVALID-MODE VALUE 01.
               88  ERROR-INVALID-IP VALUE 02.
               88  ERROR-INVALID-KEY VALUE 03.

       PROCEDURE DIVISION USING LS-IPCRYPT-REQUEST.

      ******************************************************************
      * MAIN-IPCRYPT-ENTRY
      * Main entry point for all IPCrypt operations
      ******************************************************************
       MAIN-IPCRYPT-ENTRY.
           PERFORM INITIALIZE-LIBRARY
           PERFORM VALIDATE-INPUT-PARAMETERS
           IF NOT IPCRYPT-SUCCESS
               PERFORM SECURE-CLEANUP
               GOBACK
           END-IF

           PERFORM COPY-INPUT-PARAMETERS
           
           EVALUATE LS-MODE
               WHEN "DETERMINISTIC"
                   PERFORM HANDLE-DETERMINISTIC-MODE
               WHEN "ND"
                   PERFORM HANDLE-ND-MODE
               WHEN "NDX"  
                   PERFORM HANDLE-NDX-MODE
               WHEN OTHER
                   SET ERROR-INVALID-MODE TO TRUE
                   MOVE "Unsupported mode" TO WS-ERROR-MESSAGE
           END-EVALUATE

           PERFORM COPY-OUTPUT-RESULTS
           PERFORM SECURE-CLEANUP
           GOBACK.

      ******************************************************************
      * INITIALIZE-LIBRARY
      * Initialize library components
      ******************************************************************
       INITIALIZE-LIBRARY.
           SET IPCRYPT-SUCCESS TO TRUE
           MOVE SPACES TO WS-ERROR-MESSAGE
           MOVE ALL X"00" TO WS-CRYPTO-MATERIALS
           EXIT.

      ******************************************************************
      * VALIDATE-INPUT-PARAMETERS
      * Validate all input parameters
      ******************************************************************
       VALIDATE-INPUT-PARAMETERS.
           SET IPCRYPT-SUCCESS TO TRUE

           IF NOT LS-ENCRYPT AND NOT LS-DECRYPT
               SET ERROR-INVALID-MODE TO TRUE
               MOVE "Invalid operation" TO WS-ERROR-MESSAGE
               EXIT
           END-IF

           EVALUATE LS-MODE
               WHEN "DETERMINISTIC"
               WHEN "ND"
               WHEN "NDX"
                   CONTINUE
               WHEN OTHER
                   SET ERROR-INVALID-MODE TO TRUE
                   MOVE "Invalid mode" TO WS-ERROR-MESSAGE
                   EXIT
           END-EVALUATE

           IF LS-INPUT-IP = SPACES
               SET ERROR-INVALID-IP TO TRUE
               MOVE "Invalid IP address" TO WS-ERROR-MESSAGE
               EXIT
           END-IF

           IF LS-KEY-LENGTH < 16 OR LS-KEY-LENGTH > 32
               SET ERROR-INVALID-KEY TO TRUE
               MOVE "Invalid key length" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           EXIT.

      ******************************************************************
      * COPY-INPUT-PARAMETERS
      * Copy input parameters to working storage
      ******************************************************************
       COPY-INPUT-PARAMETERS.
           MOVE LS-KEY-LENGTH TO WS-KEY-LENGTH
           MOVE LS-TWEAK-LENGTH TO WS-TWEAK-LENGTH

           IF WS-KEY-LENGTH = 16
               MOVE LS-KEY(1:16) TO WS-KEY-128
           ELSE
               MOVE LS-KEY(1:32) TO WS-KEY-256
               MOVE LS-KEY(1:16) TO WS-KEY-128
           END-IF

           IF WS-TWEAK-LENGTH > 0
               MOVE LS-TWEAK(1:WS-TWEAK-LENGTH) TO WS-TWEAK
           END-IF
           EXIT.

      ******************************************************************
      * HANDLE-DETERMINISTIC-MODE
      * Process deterministic encryption/decryption (AES-128 ECB)
      ******************************************************************
       HANDLE-DETERMINISTIC-MODE.
           SET IPCRYPT-SUCCESS TO TRUE
           
      * Convert IP address to 16-byte block
           MOVE WS-FUNC-IP-TO-BYTES TO WS-CALL-FUNCTION
           MOVE LS-INPUT-IP TO WS-CALL-PARAM-1
           MOVE SPACES TO WS-CALL-PARAM-2
           MOVE SPACES TO WS-CALL-PARAM-3
           CALL 'IPCRYPT-UTILS' USING WS-CALL-FUNCTION
               WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
               WS-UTILITY-STATUS
           END-CALL
           MOVE WS-CALL-PARAM-2(1:16) TO WS-INPUT-BLOCK
           IF NOT UTIL-SUCCESS
               SET ERROR-INVALID-IP TO TRUE
               MOVE "Invalid IP address format" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
      * Perform AES-128 encryption or decryption
           IF LS-ENCRYPT
      * Use AES-ENCRYPT-BLOCK for deterministic mode
               MOVE 'AES-ENCRYPT-BLOCK' TO WS-CALL-FUNCTION
               MOVE WS-INPUT-BLOCK TO WS-CALL-PARAM-1(1:16)
               MOVE WS-KEY-128 TO WS-CALL-PARAM-2(1:16)
               MOVE SPACES TO WS-CALL-PARAM-3
               MOVE SPACES TO WS-CALL-PARAM-4
               CALL 'IPCRYPT-AES' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3
                   WS-CALL-PARAM-4
               END-CALL
               MOVE WS-CALL-PARAM-1(1:16) TO WS-OUTPUT-BLOCK
           ELSE
      * Use AES-DECRYPT-BLOCK for deterministic mode
               MOVE 'AES-DECRYPT-BLOCK' TO WS-CALL-FUNCTION
               MOVE WS-INPUT-BLOCK TO WS-CALL-PARAM-1(1:16)
               MOVE WS-KEY-128 TO WS-CALL-PARAM-2(1:16)
               MOVE SPACES TO WS-CALL-PARAM-3
               MOVE SPACES TO WS-CALL-PARAM-4
               CALL 'IPCRYPT-AES' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3
                   WS-CALL-PARAM-4
               END-CALL
               MOVE WS-CALL-PARAM-1(1:16) TO WS-OUTPUT-BLOCK
           END-IF
           
           
      * Convert result back to IP address string
           MOVE WS-FUNC-BYTES-TO-IP TO WS-CALL-FUNCTION
           MOVE WS-OUTPUT-BLOCK TO WS-CALL-PARAM-1
           MOVE SPACES TO WS-CALL-PARAM-2
           MOVE SPACES TO WS-CALL-PARAM-3
           CALL 'IPCRYPT-UTILS' USING WS-CALL-FUNCTION
               WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
               WS-UTILITY-STATUS
           END-CALL
           MOVE WS-CALL-PARAM-2(1:39) TO WS-OUTPUT-STRING
           IF NOT UTIL-SUCCESS
               SET ERROR-INVALID-IP TO TRUE
               MOVE "Failed to convert result to IP"
                   TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
           SET IPCRYPT-SUCCESS TO TRUE
           EXIT.

      ******************************************************************
      * HANDLE-ND-MODE
      * Process ND mode encryption/decryption using KIASU-BC
      ******************************************************************
       HANDLE-ND-MODE.
           SET IPCRYPT-SUCCESS TO TRUE
           
      * Convert IP address to 16-byte block
           MOVE WS-FUNC-IP-TO-BYTES TO WS-CALL-FUNCTION
           MOVE LS-INPUT-IP TO WS-CALL-PARAM-1
           MOVE SPACES TO WS-CALL-PARAM-2
           MOVE SPACES TO WS-CALL-PARAM-3
           CALL 'IPCRYPT-UTILS' USING WS-CALL-FUNCTION
               WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
               WS-UTILITY-STATUS
           END-CALL
           MOVE WS-CALL-PARAM-2(1:16) TO WS-INPUT-BLOCK
           IF NOT UTIL-SUCCESS
               SET ERROR-INVALID-IP TO TRUE
               MOVE "Invalid IP address format" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
           IF LS-ENCRYPT
      * For encryption, use provided tweak or generate random one
               IF WS-TWEAK-LENGTH = 0
      * Generate random 8-byte tweak using CBL_GC_NANOSLEEP for seed
      * and a simple PRNG
                   PERFORM GENERATE-RANDOM-TWEAK-8
                   MOVE 8 TO WS-TWEAK-LENGTH
               END-IF
               
      * Perform KIASU-BC encryption
               MOVE 'KIASU-BC-ENCRYPT' TO WS-CALL-FUNCTION
               MOVE WS-INPUT-BLOCK TO WS-CALL-PARAM-1
               MOVE WS-KEY-128 TO WS-CALL-PARAM-2
               MOVE WS-TWEAK TO WS-CALL-PARAM-3
               CALL 'IPCRYPT-AES' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
                   WS-CALL-PARAM-4
               END-CALL
               MOVE WS-CALL-PARAM-2(1:16) TO WS-OUTPUT-BLOCK
               
      * Convert result back to IP address string
               MOVE WS-FUNC-BYTES-TO-IP TO WS-CALL-FUNCTION
               MOVE WS-OUTPUT-BLOCK TO WS-CALL-PARAM-1
               MOVE SPACES TO WS-CALL-PARAM-2
               MOVE SPACES TO WS-CALL-PARAM-3
               CALL 'IPCRYPT-UTILS' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
                   WS-UTILITY-STATUS
               END-CALL
               MOVE WS-CALL-PARAM-2(1:39) TO WS-OUTPUT-STRING
               IF NOT UTIL-SUCCESS
                   SET ERROR-INVALID-IP TO TRUE
                   MOVE "Failed to convert result to IP"
                       TO WS-ERROR-MESSAGE
                   EXIT
               END-IF
           ELSE
      * For decryption, extract tweak from input
               IF WS-TWEAK-LENGTH < 8
                   SET ERROR-INVALID-KEY TO TRUE
                   MOVE "Invalid tweak for decryption" 
                       TO WS-ERROR-MESSAGE
                   EXIT
               END-IF
               
      * Perform KIASU-BC decryption
               MOVE 'KIASU-BC-DECRYPT' TO WS-CALL-FUNCTION
               MOVE WS-INPUT-BLOCK TO WS-CALL-PARAM-1
               MOVE WS-KEY-128 TO WS-CALL-PARAM-2
               MOVE WS-TWEAK TO WS-CALL-PARAM-3
               CALL 'IPCRYPT-AES' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
                   WS-CALL-PARAM-4
               END-CALL
               MOVE WS-CALL-PARAM-2(1:16) TO WS-OUTPUT-BLOCK
               
      * Convert result back to IP address string
               MOVE WS-FUNC-BYTES-TO-IP TO WS-CALL-FUNCTION
               MOVE WS-OUTPUT-BLOCK TO WS-CALL-PARAM-1
               MOVE SPACES TO WS-CALL-PARAM-2
               MOVE SPACES TO WS-CALL-PARAM-3
               CALL 'IPCRYPT-UTILS' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
                   WS-UTILITY-STATUS
               END-CALL
               MOVE WS-CALL-PARAM-2(1:39) TO WS-OUTPUT-STRING
               IF NOT UTIL-SUCCESS
                   SET ERROR-INVALID-IP TO TRUE
                   MOVE "Failed to convert result to IP"
                       TO WS-ERROR-MESSAGE
                   EXIT
               END-IF
           END-IF
           
           SET IPCRYPT-SUCCESS TO TRUE
           EXIT.

      ******************************************************************
      * HANDLE-NDX-MODE
      * Process NDX mode encryption/decryption using AES-XTS
      ******************************************************************
       HANDLE-NDX-MODE.
           SET IPCRYPT-SUCCESS TO TRUE
           
      * Validate 32-byte key requirement for AES-XTS
           IF WS-KEY-LENGTH NOT = 32
               SET ERROR-INVALID-KEY TO TRUE
               MOVE "NDX mode requires 32-byte key" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
      * Convert IP address to 16-byte block
           MOVE WS-FUNC-IP-TO-BYTES TO WS-CALL-FUNCTION
           MOVE LS-INPUT-IP TO WS-CALL-PARAM-1
           MOVE SPACES TO WS-CALL-PARAM-2
           MOVE SPACES TO WS-CALL-PARAM-3
           CALL 'IPCRYPT-UTILS' USING WS-CALL-FUNCTION
               WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
               WS-UTILITY-STATUS
           END-CALL
           MOVE WS-CALL-PARAM-2(1:16) TO WS-INPUT-BLOCK
           IF NOT UTIL-SUCCESS
               SET ERROR-INVALID-IP TO TRUE
               MOVE "Invalid IP address format" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
           IF LS-ENCRYPT
      * For encryption, use provided tweak or generate random one
               IF WS-TWEAK-LENGTH = 0
      * Generate random 16-byte tweak using CBL_GC_NANOSLEEP for seed
      * and a simple PRNG
                   PERFORM GENERATE-RANDOM-TWEAK-16
                   MOVE 16 TO WS-TWEAK-LENGTH
               END-IF
               
      * Perform AES-XTS encryption
               MOVE 'AES-XTS-ENCRYPT' TO WS-CALL-FUNCTION
               MOVE WS-INPUT-BLOCK TO WS-CALL-PARAM-1
               MOVE WS-KEY-256 TO WS-CALL-PARAM-2
               MOVE WS-TWEAK TO WS-CALL-PARAM-3
               CALL 'IPCRYPT-AES' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
                   WS-CALL-PARAM-4
               END-CALL
               MOVE WS-CALL-PARAM-2(1:16) TO WS-OUTPUT-BLOCK
               
      * Convert result back to IP address string
               MOVE WS-FUNC-BYTES-TO-IP TO WS-CALL-FUNCTION
               MOVE WS-OUTPUT-BLOCK TO WS-CALL-PARAM-1
               MOVE SPACES TO WS-CALL-PARAM-2
               MOVE SPACES TO WS-CALL-PARAM-3
               CALL 'IPCRYPT-UTILS' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
                   WS-UTILITY-STATUS
               END-CALL
               MOVE WS-CALL-PARAM-2(1:39) TO WS-OUTPUT-STRING
               IF NOT UTIL-SUCCESS
                   SET ERROR-INVALID-IP TO TRUE
                   MOVE "Failed to convert result to IP"
                       TO WS-ERROR-MESSAGE
                   EXIT
               END-IF
           ELSE
      * For decryption, extract tweak from input
               IF WS-TWEAK-LENGTH < 16
                   SET ERROR-INVALID-KEY TO TRUE
                   MOVE "Invalid tweak for decryption" 
                       TO WS-ERROR-MESSAGE
                   EXIT
               END-IF
               
      * Perform AES-XTS decryption
               MOVE 'AES-XTS-DECRYPT' TO WS-CALL-FUNCTION
               MOVE WS-INPUT-BLOCK TO WS-CALL-PARAM-1
               MOVE WS-KEY-256 TO WS-CALL-PARAM-2
               MOVE WS-TWEAK TO WS-CALL-PARAM-3
               CALL 'IPCRYPT-AES' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
                   WS-CALL-PARAM-4
               END-CALL
               MOVE WS-CALL-PARAM-2(1:16) TO WS-OUTPUT-BLOCK
               
      * Convert result back to IP address string
               MOVE WS-FUNC-BYTES-TO-IP TO WS-CALL-FUNCTION
               MOVE WS-OUTPUT-BLOCK TO WS-CALL-PARAM-1
               MOVE SPACES TO WS-CALL-PARAM-2
               MOVE SPACES TO WS-CALL-PARAM-3
               CALL 'IPCRYPT-UTILS' USING WS-CALL-FUNCTION
                   WS-CALL-PARAM-1 WS-CALL-PARAM-2 WS-CALL-PARAM-3 
                   WS-UTILITY-STATUS
               END-CALL
               MOVE WS-CALL-PARAM-2(1:39) TO WS-OUTPUT-STRING
               IF NOT UTIL-SUCCESS
                   SET ERROR-INVALID-IP TO TRUE
                   MOVE "Failed to convert result to IP"
                       TO WS-ERROR-MESSAGE
                   EXIT
               END-IF
           END-IF
           
           SET IPCRYPT-SUCCESS TO TRUE
           EXIT.

      ******************************************************************
      * COPY-OUTPUT-RESULTS
      * Copy results back to linkage section
      ******************************************************************
       COPY-OUTPUT-RESULTS.
           IF IPCRYPT-SUCCESS
               MOVE WS-OUTPUT-STRING TO LS-OUTPUT
               MOVE 39 TO LS-OUTPUT-LENGTH
           ELSE
               MOVE SPACES TO LS-OUTPUT
               MOVE ZERO TO LS-OUTPUT-LENGTH
           END-IF
           EXIT.

      ******************************************************************
      * SECURE-CLEANUP
      * Securely clear sensitive data
      ******************************************************************
       SECURE-CLEANUP.
           MOVE ALL X"00" TO WS-CRYPTO-MATERIALS
           PERFORM 3 TIMES
               MOVE ALL X"FF" TO WS-CRYPTO-MATERIALS
               MOVE ALL X"AA" TO WS-CRYPTO-MATERIALS  
               MOVE ALL X"00" TO WS-CRYPTO-MATERIALS
           END-PERFORM
           EXIT.

      ******************************************************************
      * GENERATE-RANDOM-TWEAK-8
      * Generate 8 random bytes for ND mode
      ******************************************************************
       GENERATE-RANDOM-TWEAK-8.
      * Use FUNCTION RANDOM for each byte
           PERFORM VARYING WS-WORK-I FROM 1 BY 1 UNTIL WS-WORK-I > 8
               COMPUTE WS-RANDOM-BYTE = FUNCTION RANDOM * 256
               END-COMPUTE
               MOVE FUNCTION CHAR(WS-RANDOM-BYTE + 1) 
                   TO WS-TWEAK(WS-WORK-I:1)
           END-PERFORM
           EXIT.

      ******************************************************************
      * GENERATE-RANDOM-TWEAK-16  
      * Generate 16 random bytes for NDX mode
      ******************************************************************
       GENERATE-RANDOM-TWEAK-16.
      * Use FUNCTION RANDOM for each byte
           PERFORM VARYING WS-WORK-I FROM 1 BY 1 UNTIL WS-WORK-I > 16
               COMPUTE WS-RANDOM-BYTE = FUNCTION RANDOM * 256
               END-COMPUTE
               MOVE FUNCTION CHAR(WS-RANDOM-BYTE + 1) 
                   TO WS-TWEAK(WS-WORK-I:1)
           END-PERFORM
           EXIT.

       END PROGRAM IPCRYPT-LIB.
