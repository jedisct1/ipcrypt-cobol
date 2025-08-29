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

       01  WS-UTILITY-STATUS    PIC X(01).
           88  UTIL-SUCCESS     VALUE 'Y'.
           88  UTIL-ERROR       VALUE 'N'.

       01  WS-ERROR-MESSAGE     PIC X(50).

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
           CALL 'IPCRYPT-UTILS' USING 'IP-TO-BYTES'
               LS-INPUT-IP WS-INPUT-BLOCK SPACES WS-UTILITY-STATUS
           IF NOT UTIL-SUCCESS
               SET ERROR-INVALID-IP TO TRUE
               MOVE "Invalid IP address format" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
      * Perform AES-128 encryption or decryption
           IF LS-ENCRYPT
               CALL 'IPCRYPT-AES' USING 'AES-ENCRYPT-BLOCK'
                   WS-INPUT-BLOCK WS-KEY-128
               MOVE WS-KEY-128 TO WS-OUTPUT-BLOCK
           ELSE
               CALL 'IPCRYPT-AES' USING 'AES-DECRYPT-BLOCK'
                   WS-INPUT-BLOCK WS-KEY-128
               MOVE WS-KEY-128 TO WS-OUTPUT-BLOCK
           END-IF
           
      * Convert result back to IP address string
           CALL 'IPCRYPT-UTILS' USING 'BYTES-TO-IP'
               WS-OUTPUT-BLOCK WS-OUTPUT-STRING SPACES WS-UTILITY-STATUS
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
           CALL 'IPCRYPT-UTILS' USING 'IP-TO-BYTES'
               LS-INPUT-IP WS-INPUT-BLOCK SPACES WS-UTILITY-STATUS
           IF NOT UTIL-SUCCESS
               SET ERROR-INVALID-IP TO TRUE
               MOVE "Invalid IP address format" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
           IF LS-ENCRYPT
      * For encryption, use provided tweak or generate random one
               IF WS-TWEAK-LENGTH = 0
      * Generate random 8-byte tweak (simplified - use zeros for now)
                   MOVE ALL X"00" TO WS-TWEAK(1:8)
                   MOVE 8 TO WS-TWEAK-LENGTH
               END-IF
               
      * Perform KIASU-BC encryption
               CALL 'IPCRYPT-AES' USING 'KIASU-BC-ENCRYPT'
                   WS-INPUT-BLOCK WS-KEY-128 WS-TWEAK(1:8)
               MOVE WS-KEY-128 TO WS-OUTPUT-BLOCK
               
      * Convert result back to IP address string
               CALL 'IPCRYPT-UTILS' USING 'BYTES-TO-IP'
                   WS-OUTPUT-BLOCK WS-OUTPUT-STRING SPACES
                   WS-UTILITY-STATUS
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
               CALL 'IPCRYPT-AES' USING 'KIASU-BC-DECRYPT'
                   WS-INPUT-BLOCK WS-KEY-128 WS-TWEAK(1:8)
               MOVE WS-KEY-128 TO WS-OUTPUT-BLOCK
               
      * Convert result back to IP address string
               CALL 'IPCRYPT-UTILS' USING 'BYTES-TO-IP'
                   WS-OUTPUT-BLOCK WS-OUTPUT-STRING SPACES
                   WS-UTILITY-STATUS
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
           CALL 'IPCRYPT-UTILS' USING 'IP-TO-BYTES'
               LS-INPUT-IP WS-INPUT-BLOCK SPACES WS-UTILITY-STATUS
           IF NOT UTIL-SUCCESS
               SET ERROR-INVALID-IP TO TRUE
               MOVE "Invalid IP address format" TO WS-ERROR-MESSAGE
               EXIT
           END-IF
           
           IF LS-ENCRYPT
      * For encryption, use provided tweak or generate random one
               IF WS-TWEAK-LENGTH = 0
      * Generate random 16-byte tweak (simplified - use zeros for now)
                   MOVE ALL X"00" TO WS-TWEAK(1:16)
                   MOVE 16 TO WS-TWEAK-LENGTH
               END-IF
               
      * Perform AES-XTS encryption
               CALL 'IPCRYPT-AES' USING 'AES-XTS-ENCRYPT'
                   WS-INPUT-BLOCK WS-KEY-256 WS-TWEAK(1:16)
               MOVE WS-KEY-256(1:16) TO WS-OUTPUT-BLOCK
               
      * Convert result back to IP address string
               CALL 'IPCRYPT-UTILS' USING 'BYTES-TO-IP'
                   WS-OUTPUT-BLOCK WS-OUTPUT-STRING SPACES
                   WS-UTILITY-STATUS
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
               CALL 'IPCRYPT-AES' USING 'AES-XTS-DECRYPT'
                   WS-INPUT-BLOCK WS-KEY-256 WS-TWEAK(1:16)
               MOVE WS-KEY-256(1:16) TO WS-OUTPUT-BLOCK
               
      * Convert result back to IP address string
               CALL 'IPCRYPT-UTILS' USING 'BYTES-TO-IP'
                   WS-OUTPUT-BLOCK WS-OUTPUT-STRING SPACES
                   WS-UTILITY-STATUS
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
               MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-OUTPUT-STRING))
                    TO LS-OUTPUT-LENGTH
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

       END PROGRAM IPCRYPT-LIB.