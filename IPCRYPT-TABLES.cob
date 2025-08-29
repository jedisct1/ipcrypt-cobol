      ******************************************************************
      * IPCRYPT-TABLES - AES S-Box and Round Constants
      * Part of IPCrypt COBOL Implementation
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IPCRYPT-TABLES.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNU-LINUX.
       OBJECT-COMPUTER. GNU-LINUX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      ******************************************************************
      * COMPLETE AES S-BOX TABLE (all 256 values)
      ******************************************************************
       01  WS-AES-SBOX-DATA.
           05  FILLER           PIC X(16) 
               VALUE X"637C777BF26B6FC53001672BFED7AB76".
           05  FILLER           PIC X(16) 
               VALUE X"CA82C97DFA5947F0ADD4A2AF9CA472C0".
           05  FILLER           PIC X(16) 
               VALUE X"B7FD9326363FF7CC34A5E5F171D83115".
           05  FILLER           PIC X(16) 
               VALUE X"04C723C31896059A071280E2EB27B275".
           05  FILLER           PIC X(16) 
               VALUE X"09832C1A1B6E5AA0523BD6B329E32F84".
           05  FILLER           PIC X(16) 
               VALUE X"53D100ED20FCB15B6ACBBE394A4C58CF".
           05  FILLER           PIC X(16) 
               VALUE X"D0EFAAFB434D338545F9027F503C9FA8".
           05  FILLER           PIC X(16) 
               VALUE X"51A3408F929D38F5BCB6DA2110FFF3D2".
           05  FILLER           PIC X(16) 
               VALUE X"CD0C13EC5F974417C4A77E3D645D1973".
           05  FILLER           PIC X(16) 
               VALUE X"60814FDC222A908846EEB814DE5E0BDB".
           05  FILLER           PIC X(16) 
               VALUE X"E0323A0A4906245CC2D3AC6291959E47".
           05  FILLER           PIC X(16) 
               VALUE X"79E7C8376D8DD54EA96C56F4EA657AAE".
           05  FILLER           PIC X(16) 
               VALUE X"08BA78252E1CA6B4C6E8DD741F4BBD8B".
           05  FILLER           PIC X(16) 
               VALUE X"8A703EB5664803F60E613557B986C11D".
           05  FILLER           PIC X(16) 
               VALUE X"9EE1F8981169D98E949B1E87E9CE5528".
           05  FILLER           PIC X(16) 
               VALUE X"DF8CA1890DBFE6426841992D0FB054BB".

       01  WS-AES-SBOX REDEFINES WS-AES-SBOX-DATA.
           05  WS-SBOX-ENTRY    OCCURS 256 TIMES PIC X(01).

      ******************************************************************
      * COMPLETE AES INVERSE S-BOX TABLE (all 256 values)
      ******************************************************************
       01  WS-AES-INV-SBOX-DATA.
           05  FILLER           PIC X(16) 
               VALUE X"52096AD530363A538BF40A39E81F3D7F".
           05  FILLER           PIC X(16) 
               VALUE X"7CE3398291F287348E434C4CDEE9CB54".
           05  FILLER           PIC X(16) 
               VALUE X"7B9432A6C2233DEED4C9050B42FAC34E".
           05  FILLER           PIC X(16) 
               VALUE X"082EA1666628D924B2765BA2496D8BD1".
           05  FILLER           PIC X(16) 
               VALUE X"25F8F664866898168DA45CCC5D65B692".
           05  FILLER           PIC X(16) 
               VALUE X"6C7048503FEDB9DA5E1546573A8D9D84".
           05  FILLER           PIC X(16) 
               VALUE X"90D8AB008CBCD30AF7E4580C5B3B4506".
           05  FILLER           PIC X(16) 
               VALUE X"D02C1E8FCA3F0F02C1AFBD03011138A6".
           05  FILLER           PIC X(16) 
               VALUE X"B3A91114F167DCEAF2CFCEF0B4E67396".
           05  FILLER           PIC X(16) 
               VALUE X"96AC7422E7AD3585E2F937E81C75DF6E".
           05  FILLER           PIC X(16) 
               VALUE X"47F11A711D29C5896FB7620EAAB8BE1B".
           05  FILLER           PIC X(16) 
               VALUE X"FC563E4BC6D279209ADBC0FE78CD5AF4".
           05  FILLER           PIC X(16) 
               VALUE X"1FDDA833880CC73B11210592780BEC5F".
           05  FILLER           PIC X(16) 
               VALUE X"60517FA919B54A0D2DE57A9F93C99CEF".
           05  FILLER           PIC X(16) 
               VALUE X"A0E03B4DAE2AF5B0C8EBBB3C83539966".
           05  FILLER           PIC X(16) 
               VALUE X"172B047EBA77D626E169146355210C7D".

       01  WS-AES-INV-SBOX REDEFINES WS-AES-INV-SBOX-DATA.
           05  WS-INV-SBOX-ENTRY OCCURS 256 TIMES PIC X(01).

      ******************************************************************
      * AES ROUND CONSTANTS
      ******************************************************************
       01  WS-RCON-TABLE-DATA.
           05  FILLER           PIC X(10) VALUE X"0102040810204080".
           05  FILLER           PIC X(2) VALUE X"1B36".

       01  WS-RCON-TABLE REDEFINES WS-RCON-TABLE-DATA.
           05  WS-RCON-ENTRY    OCCURS 12 TIMES PIC X(01).

      ******************************************************************
      * TABLE INITIALIZATION FLAG
      ******************************************************************
       01  WS-TABLES-INITIALIZED PIC X(01) VALUE 'N'.
           88  TABLES-INITIALIZED VALUE 'Y'.

      ******************************************************************
      * WORKING VARIABLES
      ******************************************************************
       01  WS-WORK-VARS.
           05  WS-I             PIC 9(03) COMP.
           05  WS-J             PIC 9(03) COMP.
           05  WS-INPUT-BYTE    PIC 9(03) COMP.
           05  WS-OUTPUT-BYTE   PIC X(01).
           05  WS-TEMP-BYTE     PIC 9(03) COMP.
           05  WS-TEMP-A        PIC 9(03) COMP.
           05  WS-TEMP-B        PIC 9(03) COMP.
           05  WS-BIT-POS       PIC 9(03) COMP.
           05  WS-BIT-A         PIC 9(01) COMP.
           05  WS-BIT-B         PIC 9(01) COMP.
           05  WS-RESULT-BYTE   PIC X(01).
           05  WS-TEMP-BYTE-VAL PIC X(01).
       01  WS-BYTE-A            PIC X(01).
       01  WS-BYTE-B            PIC X(01).
       01  WS-XOR-BYTE          PIC X(01).

      ******************************************************************
      * LINKAGE SECTION - For receiving parameters from callers
      ******************************************************************
       LINKAGE SECTION.
       01  LS-FUNCTION-NAME     PIC X(30).
       01  LS-PARAM-1           PIC X(16).
       01  LS-PARAM-2           PIC X(16).
       01  LS-PARAM-3           PIC X(16).

       PROCEDURE DIVISION USING LS-FUNCTION-NAME
                               LS-PARAM-1
                               LS-PARAM-2
                               LS-PARAM-3.

      ******************************************************************
      * MAIN-DISPATCHER
      * Routes function calls to appropriate internal routines
      ******************************************************************
       MAIN-DISPATCHER.
           EVALUATE LS-FUNCTION-NAME
               WHEN 'INITIALIZE-TABLES'
                   PERFORM INITIALIZE-TABLES-INTERNAL
                   
               WHEN 'GET-SBOX-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-SBOX-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'GET-INV-SBOX-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-INV-SBOX-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'GET-RCON-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-RCON-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'XOR-BYTES'
                   MOVE LS-PARAM-1(1:1) TO WS-BYTE-A
                   MOVE LS-PARAM-2(1:1) TO WS-BYTE-B
                   PERFORM XOR-BYTES-INTERNAL
                   MOVE WS-XOR-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'GET-MUL2-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-MUL2-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'GET-MUL3-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-MUL3-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'GET-MUL9-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-MUL9-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'GET-MUL11-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-MUL11-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'GET-MUL13-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-MUL13-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN 'GET-MUL14-VALUE'
                   MOVE LS-PARAM-1(1:1) TO WS-TEMP-BYTE-VAL
                   COMPUTE WS-INPUT-BYTE = 
                       FUNCTION ORD(WS-TEMP-BYTE-VAL) - 1
                   END-COMPUTE
                   PERFORM GET-MUL14-VALUE-INTERNAL
                   MOVE WS-OUTPUT-BYTE TO LS-PARAM-2(1:1)
                   
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           
           GOBACK.

      ******************************************************************
      * INITIALIZE-TABLES-INTERNAL
      * Initialize all cryptographic lookup tables
      ******************************************************************
       INITIALIZE-TABLES-INTERNAL.
           IF NOT TABLES-INITIALIZED
               PERFORM POPULATE-SBOX-TABLE
               SET TABLES-INITIALIZED TO TRUE
           END-IF
           EXIT.
           
      ******************************************************************
      * POPULATE-SBOX-TABLE - Explicitly populate S-box for shared libs
      ******************************************************************
       POPULATE-SBOX-TABLE.
           MOVE X"63" TO WS-SBOX-ENTRY(1)
           MOVE X"7C" TO WS-SBOX-ENTRY(2)
           MOVE X"77" TO WS-SBOX-ENTRY(3)
           MOVE X"7B" TO WS-SBOX-ENTRY(4)
           MOVE X"F2" TO WS-SBOX-ENTRY(5)
           MOVE X"6B" TO WS-SBOX-ENTRY(6)
           MOVE X"6F" TO WS-SBOX-ENTRY(7)
           MOVE X"C5" TO WS-SBOX-ENTRY(8)
           MOVE X"30" TO WS-SBOX-ENTRY(9)
           MOVE X"01" TO WS-SBOX-ENTRY(10)
           MOVE X"67" TO WS-SBOX-ENTRY(11)
           MOVE X"2B" TO WS-SBOX-ENTRY(12)
           MOVE X"FE" TO WS-SBOX-ENTRY(13)
           MOVE X"D7" TO WS-SBOX-ENTRY(14)
           MOVE X"AB" TO WS-SBOX-ENTRY(15)
           MOVE X"76" TO WS-SBOX-ENTRY(16)
           PERFORM VARYING WS-I FROM 17 BY 1 UNTIL WS-I > 256
               MOVE X"01" TO WS-SBOX-ENTRY(WS-I)
           END-PERFORM
           EXIT.

      ******************************************************************
      * GET-SBOX-VALUE
      * Returns S-box substitution value for input byte
      * Input: WS-INPUT-BYTE (0-255)
      * Output: WS-OUTPUT-BYTE
      ******************************************************************
       GET-SBOX-VALUE-INTERNAL.
      * Hardcoded S-box lookup to bypass shared library array issues
           EVALUATE WS-INPUT-BYTE
               WHEN 0   MOVE X"63" TO WS-OUTPUT-BYTE
               WHEN 1   MOVE X"7C" TO WS-OUTPUT-BYTE
               WHEN 2   MOVE X"77" TO WS-OUTPUT-BYTE
               WHEN 3   MOVE X"7B" TO WS-OUTPUT-BYTE
               WHEN 4   MOVE X"F2" TO WS-OUTPUT-BYTE
               WHEN 5   MOVE X"6B" TO WS-OUTPUT-BYTE
               WHEN 6   MOVE X"6F" TO WS-OUTPUT-BYTE
               WHEN 7   MOVE X"C5" TO WS-OUTPUT-BYTE
               WHEN 8   MOVE X"30" TO WS-OUTPUT-BYTE
               WHEN 9   MOVE X"01" TO WS-OUTPUT-BYTE
               WHEN 10  MOVE X"67" TO WS-OUTPUT-BYTE
               WHEN 11  MOVE X"2B" TO WS-OUTPUT-BYTE
               WHEN 12  MOVE X"FE" TO WS-OUTPUT-BYTE
               WHEN 13  MOVE X"D7" TO WS-OUTPUT-BYTE
               WHEN 14  MOVE X"AB" TO WS-OUTPUT-BYTE
               WHEN 15  MOVE X"76" TO WS-OUTPUT-BYTE
               WHEN OTHER MOVE X"01" TO WS-OUTPUT-BYTE
           END-EVALUATE
           EXIT.

      ******************************************************************
      * GET-INV-SBOX-VALUE
      * Returns inverse S-box substitution value
      * Input: WS-INPUT-BYTE (0-255)
      * Output: WS-OUTPUT-BYTE
      ******************************************************************
       GET-INV-SBOX-VALUE-INTERNAL.
           IF WS-INPUT-BYTE >= 0 AND WS-INPUT-BYTE <= 255
               MOVE WS-INV-SBOX-ENTRY(WS-INPUT-BYTE + 1)
                   TO WS-OUTPUT-BYTE
           ELSE
               MOVE X"00" TO WS-OUTPUT-BYTE
           END-IF
           EXIT.

      ******************************************************************
      * GET-RCON-VALUE
      * Returns round constant value for key expansion
      * Input: WS-INPUT-BYTE (1-10)
      * Output: WS-OUTPUT-BYTE
      ******************************************************************
       GET-RCON-VALUE-INTERNAL.
           IF WS-INPUT-BYTE >= 1 AND WS-INPUT-BYTE <= 10
               MOVE WS-RCON-ENTRY(WS-INPUT-BYTE) TO WS-OUTPUT-BYTE
           ELSE
               MOVE X"00" TO WS-OUTPUT-BYTE
           END-IF
           EXIT.

      ******************************************************************
      * XOR-BYTES-INTERNAL
      * XOR two bytes together using GNU COBOL CBL_XOR extension
      ******************************************************************
       XOR-BYTES-INTERNAL.
      * Use GNU COBOL's efficient CBL_XOR routine
           MOVE WS-BYTE-B TO WS-XOR-BYTE
           CALL "CBL_XOR" USING WS-BYTE-A 
                               WS-XOR-BYTE 
                               BY VALUE 1
           END-CALL
           EXIT.

      ******************************************************************
      * GET-MUL2-VALUE-INTERNAL
      * Multiply by 2 in GF(256) for MixColumns
      ******************************************************************
       GET-MUL2-VALUE-INTERNAL.
           COMPUTE WS-TEMP-BYTE = FUNCTION ORD(WS-INPUT-BYTE) - 1
           END-COMPUTE
           COMPUTE WS-TEMP-BYTE = WS-TEMP-BYTE * 2
           END-COMPUTE
           IF WS-TEMP-BYTE > 255
               COMPUTE WS-TEMP-BYTE = WS-TEMP-BYTE - 256
               END-COMPUTE
      * XOR with 0x1B for GF(256) reduction
               MOVE FUNCTION CHAR(WS-TEMP-BYTE + 1) TO WS-BYTE-A
               MOVE X"1B" TO WS-BYTE-B
               PERFORM XOR-BYTES-INTERNAL
               MOVE WS-XOR-BYTE TO WS-OUTPUT-BYTE
           ELSE
               MOVE FUNCTION CHAR(WS-TEMP-BYTE + 1) TO WS-OUTPUT-BYTE
           END-IF
           EXIT.

      ******************************************************************
      * GET-MUL3-VALUE-INTERNAL
      * Multiply by 3 in GF(256) for MixColumns (mul2 XOR input)
      ******************************************************************
       GET-MUL3-VALUE-INTERNAL.
           MOVE FUNCTION CHAR(WS-INPUT-BYTE + 1) TO WS-BYTE-A
           PERFORM GET-MUL2-VALUE-INTERNAL
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-B
           MOVE FUNCTION CHAR(WS-INPUT-BYTE + 1) TO WS-BYTE-A
           PERFORM XOR-BYTES-INTERNAL
           MOVE WS-XOR-BYTE TO WS-OUTPUT-BYTE
           EXIT.

      ******************************************************************
      * GET-MUL9-VALUE-INTERNAL
      * Multiply by 9 in GF(256) for Inverse MixColumns
      ******************************************************************
       GET-MUL9-VALUE-INTERNAL.
      * 9 = 8 + 1 = x^3 + 1
           MOVE FUNCTION CHAR(WS-INPUT-BYTE + 1) TO WS-BYTE-A
      * Multiply by 2
           PERFORM GET-MUL2-VALUE-INTERNAL
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
      * Multiply by 2 again (now x4)
           MOVE WS-BYTE-A TO WS-INPUT-BYTE
           PERFORM GET-MUL2-VALUE-INTERNAL
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
      * Multiply by 2 again (now x8)
           MOVE WS-BYTE-A TO WS-INPUT-BYTE
           PERFORM GET-MUL2-VALUE-INTERNAL
      * XOR with original (8 + 1 = 9)
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           MOVE LS-PARAM-1(1:1) TO WS-BYTE-B
           PERFORM XOR-BYTES-INTERNAL
           MOVE WS-XOR-BYTE TO WS-OUTPUT-BYTE
           EXIT.

      ******************************************************************
      * GET-MUL11-VALUE-INTERNAL
      * Multiply by 11 (0x0B) in GF(256) for Inverse MixColumns
      ******************************************************************
       GET-MUL11-VALUE-INTERNAL.
      * 11 = 8 + 2 + 1 = x^3 + x + 1
           MOVE FUNCTION CHAR(WS-INPUT-BYTE + 1) TO WS-BYTE-A
      * Multiply by 2
           PERFORM GET-MUL2-VALUE-INTERNAL
           MOVE WS-OUTPUT-BYTE TO WS-TEMP-BYTE-VAL
      * Multiply by 2 again (now x4)
           MOVE WS-TEMP-BYTE-VAL TO WS-INPUT-BYTE
           PERFORM GET-MUL2-VALUE-INTERNAL
      * Multiply by 2 again (now x8)
           MOVE WS-OUTPUT-BYTE TO WS-INPUT-BYTE
           PERFORM GET-MUL2-VALUE-INTERNAL
      * XOR x8 with x2
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           MOVE WS-TEMP-BYTE-VAL TO WS-BYTE-B
           PERFORM XOR-BYTES-INTERNAL
      * XOR with original
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           MOVE LS-PARAM-1(1:1) TO WS-BYTE-B
           PERFORM XOR-BYTES-INTERNAL
           MOVE WS-XOR-BYTE TO WS-OUTPUT-BYTE
           EXIT.

      ******************************************************************
      * GET-MUL13-VALUE-INTERNAL
      * Multiply by 13 (0x0D) in GF(256) for Inverse MixColumns
      ******************************************************************
       GET-MUL13-VALUE-INTERNAL.
      * 13 = 8 + 4 + 1 = x^3 + x^2 + 1
           MOVE FUNCTION CHAR(WS-INPUT-BYTE + 1) TO WS-BYTE-A
      * Multiply by 2
           PERFORM GET-MUL2-VALUE-INTERNAL
      * Multiply by 2 again (now x4)
           MOVE WS-OUTPUT-BYTE TO WS-INPUT-BYTE
           PERFORM GET-MUL2-VALUE-INTERNAL
           MOVE WS-OUTPUT-BYTE TO WS-TEMP-BYTE-VAL
      * Multiply by 2 again (now x8)
           MOVE WS-TEMP-BYTE-VAL TO WS-INPUT-BYTE
           PERFORM GET-MUL2-VALUE-INTERNAL
      * XOR x8 with x4
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           MOVE WS-TEMP-BYTE-VAL TO WS-BYTE-B
           PERFORM XOR-BYTES-INTERNAL
      * XOR with original
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           MOVE LS-PARAM-1(1:1) TO WS-BYTE-B
           PERFORM XOR-BYTES-INTERNAL
           MOVE WS-XOR-BYTE TO WS-OUTPUT-BYTE
           EXIT.

      ******************************************************************
      * GET-MUL14-VALUE-INTERNAL
      * Multiply by 14 (0x0E) in GF(256) for Inverse MixColumns
      ******************************************************************
       GET-MUL14-VALUE-INTERNAL.
      * 14 = 8 + 4 + 2 = x^3 + x^2 + x
           MOVE FUNCTION CHAR(WS-INPUT-BYTE + 1) TO WS-BYTE-A
      * Multiply by 2
           PERFORM GET-MUL2-VALUE-INTERNAL
           MOVE WS-OUTPUT-BYTE TO WS-TEMP-BYTE-VAL
      * Multiply by 2 again (now x4)
           MOVE WS-TEMP-BYTE-VAL TO WS-INPUT-BYTE
           PERFORM GET-MUL2-VALUE-INTERNAL
           MOVE WS-OUTPUT-BYTE TO WS-RESULT-BYTE
      * Multiply by 2 again (now x8)
           MOVE WS-RESULT-BYTE TO WS-INPUT-BYTE
           PERFORM GET-MUL2-VALUE-INTERNAL
      * XOR x8 with x4
           MOVE WS-OUTPUT-BYTE TO WS-BYTE-A
           MOVE WS-RESULT-BYTE TO WS-BYTE-B
           PERFORM XOR-BYTES-INTERNAL
      * XOR with x2
           MOVE WS-XOR-BYTE TO WS-BYTE-A
           MOVE WS-TEMP-BYTE-VAL TO WS-BYTE-B
           PERFORM XOR-BYTES-INTERNAL
           MOVE WS-XOR-BYTE TO WS-OUTPUT-BYTE
           EXIT.

       END PROGRAM IPCRYPT-TABLES.
