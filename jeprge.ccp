       IDENTIFICATION DIVISION.
       PROGRAM-ID. JEPRGE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. RS-6000.
       OBJECT-COMPUTER. RS-6000.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       COPY 'JEMAP2'.    
       COPY 'DFHBMSCA'.
       COPY 'ORDFILE-LAYOUT'.
      *NEW       
       01 WS-PRODX1LI              PIC X(4).
       01 WS-PRODX2LI              PIC X(4).
       01 WS-PRODX3LI              PIC X(4).
       01 WS-PRODX4LI              PIC X(4).

       01 WS-PROD-ERROR-FLAG       PIC 9 VALUE 0.
       01 WS-PROD-ENTERED          PIC 9 VALUE 0.

       01 WS-PROD-ERROR-MSG.
          05 WS-PROD-LBL.
              10 FILLER            PIC X(5) VALUE 'PROD'.
              10 WS-PROD-NUM       PIC 9 VALUE 0.
              10 FILLER            PIC XX VALUE ': '.
          05 WS-PROD-ERROR         PIC X(36).

       01 WS-TRANSFER-FIELD        PIC X(3).
       01 WS-TRANSFER-LENGTH       PIC S9(4) COMP VALUE 3.   
      *NEW 
       LINKAGE SECTION.
           01 DFHCOMMAREA.
               05 LK-TRANSFER      PIC X(3).
               
       PROCEDURE DIVISION.
       000-START-LOGIC.
           EXEC CICS HANDLE CONDITION 
                  MAPFAIL(100-FIRST-TIME)
                  DUPREC(9999-DUPLICATE-RECORD)
           END-EXEC.
           
           EXEC CICS HANDLE AID
              PF1(910-BACK-TO-PRGM) 
              PF9(999-EXIT) 
           END-EXEC.
      *NEW
           IF EIBCALEN = 3
               GO TO 100-FIRST-TIME
           END-IF.
           
           EXEC CICS 
              RECEIVE MAP('MAP2') MAPSET('JEMAP2') 
           END-EXEC.
           
           GO TO 200-MAIN-LOGIC.
           
       100-FIRST-TIME.
           MOVE LOW-VALUES TO MAP2O.
           PERFORM 500-UNPROTECT-MAP.
           MOVE '   E N T R Y  S C R E E N' TO TITLEO.
           EXEC CICS
                 SEND MAP('MAP2') MAPSET('JEMAP2') ERASE 
           END-EXEC.

           EXEC CICS 
                RETURN TRANSID('JE03') 
           END-EXEC.
           
       200-MAIN-LOGIC.
           PERFORM 310-INVOICE-NUMBER-CHECK.
           PERFORM 500-UNPROTECT-MAP.
           PERFORM 320-CONTACT-INFORMATION-CHECK.
           PERFORM 330-ALINE-INFORMATION-CHECK.
           PERFORM 340-POSTAL-CODE-CHECK.
           PERFORM 350-PHONE-NUMBER-CHECK.
           PERFORM 360-PARTS-CHECK.
           PERFORM 600-MOVE.
           PERFORM 700-WRITE.
           GO TO 900-SEND-MAP.
           
       310-INVOICE-NUMBER-CHECK.
      *Check for Exit Input 
           IF INVNUMI  = 'XXXXXXX' OR INVNUMI(1:5) = 'ABORT'
              EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC
              EXEC CICS RETURN END-EXEC
           END-IF.

           PERFORM 500-UNPROTECT-MAP.
           
      *Check if Invoice Length is not 7 
           IF INVNUML IS NOT EQUAL TO 7
                MOVE LOW-VALUES TO MAP2O
                PERFORM 500-UNPROTECT-MAP
                MOVE 'INVOICE NUMBER MUST BE 7 LONG' TO MSGO
                MOVE -1 TO INVNUML
                MOVE DFHUNIMD TO INVNUMA
                GO TO 900-SEND-MAP
           END-IF.
      *Check if Invoice is not numeric     
           IF INVNUMI IS NOT NUMERIC
                MOVE LOW-VALUES TO MAP2O
                PERFORM 500-UNPROTECT-MAP
                MOVE 'INVOICE MUST BE NUMERIC' TO MSGO
                MOVE -1 TO INVNUML
                MOVE DFHUNIMD TO INVNUMA       
                GO TO 900-SEND-MAP
           END-IF.    
           
       320-CONTACT-INFORMATION-CHECK.
      *Check is Name length is not 4
           IF NAMEL LESS THAN 4
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'NAME MUST BE AT LEAST 4 CHARACTERS' TO MSGO
               MOVE -1 TO NAMEL
               MOVE DFHUNIMD TO NAMEA        
       
               GO TO 900-SEND-MAP
           END-IF.   
      *Check is Name input is blank     
           IF NAMEI = LOW-VALUES OR NAMEI = SPACES 
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'NAME MUST HAVE AN INPUT' TO MSGO
               MOVE -1 TO NAMEL
               MOVE DFHUNIMD TO NAMEA           
       
               GO TO 900-SEND-MAP
           END-IF.
           
       330-ALINE-INFORMATION-CHECK.
      *Check if Address Line 1 is blank 
          IF ALINE1I = LOW-VALUES
          OR ALINE1I = SPACES
              MOVE LOW-VALUES TO MAP2O
              PERFORM 500-UNPROTECT-MAP
              MOVE 'ADDRESS LINE 1 MUST HAVE AN INPUT' TO MSGO
              MOVE -1 TO ALINE1L
              MOVE DFHUNIMD TO ALINE1A          
      
              GO TO 900-SEND-MAP
          END-IF.
      *Check if Address Line 1 Length is less than 3     
           IF ALINE1L LESS THAN 3
               MOVE LOW-VALUE TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'ADDRESS LINE 1 MUST HAVE AT LEAST 3 CHARACTERS' 
               TO MSGO
               MOVE -1 TO ALINE1L
               MOVE DFHUNIMD TO ALINE1A   
                
               GO TO 900-SEND-MAP
           END-IF.    
       
      *Check if Address Line 2 is blank     
          IF ALINE2I = LOW-VALUES
          OR ALINE2I = SPACES
              MOVE LOW-VALUES TO MAP2O
              PERFORM 500-UNPROTECT-MAP
              MOVE 'ADDRESS LINE 2 MUST HAVE AN INPUT' TO MSGO
              MOVE -1 TO ALINE2L
              MOVE DFHUNIMD TO ALINE2A          
      
              GO TO 900-SEND-MAP
          END-IF.
      *Check if Address Line 2 Length is less than 3 
           IF ALINE2L LESS THAN 3
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'ADDRESS LINE 2 MUST HAVE AT LEAST 3 CHARACTERS' 
               TO MSGO
               MOVE -1 TO ALINE2L
               MOVE DFHUNIMD TO ALINE2A          
       
               GO TO 900-SEND-MAP
           END-IF.    

      *Check if Address Line 3 has input      
           IF ALINE3L >= 1
      *Check if Address Line 3 is blank
              IF ALINE3I = SPACES
              OR ALINE3I = LOW-VALUES
                  MOVE LOW-VALUES TO MAP2O
                  PERFORM 500-UNPROTECT-MAP
                  MOVE 'ADDRESS LINE 3 MUST HAVE INPUT' TO MSGO
                  MOVE -1 TO ALINE3L
                  MOVE DFHUNIMD TO ALINE3A  
                  GO TO 900-SEND-MAP
              END-IF
      *Check if Address Line 3 Length is less than 3         
               IF ALINE3I LESS THAN 3
                   MOVE LOW-VALUES TO MAP2O
                   PERFORM 500-UNPROTECT-MAP
                   MOVE 'ADDRESS LINE 3 MUST HAVE AT LEAST 3 CHARACTERS' 
                   TO MSGO
                   MOVE -1 TO ALINE3L
                   MOVE DFHUNIMD TO ALINE3A  
                   GO TO 900-SEND-MAP
           END-IF.
           
       340-POSTAL-CODE-CHECK.
      *If postal code is blank
           IF POST1I = LOW-VALUES
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'POSTAL CODE MUST HAVE INPUT' TO MSGO
               MOVE -1 TO POST1L
               MOVE DFHUNIMD TO POST1A 
               GO TO 900-SEND-MAP
           END-IF.
      *If the postal code input does not follow the format 
           IF POST1I(1:1) IS NOT ALPHABETIC 
           OR POST1I(3:1) IS NOT ALPHABETIC 
           OR POST1I(2:1) IS NOT NUMERIC
  
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'POSTAL CODE 1 MUST FOLLOW THE L#L FORMAT' TO MSGO
               MOVE -1 TO POST1L
               MOVE DFHUNIMD TO POST1A  
               GO TO 900-SEND-MAP       
           END-IF.

      *If postal code 2 is blank 
           IF POST2I = LOW-VALUES
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'POSTAL CODE 2 MUST HAVE INPUT' TO MSGO
               MOVE -1 TO POST2L
               MOVE DFHUNIMD TO POST2A  
               GO TO 900-SEND-MAP
           END-IF.         
      *If the postal code input does not follow the format 
           IF POST2I(1:1) IS NOT NUMERIC 
           OR POST2I(3:1) IS NOT NUMERIC
           OR POST2I(2:1) IS NOT ALPHABETIC
  
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'POSTAL CODE 2 MUST FOLLOW THE #L# FORMAT' TO MSGO
               MOVE -1 TO POST2L
               MOVE DFHUNIMD TO POST2A  
               GO TO 900-SEND-MAP          
           END-IF.     

       350-PHONE-NUMBER-CHECK.
      *Area Check 
      *If there is no input in area field
           IF AREAI = LOW-VALUES OR AREAI = SPACES
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'PHONE NUMBER MUST BE COMPLETE' TO MSGO 
               MOVE -1 TO AREAL
               MOVE DFHUNIMD TO AREAA  
               GO TO 900-SEND-MAP
           END-IF.

           IF AREAI IS NOT NUMERIC
                MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'AREA MUST BE NUMERIC' TO MSGO 
               MOVE -1 TO AREAL
               MOVE DFHUNIMD TO AREAA  
               GO TO 900-SEND-MAP
           END-IF.

      *If the phone number doesn't contain the 416,905 or 705 prefixes      
          IF (AREAI IS NOT EQUAL TO '416'
          AND AREAI IS NOT EQUAL TO '905' 
          AND AREAI IS NOT EQUAL TO '705')
          
              MOVE LOW-VALUES TO MAP2O
              PERFORM 500-UNPROTECT-MAP
              MOVE 'AREA MUST BE EITHER 416,905 OR 705' 
              TO MSGO
              MOVE -1 TO AREAL
              GO TO 900-SEND-MAP 
          END-IF.

      *Prefix Check   
           IF PREFIXI = LOW-VALUES OR PREFIXI = SPACES
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'PHONE NUMBER MUST BE COMPLETE' TO MSGO 
               MOVE -1 TO PREFIXL
               MOVE DFHUNIMD TO PREFIXA  
               GO TO 900-SEND-MAP
           END-IF.

           IF PREFIXI IS NOT NUMERIC
                MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'PREFIX MUST BE NUMERIC' TO MSGO 
               MOVE -1 TO PREFIXL
               MOVE DFHUNIMD TO PREFIXA  
               GO TO 900-SEND-MAP
           END-IF.
      *Line Number Check
           IF LINENUMI = LOW-VALUES OR LINENUMI = SPACES
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'PHONE NUMBER MUST BE COMPLETE' TO MSGO 
               MOVE -1 TO LINENUML
               MOVE DFHUNIMD TO LINENUMA  
               GO TO 900-SEND-MAP
           END-IF.

           IF LINENUMI IS NOT NUMERIC
                MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'LINENUM MUST BE NUMERIC' TO MSGO 
               MOVE -1 TO LINENUML
               MOVE DFHUNIMD TO LINENUMA  
               GO TO 900-SEND-MAP
           END-IF.
            
           
       360-PARTS-CHECK.
      *Check if the product 1A number has only alpabetic characters
           IF PROD1AI IS NOT ALPHABETIC
               MOVE LOW-VALUE TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'PROD1A MUST BE ALPHABETIC' TO MSGO
               MOVE -1 TO PROD1AL
               MOVE DFHUNIMD TO PROD1AA   
               GO TO 900-SEND-MAP
           END-IF.
           
      *Check is the product 1A number's length is less than 4     
           IF PROD1AL LESS THAN 4
               MOVE LOW-VALUE TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'PRODUCT 1A MUST BE AT LEAST 4 LONG' TO MSGO
               MOVE -1 TO PROD1AL
               MOVE DFHUNIMD TO PROD1AA 
               GO TO 900-SEND-MAP
           END-IF.
           
      *Check if product 1B has 4 numeric input    
           IF PROD1BI IS NOT NUMERIC
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'PROD1B MUST BE NUMERIC' TO MSGO
               MOVE -1 TO PROD1BL
               MOVE DFHUNIMD TO PROD1BA 
               GO TO 900-SEND-MAP
           END-IF.
      *Check if product 1B number's length is less than 4     
           IF PROD1BL LESS THAN 4
               MOVE LOW-VALUES TO MAP2O
               PERFORM 500-UNPROTECT-MAP
               MOVE 'PROD1B MUST BE AT LEAST 4 LONG' TO MSGO
               MOVE -1 TO PROD1BL
               MOVE DFHUNIMD TO PROD1BA 
               GO TO 900-SEND-MAP
           END-IF.
       
       
      *Allows you to enter stuff in the map? 
       500-UNPROTECT-MAP.
       
      *Unprotect Name 
           MOVE DFHBMFSE TO NAMEA.
      *Unprotect Address Line     
           MOVE DFHBMFSE TO ALINE1A.
           MOVE DFHBMFSE TO ALINE2A.
           MOVE DFHBMFSE TO ALINE3A.
      *Unprotect Postal Code     
           MOVE DFHBMFSE TO POST1A.
           MOVE DFHBMFSE TO POST2A.
      *Unprotect Phone Number     
           MOVE DFHBMFSE TO AREAA.
           MOVE DFHBMFSE TO PREFIXA.
           MOVE DFHBMFSE TO LINENUMA.
      *Unprotect Products       
           MOVE DFHBMFSE TO PROD1AA. 
           MOVE DFHBMFSE TO PROD1BA.
            
           MOVE DFHBMFSE TO PROD2AA. 
           MOVE DFHBMFSE TO PROD2BA.
           
           MOVE DFHBMFSE TO PROD3AA. 
           MOVE DFHBMFSE TO PROD3BA.
            
           MOVE DFHBMFSE TO PROD4AA. 
           MOVE DFHBMFSE TO PROD4BA.
           
           MOVE DFHBMFSE TO PROD5AA. 
           MOVE DFHBMFSE TO PROD5BA.
       
       600-MOVE.
      *Move Invoice Number 
           MOVE INVNUMI TO ORDFILE-INVOICE-NO.
      *Move Name     
           MOVE NAMEI TO ORDFILE-NAME.
           
      *Move Address Line         
           MOVE ALINE1I TO ORDFILE-ADDR-LINE1.
           MOVE ALINE2I TO ORDFILE-ADDR-LINE2.
           MOVE ALINE3I TO ORDFILE-ADDR-LINE3.  
       
      *Move Postal Code
           MOVE POST1I TO ORDFILE-POSTAL-1.
           MOVE POST2I TO ORDFILE-POSTAL-2.
           
      *Move Phone Numbers
           MOVE AREAI TO ORDFILE-AREA-CODE.
           MOVE PREFIXI TO ORDFILE-EXCHANGE.
           MOVE LINENUMI TO ORDFILE-PHONE-NUM.
                 
      *Move Products     
           MOVE PROD1AI TO ORDFILE-P1A.
           MOVE PROD1BI TO ORDFILE-P1B.
           
           MOVE PROD2AI TO ORDFILE-P2A.
           MOVE PROD2BI TO ORDFILE-P2B.
           
           MOVE PROD3AI TO ORDFILE-P3A.
           MOVE PROD3BI TO ORDFILE-P3B.
       
           MOVE PROD4AI TO ORDFILE-P4A.
           MOVE PROD4BI TO ORDFILE-P4B.
           
           MOVE PROD5AI TO ORDFILE-P5A.
           MOVE PROD5BI TO ORDFILE-P5B.

       700-WRITE.
           EXEC CICS WRITE FILE('ORDFILE')
               FROM (ORDFILE-RECORD)
               LENGTH (ORDFILE-LENGTH)
               RIDFLD (ORDFILE-KEY)
           END-EXEC.
           MOVE -1 TO INVNUML
           MOVE DFHUNIMD TO INVNUMA
           MOVE 'ENTRY HAS BEEN ADDED!' TO MSGO.     
      *Moves your cursor at whatever? 
       900-SEND-MAP.
            MOVE '   E N T R Y  S C R E E N' TO TITLEO.     
            EXEC CICS SEND MAP('MAP2') MAPSET('JEMAP2') CURSOR END-EXEC.
            EXEC CICS RETURN TRANSID('JE03') END-EXEC.

       910-BACK-TO-PRGM.
           EXEC CICS XCTL
               PROGRAM('JEPRGM')
               COMMAREA(WS-TRANSFER-FIELD)
               LENGTH(WS-TRANSFER-LENGTH)
           END-EXEC.

            MOVE LOW-VALUES TO MAP2O.
            EXEC CICS SEND MAP('MAP2') MAPSET('JEMAP2') END-EXEC.
            EXEC CICS RETURN TRANSID('JE03') END-EXEC.            
     
       999-EXIT.
            MOVE LOW-VALUES TO MAP2O.
            MOVE 'BYE' TO MSGO.
            EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC
            EXEC CICS RETURN END-EXEC
            GOBACK.     
            
       9999-DUPLICATE-RECORD.
            MOVE LOW-VALUES TO MAP2O.
            MOVE 'RECORD ALREADY EXISTS!' TO MSGO.
            MOVE -1 TO INVNUML.
            MOVE DFHUNIMD TO INVNUMA.
            GO TO 900-SEND-MAP.


        END PROGRAM JEPRGE.