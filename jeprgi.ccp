       IDENTIFICATION DIVISION.
       PROGRAM-ID. JEPRGI.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. RS-6000.
       OBJECT-COMPUTER. RS-6000.
       

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       COPY 'JEMAP2'.
       COPY 'ORDFILE-LAYOUT'.
      *NEW       
            01 WS-PRODX1LI PIC X(4).
            01 WS-PRODX2LI PIC X(4).
            01 WS-PRODX3LI PIC X(4).
            01 WS-PRODX4LI PIC X(4).

            01 WS-PROD-ERROR-FLAG  PIC 9 VALUE 0.
            01 WS-PROD-ENTERED     PIC 9 VALUE 0.

            01 WS-PROD-ERROR-MSG.
               05 WS-PROD-LBL.
                   10 FILLER           PIC X(5) VALUE 'PROD'.
                   10 WS-PROD-NUM      PIC 9 VALUE 0.
                   10 FILLER           PIC XX VALUE ': '.
               05 WS-PROD-ERROR      PIC X(36).
      *NEW 
       LINKAGE SECTION.
           01 DFHCOMMAREA.
               05 LK-TRANSFER    PIC X(3).

       PROCEDURE DIVISION.
       
       000-START-LOGIC.
           EXEC CICS HANDLE CONDITION 
                  MAPFAIL(100-FIRST-TIME)
                  NOTFND(400-RECORD-NOT-FOUND)
           END-EXEC.
           
           EXEC CICS HANDLE AID 
              PF9(999-EXIT) 
           END-EXEC.
      * NEW
           IF EIBCALEN = 3
               GO TO 100-FIRST-TIME
           END-IF.
           
           EXEC CICS 
              RECEIVE MAP('MAP2') MAPSET('JEMAP2') 
           END-EXEC.
           
           GO TO 200-MAIN-LOGIC.
           
       100-FIRST-TIME.
           MOVE LOW-VALUES TO MAP2O.
           
           EXEC CICS
                 SEND MAP('MAP2') MAPSET('JEMAP2') ERASE 
           END-EXEC.

           EXEC CICS 
                RETURN TRANSID('JE02') 
           END-EXEC.
           
       200-MAIN-LOGIC.
          IF INVNUMI  = 'XXXXXXX' OR INVNUMI(1:5) = 'ABORT'
              EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC
              EXEC CICS RETURN END-EXEC
           END-IF.
           
           IF INVNUML IS NOT EQUAL TO 7
                MOVE LOW-VALUES TO MAP2O
                MOVE 'INVOICE NUMBER MUST BE 7 LONG' TO MSGO
                GO TO 900-SEND-MAP
           END-IF.

           IF INVNUMI IS NOT NUMERIC
                MOVE LOW-VALUES TO MAP2O
                MOVE 'INVOICE MUST BE NUMERIC' TO MSGO
                GO TO 900-SEND-MAP
           END-IF.

           MOVE INVNUMI TO ORDFILE-INVOICE-NO.

           EXEC CICS READ FILE('ORDFILE')
              INTO(ORDFILE-RECORD)
              LENGTH(ORDFILE-LENGTH)
              RIDFLD(ORDFILE-KEY)
           END-EXEC.
           
           MOVE LOW-VALUES TO MAP2O.
           
           MOVE ORDFILE-INVOICE-NO TO INVNUMO.

           MOVE ORDFILE-P1A TO PROD1AO.
           MOVE ORDFILE-P1B TO PROD1BO.

           MOVE ORDFILE-P2A TO PROD2AO.
           MOVE ORDFILE-P2B TO PROD2BO.

           MOVE ORDFILE-P3A TO PROD3AO.
           MOVE ORDFILE-P3B TO PROD3BO.

           MOVE ORDFILE-P4A TO PROD4AO.
           MOVE ORDFILE-P4B TO PROD4BO.

           MOVE ORDFILE-P5A TO PROD5AO.
           MOVE ORDFILE-P5B TO PROD5BO.

           MOVE ORDFILE-NAME TO NAMEO.

           MOVE ORDFILE-ADDR-LINE1 TO ALINE1O.            
           MOVE ORDFILE-ADDR-LINE2 TO ALINE2O. 
           MOVE ORDFILE-ADDR-LINE3 TO ALINE3O.

           MOVE ORDFILE-POSTAL-1 TO POST1O. 
           MOVE ORDFILE-POSTAL-2 TO POST2O.

           MOVE ORDFILE-AREA-CODE TO AREAO.
           MOVE ORDFILE-EXCHANGE TO PREFIXO.
           MOVE ORDFILE-PHONE-NUM TO LINENUMO. 

           MOVE 'RECORD FOUND!' TO MSGO.

           GO TO 900-SEND-MAP.

       400-RECORD-NOT-FOUND.
           MOVE LOW-VALUES TO MAP2O.
           MOVE 'RECORD NOT FOUND' TO MSGO.
           GO TO 900-SEND-MAP.
           
       900-SEND-MAP.
            EXEC CICS SEND MAP('MAP2') MAPSET('JEMAP2') ERASE END-EXEC.
            EXEC CICS RETURN TRANSID('JE02') END-EXEC. 


       999-EXIT.
            MOVE LOW-VALUES TO MAP2O.
            MOVE 'BYE' TO MSGO.
            EXEC CICS SEND MAP('MAP2') MAPSET('JEMAP2') END-EXEC.
            EXEC CICS RETURN END-EXEC.
       
           GOBACK.
