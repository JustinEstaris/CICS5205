       IDENTIFICATION DIVISION.
       PROGRAM-ID. JEPRGM.
       AUTHOR. JUSTIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. RS-6000.
       OBJECT-COMPUTER. RS-6000.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY 'JEMAP1'.

       01 WS-OPT-MSG.
       05 FILLER                   PIC X(26)
           VALUE 'YOU HAVE SELECTED OPTION #'.
       05 WS-OPT-MSG-NUM           PIC 9.

       01 WS-TRANSFER-FIELD        PIC X(3).
       01 WS-TRANSFER-LENGTH       PIC S9(4) COMP VALUE 3.

       LINKAGE SECTION.
           01 DFHCOMMAREA.
               05 LK-TRANSFER      PIC X(3).
       
       PROCEDURE DIVISION.
       000-START-LOGIC.
      *NEW 
       EXEC CICS HANDLE AID 
           PF9(700-CHOICE-9)
           PF1(300-CHOICE-1)
           PF2(400-CHOICE-2)
       END-EXEC.
       
       EXEC CICS HANDLE CONDITION MAPFAIL(100-FIRST-TIME) END-EXEC.
       
       IF EIBCALEN = 3
          GO TO 100-FIRST-TIME
       END-IF.
       
       EXEC CICS RECEIVE MAP('MAP1') MAPSET('JEMAP1') END-EXEC.
       
       GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.
        MOVE LOW-VALUES TO MAP1O.

        EXEC CICS SEND MAP('MAP1') MAPSET('JEMAP1') ERASE
            END-EXEC.

        EXEC CICS RETURN TRANSID('JE01') END-EXEC.

       200-MAIN-LOGIC.
 
        IF CHOICEI IS EQUAL TO '1'
            GO TO 300-CHOICE-1
        ELSE IF CHOICEI IS EQUAL TO '2'
            GO TO 400-CHOICE-2
        ELSE IF CHOICEI IS EQUAL TO '3'
            GO TO 500-CHOICE-3
        ELSE IF CHOICEI IS EQUAL TO '4'
            GO TO 600-CHOICE-4             
        ELSE IF CHOICEI IS EQUAL TO '9'
            GO TO 700-CHOICE-9
        ELSE
            GO TO 999-SEND-ERROR-MSG
        END-IF.

       300-CHOICE-1.
           EXEC CICS XCTL
               PROGRAM('JEPRGE')
               COMMAREA(WS-TRANSFER-FIELD)
               LENGTH(WS-TRANSFER-LENGTH)
           END-EXEC.
           
            MOVE LOW-VALUES TO MAP1O.
            EXEC CICS SEND MAP('MAP1') MAPSET('JEMAP1') END-EXEC.
            EXEC CICS RETURN TRANSID('JE01') END-EXEC.

       400-CHOICE-2.
        EXEC CICS XCTL
               PROGRAM('JEPRGI')
               COMMAREA(WS-TRANSFER-FIELD)
               LENGTH(WS-TRANSFER-LENGTH)
        END-EXEC.
        
        MOVE LOW-VALUES TO MAP1O.
        MOVE 'YOU ENTERED TWO' TO MSGO.
        EXEC CICS SEND MAP('MAP1') MAPSET('JEMAP1') END-EXEC.
        EXEC CICS RETURN TRANSID('JE01') END-EXEC.

       500-CHOICE-3.
        MOVE LOW-VALUES TO MAP1O.
        MOVE 'YOU ENTERED THREE' TO MSGO.
        EXEC CICS SEND MAP('MAP1') MAPSET('JEMAP1') END-EXEC.
        EXEC CICS RETURN TRANSID('JE01') END-EXEC.

       600-CHOICE-4.
        MOVE LOW-VALUES TO MAP1O.
        MOVE 'YOU ENTERED FOUR' TO MSGO.
        EXEC CICS SEND MAP('MAP1') MAPSET('JEMAP1') END-EXEC.
        EXEC CICS RETURN TRANSID('JE01') END-EXEC.

       700-CHOICE-9.
        MOVE LOW-VALUES TO MAP1O.
        MOVE 'YOU ENTERED 9 -PROGRAM ENDING' TO MSGO.
        EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC.
        EXEC CICS RETURN END-EXEC.

       999-SEND-ERROR-MSG.
        MOVE LOW-VALUES TO MAP1O.

        IF CHOICEI IS NOT NUMERIC THEN
            MOVE 'INPUT MUST BE NUMERIC' TO MSGO
        ELSE IF (CHOICEI = SPACE OR CHOICEI = LOW-VALUES)
            MOVE 'INPUT MUST INPUTTED' TO MSGO
        END-IF.

        EXEC CICS SEND MAP('MAP1') MAPSET('JEMAP1') END-EXEC.
        EXEC CICS RETURN END-EXEC.