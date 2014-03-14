       IDENTIFICATION DIVISION.
       PROGRAM-ID. PbsRead.
       *> Koden är inte testat. Den är inte färdig !!!!!
       *>
       *> Authors: Peter B, Bertil K and Sergejs S.
       *> Purpose: Manage an invoice print company (PBS)
       *> Initial Version Created: 2014-03-11
       *>

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  INDATA ASSIGN TO 'indata.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS INFIL-FS.

           SELECT  FELDATA ASSIGN TO 'feldata.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FELFIL-FS.

       DATA DIVISION.
       FILE SECTION.

       FD  INDATA.
       01  INDATA-POST.
           05  INDATA-TYP  PIC X(2).
           05  FILLER      PIC X(298).

       FD FELDATA.
       01  FELDATA-POST.
           05  FELDATA-ROW PIC X(300).

       WORKING-STORAGE SECTION.
       01  SWITCHES.
           05  END-OF-FILE             PIC X VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           05  START-POST-EXIST-SW     PIC X VALUE 'N'
               88  START-POST-EXIST        VALUE 'Y'.
           05  END-POST-EXIST-SW       PIC X VALUE 'N'
               88  END-POST-EXIST          VALUE 'Y'.
           05  VALID-POST-FILE-SW      PIC X VALUE 'Y'.
               88  VALID-POST-FILE         VALUE 'Y'.
           05  VALID-FILE-NUMBER-SW      PIC X VALUE 'Y'.
               88  VALID-FILE-NUMBER       VALUE 'Y'.

           05 VALID-CUSTOMER-SW        PIC X VALUE 'Y'.
               88  VALID-CUSTOMER          VALUE 'Y'.
           05 VALID-ORDER-SW           PIC X VALUE 'Y'.
               88  VALID-ORDER             VALUE 'Y'.
           05 VALID-ARTIKEL-SW         PIC X VALUE 'Y'.
               88  VALID-ARTIKEL           VALUE 'Y'.
           05 VALID-ARTIKEL-SUMMA-SW   PIC X VALUE 'Y'.
               88  VALID-ARTIKEL-SUMMA     VALUE 'Y'.
           05 VALID-POSTER-COUNTS-SW   PIC X VALUE 'Y'.
               88  VALID-POSTER-COUNTS     VALUE 'Y'.

       01  FILE-STATUS-FIELDS.
           05 INFIL-FS                 PIC XX.
               88  INFIL-SUCCESSFUL        VALUE '00'.
           05 FELFIL-FS                PIC XX.
               88  FELFIL-SUCCESSFUL       VALUE '00'.

       01  COUNTS-FIELDS.
           05  POSTER-COUNT    PIC 9(6) VALUE ZERO.

       01  START-POST-10.
           05  POSTTYP-10      PIC X(2).
           05  SYSTEM-10       PIC X(8).
           05  KNDNR-10        PIC X(8).
           05  FILDAT-10       PIC X(8).
           05  FLNR-10         PIC X(5).
           05  FILLER          PIC X(269).

       01  ORDER-POST-20.
           05  POSTTYP-20      PIC X(2).
           05  ORDNR1-20       PIC X(10).
           05  GELNR2-20       PIC X(10).
           05  ORDDAT-20       PIC X(8).
           05  ORDRAB-20       PIC X(2).
           05  MOMS-20         PIC X(2).
           05  ORDSUM-20       PIC X(8).
           05  FAKTNR-20       PIC X(16).
           05  GELNMN-20       PIC X(40).
           05  GELADD1-20      PIC X(30).
           05  GELADD2-20      PIC X(30).
           05  GELADD3-20      PIC X(30).
           05  GELPNR-20       PIC X(5).
           05  GELPOR-20       PIC X(30).
           05  GELONR-20       PIC X(10).
           05  FILLER          PIC X(67).

       01  ARTIKEL-POST-30.
           05  POSTTYP-30      PIC X(2).
           05  ARTNR-30        PIC X(10).
           05  ARTANT-30       PIC X(4).
           05  ARTPRS-30       PIC X(6).
           05  SUMMA-30        PIC X(9).
           05  FILLER          PIC X(269).

       01  SLUT-POST-90.
           05  POSTTYP-90      PIC X(2).
           05  FILLER          PIC X(13).
           05  ANTAL-90        PIC X(6).
           05  FILLER          PIC X(279).


           EXEC SQL    INCLUDE SQLCA       END-EXEC.
           EXEC SQL    INCLUDE DEBTOR      END-EXEC.
           EXEC SQL    INCLUDE CUSTOMER    END-EXEC.
           EXEC SQL    INCLUDE INVOICE     END-EXEC.
           EXEC SQL    INCLUDE ERRDATA     END-EXEC.
           EXEC SQL    INCLUDE WORKTBL     END-EXEC.
           EXEC SQL    INCLUDE ITEMS       END-EXEC.
           EXEC SQL    INCLUDE INVPROD     END-EXEC.

       PROCEDURE DIVISION.

       000-POST-CUST.

           OPEN INPUT INDATA
                OUTPUT FELDATA

           IF INFIL-SUCCESSFUL
               READ INDATA
                   AT END SET EOF TO TRUE
               END-READ
           ELSE
               SET EOF TO TRUE
               DISPLAY ' Indata file error: '
           END-IF

           MOVE 'N' TO START-POST-EXIST-SW
           MOVE 'N' TO END-POST-EXIST-SW
           MOVE 'Y' TO VALID-FILE-NUMBER-SW
           MOVE ZERO TO POSTER-COUNT

           PERFORM 100-READ-CUST-FILE UNTIL EOF.

           CLOSE INDATA FELDATA
           STOP RUN.

      **********************************************************

       100-READ-CUST-FILE.

           MOVE 'Y' TO VALID-POST-FILE-SW
           MOVE 'Y' TO VALID-CUSTOMER-SW
           MOVE 'Y' TO VALID-ORDER-SW
           MOVE 'Y' TO VALID-ARTIKEL-SW
           MOVE 'Y' TO VALID-ARTIKEL-SUMMA-SW
           MOVE 'Y' TO VALID-POSTER-COUNTS-SW

           EVALUATE INDATA-TYP
               WHEN '10' PERFORM 110-START-POST
               WHEN '20' PERFORM 120-ORDER-POST
               WHEN '30' PERFORM 130-ARTIKEL-POST
               WHEN '90' PERFORM 190-SLUT-POST
               WHEN OTHER MOVE 'N' VALID-POST-FILE-SW
           END-EVALUATE

           IF NOT VALID-POST-FILE
               SET EOF TO TRUE
               DISPLAY ' Post-filen är ogiltig'
           ELSE IF NOT (START-POST-EXIST OR END-POST-EXIST)
               SET EOF TO TRUE
               DISPLAY ' Inläsningen avbryts på grund av filen har '
      -        ' inte START-POST eller SLUT-POST poster'
           END-IF

           IF NOT VALID-POSTER-COUNT
               PERFORM WRITE-BAD-FILE
           END-IF

           READ INDATA AT END SET EOF TO TRUE
       .
      **********************************************************

       110-START-POST.
           IF NOT START-POST-EXIST
               MOVE INDATA-POST TO START-POST-10
               SET START-POST-EXIST TO TRUE

               PERFORM 210-CHECK-CUSTOMER-NR
               IF VALID-POST-FILE
                   PERFORM 210-CHECK-LOPNR
               END-IF

               IF VALID-POST-FILE
                   ADD 1 TO POSTER-COUNT
               END-IF
           ELSE
               MOVE 'N' TO VALID-POST-FILE-SW
               DISPLAY ' START-POST är dubbel'
       .
      *****************************************************

       210-CHECK-CUSTOMER-NR.
           MOVE FUNCTION NUMVAL(KNDNR-10) TO CUSTOMER-CUS_NR
               EXEC SQL
                   SELECT
                   FROM CUSTOMER
                   WHERE CUS_NR = :CUSTOMER-CUS_NR
               END-EXEC

           IF SQLCODE NOT = ZERO
               MOVE 'N' TO VALID-POST-FILE-SW
               MOVE 'N' TO VALID-CUSTOMER-SW
               DISPLAY ' START-POST är ogiltig'
       .
      ****************************

       210-CHECK-LOPNR
           MOVE FUNCTION NUMVAL(FILNR-10) TO ....

               EXEC SQL
                   SELECT
                   FROM
                   WHERE
               END-EXEC

           IF SQLCODE NOT = ZERO
               MOVE 'N' TO VALID-POST-FILE-SW
               MOVE 'N' TO VALID-FILE-NUMBER-SW
               DISPLAY ' START-POST är ogiltig'
       .
      *****************************

       120-ORDER-POST.
           MOVE INDATA-POST TO ORDER-POST-20

           IF VALID-POST-FILE
               ADD 1 TO POSTER-COUNT
       .
      ****************************

       130-ARTIKEL-POST.
           MOVE INDATA-POST TO ARTIKEL-POST-30

           IF VALID-POST-FILE
               ADD 1 TO POSTER-COUNT
       .
      ***************************

       190-SLUT-POST.
           IF NOT END-POST-EXIST
               MOVE INDATA-POST TO SLUT-POST-90
               SET END-POST-EXIST TO TRUE

               IF FUNCTION NUMVAL(ANTAL-90) NOT = POSTER-COUNT
                   MOVE 'N' TO VALID-POSTER-COUNTS-SW
               END-IF
           ELSE
               MOVE 'N' TO VALID-POST-FILE-SW
               DISPLAY ' SLUTT-POST är dubbel'
       .
      ****************************

       WRITE-BAD-FILE.
           MOVE INDATA-POST TO FELDATA-POST
           WRITE FELDATA-POST
           DISPLAY ' *** See Bad transactions file !!! '
       .

