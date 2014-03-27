      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. servicemenu IS INITIAL.
      *
      * Authors: Peter B, Bertil K and Sergejs S.
      * Purpose: Maintain products/articles database table
      * Initial Version Created: 2014-03-19
      *
      **********************************************************
       ENVIRONMENT DIVISION.
      *---------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      **********************************************************
       DATA DIVISION.
      *---------------------------------------------------------
       FILE SECTION.
      **********************************************************
       WORKING-STORAGE SECTION.

           EXEC SQL INCLUDE SQLCA END-EXEC.

           EXEC SQL INCLUDE SRV END-EXEC.

           EXEC SQL INCLUDE CUSTOMER END-EXEC.

      *    cursors

      *    list produkter
           EXEC SQL
               DECLARE BCURSRV1 CURSOR FOR
               SELECT S.SRV_ID, S.ARTNO,
                      S.DESCRIPTION, S.CHARGE
               FROM TUTORIAL.SRV S
               ORDER BY S.SRV_ID
           END-EXEC

      *    get highest primary key
           EXEC SQL
               DECLARE BCURSRV2 CURSOR FOR
               SELECT SRV_ID
               FROM TUTORIAL.SRV
               ORDER BY SRV_ID DESC
           END-EXEC

      *    list customers ids
           EXEC SQL
               DECLARE BCURSRV3 CURSOR FOR
               SELECT C.CUST_ID, C.NAME
               FROM TUTORIAL.CUSTOMER C
               WHERE C.CUSTNO NOT LIKE 'PBS%'
                              AND ACTIVE = 'Y'
               ORDER BY C.CUST_ID
           END-EXEC

      *    switches
       01  menu-switches.
           05 is-exit-update-menu-switch      PIC X(1) VALUE 'N'.
               88  is-exit-update-menu                 VALUE 'Y'.
           05 is-existing-id-number-switch    PIC X(1) VALUE 'N'.
               88  is-existing-id-number               VALUE 'Y'.

      *    working storage data for error routine
           COPY Z0900-error-wkstg.

      *    Various generic variables
       01  wc-accept                    PIC X(2)    VALUE SPACE.
       01  we-srv-id                    PIC Z9      VALUE ZERO.
       01  we-charge                    PIC Z9.99   VALUE ZERO.
       01  we-cust-id                   PIC Z9      VALUE ZERO.


      *    Updating table variables
       01  w9-srv-id                    PIC S9(9)         COMP.
       01  wc-artno                     PIC X(10)    VALUE SPACE.
       01  wc-description               PIC X(40)    VALUE SPACE.
       01  w9-charge                    PIC S9(3)V9(2) COMP-3.


      *    Various constants
       01  HEADLINE                     PIC X(72)   VALUE ALL '-'.

       LINKAGE SECTION.
       01  lc-accept                    PIC X(2)    VALUE SPACE.
       
           
      **********************************************************
       PROCEDURE DIVISION USING lc-accept.
       0000-servicemenu.

      *    current source file to error handler
           MOVE 'servicemenu.cbl' TO wc-msg-srcfile

           EVALUATE lc-accept

               WHEN '61'
                   PERFORM M0110-list-articles
               WHEN '62'
                   PERFORM M0120-update-article
               WHEN '63'
                   PERFORM M0160-add-article
               WHEN '64'
               CONTINUE
                   PERFORM M0180-delete-article
               WHEN OTHER
                   DISPLAY 'Fel menyval från huvudprogram!'
           END-EVALUATE

           EXIT PROGRAM
           .


      **********************************************************
       M0110-list-articles.

           DISPLAY HEADLINE
           DISPLAY 'PRODUKTREGISTER'
           DISPLAY HEADLINE
           DISPLAY 'Id|Artikel   |Beskrivning' WITH NO ADVANCING
           DISPLAY '                             |Pris/faktura (kr)'
           DISPLAY HEADLINE

           EXEC SQL
               OPEN BCURSRV1
           END-EXEC

           EXEC SQL
               FETCH BCURSRV1
                   INTO :SRV-SRV-ID, :SRV-ARTNO,
                        :SRV-DESCRIPTION, :SRV-CHARGE
           END-EXEC

           PERFORM UNTIL SQLCODE NOT = ZERO

               MOVE SRV-SRV-ID TO we-srv-id
               MOVE SRV-CHARGE TO we-charge
               DISPLAY we-srv-id
                       '|' SRV-ARTNO
                       '|' SRV-DESCRIPTION
                       '|' we-charge

      *        fetch next row
               EXEC SQL
               FETCH BCURSRV1
                   INTO :SRV-SRV-ID, :SRV-ARTNO,
                        :SRV-DESCRIPTION, :SRV-CHARGE
               END-EXEC

           END-PERFORM

      *    end of data
           IF SQLSTATE NOT = "02000"
               PERFORM Z0900-error-routine
           END-IF

      *    close cursor sum up revenue
           EXEC SQL
               CLOSE BCURSRV1
           END-EXEC

           DISPLAY SPACE
           DISPLAY 'Press <Enter> för att fortsätta...'
           ACCEPT wc-accept

           .

      **********************************************************
       M0120-update-article.

           MOVE 'N' TO is-exit-update-menu-switch
           PERFORM UNTIL is-exit-update-menu

               DISPLAY HEADLINE
               DISPLAY 'UPPDATERA PRODUKTREGISTER'
               DISPLAY HEADLINE

               DISPLAY 'A - Artikel nummer'
               DISPLAY 'B - Beskrivning'
               DISPLAY 'K - Kostnad per faktura'
               DISPLAY SPACE
               DISPLAY 'X - Tillbaka till föregående meny'

               DISPLAY HEADLINE
               DISPLAY ': ' WITH NO ADVANCING
               ACCEPT wc-accept

               EVALUATE FUNCTION UPPER-CASE(wc-accept)
                   WHEN 'A'
                       PERFORM M0130-update-article-number
                   WHEN 'B'
                       PERFORM M0140-update-description
                   WHEN 'K'
                       PERFORM M0150-update-charge
                   WHEN 'X'
                       SET is-exit-update-menu TO TRUE
                       CONTINUE
                   WHEN OTHER
                       DISPLAY 'Ogiltigt val!'
               END-EVALUATE

           END-PERFORM
           .
      **********************************************************
       M0130-update-article-number.

           PERFORM M0190-confirm-id-number
           IF is-existing-id-number

               DISPLAY HEADLINE
               DISPLAY 'Existerande artikelnummer: ' wc-artno
               DISPLAY 'Ge artikelnummeret för uppdatering'
               DISPLAY ': ' WITH NO ADVANCING
               ACCEPT wc-artno(1:10)

               EXEC SQL
                   UPDATE TUTORIAL.SRV
                   SET ARTNO = :wc-artno
                   WHERE SRV_ID = :w9-srv-id
               END-EXEC

               IF SQLCODE = ZERO
                   DISPLAY 'Artikelnumret har uppdaterats!'
               ELSE
                   DISPLAY 'Ett problem uppstod vid uppdateringen!'

      *            add error trace information
                   MOVE  SQLCODE            TO wn-msg-sqlcode
                   MOVE 'TUTORIAL.SRV'      TO wc-msg-tblcurs
                   MOVE 'M0130-update-article-number' TO wc-msg-para

                   PERFORM Z0900-error-routine
               END-IF

           ELSE
               DISPLAY 'Ogiltigt id nummer - se meny 61'
           END-IF
           .

      **********************************************************
       M0140-update-description.

           PERFORM M0190-confirm-id-number
           IF is-existing-id-number

               DISPLAY HEADLINE
               DISPLAY 'Existerande beskrivning: ' wc-description
               DISPLAY 'Ge en ny beskrivning'
               DISPLAY ': ' WITH NO ADVANCING
               ACCEPT wc-description(1:40)

               EXEC SQL
                   UPDATE TUTORIAL.SRV
                   SET DESCRIPTION = :wc-description
                   WHERE SRV_ID = :w9-srv-id
               END-EXEC

               IF SQLCODE = ZERO
                   DISPLAY 'Beskrivningen har uppdaterats!'
               ELSE
                   DISPLAY 'Ett problem uppstod vid uppdateringen!'

      *            add error trace information
                   MOVE  SQLCODE            TO wn-msg-sqlcode
                   MOVE 'TUTORIAL.SRV'      TO wc-msg-tblcurs
                   MOVE 'M0140-update-description' TO wc-msg-para

                   PERFORM Z0900-error-routine
               END-IF

           ELSE
               DISPLAY 'Ogiltigt id nummer - se meny 61'
           END-IF
           .


      **********************************************************
       M0150-update-charge.

           PERFORM M0190-confirm-id-number
           IF is-existing-id-number

               MOVE w9-charge TO we-charge

               DISPLAY HEADLINE
               DISPLAY 'Existerande produktavgift: ' we-charge
               DISPLAY 'Ge en ny avgift för denna produkt'
               DISPLAY ': ' WITH NO ADVANCING
               ACCEPT w9-charge

               EXEC SQL
                   UPDATE TUTORIAL.SRV
                   SET CHARGE = :w9-charge
                   WHERE SRV_ID = :w9-srv-id
               END-EXEC

               IF SQLCODE = ZERO
                   DISPLAY 'Produktavgiften har uppdaterats!'
               ELSE
                   DISPLAY 'Ett problem uppstod vid uppdateringen!'

      *            add error trace information
                   MOVE  SQLCODE            TO wn-msg-sqlcode
                   MOVE 'TUTORIAL.SRV'      TO wc-msg-tblcurs
                   MOVE 'M0150-update-charge' TO wc-msg-para

                   PERFORM Z0900-error-routine
               END-IF

           ELSE
               DISPLAY 'Ogiltigt id nummer - se meny 61'
           END-IF
           .

      **********************************************************
       M0160-add-article.

           DISPLAY HEADLINE
           DISPLAY 'Ge ett nytt artikelnummer för denna nya produkt'
           DISPLAY ': ' WITH NO ADVANCING
           ACCEPT wc-artno(1:10)

           DISPLAY HEADLINE
           DISPLAY 'Ge en ny beskrivning'
           DISPLAY ': ' WITH NO ADVANCING
           ACCEPT wc-description(1:40)

           DISPLAY HEADLINE
           DISPLAY 'Ge en ny avgift för denna produkt'
           DISPLAY ': ' WITH NO ADVANCING
           ACCEPT w9-charge

      *    TODO not really ok, SRV-CUSTOMER table relations needs fix
           PERFORM M0170-list-customer-ids

           DISPLAY 'Välj ett kund id att knyta till denna produkt'
           DISPLAY ': ' WITH NO ADVANCING
           ACCEPT customer-cust-id

      *    open cursor
           EXEC SQL
               OPEN BCURSRV2
           END-EXEC

      *    fetch first row (which now have the highest id - i.e. pk)
           EXEC SQL
               FETCH BCURSRV2
               INTO :w9-srv-id
           END-EXEC

           IF SQLCODE NOT = ZERO
               DISPLAY 'Ett problem uppstod för att hitta nästa rad!'

      *        add error trace information
               MOVE  SQLCODE            TO wn-msg-sqlcode
               MOVE 'BCURSRV2'          TO wc-msg-tblcurs
               MOVE 'M0160-add-article' TO wc-msg-para

               PERFORM Z0900-error-routine
           ELSE
      *        add one for new article
               ADD 1 TO w9-srv-id

      *        add product to table
               EXEC SQL
                   INSERT INTO TUTORIAL.SRV
                   VALUES (:w9-srv-id, :wc-artno,:wc-description,
                           :w9-charge, :customer-cust-id)
               END-EXEC

               IF SQLCODE NOT = ZERO
                   DISPLAY 'Produkten kunde inte läggas till!'

      *            add error trace information
                   MOVE  SQLCODE            TO wn-msg-sqlcode
                   MOVE 'TUTORIAL.SRV'      TO wc-msg-tblcurs
                   MOVE 'M0160-add-article' TO wc-msg-para

                   PERFORM Z0900-error-routine

               ELSE
                   DISPLAY 'Produkten har lagts till i registret!'
               END-IF

           END-IF

      *    close cursor
           EXEC SQL
               CLOSE BCURSRV2
           END-EXEC

           .
      **********************************************************
       M0170-list-customer-ids.

           EXEC SQL
               OPEN BCURSRV3
           END-EXEC

           EXEC SQL
               FETCH BCURSRV3
                   INTO :CUSTOMER-CUST-ID,:CUSTOMER-NAME
           END-EXEC

           DISPLAY 'Id|Kundnamn'
           DISPLAY HEADLINE
           PERFORM UNTIL SQLCODE NOT = ZERO

               MOVE CUSTOMER-CUST-ID TO we-cust-id
               DISPLAY we-cust-id '|' CUSTOMER-NAME

      *        fetch next row
               EXEC SQL
               FETCH BCURSRV3
                   INTO :CUSTOMER-CUST-ID,:CUSTOMER-NAME
               END-EXEC

           END-PERFORM
           DISPLAY HEADLINE

      *    end of data
           IF SQLSTATE NOT = "02000"

      *        add error trace information
               MOVE  SQLCODE                  TO wn-msg-sqlcode
               MOVE 'BCURSRV3'                TO wc-msg-tblcurs
               MOVE 'M0170-list-customer-ids' TO wc-msg-para

               PERFORM Z0900-error-routine
           END-IF

      *    close cursor
           EXEC SQL
               CLOSE BCURSRV3
           END-EXEC
           .

      **********************************************************
       M0180-delete-article.

           PERFORM M0190-confirm-id-number
           IF is-existing-id-number


               DISPLAY HEADLINE
               DISPLAY 'Följande produkt kommer att tas bort:'
               DISPLAY 'Artikel nummer: ' wc-artno
               DISPLAY 'Beskrivning: ' wc-description
               DISPLAY 'Är du säker på att du vill ta bort [y/N]?'
               DISPLAY ': ' WITH NO ADVANCING

               ACCEPT wc-accept
               IF FUNCTION UPPER-CASE(wc-accept) = 'Y'

                   EXEC SQL
                       DELETE FROM TUTORIAL.SRV
                       WHERE SRV_ID = :w9-srv-id
                   END-EXEC

                   IF SQLCODE = ZERO
                       DISPLAY HEADLINE
                       DISPLAY 'Produkten har tagits bort!'
                   ELSE
                       DISPLAY 'Ett problem uppstod vid borttagningen'

      *                add error trace information
                       MOVE  SQLCODE               TO wn-msg-sqlcode
                       MOVE 'TUTORIAL.SRV'         TO wc-msg-tblcurs
                       MOVE 'M0180-delete-article' TO wc-msg-para

                       PERFORM Z0900-error-routine
                   END-IF

               ELSE
                   DISPLAY HEADLINE
                   DISPLAY 'Bortagning avbröts av användaren'
               END-IF

           ELSE
               DISPLAY 'Ogiltigt id nummer - se meny 61'
           END-IF
           .


      **********************************************************
       M0190-confirm-id-number.

           MOVE 'N' TO is-existing-id-number-switch

      *    TODO: display list of SRV ids to chose from

           DISPLAY HEADLINE
           DISPLAY 'Ge aktuellt id-nummer för uppdatering'
           DISPLAY ': ' WITH NO ADVANCING
           ACCEPT w9-srv-id

           EXEC SQL
               SELECT S.ARTNO, S.DESCRIPTION, S.CHARGE
                   INTO :wc-artno, :wc-description, :w9-charge
                   FROM TUTORIAL.SRV S
                   WHERE S.SRV_ID = :w9-srv-id
           END-EXEC

           IF SQLSTATE = "00000"
                SET is-existing-id-number TO TRUE
           ELSE
               IF SQLSTATE NOT = "02000"

      *            add error trace information
                   MOVE  SQLCODE                  TO wn-msg-sqlcode
                   MOVE 'TUTORIAL.SRV'            TO wc-msg-tblcurs
                   MOVE 'M0190-confirm-id-number' TO wc-msg-para

                   PERFORM Z0900-error-routine
               END-IF
           END-IF

           .

      **********************************************************
       Z0900-error-routine.

      *    requires the ending dot (and no extension)!
           COPY Z0900-error-routine.
           .

      **********************************************************
