      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. servicemenu IS INITIAL.
      *
      * Coder: BK
      * Purpose: Maintain PBS products (services) database table
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


      *    cursors

      *    list 'PBS Ekonomi' services
           EXEC SQL
               DECLARE BCURSRV1 CURSOR FOR
               SELECT S.SRV_ID, S.ARTNO,
                      S.DESCRIPTION, S.CHARGE
               FROM TUTORIAL.SRV S
               ORDER BY S.ARTNO
           END-EXEC


      *    switches

      *    Various generic variables
       01  wc-accept                    PIC X(2)    VALUE SPACE.
       
      *    Various constants
       01  HEADLINE                     PIC X(72)   VALUE ALL '-'.

       LINKAGE SECTION.
       01  lc-accept                    PIC X(2)    VALUE SPACE.
       
           
      **********************************************************
       PROCEDURE DIVISION USING lc-accept.
       0000-servicemenu.


          EVALUATE lc-accept

               WHEN '61'
                   PERFORM M0110-list-articles
               WHEN '62'
               CONTINUE
      *            PERFORM M0120-update-articles
               WHEN '63'
               CONTINUE
      *            PERFORM M0130-add-article
               WHEN '64'
               CONTINUE
      *            PERFORM M0140-delete-article
               WHEN OTHER
                   DISPLAY 'Ogiltigt meny val!'
           END-EVALUATE

           DISPLAY SPACE
           DISPLAY 'Tryck <Enter> för att fortsätta...'
               WITH NO ADVANCING
           ACCEPT wc-accept


           EXIT PROGRAM
           .


      **********************************************************
       M0110-list-articles.


           DISPLAY '-------------------'
           DISPLAY 'BEFINTLIGA TJÄNSTER'
           DISPLAY '-------------------'

           EXEC SQL
               OPEN BCURSRV1
           END-EXEC

           EXEC SQL
               FETCH BCURSRV1
                   INTO :SRV-SRV-ID, :SRV-ARTNO,
                        :SRV-DESCRIPTION, :SRV-CHARGE
           END-EXEC

           PERFORM UNTIL SQLCODE NOT = ZERO

               DISPLAY SRV-SRV-ID
                       '|' SRV-ARTNO
                       '|' SRV-DESCRIPTION
                       '|' SRV-CHARGE

      *        fetch next row (these coloumns are tbl VARCHAR)
               MOVE SPACE TO SRV-ARTNO
               MOVE SPACE TO SRV-DESCRIPTION

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

           .

      **********************************************************
       Z0900-error-routine.

      *    simple error check - see pp 277 for improved version
           DISPLAY 'ERROR SQLCODE: ' SQLCODE

      *    SQLSTATE for XDB may be different from DB2 and other stds
           EVALUATE SQLSTATE

              WHEN "02000"
                  DISPLAY 'Data återfinns ej i databasen!'
              WHEN "24501"
                  DISPLAY 'Cursorn öppnades ej korrekt (SQL syntax).'
              WHEN "21000"
                  DISPLAY 'Embedded SQL returnerade mer än en rad.'
              WHEN "08003"
              WHEN "08001"
                  DISPLAY 'Anslutning till databas misslyckades.'
              WHEN "23503"
                  DISPLAY 'Ej giltigt namn för Constraint (fk).'
              WHEN "42612"
              WHEN "37512"
                  DISPLAY 'Kan ej köra direkt SQL (ev SQL syntax)'
              WHEN SPACE
                  DISPLAY 'Obekant fel!'
              WHEN OTHER
                  DISPLAY 'SQLSTATE : ' SQLSTATE

            END-EVALUATE

            .
      **********************************************************
