      *
      *    SQL Copybook Z0900-error-routine.cpy
      *
      *    Put this file in the /COPYLIB directory.
      *    Include in source with: 'COPY Z0900-error-routine.'

           DISPLAY 'ERROR SQLCODE: ' SQLCODE

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



