
           EXEC SQL DECLARE TUTORIAL.ADDR TABLE
           (
              ADDR_ID                        INT
                                             NOT NULL,
              STREET                         CHAR (30)
                                             NOT NULL,
              POSTNO                         CHAR (5)
                                             NOT NULL,
              PLACE                          CHAR (30)
                                             NOT NULL
           )
           END-EXEC.


       01  ADDR.
           03 ADDR-ADDR-ID                   PIC S9(9) COMP.
           03 ADDR-STREET                    PIC X(30).
           03 ADDR-POSTNO                    PIC X(5).
           03 ADDR-PLACE                     PIC X(30).
