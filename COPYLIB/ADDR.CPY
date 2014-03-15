
           EXEC SQL DECLARE TUTORIAL.ADDR TABLE
           (
              ADDR_ID                        INT
                                             NOT NULL,
              STREET                         VARCHAR (254)
                                             NOT NULL,
              POSTNO                         CHAR (5)
                                             NOT NULL,
              PLACE                          VARCHAR (254)
                                             NOT NULL
           )
           END-EXEC.


       01  ADDR.
           03 ADDR-ADDR-ID                   PIC S9(9) COMP.
           03 ADDR-STREET.
              49 ADDR-STREET-LEN             PIC S9(4) COMP.
              49 ADDR-STREET-TEXT            PIC X(254).
           03 ADDR-POSTNO                    PIC X(5).
           03 ADDR-PLACE.
              49 ADDR-PLACE-LEN              PIC S9(4) COMP.
              49 ADDR-PLACE-TEXT             PIC X(254).
