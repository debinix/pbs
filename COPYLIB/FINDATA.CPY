
           EXEC SQL DECLARE TUTORIAL.FINDATA TABLE
           (
              FIN_ID                         INT
                                             NOT NULL,
              VATREGNO                       CHAR (14)
                                             NOT NULL,
              BANKGIRO                       CHAR (9)
                                             NOT NULL,
              POSTGIRO                       CHAR (8)
                                             NOT NULL
           )
           END-EXEC.


       01  FINDATA.
           03 FINDATA-FIN-ID                 PIC S9(9) COMP.
           03 FINDATA-VATREGNO               PIC X(14).
           03 FINDATA-BANKGIRO               PIC X(9).
           03 FINDATA-POSTGIRO               PIC X(8).
