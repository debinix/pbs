
           EXEC SQL DECLARE TUTORIAL.INERROR TABLE
           (
              INERR_ID                       INT
                                             NOT NULL,
              DESCRIPTION                    VARCHAR (60),
              INLOG_ID                       INT
                                             NOT NULL
           )
           END-EXEC.


       01  INERROR.
           03 INERROR-INERR-ID               PIC S9(9) COMP.
           03 INERROR-DESCRIPTION.
              49 INERROR-DESCRIPTION-LEN     PIC S9(4) COMP.
              49 INERROR-DESCRIPTION-TEXT    PIC X(60).
           03 INERROR-INLOG-ID               PIC S9(9) COMP.
