
           EXEC SQL DECLARE TUTORIAL.INERROR TABLE
           (
              INERR_ID                       INT
                                             NOT NULL,
              DESCRIPTION                    CHAR (60)
                                             NOT NULL,
              INLOG_ID                       INT
                                             NOT NULL
           )
           END-EXEC.


       01  INERROR.
           03 INERROR-INERR-ID               PIC S9(9) COMP.
           03 INERROR-DESCRIPTION            PIC X(60).
           03 INERROR-INLOG-ID               PIC S9(9) COMP.
