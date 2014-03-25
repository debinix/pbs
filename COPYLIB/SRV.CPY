
           EXEC SQL DECLARE TUTORIAL.SRV TABLE
           (
              SRV_ID                         INT
                                             NOT NULL,
              ARTNO                          CHAR (10)
                                             NOT NULL,
              DESCRIPTION                    CHAR (40)
                                             NOT NULL,
              CHARGE                         DECIMAL (5, 2)
                                             NOT NULL
           )
           END-EXEC.


       01  SRV.
           03 SRV-SRV-ID                     PIC S9(9) COMP.
           03 SRV-ARTNO                      PIC X(10).
           03 SRV-DESCRIPTION                PIC X(40).
           03 SRV-CHARGE                     PIC S9(3)V9(2) COMP-3.
