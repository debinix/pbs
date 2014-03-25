
           EXEC SQL DECLARE TUTORIAL.SRVCUST TABLE
           (
              SRV_ID                         INT
                                             NOT NULL,
              CUST_ID                        INT
                                             NOT NULL
           )
           END-EXEC.


       01  SRVCUST.
           03 SRVCUST-SRV-ID                 PIC S9(9) COMP.
           03 SRVCUST-CUST-ID                PIC S9(9) COMP.
