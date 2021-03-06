
           EXEC SQL DECLARE TUTORIAL.INLOG TABLE
           (
              INLOG_ID                       INT
                                             NOT NULL,
              CUST_ID                        INT
                                             NOT NULL,
              FILENO                         INT
                                             NOT NULL,
              PROCDATE                       DATE
                                             NOT NULL,
              INVNO                          CHAR (16)
                                             NOT NULL,
              RESULTCODE                     SMALLINT
                                             NOT NULL
           )
           END-EXEC.


       01  INLOG.
           03 INLOG-INLOG-ID                 PIC S9(9) COMP.
           03 INLOG-CUST-ID                  PIC S9(9) COMP.
           03 INLOG-FILENO                   PIC S9(9) COMP.
           03 INLOG-PROCDATE                 PIC X(10).
           03 INLOG-INVNO                    PIC X(16).
           03 INLOG-RESULTCODE               PIC S9(4) COMP.
