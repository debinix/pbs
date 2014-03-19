
           EXEC SQL DECLARE TUTORIAL.DEBTOR TABLE
           (
              DEBT_ID                        INT
                                             NOT NULL,
              NAME                           CHAR (40)
                                             NOT NULL,
              CONTACT                        CHAR (40)
                                             NOT NULL,
              NOTE                           CHAR (60),
              ADDR_ID                        INT
                                             NOT NULL
           )
           END-EXEC.


       01  DEBTOR.
           03 DEBTOR-DEBT-ID                 PIC S9(9) COMP.
           03 DEBTOR-NAME                    PIC X(40).
           03 DEBTOR-CONTACT                 PIC X(40).
           03 DEBTOR-NOTE                    PIC X(60).
           03 DEBTOR-ADDR-ID                 PIC S9(9) COMP.
